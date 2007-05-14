#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <argp.h>
#include <assert.h>

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-sel-cmds.h"
#include "freeipmi/ipmi-sel-record-types.h"
#include "freeipmi/ipmi-sensor-and-event-code-tables.h"
#include "freeipmi/udm/ipmi-sel-cmds-udm.h"

#include "freeipmi-portability.h"
#include "ipmi-common.h"
#include "ipmi-sdr-cache.h"
#include "ipmi-sensor-api.h"

#include "ipmi-sel-wrapper.h"

#define _FIID_OBJ_CREATE(__obj, __tmpl)         \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    {                                           \
      pstdout_fprintf(state_data->pstate,       \
                      stderr,                   \
                      "fiid_obj_create: %s\n",  \
                      strerror(errno));         \
      goto cleanup;                             \
    }                                           \
} while (0)

#define _FIID_OBJ_GET(__obj, __field, __val)                    \
do {                                                            \
    uint64_t __localval = 0, *__localval_ptr;                   \
    __localval_ptr = (__val);                                   \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)     \
      {                                                         \
        pstdout_fprintf(state_data->pstate,                     \
                        stderr,                                 \
                        "fiid_obj_get: %s: %s\n",               \
                        __field,                                \
                        fiid_strerror(fiid_obj_errnum(__obj))); \
         goto cleanup;                                          \
      }                                                         \
    *__localval_ptr = __localval;                               \
} while (0)

#define _FIID_OBJ_SET_ALL(__obj, __data, __data_len)                        \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         pstdout_fprintf(state_data->pstate,                                \
                        stderr,                                             \
                        "fiid_obj_set_all: %s\n",                           \
                        fiid_strerror(fiid_obj_errnum(__obj)));             \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define _FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)               \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         pstdout_fprintf(state_data->pstate,                                            \
                        stderr,                                                         \
                        "fiid_obj_get_data: %s: %s\n",                                  \
                        __field,                                                        \
                        fiid_strerror(fiid_obj_errnum(__obj)));                         \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

int 
get_sel_info (ipmi_sel_state_data_t *state_data, 
              local_sel_info_t *sel_info)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(state_data);
  assert(sel_info);

  _FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_info_rs);

  if (ipmi_cmd_get_sel_info (state_data->dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  _FIID_OBJ_GET (obj_cmd_rs, "sel_version_major", &val);
  sel_info->sel_version_major = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "sel_version_minor", &val);
  sel_info->sel_version_minor = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "entries", &val);
  sel_info->log_entry_count = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "free_space", &val);
  sel_info->free_space = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "most_recent_addition_timestamp", &val);
  sel_info->recent_addition_timestamp = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "most_recent_erase_timestamp", &val);
  sel_info->recent_erase_timestamp = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "get_sel_allocation_info_command_supported", &val);
  sel_info->get_sel_alloc_info_cmd_support = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "reserve_sel_command_supported", &val);
  sel_info->reserve_sel_cmd_support = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "partial_add_sel_entry_command_supported", &val);
  sel_info->partial_add_sel_entry_cmd_support = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "delete_sel_command_supported", &val);
  sel_info->delete_sel_cmd_support = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "overflow_flag", &val);
  sel_info->overflow_flag = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static sdr_record_t *
_find_sdr_record(ipmi_sel_state_data_t *state_data,
                 uint8_t sensor_number)

{
  sdr_record_t *sdr_record;
  int i;
  
  assert(state_data);
  assert(state_data->sdr_record_list);
  assert(state_data->sdr_record_count);

  for (i = 0; i < state_data->sdr_record_count; i++)
    {
      sdr_record = state_data->sdr_record_list + i;
      
      if ((sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD 
           && sdr_record->record.sdr_full_record.sensor_number == sensor_number)
          || (sdr_record->record_type == IPMI_SDR_FORMAT_COMPACT_RECORD
              && sdr_record->record.sdr_compact_record.sensor_number == sensor_number)
          || (sdr_record->record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD
              && sdr_record->record.sdr_event_only_record.sensor_number == sensor_number))
	return sdr_record;
    }

  return NULL;
}

static double
_round_double2 (double d)
{
  double r = 0.0;

  r = (d - (long) d) * 100.0;

  if ((r - (long) r) > 0.5)
    return ((long) d + (((long) r + 1) / 100.0));

  return ((long) d + ((long) r / 100.0));
}

static int 
_get_sel_system_event_record (ipmi_sel_state_data_t *state_data,
                              uint8_t *record_data, 
                              uint32_t record_data_len,
                              sel_record_t *sel_record)
{
  uint16_t record_id;
  uint32_t timestamp;
  uint8_t generator_id_type;
  uint8_t generator_id;
  uint8_t channel_number;
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_type_code;
  uint8_t event_dir;
  uint8_t offset_from_event_reading_type_code;
  uint8_t event_data2_flag;
  uint8_t event_data3_flag;
  uint8_t event_data2;
  uint8_t event_data3;

  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;

  sdr_record_t *sdr_record = NULL;

  assert(state_data);
  assert(record_data);
  assert(sel_record);

  _FIID_OBJ_CREATE (obj, tmpl_sel_system_event_record);

  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  _FIID_OBJ_GET (obj, "record_id", &val);
  record_id = val;
  
  _FIID_OBJ_GET (obj, "timestamp", &val);
  timestamp = val;
  
  _FIID_OBJ_GET (obj, "generator_id.id_type", &val);
  generator_id_type = val;
  
  _FIID_OBJ_GET (obj, "generator_id.id", &val);
  generator_id = val;
  
  _FIID_OBJ_GET (obj, "channel_number", &val);
  channel_number = val;
  
  _FIID_OBJ_GET (obj, "sensor_type", &val);
  sensor_type = val;
  
  _FIID_OBJ_GET (obj, "sensor_number", &val);
  sensor_number = val;
  
  _FIID_OBJ_GET (obj, "event_type_code", &val);
  event_type_code = val;
  
  _FIID_OBJ_GET (obj, "event_dir", &val);
  event_dir = val;
  
  _FIID_OBJ_GET (obj, "offset_from_event_reading_type_code", &val);
  offset_from_event_reading_type_code = val;
  
  _FIID_OBJ_GET (obj, "event_data2_flag", &val);
  event_data2_flag = val;
  
  _FIID_OBJ_GET (obj, "event_data3_flag", &val);
  event_data3_flag = val;
  
  _FIID_OBJ_GET (obj, "event_data2", &val);
  event_data2 = val;
  
  _FIID_OBJ_GET (obj, "event_data3", &val);
  event_data3 = val;
  
  sel_record->record_id = record_id;
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    sel_record->timestamp = strdup (buffer);
  }

  sel_record->sensor_info = NULL;

  sdr_record = _find_sdr_record(state_data, sensor_number);

  if (sdr_record)
    {
      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD) 
        asprintf (&(sel_record->sensor_info), 
                  "%s %s", 
                  ipmi_get_sensor_group (sensor_type), 
                  sdr_record->record.sdr_full_record.sensor_name);
      else if (sdr_record->record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
        asprintf (&(sel_record->sensor_info), 
                  "%s %s", 
                  ipmi_get_sensor_group (sensor_type), 
                  sdr_record->record.sdr_compact_record.sensor_name);
      else if (sdr_record->record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
        asprintf (&(sel_record->sensor_info), 
                  "%s %s", 
                  ipmi_get_sensor_group (sensor_type), 
                  sdr_record->record.sdr_event_only_record.sensor_name);
      else
        asprintf (&(sel_record->sensor_info), 
                  "%s #%d", 
                  ipmi_get_sensor_group (sensor_type), sensor_number);
    }
  else
    asprintf (&(sel_record->sensor_info), 
              "%s #%d", 
              ipmi_get_sensor_group (sensor_type), sensor_number);
  
  {
    char buffer[1024];
    int rv;

    switch (ipmi_sensor_classify (event_type_code))
      {
      case IPMI_SENSOR_CLASS_THRESHOLD:
      case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
	rv = ipmi_get_generic_event_message(event_type_code,
					    offset_from_event_reading_type_code,
					    buffer, 
					    1024);
	break;
      case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
	rv = ipmi_get_sensor_type_code_message(sensor_type,
					       offset_from_event_reading_type_code,
					       buffer,
					       1024);
	break;
      case IPMI_SENSOR_CLASS_OEM:
      default:
	snprintf(buffer, 1024, "Event Type Code = %02Xh", event_type_code);
	rv = 0;
	break;
      }

    if (!rv)
      {
        if (!(sel_record->event_message = strdup(buffer)))
          {
            pstdout_perror (state_data->pstate, "strdup");
            goto cleanup;
          }
      }
    else
      sel_record->event_message = NULL;
  }

  switch (ipmi_sensor_classify (event_type_code))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      {
	sel_record->event_data2_message = NULL;
	switch (event_data2_flag)
	  {
	  case IPMI_SEL_TRIGGER_READING:
	    if (state_data->sdr_record_list 
		&& state_data->sdr_record_count
		&& sdr_record 
		&& sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD
		&& sdr_record->record.sdr_full_record.event_reading_type_code == IPMI_SENSOR_CLASS_THRESHOLD)
	      {
		double current_reading;

		if (ipmi_sensor_decode_value (sdr_record->record.sdr_full_record.r_exponent,
                                              sdr_record->record.sdr_full_record.b_exponent,
                                              sdr_record->record.sdr_full_record.m,
                                              sdr_record->record.sdr_full_record.b,
                                              sdr_record->record.sdr_full_record.linear,
                                              sdr_record->record.sdr_full_record.analog_data_format,
                                              event_data2,
                                              &current_reading) < 0)
                  {
                    pstdout_fprintf (state_data->pstate,
                                     stderr,
                                     "ipmi_sensor_decode_value: %s\n",
                                     strerror(errno));
                    goto cleanup;
                  }
		
		asprintf (&(sel_record->event_data2_message),
			  "Reading = %.2f %s",
			  _round_double2 (current_reading),
			  ipmi_sensor_units_abbreviated[sdr_record->record.sdr_full_record.sensor_unit]);
	      }
	    else
	      {
		asprintf (&(sel_record->event_data2_message), 
			  "Trigger reading = %02Xh", 
			  event_data2);
	      }
	    break;
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data2_message), 
		      "OEM code = %02Xh", 
		      event_data2);
	    break;
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data2_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 buffer,
						 1024);
	      if (!rv)
                {
                  if (!(sel_record->event_data2_message = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
	      else
		sel_record->event_data2_message = NULL;
	    }
	    break;
	  }
	
	sel_record->event_data3_message = NULL;
	switch (event_data3_flag)
	  {
	  case IPMI_SEL_TRIGGER_THRESHOLD_VALUE:
	    if (state_data->sdr_record_list 
		&& state_data->sdr_record_count
		&& (sdr_record = _find_sdr_record(state_data, sensor_number))
		&& sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD
		&& sdr_record->record.sdr_full_record.event_reading_type_code == IPMI_SENSOR_CLASS_THRESHOLD)
	      {
		double current_reading;

		if (ipmi_sensor_decode_value (sdr_record->record.sdr_full_record.r_exponent,
                                              sdr_record->record.sdr_full_record.b_exponent,
                                              sdr_record->record.sdr_full_record.m,
                                              sdr_record->record.sdr_full_record.b,
                                              sdr_record->record.sdr_full_record.linear,
                                              sdr_record->record.sdr_full_record.analog_data_format,
                                              event_data3,
                                              &current_reading) < 0)
                  {
                    pstdout_fprintf (state_data->pstate,
                                     stderr,
                                     "ipmi_sensor_decode_value: %s\n",
                                     strerror(errno));
                    goto cleanup;
                  }
		
		asprintf (&(sel_record->event_data3_message),
			  "Threshold = %.2f %s",
			  _round_double2 (current_reading),
			  ipmi_sensor_units_abbreviated[sdr_record->record.sdr_full_record.sensor_unit]);
	      }
	    else
	      {
		asprintf (&(sel_record->event_data3_message), 
			  "Trigger reading = %02Xh", 
			  event_data3);
	      }
	    break;
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data3_message), 
		      "OEM code = %02Xh", 
		      event_data3);
	    break;
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data3_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 event_data3,
						 buffer,
						 1024);
	      if (!rv)
                {
                  if (!(sel_record->event_data3_message = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
	      else
		sel_record->event_data3_message = NULL;
	    }
	    break;
	  }
	
	break;
      }
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      {
	sel_record->event_data2_message = NULL;
	switch (event_data2_flag)
	  {
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data2_message),
		      "OEM code = %02Xh",
		      event_data2);
	    break;
	  case IPMI_SEL_PREV_STATE_SEVERITY:
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data2_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 buffer,
						 1024);
	      if (!rv)
                {
                  if (!(sel_record->event_data2_message = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
	      else
		sel_record->event_data2_message = NULL;
	    }
	  }
	
	sel_record->event_data3_message = NULL;
	switch (event_data3_flag)
	  {
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data3_message),
		      "OEM code = %02Xh",
		      event_data3);
	    break;
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data3_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 event_data3,
						 buffer,
						 1024);
	      if (!rv)
                {
                  if (!(sel_record->event_data3_message = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
	      else
		sel_record->event_data3_message = NULL;
	    }
	    break;
	  }
	
	break;
      }
    case IPMI_SENSOR_CLASS_OEM:
      {
	asprintf (&(sel_record->event_data2_message), 
		  "Event Data2 = %02Xh", 
		  event_data2);
	asprintf (&(sel_record->event_data3_message), 
		  "Event Data3 = %02Xh", 
		  event_data3);
	break;
      }
    default:
      sel_record->event_data2_message = NULL;
      sel_record->event_data3_message = NULL;
    }
  
  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return (rv);
}

static int 
_get_sel_timestamped_oem_record (ipmi_sel_state_data_t *state_data,
                                 uint8_t *record_data, 
                                 uint32_t record_data_len,
                                 sel_record_t *sel_record)
{
  uint16_t record_id;
  uint32_t timestamp;
  uint32_t manufacturer_id;
  uint64_t oem_defined;
  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;

  assert (state_data);
  assert (record_data);
  assert (sel_record);

  _FIID_OBJ_CREATE (obj, tmpl_sel_timestamped_oem_record);
  
  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  _FIID_OBJ_GET (obj, "record_id", &val);
  record_id = val;
  
  _FIID_OBJ_GET (obj, "timestamp", &val);
  timestamp = val;
  
  _FIID_OBJ_GET (obj, "manufacturer_id", &val);
  manufacturer_id = val;
  
  _FIID_OBJ_GET (obj, "oem_defined", &val);
  oem_defined = val;
  
  sel_record->record_id = record_id;
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    sel_record->timestamp = strdup (buffer);
  }
  asprintf (&(sel_record->sensor_info), 
	    "Manufacturer ID %02Xh", 
	    manufacturer_id);
  asprintf (&(sel_record->event_message), 
	    "OEM Defined = " FI_64 "Xh",
	    oem_defined);
  
  sel_record->event_data2_message = NULL;
  sel_record->event_data3_message = NULL;
  
  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return (rv);
}

static int
_get_sel_non_timestamped_oem_record (ipmi_sel_state_data_t *state_data,
                                     uint8_t *record_data, 
                                     uint32_t record_data_len, 
                                     sel_record_t *sel_record)
{
  uint16_t record_id;
  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;
  uint8_t buf[1024];
  int32_t len;
  char *str = NULL;
  char *tmp_str = NULL;
  int i;

  assert (state_data);
  assert (record_data);
  assert (sel_record);

  _FIID_OBJ_CREATE (obj, tmpl_sel_non_timestamped_oem_record);

  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  _FIID_OBJ_GET (obj, "record_id", &val);
  record_id = val;

  memset(buf, '\0', 1024);
  _FIID_OBJ_GET_DATA_LEN (len,
                          obj,
                          "oem_defined",
                          buf,
                          1024);

  sel_record->record_id = record_id;
  sel_record->timestamp = NULL;
  sel_record->sensor_info = NULL;
  sel_record->event_message = NULL;

  for (i = 0; i < len; i++)
    {
      tmp_str = str;
      if (str)
        {
          str = NULL;
          asprintf (&str, "%s %02X", tmp_str, buf[i]);
          free (tmp_str);
        }
      else
        asprintf (&str, "%02X", buf[i]);
    }

  if (str)
    {
      asprintf (&(sel_record->event_message), "OEM defined = %s", str);
      free (str);
    }

  sel_record->event_data2_message = NULL;
  sel_record->event_data3_message = NULL;

  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return (rv);
}

static int
_parse_sel_record (ipmi_sel_state_data_t *state_data, 
                   uint8_t *record_data,
                   uint32_t record_data_len,
                   sel_record_t *sel_record)
{
  fiid_obj_t obj = NULL;
  uint8_t record_type;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (record_data);
  assert (sel_record);

  _FIID_OBJ_CREATE (obj, tmpl_sel_record_header);

  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  _FIID_OBJ_GET (obj, "record_type", &val);
  record_type = val;

  switch (ipmi_get_sel_record_type (record_type))
    {
    case IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD:
      rv = _get_sel_system_event_record (state_data, record_data, record_data_len, sel_record);
      break;
    case IPMI_SEL_RECORD_TYPE_TIMESTAMPED_OEM_RECORD:
      rv = _get_sel_timestamped_oem_record (state_data, record_data, record_data_len, sel_record);
      break;
    case IPMI_SEL_RECORD_TYPE_NON_TIMESTAMPED_OEM_RECORD:
      rv = _get_sel_non_timestamped_oem_record (state_data, record_data, record_data_len, sel_record);
      break;
    }

 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return (rv);
}

sel_record_t *
get_sel_record (ipmi_sel_state_data_t *state_data, 
                uint16_t record_id, 
                uint16_t *next_record_id)
{
  sel_record_t *sel_rec=NULL;
  fiid_obj_t obj_cmd_rs;
  uint64_t val;
  int32_t len;
  
  uint8_t record_data[SEL_RECORD_SIZE];
  uint32_t record_data_len = SEL_RECORD_SIZE;
  
  assert (state_data);
  assert (next_record_id);
  
  _FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_entry_rs);
  
  if (!(sel_rec = (sel_record_t *)malloc(sizeof(sel_record_t))))
    goto cleanup;

  if (ipmi_cmd_get_sel_entry (state_data->dev, 
			      0,
			      record_id, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
			      obj_cmd_rs) != 0)
    goto cleanup;
  
  _FIID_OBJ_GET (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  _FIID_OBJ_GET_DATA_LEN (len,
                          obj_cmd_rs, 
                          "record_data", 
                          record_data,
                          record_data_len);
  record_data_len = len;

  if (_parse_sel_record (state_data,
                         record_data, 
                         record_data_len, 
                         sel_rec) < 0)
    goto cleanup;

  fiid_obj_destroy(obj_cmd_rs);
  return sel_rec;

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  if (sel_rec)
    destroy_sel_record(sel_rec);
  return NULL;
}

void
destroy_sel_record (sel_record_t *sel_rec)
{
  if (sel_rec)
    {
      if (sel_rec->timestamp)
        free (sel_rec->timestamp);
      if (sel_rec->sensor_info) 
        free (sel_rec->sensor_info);
      if (sel_rec->event_message) 
        free (sel_rec->event_message);
      if (sel_rec->event_data2_message) 
        free (sel_rec->event_data2_message);
      if (sel_rec->event_data3_message) 
        free (sel_rec->event_data3_message);
      free(sel_rec);
    }
}

int 
get_sel_record_raw (ipmi_sel_state_data_t *state_data, 
                    uint16_t record_id, 
                    uint8_t *record_data, 
                    uint32_t record_data_len, 
                    uint16_t *next_record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint64_t val;
  int rv = -1;
  int32_t len;
  
  assert (state_data);
  assert (record_data);
  assert (next_record_id);
  
  _FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_entry_rs);

  if (ipmi_cmd_get_sel_entry (state_data->dev, 
			      0,
			      record_id, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
			      obj_cmd_rs) != 0)
    goto cleanup;
  
  _FIID_OBJ_GET (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  _FIID_OBJ_GET_DATA_LEN (len,
                          obj_cmd_rs, 
                          "record_data", 
                          record_data,
                          record_data_len);
  record_data_len = len;
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int 
delete_sel_entry (ipmi_sel_state_data_t *state_data, 
                  uint16_t record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;
  
  if (ipmi_cmd_reserve_sel (state_data->dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_delete_sel_entry_rs)))
    goto cleanup;
  
  if (ipmi_cmd_delete_sel_entry (state_data->dev, 
				 reservation_id, 
				 record_id, 
				 obj_cmd_rs) != 0)
    goto cleanup;
  
  fiid_obj_destroy(obj_cmd_rs);
  return 0;
  
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (-1);
}

int 
clear_sel_entries (ipmi_sel_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (state_data->dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (state_data->dev, 
			  reservation_id, 
			  IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE, 
			  obj_cmd_rs) != 0)
    goto cleanup;
  
  return 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (-1);
}

int 
get_sel_clear_status (ipmi_sel_state_data_t *state_data, 
                      int *status)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (state_data->dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (state_data->dev, 
			  reservation_id, 
			  IPMI_SEL_CLEAR_OPERATION_GET_ERASURE_STATUS, 
			  obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, "erasure_progress", &val) < 0)
    goto cleanup;
  
  fiid_obj_destroy(obj_cmd_rs);
  *status = val;
  return 0;

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (-1);
}
