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
#include <error.h>
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

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-sel-cmds.h"
#include "freeipmi/ipmi-sel-record-types.h"
#include "freeipmi/ipmi-sensor-and-event-code-tables.h"
#include "freeipmi/udm/ipmi-sel-cmds-udm.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"
#include "ipmi-sensor-api.h"

#include "ipmi-sel-wrapper.h"

int 
get_sel_info (ipmi_device_t dev, local_sel_info_t *sel_info)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  ERR_EINVAL (dev && sel_info);

  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_info_rs);

  if (ipmi_cmd_get_sel_info (dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sel_version_major", &val);
  sel_info->sel_version_major = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sel_version_minor", &val);
  sel_info->sel_version_minor = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "entries", &val);
  sel_info->log_entry_count = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "free_space", &val);
  sel_info->free_space = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "most_recent_addition_timestamp", &val);
  sel_info->recent_addition_timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "most_recent_erase_timestamp", &val);
  sel_info->recent_erase_timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "get_sel_allocation_info_command_supported", &val);
  sel_info->get_sel_alloc_info_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "reserve_sel_command_supported", &val);
  sel_info->reserve_sel_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "partial_add_sel_entry_command_supported", &val);
  sel_info->partial_add_sel_entry_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "delete_sel_command_supported", &val);
  sel_info->delete_sel_cmd_support = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "overflow_flag", &val);
  sel_info->overflow_flag = val;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

static int 
_get_sel_system_event_record (uint8_t *record_data, 
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

  ERR_EINVAL (record_data && sel_record);

  FIID_OBJ_CREATE (obj, tmpl_sel_system_event_record);

  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "record_id", &val);
  record_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "timestamp", &val);
  timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "generator_id.id_type", &val);
  generator_id_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "generator_id.id", &val);
  generator_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "channel_number", &val);
  channel_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_type", &val);
  sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_number", &val);
  sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_type_code", &val);
  event_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_dir", &val);
  event_dir = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "offset_from_event_reading_type_code", &val);
  offset_from_event_reading_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_data2_flag", &val);
  event_data2_flag = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_data3_flag", &val);
  event_data3_flag = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_data2", &val);
  event_data2 = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_data3", &val);
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
	snprintf(buffer, 1024, "Event Type Code = %02Xh", event_type_code);
	rv = 0;
	break;
      }

    if (!rv)
      ERR_CLEANUP ((sel_record->event_message = strdup(buffer)));
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
	    asprintf (&(sel_record->event_data2_message), 
		      "Trigger reading = %02Xh", 
		      event_data2);
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
		ERR_CLEANUP ((sel_record->event_data2_message = strdup(buffer)));
	      else
		sel_record->event_data2_message = NULL;
	    }
	    break;
	  }
	
	sel_record->event_data3_message = NULL;
	switch (event_data3_flag)
	  {
	  case IPMI_SEL_TRIGGER_THRESHOLD_VALUE:
	    asprintf (&(sel_record->event_data3_message), 
		      "Trigger reading = %02Xh", 
		      event_data3);
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
		ERR_CLEANUP ((sel_record->event_data3_message = strdup(buffer)));
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
		ERR_CLEANUP ((sel_record->event_data2_message = strdup(buffer)));
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
		ERR_CLEANUP ((sel_record->event_data3_message = strdup(buffer)));
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
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

static int 
_get_sel_timestamped_oem_record (uint8_t *record_data, 
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

  ERR_EINVAL (record_data && sel_record);

  FIID_OBJ_CREATE (obj, tmpl_sel_timestamped_oem_record);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "record_id", &val);
  record_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "timestamp", &val);
  timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "manufacturer_id", &val);
  manufacturer_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "oem_defined", &val);
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
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

static int
_get_sel_non_timestamped_oem_record (uint8_t *record_data, 
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

  ERR_EINVAL (record_data && sel_record);

  FIID_OBJ_CREATE (obj, tmpl_sel_non_timestamped_oem_record);

  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "record_id", &val);
  record_id = val;

  memset(buf, '\0', 1024);
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
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
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

static int
_parse_sel_record (uint8_t *record_data,
                   uint32_t record_data_len,
                   sel_record_t *sel_record)
{
  fiid_obj_t obj = NULL;
  uint8_t record_type;
  uint64_t val;
  int rv = -1;

  ERR_EINVAL (record_data && sel_record);

  FIID_OBJ_CREATE (obj, tmpl_sel_record_header);

  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "record_type", &val);
  record_type = val;

  switch (ipmi_get_sel_record_type (record_type))
    {
    case IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD:
      rv = _get_sel_system_event_record (record_data, record_data_len, sel_record);
      break;
    case IPMI_SEL_RECORD_TYPE_TIMESTAMPED_OEM_RECORD:
      rv = _get_sel_timestamped_oem_record (record_data, record_data_len, sel_record);
      break;
    case IPMI_SEL_RECORD_TYPE_NON_TIMESTAMPED_OEM_RECORD:
      rv = _get_sel_non_timestamped_oem_record (record_data, record_data_len, sel_record);
      break;
    }

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

int 
get_sel_record (ipmi_device_t dev, 
                uint16_t record_id, 
                sel_record_t *sel_rec, 
                uint16_t *next_record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint64_t val;
  int rv = -1;
  int32_t len;
  
  uint8_t record_data[SEL_RECORD_SIZE];
  uint32_t record_data_len = SEL_RECORD_SIZE;
  
  ERR_EINVAL (dev && sel_rec && next_record_id);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_entry_rs);
  
  if (ipmi_cmd_get_sel_entry (dev, 
			      0,
			      record_id, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
			      obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj_cmd_rs, 
				 "record_data", 
				 record_data,
				 record_data_len);
  record_data_len = len;
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  if (rv == 0)
    {
      return (_parse_sel_record (record_data, record_data_len, sel_rec));
    }
  else
    {
      return (rv);
    }
}

int 
get_sel_record_raw (ipmi_device_t dev, 
                    uint16_t record_id, 
                    uint8_t *record_data, 
                    uint32_t record_data_len, 
                    uint16_t *next_record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint64_t val;
  int rv = -1;
  int32_t len;
  
  ERR_EINVAL (dev && record_data && next_record_id);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_entry_rs);
  if (ipmi_cmd_get_sel_entry (dev, 
			      0,
			      record_id, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
			      obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj_cmd_rs, 
				 "record_data", 
				 record_data,
				 record_data_len);
  record_data_len = len;
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

int 
delete_sel_entry (ipmi_device_t dev, uint16_t record_id)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;
  
  if (ipmi_cmd_reserve_sel (dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_delete_sel_entry_rs)))
    goto cleanup;
  
  if (ipmi_cmd_delete_sel_entry (dev, 
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
clear_sel_entries (ipmi_device_t dev)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (dev, 
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
get_sel_clear_status (ipmi_device_t dev, int *status)
{
  fiid_obj_t obj_cmd_rs;
  uint16_t reservation_id;
  uint64_t val;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_reserve_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_reserve_sel (dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, "reservation_id", &val) < 0)
    goto cleanup;
  reservation_id = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_clear_sel_rs)))
    goto cleanup;

  if (ipmi_cmd_clear_sel (dev, 
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
