#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "freeipmi.h"

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include <stdlib.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "fi-utils.h"
#include "ipmi-wrapper-sel.h"
#include "fish.h"

sel_descriptor_t seld;

int
get_sel_info (sel_info_t* pinfo)
{
  fiid_obj_t obj_data_rs;
  int status;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sel_info_rs));
  status = ipmi_kcs_get_sel_info (obj_data_rs);
  
  if (status != 0)
    {
      fprintf (stderr,
	       "error: ipmi_kcs_get_sel_info() failed.\n");
      return (-1);
    }
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr,
	       "error: ipmi_kcs_get_sel_info() failed with %s\n",
	       err_msg);
      return (-1);
    }
  
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "sel_version_major", &val);
  pinfo->version_major = val;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "sel_version_minor", &val);
  pinfo->version_minor = val;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "log_entry_count", &val);
  pinfo->entry_count = val;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "free_space", &val);
  pinfo->free_space = val;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "recent_addition_timestamp", &val);
  pinfo->last_add_time = val;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "recent_erase_timestamp", &val);
  pinfo->last_erase_time = val;
  pinfo->flags = 0;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "get_sel_alloc_info_cmd_support", &val);
  if (val) pinfo->flags |= get_sel_alloc_info_cmd_support;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "reserve_sel_cmd_support", &val);
  if (val) pinfo->flags |= reserve_sel_cmd_support;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "partial_add_sel_entry_cmd_support", &val);
  if (val) pinfo->flags |= partial_add_sel_entry_cmd_support;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "delete_sel_cmd_support", &val);
  if (val) pinfo->flags |= delete_sel_cmd_support;
  fiid_obj_get (obj_data_rs, tmpl_get_sel_info_rs, "overflow_flag", &val);
  if (val) pinfo->flags |= overflow_flag;

  return 0;
}

sel_descriptor_t *
get_seld ()
{
  return &seld;
}

int 
get_sel_system_event_record (u_int8_t *record_data, struct sel_record *sel_record)
{
  u_int16_t record_id;
  u_int32_t timestamp;
  u_int8_t ipmb_slave_addr_sys_soft_id_flag;
  u_int8_t ipmb_slave_addr_sys_soft_id;
  u_int8_t channel_number;
  u_int8_t sensor_type;
  u_int8_t sensor_number;
  u_int8_t event_type_code;
  u_int8_t assertion_deassertion_event;
  u_int8_t event_reading_code_offset;
  u_int8_t event_data2_flag;
  u_int8_t event_data3_flag;
  u_int8_t event_data2;
  u_int8_t event_data3;
  
  u_int64_t val;
  
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"record_id", 
		&val);
  record_id = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"timestamp", 
		&val);
  timestamp = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"ipmb_slave_addr_sys_soft_id_flag", 
		&val);
  ipmb_slave_addr_sys_soft_id_flag = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"ipmb_slave_addr_sys_soft_id", 
		&val);
  ipmb_slave_addr_sys_soft_id = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"channel_number", 
		&val);
  channel_number = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"sensor_type", 
		&val);
  sensor_type = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"sensor_number", 
		&val);
  sensor_number = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"event_type_code", 
		&val);
  event_type_code = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"assertion_deassertion_event", 
		&val);
  assertion_deassertion_event = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"event_data2_flag", 
		&val);
  event_data2_flag = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"event_data3_flag", 
		&val);
  event_data3_flag = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"event_data2", 
		&val);
  event_data2 = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_system_event_record, 
		"event_data3", 
		&val);
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
  sel_record->event_message = ipmi_get_event_message (sensor_type, 
						      event_reading_code_offset);
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
	    sel_record->event_data2_message = 
	      ipmi_get_event_data2_message (sensor_type, 
					    event_reading_code_offset, 
					    event_data2);
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
	    sel_record->event_data3_message = 
	      ipmi_get_event_data3_message (sensor_type, 
					    event_reading_code_offset, 
					    event_data3);
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
/* 	  case IPMI_SEL_PREV_STATE_SEVERITY: */
/* 	    asprintf (&(sel_record->event_data2_message),  */
/* 		      "Previous state and/or severity = %02Xh",  */
/* 		      event_data2); */
/* 	    break; */
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data2_message),
		      "OEM code = %02Xh",
		      event_data2);
	    break;
	  case IPMI_SEL_PREV_STATE_SEVERITY:
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    sel_record->event_data2_message = 
	      ipmi_get_event_data2_message (sensor_type, 
					    event_reading_code_offset, 
					    event_data2);
	    break;
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
	    sel_record->event_data3_message = 
	      ipmi_get_event_data3_message (sensor_type, 
					    event_reading_code_offset, 
					    event_data3);
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
  
  return 0;
}

int 
get_sel_timestamped_oem_record (u_int8_t *record_data, struct sel_record *sel_record)
{
  u_int16_t record_id;
  u_int32_t timestamp;
  u_int32_t manufacturer_id;
  u_int64_t oem_defined;
  u_int64_t val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_timestamped_oem_record, 
		"record_id", 
		&val);
  record_id = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_timestamped_oem_record, 
		"timestamp", 
		&val);
  timestamp = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_timestamped_oem_record, 
		"manufacturer_id", 
		&val);
  manufacturer_id = val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_timestamped_oem_record, 
		"oem_defined", 
		&val);
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
	    "OEM Defined = " F_X64 "h", 
	    oem_defined);
  
  sel_record->event_data2_message = NULL;
  sel_record->event_data3_message = NULL;
  
  return 0;
}

int 
get_sel_non_timestamped_oem_record (u_int8_t *record_data, struct sel_record *sel_record)
{
  u_int16_t record_id;
  u_int8_t *oem_defined;
  int8_t oem_defined_index;
  u_int64_t val;
  int i;
  char *str = NULL;
  char *tmp_str = NULL;
  
  fiid_obj_get (record_data, 
		tmpl_sel_non_timestamped_oem_record, 
		"record_id", 
		&val);
  record_id = val;
  
  oem_defined_index = fiid_obj_field_start_bytes (tmpl_sel_non_timestamped_oem_record, 
						  "oem_defined");
  if (oem_defined_index == -1)
    return 1;
  oem_defined = record_data + oem_defined_index;
  
  sel_record->record_id = record_id;
  sel_record->timestamp = NULL;
  sel_record->sensor_info = NULL;
  sel_record->event_message = NULL;
  
  for (i = 0; 
       i < (fiid_obj_len_bytes (tmpl_sel_non_timestamped_oem_record) - 
	    oem_defined_index); 
       i++)
    {
      tmp_str = str;
      if (str)
	{
	  str = NULL;
	  asprintf (&str, "%s %02X", tmp_str, oem_defined[i]);
	  free (tmp_str);
	}
      else
	asprintf (&str, "%02X", oem_defined[i]);
    }
  
  if (str)
    {
      asprintf (&(sel_record->event_message), "OEM defined = %s", str);
      free (str);
    }
  
  sel_record->event_data2_message = NULL;
  sel_record->event_data3_message = NULL;
  
  return 0;
}

int 
get_sel_record (u_int8_t *record_data, struct sel_record *sel_rec)
{
  u_int8_t record_type;
  u_int64_t val;
  
  fiid_obj_get (record_data,
		tmpl_sel_record_header,
		"record_type",
		&val);
  record_type = val;
  
  switch (ipmi_get_sel_record_type (record_type))
    {
    case IPMI_SEL_SYSTEM_EVENT_RECORD:
      return get_sel_system_event_record (record_data, sel_rec);
    case IPMI_SEL_TIMESTAMPED_OEM_RECORD:
      return get_sel_timestamped_oem_record (record_data, sel_rec);
    case IPMI_SEL_NON_TIMESTAMPED_OEM_RECORD:
      return get_sel_non_timestamped_oem_record (record_data, sel_rec);
    }
  
  return -1;
}
