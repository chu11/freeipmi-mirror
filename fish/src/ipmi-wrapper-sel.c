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
  status = ipmi_kcs_get_sel_info (fi_get_sms_io_base (), obj_data_rs);
  
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
display_sel_threshold_system_event_record (u_int8_t *record_data)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t trigger_threshold_value_flag;
  u_int8_t trigger_reading_flag;
  u_int8_t trigger_reading;
  u_int8_t trigger_threshold_value;
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_threshold_value_flag", 
		&val);
  trigger_threshold_value_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_reading_flag", 
		&val);
  trigger_reading_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_reading", 
		&val);
  trigger_reading = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_threshold_value", 
		&val);
  trigger_threshold_value = val;
  
  printf ("%d: ", record_id);
  
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    printf ("%s: ", buffer);
  }
  
  printf ("%s #%d: ", ipmi_get_sensor_group (sensor_type), sensor_number);
  
  printf ("%s: ", ipmi_sensor_type_threshold_desc[event_reading_code_offset]);
  
  printf ("%s", ipmi_system_software_type_desc[ipmi_get_system_software_type (ipmb_slave_addr_sys_soft_id)]);
  
/*   switch (trigger_reading_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_TRIGGER_READING: */
/*       printf ("trigger event reading: %d, ", trigger_reading); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d, ", trigger_reading); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("sensor specific event code: %d, ", trigger_reading); */
/*       break; */
/*     } */
  
/*   switch (trigger_threshold_value_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_TRIGGER_THRESHOLD_VALUE: */
/*       printf ("trigger threshold reading: %d", trigger_threshold_value); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", trigger_threshold_value); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("sensor specific event code: %d", trigger_threshold_value); */
/*       break; */
/*     } */
  printf ("\n");
  
  return 0;
}

int 
display_sel_generic_discrete_system_event_record (u_int8_t *record_data)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t oem_code_flag;
  u_int8_t prev_discrete_event_state_severity_flag;
  u_int8_t prev_discrete_event_state;
  u_int8_t severity_event_reading_code;
  u_int8_t oem_code;
  
  u_int64_t val;
  time_t time;
  
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code_flag", 
		&val);
  oem_code_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state_severity_flag", 
		&val);
  prev_discrete_event_state_severity_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state", 
		&val);
  prev_discrete_event_state = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"severity_event_reading_code", 
		&val);
  severity_event_reading_code = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code", 
		&val);
  oem_code = val;
  
  time = timestamp;
  printf ("%d: ", record_id);
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    printf ("%s: ", buffer);
  }
  printf ("%s #%d: ", ipmi_get_sensor_group (sensor_type), sensor_number);
  
  printf ("%s: ", 
	  ipmi_event_reading_type_code_desc_ptr[event_type_code][event_reading_code_offset]);
  
  printf ("%s", ipmi_system_software_type_desc[ipmi_get_system_software_type (ipmb_slave_addr_sys_soft_id)]);

/*   switch (prev_discrete_event_state_severity_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_PREV_STATE_SEVERITY: */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     } */
  
/*   switch (oem_code_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     } */
  printf ("\n");
  
  return 0;
}

int 
display_sel_discrete_system_event_record (u_int8_t *record_data)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t oem_code_flag;
  u_int8_t prev_discrete_event_state_severity_flag;
  u_int8_t prev_discrete_event_state;
  u_int8_t severity_event_reading_code;
  u_int8_t oem_code;
  
  u_int64_t val;
  time_t time;
  
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code_flag", 
		&val);
  oem_code_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state_severity_flag", 
		&val);
  prev_discrete_event_state_severity_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state", 
		&val);
  prev_discrete_event_state = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"severity_event_reading_code", 
		&val);
  severity_event_reading_code = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code", 
		&val);
  oem_code = val;
  
  time = timestamp;
  printf ("%d: ", record_id);
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    printf ("%s: ", buffer);
  }
  printf ("%s #%d: ", ipmi_get_sensor_group (sensor_type), sensor_number);
  
  {
    struct ipmi_discrete_desc *discrete_sensor_desc;
    discrete_sensor_desc = (struct ipmi_discrete_desc *) 
      ipmi_sensor_type_desc_ptr[sensor_type];
    printf ("%s: ", 
	    discrete_sensor_desc[event_reading_code_offset].message);
  }
  
  printf ("%s", ipmi_system_software_type_desc[ipmi_get_system_software_type (ipmb_slave_addr_sys_soft_id)]);
  
/*   switch (prev_discrete_event_state_severity_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_PREV_STATE_SEVERITY: */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     } */
  
/*   switch (oem_code_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     } */
  printf ("\n");
  
  return 0;
}

int 
display_sel_oem_system_event_record (u_int8_t *record_data)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t oem_code_flag;
  u_int8_t prev_discrete_event_state_severity_flag;
  u_int8_t oem_code_bits_prev_discrete_event_state;
  u_int8_t oem_code_severity_event_reading_code;
  u_int8_t oem_code;
  
  u_int64_t val;
  time_t time;
  
  
  
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code_flag", 
		&val);
  oem_code_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"prev_discrete_event_state_severity_flag", 
		&val);
  prev_discrete_event_state_severity_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code_bits_prev_discrete_event_state", 
		&val);
  oem_code_bits_prev_discrete_event_state = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code_severity_event_reading_code", 
		&val);
  oem_code_severity_event_reading_code = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code", 
		&val);
  oem_code = val;
  
  time = timestamp;
  printf ("%d: ", record_id);
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    printf ("%s: ", buffer);
  }
  printf ("%s #%d: ", ipmi_get_sensor_group (sensor_type), sensor_number);
  printf ("\n");
  
  return 0;
}

int 
display_sel_timestamped_oem_record (u_int8_t *record_data)
{
  u_int16_t record_id;
  u_int32_t timestamp;
  u_int32_t manufacturer_id;
  u_int64_t oem_defined;
  u_int64_t val;
  time_t time;
  
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
  
  time = timestamp;
  printf ("%d: ", record_id);
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    printf ("%s: ", buffer);
  }
  printf ("%X: " F_X64 "\n", manufacturer_id, oem_defined);
  
  return 0;
}

int 
display_sel_non_timestamped_oem_record (u_int8_t *record_data)
{
  u_int16_t record_id;
  u_int8_t *oem_defined;
  int8_t oem_defined_index;
  u_int64_t val;
  int i;
  
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
  
  printf ("%d: ", record_id);
  for (i = 0; 
       i < (fiid_obj_len_bytes (tmpl_sel_non_timestamped_oem_record) - 
	    oem_defined_index); 
       i++)
    printf ("%02X ", oem_defined[i]);
  printf ("\n");
  return 0;
}

int 
display_sel_record (u_int8_t *record_data)
{
  u_int8_t record_type;
  u_int8_t event_type_code;
  u_int64_t val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_record_header, 
		"record_type", 
		&val);
  record_type = val;
  
  switch (ipmi_get_sel_record_type (record_type))
    {
    case IPMI_SEL_SYSTEM_EVENT_RECORD:
      fiid_obj_get (record_data, 
		    tmpl_sel_system_event_record, 
		    "event_type_code", 
		    &val);
      event_type_code = val;
      /* 	  printf ("event_type_code: %02X\n", event_type_code); */
      switch (ipmi_sensor_classify (event_type_code))
	{
	case IPMI_SENSOR_CLASS_THRESHOLD:
	  return display_sel_threshold_system_event_record (record_data);
	case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
	  return display_sel_generic_discrete_system_event_record (record_data);
	case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
	  return display_sel_discrete_system_event_record (record_data);
	case IPMI_SENSOR_CLASS_OEM:
	  return display_sel_oem_system_event_record (record_data);
	}
      break;
    case IPMI_SEL_TIMESTAMPED_OEM_RECORD:
      return display_sel_timestamped_oem_record (record_data);
    case IPMI_SEL_NON_TIMESTAMPED_OEM_RECORD:
      return display_sel_non_timestamped_oem_record (record_data);
    }
  return -1;
}

int 
get_sel_threshold_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t trigger_threshold_value_flag;
  u_int8_t trigger_reading_flag;
  u_int8_t trigger_reading;
  u_int8_t trigger_threshold_value;
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_threshold_value_flag", 
		&val);
  trigger_threshold_value_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_reading_flag", 
		&val);
  trigger_reading_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_reading", 
		&val);
  trigger_reading = val;
  
  fiid_obj_get (event_data, 
		tmpl_threshold_event_data, 
		"trigger_threshold_value", 
		&val);
  trigger_threshold_value = val;
  
  sel_rec->record_id = record_id;
  sel_rec->timestamp = timestamp;
  asprintf (&(sel_rec->sensor_desc), 
	    "%s #%d", 
	    ipmi_get_sensor_group (sensor_type), sensor_number);
  asprintf (&(sel_rec->event_desc), 
	    "%s", 
	    ipmi_sensor_type_threshold_desc[event_reading_code_offset]);
  asprintf (&(sel_rec->generator_id), 
	    "%s", 
	    ipmi_system_software_type_desc[ipmi_get_system_software_type (ipmb_slave_addr_sys_soft_id)]);
  
/*   switch (trigger_reading_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_TRIGGER_READING: */
/*       printf ("trigger event reading: %d, ", trigger_reading); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d, ", trigger_reading); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("sensor specific event code: %d, ", trigger_reading); */
/*       break; */
/*     } */
  
/*   switch (trigger_threshold_value_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_TRIGGER_THRESHOLD_VALUE: */
/*       printf ("trigger threshold reading: %d", trigger_threshold_value); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", trigger_threshold_value); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("sensor specific event code: %d", trigger_threshold_value); */
/*       break; */
/*     } */
  
  return 0;
}

int 
get_sel_generic_discrete_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t oem_code_flag;
  u_int8_t prev_discrete_event_state_severity_flag;
  u_int8_t prev_discrete_event_state;
  u_int8_t severity_event_reading_code;
  u_int8_t oem_code;
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code_flag", 
		&val);
  oem_code_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state_severity_flag", 
		&val);
  prev_discrete_event_state_severity_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state", 
		&val);
  prev_discrete_event_state = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"severity_event_reading_code", 
		&val);
  severity_event_reading_code = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code", 
		&val);
  oem_code = val;
  
  sel_rec->record_id = record_id;
  sel_rec->timestamp = timestamp;
  asprintf (&(sel_rec->sensor_desc), 
	    "%s #%d", 
	    ipmi_get_sensor_group (sensor_type), sensor_number);
  asprintf (&(sel_rec->event_desc), 
	    "%s", 
	    ipmi_event_reading_type_code_desc_ptr[event_type_code][event_reading_code_offset]);
  asprintf (&(sel_rec->generator_id), 
	    "%s", 
	    ipmi_system_software_type_desc[ipmi_get_system_software_type (ipmb_slave_addr_sys_soft_id)]);

/*   switch (prev_discrete_event_state_severity_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_PREV_STATE_SEVERITY: */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     } */
  
/*   switch (oem_code_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     } */
  
  return 0;
}

int 
get_sel_discrete_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t oem_code_flag;
  u_int8_t prev_discrete_event_state_severity_flag;
  u_int8_t prev_discrete_event_state;
  u_int8_t severity_event_reading_code;
  u_int8_t oem_code;
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code_flag", 
		&val);
  oem_code_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state_severity_flag", 
		&val);
  prev_discrete_event_state_severity_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"prev_discrete_event_state", 
		&val);
  prev_discrete_event_state = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"severity_event_reading_code", 
		&val);
  severity_event_reading_code = val;
  
  fiid_obj_get (event_data, 
		tmpl_discrete_event_data, 
		"oem_code", 
		&val);
  oem_code = val;
  
  sel_rec->record_id = record_id;
  sel_rec->timestamp = timestamp;
  asprintf (&(sel_rec->sensor_desc), 
	    "%s #%d", 
	    ipmi_get_sensor_group (sensor_type), sensor_number);
  {
    struct ipmi_discrete_desc *discrete_sensor_desc;
    discrete_sensor_desc = (struct ipmi_discrete_desc *) 
      ipmi_sensor_type_desc_ptr[sensor_type];
    asprintf (&(sel_rec->event_desc), 
	      "%s", 
	      discrete_sensor_desc[event_reading_code_offset].message);
  }
  asprintf (&(sel_rec->generator_id), 
	    "%s", 
	    ipmi_system_software_type_desc[ipmi_get_system_software_type (ipmb_slave_addr_sys_soft_id)]);
  
/*   switch (prev_discrete_event_state_severity_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_PREV_STATE_SEVERITY: */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("previous discrete event state: %d, ", prev_discrete_event_state); */
/*       printf ("severity event reading code: %d, ", severity_event_reading_code); */
/*       break; */
/*     } */
  
/*   switch (oem_code_flag) */
/*     { */
/*     case IPMI_SEL_UNSPECIFIED_BYTE: */
/*       break; */
/*     case IPMI_SEL_OEM_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE: */
/*       /\* i think this should not happend *\/ */
/*       printf ("oem code: %d", oem_code); */
/*       break; */
/*     } */
  
  return 0;
}

int 
get_sel_oem_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec)
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
  
  u_int8_t *event_data;
  int8_t event_data_index;
  
  u_int8_t event_reading_code_offset;
  u_int8_t oem_code_flag;
  u_int8_t prev_discrete_event_state_severity_flag;
  u_int8_t oem_code_bits_prev_discrete_event_state;
  u_int8_t oem_code_severity_event_reading_code;
  u_int8_t oem_code;
  
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
  
  event_data_index = fiid_obj_field_start_bytes (tmpl_sel_system_event_record, 
						 "event_data");
  if (event_data_index == -1)
    return 1;
  event_data = record_data + event_data_index;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"event_reading_code_offset", 
		&val);
  event_reading_code_offset = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code_flag", 
		&val);
  oem_code_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"prev_discrete_event_state_severity_flag", 
		&val);
  prev_discrete_event_state_severity_flag = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code_bits_prev_discrete_event_state", 
		&val);
  oem_code_bits_prev_discrete_event_state = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code_severity_event_reading_code", 
		&val);
  oem_code_severity_event_reading_code = val;
  
  fiid_obj_get (event_data, 
		tmpl_oem_event_data, 
		"oem_code", 
		&val);
  oem_code = val;
  
  sel_rec->record_id = record_id;
  sel_rec->timestamp = timestamp;
  asprintf (&(sel_rec->sensor_desc), 
	    "%s #%d", 
	    ipmi_get_sensor_group (sensor_type), sensor_number);
  asprintf (&(sel_rec->event_desc), "%s", "N/A");
  asprintf (&(sel_rec->generator_id), "%s", "N/A");
  
  return 0;
}

int 
get_sel_timestamped_oem_record (u_int8_t *record_data, struct sel_record *sel_rec)
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
  
  sel_rec->record_id = record_id;
  sel_rec->timestamp = timestamp;
  asprintf (&(sel_rec->sensor_desc), "%s", "N/A");
  asprintf (&(sel_rec->event_desc), 
	    "Manufacturer ID: %X, OEM defined: " F_X64, 
	    manufacturer_id, oem_defined);
  asprintf (&(sel_rec->generator_id), "%s", "N/A");
  
  return 0;
}

int 
get_sel_non_timestamped_oem_record (u_int8_t *record_data, struct sel_record *sel_rec)
{
  u_int16_t record_id;
  u_int8_t *oem_defined;
  int8_t oem_defined_index;
  u_int64_t val;
  int i;
  
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
  
  sel_rec->record_id = record_id;
  sel_rec->timestamp = 0;
  asprintf (&(sel_rec->sensor_desc), "%s", "N/A");
  {
    int len = 0;
    char *oem_defined_string = NULL;
    char element[4];
    
    len = (fiid_obj_len_bytes (tmpl_sel_non_timestamped_oem_record) - 
	   oem_defined_index);
    oem_defined_string = alloca (sizeof (char) * ((len * 3) + 1));
    oem_defined_string[0] = '\0';
    for (i = 0; i < len; i++)
      {
	snprintf (element, 4, "%02X ", oem_defined[i]);
	strcat (oem_defined_string, element);
      }
    
    asprintf (&(sel_rec->event_desc), "OEM defined: %s", oem_defined_string);
  }
  asprintf (&(sel_rec->generator_id), "%s", "N/A");
  
  return 0;
}

int 
get_sel_record (u_int8_t *record_data, struct sel_record *sel_rec)
{
  u_int8_t record_type;
  u_int8_t event_type_code;
  u_int64_t val;
  
  fiid_obj_get (record_data, 
		tmpl_sel_record_header, 
		"record_type", 
		&val);
  record_type = val;
  
  switch (ipmi_get_sel_record_type (record_type))
    {
    case IPMI_SEL_SYSTEM_EVENT_RECORD:
      fiid_obj_get (record_data, 
		    tmpl_sel_system_event_record, 
		    "event_type_code", 
		    &val);
      event_type_code = val;
      /* 	  printf ("event_type_code: %02X\n", event_type_code); */
      switch (ipmi_sensor_classify (event_type_code))
	{
	case IPMI_SENSOR_CLASS_THRESHOLD:
	  return get_sel_threshold_system_event_record (record_data, sel_rec);
	case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
	  return get_sel_generic_discrete_system_event_record (record_data, sel_rec);
	case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
	  return get_sel_discrete_system_event_record (record_data, sel_rec);
	case IPMI_SENSOR_CLASS_OEM:
	  return get_sel_oem_system_event_record (record_data, sel_rec);
	}
      break;
    case IPMI_SEL_TIMESTAMPED_OEM_RECORD:
      return get_sel_timestamped_oem_record (record_data, sel_rec);
    case IPMI_SEL_NON_TIMESTAMPED_OEM_RECORD:
      return get_sel_non_timestamped_oem_record (record_data, sel_rec);
    }
  return -1;
}

