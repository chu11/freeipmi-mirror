/* 
   ipmi-sel-record-types.c - IPMI System Event Log Record Type Definitions
   
   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include "freeipmi.h"

fiid_template_t tmpl_sel_record_header = 
  {
    {16, "record_id"}, 
    {8,  "record_type"}, 
    {0, ""}
  };

fiid_template_t tmpl_sel_system_event_record = 
  {
    {16, "record_id"}, 
    {8,  "record_type"}, 
    {32, "timestamp"}, 
    
    /* Generator ID */
    {1, "ipmb_slave_addr_sys_soft_id_flag"}, 
    {7, "ipmb_slave_addr_sys_soft_id"}, 
    {2, "ipmb_device_lun"}, 
    {2, "reserved"}, 
    {4, "channel_number"}, 
    
    {8, "event_msg_format_version"}, 
    {8, "sensor_type"}, 
    {8, "sensor_number"}, 
    
    /* Event Dir | Event Type */
    {7, "event_type_code"}, 
    {1, "assertion_deassertion_event"}, 
    
    //Event Data 1
    {4, "event_reading_code_offset"}, 
    {2, "event_data3_flag"}, 
    {2, "event_data2_flag"}, 
    
    //Event Data 2
    {8, "event_data2"}, 
    
    //Event Data 3
    {8, "event_data3"}, 
    
    {0, ""}
  };


fiid_template_t tmpl_sel_timestamped_oem_record = 
  {
    {16, "record_id"}, 
    {8,  "record_type"}, 
    {32, "timestamp"}, 
    {24, "manufacturer_id"}, 
    {48, "oem_defined"}, 
    {0, ""}
  };

fiid_template_t tmpl_sel_non_timestamped_oem_record = 
  {
    {16, "record_id"}, 
    {8,  "record_type"}, 
    {104, "oem_defined"}, 
    {0, ""}
  };

int 
ipmi_get_sel_record_type (uint8_t record_type)
{
  if (record_type == 0x02)
    return IPMI_SEL_SYSTEM_EVENT_RECORD;
  
  if ((record_type >= 0xC0) && (record_type <= 0xDF))
    return IPMI_SEL_TIMESTAMPED_OEM_RECORD;
  
  /* To avoid "warning: comparison is always true due to limited range of data type" */
  if ((record_type >= 0xE0) && ((record_type - 1) <= 0xFE))
    return IPMI_SEL_NON_TIMESTAMPED_OEM_RECORD;
  
  return IPMI_SEL_UNKNOWN_RECORD;
}

static int 
get_sel_system_event_record (uint8_t *record_data, sel_record_t *sel_record)
{
  uint16_t record_id;
  uint32_t timestamp;
  uint8_t ipmb_slave_addr_sys_soft_id_flag;
  uint8_t ipmb_slave_addr_sys_soft_id;
  uint8_t channel_number;
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_type_code;
  uint8_t assertion_deassertion_event;
  uint8_t event_reading_code_offset;
  uint8_t event_data2_flag;
  uint8_t event_data3_flag;
  uint8_t event_data2;
  uint8_t event_data3;
  
  uint64_t val;
  
  
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

static int 
get_sel_timestamped_oem_record (uint8_t *record_data, sel_record_t *sel_record)
{
  uint16_t record_id;
  uint32_t timestamp;
  uint32_t manufacturer_id;
  uint64_t oem_defined;
  uint64_t val;
  
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
	    "OEM Defined = %lXh", 
	    oem_defined);
  
  sel_record->event_data2_message = NULL;
  sel_record->event_data3_message = NULL;
  
  return 0;
}

static int 
get_sel_non_timestamped_oem_record (uint8_t *record_data, sel_record_t *sel_record)
{
  uint16_t record_id;
  uint8_t *oem_defined;
  int8_t oem_defined_index;
  uint64_t val;
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
get_sel_record (uint8_t *record_data, sel_record_t *sel_record)
{
  uint8_t record_type;
  uint64_t val;
  
  fiid_obj_get (record_data,
		tmpl_sel_record_header,
		"record_type",
		&val);
  record_type = val;

  switch (ipmi_get_sel_record_type (record_type))
    {
    case IPMI_SEL_SYSTEM_EVENT_RECORD:
      return get_sel_system_event_record (record_data, sel_record);
    case IPMI_SEL_TIMESTAMPED_OEM_RECORD:
      return get_sel_timestamped_oem_record (record_data, sel_record);
    case IPMI_SEL_NON_TIMESTAMPED_OEM_RECORD:
      return get_sel_non_timestamped_oem_record (record_data, sel_record);
    }
  
  return -1;
}

