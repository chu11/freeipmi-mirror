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
ipmi_get_sel_record_type (u_int8_t record_type)
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

int 
ipmi_sel_get_first_entry (sel_descriptor_t *seld, 
			  u_int8_t *record_data)
{
  fiid_obj_t obj_data_rs;
  int status;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sel_entry_rs));
  status = ipmi_kcs_get_sel_entry (IPMI_SEL_FIRST_ENTRY, 
				   obj_data_rs);
  
  if (status != 0)
    {
      fprintf (stderr, 
	       "error: ipmi_kcs_get_sel_entry() failed.\n");
      return (-1);
    }
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_kcs_get_sel_entry() failed with %s\n", 
	       err_msg);
      return (-1);
    }
  
  seld->first_record_id = IPMI_SEL_FIRST_ENTRY;
  fiid_obj_get (obj_data_rs, 
		tmpl_get_sel_entry_rs, 
		"next_record_id", 
		&val);
  seld->next_record_id = val;
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_sel_entry_rs, 
		     "record_data", 
		     record_data);
  
/*   { */
/*     int i; */
/*     for (i = 0; i < fiid_obj_len_bytes (tmpl_get_sel_entry_rs) - record_data_index; i++) */
/*       printf ("%02X ", record_data[i]); */
/*     printf ("\n"); */
/*   } */
  return 0;
}

int 
ipmi_sel_get_next_entry (sel_descriptor_t *seld, 
			 u_int8_t *record_data)
{
  fiid_obj_t obj_data_rs;
  int status;
  u_int64_t val;
  
  if (seld->next_record_id == IPMI_SEL_LAST_ENTRY)
    return -1;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sel_entry_rs));
  status = ipmi_kcs_get_sel_entry (seld->next_record_id, 
				   obj_data_rs);
  
  if (status != 0)
    {
      fprintf (stderr, 
	       "error: ipmi_kcs_get_sel_entry() failed.\n");
      return (-1);
    }
  
  if (IPMI_COMP_CODE(obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_kcs_get_sel_entry() failed with %s\n", 
	       err_msg);
      return (-1);
    }
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_sel_entry_rs, 
		"next_record_id", 
		&val);
  seld->next_record_id = val;
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_sel_entry_rs, 
		     "record_data", 
		     record_data);
  
/*   { */
/*     int i; */
/*     for (i = 0; i < fiid_obj_len_bytes (tmpl_get_sel_entry_rs) - record_data_index; i++) */
/*       printf ("%02X ", record_data[i]); */
/*     printf ("\n"); */
/*   } */
  return 0;
}
