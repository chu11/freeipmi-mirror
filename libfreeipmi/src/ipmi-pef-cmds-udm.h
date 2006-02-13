/* 
   ipmi-pef-cmds-udm.h - IPMI UDM Platform Event Filtering Commands 
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

/* $Id: ipmi-pef-cmds-udm.h,v 1.1.4.2 2006-02-13 22:21:17 chu11 Exp $ */

#ifndef _IPMI_PEF_CMDS_UDM_H
#define _IPMI_PEF_CMDS_UDM_H

#ifdef __cplusplus
extern "C" {
#endif

int8_t ipmi_cmd_set_pef_control2 (ipmi_device_t *dev, 
				  uint8_t enable_pef, 
				  uint8_t enable_pef_event_msgs, 
				  uint8_t enable_startup_delay, 
				  uint8_t enable_alert_startup_delay, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_global_action_control2 (ipmi_device_t *dev, 
					    uint8_t enable_alert,
					    uint8_t enable_powerdown, 
					    uint8_t enable_reset,
					    uint8_t enable_powercycle, 
					    uint8_t enable_oem,
					    uint8_t enable_diag_interrupt, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_startup_delay2 (ipmi_device_t *dev, 
				    uint8_t startup_delay, 
				    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_alert_startup_delay2 (ipmi_device_t *dev, 
					  uint8_t alert_startup_delay, 
					  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_num_event_filters2 (ipmi_device_t *dev, 
					uint8_t num_event_filters, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_filter_table_entry2 (ipmi_device_t *dev, 
					 const event_filter_table_entry_t *eft_entry, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_filter_table_data1_2 (ipmi_device_t *dev, 
					  uint8_t filter_number,
					  filter_type_t filter_type, 
					  uint8_t enabled, 
					  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_num_alert_policies2 (ipmi_device_t *dev, 
					 uint8_t num_alert_policies, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_alert_immediate2 (ipmi_device_t *dev,
				  uint8_t channel_number, 
				  uint8_t destination_selector,
				  uint8_t string_selector, 
				  uint8_t string_enable, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_alert_string2 (ipmi_device_t *dev,
				       uint8_t parameter_type, 
				       uint8_t set_selector,
				       uint8_t block_selector, 
				       fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_alert_string_keys2 (ipmi_device_t *dev,
					    uint8_t parameter_type, 
					    uint8_t set_selector,
					    uint8_t block_selector, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_num_alert_policies2 (ipmi_device_t *dev,
					     uint8_t parameter_type, 
					     uint8_t set_selector,
					     uint8_t block_selector, 
					     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_num_alert_strings2 (ipmi_device_t *dev, 
					    uint8_t parameter_type, 
					    uint8_t set_selector, 
					    uint8_t block_selector, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_filter_data1_2 (ipmi_device_t *dev, 
					uint8_t parameter_type, 
					uint8_t set_selector, 
					uint8_t block_selector, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_control2 (ipmi_device_t *dev, 
				  uint8_t parameter_type, 
				  uint8_t set_selector, 
				  uint8_t block_selector, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_global_action_control2 (ipmi_device_t *dev, 
						uint8_t parameter_type, 
						uint8_t set_selector,
						uint8_t block_selector, 
						fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_startup_delay2 (ipmi_device_t *dev, 
					uint8_t parameter_type, 
					uint8_t set_selector,
					uint8_t block_selector, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_alert_startup_delay2 (ipmi_device_t *dev, 
					      uint8_t parameter_type, 
					      uint8_t set_selector,
					      uint8_t block_selector, 
					      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_num_event_filters2 (ipmi_device_t *dev, 
					    uint8_t parameter_type, 
					    uint8_t set_selector,
					    uint8_t block_selector, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_filter_table_entry2 (ipmi_device_t *dev, 
					     uint8_t parameter_type, 
					     uint8_t set_selector,
					     uint8_t block_selector, 
					     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_caps2 (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_arm_pef_postpone_timer2 (ipmi_device_t *dev, 
					 uint8_t countdown, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_last_processed_event2 (ipmi_device_t *dev, 
					   which_event_t which, 
					   uint16_t id, 
					   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_last_processed_event2 (ipmi_device_t *dev, 
					   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_pet_ack2 (ipmi_device_t *dev, 
			  uint16_t sequence_number, 
			  uint32_t timestamp, 
			  uint8_t source_type, 
			  uint8_t sensor_device, 
			  uint8_t sensor_number, 
			  uint32_t event_data, 
			  fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif

#endif
