/* 
   ipmi-pef-cmds.h - IPMI Platform Event Filtering Commands 
   
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

/* $Id: ipmi-pef-cmds.h,v 1.11.2.3 2006-02-13 23:54:48 chu11 Exp $ */

#ifndef _IPMI_PEF_CMDS_H
#define _IPMI_PEF_CMDS_H

#define IPMI_GET_PEF_PARAMETER                          0x0
#define IPMI_GET_PEF_PARAMETER_REVISION_ONLY            0x1

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_get_pef_caps_rq;
extern fiid_template_t tmpl_get_pef_caps_rs;

extern fiid_template_t tmpl_arm_pef_postpone_timer_rq;
extern fiid_template_t tmpl_arm_pef_postpone_timer_rs;

extern fiid_template_t tmpl_set_last_processed_event_rq;
extern fiid_template_t tmpl_set_last_processed_event_rs;

extern fiid_template_t tmpl_get_last_processed_event_rq;
extern fiid_template_t tmpl_get_last_processed_event_rs;

extern fiid_template_t tmpl_pet_ack_rq;
extern fiid_template_t tmpl_pet_ack_rs;

extern fiid_template_t tmpl_get_pef_conf_param_rq;
extern fiid_template_t tmpl_get_pef_conf_param_pef_control_rs;
extern fiid_template_t tmpl_get_pef_conf_param_global_action_control_rs;
extern fiid_template_t tmpl_get_pef_conf_param_startup_delay_rs;
extern fiid_template_t tmpl_get_pef_conf_param_alert_startup_delay_rs;
extern fiid_template_t tmpl_get_pef_conf_param_num_event_filters_rs;
extern fiid_template_t tmpl_get_pef_conf_param_event_filter_table_rs;
extern fiid_template_t tmpl_get_pef_conf_param_event_filter_data1_rs;
extern fiid_template_t tmpl_get_pef_conf_param_num_alert_policies_rs;
extern fiid_template_t tmpl_get_pef_conf_param_num_alert_strings_rs;
extern fiid_template_t tmpl_get_pef_conf_param_alert_string_keys_rs;
extern fiid_template_t tmpl_get_pef_conf_param_alert_strings_rs;

extern fiid_template_t tmpl_set_pef_conf_param_rs;
extern fiid_template_t tmpl_set_pef_conf_param_pef_control_rq;
extern fiid_template_t tmpl_set_pef_conf_param_global_action_control_rq;
extern fiid_template_t tmpl_set_pef_conf_param_startup_delay_rq;
extern fiid_template_t tmpl_set_pef_conf_param_alert_startup_delay_rq;
extern fiid_template_t tmpl_set_pef_conf_param_num_event_filters_rq;
extern fiid_template_t tmpl_set_pef_conf_param_event_filter_table_rq;
extern fiid_template_t tmpl_set_pef_conf_param_event_filter_data1_rq;
extern fiid_template_t tmpl_set_pef_conf_param_num_alert_policies_rq;
extern fiid_template_t tmpl_set_pef_conf_param_alert_string_keys_rq;

extern fiid_template_t tmpl_alert_immediate_rq;
extern fiid_template_t tmpl_alert_immediate_rs;

enum which_event
  {
    last_software_event = 0,
    last_bmc_event = 1,
  };
typedef enum which_event which_event_t;

enum filter_type 
  {
    IPMI_PEF_SOFTWARE_FILTER = 0x0,
    IPMI_PEF_FACTORY_FILTER  = 0x2
  };
typedef enum filter_type filter_type_t;

struct event_filter_table_entry
{
  uint8_t      filter_number;
  filter_type_t filter_type;
  uint8_t      filter_enable;
  uint8_t      event_filter_action_alert;
  uint8_t      event_filter_action_poweroff;
  uint8_t      event_filter_action_reset;
  uint8_t      event_filter_action_powercycle;
  uint8_t      event_filter_action_oem;
  uint8_t      event_filter_action_diag_interrupt;
  uint8_t      event_filter_action_group_control_operation;
  uint8_t      policy_number;
  uint8_t      group_control_selector;
  uint8_t      event_severity;
  uint8_t      generator_id_byte1;
  uint8_t      generator_id_byte2;
  uint8_t      sensor_type;
  uint8_t      sensor_number;
  uint8_t      event_reading_type;
  uint16_t     event_data1_offset_mask;
  uint8_t      event_data1_AND_mask;
  uint8_t      event_data1_compare1;
  uint8_t      event_data1_compare2;
  uint8_t      event_data2_AND_mask;
  uint8_t      event_data2_compare1;
  uint8_t      event_data2_compare2;
  uint8_t      event_data3_AND_mask;
  uint8_t      event_data3_compare1;
  uint8_t      event_data3_compare2;
};
typedef struct event_filter_table_entry event_filter_table_entry_t;

int8_t fill_kcs_alert_immediate (uint8_t channel_number,
                                 uint8_t destination_selector,
                                 uint8_t string_selector,
                                 uint8_t string_enable,
                                 fiid_obj_t obj_data_rq);
int8_t fill_kcs_get_pef_conf_param (uint8_t parameter_selector,
                                    uint8_t parameter_type,
                                    uint8_t set_selector,
                                    uint8_t block_selector,
                                    fiid_obj_t obj_data_rq);

int8_t fill_kcs_set_pef_control (uint8_t enable_pef,
                                 uint8_t enable_pef_event_msgs,
                                 uint8_t enable_startup_delay,
                                 uint8_t enable_alert_startup_delay,
                                 fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_global_action_control (uint8_t enable_alert,
                                           uint8_t enable_powerdown,
                                           uint8_t enable_reset,
                                           uint8_t enable_powercycle,
                                           uint8_t enable_oem,
                                           uint8_t enable_diag_interrupt,
                                           fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_startup_delay (uint8_t startup_delay, fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_alert_startup_delay (uint8_t alert_startup_delay, fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_num_event_filters (uint8_t num_event_filters, fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_filter_table_entry (const event_filter_table_entry_t *eft_entry,
                                        fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_filter_table_data1 (uint8_t filter_number,
                                        filter_type_t filter_type,
                                        uint8_t enabled,
                                        fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_num_alert_policies (uint8_t num_alert_policies,
                                        fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_alert_string_keys (uint8_t string_selector,
                                       uint8_t filter_number,
                                       uint8_t string_set_number,
                                       fiid_obj_t obj_data_rq);
int8_t fill_kcs_get_pef_caps (fiid_obj_t obj_data_rq);
int8_t fill_kcs_arm_pef_postpone_timer (uint8_t countdown, fiid_obj_t obj_data_rq);
int8_t fill_kcs_set_last_processed_event (which_event_t which, uint16_t id, fiid_obj_t obj_data_rq);
int8_t fill_kcs_get_last_proessed_event (fiid_obj_t obj_data_rq);
int8_t fill_kcs_pet_ack (uint16_t sequence_number,
                         uint32_t timestamp,
                         uint8_t source_type,
                         uint8_t sensor_device,
                         uint8_t sensor_number,
                         uint32_t event_data,
                         fiid_obj_t obj_data_rq);

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
