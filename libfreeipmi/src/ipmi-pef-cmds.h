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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

/* $Id: ipmi-pef-cmds.h,v 1.9 2005-10-06 10:41:10 balamurugan Exp $ */

#ifndef _IPMI_PEF_CMDS_H
#define _IPMI_PEF_CMDS_H

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

enum event_filter_type
  {
    software_filter = 0,
    factory_filter = 2,
  };
typedef enum event_filter_type event_filter_type_t;

struct event_filter_table_entry
{
  event_filter_type_t filter_type;
  /* yes or no */
  u_int8_t enabled;
  /* actions flags */
  u_int8_t alert;
  u_int8_t poweroff;
  u_int8_t reset;
  u_int8_t powercycle;
  u_int8_t oem;
  u_int8_t diag_interrupt;

  u_int8_t policy_number;

  u_int8_t severity;
  u_int8_t id;
  u_int8_t channel;
  u_int8_t sensor_type;
  u_int8_t sensor_number;
  u_int8_t event_trigger;
  u_int16_t data_offset_mask;
  u_int8_t data1_and_mask;
  u_int8_t data1_compare1_mask;
  u_int8_t data1_compare2_mask;
  u_int8_t data2_and_mask;
  u_int8_t data2_compare1_mask;
  u_int8_t data2_compare2_mask;
  u_int8_t data3_and_mask;
  u_int8_t data3_compare1_mask;
  u_int8_t data3_compare2_mask;
};
typedef struct event_filter_table_entry event_filter_table_entry_t;

int8_t ipmi_kcs_get_pef_caps (u_int16_t sms_io_base, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_arm_pef_postpone_timer (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t countdown);
int8_t ipmi_kcs_set_last_processed_event (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                          which_event_t which, u_int16_t id);
int8_t ipmi_kcs_get_last_processed_event (u_int16_t sms_io_base, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_pet_ack (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int16_t sequence_number,
                     u_int32_t timestamp, u_int8_t source_type, u_int8_t sensor_device,
                     u_int8_t sensor_number, u_int32_t event_data);
int8_t ipmi_kcs_get_pef_control (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                 u_int8_t parameter_type, u_int8_t set_selector, u_int8_t block_selector);
int8_t ipmi_kcs_get_pef_global_action_control (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                               u_int8_t parameter_type, u_int8_t set_selector,
                                               u_int8_t block_selector);
int8_t ipmi_kcs_get_pef_startup_delay (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                       u_int8_t parameter_type, u_int8_t set_selector,
                                       u_int8_t block_selector);
int8_t ipmi_kcs_get_pef_alert_startup_delay (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                             u_int8_t parameter_type, u_int8_t set_selector,
                                             u_int8_t block_selector);
int8_t ipmi_kcs_get_pef_num_event_filters (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                           u_int8_t parameter_type, u_int8_t set_selector,
                                           u_int8_t block_selector);
int8_t ipmi_kcs_get_pef_filter_table_entry (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                            u_int8_t parameter_type, u_int8_t set_selector,
                                            u_int8_t block_selector);
int8_t
ipmi_kcs_get_pef_filter_data1 (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                               u_int8_t parameter_type, u_int8_t set_selector, u_int8_t block_selector);
int8_t
ipmi_kcs_get_pef_num_alert_policies (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                     u_int8_t parameter_type, u_int8_t set_selector,
                                     u_int8_t block_selector);
int8_t
ipmi_kcs_get_pef_alert_string_keys (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                    u_int8_t parameter_type, u_int8_t set_selector,
                                    u_int8_t block_selector);
int8_t
ipmi_kcs_get_pef_alert_string (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                               u_int8_t parameter_type, u_int8_t set_selector,
                               u_int8_t block_selector);
int8_t
ipmi_kcs_alert_immediate (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                          u_int8_t channel_number, u_int8_t destination_selector,
                          u_int8_t string_selector, u_int8_t string_enable);

int8_t
ipmi_kcs_set_pef_control (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t enable_pef,
                          u_int8_t enable_pef_event_msgs, u_int8_t enable_startup_delay,
                          u_int8_t enable_alert_startup_delay);
int8_t
ipmi_kcs_set_global_action_control (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t enable_alert,
                                    u_int8_t enable_powerdown, u_int8_t enable_reset,
                                    u_int8_t enable_powercycle, u_int8_t enable_oem,
                                    u_int8_t enable_diag_interrupt);
int8_t
ipmi_kcs_set_startup_delay (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t startup_delay);

int8_t
ipmi_kcs_set_alert_startup_delay (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t alert_startup_delay);

int8_t
ipmi_kcs_set_num_event_filters (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t num_event_filters);

int8_t
ipmi_kcs_set_filter_table_entry (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t filter_number,
                                 const event_filter_table_entry_t* entry);

int8_t
ipmi_kcs_set_filter_table_data1 (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t filter_number,
                                 event_filter_type_t filter_type, u_int8_t enabled);

int8_t
ipmi_kcs_set_num_alert_policies (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t num_alert_policies);


int8_t ipmi_cmd_set_pef_control2 (ipmi_device_t *dev, 
				  u_int8_t enable_pef, 
				  u_int8_t enable_pef_event_msgs, 
				  u_int8_t enable_startup_delay, 
				  u_int8_t enable_alert_startup_delay, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_global_action_control2 (ipmi_device_t *dev, 
					    u_int8_t enable_alert,
					    u_int8_t enable_powerdown, 
					    u_int8_t enable_reset,
					    u_int8_t enable_powercycle, 
					    u_int8_t enable_oem,
					    u_int8_t enable_diag_interrupt, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_startup_delay2 (ipmi_device_t *dev, 
				    u_int8_t startup_delay, 
				    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_alert_startup_delay2 (ipmi_device_t *dev, 
					  u_int8_t alert_startup_delay, 
					  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_num_event_filters2 (ipmi_device_t *dev, 
					u_int8_t num_event_filters, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_filter_table_entry2 (ipmi_device_t *dev, 
					 u_int8_t filter_number,
					 const event_filter_table_entry_t *entry, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_filter_table_data1_2 (ipmi_device_t *dev, 
					  u_int8_t filter_number,
					  event_filter_type_t filter_type, 
					  u_int8_t enabled, 
					  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_num_alert_policies2 (ipmi_device_t *dev, 
					 u_int8_t num_alert_policies, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_alert_immediate2 (ipmi_device_t *dev,
				  u_int8_t channel_number, 
				  u_int8_t destination_selector,
				  u_int8_t string_selector, 
				  u_int8_t string_enable, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_alert_string2 (ipmi_device_t *dev,
				       u_int8_t parameter_type, 
				       u_int8_t set_selector,
				       u_int8_t block_selector, 
				       fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_alert_string_keys2 (ipmi_device_t *dev,
					    u_int8_t parameter_type, 
					    u_int8_t set_selector,
					    u_int8_t block_selector, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_num_alert_policies2 (ipmi_device_t *dev,
					     u_int8_t parameter_type, 
					     u_int8_t set_selector,
					     u_int8_t block_selector, 
					     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_filter_data1_2 (ipmi_device_t *dev, 
					u_int8_t parameter_type, 
					u_int8_t set_selector, 
					u_int8_t block_selector, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_control2 (ipmi_device_t *dev, 
				  u_int8_t parameter_type, 
				  u_int8_t set_selector, 
				  u_int8_t block_selector, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_global_action_control2 (ipmi_device_t *dev, 
						u_int8_t parameter_type, 
						u_int8_t set_selector,
						u_int8_t block_selector, 
						fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_startup_delay2 (ipmi_device_t *dev, 
					u_int8_t parameter_type, 
					u_int8_t set_selector,
					u_int8_t block_selector, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_alert_startup_delay2 (ipmi_device_t *dev, 
					      u_int8_t parameter_type, 
					      u_int8_t set_selector,
					      u_int8_t block_selector, 
					      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_num_event_filters2 (ipmi_device_t *dev, 
					    u_int8_t parameter_type, 
					    u_int8_t set_selector,
					    u_int8_t block_selector, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_filter_table_entry2 (ipmi_device_t *dev, 
					     u_int8_t parameter_type, 
					     u_int8_t set_selector,
					     u_int8_t block_selector, 
					     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_pef_caps2 (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_arm_pef_postpone_timer2 (ipmi_device_t *dev, 
					 u_int8_t countdown, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_last_processed_event2 (ipmi_device_t *dev, 
					   which_event_t which, 
					   u_int16_t id, 
					   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_last_processed_event2 (ipmi_device_t *dev, 
					   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_pet_ack2 (ipmi_device_t *dev, 
			  u_int16_t sequence_number, 
			  u_int32_t timestamp, 
			  u_int8_t source_type, 
			  u_int8_t sensor_device, 
			  u_int8_t sensor_number, 
			  u_int32_t event_data, 
			  fiid_obj_t obj_cmd_rs);



#ifdef __cplusplus
}
#endif

#endif
