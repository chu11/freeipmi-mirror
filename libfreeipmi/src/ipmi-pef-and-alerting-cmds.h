/* 
   ipmi-pef-and-alerting-cmds.h - IPMI Platform Event Filtering Commands 
   
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

/* $Id: ipmi-pef-and-alerting-cmds.h,v 1.1.2.1 2006-02-17 23:59:50 chu11 Exp $ */

#ifndef _IPMI_PEF_AND_ALERTING_CMDS_H
#define _IPMI_PEF_AND_ALERTING_CMDS_H

#define IPMI_PEF_POSTPONE_TIMER_DISABLE                     0x00
#define IPMI_PEF_POSTPONE_TIMER_TEMPORARY_DISABLE           0xFE
#define IPMI_PEF_POSTPONE_TIMER_GET_PRESENT_COUNTDOWN_VALUE 0xFF

#define IPMI_PEF_ENABLE  0x1
#define IPMI_PEF_DISABLE 0x0

#define IPMI_PEF_VALID(__val) \
        (((__val) == IPMI_PEF_ENABLE \
          || (__val) == IPMI_PEF_DISABLE) ? 1 : 0)

#define IPMI_PEF_ENABLE  0x1
#define IPMI_PEF_DISABLE 0x0

#define IPMI_PEF_VALID(__val) \
        (((__val) == IPMI_PEF_ENABLE \
          || (__val) == IPMI_PEF_DISABLE) ? 1 : 0)

#define IPMI_PEF_EVENT_MESSAGES_ENABLE  0x1
#define IPMI_PEF_EVENT_MESSAGES_DISABLE 0x0

#define IPMI_PEF_EVENT_MESSAGES_VALID(__val) \
        (((__val) == IPMI_PEF_EVENT_MESSAGES_ENABLE \
          || (__val) == IPMI_PEF_EVENT_MESSAGES_DISABLE) ? 1 : 0)

#define IPMI_PEF_STARTUP_DELAY_ENABLE  0x1
#define IPMI_PEF_STARTUP_DELAY_DISABLE 0x0

#define IPMI_PEF_STARTUP_DELAY_VALID(__val) \
        (((__val) == IPMI_PEF_STARTUP_DELAY_ENABLE \
          || (__val) == IPMI_PEF_STARTUP_DELAY_DISABLE) ? 1 : 0)

#define IPMI_PEF_ALERT_STARTUP_DELAY_ENABLE  0x1
#define IPMI_PEF_ALERT_STARTUP_DELAY_DISABLE 0x0

#define IPMI_PEF_ALERT_STARTUP_DELAY_VALID(__val) \
        (((__val) == IPMI_PEF_ALERT_STARTUP_DELAY_ENABLE \
          || (__val) == IPMI_PEF_ALERT_STARTUP_DELAY_DISABLE) ? 1 : 0)

#define IPMI_PEF_ALERT_ACTION_ENABLE  0x1
#define IPMI_PEF_ALERT_ACTION_DISABLE 0x0

#define IPMI_PEF_ALERT_ACTION_VALID(__val) \
        (((__val) == IPMI_PEF_ALERT_ACTION_ENABLE \
          || (__val) == IPMI_PEF_ALERT_ACTION_DISABLE) ? 1 : 0)

#define IPMI_PEF_POWER_DOWN_ACTION_ENABLE  0x1
#define IPMI_PEF_POWER_DOWN_ACTION_DISABLE 0x0

#define IPMI_PEF_POWER_DOWN_ACTION_VALID(__val) \
        (((__val) == IPMI_PEF_POWER_DOWN_ACTION_ENABLE \
          || (__val) == IPMI_PEF_POWER_DOWN_ACTION_DISABLE) ? 1 : 0)

#define IPMI_PEF_RESET_ACTION_ENABLE  0x1
#define IPMI_PEF_RESET_ACTION_DISABLE 0x0

#define IPMI_PEF_RESET_ACTION_VALID(__val) \
        (((__val) == IPMI_PEF_RESET_ACTION_ENABLE \
          || (__val) == IPMI_PEF_RESET_ACTION_DISABLE) ? 1 : 0)

#define IPMI_PEF_POWER_CYCLE_ACTION_ENABLE  0x1
#define IPMI_PEF_POWER_CYCLE_ACTION_DISABLE 0x0

#define IPMI_PEF_POWER_CYCLE_ACTION_VALID(__val) \
        (((__val) == IPMI_PEF_POWER_CYCLE_ACTION_ENABLE \
          || (__val) == IPMI_PEF_POWER_CYCLE_ACTION_DISABLE) ? 1 : 0)

#define IPMI_PEF_OEM_ACTION_ENABLE  0x1
#define IPMI_PEF_OEM_ACTION_DISABLE 0x0

#define IPMI_PEF_OEM_ACTION_VALID(__val) \
        (((__val) == IPMI_PEF_OEM_ACTION_ENABLE \
          || (__val) == IPMI_PEF_OEM_ACTION_DISABLE) ? 1 : 0)

#define IPMI_PEF_DIAGNOSTIC_INTERRUPT_ENABLE  0x1
#define IPMI_PEF_DIAGNOSTIC_INTERRUPT_DISABLE 0x0

#define IPMI_PEF_DIAGNOSTIC_INTERRUPT_VALID(__val) \
        (((__val) == IPMI_PEF_DIAGNOSTIC_INTERRUPT_ENABLE \
          || (__val) == IPMI_PEF_DIAGNOSTIC_INTERRUPT_DISABLE) ? 1 : 0)

#define IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER  0x2
#define IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER        0x0

#define IPMI_FILTER_CONFIGURATION_FILTER_TYPE_VALID(__val) \
        (((__val) == IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER \
          || (__val) == IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER) ? 1 : 0)

#define IPMI_FILTER_CONFIGURATION_FILTER_ENABLE  0x1
#define IPMI_FILTER_CONFIGURATION_FILTER_DISABLE 0x0

#define IPMI_FILTER_CONFIGURATION_FILTER_VALID(__val) \
        (((__val) == IPMI_FILTER_CONFIGURATION_FILTER_ENABLE \
          || (__val) == IPMI_FILTER_CONFIGURATION_FILTER_DISABLE) ? 1 : 0)

#define IPMI_EVENT_FILTER_ACTION_ALERT    0x1
#define IPMI_EVENT_FILTER_ACTION_NO_ALERT 0x0

#define IPMI_EVENT_FILTER_ACTION_ALERT_VALID(__val) \
        (((__val) == IPMI_EVENT_FILTER_ACTION_ALERT \
          || (__val) == IPMI_EVENT_FILTER_ACTION_NO_ALERT) ? 1 : 0)

#define IPMI_EVENT_FILTER_ACTION_POWER_OFF         0x1
#define IPMI_EVENT_FILTER_ACTION_NO_POWER_OFF      0x0

#define IPMI_EVENT_FILTER_ACTION_POWER_OFF_VALID(__val) \
        (((__val) == IPMI_EVENT_FILTER_ACTION_POWER_OFF \
          || (__val) == IPMI_EVENT_FILTER_ACTION_NO_POWER_OFF) ? 1 : 0)

#define IPMI_EVENT_FILTER_ACTION_RESET    0x1
#define IPMI_EVENT_FILTER_ACTION_NO_RESET 0x0

#define IPMI_EVENT_FILTER_ACTION_RESET_VALID(__val) \
        (((__val) == IPMI_EVENT_FILTER_ACTION_RESET \
          || (__val) == IPMI_EVENT_FILTER_ACTION_NO_RESET) ? 1 : 0)

#define IPMI_EVENT_FILTER_ACTION_POWER_CYCLE    0x1
#define IPMI_EVENT_FILTER_ACTION_NO_POWER_CYCLE 0x0

#define IPMI_EVENT_FILTER_ACTION_POWER_CYCLE_VALID(__val) \
        (((__val) == IPMI_EVENT_FILTER_ACTION_POWER_CYCLE \
          || (__val) == IPMI_EVENT_FILTER_ACTION_NO_POWER_CYCLE) ? 1 : 0)

#define IPMI_EVENT_FILTER_ACTION_OEM_ACTION  0x1
#define IPMI_EVENT_FILTER_ACTION_NO_OEM      0x0

#define IPMI_EVENT_FILTER_ACTION_OEM_VALID(__val) \
        (((__val) == IPMI_EVENT_FILTER_ACTION_OEM_ACTION \
          || (__val) == IPMI_EVENT_FILTER_ACTION_NO_OEM) ? 1 : 0)

#define IPMI_EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT    0x1
#define IPMI_EVENT_FILTER_ACTION_NO_DIAGNOSTIC_INTERRUPT 0x0

#define IPMI_EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_VALID(__val) \
        (((__val) == IPMI_EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT \
          || (__val) == IPMI_EVENT_FILTER_ACTION_NO_DIAGNOSTIC_INTERRUPT) ? 1 : 0)

#define IPMI_EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION    0x1
#define IPMI_EVENT_FILTER_ACTION_NO_GROUP_CONTROL_OPERATION 0x0

#define IPMI_EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_VALID(__val) \
        (((__val) == IPMI_EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION \
          || (__val) == IPMI_EVENT_FILTER_ACTION_NO_GROUP_CONTROL_OPERATION) ? 1 : 0)

#define IPMI_EVENT_SEVERITY_UNSPECIFIED                0x00
#define IPMI_EVENT_SEVERITY_MONITOR                    0x01
#define IPMI_EVENT_SEVERITY_INFORMATION                0x02
#define IPMI_EVENT_SEVERITY_OK                         0x04
#define IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION     0x08
#define IPMI_EVENT_SEVERITY_CRITICAL_CONDITION         0x10
#define IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION  0x20

#define IPMI_EVENT_SEVERITY_VALID(__val) \
        (((__val) == IPMI_EVENT_SEVERITY_UNSPECIFIED \
          || (__val) == IPMI_EVENT_SEVERITY_MONITOR \
          || (__val) == IPMI_EVENT_SEVERITY_INFORMATION \
          || (__val) == IPMI_EVENT_SEVERITY_OK \
          || (__val) == IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION \
          || (__val) == IPMI_EVENT_SEVERITY_CRITICAL_CONDITION \
          || (__val) == IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION) ? 1 : 0)

#define IPMI_STRING_SELECTOR_MIN 0x00
#define IPMI_STRING_SELECTOR_MAX 0x7F

/* To avoid gcc warnings, added +1 in comparison */
#define IPMI_STRING_SELECTOR_VALID(__val) \
        (((__val+1) >= (IPMI_STRING_SELECTOR_MIN + 1) \
          && (__val) <= IPMI_STRING_SELECTOR_MAX) ? 1 : 0)

#define IPMI_SET_RECORD_ID_FOR_LAST_RECORD_PROCESSED_BY_SOFTWARE 0x0
#define IPMI_SET_RECORD_ID_FOR_LAST_RECORD_PROCESSED_BY_BMC      0x1

#define IPMI_SET_RECORD_ID_FOR_LAST_RECORD_PROCESSED_VALID(__val) \
        (((__val) == IPMI_SET_RECORD_ID_FOR_LAST_RECORD_PROCESSED_BY_SOFTWARE \
          || (__val) == IPMI_SET_RECORD_ID_FOR_LAST_RECORD_PROCESSED_BY_BMC) ? 1 : 0)

#define IPMI_ALERT_IMMEDIATE_OPERATION_INITIATE_ALERT               0x0
#define IPMI_ALERT_IMMEDIATE_OPERATION_GET_ALERT_IMMEDIATE_STATUS   0x1
#define IPMI_ALERT_IMMEDIATE_OPERATION_CLEAR_ALERT_IMMEDIATE_STATUS 0x2
#define IPMI_ALERT_IMMEDIATE_OPERATION_RESERVED                     0x3

#define IPMI_ALERT_IMMEDIATE_OPERATION_VALID(__val) \
        (((__val) == IPMI_ALERT_IMMEDIATE_OPERATION_INITIATE_ALERT \
          || (__val) == IPMI_ALERT_IMMEDIATE_OPERATION_GET_ALERT_IMMEDIATE_STATUS \
          || (__val) == IPMI_ALERT_IMMEDIATE_OPERATION_CLEAR_ALERT_IMMEDIATE_STATUS) ? 1 : 0)
        
#define IPMI_SEND_ALERT_STRING_IDENTIFIED_BY_STRING_SELECTOR 0x1
#define IPMI_DO_NOT_SEND_AN_ALERT_STRING                     0x0

#define IPMI_SEND_ALERT_STRING_VALID(__val) \
        (((__val) == IPMI_SEND_ALERT_STRING_IDENTIFIED_BY_STRING_SELECTOR \
          || (__val) == IPMI_DO_NOT_SEND_AN_ALERT_STRING) ? 1 : 0)

#define IPMI_GET_PEF_PARAMETER                          0x0
#define IPMI_GET_PEF_PARAMETER_REVISION_ONLY            0x1

#define IPMI_GET_PEF_PARAMETER_VALID(__val) \
        (((__val) == IPMI_GET_PEF_PARAMETER \
          || (__val) == IPMI_GET_PEF_PARAMETER_REVISION_ONLY) ? 1 : 0)

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_get_pef_capabilities_rq;
extern fiid_template_t tmpl_get_pef_capabilities_rs;

extern fiid_template_t tmpl_arm_pef_postpone_timer_rq;
extern fiid_template_t tmpl_arm_pef_postpone_timer_rs;

extern fiid_template_t tmpl_set_pef_configuration_parameters_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_rs;
extern fiid_template_t tmpl_set_pef_configuration_parameters_pef_control_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_pef_action_global_control_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_pef_startup_delay_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_pef_alert_startup_delay_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_event_filter_table_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_event_filter_table_data1_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_alert_string_keys_rq;
extern fiid_template_t tmpl_set_pef_configuration_parameters_alert_strings_rq;

extern fiid_template_t tmpl_get_pef_configuration_parameters_rq;
extern fiid_template_t tmpl_get_pef_configuration_parameters_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_pef_control_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_pef_action_global_control_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_pef_startup_delay_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_pef_alert_startup_delay_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_number_of_event_filters_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_event_filter_table_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_event_filter_table_data1_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_number_of_alert_policy_entries_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_number_of_alert_strings_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_alert_string_keys_rs;
extern fiid_template_t tmpl_get_pef_configuration_parameters_alert_strings_rs;

extern fiid_template_t tmpl_set_last_processed_event_id_rq;
extern fiid_template_t tmpl_set_last_processed_event_id_rs;
extern fiid_template_t tmpl_get_last_processed_event_id_rq;
extern fiid_template_t tmpl_get_last_processed_event_id_rs;

extern fiid_template_t tmpl_alert_immediate_rq;
extern fiid_template_t tmpl_alert_immediate_rs;

extern fiid_template_t tmpl_pet_acknowledge_rq;
extern fiid_template_t tmpl_pet_acknowledge_rs;


int8_t fill_cmd_get_pef_capabilities (fiid_obj_t obj_data_rq);

int8_t fill_cmd_arm_pef_postpone_timer (uint8_t pef_postpone_timeout,
					fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters (fiid_obj_t obj_data_rq,
						  uint8_t parameter_selector,
						  uint8_t *configuration_parameter_data,
						  uint8_t configuration_parameter_data_len);

int8_t fill_cmd_set_pef_configuration_parameters_pef_control (uint8_t pef,
                                                              uint8_t pef_event_messages,
                                                              uint8_t pef_startup_delay,
                                                              uint8_t pef_alert_startup_delay,
                                                              fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters_pef_action_global_control (uint8_t alert_action,
                                                                            uint8_t power_down_action,
                                                                            uint8_t reset_action,
                                                                            uint8_t power_cycle_action,
                                                                            uint8_t oem_action,
                                                                            uint8_t diagnostic_interrupt,
                                                                            fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters_pef_startup_delay (uint8_t pef_startup_delay, 
                                                                    fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (uint8_t pef_alert_startup_delay, 
                                                                          fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters_event_filter_table (uint8_t filter_number,
                                                                     uint8_t filter_configuration_type,
                                                                     uint8_t filter_configuration_enable,
                                                                     uint8_t event_filter_action_alert,
                                                                     uint8_t event_filter_action_power_off,
                                                                     uint8_t event_filter_action_reset,
                                                                     uint8_t event_filter_action_power_cycle,
                                                                     uint8_t event_filter_action_oem,
                                                                     uint8_t event_filter_action_diagnostic_interrupt,
                                                                     uint8_t event_filter_action_group_control_operation,
                                                                     uint8_t alert_policy_number_policy_number,
                                                                     uint8_t alert_policy_number_group_control_selector,
                                                                     uint8_t event_severity,
                                                                     uint8_t generator_id_byte1,
                                                                     uint8_t generator_id_byte2,
                                                                     uint8_t sensor_type,
                                                                     uint8_t sensor_number,
                                                                     uint8_t event_trigger,
                                                                     uint16_t event_data1_offset_mask,
                                                                     uint8_t event_data1_AND_mask,
                                                                     uint8_t event_data1_compare1,
                                                                     uint8_t event_data1_compare2,
                                                                     uint8_t event_data2_AND_mask,
                                                                     uint8_t event_data2_compare1,
                                                                     uint8_t event_data2_compare2,
                                                                     uint8_t event_data3_AND_mask,
                                                                     uint8_t event_data3_compare1,
                                                                     uint8_t event_data3_compare2,
                                                                     fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters_event_filter_table_data1 (uint8_t filter_number,
                                                                           uint8_t filter_configuration_type,
                                                                           uint8_t filter_configuration_enable,
                                                                           fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters_alert_string_keys (uint8_t string_selector,
                                                                    uint8_t filter_number,
                                                                    uint8_t set_number_for_string,
                                                                    fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_pef_configuration_parameters_alert_strings (uint8_t string_selector,
                                                                uint8_t block_selector,
                                                                uint8_t *string_data,
                                                                uint32_t string_data_len,
                                                                fiid_obj_t obj_data_rq);

int8_t fill_cmd_get_pef_configuration_parameters (uint8_t parameter_selector,
						  uint8_t get_parameter,
						  uint8_t set_selector,
						  uint8_t block_selector,
						  fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_last_processed_event_id (uint8_t set_record_id_for_last_record,
                                             uint16_t record_id,
                                             fiid_obj_t obj_data_rq);

int8_t fill_cmd_get_last_processed_event_id (fiid_obj_t obj_data_rq);

int8_t fill_cmd_alert_immediate (uint8_t channel_number,
                                 uint8_t destination_selector,
                                 uint8_t operation,
                                 uint8_t string_selector,
                                 uint8_t send_alert_string,
                                 fiid_obj_t obj_data_rq);

int8_t fill_cmd_pet_acknowledge (uint16_t sequence_number,
                                 uint32_t local_timestamp,
                                 uint8_t event_source_type,
                                 uint8_t sensor_device,
                                 uint8_t sensor_number,
                                 uint32_t event_data,
                                 fiid_obj_t obj_data_rq);

#ifdef __cplusplus
}
#endif

#endif
