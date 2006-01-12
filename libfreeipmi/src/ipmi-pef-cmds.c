/*   ipmi-pef-cmds.c - IPMI Platform Event Filtering Commands 
   
  Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

$Id: ipmi-pef-cmds.c,v 1.13.2.3 2006-01-12 22:02:43 chu11 Exp $  */

#include "freeipmi.h"

fiid_template_t tmpl_get_pef_caps_rq =
  {
    {8, "cmd"}, 

    {0, ""}
  };

fiid_template_t tmpl_get_pef_caps_rs =
  {
    {8,  "cmd"}, 

    {8,  "comp_code"}, 

    {4,  "pef_version_major"}, 
    {4,  "pef_version_minor"}, 

    {1,  "action_support.alert"}, 
    {1,  "action_support.powerdown"}, 
    {1,  "action_support.reset"}, 
    {1,  "action_support.powercycle"},
    {1,  "action_support.oem"},
    {1,  "action_support.diag_interrupt"},
    {2,  "reserved"}, 

    {8,  "number_of_eft_entries"}, 

    {0,  ""}
  };

fiid_template_t tmpl_arm_pef_postpone_timer_rq =
  {
    {8, "cmd"},

    {8, "arm_pef_postpone_timer_countdown"},

    {0, ""}
  };

fiid_template_t tmpl_arm_pef_postpone_timer_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {8, "arm_pef_postpone_timer_countdown"},

    {0, ""}
  };

fiid_template_t tmpl_set_last_processed_event_rq =
  {
    {8, "cmd"},

    {1, "set_last_processed_event_which"},
    {7, "reserved"},

    {16, "set_last_processed_event_id"},

    {0, ""}
  };

fiid_template_t tmpl_set_last_processed_event_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {0, ""}
  };

fiid_template_t tmpl_get_last_processed_event_rq =
  {
    {8, "cmd"},

    {0, ""}
  };

fiid_template_t tmpl_get_last_processed_event_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {32, "get_last_processed_event_timestamp"},

    {16, "get_last_processed_event_sel_id"},

    {16, "get_last_processed_event_software_id"},

    {16, "get_last_processed_event_bmc_id"},

    {0, ""}
  };

fiid_template_t tmpl_pet_ack_rq =
  {
    {8, "cmd"},

    {16, "pet_ack_sequence_number"},

    {32, "pet_ack_timestamp"},

    {8, "pet_ack_source_type"},

    {8, "pet_ack_sensor_device"},

    {8, "pet_ack_sensor_number"},

    {24, "pet_ack_event_data"},

    {0, ""}
  };

fiid_template_t tmpl_pet_ack_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "parameter_type"},

    {8, "set_selector"},

    {8, "block_selector"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_pef_control_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "reserved"},

    {1, "enable_pef"},
    {1, "enable_pef_event_msgs"},
    {1, "enable_startup_delay"},
    {1, "enable_alert_startup_delay"},
    {4, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_pef_control_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {1, "enable_pef"},
    {1, "enable_pef_event_msgs"},
    {1, "enable_startup_delay"},
    {1, "enable_alert_startup_delay"},
    {4, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_global_action_control_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "reserved"},

    {1, "enable_alert_action"},
    {1, "enable_powerdown_action"},
    {1, "enable_reset_action"},
    {1, "enable_powercycle_action"},
    {1, "enable_oem_action"},
    {1, "enable_diag_interrupt"},
    {2, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_global_action_control_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {1, "enable_alert_action"},
    {1, "enable_powerdown_action"},
    {1, "enable_reset_action"},
    {1, "enable_powercycle_action"},
    {1, "enable_oem_action"},
    {1, "enable_diag_interrupt"},
    {2, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_startup_delay_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "reserved"},

    {8, "startup_delay"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_startup_delay_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {8, "pef_startup_delay"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_alert_startup_delay_rq =
  {
    {8, "cmd"},
    
    {7, "parameter_selector"},
    {1, "reserved"},

    {8, "alert_startup_delay"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_alert_startup_delay_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {8, "pef_alert_startup_delay"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_num_event_filters_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "reserved"},

    {7, "num_event_filters"},
    {1, "reserved"},

    {0, ""}
  };
    
fiid_template_t tmpl_get_pef_conf_param_num_event_filters_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {7, "num_event_filters"},
    {1, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_event_filter_table_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "parameter_selector.reserved"},

    {7, "filter_number"},
    {1, "filter_number.reserved"},

    {5, "filter_conf.reserved"},
    {2, "filter_type"},
    {1, "filter_enable"},

    {1, "filter_action_alert"},
    {1, "filter_action_poweroff"},
    {1, "filter_action_reset"},
    {1, "filter_action_powercycle"},
    {1, "filter_action_oem"},
    {1, "filter_action_diag_interrupt"},
    {1, "filter_action_group_control_operation"},
    {1, "filter_action.reserved"},

    {4, "policy_number"},
    {3, "group_control_selector"},
    {1, "alert_policy_number.reserved"},

    {8, "event_severity"},

    {8, "generator_id_byte1"},

    {8, "generator_id_byte2"},

    {8, "sensor_type"},

    {8, "sensor_number"},

    {8, "event_reading_type"},

    {16, "event_data1_offset_mask"},

    {8, "event_data1_AND_mask"},

    {8, "event_data1_compare1"},

    {8, "event_data1_compare2"},

    {8, "event_data2_AND_mask"},

    {8, "event_data2_compare1"},

    {8, "event_data2_compare2"},

    {8, "event_data3_AND_mask"},

    {8, "event_data3_compare1"},

    {8, "event_data3_compare2"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_event_filter_table_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {7, "filter_number"},
    {1, "filter_number.reserved"},

    {5, "filter_conf.reserved"},
    {2, "filter_type"},
    {1, "filter_enable"},

    {1, "filter_action_alert"},
    {1, "filter_action_poweroff"},
    {1, "filter_action_reset"},
    {1, "filter_action_powercycle"},
    {1, "filter_action_oem"},
    {1, "filter_action_diag_interrupt"},
    {1, "filter_action_group_control_operation"},
    {1, "filter_action.reserved"},

    {4, "policy_number"},
    {3, "group_control_selector"},
    {1, "alert_policy_number.reserved"},

    {8, "event_severity"},

    {8, "generator_id_byte1"},

    {8, "generator_id_byte2"},

    {8, "sensor_type"},

    {8, "sensor_number"},

    {8, "event_reading_type"},

    {16, "event_data1_offset_mask"},

    {8, "event_data1_AND_mask"},

    {8, "event_data1_compare1"},

    {8, "event_data1_compare2"},

    {8, "event_data2_AND_mask"},

    {8, "event_data2_compare1"},

    {8, "event_data2_compare2"},

    {8, "event_data3_AND_mask"},

    {8, "event_data3_compare1"},

    {8, "event_data3_compare2"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_event_filter_data1_rq =
  {
    {8, "cmd"},
    
    {7, "parameter_selector"},
    {1, "reserved"},

    {7, "filter_number"},
    {1, "reserved"},

    {5, "reserved"},
    {2, "filter_type"},
    {1, "filter_enable"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_event_filter_data1_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {7, "filter_number"},
    {1, "reserved"},

    {5, "reserved"},
    {2, "filter_type"},
    {1, "filter_enable"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_num_alert_policies_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "reserved"},

    {7, "num_alert_policies"},
    {1, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_num_alert_policies_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {7, "num_alert_policies"},
    {1, "reserved"},

    {0, ""}
  };    

fiid_template_t tmpl_get_pef_conf_param_num_alert_strings_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {7, "num_alert_strings"},
    {1, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_set_pef_conf_param_alert_string_keys_rq =
  {
    {8, "cmd"},

    {7, "parameter_selector"},
    {1, "reserved"},

    {7, "string_selector"},
    {1, "reserved"},

    {7, "filter_number"},
    {1, "reserved"},

    {7, "string_set_number"},
    {1, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_alert_string_keys_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {7, "string_selector"},
    {1, "reserved"},

    {7, "filter_number"},
    {1, "reserved"},

    {7, "string_set_number"},
    {1, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_get_pef_conf_param_alert_strings_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {7, "string_selector"},
    {1, "reserved"},

    {8, "block_selector"},

    {128, "alert_string_data"},

    {0, ""}
  };

fiid_template_t tmpl_alert_immediate_rq =
  {
    {8, "cmd"},

    {4, "alert_immediate_channel_number"},
    {4, "reserved"},

    {4, "alert_immediate_destination_selector"},
    {4, "reserved"},

    {7, "alert_immediate_string_selector"},
    {1, "alert_immediate_string_enable"},

    {0, ""}
  };

fiid_template_t tmpl_alert_immediate_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {0, ""}
  };

static int8_t
fill_kcs_alert_immediate (fiid_obj_t obj_data_rq, uint8_t channel_number,
                          uint8_t destination_selector, uint8_t string_selector,
                          uint8_t string_enable)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_alert_immediate_rq,
                "cmd",
                IPMI_CMD_ALERT_IMMEDIATE);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_alert_immediate_rq,
                "alert_immediate_channel_number",
                channel_number);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_alert_immediate_rq,
                "alert_immediate_destination_selector",
                destination_selector);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_alert_immediate_rq,
                "alert_immediate_string_selector",
                string_selector);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_alert_immediate_rq,
                "alert_immediate_string_enable",
                string_enable);
  return 0;
}

static int8_t
fill_kcs_get_pef_conf_param (fiid_obj_t obj_data_rq, uint8_t parameter_selector,
                             uint8_t parameter_type,
                             uint8_t set_selector,
                             uint8_t block_selector)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_pef_conf_param_rq, 
		"cmd", 
		IPMI_CMD_GET_PEF_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_pef_conf_param_rq, 
		"parameter_type", 
		parameter_type);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_pef_conf_param_rq, 
		"parameter_selector", 
		parameter_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_pef_conf_param_rq, 
		"set_selector", 
		set_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_pef_conf_param_rq, 
		"block_selector", 
		block_selector);
  
  return 0;
}

static int8_t
fill_kcs_set_pef_control (fiid_obj_t obj_data_rq, uint8_t enable_pef,
                          uint8_t enable_pef_event_msgs, uint8_t enable_startup_delay,
                          uint8_t enable_alert_startup_delay)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_pef_control_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_pef_control_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_PEF_CONTROL);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_pef_control_rq,
                "enable_pef",
                enable_pef);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_pef_control_rq,
                "enable_startup_delay",
                enable_startup_delay);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_pef_control_rq,
                "enable_alert_startup_delay",
                enable_alert_startup_delay);
  return 0;
}

static int8_t
fill_kcs_set_global_action_control (fiid_obj_t obj_data_rq, uint8_t enable_alert,
                                    uint8_t enable_powerdown, uint8_t enable_reset,
                                    uint8_t enable_powercycle, uint8_t enable_oem,
                                    uint8_t enable_diag_interrupt)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_ACTION_GLOBAL_CONTROL);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "enable_alert_action",
                enable_alert);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "enable_powerdown_action",
                enable_powerdown);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "enable_reset_action",
                enable_reset);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "enable_powercycle_action",
                enable_powercycle);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "enable_oem_action",
                enable_oem);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_global_action_control_rq,
                "enable_diag_interrupt",
                enable_diag_interrupt);
  return 0;
}

static int8_t
fill_kcs_set_startup_delay (fiid_obj_t obj_data_rq, uint8_t startup_delay)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_startup_delay_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_startup_delay_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_STARTUP_DELAY);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_startup_delay_rq,
                "startup_delay",
                startup_delay);
  return 0;
}

static int8_t
fill_kcs_set_alert_startup_delay (fiid_obj_t obj_data_rq, uint8_t alert_startup_delay)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_alert_startup_delay_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_alert_startup_delay_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_ALERT_STARTUP_DELAY);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_alert_startup_delay_rq,
                "alert_startup_delay",
                alert_startup_delay);
  return 0;
}

static int8_t
fill_kcs_set_num_event_filters (fiid_obj_t obj_data_rq, uint8_t num_event_filters)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_num_event_filters_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_num_event_filters_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_NUM_EVENT_FILTERS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_num_event_filters_rq,
                "num_event_filters",
                num_event_filters);
  return 0;
}

static int8_t 
fill_kcs_set_filter_table_entry (fiid_obj_t obj_data_rq, 
                                 const event_filter_table_entry_t *eft_entry)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_EVENT_FILTER_TABLE);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_number",
                eft_entry->filter_number);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_type",
                eft_entry->filter_type);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_enable",
                eft_entry->filter_enable);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_action_alert",
                eft_entry->event_filter_action_alert);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_action_poweroff",
                eft_entry->event_filter_action_poweroff);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_action_reset",
                eft_entry->event_filter_action_reset);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_action_powercycle",
                eft_entry->event_filter_action_powercycle);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_action_oem",
                eft_entry->event_filter_action_oem);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "filter_action_diag_interrupt",
                eft_entry->event_filter_action_diag_interrupt);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
		"filter_action_group_control_operation", 
                eft_entry->event_filter_action_group_control_operation);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "policy_number",
                eft_entry->policy_number);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
		"group_control_selector", 
                eft_entry->group_control_selector);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_severity",
                eft_entry->event_severity);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "generator_id_byte1",
                eft_entry->generator_id_byte1);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "generator_id_byte2",
                eft_entry->generator_id_byte2);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "sensor_type",
                eft_entry->sensor_type);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "sensor_number",
                eft_entry->sensor_number);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_reading_type",
                eft_entry->event_reading_type);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data1_offset_mask",
                eft_entry->event_data1_offset_mask);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data1_AND_mask",
                eft_entry->event_data1_AND_mask);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data1_compare1",
                eft_entry->event_data1_compare1);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data1_compare2",
                eft_entry->event_data1_compare2);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data2_AND_mask",
                eft_entry->event_data2_AND_mask);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data2_compare1",
                eft_entry->event_data2_compare1);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data2_compare2",
                eft_entry->event_data2_compare2);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data3_AND_mask",
                eft_entry->event_data3_AND_mask);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data3_compare1",
                eft_entry->event_data3_compare1);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_table_rq,
                "event_data3_compare2",
                eft_entry->event_data3_compare2);
  return 0;
}

static int8_t
fill_kcs_set_filter_table_data1 (fiid_obj_t obj_data_rq, uint8_t filter_number,
                                 filter_type_t filter_type, uint8_t enabled)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_data1_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_data1_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_EVENT_FILTER_TABLE_DATA_1);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_data1_rq,
                "filter_number",
                filter_number);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_data1_rq,
                "filter_type",
                filter_type);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_event_filter_data1_rq,
                "filter_enable",
                enabled);
  return 0;
}

static int8_t
fill_kcs_set_num_alert_policies (fiid_obj_t obj_data_rq, uint8_t num_alert_policies)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_num_alert_policies_rq,
                "cmd",
                IPMI_CMD_SET_PEF_CONF_PARAMS);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_num_alert_policies_rq,
                "parameter_selector",
                IPMI_PEF_PARAM_NUM_ALERT_POLICY_ENTRIES);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_pef_conf_param_num_alert_policies_rq,
                "num_alert_policies",
                num_alert_policies);
  return 0;
}

/* This function is not used yet.  When you find it useful, uncomment them */
/* static int8_t */
/* fill_kcs_set_alert_string_keys (fiid_obj_t obj_data_rq, uint8_t string_selector, */
/*                                 uint8_t filter_number, uint8_t string_set_number) */
/* { */
/*   FIID_OBJ_SET (obj_data_rq, */
/*                 tmpl_set_pef_conf_param_alert_string_keys_rq, */
/*                 "cmd", */
/*                 IPMI_CMD_SET_PEF_CONF_PARAMS); */
/*   FIID_OBJ_SET (obj_data_rq, */
/*                 tmpl_set_pef_conf_param_alert_string_keys_rq, */
/*                 "parameter_selector", */
/*                 IPMI_PEF_PARAM_ALERT_STRING_KEYS); */
/*   FIID_OBJ_SET (obj_data_rq, */
/*                 tmpl_set_pef_conf_param_alert_string_keys_rq, */
/*                 "string_selector", */
/*                 string_selector); */
/*   FIID_OBJ_SET (obj_data_rq, */
/*                 tmpl_set_pef_conf_param_alert_string_keys_rq, */
/*                 "filter_number", */
/*                 filter_number); */
/*   FIID_OBJ_SET (obj_data_rq, */
/*                 tmpl_set_pef_conf_param_alert_string_keys_rq, */
/*                 "string_set_number", */
/*                 string_set_number); */
/*   return 0; */
/* } */

static int8_t 
fill_kcs_get_pef_caps (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_pef_caps_rq, 
		"cmd", 
                IPMI_CMD_GET_PEF_CAPS);
  return 0;
}

static int8_t
fill_kcs_arm_pef_postpone_timer (fiid_obj_t obj_data_rq, uint8_t countdown)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_arm_pef_postpone_timer_rq,
                "cmd",
                IPMI_CMD_ARM_PEF_POSTPONE_TIMER);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_arm_pef_postpone_timer_rq,
                "arm_pef_postpone_timer_countdown",
                countdown);
  return 0;
}

static int8_t
fill_kcs_set_last_processed_event (fiid_obj_t obj_data_rq, which_event_t which, uint16_t id)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_last_processed_event_rq,
                "cmd",
                IPMI_CMD_SET_LAST_PROCESSED_EVENT_ID);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_last_processed_event_rq,
                "set_last_processed_event_which",
                which);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_set_last_processed_event_rq,
                "set_last_processed_event_id",
                id);
  return 0;                
}

static int8_t
fill_kcs_get_last_proessed_event (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_get_last_processed_event_rq,
                "cmd",
                IPMI_CMD_GET_LAST_PROCESSED_EVENT_ID);
  return 0;
}

static int8_t
fill_kcs_pet_ack (fiid_obj_t obj_data_rq, uint16_t sequence_number, uint32_t timestamp,
                  uint8_t source_type, uint8_t sensor_device, uint8_t sensor_number,
                  uint32_t event_data)
{
  FIID_OBJ_SET (obj_data_rq,
                tmpl_pet_ack_rq,
                "cmd",
                IPMI_CMD_PET_ACKNOWLEDGE);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_pet_ack_rq,
                "pet_ack_sequence_number",
                sequence_number);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_pet_ack_rq,
                "pet_ack_timestamp",
                timestamp);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_pet_ack_rq,
                "pet_ack_source_type",
                source_type);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_pet_ack_rq,
                "pet_ack_sensor_device",
                sensor_device);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_pet_ack_rq,
                "pet_ack_sensor_number",
                sensor_number);
  FIID_OBJ_SET (obj_data_rq,
                tmpl_pet_ack_rq,
                "pet_ack_event_data",
                event_data);
  return 0;  
}

int8_t 
ipmi_cmd_set_pef_control2 (ipmi_device_t *dev, 
			   uint8_t enable_pef, 
			   uint8_t enable_pef_event_msgs, 
			   uint8_t enable_startup_delay, 
			   uint8_t enable_alert_startup_delay, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_pef_control_rq);
  ERR (fill_kcs_set_pef_control (obj_cmd_rq, 
				 enable_pef, 
				 enable_pef_event_msgs,
				 enable_startup_delay, 
				 enable_alert_startup_delay) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_pef_control_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_global_action_control2 (ipmi_device_t *dev, 
				     uint8_t enable_alert,
				     uint8_t enable_powerdown, 
				     uint8_t enable_reset,
				     uint8_t enable_powercycle, 
				     uint8_t enable_oem,
				     uint8_t enable_diag_interrupt, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_global_action_control_rq);
  ERR (fill_kcs_set_global_action_control (obj_cmd_rq, 
					   enable_alert, 
					   enable_powerdown,
					   enable_reset, 
					   enable_powercycle, 
					   enable_oem, 
					   enable_diag_interrupt) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_global_action_control_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_startup_delay2 (ipmi_device_t *dev, 
			     uint8_t startup_delay, 
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_startup_delay_rq);
  ERR (fill_kcs_set_startup_delay (obj_cmd_rq, startup_delay) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_startup_delay_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_alert_startup_delay2 (ipmi_device_t *dev, 
				   uint8_t alert_startup_delay, 
				   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_alert_startup_delay_rq);
  ERR (fill_kcs_set_alert_startup_delay (obj_cmd_rq, 
					 alert_startup_delay) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_alert_startup_delay_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_num_event_filters2 (ipmi_device_t *dev, 
				 uint8_t num_event_filters, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_num_event_filters_rq);
  ERR (fill_kcs_set_num_event_filters (obj_cmd_rq, 
				       num_event_filters) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_num_event_filters_rq, 
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_filter_table_entry2 (ipmi_device_t *dev, 
				  const event_filter_table_entry_t *eft_entry, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_event_filter_table_rq);
  ERR (fill_kcs_set_filter_table_entry (obj_cmd_rq, 
					eft_entry) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_event_filter_table_rq, 
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_filter_table_data1_2 (ipmi_device_t *dev, 
				   uint8_t filter_number,
				   filter_type_t filter_type, 
				   uint8_t enabled, 
				   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_event_filter_data1_rq);
  ERR (fill_kcs_set_filter_table_data1 (obj_cmd_rq, 
					filter_number, 
					filter_type, 
					enabled) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_event_filter_data1_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_num_alert_policies2 (ipmi_device_t *dev, 
				  uint8_t num_alert_policies, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_num_alert_policies_rq);
  ERR (fill_kcs_set_num_alert_policies (obj_cmd_rq, 
					num_alert_policies) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_num_alert_policies_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_alert_immediate2 (ipmi_device_t *dev,
			   uint8_t channel_number, 
			   uint8_t destination_selector,
			   uint8_t string_selector, 
			   uint8_t string_enable, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_alert_immediate_rq);
  ERR (fill_kcs_alert_immediate (obj_cmd_rq, 
				 channel_number, 
				 destination_selector,
				 string_selector, 
				 string_enable) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_alert_immediate_rq,
		 obj_cmd_rs, 
		 tmpl_alert_immediate_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);                            
}

int8_t 
ipmi_cmd_get_pef_alert_string2 (ipmi_device_t *dev,
				uint8_t parameter_type, 
				uint8_t set_selector,
				uint8_t block_selector, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq,
				    IPMI_PEF_PARAM_ALERT_STRINGS,
				    parameter_type,
				    set_selector,
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq,
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_alert_strings_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_alert_string_keys2 (ipmi_device_t *dev,
				     uint8_t parameter_type, 
				     uint8_t set_selector,
				     uint8_t block_selector, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq,
				    IPMI_PEF_PARAM_ALERT_STRING_KEYS,
				    parameter_type,
				    set_selector,
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_alert_string_keys_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_num_alert_policies2 (ipmi_device_t *dev,
				      uint8_t parameter_type, 
				      uint8_t set_selector,
				      uint8_t block_selector, 
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq,
				    IPMI_PEF_PARAM_NUM_ALERT_POLICY_ENTRIES,
				    parameter_type,
				    set_selector,
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq,
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_num_alert_policies_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_num_alert_strings2 (ipmi_device_t *dev, 
				     uint8_t parameter_type, 
				     uint8_t set_selector, 
				     uint8_t block_selector, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq, 
				    IPMI_PEF_PARAM_NUM_ALERT_STRINGS, 
				    parameter_type, 
				    set_selector, 
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_num_alert_strings_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_filter_data1_2 (ipmi_device_t *dev, 
				 uint8_t parameter_type, 
				 uint8_t set_selector, 
				 uint8_t block_selector, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq,
				    IPMI_PEF_PARAM_EVENT_FILTER_TABLE_DATA_1,
				    parameter_type,
				    set_selector,
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq,
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_event_filter_data1_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_control2 (ipmi_device_t *dev, 
			   uint8_t parameter_type, 
			   uint8_t set_selector, 
			   uint8_t block_selector, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq, 
				    IPMI_PEF_PARAM_PEF_CONTROL,
				    parameter_type, 
				    set_selector, 
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_pef_control_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_global_action_control2 (ipmi_device_t *dev, 
					 uint8_t parameter_type, 
					 uint8_t set_selector,
					 uint8_t block_selector, 
					 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq, 
				    IPMI_PEF_PARAM_ACTION_GLOBAL_CONTROL,
				    parameter_type, 
				    set_selector, 
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_global_action_control_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_startup_delay2 (ipmi_device_t *dev, 
				 uint8_t parameter_type, 
				 uint8_t set_selector,
				 uint8_t block_selector, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq, 
				    IPMI_PEF_PARAM_STARTUP_DELAY,
				    parameter_type, 
				    set_selector, 
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_startup_delay_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_alert_startup_delay2 (ipmi_device_t *dev, 
				       uint8_t parameter_type, 
				       uint8_t set_selector,
				       uint8_t block_selector, 
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq, 
				    IPMI_PEF_PARAM_ALERT_STARTUP_DELAY,
				    parameter_type, 
				    set_selector, 
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_alert_startup_delay_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_num_event_filters2 (ipmi_device_t *dev, 
				     uint8_t parameter_type, 
				     uint8_t set_selector,
				     uint8_t block_selector, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq, 
				    IPMI_PEF_PARAM_NUM_EVENT_FILTERS,
				    parameter_type, 
				    set_selector, 
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_num_event_filters_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_filter_table_entry2 (ipmi_device_t *dev, 
				      uint8_t parameter_type, 
				      uint8_t set_selector,
				      uint8_t block_selector, 
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (obj_cmd_rq, 
				    IPMI_PEF_PARAM_EVENT_FILTER_TABLE,
				    parameter_type, 
				    set_selector, 
				    block_selector) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_event_filter_table_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_caps2 (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_caps_rq);
  ERR (fill_kcs_get_pef_caps (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_caps_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_caps_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_arm_pef_postpone_timer2 (ipmi_device_t *dev, 
				  uint8_t countdown, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_arm_pef_postpone_timer_rq);
  ERR (fill_kcs_arm_pef_postpone_timer (obj_cmd_rq, 
					countdown) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_arm_pef_postpone_timer_rq, 
		 obj_cmd_rs, 
		 tmpl_arm_pef_postpone_timer_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}  

int8_t 
ipmi_cmd_set_last_processed_event2 (ipmi_device_t *dev, 
				    which_event_t which, 
				    uint16_t id, 
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_last_processed_event_rq);
  ERR (fill_kcs_set_last_processed_event (obj_cmd_rq, 
					  which, 
					  id) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_last_processed_event_rq, 
		 obj_cmd_rs, 
		 tmpl_set_last_processed_event_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_last_processed_event2 (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_last_processed_event_rq);
  ERR (fill_kcs_get_last_proessed_event (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_last_processed_event_rq, 
		 obj_cmd_rs, 
		 tmpl_get_last_processed_event_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_pet_ack2 (ipmi_device_t *dev, 
		   uint16_t sequence_number, 
		   uint32_t timestamp, 
		   uint8_t source_type, 
		   uint8_t sensor_device, 
		   uint8_t sensor_number, 
		   uint32_t event_data, 
		   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_pet_ack_rq);
  ERR (fill_kcs_pet_ack (obj_cmd_rq, 
			 sequence_number, 
			 timestamp, 
			 source_type,
			 sensor_device, 
			 sensor_number, 
			 event_data) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_pet_ack_rq,
		 obj_cmd_rs, 
		 tmpl_pet_ack_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

