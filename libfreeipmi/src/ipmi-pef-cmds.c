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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

$Id: ipmi-pef-cmds.c,v 1.23.2.4 2006-02-15 21:55:40 chu11 Exp $  */

#include "freeipmi.h"

fiid_template_t tmpl_get_pef_caps_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_caps_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {4,  "pef_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "pef_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {1,  "action_support.alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "action_support.powerdown", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "action_support.reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "action_support.powercycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "action_support.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "action_support.diag_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {8,  "number_of_eft_entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {0,  "", 0}
  };

fiid_template_t tmpl_arm_pef_postpone_timer_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "arm_pef_postpone_timer_countdown", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_arm_pef_postpone_timer_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "arm_pef_postpone_timer_countdown", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_last_processed_event_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "set_last_processed_event_which", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {16, "set_last_processed_event_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_last_processed_event_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_last_processed_event_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_last_processed_event_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {32, "get_last_processed_event_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {16, "get_last_processed_event_sel_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {16, "get_last_processed_event_software_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {16, "get_last_processed_event_bmc_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_pet_ack_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {16, "pet_ack_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {32, "pet_ack_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "pet_ack_source_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "pet_ack_sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "pet_ack_sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {24, "pet_ack_event_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_pet_ack_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_pef_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "enable_pef", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_pef_event_msgs", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_pef_control_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "enable_pef", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_pef_event_msgs", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_global_action_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "enable_alert_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_powerdown_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_reset_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_powercycle_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_diag_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_global_action_control_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "enable_alert_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_powerdown_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_reset_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_powercycle_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_diag_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_startup_delay_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_startup_delay_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_alert_startup_delay_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_alert_startup_delay_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_num_event_filters_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "num_event_filters", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };
    
fiid_template_t tmpl_get_pef_conf_param_num_event_filters_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "num_event_filters", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_event_filter_table_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_selector.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {5, "filter_conf.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "filter_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "filter_action_alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_poweroff", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_powercycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_diag_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_group_control_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "group_control_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_policy_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_severity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "generator_id_byte1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "generator_id_byte2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_reading_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {16, "event_data1_offset_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data1_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data1_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data1_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data2_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data2_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data2_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data3_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data3_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data3_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_event_filter_table_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {5, "filter_conf.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "filter_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "filter_action_alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_poweroff", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_powercycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_diag_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action_group_control_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_action.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "group_control_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_policy_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_severity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "generator_id_byte1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "generator_id_byte2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_reading_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {16, "event_data1_offset_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data1_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data1_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data1_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data2_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data2_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data2_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data3_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data3_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "event_data3_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_event_filter_data1_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {5, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "filter_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_event_filter_data1_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {5, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "filter_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_num_alert_policies_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "num_alert_policies", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_num_alert_policies_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "num_alert_policies", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };    

fiid_template_t tmpl_get_pef_conf_param_num_alert_strings_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "num_alert_strings", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_pef_conf_param_alert_string_keys_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "string_set_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_alert_string_keys_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "string_set_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_pef_conf_param_alert_strings_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {128, "alert_string_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_alert_immediate_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "alert_immediate_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "alert_immediate_destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {7, "alert_immediate_string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_immediate_string_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_alert_immediate_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

int8_t
fill_kcs_alert_immediate (uint8_t channel_number,
                          uint8_t destination_selector, 
                          uint8_t string_selector,
                          uint8_t string_enable,
                          fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_alert_immediate_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_ALERT_IMMEDIATE);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"alert_immediate_channel_number",
                channel_number);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved1",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"alert_immediate_destination_selector",
                destination_selector);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved2",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"alert_immediate_string_selector",
                string_selector);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"alert_immediate_string_enable",
                string_enable);
  return 0;
}

int8_t
fill_kcs_get_pef_conf_param (uint8_t parameter_selector,
                             uint8_t parameter_type,
                             uint8_t set_selector,
                             uint8_t block_selector,
                             fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_pef_conf_param_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_PEF_CONFIGURATION_PARAMETERS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_type", 
		parameter_type);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"set_selector", 
		set_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"block_selector", 
		block_selector);
  
  return 0;
}

int8_t
fill_kcs_set_pef_control (uint8_t enable_pef,
                          uint8_t enable_pef_event_msgs, 
                          uint8_t enable_startup_delay,
                          uint8_t enable_alert_startup_delay,
                          fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_pef_control_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_PEF_CONTROL);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved1",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_pef",
                enable_pef);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_startup_delay",
                enable_startup_delay);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_alert_startup_delay",
                enable_alert_startup_delay);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved2",
                0);
  return 0;
}

int8_t
fill_kcs_set_global_action_control (uint8_t enable_alert,
                                    uint8_t enable_powerdown, 
                                    uint8_t enable_reset,
                                    uint8_t enable_powercycle, 
                                    uint8_t enable_oem,
                                    uint8_t enable_diag_interrupt,
                                    fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_global_action_control_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_ACTION_GLOBAL_CONTROL);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved1",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_alert_action",
                enable_alert);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_powerdown_action",
                enable_powerdown);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_reset_action",
                enable_reset);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_powercycle_action",
                enable_powercycle);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_oem_action",
                enable_oem);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"enable_diag_interrupt",
                enable_diag_interrupt);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved2",
                0);
  return 0;
}

int8_t
fill_kcs_set_startup_delay (uint8_t startup_delay, fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_startup_delay_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_STARTUP_DELAY);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"startup_delay",
                startup_delay);
  return 0;
}

int8_t
fill_kcs_set_alert_startup_delay (uint8_t alert_startup_delay, fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_alert_startup_delay_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_ALERT_STARTUP_DELAY);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"alert_startup_delay",
                alert_startup_delay);
  return 0;
}

int8_t
fill_kcs_set_num_event_filters (uint8_t num_event_filters, fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_num_event_filters_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved1",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_NUM_EVENT_FILTERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"num_event_filters",
                num_event_filters);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved2",
                0);
  return 0;
}

int8_t 
fill_kcs_set_filter_table_entry (const event_filter_table_entry_t *eft_entry,
                                 fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_event_filter_table_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_EVENT_FILTER_TABLE);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"parameter_selector.reserved",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_number",
                eft_entry->filter_number);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"filter_number.reserved",
                0);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"filter_conf.reserved",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_type",
                eft_entry->filter_type);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_enable",
                eft_entry->filter_enable);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_action_alert",
                eft_entry->event_filter_action_alert);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_action_poweroff",
                eft_entry->event_filter_action_poweroff);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_action_reset",
                eft_entry->event_filter_action_reset);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_action_powercycle",
                eft_entry->event_filter_action_powercycle);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_action_oem",
                eft_entry->event_filter_action_oem);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_action_diag_interrupt",
                eft_entry->event_filter_action_diag_interrupt);
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"filter_action_group_control_operation", 
                eft_entry->event_filter_action_group_control_operation);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"filter_action.reserved",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"policy_number",
                eft_entry->policy_number);
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"group_control_selector", 
                eft_entry->group_control_selector);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"alert_policy_number.reserved",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_severity",
                eft_entry->event_severity);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"generator_id_byte1",
                eft_entry->generator_id_byte1);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"generator_id_byte2",
                eft_entry->generator_id_byte2);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"sensor_type",
                eft_entry->sensor_type);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"sensor_number",
                eft_entry->sensor_number);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_reading_type",
                eft_entry->event_reading_type);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data1_offset_mask",
                eft_entry->event_data1_offset_mask);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data1_AND_mask",
                eft_entry->event_data1_AND_mask);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data1_compare1",
                eft_entry->event_data1_compare1);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data1_compare2",
                eft_entry->event_data1_compare2);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data2_AND_mask",
                eft_entry->event_data2_AND_mask);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data2_compare1",
                eft_entry->event_data2_compare1);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data2_compare2",
                eft_entry->event_data2_compare2);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data3_AND_mask",
                eft_entry->event_data3_AND_mask);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data3_compare1",
                eft_entry->event_data3_compare1);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"event_data3_compare2",
                eft_entry->event_data3_compare2);
  return 0;
}

int8_t
fill_kcs_set_filter_table_data1 (uint8_t filter_number,
                                 filter_type_t filter_type, 
                                 uint8_t enabled,
                                 fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_event_filter_data1_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_EVENT_FILTER_TABLE_DATA_1);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved1",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_number",
                filter_number);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved2",
                0);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved3",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_type",
                filter_type);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"filter_enable",
                enabled);
  return 0;
}

int8_t
fill_kcs_set_num_alert_policies (uint8_t num_alert_policies,
                                 fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_num_alert_policies_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                IPMI_PEF_PARAM_NUM_ALERT_POLICY_ENTRIES);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved1",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"num_alert_policies",
                num_alert_policies);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved2",
                0);
  return 0;
}

int8_t 
fill_kcs_set_alert_string_keys (uint8_t string_selector, 
                                uint8_t filter_number, 
                                uint8_t string_set_number,
                                fiid_obj_t obj_data_rq)
{ 
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_pef_conf_param_alert_string_keys_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
                "cmd", 
                IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS); 
  FIID_OBJ_SET (obj_data_rq, 
                "parameter_selector", 
                IPMI_PEF_PARAM_ALERT_STRING_KEYS); 
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved1",
                0);
  FIID_OBJ_SET (obj_data_rq, 
                "string_selector", 
                string_selector); 
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved2",
                0);
  FIID_OBJ_SET (obj_data_rq, 
                "filter_number", 
                filter_number); 
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved3",
                0);
  FIID_OBJ_SET (obj_data_rq, 
                "string_set_number", 
                string_set_number); 
  return 0; 
} 

int8_t 
fill_kcs_get_pef_caps (fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_pef_caps_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
                IPMI_CMD_GET_PEF_CAPABILITIES);
  return 0;
}

int8_t
fill_kcs_arm_pef_postpone_timer (uint8_t countdown, fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_arm_pef_postpone_timer_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_ARM_PEF_POSTPONE_TIMER);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"arm_pef_postpone_timer_countdown",
                countdown);
  return 0;
}

int8_t
fill_kcs_set_last_processed_event (which_event_t which, uint16_t id, fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_last_processed_event_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_LAST_PROCESSED_EVENT_ID);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"set_last_processed_event_which",
                which);
  FIID_OBJ_SET (obj_data_rq, 
                (uint8_t *)"reserved",
                0);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"set_last_processed_event_id",
                id);
  return 0;                
}

int8_t
fill_kcs_get_last_proessed_event (fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_last_processed_event_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_GET_LAST_PROCESSED_EVENT_ID);
  return 0;
}

int8_t
fill_kcs_pet_ack (uint16_t sequence_number, 
                  uint32_t timestamp,
                  uint8_t source_type, 
                  uint8_t sensor_device, 
                  uint8_t sensor_number,
                  uint32_t event_data, 
                  fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_pet_ack_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_PET_ACKNOWLEDGE);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"pet_ack_sequence_number",
                sequence_number);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"pet_ack_timestamp",
                timestamp);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"pet_ack_source_type",
                source_type);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"pet_ack_sensor_device",
                sensor_device);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"pet_ack_sensor_number",
                sensor_number);
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"pet_ack_event_data",
                event_data);
  return 0;  
}

