/*   
     Copyright (C) 2003-2008 FreeIPMI Core Team

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
     
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-pef-and-alerting-cmds.h"
#include "freeipmi/spec/ipmi-channel-spec.h" 
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-pef-parameter-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_pef_capabilities_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_capabilities_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "pef_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "pef_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "action_support.alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "action_support.power_down", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "action_support.reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "action_support.power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "action_support.oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "action_support.diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "oem_event_record_filtering_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "number_of_event_filter_table_entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_arm_pef_postpone_timer_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "pef_postpone_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_arm_pef_postpone_timer_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "present_timer_countdown_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_rq =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,    "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,    "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_action_global_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_down_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reset_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_cycle_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_startup_delay_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_alert_startup_delay_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_event_filter_table_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5,  "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1,  "event_filter_action.alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.power_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.group_control_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4,  "alert_policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "alert_policy_number.group_control_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "alert_policy_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_severity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "generator_id_byte1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "generator_id_byte2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_trigger", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "event_data1_offset_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data1_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data1_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data1_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data2_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data2_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data2_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data3_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data3_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data3_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_event_filter_table_data1_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5, "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_alert_string_keys_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "set_number_for_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_alert_strings_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "string_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_alert_policy_table_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "alert_policy_entry_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "policy_number.policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "policy_number.enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_destination.destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_destination.channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "alert_string_key.alert_string_set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_string_key.event_specific_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_rs =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_control_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_down_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reset_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_cycle_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "number_of_event_filters", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_event_filter_table_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5,  "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1,  "event_filter_action.alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.power_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.group_control_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "event_filter_action.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4,  "alert_policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "alert_policy_number.group_control_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "alert_policy_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_severity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "generator_id_byte1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "generator_id_byte2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_trigger", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "event_data1_offset_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data1_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data1_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data1_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data2_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data2_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data2_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data3_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data3_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_data3_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_event_filter_table_data1_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5, "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "number_of_alert_policy_entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };    

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "number_of_alert_strings", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_alert_string_keys_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "set_number_for_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_alert_strings_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "string_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_alert_policy_table_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "alert_policy_entry_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_policy_entry_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "policy_number.policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "policy_number.enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_destination.destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_destination.channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "alert_string_key.alert_string_set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "alert_string_key.event_specific_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_last_processed_event_id_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "set_record_id_for_last_record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_last_processed_event_id_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_last_processed_event_id_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_last_processed_event_id_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "most_recent_addition_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "record_id_for_last_record_in_sel", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "last_sw_processed_event_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "last_bmc_processed_event_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_alert_immediate_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "send_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_alert_immediate_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_pet_acknowledge_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "local_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "event_source_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24, "event_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_pet_acknowledge_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t 
fill_cmd_get_pef_capabilities (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));;

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_pef_capabilities_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_PEF_CAPABILITIES);
  return 0;
}

int8_t
fill_cmd_arm_pef_postpone_timer (uint8_t pef_postpone_timeout, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));;

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_arm_pef_postpone_timer_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_ARM_PEF_POSTPONE_TIMER);
  FIID_OBJ_SET (obj_cmd_rq, "pef_postpone_timeout", pef_postpone_timeout);
  return 0;
}

int8_t
fill_cmd_set_pef_configuration_parameters (fiid_obj_t obj_cmd_rq,
                                           uint8_t parameter_selector,
                                           uint8_t *configuration_parameter_data,
                                           uint8_t configuration_parameter_data_len)
{
  ERR_EINVAL (configuration_parameter_data
              && configuration_parameter_data_len
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET_DATA (obj_cmd_rq,
                     "configuration_parameter_data",
                     configuration_parameter_data,
                     configuration_parameter_data_len);

  return 0;
}

int8_t
fill_cmd_set_pef_configuration_parameters_pef_control (uint8_t pef,
                                                       uint8_t pef_event_messages, 
                                                       uint8_t pef_startup_delay,
                                                       uint8_t pef_alert_startup_delay,
                                                       fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_PEF_VALID(pef)
              && IPMI_PEF_EVENT_MESSAGES_VALID(pef_event_messages)
              && IPMI_PEF_STARTUP_DELAY_VALID(pef_startup_delay)
              && IPMI_PEF_ALERT_STARTUP_DELAY_VALID(pef_alert_startup_delay)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_control_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_PEF_CONTROL);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "pef", pef);
  FIID_OBJ_SET (obj_cmd_rq, "pef_event_messages", pef);
  FIID_OBJ_SET (obj_cmd_rq, "pef_startup_delay", pef_startup_delay);
  FIID_OBJ_SET (obj_cmd_rq, "pef_alert_startup_delay", pef_alert_startup_delay);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  return 0;
}

int8_t
fill_cmd_set_pef_configuration_parameters_pef_action_global_control (uint8_t alert_action,
                                                                     uint8_t power_down_action, 
                                                                     uint8_t reset_action,
                                                                     uint8_t power_cycle_action, 
                                                                     uint8_t oem_action,
                                                                     uint8_t diagnostic_interrupt,
                                                                     fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_PEF_ALERT_ACTION_VALID(alert_action)
              && IPMI_PEF_POWER_DOWN_ACTION_VALID(power_down_action)
              && IPMI_PEF_RESET_ACTION_VALID(reset_action)
              && IPMI_PEF_POWER_CYCLE_ACTION_VALID(power_cycle_action)
              && IPMI_PEF_OEM_ACTION_VALID(oem_action)
              && IPMI_PEF_DIAGNOSTIC_INTERRUPT_VALID(diagnostic_interrupt)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_action_global_control_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_PEF_ACTION_GLOBAL_CONTROL);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "alert_action", alert_action);
  FIID_OBJ_SET (obj_cmd_rq, "power_down_action", power_down_action);
  FIID_OBJ_SET (obj_cmd_rq, "reset_action", reset_action);
  FIID_OBJ_SET (obj_cmd_rq, "power_cycle_action", power_cycle_action);
  FIID_OBJ_SET (obj_cmd_rq, "oem_action", oem_action);
  FIID_OBJ_SET (obj_cmd_rq, "diagnostic_interrupt", diagnostic_interrupt);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  return 0;
}

int8_t
fill_cmd_set_pef_configuration_parameters_pef_startup_delay (uint8_t pef_startup_delay, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_startup_delay_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_PEF_STARTUP_DELAY);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "pef_startup_delay", pef_startup_delay);
  return 0;
}

int8_t
fill_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (uint8_t pef_alert_startup_delay, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_alert_startup_delay_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_PEF_ALERT_STARTUP_DELAY);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "pef_alert_startup_delay", pef_alert_startup_delay);
  return 0;
}

int8_t 
fill_cmd_set_pef_configuration_parameters_event_filter_table (uint8_t filter_number,
                                                              uint8_t filter_configuration_type,
                                                              uint8_t filter_configuration_filter,
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
                                                              fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_FILTER_CONFIGURATION_FILTER_TYPE_VALID(filter_configuration_type)
              && IPMI_FILTER_CONFIGURATION_FILTER_VALID(filter_configuration_filter)
              && IPMI_EVENT_FILTER_ACTION_ALERT_VALID(event_filter_action_alert)
              && IPMI_EVENT_FILTER_ACTION_POWER_OFF_VALID(event_filter_action_power_off)
              && IPMI_EVENT_FILTER_ACTION_RESET_VALID(event_filter_action_reset)
              && IPMI_EVENT_FILTER_ACTION_POWER_CYCLE_VALID(event_filter_action_power_cycle)
              && IPMI_EVENT_FILTER_ACTION_OEM_VALID(event_filter_action_oem)
              && IPMI_EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_VALID(event_filter_action_diagnostic_interrupt)
              && IPMI_EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_VALID(event_filter_action_group_control_operation)
              && IPMI_EVENT_SEVERITY_VALID(event_severity)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_event_filter_table_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_EVENT_FILTER_TABLE);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "filter_number", filter_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "filter_configuration.reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "filter_configuration.type", filter_configuration_type);
  FIID_OBJ_SET (obj_cmd_rq, "filter_configuration.filter", filter_configuration_filter);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.alert", event_filter_action_alert);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.power_off", event_filter_action_power_off);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.reset", event_filter_action_reset);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.power_cycle", event_filter_action_power_cycle);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.oem", event_filter_action_oem);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.diagnostic_interrupt", event_filter_action_diagnostic_interrupt);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.group_control_operation", event_filter_action_group_control_operation);
  FIID_OBJ_SET (obj_cmd_rq, "event_filter_action.reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "alert_policy_number.policy_number", alert_policy_number_policy_number);
  FIID_OBJ_SET (obj_cmd_rq, "alert_policy_number.group_control_selector", alert_policy_number_group_control_selector);
  FIID_OBJ_SET (obj_cmd_rq, "alert_policy_number.reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "event_severity", event_severity);
  FIID_OBJ_SET (obj_cmd_rq, "generator_id_byte1", generator_id_byte1);
  FIID_OBJ_SET (obj_cmd_rq, "generator_id_byte2",generator_id_byte2);
  FIID_OBJ_SET (obj_cmd_rq, "sensor_type", sensor_type);
  FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FIID_OBJ_SET (obj_cmd_rq, "event_trigger", event_trigger);
  FIID_OBJ_SET (obj_cmd_rq, "event_data1_offset_mask", event_data1_offset_mask);
  FIID_OBJ_SET (obj_cmd_rq, "event_data1_AND_mask", event_data1_AND_mask);
  FIID_OBJ_SET (obj_cmd_rq, "event_data1_compare1", event_data1_compare1);
  FIID_OBJ_SET (obj_cmd_rq, "event_data1_compare2", event_data1_compare2);
  FIID_OBJ_SET (obj_cmd_rq, "event_data2_AND_mask", event_data2_AND_mask);
  FIID_OBJ_SET (obj_cmd_rq, "event_data2_compare1", event_data2_compare1);
  FIID_OBJ_SET (obj_cmd_rq, "event_data2_compare2", event_data2_compare2);
  FIID_OBJ_SET (obj_cmd_rq, "event_data3_AND_mask", event_data3_AND_mask);
  FIID_OBJ_SET (obj_cmd_rq, "event_data3_compare1", event_data3_compare1);
  FIID_OBJ_SET (obj_cmd_rq, "event_data3_compare2", event_data3_compare2);
  return 0;
}


int8_t
fill_cmd_set_pef_configuration_parameters_event_filter_table_data1 (uint8_t filter_number,
                                                                    uint8_t filter_configuration_type,
                                                                    uint8_t filter_configuration_filter,
                                                                    fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_FILTER_CONFIGURATION_FILTER_TYPE_VALID(filter_configuration_type)
              && IPMI_FILTER_CONFIGURATION_FILTER_VALID(filter_configuration_filter)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_event_filter_table_data1_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq,"cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_EVENT_FILTER_TABLE_DATA_1);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "filter_number",  filter_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "filter_configuration.reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "filter_configuration.type", filter_configuration_type);
  FIID_OBJ_SET (obj_cmd_rq, "filter_configuration.filter", filter_configuration_filter);
  return 0;
}

int8_t 
fill_cmd_set_pef_configuration_parameters_alert_string_keys (uint8_t string_selector, 
                                                             uint8_t filter_number, 
                                                             uint8_t set_number_for_string,
                                                             fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (IPMI_STRING_SELECTOR_VALID(string_selector)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_alert_string_keys_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS); 
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_ALERT_STRING_KEYS); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "string_selector", string_selector); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "filter_number", filter_number); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FIID_OBJ_SET (obj_cmd_rq, "set_number_for_string", set_number_for_string); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved4", 0);
  return 0; 
} 

int8_t 
fill_cmd_set_pef_configuration_parameters_alert_strings (uint8_t string_selector, 
                                                         uint8_t block_selector,
                                                         uint8_t *string_data,
                                                         uint32_t string_data_len,
                                                         fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (IPMI_STRING_SELECTOR_VALID(string_selector)
              && string_data
              && string_data_len
	      && IPMI_PEF_ALERT_STRINGS_BLOCK_SIZE_VALID (string_data_len)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_alert_strings_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS); 
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_ALERT_STRINGS); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "string_selector", string_selector); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "block_selector", block_selector); 
  FIID_OBJ_SET_DATA (obj_cmd_rq,
                     "string_data",
                     string_data,
                     string_data_len);

  return 0; 
} 


int8_t 
fill_cmd_set_pef_configuration_parameters_alert_policy_table (uint8_t alert_policy_entry_number, 
							      uint8_t policy_type, 
							      uint8_t policy_enabled, 
							      uint8_t policy_number, 
							      uint8_t destination_selector, 
							      uint8_t channel_number, 
							      uint8_t alert_string_set_selector, 
							      uint8_t event_specific_alert_string, 
							      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_ALERT_POLICY_TABLE_POLICY_TYPE_VALID (policy_type) && 
	      IPMI_ALERT_POLICY_ENABLED_DISABLED_VALID (policy_enabled) && 
	      IPMI_EVENT_SPECIFIC_ALERT_STRING_VALID (event_specific_alert_string) && 
	      fiid_obj_valid (obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, 
			     tmpl_cmd_set_pef_configuration_parameters_alert_policy_table_rq);
  
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_PEF_PARAMETER_ALERT_POLICY_TABLE);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "alert_policy_entry_number", alert_policy_entry_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "policy_number.policy_type", policy_type);
  FIID_OBJ_SET (obj_cmd_rq, "policy_number.enabled", policy_enabled);
  FIID_OBJ_SET (obj_cmd_rq, "policy_number.policy_number", policy_number);
  FIID_OBJ_SET (obj_cmd_rq, "channel_destination.destination_selector", destination_selector);
  FIID_OBJ_SET (obj_cmd_rq, "channel_destination.channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "alert_string_key.alert_string_set_selector", alert_string_set_selector);
  FIID_OBJ_SET (obj_cmd_rq, "alert_string_key.event_specific_alert_string", event_specific_alert_string);
  
  return 0;
}


int8_t
fill_cmd_get_pef_configuration_parameters (uint8_t parameter_selector,
                                           uint8_t get_parameter,
                                           uint8_t set_selector,
                                           uint8_t block_selector,
                                           fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_PEF_PARAMETER_VALID(parameter_selector)
              && IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_PEF_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_cmd_rq, "get_parameter", get_parameter);
  FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FIID_OBJ_SET (obj_cmd_rq, "block_selector", block_selector);
  
  return 0;
}

int8_t
fill_cmd_set_last_processed_event_id (uint8_t set_record_id_for_last_record,
                                      uint16_t record_id,
                                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_SET_RECORD_ID_FOR_LAST_RECORD_PROCESSED_VALID(set_record_id_for_last_record)
              && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_last_processed_event_id_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAST_PROCESSED_EVENT_ID);
  FIID_OBJ_SET (obj_cmd_rq, "set_record_id_for_last_record,", set_record_id_for_last_record);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "record_id", record_id);
  return 0;                
}

int8_t
fill_cmd_get_last_processed_event_id (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_last_processed_event_id_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_LAST_PROCESSED_EVENT_ID);
  return 0;
}

int8_t
fill_cmd_alert_immediate (uint8_t channel_number,
                          uint8_t destination_selector, 
                          uint8_t operation,
                          uint8_t string_selector,
                          uint8_t send_alert_string,
                          fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && IPMI_ALERT_IMMEDIATE_OPERATION_VALID(operation)
              && IPMI_STRING_SELECTOR_VALID(string_selector)
              && IPMI_SEND_ALERT_STRING_VALID(send_alert_string)
              && IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_alert_immediate_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_ALERT_IMMEDIATE);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "destination_selector", destination_selector);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "string_selector", string_selector);
  FIID_OBJ_SET (obj_cmd_rq, "send_alert_string", send_alert_string);
  return 0;
}

int8_t
fill_cmd_pet_acknowledge (uint16_t sequence_number, 
                          uint32_t local_timestamp,
                          uint8_t event_source_type, 
                          uint8_t sensor_device, 
                          uint8_t sensor_number,
                          uint32_t event_data, 
                          fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_pet_acknowledge_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_PET_ACKNOWLEDGE);
  FIID_OBJ_SET (obj_cmd_rq, "sequence_number", sequence_number);
  FIID_OBJ_SET (obj_cmd_rq, "local_timestamp", local_timestamp);
  FIID_OBJ_SET (obj_cmd_rq, "event_source_type", event_source_type);
  FIID_OBJ_SET (obj_cmd_rq, "sensor_device", sensor_device);
  FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FIID_OBJ_SET (obj_cmd_rq, "event_data", event_data);
  return 0;  
}

