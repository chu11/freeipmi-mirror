/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef _IPMI_PEF_AND_ALERTING_CMDS_TEMPLATES_H
#define _IPMI_PEF_AND_ALERTING_CMDS_TEMPLATES_H

#ifdef __cplusplus
extern "C" {
#endif

/* This header file is for documentation only */

#if 0

Format = { bits, "field name", field flags }

FIID_FIELD_REQUIRED - field is required for the payload
FIID_FIELD_OPTIONAL - field is optional for the payload

FIID_FIELD_LENGTH_FIXED - field length is fixed at the number of bits listed
FIID_FIELD_LENGTH_VARIABLE - field length is variable for the number of bits listed

FIID_FIELD_MAKES_PACKET_SUFFICIENT - indicates field or fields are "sufficient" to make a valid packet

Get PEF Capabilities Request
----------------------------

fiid_template_t tmpl_cmd_get_pef_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Capabilities Response
-----------------------------

fiid_template_t tmpl_cmd_get_pef_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "pef_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "pef_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "action_support.alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "action_support.power_down", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "action_support.reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "action_support.power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "action_support.oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "action_support.diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_event_record_filtering_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_event_filter_table_entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

ARM PEF Postpone Timer Request
------------------------------

fiid_template_t tmpl_cmd_arm_pef_postpone_timer_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pef_postpone_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

ARM PEF Postpone Timer Response
-------------------------------

fiid_template_t tmpl_cmd_arm_pef_postpone_timer_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "present_timer_countdown_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters Request
----------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set PEF Configuration Parameters Response
-----------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (Set In Progress) Request
----------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (PEF Control) Request
------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_control_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (PEF Action Global Control) Request
--------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_action_global_control_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_down_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reset_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_cycle_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (PEF Startup Delay) Request
------------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_startup_delay_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (PEF Alert Startup Delay) Request
------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_pef_alert_startup_delay_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (Event Filter Table) Request
-------------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_event_filter_table_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "event_filter_action.alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.power_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.group_control_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 4, "alert_policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "alert_policy_number.group_control_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_policy_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_severity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "generator_id_byte1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "generator_id_byte2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_trigger", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "event_data1_offset_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (Event Filter Table Data1) Request
-------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_event_filter_table_data1_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (Alert String Keys) Request
------------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_alert_string_keys_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "set_number_for_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (Alert Strings) Request
--------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_alert_strings_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set PEF Configuration Parameters (Alert Policy Table) Request
-------------------------------------------------------------

fiid_template_t tmpl_cmd_set_pef_configuration_parameters_alert_policy_table_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "alert_policy_entry_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "policy_number.policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "policy_number.enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_destination.destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_destination.channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "alert_string_key.alert_string_set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_string_key.event_specific_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters Request
----------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters Response
-----------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Set In Progress) Response
-----------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_set_in_progress_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (PEF Control) Response
-------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_control_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (PEF Action Global Control) Response
---------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_down_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reset_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_cycle_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (PEF Startup Delay) Response
-------------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pef_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (PEF Alert Startup Delay) Response
-------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pef_alert_startup_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Number of Event Filters) Response
-------------------------------------------------------------------

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "number_of_event_filters", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Event Filter Table) Response
--------------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_event_filter_table_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "event_filter_action.alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.power_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.group_control_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_filter_action.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 4, "alert_policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "alert_policy_number.group_control_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_policy_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_severity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "generator_id_byte1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "generator_id_byte2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_trigger", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "event_data1_offset_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3_AND_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3_compare1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3_compare2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Event Filter Table Data1) Response
--------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_event_filter_table_data1_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "filter_configuration.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "filter_configuration.type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "filter_configuration.filter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Number of Alert Policy Entries) Response
--------------------------------------------------------------------------

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "number_of_alert_policy_entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Number of Alert Strings) Response
-------------------------------------------------------------------

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "number_of_alert_strings", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Alert String Keys) Response
-------------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_alert_string_keys_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "filter_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "set_number_for_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Alert Strings) Response
---------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_alert_strings_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get PEF Configuration Parameters (Alert Policy Table) Response
--------------------------------------------------------------

fiid_template_t tmpl_cmd_get_pef_configuration_parameters_alert_policy_table_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "alert_policy_entry_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_policy_entry_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "policy_number.policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "policy_number.enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "policy_number.policy_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_destination.destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_destination.channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "alert_string_key.alert_string_set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_string_key.event_specific_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Last Processed Event ID Request
-----------------------------------

fiid_template_t tmpl_cmd_set_last_processed_event_id_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_record_id_for_last_record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Last Processed Event ID Response
------------------------------------

fiid_template_t tmpl_cmd_set_last_processed_event_id_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Last Processed Event ID Request
-----------------------------------

fiid_template_t tmpl_cmd_get_last_processed_event_id_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Last Processed Event ID Response
------------------------------------

fiid_template_t tmpl_cmd_get_last_processed_event_id_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "most_recent_addition_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "record_id_for_last_record_in_sel", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "last_sw_processed_event_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "last_bmc_processed_event_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Alert Immediate Request
-----------------------

fiid_template_t tmpl_cmd_alert_immediate_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "send_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Alert Immediate Response
------------------------

fiid_template_t tmpl_cmd_alert_immediate_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

PET Acknowledge Request
-----------------------

fiid_template_t tmpl_cmd_pet_acknowledge_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "local_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_source_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "event_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

PET Acknowledge Response
------------------------

fiid_template_t tmpl_cmd_pet_acknowledge_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_PEF_AND_ALERTING_CMDS_TEMPLATES_H */
