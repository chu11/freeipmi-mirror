/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef _IPMI_CHASSIS_CMDS_TEMPLATES_H
#define _IPMI_CHASSIS_CMDS_TEMPLATES_H

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

Get Chassis Capabilities Request
--------------------------------

fiid_template_t tmpl_cmd_get_chassis_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Chassis Capabilities Response
---------------------------------

fiid_template_t tmpl_cmd_get_chassis_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "capabilities_flags.provides_intrusion_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "capabilities_flags.provides_front_panel_lockout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "capabilities_flags.provides_diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "capabilities_flags.provides_power_interlock", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "capabilities_flags.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "fru_info_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sdr_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sel_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "system_management_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "bridge_device_address", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Chassis Status Request
--------------------------

fiid_template_t tmpl_cmd_get_chassis_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Chassis Status Response
---------------------------

fiid_template_t tmpl_cmd_get_chassis_status_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "current_power_state.power_is_on", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "current_power_state.power_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "current_power_state.interlock", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "current_power_state.power_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "current_power_state.power_control_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "current_power_state.power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "current_power_state.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "last_power_event.ac_failed", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "last_power_event.power_down_caused_by_power_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "last_power_event.power_down_caused_by_power_interlock_being_activated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "last_power_event.power_down_caused_by_power_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "last_power_event.power_on_entered_via_ipmi", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "last_power_event.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "misc_chassis_state.chassis_intrusion_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "misc_chassis_state.front_panel_lockout_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "misc_chassis_state.drive_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "misc_chassis_state.cooling_fan_fault_detected", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "misc_chassis_state.chassis_identify_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "misc_chassis_state.chassis_identify_command_and_state_info_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "misc_chassis_state.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.power_off_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.reset_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.diagnostic_interrupt_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.standby_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.power_off_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.reset_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.diagnostic_interrupt_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "front_panel.standby_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Chassis Control Request
-----------------------

fiid_template_t tmpl_cmd_chassis_control_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "chassis_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Chassis Control Response
------------------------

fiid_template_t tmpl_cmd_chassis_control_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Chassis Identify Request
------------------------

fiid_template_t tmpl_cmd_chassis_identify_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "identify_interval", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_identify", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Chassis Identify Response
-------------------------

fiid_template_t tmpl_cmd_chassis_identify_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set Front Panel Button Enables Request
--------------------------------------

fiid_template_t tmpl_cmd_set_front_panel_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "disable_power_off_button_for_power_off_only", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "disable_reset_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "disable_diagnostic_interrupt_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "disable_standby_button_for_entering_standby", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Front Panel Button Enables Response
---------------------------------------

fiid_template_t tmpl_cmd_set_front_panel_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set Power Restore Policy Request
--------------------------------

fiid_template_t tmpl_cmd_set_power_restore_policy_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Power Restore Policy Response
---------------------------------

fiid_template_t tmpl_cmd_set_power_restore_policy_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "powered_off_after_ac_mains_returns", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "restoring_power_to_state_when_ac_mains_was_lost", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "always_powering_up_after_ac_mains_returns", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Power Cycle Interval Request
--------------------------------

fiid_template_t tmpl_cmd_set_power_cycle_interval_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

Set Power Cycle Interval Response
---------------------------------

fiid_template_t tmpl_cmd_set_power_cycle_interval_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get System Restart Cause Request
--------------------------------

fiid_template_t tmpl_cmd_get_system_restart_cause_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Restart Cause Response
---------------------------------

fiid_template_t tmpl_cmd_get_system_restart_cause_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "restart_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "channel", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Boot Options Request
-------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 296, "configuration_parameter_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE },
    { 0, "", 0},
  };

Set System Boot Options Response
--------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0},
  };

Set System Boot Options (Set In Progress) Request
-------------------------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

Set System Boot Options (Service Partition Selector) Request
------------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_service_partition_selector_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "service_partition_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Boot Options (Service Partition Scan) Request
--------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_service_partition_scan_rq =
{
  { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  { 1, "service_partition_discovered", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  { 1, "service_partition_scan", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  { 0, "", 0}
};

Set System Boot Options (BMC Boot Flag Valid Bit Clearing) Request
------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_power_up_via_power_pushbutton_or_wake_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_pushbutton_reset_soft_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_watchdog_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_automatically_clear_boot_flag_valid_bit_if_chassis_control_command_not_received_within_60_second_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_PEF", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Boot Options (Boot Info Acknowledge) Request
-------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_boot_info_acknowledge_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bios_or_post_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "os_loader_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "os_or_service_partition_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sms_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Boot Options (Boot Flags) Request
--------------------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_boot_flags_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bios_boot_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "boot_flags_persistent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "boot_flags_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_out_reset_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "screen_blank", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "boot_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_keyboard", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cmos_clear", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "console_redirection", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_out_sleep_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_password_bypass", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_progress_event_traps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "firmware_bios_verbosity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_out_via_power_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "bios_mux_control_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bios_shared_mode_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "device_instance_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Boot Options (Boot Initiator Info) Request
-----------------------------------------------------

fiid_template_t tmpl_cmd_set_system_boot_options_boot_initiator_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "boot_source.channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "boot_source.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "boot_info_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Boot Options (Boot Initiator Mailbox) Request
--------------------------------------------------------

/* achu: 16 bytes per block, 16*8 = 128 bits */
fiid_template_t tmpl_cmd_set_system_boot_options_boot_initiator_mailbox_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "block_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get System Boot Options Request
-------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

Get System Boot Options Response
--------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 296, "configuration_parameter_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0},
  };

Get System Boot Options (Set In Progress) Response
--------------------------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_set_in_progress_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Boot Options (Service Partition Selector) Response
-------------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_service_partition_selector_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "service_partition_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Boot Options (Service Partition Scan) Response
---------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_service_partition_scan_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "service_partition_discovered", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "service_partition_scan", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Boot Options (BMC Boot Flag Valid Bit Clearing) Response
-------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_BMC_boot_flag_valid_bit_clearing_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_power_up_via_power_pushbutton_or_wake_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_pushbutton_reset_soft_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_watchdog_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_automatically_clear_boot_flag_valid_bit_if_chassis_control_command_not_received_within_60_second_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_PEF", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Boot Options (Boot Info Acknowledge) Response
--------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_boot_info_acknowledge_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "enable_write_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bios_or_post_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "os_loader_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "os_or_service_partition_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sms_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Boot Options (Boot Flags) Response
---------------------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_boot_flags_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bios_boot_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "boot_flags_persistent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "boot_flags_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_out_reset_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "screen_blank", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "boot_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_keyboard", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cmos_clear", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "console_redirection", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_out_sleep_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_password_bypass", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_progress_event_traps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "firmware_bios_verbosity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "lock_out_via_power_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "bios_mux_control_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bios_shared_mode_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "device_instance_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

Get System Boot Options (Boot Initiator Info) Response
------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_boot_options_boot_initiator_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "boot_source.channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "boot_source.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "boot_info_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };
  
Get System Boot Options (Boot Initiator Mailbox) Response
---------------------------------------------------------

/* achu: 16 bytes per block, 16*8 = 128 bits */
fiid_template_t tmpl_cmd_get_system_boot_options_boot_initiator_mailbox_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "block_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0},
  };

Get Power On Hours Counter Request
----------------------------------

fiid_template_t tmpl_cmd_get_power_on_hours_counter_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Power On Hours Counter Response
-----------------------------------

fiid_template_t tmpl_cmd_get_power_on_hours_counter_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "minutes_per_counter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "counter_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_CHASSIS_CMDS_TEMPLATES_H */
