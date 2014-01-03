/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-chassis-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-system-boot-option-parameters-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_chassis_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_get_chassis_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_chassis_control_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "chassis_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* NOTE:
   The implementation is allowed to return the completion code prior
   to performing the selected control action if necessary.
*/
fiid_template_t tmpl_cmd_chassis_control_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_identify_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "identify_interval", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_identify", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_identify_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_set_front_panel_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_power_restore_policy_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_set_power_cycle_interval_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

fiid_template_t tmpl_cmd_set_power_cycle_interval_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_restart_cause_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_restart_cause_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "restart_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "channel", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_boot_options_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 296, "configuration_parameter_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE },
    { 0, "", 0},
  };

fiid_template_t tmpl_cmd_set_system_boot_options_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0},
  };

fiid_template_t tmpl_cmd_set_system_boot_options_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

fiid_template_t tmpl_cmd_set_system_boot_options_service_partition_selector_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "service_partition_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_get_system_boot_options_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

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

fiid_template_t tmpl_cmd_get_power_on_hours_counter_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_power_on_hours_counter_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "minutes_per_counter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "counter_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0},
  };

int
fill_cmd_get_chassis_capabilities (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_chassis_capabilities_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHASSIS_CAPABILITIES);
  return (0);
}

int
fill_cmd_get_chassis_status (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_chassis_status_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHASSIS_STATUS);
  return (0);
}

int
fill_cmd_chassis_control (uint8_t chassis_control,
                          fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHASSIS_CONTROL_VALID (chassis_control)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_chassis_control_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_CHASSIS_CONTROL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "chassis_control", chassis_control);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  return (0);
}

int
fill_cmd_chassis_identify (const uint8_t *identify_interval,
                           const uint8_t *force_identify,
                           fiid_obj_t obj_cmd_rq)
{
  if ((force_identify
       && !IPMI_CHASSIS_FORCE_IDENTIFY_VALID (*force_identify))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_chassis_identify_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_CHASSIS_IDENTIFY);
  if (identify_interval)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "identify_interval", *identify_interval);
      if (force_identify)
        {
          FILL_FIID_OBJ_SET (obj_cmd_rq, "force_identify", *force_identify);
          FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
        }
    }

  return (0);
}

int
fill_cmd_set_front_panel_enables (uint8_t disable_power_off_button_for_power_off_only,
                                  uint8_t disable_reset_button,
                                  uint8_t disable_diagnostic_interrupt_button,
                                  uint8_t disable_standby_button_for_entering_standby,
                                  fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHASSIS_BUTTON_VALID (disable_power_off_button_for_power_off_only)
      || !IPMI_CHASSIS_BUTTON_VALID (disable_reset_button)
      || !IPMI_CHASSIS_BUTTON_VALID (disable_diagnostic_interrupt_button)
      || !IPMI_CHASSIS_BUTTON_VALID (disable_standby_button_for_entering_standby)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_front_panel_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_FRONT_PANEL_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "disable_power_off_button_for_power_off_only", disable_power_off_button_for_power_off_only);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "disable_reset_button", disable_reset_button);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "disable_diagnostic_interrupt_button", disable_diagnostic_interrupt_button);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "disable_standby_button_for_entering_standby", disable_standby_button_for_entering_standby);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_set_power_restore_policy (uint8_t power_restore_policy,
                                   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_POWER_RESTORE_POLICY_VALID (power_restore_policy)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_power_restore_policy_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "cmd",
                     IPMI_CMD_SET_POWER_RESTORE_POLICY);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "power_restore_policy", power_restore_policy);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_set_power_cycle_interval (uint8_t interval,
                                   fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_power_cycle_interval_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_POWER_CYCLE_INTERVAL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "interval", interval);
  return (0);
}

int
fill_cmd_get_system_restart_cause (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_system_restart_cause_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_RESTART_CAUSE);
  return (0);
}

int
fill_cmd_set_system_boot_options (uint8_t parameter_selector,
                                  uint8_t parameter_valid,
                                  const void *configuration_parameter_data,
                                  unsigned int configuration_parameter_data_len,
                                  fiid_obj_t obj_cmd_rq)
{
  if ((!IPMI_SYSTEM_BOOT_OPTION_PARAMETER_SELECTOR_VALID (parameter_selector)
       && !IPMI_SYSTEM_BOOT_OPTION_PARAMETER_SELECTOR_IS_OEM (parameter_selector))
      || !IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || !configuration_parameter_data
      || !configuration_parameter_data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);

  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "configuration_parameter_data",
                          configuration_parameter_data,
                          configuration_parameter_data_len);
  return (0);
}

int
fill_cmd_set_system_boot_options_set_in_progress (uint8_t parameter_valid,
                                                  uint8_t state,
                                                  fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || !IPMI_SYSTEM_BOOT_OPTION_SET_IN_PROGRESS_VALID (state)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_set_in_progress_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_SET_IN_PROGRESS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "state", state);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_set_system_boot_options_service_partition_selector (uint8_t parameter_valid,
                                                             uint8_t service_partition_selector,
                                                             fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_service_partition_selector_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_SERVICE_PARTITION_SELECTOR);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "service_partition_selector", service_partition_selector);

  return (0);
}

int
fill_cmd_set_system_boot_options_service_partition_scan (uint8_t parameter_valid,
                                                         uint8_t service_partition_discovered,
                                                         uint8_t service_partition_scan,
                                                         fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || !IPMI_SYSTEM_BOOT_OPTION_SERVICE_PARTITION_DISCOVERED_VALID (service_partition_discovered)
      || !IPMI_SYSTEM_BOOT_OPTION_SERVICE_PARTITION_SCAN_VALID (service_partition_scan)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_service_partition_scan_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  
  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_SERVICE_PARTITION_SCAN);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);
  
  FILL_FIID_OBJ_SET (obj_cmd_rq, "service_partition_discovered", service_partition_discovered);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "service_partition_scan", service_partition_scan);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing (uint8_t parameter_valid,
                                                                   uint8_t dont_clear_on_power_up,
                                                                   uint8_t dont_clear_on_pushbutton_reset_soft_reset,
                                                                   uint8_t dont_clear_on_watchdog_timeout,
                                                                   uint8_t dont_clear_on_chassis_control,
                                                                   uint8_t dont_clear_on_PEF,
                                                                   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || !IPMI_SYSTEM_BOOT_OPTION_CLEAR_VALID_BIT_VALID (dont_clear_on_power_up)
      || !IPMI_SYSTEM_BOOT_OPTION_CLEAR_VALID_BIT_VALID (dont_clear_on_pushbutton_reset_soft_reset)
      || !IPMI_SYSTEM_BOOT_OPTION_CLEAR_VALID_BIT_VALID (dont_clear_on_watchdog_timeout)
      || !IPMI_SYSTEM_BOOT_OPTION_CLEAR_VALID_BIT_VALID (dont_clear_on_chassis_control)
      || !IPMI_SYSTEM_BOOT_OPTION_CLEAR_VALID_BIT_VALID (dont_clear_on_PEF)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_BMC_BOOT_FLAG_VALID_BIT_CLEARING);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_power_up_via_power_pushbutton_or_wake_event", dont_clear_on_power_up);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_pushbutton_reset_soft_reset", dont_clear_on_pushbutton_reset_soft_reset);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_watchdog_timeout", dont_clear_on_watchdog_timeout);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "dont_automatically_clear_boot_flag_valid_bit_if_chassis_control_command_not_received_within_60_second_timeout", dont_clear_on_chassis_control);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_PEF", dont_clear_on_PEF);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_set_system_boot_options_boot_info_acknowledge (uint8_t parameter_valid,
                                                        const uint8_t *bios_or_post_handled_boot_info,
                                                        const uint8_t *os_loader_handled_boot_info,
                                                        const uint8_t *os_or_service_partition_handled_boot_info,
                                                        const uint8_t *sms_handled_boot_info,
                                                        const uint8_t *oem_handled_boot_info,
                                                        fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || (bios_or_post_handled_boot_info
          && !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (*bios_or_post_handled_boot_info))
      || (os_loader_handled_boot_info
          && !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (*os_loader_handled_boot_info))
      || (os_or_service_partition_handled_boot_info
          && !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (*os_or_service_partition_handled_boot_info))
      || (sms_handled_boot_info
          && !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (*sms_handled_boot_info))
      || (oem_handled_boot_info
          && !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (*oem_handled_boot_info))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_boot_info_acknowledge_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_BOOT_INFO_ACKNOWLEDGE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);

  if (bios_or_post_handled_boot_info)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_0", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "bios_or_post_handled_boot_info", *bios_or_post_handled_boot_info);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_0", IPMI_SYSTEM_BOOT_OPTION_DISABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "bios_or_post_handled_boot_info", IPMI_SYSTEM_BOOT_OPTION_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (os_loader_handled_boot_info)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_1", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "os_loader_handled_boot_info", *os_loader_handled_boot_info);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_1", IPMI_SYSTEM_BOOT_OPTION_DISABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "os_loader_handled_boot_info", IPMI_SYSTEM_BOOT_OPTION_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (os_or_service_partition_handled_boot_info)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_2", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "os_or_service_partition_handled_boot_info", *os_or_service_partition_handled_boot_info);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_2", IPMI_SYSTEM_BOOT_OPTION_DISABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "os_or_service_partition_handled_boot_info", IPMI_SYSTEM_BOOT_OPTION_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (sms_handled_boot_info)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_3", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "sms_handled_boot_info", *sms_handled_boot_info);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_3", IPMI_SYSTEM_BOOT_OPTION_DISABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "sms_handled_boot_info", IPMI_SYSTEM_BOOT_OPTION_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (oem_handled_boot_info)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_4", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_handled_boot_info", *oem_handled_boot_info);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_4", IPMI_SYSTEM_BOOT_OPTION_DISABLE);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_handled_boot_info", IPMI_SYSTEM_BOOT_OPTION_BOOT_INFO_UNACKNOWLEDGE);
    }

  FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_5", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_6", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_7", IPMI_SYSTEM_BOOT_OPTION_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0x7);
  return (0);
}

int
fill_cmd_set_system_boot_options_boot_flags (uint8_t parameter_valid,
                                             uint8_t bios_boot_type,
                                             uint8_t boot_flags_persistent,
                                             uint8_t boot_flags_valid,
                                             uint8_t lock_out_reset_button,
                                             uint8_t screen_blank,
                                             uint8_t boot_device,
                                             uint8_t lock_keyboard,
                                             uint8_t cmos_clear,
                                             uint8_t console_redirection,
                                             uint8_t lock_out_sleep_button,
                                             uint8_t user_password_bypass,
                                             uint8_t force_progress_event_traps,
                                             uint8_t firmware_bios_verbosity,
                                             uint8_t lock_out_via_power_button,
                                             uint8_t bios_mux_control_override,
                                             uint8_t bios_shared_mode_override,
                                             uint8_t device_instance_selector,
                                             fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (boot_flags_valid)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (boot_flags_persistent)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (cmos_clear)
      || !IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BIOS_BOOT_TYPE_VALID (bios_boot_type)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (lock_keyboard)
      || !IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_VALID (boot_device)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (screen_blank)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (lock_out_reset_button)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (lock_out_via_power_button)
      || !IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VALID (firmware_bios_verbosity)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (force_progress_event_traps)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (user_password_bypass)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (lock_out_sleep_button)
      || !IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_VALID (console_redirection)
      || !IPMI_SYSTEM_BOOT_OPTION_ENABLE_VALID (bios_shared_mode_override)
      || !IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAGS_BIOS_MUX_CONTROL_OVERRIDE_VALID (bios_mux_control_override)
      || !IPMI_SYSTEM_BOOT_OPTION_DEVICE_INSTANCE_SELECTOR_VALID (device_instance_selector)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_boot_flags_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_BOOT_FLAGS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "bios_boot_type", bios_boot_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "boot_flags_persistent", boot_flags_persistent);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "boot_flags_valid", boot_flags_valid);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lock_out_reset_button", lock_out_reset_button);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "screen_blank", screen_blank);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "boot_device", boot_device);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lock_keyboard", lock_keyboard);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmos_clear", cmos_clear);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "console_redirection", console_redirection);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lock_out_sleep_button", lock_out_sleep_button);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_password_bypass", user_password_bypass);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "force_progress_event_traps", force_progress_event_traps);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "firmware_bios_verbosity", firmware_bios_verbosity);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lock_out_via_power_button", lock_out_via_power_button);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "bios_mux_control_override", bios_mux_control_override);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "bios_shared_mode_override", bios_shared_mode_override);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "device_instance_selector", device_instance_selector); 
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);

  return (0);
}

int
fill_cmd_set_system_boot_options_boot_initiator_info (uint8_t parameter_valid,
                                                      uint8_t boot_source_channel_number,
                                                      uint32_t session_id,
                                                      uint32_t boot_info_timestamp,
                                                      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || !IPMI_CHANNEL_NUMBER_VALID (boot_source_channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_boot_initiator_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_BOOT_INITIATOR_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "boot_source.channel_number", boot_source_channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "boot_source.reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "session_id", session_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "boot_info_timestamp", boot_info_timestamp);

  return (0);
}

int
fill_cmd_set_system_boot_options_boot_initiator_mailbox (uint8_t parameter_valid,
                                                         uint8_t set_selector,
                                                         const void *block_data,
                                                         unsigned int block_data_length,
                                                         fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_VALID (parameter_valid)
      || block_data_length > IPMI_SYSTEM_BOOT_OPTION_BLOCK_DATA_LEN_MAX
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_boot_initiator_mailbox_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  
  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_BOOT_OPTION_PARAMETER_BOOT_INITIATOR_MAILBOX);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", parameter_valid);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  if (block_data && block_data_length)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "block_data", block_data, block_data_length);
  
  return (0);
}

int
fill_cmd_get_system_boot_options (uint8_t parameter_selector,
                                  uint8_t set_selector,
                                  uint8_t block_selector,
                                  fiid_obj_t obj_cmd_rq)
{
  if ((!IPMI_SYSTEM_BOOT_OPTION_PARAMETER_SELECTOR_VALID (parameter_selector)
       && !IPMI_SYSTEM_BOOT_OPTION_PARAMETER_SELECTOR_IS_OEM (parameter_selector))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_system_boot_options_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_BOOT_OPTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "block_selector", block_selector);
  return (0);
}

int
fill_cmd_get_power_on_hours_counter (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_power_on_hours_counter_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_POWER_ON_HOURS_COUNTER);
  return (0);
}

