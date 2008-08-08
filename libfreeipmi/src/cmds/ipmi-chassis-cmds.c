/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <freeipmi/cmds/ipmi-chassis-cmds.h>
#include <freeipmi/api/ipmi-chassis-cmds-api.h>
#include <freeipmi/spec/ipmi-chassis-boot-options-parameter-spec.h>
#include <freeipmi/spec/ipmi-cmd-spec.h>

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_chassis_capabilities_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_chassis_capabilities_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "capabilities_flags.provides_intrusion_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "capabilities_flags.provides_front_panel_lockout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "capabilities_flags.provides_diagnostic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "capabilities_flags.provides_power_interlock", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "capabilities_flags.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "fru_info_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "sdr_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "sel_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "system_management_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "bridge_device_address", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_chassis_status_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_chassis_status_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_is_on", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.interlock", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_control_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "current_power_state.power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.ac_failed", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_caused_by_power_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_caused_by_power_interlock_being_activated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_caused_by_power_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_on_entered_via_ipmi", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "last_power_event.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.chassis_intrusion_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.front_panel_lockout_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.drive_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.cooling_fan_fault_detected", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "misc_chassis_state.chassis_identify_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.chassis_identify_command_and_state_info_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.power_off_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.reset_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.diagnostic_interrupt_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.standby_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.power_off_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.reset_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.diagnostic_interrupt_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.standby_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "chassis_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* NOTE: 
   The implementation is allowed to return the completion code prior
   to performing the selected control action if necessary. 
*/
fiid_template_t tmpl_cmd_chassis_control_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_identify_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "identify_interval", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "force_identify", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {7, "reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_identify_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_front_panel_enables_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "disable_power_off_button_for_power_off_only", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "disable_reset_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "disable_diagnostic_interrupt_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "disable_standby_button_for_entering_standby", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_front_panel_enables_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_power_restore_policy_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_power_restore_policy_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "powered_off_after_ac_mains_returns", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "restoring_power_to_state_when_ac_mains_was_lost", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "always_powering_up_after_ac_mains_returns", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_cmd_set_power_cycle_interval_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0},
  };

fiid_template_t tmpl_cmd_set_power_cycle_interval_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_restart_cause_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_restart_cause_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "restart_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "channel", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_boot_options_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid",  FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {296, "configuration_parameter_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE },
    {0, "", 0},
   };

fiid_template_t tmpl_cmd_set_system_boot_options_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0},
  };

fiid_template_t tmpl_cmd_set_system_boot_options_set_in_progress_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "set_in_progress", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0},
  };

fiid_template_t tmpl_cmd_set_system_boot_options_boot_info_acknowledge_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "bios_or_post_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "os_loader_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "os_or_service_partition_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "sms_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_power_up_via_power_pushbutton_or_wake_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_pushbutton_reset_soft_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_watchdog_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_automatically_clear_boot_flag_valid_bit_if_chassis_control_command_not_received_within_60_second_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_PEF", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_boot_options_boot_flags_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "bios_boot_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "boot_flags_persistent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "boot_flags_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_out_reset_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "screen_blank", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "boot_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_keyboard", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "cmos_clear", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "console_redirection", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_out_sleep_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_password_bypass", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "force_progress_event_traps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "firmware_bios_verbosity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_out_via_power_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "bios_mux_control_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "bios_shared_mode_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_boot_options_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0},
  };

fiid_template_t tmpl_cmd_get_system_boot_options_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {296, "configuration_parameter_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0},
 };

fiid_template_t tmpl_cmd_get_system_boot_options_boot_info_acknowledge_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "enable_write_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "bios_or_post_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "os_loader_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "os_or_service_partition_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "sms_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_handled_boot_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
 };

fiid_template_t tmpl_cmd_get_system_boot_options_BMC_boot_flag_valid_bit_clearing_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_power_up_via_power_pushbutton_or_wake_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_pushbutton_reset_soft_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_watchdog_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_automatically_clear_boot_flag_valid_bit_if_chassis_control_command_not_received_within_60_second_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_PEF", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_boot_options_boot_flags_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "parameter_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "bios_boot_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "boot_flags_persistent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "boot_flags_valid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_out_reset_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "screen_blank", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "boot_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_keyboard", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "cmos_clear", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "console_redirection", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_out_sleep_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_password_bypass", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "force_progress_event_traps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "firmware_bios_verbosity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "lock_out_via_power_button", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "bios_mux_control_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "bios_shared_mode_override", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0},
  };

fiid_template_t tmpl_cmd_get_power_on_hours_counter_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_power_on_hours_counter_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "minutes_per_counter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "counter_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0},
  };

int8_t
fill_cmd_get_chassis_capabilities (fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_chassis_capabilities_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
 
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHASSIS_CAPABILITIES);
  return 0;
}

int8_t
fill_cmd_get_chassis_status (fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_chassis_status_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHASSIS_STATUS);
  return 0; 
}

int8_t
fill_cmd_chassis_control (uint8_t chassis_control,
                          fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHASSIS_CONTROL_VALID (chassis_control)
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_chassis_control_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_CHASSIS_CONTROL);
  FIID_OBJ_SET (obj_cmd_rq, "chassis_control", chassis_control);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  return 0;
}  

int8_t
fill_cmd_chassis_identify (uint8_t *identify_interval, 
                           uint8_t *force_identify,
                           fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL ((!force_identify 
               || IPMI_CHASSIS_FORCE_IDENTIFY_VALID (*force_identify))
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_chassis_identify_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_CHASSIS_IDENTIFY);
  if (identify_interval)
    {
      FIID_OBJ_SET (obj_cmd_rq, "identify_interval", *identify_interval);
      if (force_identify)
        {
          FIID_OBJ_SET (obj_cmd_rq, "force_identify", *force_identify);
          FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
        }
    }

  return 0;
}  

int8_t
fill_cmd_set_front_panel_enables (uint8_t disable_power_off_button_for_power_off_only,
                                  uint8_t disable_reset_button,
                                  uint8_t disable_diagnostic_interrupt_button,
                                  uint8_t disable_standby_button_for_entering_standby,
                                  fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHASSIS_BUTTON_VALID(disable_power_off_button_for_power_off_only)
              && IPMI_CHASSIS_BUTTON_VALID(disable_reset_button)
              && IPMI_CHASSIS_BUTTON_VALID(disable_diagnostic_interrupt_button)
              && IPMI_CHASSIS_BUTTON_VALID(disable_standby_button_for_entering_standby)
              && fiid_obj_valid (obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_front_panel_enables_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_FRONT_PANEL_ENABLES);
  FIID_OBJ_SET (obj_cmd_rq, "disable_power_off_button_for_power_off_only", disable_power_off_button_for_power_off_only);
  FIID_OBJ_SET (obj_cmd_rq, "disable_reset_button", disable_reset_button);
  FIID_OBJ_SET (obj_cmd_rq, "disable_diagnostic_interrupt_button", disable_diagnostic_interrupt_button);
  FIID_OBJ_SET (obj_cmd_rq, "disable_standby_button_for_entering_standby", disable_standby_button_for_entering_standby);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  
  return 0;
}

int8_t 
fill_cmd_set_power_restore_policy (uint8_t power_restore_policy,
                                   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_POWER_RESTORE_POLICY_VALID (power_restore_policy)
              && fiid_obj_valid (obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_power_restore_policy_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq,
                "cmd", 
                IPMI_CMD_SET_POWER_RESTORE_POLICY); 

  FIID_OBJ_SET (obj_cmd_rq, "power_restore_policy", power_restore_policy);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  
  return 0;
}

int8_t 
fill_cmd_set_power_cycle_interval (uint8_t interval, 
                                   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_power_cycle_interval_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_POWER_CYCLE_INTERVAL);
  FIID_OBJ_SET (obj_cmd_rq, "interval", interval);
  return 0;
}

int8_t 
fill_cmd_get_system_restart_cause (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_system_restart_cause_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_RESTART_CAUSE);
  return 0;
}

int8_t 
fill_cmd_set_system_boot_options (uint8_t parameter_selector,
                                  uint8_t *configuration_parameter_data,
                                  uint8_t data_len,
                                  fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_SELECTOR_VALID (parameter_selector) 
              && configuration_parameter_data != NULL 
              && data_len > 0
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_rq);
  
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED);

  FIID_OBJ_SET_DATA (obj_cmd_rq, "configuration_parameter_data", configuration_parameter_data, data_len); 
  return 0;
}

int8_t
fill_cmd_set_system_boot_options_set_in_progress (uint8_t value,
                                                 fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHASSIS_BOOT_OPTIONS_SET_IN_PROGRESS_VALID (value)
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_set_in_progress_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);

  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_SET_IN_PROGRESS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED);

  FIID_OBJ_SET (obj_cmd_rq, "set_in_progress", value);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return 0;
}

int8_t
fill_cmd_set_system_boot_options_boot_info_acknowledge (uint8_t *bios_or_post_handled_boot_info,
                                                        uint8_t *os_loader_handled_boot_info,
                                                        uint8_t *os_or_service_partition_handled_boot_info,
                                                        uint8_t *sms_handled_boot_info,
                                                        uint8_t *oem_handled_boot_info,
                                                        fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL ((!bios_or_post_handled_boot_info 
                  || IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (*bios_or_post_handled_boot_info))
              && (!os_loader_handled_boot_info 
                  || IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (*os_loader_handled_boot_info))
              && (!os_or_service_partition_handled_boot_info 
                  || IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (*os_or_service_partition_handled_boot_info))
              && (!sms_handled_boot_info 
                  || IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (*sms_handled_boot_info))
              && (!oem_handled_boot_info 
                  || IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (*oem_handled_boot_info))
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_boot_info_acknowledge_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_BOOT_INFO_ACKNOWLEDGE);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED);

  if (bios_or_post_handled_boot_info)
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_0", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
      FIID_OBJ_SET (obj_cmd_rq, "bios_or_post_handled_boot_info", *bios_or_post_handled_boot_info);
    }
  else
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_0", IPMI_CHASSIS_BOOT_OPTIONS_DISABLE);
      FIID_OBJ_SET (obj_cmd_rq, "bios_or_post_handled_boot_info", IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (os_loader_handled_boot_info)
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_1", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
      FIID_OBJ_SET (obj_cmd_rq, "os_loader_handled_boot_info", *os_loader_handled_boot_info);
    }
  else
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_1", IPMI_CHASSIS_BOOT_OPTIONS_DISABLE);
      FIID_OBJ_SET (obj_cmd_rq, "os_loader_handled_boot_info", IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (os_or_service_partition_handled_boot_info)
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_2", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
      FIID_OBJ_SET (obj_cmd_rq, "os_or_service_partition_handled_boot_info", *os_or_service_partition_handled_boot_info);
    }
  else
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_2", IPMI_CHASSIS_BOOT_OPTIONS_DISABLE);
      FIID_OBJ_SET (obj_cmd_rq, "os_or_service_partition_handled_boot_info", IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (sms_handled_boot_info)
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_3", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
      FIID_OBJ_SET (obj_cmd_rq, "sms_handled_boot_info", *sms_handled_boot_info);
    }
  else
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_3", IPMI_CHASSIS_BOOT_OPTIONS_DISABLE);
      FIID_OBJ_SET (obj_cmd_rq, "sms_handled_boot_info", IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_UNACKNOWLEDGE);
    }

  if (oem_handled_boot_info)
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_4", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
      FIID_OBJ_SET (obj_cmd_rq, "oem_handled_boot_info", *oem_handled_boot_info);
    }
  else
    {
      FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_4", IPMI_CHASSIS_BOOT_OPTIONS_DISABLE);
      FIID_OBJ_SET (obj_cmd_rq, "oem_handled_boot_info", IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_UNACKNOWLEDGE);
    }

  FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_5", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
  FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_6", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
  FIID_OBJ_SET (obj_cmd_rq, "enable_write_bit_7", IPMI_CHASSIS_BOOT_OPTIONS_ENABLE);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0x7);
  return 0;
}

int8_t
fill_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing (uint8_t dont_clear_on_power_up,
                                                                   uint8_t dont_clear_on_pushbutton_reset_soft_reset,
                                                                   uint8_t dont_clear_on_watchdog_timeout,
                                                                   uint8_t dont_clear_on_chassis_control,
                                                                   uint8_t dont_clear_on_PEF,
                                                                   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT_VALID (dont_clear_on_power_up)
              && IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT_VALID (dont_clear_on_pushbutton_reset_soft_reset)
              && IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT_VALID (dont_clear_on_watchdog_timeout)
              && IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT_VALID (dont_clear_on_chassis_control)
              && IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT_VALID (dont_clear_on_PEF)
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing_rq);
  
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_BMC_BOOT_FLAG_VALID_BIT_CLEARING);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED);
  FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_power_up_via_power_pushbutton_or_wake_event", dont_clear_on_power_up);
  FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_pushbutton_reset_soft_reset", dont_clear_on_pushbutton_reset_soft_reset);
  FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_watchdog_timeout", dont_clear_on_watchdog_timeout);
  FIID_OBJ_SET (obj_cmd_rq, "dont_automatically_clear_boot_flag_valid_bit_if_chassis_control_command_not_received_within_60_second_timeout", dont_clear_on_chassis_control);
  FIID_OBJ_SET (obj_cmd_rq, "dont_clear_valid_bit_on_reset_power_cycle_caused_by_PEF", dont_clear_on_PEF);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  
  return 0;
}

int8_t
fill_cmd_set_system_boot_options_boot_flags (uint8_t bios_boot_type,
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
                                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (boot_flags_valid)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (boot_flags_persistent)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (cmos_clear)
              && IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BIOS_BOOT_TYPE_VALID (bios_boot_type)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (lock_keyboard)
              && IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_VALID (boot_device)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (screen_blank)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (lock_out_reset_button)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (lock_out_via_power_button)
              && IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VALID (firmware_bios_verbosity)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (force_progress_event_traps)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (user_password_bypass)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (lock_out_sleep_button)
              && IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_VALID (console_redirection)
              && IPMI_CHASSIS_BOOT_OPTIONS_ENABLE_VALID (bios_shared_mode_override)
              && IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAGS_BIOS_MUX_CONTROL_OVERRIDE_VALID (bios_mux_control_override)
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_boot_options_boot_flags_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_BOOT_FLAGS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_valid", IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "bios_boot_type", bios_boot_type);
  FIID_OBJ_SET (obj_cmd_rq, "boot_flags_persistent", boot_flags_persistent);
  FIID_OBJ_SET (obj_cmd_rq, "boot_flags_valid", boot_flags_valid);
  FIID_OBJ_SET (obj_cmd_rq, "lock_out_reset_button", lock_out_reset_button);
  FIID_OBJ_SET (obj_cmd_rq, "screen_blank", screen_blank);
  FIID_OBJ_SET (obj_cmd_rq, "boot_device", boot_device);
  FIID_OBJ_SET (obj_cmd_rq, "lock_keyboard", lock_keyboard);
  FIID_OBJ_SET (obj_cmd_rq, "cmos_clear", cmos_clear);
  FIID_OBJ_SET (obj_cmd_rq, "console_redirection", console_redirection);
  FIID_OBJ_SET (obj_cmd_rq, "lock_out_sleep_button", lock_out_sleep_button);
  FIID_OBJ_SET (obj_cmd_rq, "user_password_bypass", user_password_bypass);
  FIID_OBJ_SET (obj_cmd_rq, "force_progress_event_traps", force_progress_event_traps);
  FIID_OBJ_SET (obj_cmd_rq, "firmware_bios_verbosity", firmware_bios_verbosity);
  FIID_OBJ_SET (obj_cmd_rq, "lock_out_via_power_button", lock_out_via_power_button);
  FIID_OBJ_SET (obj_cmd_rq, "bios_mux_control_override", bios_mux_control_override);
  FIID_OBJ_SET (obj_cmd_rq, "bios_shared_mode_override", bios_shared_mode_override);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);

  return 0;
}

int8_t 
fill_cmd_get_system_boot_options (uint8_t parameter_selector,
                                 uint8_t set_selector,
                                 uint8_t block_selector,
                                 fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHASSIS_BOOT_OPTIONS_PARAMETER_SELECTOR_VALID (parameter_selector)
              && fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_system_boot_options_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_BOOT_OPTIONS);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FIID_OBJ_SET (obj_cmd_rq, "block_selector", block_selector);
  return 0;
}
  
int8_t
fill_cmd_get_power_on_hours_counter (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid (obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_power_on_hours_counter_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_POWER_ON_HOURS_COUNTER);
  return 0;
}

