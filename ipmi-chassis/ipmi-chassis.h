/*
 * Copyright (C) 2007-2014 FreeIPMI Core Team
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

#ifndef IPMI_CHASSIS_H
#define IPMI_CHASSIS_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

enum ipmi_chassis_argp_option_keys
  {
    GET_CHASSIS_CAPABILITIES_KEY = 160,
    GET_CHASSIS_STATUS_KEY = 161,
    CHASSIS_CONTROL_KEY = 162,
    CHASSIS_IDENTIFY_KEY = 163,
    GET_SYSTEM_RESTART_CAUSE_KEY = 164,
    GET_POWER_ON_HOURS_COUNTER_KEY = 165,
    SET_POWER_CYCLE_INTERVAL_KEY = 166,
    GET_BOOT_FLAGS_KEY = 167,
    SET_BOOT_FLAGS_KEY = 168,
    SET_BOOT_FLAGS_BOOT_TYPE_KEY = 169,
    SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY = 170,
    SET_BOOT_FLAGS_SCREEN_BLANK_KEY = 171,
    SET_BOOT_FLAGS_BOOT_DEVICE_KEY = 172,
    SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY = 173,
    SET_BOOT_FLAGS_CMOS_CLEAR_KEY = 174,
    SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY = 175,
    SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY = 176,
    SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY = 177,
    SET_BOOT_FLAGS_FIRMWARE_BIOS_VERBOSITY_KEY = 178,
    SET_POWER_RESTORE_POLICY_KEY = 179,
  };

struct cmd_set_system_boot_options
{
  int bios_boot_type;
  uint8_t bios_boot_type_arg;
  int lock_out_reset_button;
  uint8_t lock_out_reset_button_arg;
  int screen_blank;
  uint8_t screen_blank_arg;
  int boot_device;
  uint8_t boot_device_arg;
  int lock_keyboard;
  uint8_t lock_keyboard_arg;
  int cmos_clear;
  uint8_t cmos_clear_arg;
  int console_redirection;
  uint8_t console_redirection_arg;
  int user_password_bypass;
  uint8_t user_password_bypass_arg;
  int force_progress_event_traps;
  uint8_t force_progress_event_traps_arg;
  int firmware_bios_verbosity;
  uint8_t firmware_bios_verbosity_arg;
};

struct cmd_chassis_identify
{
  int identify_interval;
  uint8_t identify_interval_arg;
  int force_identify;
  uint8_t force_identify_arg;
};

struct ipmi_chassis_arguments
{
  struct common_cmd_args common_args;
  int get_chassis_capabilities;
  int get_chassis_status;
  int chassis_control;
  uint8_t chassis_control_arg;
  int chassis_identify;
  struct cmd_chassis_identify chassis_identify_args;
  int set_power_restore_policy;
  uint8_t set_power_restore_policy_arg;
  int set_power_cycle_interval;
  uint8_t set_power_cycle_interval_arg;
  int get_system_restart_cause;
  int set_system_boot_options;
  struct cmd_set_system_boot_options set_system_boot_options_args;
  int get_system_boot_options;
  int get_power_on_hours_counter;
};

typedef struct ipmi_chassis_prog_data
{
  char *progname;
  struct ipmi_chassis_arguments *args;
} ipmi_chassis_prog_data_t;

typedef struct ipmi_chassis_state_data
{
  ipmi_chassis_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
} ipmi_chassis_state_data_t;

#endif /* IPMI_CHASSIS_H */
