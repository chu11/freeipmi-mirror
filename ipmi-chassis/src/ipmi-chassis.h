/*
  Copyright (C) 2007-2009 FreeIPMI Core Team

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

#ifndef _IPMI_CHASSIS_H
#define _IPMI_CHASSIS_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

enum ipmi_chassis_argp_option_keys
  {
    GET_CHASSIS_CAPABILITIES_KEY = 'c',
    GET_CHASSIS_STATUS_KEY = 's',
    CHASSIS_CONTROL_KEY = 'O',
    CHASSIS_IDENTIFY_KEY = 'i',
    GET_SYSTEM_RESTART_CAUSE_KEY = 'R',
    GET_POWER_ON_HOURS_COUNTER_KEY = 'H',
    SET_POWER_CYCLE_INTERVAL_KEY = 'S',
    GET_BOOT_FLAGS_KEY = 'G',
    SET_BOOT_FLAGS_KEY = 'L',
    SET_BOOT_FLAGS_BOOT_TYPE_KEY = 160,
    SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY = 161,
    SET_BOOT_FLAGS_SCREEN_BLANK_KEY = 162,
    SET_BOOT_FLAGS_BOOT_DEVICE_KEY = 163,
    SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY = 164,
    SET_BOOT_FLAGS_CMOS_CLEAR_KEY = 165,
    SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY = 166,
    SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY = 167,
    SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY = 168,
    SET_BOOT_FLAGS_FIRMWARE_BIOS_VERBOSITY_KEY = 169,
    SET_POWER_RESTORE_POLICY_KEY = 'X',
  };

struct cmd_set_system_boot_options
{
  uint8_t bios_boot_type;
  int bios_boot_type_set;
  uint8_t lock_out_reset_button;
  int lock_out_reset_button_set;
  uint8_t screen_blank;
  int screen_blank_set;
  uint8_t boot_device;
  int boot_device_set;
  uint8_t lock_keyboard;
  int lock_keyboard_set;
  uint8_t cmos_clear;
  int cmos_clear_set;
  uint8_t console_redirection;
  int console_redirection_set;
  uint8_t user_password_bypass;
  int user_password_bypass_set;
  uint8_t force_progress_event_traps;
  int force_progress_event_traps_set;
  uint8_t firmware_bios_verbosity;
  int firmware_bios_verbosity_set;
};

struct cmd_chassis_identify
{
  uint8_t identify_interval;
  int identify_interval_set;
  uint8_t force_identify;
  int force_identify_set;
};

struct ipmi_chassis_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;
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

#endif
