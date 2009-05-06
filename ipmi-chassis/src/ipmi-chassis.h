/* 
   Copyright (C) 2007-2008 FreeIPMI Core Team
   
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
    GET_CAPABILITIES_KEY = 'c',
    GET_STATUS_KEY = 's',
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

enum ipmi_chassis_cmds
  {
    CHASSIS_CMD_GET_CHASSIS_CAPABILITIES = 0,
    CHASSIS_CMD_GET_CHASSIS_STATUS = 1,
    CHASSIS_CMD_CHASSIS_CONTROL = 2,
    CHASSIS_CMD_CHASSIS_IDENTIFY = 3,
    CHASSIS_CMD_SET_POWER_RESTORE_POLICY = 4, 
    CHASSIS_CMD_SET_POWER_CYCLE_INTERVAL = 5,
    CHASSIS_CMD_GET_SYSTEM_RESTART_CAUSE = 6,
    CHASSIS_CMD_GET_POWER_ON_HOURS_COUNTER = 7,
    CHASSIS_CMD_SET_SYSTEM_BOOT_OPTIONS = 8,
    CHASSIS_CMD_GET_SYSTEM_BOOT_OPTIONS = 9,
  };

struct cmd_boot_option
{
  int8_t bios_boot_type;
  int8_t lock_out_reset_button;
  int8_t screen_blank;
  int8_t boot_device;
  int8_t lock_keyboard;
  int8_t cmos_clear;
  int8_t console_redirection;
  int8_t user_password_bypass;
  int8_t force_progress_event_traps;
  int8_t firmware_bios_verbosity;
};

struct cmd_identify 
{
  uint8_t identify_interval;
  uint8_t identify_interval_set;
  uint8_t force_identify;
  uint8_t force_identify_set;
};

struct ipmi_chassis_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;
  int32_t cmd;

  uint8_t chassis_control;
  uint8_t power_restore_policy;
  uint8_t power_cycle_interval;
  struct cmd_identify identify_args;
  struct cmd_boot_option boot_option_args;
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
