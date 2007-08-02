/* 
   ipmi-chassis.h - IPMI Chassis information
   
   Copyright (C) 2007 FreeIPMI Core Team
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _IPMI_CHASSIS_H
#define _IPMI_CHASSIS_H

#include <freeipmi/freeipmi.h>

#include "cmdline-parse-common.h"
#include "pstdout.h"

enum ipmi_chassis_keys
  {
    IPMI_CHASSIS_KEY_BOOT_TYPE = 150,
    IPMI_CHASSIS_KEY_LOCK_OUT_RESET_BUTTON,
    IPMI_CHASSIS_KEY_SCREEN_BLANK,
    IPMI_CHASSIS_KEY_BOOT_DEVICE_SELECTOR,
    IPMI_CHASSIS_KEY_LOCK_KEYBOARD,
    IPMI_CHASSIS_KEY_CLEAR_CMOS,
    IPMI_CHASSIS_KEY_CONSOLE_REDIRECTION,
    IPMI_CHASSIS_KEY_USER_PASSWORD_BYPASS,
    IPMI_CHASSIS_KEY_FORCE_PROGRESS_EVENT_TRAPS,
    IPMI_CHASSIS_KEY_FIRMWARE_BIOS_VERBOSITY,
  };

struct cmd_boot_option
{
  int8_t bios_boot_type;
  int8_t lock_out_reset_button;
  int8_t screen_blank;
  int8_t boot_device_selector;
  int8_t lock_keyboard;
  int8_t clear_cmos;
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

  union cmd_args {
    uint8_t chassis_control;
    uint8_t power_restore_policy;
    uint8_t power_cycle_interval;
    
    struct cmd_identify identify_args;
    struct cmd_boot_option boot_option_args;
  } args;
};

typedef struct ipmi_chassis_prog_data
{
  char *progname;
  struct ipmi_chassis_arguments *args;
} ipmi_chassis_prog_data_t;

typedef struct ipmi_chassis_state_data
{
  ipmi_chassis_prog_data_t *prog_data;
  ipmi_device_t dev;
  pstdout_state_t pstate;
} ipmi_chassis_state_data_t;

#endif
