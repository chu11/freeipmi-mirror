/*
 * Copyright (C) 2007-2015 FreeIPMI Core Team
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
  int get_system_restart_cause;
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
