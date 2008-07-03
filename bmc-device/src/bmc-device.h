/* 
   Copyright (C) 2008 FreeIPMI Core Team
   
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

#ifndef _BMC_DEVICE_H
#define _BMC_DEVICE_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

enum bmc_device_argp_option_keys
  {
    CMD_COLD_RESET_KEY = 160,
    CMD_WARM_RESET_KEY = 161,
    CMD_GET_SELF_TEST_RESULTS_KEY = 162,
    CMD_GET_ACPI_POWER_STATE_KEY = 163,
    VERBOSE_KEY = 164,
  };

struct bmc_device_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;
  int cold_reset;
  int warm_reset;
  int get_self_test_results;
  int get_acpi_power_state;
  int verbose;
};

typedef struct bmc_device_prog_data
{
  char *progname;
  struct bmc_device_arguments *args;
} bmc_device_prog_data_t;

typedef struct bmc_device_state_data
{
  bmc_device_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
} bmc_device_state_data_t;

#endif
