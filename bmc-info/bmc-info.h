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

#ifndef BMC_INFO_H
#define BMC_INFO_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

enum bmc_info_argp_option_keys
  {
    GET_DEVICE_ID_KEY = 160,
    GUID_KEY = 'g',             /* legacy */
    GET_DEVICE_GUID_KEY = 161,
    GET_SYSTEM_GUID_KEY = 162,
    GET_SYSTEM_INFO_KEY = 163,
    GET_CHANNEL_INFO_KEY = 164,
    INTERPRET_OEM_DATA_KEY = 165,
  };

struct bmc_info_arguments
{
  struct common_cmd_args common_args;
  int get_device_id;
  int get_device_guid;
  int get_system_guid;
  int get_system_info;
  int get_channel_info;
  int interpret_oem_data;
};

typedef struct bmc_info_prog_data
{
  char *progname;
  struct bmc_info_arguments *args;
} bmc_info_prog_data_t;

typedef struct bmc_info_state_data
{
  bmc_info_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
} bmc_info_state_data_t;

#endif /* BMC_INFO_H */
