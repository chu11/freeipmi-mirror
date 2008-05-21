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

#ifndef _BMC_INFO_H
#define _BMC_INFO_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

enum bmc_info_argp_option_keys
  {
    CMD_GUID_KEY = 'g',
  };

struct bmc_info_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;
  int guid;
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

#endif
