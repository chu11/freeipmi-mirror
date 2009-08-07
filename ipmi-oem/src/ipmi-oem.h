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

#ifndef _IPMI_OEM_H
#define _IPMI_OEM_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

enum ipmi_sensors_argp_option_keys
  {
    LIST_KEY = 'L',
    VERBOSE_KEY = 'v',
  };

struct ipmi_oem_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;
  int list;
  int verbose_count;
  char *oem_id;
  char *oem_command;
  char **oem_options;
  unsigned int oem_options_count;
  long arg_max;
};

typedef struct ipmi_oem_prog_data
{
  char *progname;
  struct ipmi_oem_arguments *args;
} ipmi_oem_prog_data_t;

typedef struct ipmi_oem_state_data
{
  ipmi_oem_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
} ipmi_oem_state_data_t;

#endif
