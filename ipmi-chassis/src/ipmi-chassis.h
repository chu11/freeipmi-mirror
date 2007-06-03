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

#include "argp-common.h"
#include "pstdout.h"

struct ipmi_chassis_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;
  int32_t cmd;
};

typedef struct ipmi_chassis_prog_data
{
  char *progname;
  struct ipmi_chassis_arguments *args;
  uint32_t debug_flags;
} ipmi_chassis_prog_data_t;

typedef struct ipmi_chassis_state_data
{
  ipmi_chassis_prog_data_t *prog_data;
  ipmi_device_t dev;
  pstdout_state_t pstate;
} ipmi_chassis_state_data_t;

#endif
