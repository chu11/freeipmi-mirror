/* 
   $Id: ipmi-raw.h,v 1.1 2007-01-22 23:39:27 chu11 Exp $ 
   
   ipmi-raw.h - ipmi-raw command line argument parser.
   
   Copyright (C) 2005 FreeIPMI Core Team
   
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

#ifndef _IPMI_RAW_H
#define _IPMI_RAW_H

#include <freeipmi/freeipmi.h>
#include <limits.h>	/* ARG_MAX */

enum argp_option_keys
  {
    CMD_FILE_KEY = 'f',
  };

struct ipmi_raw_arguments
{
  struct common_cmd_args common;
  char *cmd_file;
  uint8_t cmd[ARG_MAX];
  int cmd_length;
};

typedef struct ipmi_raw_prog_data
{
  char *progname;
  struct ipmi_raw_arguments *args;
  uint32_t debug_flags;
} ipmi_raw_prog_data_t;

typedef struct ipmi_raw_state_data
{
  ipmi_raw_prog_data_t *prog_data;
  ipmi_device_t dev;
} ipmi_raw_state_data_t;

#endif
