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

#ifndef _IPMI_SENSORS_H
#define _IPMI_SENSORS_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "tool-sdr-cache-common.h"
#include "pstdout.h"

#define IPMI_SENSORS_MAX_RECORD_IDS           256
#define IPMI_SENSORS_MAX_GROUPS               256
#define IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH 256

enum ipmi_sensors_argp_option_keys
  { 
    VERBOSE_KEY = 'v', 
    QUIET_READINGS_KEY = 'q',
    SDR_INFO_KEY = 'i', 
    LIST_GROUPS_KEY = 'L', 
    GROUP_KEY = 160,            /* legacy */
    GROUPS_KEY = 'g', 
    SENSORS_KEY = 's', 
    BRIDGE_SENSORS_KEY = 'b',
  };

struct ipmi_sensors_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
  int verbose;
  int verbose_count;
  int quiet_readings;
  int sdr_info;
  int list_groups;
  int groups_wanted;
  char groups[IPMI_SENSORS_MAX_GROUPS][IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH+1];
  unsigned int groups_length;
  int sensors_wanted;
  unsigned int sensors[IPMI_SENSORS_MAX_RECORD_IDS];
  unsigned int sensors_length;
  int bridge_sensors;
};

typedef struct ipmi_sensors_prog_data
{
  char *progname;
  struct ipmi_sensors_arguments *args;
} ipmi_sensors_prog_data_t;

typedef struct ipmi_sensors_state_data
{
  ipmi_sensors_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_cache_ctx_t ipmi_sdr_cache_ctx;
} ipmi_sensors_state_data_t;

#endif
