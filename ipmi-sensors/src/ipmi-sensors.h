/* 
   Copyright (C) 2006 FreeIPMI Core Team
   
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
#include <freeipmi/api/api.h>

#include "cmdline-parse-common.h"
#include "ipmi-sdr-cache.h"
#include "pstdout.h"

enum ipmi_sensors_argp_option_keys
  { 
    VERBOSE_KEY = 'v', 
    QUIET_READINGS_KEY = 'q',
    SDR_INFO_KEY = 'i', 
    LIST_GROUPS_KEY = 'L', 
    GROUP_KEY = 'g', 
    SENSORS_LIST_KEY = 's', 
  };

struct ipmi_sensors_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
  int verbose_wanted;
  int verbose_count;
  int quiet_readings_wanted;
  int sdr_info_wanted;
  int list_groups_wanted;
  int group_wanted;
  char *group;
  int sensors_list_wanted;
  unsigned int *sensors_list;
  unsigned int sensors_list_length;
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
  sdr_cache_ctx_t sdr_cache_ctx;
  sdr_record_t *sdr_record_list;
  unsigned int sdr_record_count;
} ipmi_sensors_state_data_t;

#endif
