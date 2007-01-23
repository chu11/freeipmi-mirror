/* 
   $Id: ipmi-sensors.h,v 1.2 2007-01-23 00:53:16 chu11 Exp $ 
   
   ipmi-sensors.h - IPMI Sensors utility.
   
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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifndef _IPMI_SENSORS_H
#define _IPMI_SENSORS_H

#include <freeipmi/freeipmi.h>

#include "argp-common.h"
#include "ipmi-sdr-api.h"

enum ipmi_sensors_argp_option_keys
  { 
    VERBOSE_KEY = 'v', 
    SDR_INFO_KEY = 'i', 
    FLUSH_CACHE_KEY = 'f', 
    QUIET_CACHE_KEY = 'Q',
    LIST_GROUPS_KEY = 'L', 
    GROUP_KEY = 'g', 
    SENSORS_LIST_KEY = 's', 
    SDR_CACHE_DIR_KEY = 200
  };

struct ipmi_sensors_arguments
{
  struct common_cmd_args common;
  int verbose_wanted;
  int verbose_count;
  int sdr_info_wanted;
  int flush_cache_wanted;
  int quiet_cache_wanted;
  int list_groups_wanted;
  int group_wanted;
  char *group;
  int sensors_list_wanted;
  unsigned int *sensors_list;
  unsigned int sensors_list_length;
  int sdr_cache_dir_wanted;
  char *sdr_cache_dir;
};

typedef struct ipmi_sensors_prog_data
{
  char *progname;
  struct ipmi_sensors_arguments *args;
  uint32_t debug_flags;
} ipmi_sensors_prog_data_t;

typedef struct ipmi_sensors_state_data
{
  ipmi_sensors_prog_data_t *prog_data;
  ipmi_device_t dev;
  sdr_repository_info_t sdr_info;
  sdr_record_t *sdr_record_list;
  int sdr_record_count;
} ipmi_sensors_state_data_t;

#endif
