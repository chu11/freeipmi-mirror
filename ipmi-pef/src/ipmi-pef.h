/*
ipmi-pef.h: Platform Event Filtering utility.
Copyright (C) 2007 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _IPMI_PEF_H
#define _IPMI_PEF_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"

enum argp_option_keys
  { 
    INFO_KEY = 'i', 
    CHECKOUT_KEY = 'o', 
    COMMIT_KEY = 'c', 
    EVENT_FILTER_TABLE_KEY = 'e',
    ALERT_POLICY_TABLE_KEY = 't', 
    LAN_ALERT_DESTINATIONS_KEY = 'd', 
    COMMUNITY_STRING_KEY = 's',
    VERBOSE_KEY = 'v',
    FILENAME_KEY = 'f'
  };

struct ipmi_pef_arguments
{
  struct common_cmd_args common;
  int info_wanted;
  int checkout_wanted;
  int commit_wanted;
  int community_string_wanted;
  int lan_alert_destinations_wanted;
  int alert_policy_table_wanted;
  int event_filter_table_wanted;
  char *community_string;
  int verbose_wanted;
  char *filename;
};

typedef struct ipmi_pef_prog_data
{ 
  char *progname;
  struct ipmi_pef_arguments *args;
  uint32_t debug_flags;
} ipmi_pef_prog_data_t;

typedef struct ipmi_pef_state_data
{ 
  ipmi_pef_prog_data_t *prog_data;
  ipmi_device_t dev;
} ipmi_pef_state_data_t;

#endif
