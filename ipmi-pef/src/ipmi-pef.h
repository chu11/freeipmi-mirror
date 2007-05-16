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

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"

enum argp_option_keys
  { 
    INFO_KEY = 'i', 
    CHECKOUT_KEY = 'o', 
    COMMIT_KEY = 'c', 
    ALERT_POLICY_TABLE_KEY = 't', 
    LAN_ALERT_DESTINATION_KEY = 'd', 
    COMMUNITY_STRING_KEY = 's'
  };

struct ipmi_pef_arguments
{
  struct common_cmd_args common;
  int info_wanted;
  int checkout_wanted;
  char *checkout_filename;
  int commit_wanted;
  char *commit_filename;
  int alert_policy_table_wanted;
  int lan_alert_destination_wanted;
  int community_string_wanted;
  char *community_string;
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
