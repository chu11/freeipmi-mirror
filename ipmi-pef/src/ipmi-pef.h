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
    DIFF_KEY = 'd',
    LIST_SECTIONS_KEY = 'L',
    VERBOSE_KEY = 'v',
    FILENAME_KEY = 'f',
    SECTIONS_KEY = 'S',
  };

typedef enum
  {
    PEF_ACTION_INFO = 1,
    PEF_ACTION_CHECKOUT,
    PEF_ACTION_COMMIT,
    PEF_ACTION_DIFF,
    PEF_ACTION_LIST_SECTIONS,
  } pef_action_t;

typedef enum
  {
    PEF_ERR_FATAL_ERROR = -2,
    PEF_ERR_NON_FATAL_ERROR = -1,
    PEF_ERR_SUCCESS = 0,
  } pef_err_t;

typedef enum
  {
    PEF_DIFF_FATAL_ERROR = -2,
    PEF_DIFF_NON_FATAL_ERROR = -1,
    PEF_DIFF_SAME = 0,
    PEF_DIFF_DIFFERENT = 1,
  } pef_diff_t;

typedef enum
  {
    PEF_VALIDATE_FATAL_ERROR = -2,
    PEF_VALIDATE_INVALID_VALUE = -1,
    PEF_VALIDATE_VALID_VALUE = 0,
  } pef_validate_t;

struct keypair
{
  char *keypair;
  struct keypair *next;
};

struct sectionstr
{
  char *sectionstr;
  struct sectionstr *next;
};

struct ipmi_pef_arguments
{
  struct common_cmd_args common;
  
  pef_action_t action;

  int verbose;
  char *filename;
  struct sectionstr *sectionstrs;
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
  struct section *sections;

  /* achu: caching to make ipmi-pef work more quickly */
  int lan_channel_number_initialized;
  int8_t lan_channel_number;
  int number_of_lan_alert_destinations_initialized;
  int8_t number_of_lan_alert_destinations;
  int number_of_alert_policy_entries_initialized;
  int8_t number_of_alert_policy_entries;
  int number_of_event_filters_initialized;
  int8_t number_of_event_filters;
} ipmi_pef_state_data_t;

#endif
