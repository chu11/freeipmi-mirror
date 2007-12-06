/*
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

#ifndef _PEF_CONFIG_H
#define _PEF_CONFIG_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "cmdline-parse-common.h"

#include "config-common.h"
#include "config-comment.h"
#include "config-checkout.h"
#include "config-commit.h"
#include "config-diff.h"
#include "config-fiid.h"
#include "config-parse.h"
#include "config-section.h"
#include "config-util.h"
#include "config-validate.h"

enum argp_option_keys
  { 
    INFO_KEY = 'i', 
    CHECKOUT_KEY = 'o', 
    COMMIT_KEY = 'c', 
    DIFF_KEY = 'd',
    FILENAME_KEY = 'f',
    KEYPAIR_KEY = 'e',
    SECTIONS_KEY = 'S',
    LIST_SECTIONS_KEY = 'L',
    VERBOSE_KEY = 'v',
  };

typedef struct pef_config_prog_data
{ 
  char *progname;
  struct config_arguments *args;
} pef_config_prog_data_t;

typedef struct pef_config_state_data
{ 
  pef_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  struct config_section *sections;

  /* achu: caching to make pef-config work more quickly */
  int lan_channel_number_initialized;
  int8_t lan_channel_number;
  int number_of_lan_alert_destinations_initialized;
  int8_t number_of_lan_alert_destinations;
  int number_of_alert_strings_initialized;
  int8_t number_of_alert_strings;
  int number_of_alert_policy_entries_initialized;
  int8_t number_of_alert_policy_entries;
  int number_of_event_filters_initialized;
  int8_t number_of_event_filters;
} pef_config_state_data_t;

#endif
