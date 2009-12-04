/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

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

#include "tool-cmdline-common.h"
#include "pstdout.h"

#include "config-tool-argp.h"
#include "config-tool-common.h"
#include "config-tool-comment.h"
#include "config-tool-checkout.h"
#include "config-tool-commit.h"
#include "config-tool-diff.h"
#include "config-tool-parse.h"
#include "config-tool-section.h"
#include "config-tool-utils.h"
#include "config-tool-validate.h"

enum pef_config_argp_option_keys
  {
    INFO_KEY = 'i',
  };

struct pef_config_arguments
{
  struct config_arguments config_args;
  int info;
};

typedef struct pef_config_prog_data
{ 
  char *progname;
  struct pef_config_arguments *args;
  int hosts_count;
} pef_config_prog_data_t;

typedef struct pef_config_state_data
{ 
  pef_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;

  /* achu: workaround for OEM compliance issue, see alert policy section */
  unsigned int alert_policy_sections_len;
  struct config_section **alert_policy_sections;

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
