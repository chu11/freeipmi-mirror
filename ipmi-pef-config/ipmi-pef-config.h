/*
 * Copyright (C) 2007-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef IPMI_PEF_CONFIG_H
#define IPMI_PEF_CONFIG_H

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

enum ipmi_pef_config_argp_option_keys
  {
    INFO_KEY = 'i',
  };

struct ipmi_pef_config_arguments
{
  struct config_arguments config_args;
  int info;
};

typedef struct ipmi_pef_config_prog_data
{
  char *progname;
  struct ipmi_pef_config_arguments *args;
  int hosts_count;
} ipmi_pef_config_prog_data_t;

typedef struct ipmi_pef_config_state_data
{
  ipmi_pef_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  struct config_section *sections;

  /* For multi-channel settings
   *
   * base is for base section name (e.g. "Community_String")
   * channel is for channel suffixed section name (e.g. "Community_String_Channel_1")
   */
  unsigned int lan_base_config_flags;
  unsigned int lan_channel_config_flags;

  /* For channel reading */
  uint8_t lan_channel_numbers[IPMI_CHANNEL_NUMBERS_MAX];
  unsigned int lan_channel_numbers_count;
  unsigned int lan_channel_numbers_loaded;
} ipmi_pef_config_state_data_t;

#endif /* IPMI_PEF_CONFIG_H */
