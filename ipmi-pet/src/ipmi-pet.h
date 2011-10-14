/*
 * Copyright (C) 2011 FreeIPMI Core Team
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

#ifndef _IPMI_PET_H
#define _IPMI_PET_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"

enum ipmi_pet_argp_option_keys
  {
    VERBOSE_KEY = 'v',
    INTERPRET_OEM_DATA_KEY = 160,
  };

struct ipmi_pet_arguments
{
  struct common_cmd_args common;
  int interpret_oem_data;
};

typedef struct ipmi_pet_prog_data
{
  char *progname;
  struct ipmi_pet_arguments *args;
} ipmi_pet_prog_data_t;

typedef struct ipmi_pet_state_data
{
  ipmi_pet_prog_data_t *prog_data;
  ipmi_sdr_cache_ctx_t sdr_cache_ctx;
  ipmi_sdr_parse_ctx_t sdr_parse_ctx;
  ipmi_interpret_ctx_t interpret_ctx;
} ipmi_pet_state_data_t;

#endif
