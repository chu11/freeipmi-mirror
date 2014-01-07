/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

#ifndef IPMI_OEM_H
#define IPMI_OEM_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

/* seems like a nice size */
#define IPMI_OEM_MAX_ARGS 1024

enum ipmi_sensors_argp_option_keys
  {
    LIST_KEY = 'L',
    VERBOSE_KEY = 'v',
  };

struct ipmi_oem_arguments
{
  struct common_cmd_args common_args;
  int list;
  unsigned int verbose_count;
  char *oem_id;
  char *oem_command;
  char *oem_options[IPMI_OEM_MAX_ARGS];
  unsigned int oem_options_count;
};

typedef struct ipmi_oem_prog_data
{
  char *progname;
  struct ipmi_oem_arguments *args;
} ipmi_oem_prog_data_t;

typedef struct ipmi_oem_state_data
{
  ipmi_oem_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_ctx_t sdr_ctx;
} ipmi_oem_state_data_t;

#endif /* IPMI_OEM_H */
