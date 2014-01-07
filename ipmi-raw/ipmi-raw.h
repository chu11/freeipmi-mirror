/*
 * Copyright (C) 2005-2014 FreeIPMI Core Team
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

#ifndef IPMI_RAW_H
#define IPMI_RAW_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

/* IPMI 2.0 Payload is 2 bytes, so we'll assume that size * 2 for good measure */
#define IPMI_RAW_MAX_ARGS (65536*2)

enum ipmi_raw_argp_option_keys
  {
    CHANNEL_NUMBER_KEY = 160,	/* legacy */
    SLAVE_ADDRESS_KEY = 161,	/* legacy */
    CMD_FILE_KEY = 162,
  };

struct ipmi_raw_arguments
{
  struct common_cmd_args common_args;
  char *cmd_file;
  uint8_t cmd[IPMI_RAW_MAX_ARGS];
  unsigned int cmd_length;
};

typedef struct ipmi_raw_prog_data
{
  char *progname;
  struct ipmi_raw_arguments *args;
} ipmi_raw_prog_data_t;

typedef struct ipmi_raw_state_data
{
  ipmi_raw_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
} ipmi_raw_state_data_t;

#endif /* IPMI_RAW_H */
