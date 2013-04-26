/*
 * Copyright (C) 2008-2013 FreeIPMI Core Team
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

#ifndef IPMI_SENSORS_CONFIG_H
#define IPMI_SENSORS_CONFIG_H

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

struct ipmi_sensors_config_arguments
{
  struct config_arguments config_args;
};

typedef struct ipmi_sensors_config_prog_data
{
  char *progname;
  struct ipmi_sensors_config_arguments *args;
  int hosts_count;
} ipmi_sensors_config_prog_data_t;

typedef struct ipmi_sensors_config_state_data
{
  ipmi_sensors_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  struct config_section *sections;
  ipmi_sdr_ctx_t sdr_ctx;
} ipmi_sensors_config_state_data_t;

#endif /* IPMI_SENSORS_CONFIG_H */
