/* 
   Copyright (C) 2007-2008 FreeIPMI Core Team
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "tool-cmdline-common.h"
#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI PEF [ipmi-sensors-config-" PACKAGE_VERSION "]\n"
"Copyright (C) 2007-2008 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI-SENSORS-CONFIG - sensor configuration utility.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_OPERATOR,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    CONFIG_ARGP_COMMON_OPTIONS,
    { 0, }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmi_sensors_config_arguments *cmd_args = state->input;
  error_t ret;
  
  switch (key)
    {
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      ret = config_parse_opt (key, arg, state, &cmd_args->config_args);
      if (ret == ARGP_ERR_UNKNOWN)
        ret = common_parse_opt (key, arg, state, &(cmd_args->config_args.common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = sdr_parse_opt (key, arg, state, &(cmd_args->sdr));
      return ret;
    }
  
  return 0;
}

void 
ipmi_sensors_config_argp_parse (int argc, char **argv, struct ipmi_sensors_config_arguments *cmd_args)
{
  init_config_args (&(cmd_args->config_args));
  init_common_cmd_args_operator (&(cmd_args->config_args.common));
  init_sdr_cmd_args (&(cmd_args->sdr));

  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_sdr_cmd_args (&(cmd_args->sdr));
  verify_common_cmd_args (&(cmd_args->config_args.common));
  config_args_validate(&(cmd_args->config_args));
}
