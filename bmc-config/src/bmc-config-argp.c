/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
#include <argp.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "tool-cmdline-common.h"

#include "bmc-config.h"
#include "bmc-config-argp.h"

const char *argp_program_version = PACKAGE_VERSION;
const char *argp_program_bug_address = "<" PACKAGE_BUGREPORT ">";
/* Program documentation. */
static char doc[] =  "GNU FreeIPMI (bmc-config) -- BMC config tool";
/* A description of the arguments we accept. */
static char args_doc[] = "";

/* The options we understand. */
static struct argp_option options[] = {
  ARGP_COMMON_OPTIONS_DRIVER,
  ARGP_COMMON_OPTIONS_INBAND,
  ARGP_COMMON_OPTIONS_OUTOFBAND,
  ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
  ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
  ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
  ARGP_COMMON_OPTIONS_DEBUG,
  CONFIG_ARGP_COMMON_OPTIONS,
  CONFIG_ARGP_COMMON_OPTIONS_LEGACY,
  { 0, }
};

static error_t parse_opt (int key, char *arg, struct argp_state *state);

/* Our argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc};

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct bmc_config_arguments *cmd_args = state->input;
  error_t ret;

  switch (key)
    {
    case ARGP_KEY_ARG:
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      ret = config_parse_opt (key, arg, state, &cmd_args->config_args);
      if (ret == ARGP_ERR_UNKNOWN)
        ret = common_parse_opt (key, arg, state, &cmd_args->config_args.common);
      return ret;
    }
  return 0;
}

void
bmc_config_argp_parse (int argc, char *argv[], struct bmc_config_arguments *cmd_args)
{
  init_config_args (&(cmd_args->config_args));
  init_common_cmd_args_admin (&(cmd_args->config_args.common));

  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->config_args.common));
  config_args_validate(&(cmd_args->config_args));
}
