/* 
   Copyright (C) 2008 FreeIPMI Core Team
   
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
#include "bmc-device.h"
#include "bmc-device-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"BMC Device [bmc-device-" PACKAGE_VERSION "]\n"
"Copyright (C) 2008 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "BMC Device - Perform BMC device commands.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_USER,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"cold-reset", CMD_COLD_RESET_KEY, NULL, 0,
     "Perform a cold reset.", 30},
    {"warm-reset", CMD_WARM_RESET_KEY, NULL, 0,
     "Perform a warm reset.", 31},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct bmc_device_arguments *cmd_args = state->input;
  error_t ret;

  switch (key)
    {
    case CMD_COLD_RESET_KEY:
      cmd_args->cold_reset_wanted++;
      break;
    case CMD_WARM_RESET_KEY:
      cmd_args->warm_reset_wanted++;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->hostrange));
      return ret;
    }
  
  return 0;
}

void 
bmc_device_argp_parse (int argc, char **argv, struct bmc_device_arguments *cmd_args)
{
  init_common_cmd_args (&(cmd_args->common));
  init_hostrange_cmd_args (&(cmd_args->hostrange));

  cmd_args->cold_reset_wanted = 0;
  cmd_args->warm_reset_wanted = 0;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}

int
bmc_device_args_validate (struct bmc_device_arguments *cmd_args)
{ 
  if (!cmd_args->cold_reset_wanted && !cmd_args->warm_reset_wanted)
    {
      fprintf (stderr, 
               "No BMC device command specified.\n");
      return -1;
    }

  if (cmd_args->cold_reset_wanted && cmd_args->warm_reset_wanted)
    {
      fprintf (stderr, 
               "Multiple BMC device commands specified.\n");
      return -1;
    }

  return 0;
}
