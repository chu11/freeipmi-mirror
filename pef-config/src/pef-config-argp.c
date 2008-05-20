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
#include "pef-config.h"
#include "pef-config-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI PEF [pef-config-" PACKAGE_VERSION "]\n"
"Copyright (C) 2007-2008 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "PEF-CONFIG - PEF configuration utility.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about PEF configuration.", 30},
    CONFIG_ARGP_COMMON_OPTIONS,
    CONFIG_ARGP_COMMON_OPTIONS_LEGACY,
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct pef_config_arguments *cmd_args = state->input;
  error_t ret;
  
  switch (key)
    {
    case INFO_KEY:
      cmd_args->info_wanted++;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
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
_pef_config_args_validate (struct pef_config_arguments *cmd_args)
{
  if ((!cmd_args->config_args.action && !cmd_args->info_wanted)
      || (cmd_args->config_args.action && cmd_args->info_wanted)
      || cmd_args->config_args.action == -1)
    {
      fprintf (stderr,
               "Exactly one of --info, --checkout, --commit, --diff, or --listsections MUST be given\n");
      exit(1);
    }

  /* make dummy argument for args validate to pass */
  cmd_args->config_args.action = 1;
  
  config_args_validate(&(cmd_args->config_args));
}

void 
pef_config_argp_parse (int argc, char **argv, struct pef_config_arguments *cmd_args)
{
  init_config_args (&(cmd_args->config_args));
  init_common_cmd_args_admin (&(cmd_args->config_args.common));
  cmd_args->info_wanted = 0;

  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->config_args.common));
  _pef_config_args_validate (cmd_args);
}

