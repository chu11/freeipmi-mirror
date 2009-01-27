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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */

#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version = 
  "ipmi-sensors-config - " PACKAGE_VERSION "\n"
  "Copyright (C) 2008 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = 
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] = 
  "ipmi-sensors-config - configure sensors";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_OPERATOR,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    CONFIG_ARGP_COMMON_OPTIONS,
    { 0, }
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse, 
                                    cmdline_args_doc, 
                                    cmdline_doc };

static struct argp cmdline_config_file_argp = { cmdline_options,
                                                cmdline_config_file_parse,
                                                cmdline_args_doc,
                                                cmdline_doc };

static error_t 
cmdline_parse (int key, char *arg, struct argp_state *state)
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
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->config_args.hostrange));
      return ret;
    }
  
  return 0;
}

static void
_ipmi_sensors_config_config_file_parse(struct ipmi_sensors_config_arguments *cmd_args)
{
  if (config_file_parse (cmd_args->config_args.common.config_file,
                         0,
                         &(cmd_args->config_args.common),
                         &(cmd_args->sdr),
                         &(cmd_args->config_args.hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_SENSORS_CONFIG,
                         NULL) < 0)
    {
      fprintf(stderr, "config_file_parse: %s\n", strerror(errno));
      exit(1);
    }
}

void
_ipmi_sensors_config_config_args_validate (struct ipmi_sensors_config_arguments *cmd_args)
{
  if ((!cmd_args->config_args.action && !cmd_args->sdr.flush_cache)
      || (cmd_args->config_args.action && cmd_args->sdr.flush_cache)
      || cmd_args->config_args.action == -1)
    {
      fprintf (stderr,
               "Exactly one of --flush-cache, --checkout, --commit, --diff, or --listsections MUST be given\n");
      exit(1);
    }

  /* make dummy argument for args validate to pass */
  if (!cmd_args->config_args.action)
    cmd_args->config_args.action = 1;

  config_args_validate(&(cmd_args->config_args));
}

void 
ipmi_sensors_config_argp_parse (int argc, char **argv, struct ipmi_sensors_config_arguments *cmd_args)
{
  init_config_args (&(cmd_args->config_args));
  init_common_cmd_args_operator (&(cmd_args->config_args.common));
  init_hostrange_cmd_args (&(cmd_args->config_args.hostrange));
  init_sdr_cmd_args (&(cmd_args->sdr));

  argp_parse (&cmdline_config_file_argp, argc, argv, ARGP_IN_ORDER, NULL, &(cmd_args->config_args.common));

  _ipmi_sensors_config_config_file_parse(cmd_args);

  argp_parse (&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_sdr_cmd_args (&(cmd_args->sdr));
  verify_common_cmd_args (&(cmd_args->config_args.common));
  _ipmi_sensors_config_config_args_validate (cmd_args);
}
