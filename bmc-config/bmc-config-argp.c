/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "bmc-config - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2013 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "bmc-config - configure BMC values";

static char cmdline_args_doc[] = "";

/* The options we understand. */
static struct argp_option cmdline_options[] = {
  ARGP_COMMON_OPTIONS_DRIVER,
  ARGP_COMMON_OPTIONS_INBAND,
  ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED,
  ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
  ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
  ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
  ARGP_COMMON_OPTIONS_CONFIG_FILE,
  ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
  ARGP_COMMON_HOSTRANGED_OPTIONS,
  ARGP_COMMON_OPTIONS_DEBUG,
  CONFIG_ARGP_COMMON_OPTIONS,
  CONFIG_ARGP_COMMON_OPTIONS_LEGACY,
  CONFIG_ARGP_LAN_CHANNEL_OPTION,
  CONFIG_ARGP_SERIAL_CHANNEL_OPTION,
  CONFIG_ARGP_SOL_CHANNEL_OPTION,
  { NULL, 0, NULL, 0, NULL, 0}
};

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc};

static struct argp cmdline_config_file_argp = { cmdline_options,
                                                cmdline_config_file_parse,
                                                cmdline_args_doc,
                                                cmdline_doc };

/* Parse a single option. */
static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct bmc_config_arguments *cmd_args;
  error_t ret;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    case CONFIG_ARGP_FILENAME_KEY_LEGACY:
      key = CONFIG_ARGP_FILENAME_KEY;
      /* fall through */
    default:
      ret = config_parse_opt (key, arg, &cmd_args->config_args);
      if (ret == ARGP_ERR_UNKNOWN)
        ret = common_parse_opt (key, arg, &cmd_args->config_args.common_args);
      return (ret);
    }
  return (0);
}

static void
_bmc_config_config_file_parse (struct bmc_config_arguments *cmd_args)
{
  struct config_file_data_bmc_config config_file_data;
  
  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_bmc_config));
  
  if (config_file_parse (cmd_args->config_args.common_args.config_file,
                         0,
                         &(cmd_args->config_args.common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_BMC_CONFIG,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->config_args.verbose_count = config_file_data.verbose_count;
}

static void
_bmc_config_args_validate (struct bmc_config_arguments *cmd_args)
{
  assert (cmd_args);

  if (!cmd_args->config_args.action || cmd_args->config_args.action == -1)
    {
      fprintf (stderr,
               "Exactly one of --checkout, --commit, --diff, or --listsections MUST be given\n");
      exit (EXIT_FAILURE);
    }

  config_args_validate (&(cmd_args->config_args));
}


void
bmc_config_argp_parse (int argc, char *argv[], struct bmc_config_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_config_args (&(cmd_args->config_args));
  init_common_cmd_args_admin (&(cmd_args->config_args.common_args));

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->config_args.common_args));

  _bmc_config_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->config_args.common_args));
  _bmc_config_args_validate (cmd_args);
}
