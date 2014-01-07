/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#include "bmc-info.h"
#include "bmc-info-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "bmc-info - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2014 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "bmc-info - display BMC information";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
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
    /* legacy */
    { "guid", GUID_KEY, NULL, OPTION_HIDDEN,
      "Display only device guid.", 40},
    { "get-device-id", GET_DEVICE_ID_KEY, NULL, 0,
      "Display only device ID information.", 41},
    { "get-device-guid", GET_DEVICE_GUID_KEY, NULL, 0,
      "Display only device guid.", 42},
    { "get-system-guid", GET_SYSTEM_GUID_KEY, NULL, 0,
      "Display only system guid.", 43},
    { "get-system-info", GET_SYSTEM_INFO_KEY, NULL, 0,
      "Display only system information.", 44},
    { "get-channel-info", GET_CHANNEL_INFO_KEY, NULL, 0,
      "Display only channel information.", 45},
    { "interpret-oem-data", INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 46},
    { NULL, 0, NULL, 0, NULL, 0}
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
  struct bmc_info_arguments *cmd_args;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case GET_DEVICE_ID_KEY:
      cmd_args->get_device_id++;
      break;
      /* legacy */
    case GUID_KEY:
    case GET_DEVICE_GUID_KEY:
      cmd_args->get_device_guid++;
      break;
    case GET_SYSTEM_GUID_KEY:
      cmd_args->get_system_guid++;
      break;
    case GET_SYSTEM_INFO_KEY:
      cmd_args->get_system_info++;
      break;
    case GET_CHANNEL_INFO_KEY:
      cmd_args->get_channel_info++;
      break;
    case INTERPRET_OEM_DATA_KEY:
      cmd_args->interpret_oem_data = 1;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return (common_parse_opt (key, arg, &(cmd_args->common_args)));
    }

  return (0);
}

static void
_bmc_info_config_file_parse (struct bmc_info_arguments *cmd_args)
{
  struct config_file_data_bmc_info config_file_data;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_bmc_info));

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_BMC_INFO,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
}

void
bmc_info_argp_parse (int argc, char **argv, struct bmc_info_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_user (&(cmd_args->common_args));

  cmd_args->get_device_id = 0;
  cmd_args->get_device_guid = 0;
  cmd_args->get_system_guid = 0;
  cmd_args->get_system_info = 0;
  cmd_args->get_channel_info = 0;
  cmd_args->interpret_oem_data = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER, NULL,
              &(cmd_args->common_args));

  _bmc_info_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
}
