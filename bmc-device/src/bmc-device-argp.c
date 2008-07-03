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

#include "bmc-device.h"
#include "bmc-device-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version = 
  "bmc-device - " PACKAGE_VERSION "\n"
  "Copyright (C) 2008 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = 
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] = 
  "bmc-device - perform BMC device commands";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_USER,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"cold-reset", CMD_COLD_RESET_KEY, NULL, 0,
     "Perform a cold reset.", 30},
    {"warm-reset", CMD_WARM_RESET_KEY, NULL, 0,
     "Perform a warm reset.", 31},
    {"get-self-test-results", CMD_GET_SELF_TEST_RESULTS_KEY, NULL, 0,
     "Output BMC self test results.", 32},
    {"get-acpi-power-state", CMD_GET_ACPI_POWER_STATE_KEY, NULL, 0,
     "Get ACPI system and device power state.", 33},
    {"verbose", VERBOSE_KEY, 0, 0,
     "Increase verbosity in output.", 34},
    { 0 }
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
  struct bmc_device_arguments *cmd_args = state->input;
  error_t ret;

  switch (key)
    {
    case CMD_COLD_RESET_KEY:
      cmd_args->cold_reset++;
      break;
    case CMD_WARM_RESET_KEY:
      cmd_args->warm_reset++;
      break;
    case CMD_GET_SELF_TEST_RESULTS_KEY:
      cmd_args->get_self_test_results++;
      break;
    case CMD_GET_ACPI_POWER_STATE_KEY:
      cmd_args->get_acpi_power_state++;
      break;
    case VERBOSE_KEY:
      cmd_args->verbose++;
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

static void
_bmc_device_config_file_parse(struct bmc_device_arguments *cmd_args)
{
  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         NULL,
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         0,
                         NULL) < 0)
    {
      fprintf(stderr, "config_file_parse: %s\n", strerror(errno));
      exit(1);
    }
}

void
_bmc_device_args_validate (struct bmc_device_arguments *cmd_args)
{ 
  if (!cmd_args->cold_reset 
      && !cmd_args->warm_reset
      && !cmd_args->get_self_test_results
      && !cmd_args->get_acpi_power_state)
    {
      fprintf (stderr, 
               "No BMC device command specified.\n");
      exit(1);
    }

  if ((cmd_args->cold_reset 
       + cmd_args->warm_reset
       + cmd_args->get_self_test_results
       + cmd_args->get_acpi_power_state) > 1)
    {
      fprintf (stderr, 
               "Multiple BMC device commands specified.\n");
      exit(1);
    }
}

void
bmc_device_argp_parse (int argc, char **argv, struct bmc_device_arguments *cmd_args)
{
  init_common_cmd_args_admin (&(cmd_args->common));
  init_hostrange_cmd_args (&(cmd_args->hostrange));

  cmd_args->cold_reset = 0;
  cmd_args->warm_reset = 0;
  cmd_args->get_self_test_results = 0;
  cmd_args->get_acpi_power_state = 0;
  cmd_args->verbose = 0;

  argp_parse (&cmdline_config_file_argp, argc, argv, ARGP_IN_ORDER, NULL, &(cmd_args->common));

  _bmc_device_config_file_parse(cmd_args);

  argp_parse (&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
  _bmc_device_args_validate (cmd_args);
}

