/*
 * Copyright (C) 2007-2015 FreeIPMI Core Team
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
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-chassis.h"
#include "ipmi-chassis-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-chassis - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2015 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-chassis - IPMI chassis management utility";

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
    { "get-chassis-capabilities", GET_CHASSIS_CAPABILITIES_KEY, NULL, 0,
      "Get chassis capabilities.", 40},
    { "get-chassis-status", GET_CHASSIS_STATUS_KEY, NULL, 0,
      "Get chassis status.", 41},
    { "chassis-control", CHASSIS_CONTROL_KEY, "CONTROL", 0,
      "Control the chassis.", 42},
    { "chassis-identify", CHASSIS_IDENTIFY_KEY, "IDENTIFY", 0,
      "Set chassis Identification.", 43},
    { "get-system-restart-cause", GET_SYSTEM_RESTART_CAUSE_KEY, NULL, 0,
      "Get system restart cause.", 44},
    { "get-power-on-hours-counter", GET_POWER_ON_HOURS_COUNTER_KEY, NULL, 0,
      "Get power on hours (POH) counter.", 45},
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
  char *endptr = NULL;
  struct ipmi_chassis_arguments *cmd_args;
  int tmp;

  assert (state);

  cmd_args = state->input;

  switch (key)
    {
    case GET_CHASSIS_CAPABILITIES_KEY:
      cmd_args->get_chassis_capabilities++;
      break;

    case GET_CHASSIS_STATUS_KEY:
      cmd_args->get_chassis_status++;
      break;

    case CHASSIS_CONTROL_KEY:
      if (!strcasecmp (arg, "power-down"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_POWER_DOWN;
      else if (!strcasecmp (arg, "power-up"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_POWER_UP;
      else if (!strcasecmp (arg, "power-cycle"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_POWER_CYCLE;
      else if (!strcasecmp (arg, "hard-reset"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_HARD_RESET;
      else if (!strcasecmp (arg, "diagnostic-interrupt"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_PULSE_DIAGNOSTIC_INTERRUPT;
      else if (!strcasecmp (arg, "soft-shutdown"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_INITIATE_SOFT_SHUTDOWN;
      else
        {
          fprintf (stderr, "invalid value for chassis control\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->chassis_control++;
      break;

    case CHASSIS_IDENTIFY_KEY:
      if (!strcasecmp (arg, "turn-off"))
        {
          cmd_args->chassis_identify_args.identify_interval = 1;
          cmd_args->chassis_identify_args.identify_interval_arg = 0;

          cmd_args->chassis_identify_args.force_identify = 0;
        }
      else if (!strcasecmp (arg, "force"))
        {
          cmd_args->chassis_identify_args.force_identify = 1;
          cmd_args->chassis_identify_args.force_identify_arg = IPMI_CHASSIS_FORCE_IDENTIFY_ON;

          /* Need to have identify_interval set if force_identify is set */
          cmd_args->chassis_identify_args.identify_interval = 1;
          cmd_args->chassis_identify_args.identify_interval_arg = 0xFF;
        }
      else
        {
          errno = 0;
          tmp = strtol (arg, &endptr, 10);
          if (errno
              || endptr[0] != '\0')
            {
              fprintf (stderr, "invalid value for chassis-identify\n");
              exit (EXIT_FAILURE);
            }
          if (tmp < IPMI_CHASSIS_IDENTIFY_INTERVAL_MIN
              || tmp > IPMI_CHASSIS_IDENTIFY_INTERVAL_MAX)
            {
              fprintf (stderr, "chassis-identify interval out of range\n");
              exit (EXIT_FAILURE);
            }
          cmd_args->chassis_identify_args.identify_interval = 1;
          cmd_args->chassis_identify_args.identify_interval_arg = tmp;
          cmd_args->chassis_identify_args.force_identify = 0;
        }
      cmd_args->chassis_identify++;
      break;

    case GET_SYSTEM_RESTART_CAUSE_KEY:
      cmd_args->get_system_restart_cause++;
      break;

    case GET_POWER_ON_HOURS_COUNTER_KEY:
      cmd_args->get_power_on_hours_counter++;
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
_ipmi_chassis_config_file_parse (struct ipmi_chassis_arguments *cmd_args)
{
  assert (cmd_args);

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_CHASSIS,
                         NULL) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }
}

static void
_ipmi_chassis_args_validate (struct ipmi_chassis_arguments *cmd_args)
{
  assert (cmd_args);

  if (!cmd_args->get_chassis_capabilities
      && !cmd_args->get_chassis_status
      && !cmd_args->chassis_control
      && !cmd_args->chassis_identify
      && !cmd_args->get_system_restart_cause
      && !cmd_args->get_power_on_hours_counter)
    {
      fprintf (stderr,
               "No command specified.\n");
      exit (EXIT_FAILURE);
    }

  if ((cmd_args->get_chassis_capabilities
       + cmd_args->get_chassis_status
       + cmd_args->chassis_control
       + cmd_args->chassis_identify
       + cmd_args->get_system_restart_cause
       + cmd_args->get_power_on_hours_counter) > 1)
    {
      fprintf (stderr,
               "Multiple commands specified.\n");
      exit (EXIT_FAILURE);
    }
}

void
ipmi_chassis_argp_parse (int argc,
                         char **argv,
                         struct ipmi_chassis_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_admin (&(cmd_args->common_args));

  cmd_args->get_chassis_capabilities = 0;
  cmd_args->get_chassis_status = 0;
  cmd_args->chassis_control = 0;
  cmd_args->chassis_control_arg = 0;
  cmd_args->chassis_identify = 0;
  cmd_args->chassis_identify_args.identify_interval = 0;
  cmd_args->chassis_identify_args.force_identify = 0;
  cmd_args->get_system_restart_cause = 0;
  cmd_args->get_power_on_hours_counter = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _ipmi_chassis_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _ipmi_chassis_args_validate (cmd_args);
}

