/*****************************************************************************\
 *  $Id: ipmi-dcmi-argp.c,v 1.3 2009-05-28 17:35:33 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2009 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmi-Dcmi, tools and libraries to support the
 *  data center manageability interface (DCMI).  For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Ipmi-Dcmi is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-Dcmi is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-Dcmi.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

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

#include "ipmi-dcmi.h"
#include "ipmi-dcmi-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-dcmi - " PACKAGE_VERSION "\n"
  "Copyright (C) 2009 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-dcmi - perform DCMI commands";

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
    { "get-dcmi-capability-info", GET_DCMI_CAPABILITY_INFO, NULL, 0,
      "Get DCMI capability information.", 30},
    { "get-system-power-statistics", GET_SYSTEM_POWER_STATISTICS, NULL, 0,
      "Get system power statistics.", 31},
    { "get-enhanced-system-power-statistics", GET_ENHANCED_SYSTEM_POWER_STATISTICS, NULL, 0,
      "Get enhanced system power statistics.", 32},
    { "get-power-limit", GET_POWER_LIMIT, NULL, 0,
      "Get power limit information.", 33},
    { "set-power-limit", SET_POWER_LIMIT, NULL, 0,
      "Set power limit configuration.", 34},
    { "exception-actions", EXCEPTION_ACTIONS, "BITMASK", 0,
      "Specify exception actions for set power limit configuration.", 35},
    { "power-limit-requested", POWER_LIMIT_REQUESTED, "WATTS", 0,
      "Specify power limit for set power limit configuration.", 36},
    { "correction-time-limit", CORRECTION_TIME_LIMIT, "MILLISECONDS", 0,
      "Specify correction time limit for set power limit configuration.", 37},
    { "statistics-sampling-period", STATISTICS_SAMPLING_PERIOD, "SECONDS", 0,
      "Specify management application statistics sampling period for set power limit configuration.", 38},
    { "activate-deactivate-power-limit", ACTIVATE_DEACTIVATE_POWER_LIMIT, NULL, 0,
      "Activate or deactivate power limit.", 39},
    { "get-asset-tag", GET_ASSET_TAG, NULL, 0,
      "Get asset tag.", 40},
    { "get-dcmi-sensor-info", GET_DCMI_SENSOR_INFO, NULL, 0,
      "Get DCMI sensor information.", 41},
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
  struct ipmi_dcmi_arguments *cmd_args = state->input;
  error_t ret;
  char *ptr = NULL;
  int tmp;
  long long lltmp;

  switch (key)
    {
    case GET_DCMI_CAPABILITY_INFO:
      cmd_args->get_dcmi_capability_info++;
      break;
    case GET_SYSTEM_POWER_STATISTICS:
      cmd_args->get_system_power_statistics++;
      break;
    case GET_ENHANCED_SYSTEM_POWER_STATISTICS:
      cmd_args->get_enhanced_system_power_statistics++;
      break;
    case GET_POWER_LIMIT:
      cmd_args->get_power_limit++;
      break;
    case SET_POWER_LIMIT:
      cmd_args->set_power_limit++;
      break;
    case EXCEPTION_ACTIONS:
      /* special case */
      if (!strcasecmp (arg, "HARD_POWER_OFF_SYSTEM"))
        {
          cmd_args->exception_actions_arg = IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM;
          break;
        }
      tmp = strtol (arg, &ptr, 0);
      if (*ptr != '\0')
        {
          fprintf (stderr, "invalid value for exception actions\n");
          exit (1);
        }
      if (tmp < IPMI_DCMI_EXCEPTION_ACTIONS_MIN
          || tmp > IPMI_DCMI_EXCEPTION_ACTIONS_MAX)
        {
          fprintf (stderr, "exception actions out of range\n");
          exit (1);
        }
      cmd_args->exception_actions_arg = tmp;
      cmd_args->exception_actions++;
      break;
    case POWER_LIMIT_REQUESTED:
      tmp = strtol (arg, &ptr, 10);
      if (*ptr != '\0')
        {
          fprintf (stderr, "invalid value for power limit requested\n");
          exit (1);
        }
      if (tmp < IPMI_DCMI_POWER_LIMIT_REQUESTED_MIN
          || tmp > IPMI_DCMI_POWER_LIMIT_REQUESTED_MAX)
        {
          fprintf (stderr, "power limit requested out of range\n");
          exit (1);
        }
      cmd_args->power_limit_requested_arg = tmp;
      cmd_args->power_limit_requested++;
      break;
    case CORRECTION_TIME_LIMIT:
      lltmp = strtoll (arg, &ptr, 10);
      if (*ptr != '\0')
        {
          fprintf (stderr, "invalid value for correction time limit\n");
          exit (1);
        }
      if (lltmp < IPMI_DCMI_CORRECTION_TIME_LIMIT_MIN
          || lltmp > IPMI_DCMI_CORRECTION_TIME_LIMIT_MAX)
        {
          fprintf (stderr, "correction time limit out of range\n");
          exit (1);
        }
      cmd_args->correction_time_limit_arg = lltmp;
      cmd_args->correction_time_limit++;
      break;
    case STATISTICS_SAMPLING_PERIOD:
      tmp = strtol (arg, &ptr, 10);
      if (*ptr != '\0')
        {
          fprintf (stderr, "invalid value for statistics sampling period\n");
          exit (1);
        }
      if (tmp < IPMI_DCMI_MANAGEMENT_APPLICATION_STATISTICS_SAMPLING_PERIOD_MIN
          || tmp > IPMI_DCMI_MANAGEMENT_APPLICATION_STATISTICS_SAMPLING_PERIOD_MAX)
        {
          fprintf (stderr, "statistics sampling period out of range\n");
          exit (1);
        }
      cmd_args->statistics_sampling_period_arg = tmp;
      cmd_args->statistics_sampling_period++;
      break;
    case ACTIVATE_DEACTIVATE_POWER_LIMIT:
      if (!strcasecmp (arg, "ACTIVATE"))
        cmd_args->activate_deactivate_power_limit_arg = IPMI_DCMI_POWER_LIMIT_ACTIVATION_ACTIVATE_POWER_LIMIT;
      else if (!strcasecmp (arg, "DEACTIVATE"))
        cmd_args->activate_deactivate_power_limit_arg = IPMI_DCMI_POWER_LIMIT_ACTIVATION_DEACTIVATE_POWER_LIMIT;
      else
        {
          fprintf (stderr, "invalid value for activate/deactivate power limit\n");
          exit (1);
        }
      cmd_args->activate_deactivate_power_limit++;
      break;
    case GET_ASSET_TAG:
      cmd_args->get_asset_tag++;
      break;
    case GET_DCMI_SENSOR_INFO:
      cmd_args->get_dcmi_sensor_info++;
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
      return (ret);
    }

  return (0);
}

static void
_ipmi_dcmi_config_file_parse (struct ipmi_dcmi_arguments *cmd_args)
{
  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         NULL,
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_DCMI,
                         NULL) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (1);
    }
}

static void
_ipmi_dcmi_args_validate (struct ipmi_dcmi_arguments *cmd_args)
{
  if (!cmd_args->get_dcmi_capability_info
      && !cmd_args->get_system_power_statistics
      && !cmd_args->get_enhanced_system_power_statistics
      && !cmd_args->get_power_limit
      && !cmd_args->set_power_limit
      && !cmd_args->activate_deactivate_power_limit
      && !cmd_args->get_asset_tag
      && !cmd_args->get_dcmi_sensor_info)
    {
      fprintf (stderr,
               "No command specified.\n");
      exit (1);
    }
  
  if ((cmd_args->get_dcmi_capability_info
       + cmd_args->get_system_power_statistics
       + cmd_args->get_enhanced_system_power_statistics
       + cmd_args->get_power_limit
       + cmd_args->set_power_limit
       + cmd_args->activate_deactivate_power_limit
       + cmd_args->get_asset_tag
       + cmd_args->get_dcmi_sensor_info) > 1)
    {
      fprintf (stderr,
               "Multiple commands specified.\n");
      exit (1);
    }

  if (cmd_args->set_power_limit
      && (!cmd_args->exception_actions
          && !cmd_args->power_limit_requested
          && !cmd_args->correction_time_limit
          && !cmd_args->statistics_sampling_period))
    {
      fprintf (stderr,
               "No power limit configuration changes specified\n");
      exit (1);
    }

}

void
ipmi_dcmi_argp_parse (int argc, char **argv, struct ipmi_dcmi_arguments *cmd_args)
{
  init_common_cmd_args_admin (&(cmd_args->common));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  
  cmd_args->get_dcmi_capability_info = 0;
  cmd_args->get_system_power_statistics = 0;
  cmd_args->get_enhanced_system_power_statistics = 0;
  cmd_args->get_power_limit = 0;
  cmd_args->set_power_limit = 0;
  cmd_args->exception_actions = 0;
  cmd_args->power_limit_requested = 0;
  cmd_args->correction_time_limit = 0;
  cmd_args->statistics_sampling_period = 0;
  cmd_args->activate_deactivate_power_limit = 0;
  cmd_args->get_asset_tag = 0;
  cmd_args->get_dcmi_sensor_info = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common));

  _ipmi_dcmi_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
  _ipmi_dcmi_args_validate (cmd_args);
}
