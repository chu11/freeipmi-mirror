/*****************************************************************************\
 *  $Id: ipmi-dcmi-argp.c,v 1.10 2010-05-17 17:42:45 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2009-2011 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-413270
 *
 *  This file is part of Ipmi-Dcmi, tools and libraries to support the
 *  data center manageability interface (DCMI).  For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Ipmi-Dcmi is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
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
  "Copyright (C) 2009-2011 FreeIPMI Core Team\n"
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
    { "get-asset-tag", GET_ASSET_TAG, NULL, 0,
      "Get asset tag.", 31},
    { "set-asset-tag", SET_ASSET_TAG, "STRING", 0,
      "Set asset tag.", 32},
    { "get-management-controller-identifier-string", GET_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING, NULL, 0,
      "Get management controller identifier string.", 33},
    { "set-management-controller-identifier-string", SET_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING, "STRING", 0,
      "Set management controller identifier string.", 34},
    { "get-dcmi-sensor-info", GET_DCMI_SENSOR_INFO, NULL, 0,
      "Get DCMI sensor information.", 35},
    { "get-system-power-statistics", GET_SYSTEM_POWER_STATISTICS, NULL, 0,
      "Get system power statistics.", 36},
    { "get-enhanced-system-power-statistics", GET_ENHANCED_SYSTEM_POWER_STATISTICS, NULL, 0,
      "Get enhanced system power statistics.", 37},
    { "get-power-limit", GET_POWER_LIMIT, NULL, 0,
      "Get power limit information.", 38},
    { "set-power-limit", SET_POWER_LIMIT, NULL, 0,
      "Set power limit configuration.", 39},
    { "exception-actions", EXCEPTION_ACTIONS, "BITMASK", 0,
      "Specify exception actions for set power limit configuration.", 40},
    { "power-limit-requested", POWER_LIMIT_REQUESTED, "WATTS", 0,
      "Specify power limit for set power limit configuration.", 41},
    { "correction-time-limit", CORRECTION_TIME_LIMIT, "MILLISECONDS", 0,
      "Specify correction time limit for set power limit configuration.", 42},
    { "statistics-sampling-period", STATISTICS_SAMPLING_PERIOD, "SECONDS", 0,
      "Specify management application statistics sampling period for set power limit configuration.", 43},
    { "activate-deactivate-power-limit", ACTIVATE_DEACTIVATE_POWER_LIMIT, "ACTION", 0,
      "Activate or deactivate power limit.", 44},
    { "interpret-oem-data", INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 45},
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
    case GET_ASSET_TAG:
      cmd_args->get_asset_tag++;
      break;
    case SET_ASSET_TAG:
      cmd_args->set_asset_tag++;
      cmd_args->set_asset_tag_arg = arg;

      /* achu: note that the check for the max asset tag length and
       * the management controller identifier string are different.
       * The spec is written such that the max length of the asset tag
       * is 63, while the max length of the management controller
       * identifier string is 64 w/ a NUL byte included.  That's the
       * literal wording.  Because that's what they're writing, that's
       * what I'm programming.
       */

      if (strlen (cmd_args->set_asset_tag_arg) > IPMI_DCMI_MAX_ASSET_TAG_LENGTH)
        {
          fprintf (stderr, "asset tag invalid length\n");
          exit (1);
        }
      break;
    case GET_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING:
      cmd_args->get_management_controller_identifier_string++;
      break;
    case SET_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING:
      cmd_args->set_management_controller_identifier_string++;
      cmd_args->set_management_controller_identifier_string_arg = arg;
      /* IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH includes NUL char, so subtract 1 in check */
      if (strlen (cmd_args->set_management_controller_identifier_string_arg) > (IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH - 1))
        {
          fprintf (stderr, "management controller identifier string invalid length\n");
          exit (1);
        }
      break;
    case GET_DCMI_SENSOR_INFO:
      cmd_args->get_dcmi_sensor_info++;
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
      if (!strcasecmp (arg, "NO_ACTION"))
        {
          cmd_args->exception_actions_arg = IPMI_DCMI_EXCEPTION_ACTION_NO_ACTION;
          break;
        }
      else if (!strcasecmp (arg, "HARD_POWER_OFF_SYSTEM"))
        {
          cmd_args->exception_actions_arg = IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM;
          break;
        }
      else if (!strcasecmp (arg, "LOG_EVENT_TO_SEL_ONLY"))
        {
          cmd_args->exception_actions_arg = IPMI_DCMI_EXCEPTION_ACTION_LOG_EVENT_TO_SEL_ONLY;
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
      ret = common_parse_opt (key, arg, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, &(cmd_args->hostrange));
      return (ret);
    }

  return (0);
}

static void
_ipmi_dcmi_config_file_parse (struct ipmi_dcmi_arguments *cmd_args)
{
  struct config_file_data_ipmi_dcmi config_file_data;

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_dcmi));

  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         NULL,
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_DCMI,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (1);
    }

  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
}

static void
_ipmi_dcmi_args_validate (struct ipmi_dcmi_arguments *cmd_args)
{
  if (!cmd_args->get_dcmi_capability_info
      && !cmd_args->get_asset_tag
      && !cmd_args->set_asset_tag
      && !cmd_args->get_management_controller_identifier_string
      && !cmd_args->set_management_controller_identifier_string
      && !cmd_args->get_dcmi_sensor_info
      && !cmd_args->get_system_power_statistics
      && !cmd_args->get_enhanced_system_power_statistics
      && !cmd_args->get_power_limit
      && !cmd_args->set_power_limit
      && !cmd_args->activate_deactivate_power_limit)
    {
      fprintf (stderr,
               "No command specified.\n");
      exit (1);
    }
  
  if ((cmd_args->get_dcmi_capability_info
       + cmd_args->get_asset_tag
       + cmd_args->set_asset_tag
       + cmd_args->get_management_controller_identifier_string
       + cmd_args->set_management_controller_identifier_string
       + cmd_args->get_dcmi_sensor_info
       + cmd_args->get_system_power_statistics
       + cmd_args->get_enhanced_system_power_statistics
       + cmd_args->get_power_limit
       + cmd_args->set_power_limit
       + cmd_args->activate_deactivate_power_limit) > 1)
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
  cmd_args->get_asset_tag = 0;
  cmd_args->set_asset_tag = 0;
  cmd_args->set_asset_tag_arg = NULL;
  cmd_args->get_management_controller_identifier_string = 0;
  cmd_args->set_management_controller_identifier_string = 0;
  cmd_args->set_management_controller_identifier_string_arg = NULL;
  cmd_args->get_dcmi_sensor_info = 0;
  cmd_args->get_system_power_statistics = 0;
  cmd_args->get_enhanced_system_power_statistics = 0;
  cmd_args->get_power_limit = 0;
  cmd_args->set_power_limit = 0;
  cmd_args->exception_actions = 0;
  cmd_args->power_limit_requested = 0;
  cmd_args->correction_time_limit = 0;
  cmd_args->statistics_sampling_period = 0;
  cmd_args->activate_deactivate_power_limit = 0;
  cmd_args->interpret_oem_data = 0;

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
