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
  "bmc-device - perform advanced BMC commands";

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
    {"set-acpi-power-state", CMD_SET_ACPI_POWER_STATE_KEY, NULL, 0,
     "Set ACPI power state.", 34},
    {"set-acpi-system-power-state", SET_ACPI_SYSTEM_POWER_STATE_KEY, "SYSTEM_POWER_STATE", 0,
     "Set ACPI system power state.", 35},
    {"set-acpi-device-power-state", SET_ACPI_DEVICE_POWER_STATE_KEY, "DEVICE_POWER_STATE", 0,
     "Set ACPI device power state.", 36},
    {"get-lan-statistics", CMD_GET_LAN_STATISTICS_KEY, NULL, 0,
     "Get IP, UDP, and RMCP statistics.", 37},
    {"clear-lan-statistics", CMD_CLEAR_LAN_STATISTICS_KEY, NULL, 0,
     "Clear IP, UDP, and RMCP statistics.", 38},
    {"get-sdr-repository-time",   CMD_GET_SDR_REPOSITORY_TIME_KEY,  0, 0,
     "Get SDR repository time.", 39},
    {"set-sdr-repository-time",   CMD_SET_SDR_REPOSITORY_TIME_KEY,  "TIME", 0,
     "Set SDR repository time.  Input format = \"MM/DD/YYYY - HH:MM:SS\" or \"now\".", 40},
    {"get-sel-time", CMD_GET_SEL_TIME_KEY,  0, 0,
     "Get SEL time.", 41},
    {"set-sel-time", CMD_SET_SEL_TIME_KEY,  "TIME", 0,
     "Set SEL time.  Input format = \"MM/DD/YYYY - HH:MM:SS\" or \"now\".", 42},
    { "platform-event", PLATFORM_EVENT_KEY, "[generator_id] <event_message_format_version> <sensor_type> <sensor_number> <event_type> <event_direction> <event_data1> <event_data2> <event_data3>", 0,
      "Instruct the BMC to process the specified event data.", 43},
    {"get-mca-auxiliary-log-status", CMD_GET_MCA_AUXILIARY_LOG_STATUS_KEY, NULL, 0,
     "Get machine check architecture (MCA) auxiliary log status information.", 44},
    {"get-ssif-interface-capabilities", CMD_GET_SSIF_INTERFACE_CAPABILITIES_KEY, NULL, 0,
     "Get SSIF interface capabilities.", 45},
    {"get-kcs-interface-capabilities", CMD_GET_KCS_INTERFACE_CAPABILITIES_KEY, NULL, 0,
     "Get KCS interface capabilities.", 46},
    {"get-bt-interface-capabilities", CMD_GET_BT_INTERFACE_CAPABILITIES_KEY, NULL, 0,
     "Get BT interface capabilities.", 47},
    {"verbose", VERBOSE_KEY, 0, 0,
     "Increase verbosity in output.", 48},
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
    case CMD_SET_ACPI_POWER_STATE_KEY:
      cmd_args->set_acpi_power_state++;
      break;
    case SET_ACPI_SYSTEM_POWER_STATE_KEY:
      if (!strcasecmp(arg, "S0") /* acceptable here */
          || !strcasecmp(arg, "G0") /* acceptable here */
          || !strcasecmp(arg, "S0_G0")
          || !strcasecmp(arg, "S0/G0")) /* just in case */
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S0_G0;
      else if (!strcasecmp(arg, "S1"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S1;
      else if (!strcasecmp(arg, "S2"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S2;
      else if (!strcasecmp(arg, "S3"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S3;
      else if (!strcasecmp(arg, "S4"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S4;
      else if (!strcasecmp(arg, "G2") /* acceptable here */
               || !strcasecmp(arg, "S5_G2")
               || !strcasecmp(arg, "S5/G2")) /* just in case */
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S5_G2;
      else if (!strcasecmp(arg, "S4_S5")
               || !strcasecmp(arg, "S4/S5")) /* just in case */
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S4_S5;
      else if (!strcasecmp(arg, "G3"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_G3;
      else if (!strcasecmp(arg, "SLEEPING"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_SLEEPING;
      else if (!strcasecmp(arg, "G1_SLEEPING"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_G1_SLEEPING;
      else if (!strcasecmp(arg, "OVERRIDE"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_OVERRIDE;
      else if (!strcasecmp(arg, "LEGACY_ON")) 
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_LEGACY_ON;
      else if (!strcasecmp(arg, "LEGACY_OFF"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_LEGACY_OFF;
      else if (!strcasecmp(arg, "UNKNOWN"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_UNKNOWN;
      else
        {
          fprintf(stderr, "invalid value for system power state\n");
          exit(1);
        }
      break;
    case SET_ACPI_DEVICE_POWER_STATE_KEY:
      if (!strcasecmp(arg, "D0"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D0;
      else if (!strcasecmp(arg, "D1"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D1;
      else if (!strcasecmp(arg, "D2"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D2;
      else if (!strcasecmp(arg, "D3"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D3;
      else if (!strcasecmp(arg, "UNKNOWN"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_UNKNOWN;
      else
        {
          fprintf(stderr, "invalid value for device power state\n");
          exit(1);
        }
      break;
    case CMD_GET_LAN_STATISTICS_KEY:
      cmd_args->get_lan_statistics++;
      break;
    case CMD_CLEAR_LAN_STATISTICS_KEY:
      cmd_args->clear_lan_statistics++;
      break;
    case CMD_GET_SDR_REPOSITORY_TIME_KEY:
      cmd_args->get_sdr_repository_time = 1;
      break;
    case CMD_SET_SDR_REPOSITORY_TIME_KEY:
      cmd_args->set_sdr_repository_time = 1;
      cmd_args->set_sdr_repository_time_arg = arg;
      break;
    case CMD_GET_SEL_TIME_KEY:
      cmd_args->get_sel_time = 1;
      break;
    case CMD_SET_SEL_TIME_KEY:
      cmd_args->set_sel_time = 1;
      cmd_args->set_sel_time_arg = arg;
      break;
    case PLATFORM_EVENT_KEY:
      cmd_args->platform_event = 1;
      cmd_args->platform_event_arg = arg;
      break;
    case CMD_GET_MCA_AUXILIARY_LOG_STATUS_KEY:
      cmd_args->get_mca_auxiliary_log_status = 1;
      break;
    case CMD_GET_SSIF_INTERFACE_CAPABILITIES_KEY:
      cmd_args->get_ssif_interface_capabilities = 1;
      break;
    case CMD_GET_KCS_INTERFACE_CAPABILITIES_KEY:
      cmd_args->get_kcs_interface_capabilities = 1;
      break;
    case CMD_GET_BT_INTERFACE_CAPABILITIES_KEY:
      cmd_args->get_bt_interface_capabilities = 1;
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
                         CONFIG_FILE_TOOL_BMC_DEVICE,
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
      && !cmd_args->get_acpi_power_state
      && !cmd_args->set_acpi_power_state
      && !cmd_args->get_lan_statistics
      && !cmd_args->clear_lan_statistics
      && !cmd_args->get_sdr_repository_time
      && !cmd_args->set_sdr_repository_time
      && !cmd_args->get_sel_time
      && !cmd_args->set_sel_time
      && !cmd_args->platform_event
      && !cmd_args->get_mca_auxiliary_log_status
      && !cmd_args->get_ssif_interface_capabilities
      && !cmd_args->get_kcs_interface_capabilities
      && !cmd_args->get_bt_interface_capabilities)
    {
      fprintf (stderr, 
               "No command specified.\n");
      exit(1);
    }

  if ((cmd_args->cold_reset 
       + cmd_args->warm_reset
       + cmd_args->get_self_test_results
       + cmd_args->get_acpi_power_state
       + cmd_args->set_acpi_power_state
       + cmd_args->get_lan_statistics
       + cmd_args->clear_lan_statistics
       + cmd_args->get_sdr_repository_time
       + cmd_args->set_sdr_repository_time
       + cmd_args->get_sel_time
       + cmd_args->set_sel_time
       + cmd_args->platform_event
       + cmd_args->get_mca_auxiliary_log_status
       + cmd_args->get_ssif_interface_capabilities
       + cmd_args->get_kcs_interface_capabilities
       + cmd_args->get_bt_interface_capabilities) > 1)
    {
      fprintf (stderr, 
               "Multiple commands specified.\n");
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
  cmd_args->set_acpi_power_state = 0;
  cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_NO_CHANGE;
  cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_NO_CHANGE;
  cmd_args->get_lan_statistics = 0;
  cmd_args->clear_lan_statistics = 0;
  cmd_args->get_sdr_repository_time = 0;
  cmd_args->set_sdr_repository_time = 0;
  cmd_args->set_sdr_repository_time_arg = NULL;
  cmd_args->get_sel_time = 0;
  cmd_args->set_sel_time = 0;
  cmd_args->set_sel_time_arg = NULL;
  cmd_args->platform_event = 0;
  cmd_args->platform_event_arg = NULL;
  cmd_args->get_mca_auxiliary_log_status = 0;
  cmd_args->get_ssif_interface_capabilities = 0;
  cmd_args->get_kcs_interface_capabilities = 0;
  cmd_args->get_bt_interface_capabilities = 0;
  cmd_args->verbose = 0;

  argp_parse (&cmdline_config_file_argp, argc, argv, ARGP_IN_ORDER, NULL, &(cmd_args->common));

  _bmc_device_config_file_parse(cmd_args);

  argp_parse (&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
  _bmc_device_args_validate (cmd_args);
}

