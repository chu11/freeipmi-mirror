/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

#include "bmc-device.h"
#include "bmc-device-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "bmc-device - " PACKAGE_VERSION "\n"
  "Copyright (C) 2008-2014 FreeIPMI Core Team\n"
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
    ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_CACHE_OPTIONS,
    ARGP_COMMON_SDR_CACHE_OPTIONS_FILE_DIRECTORY,
    ARGP_COMMON_TIME_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "cold-reset", COLD_RESET_KEY, NULL, 0,
      "Perform a cold reset.", 40},
    { "warm-reset", WARM_RESET_KEY, NULL, 0,
      "Perform a warm reset.", 41},
    { "get-self-test-results", GET_SELF_TEST_RESULTS_KEY, NULL, 0,
      "Output BMC self test results.", 42},
    { "get-acpi-power-state", GET_ACPI_POWER_STATE_KEY, NULL, 0,
      "Get ACPI system and device power state.", 43},
    { "set-acpi-power-state", SET_ACPI_POWER_STATE_KEY, NULL, 0,
      "Set ACPI power state.", 44},
    { "set-acpi-system-power-state", SET_ACPI_SYSTEM_POWER_STATE_KEY, "SYSTEM_POWER_STATE", 0,
      "Set ACPI system power state.", 45},
    { "set-acpi-device-power-state", SET_ACPI_DEVICE_POWER_STATE_KEY, "DEVICE_POWER_STATE", 0,
      "Set ACPI device power state.", 46},
    { "get-lan-statistics", GET_LAN_STATISTICS_KEY, NULL, 0,
      "Get IP, UDP, and RMCP statistics.", 47},
    { "clear-lan-statistics", CLEAR_LAN_STATISTICS_KEY, NULL, 0,
      "Clear IP, UDP, and RMCP statistics.", 48},
    { "rearm-sensor", REARM_SENSOR_KEY, "<record_id> [<assertion_bitmask> <deassertion_bitmask>]", 0,
      "Re-arm a sensor.", 49},
    { "get-sdr-repository-time",   GET_SDR_REPOSITORY_TIME_KEY,  0, 0,
      "Get SDR repository time.", 50},
    { "set-sdr-repository-time",   SET_SDR_REPOSITORY_TIME_KEY,  "TIME", 0,
      "Set SDR repository time.  Input format = \"MM/DD/YYYY - HH:MM:SS\" or \"now\".", 51},
    { "get-sel-time", GET_SEL_TIME_KEY,  0, 0,
      "Get SEL time.", 52},
    { "set-sel-time", SET_SEL_TIME_KEY,  "TIME", 0,
      "Set SEL time.  Input format = \"MM/DD/YYYY - HH:MM:SS\" or \"now\".", 53},
    { "get-sel-time-utc-offset", GET_SEL_TIME_UTC_OFFSET_KEY,  0, 0,
      "Get SEL time UTC offset.", 53},
    { "set-sel-time-utc-offset", SET_SEL_TIME_UTC_OFFSET_KEY,  "OFFSET", 0,
      "Set SEL time UTC offset.  Offset in minutes or \"none\".", 54},
    { "platform-event", PLATFORM_EVENT_KEY, "[generator_id] <event_message_format_version> <sensor_type> <sensor_number> <event_type> <event_direction> <event_data1> <event_data2> <event_data3>", 0,
      "Instruct the BMC to process the specified event data.", 54},
    { "set-sensor-reading-and-event-status", SET_SENSOR_READING_AND_EVENT_STATUS_KEY, "<sensor_number> <sensor_reading> <sensor_reading_operation> <assertion_bitmask> <assertion_bitmask_operation> <deassertion_bitmask> <deassertion_bitmask_operation> <event_data1> <event_data2> <event_data3> <event_data_operation>", 0,
      "Instruct the BMC to set a sensor reading and/or event status.", 55},
    { "get-mca-auxiliary-log-status", GET_MCA_AUXILIARY_LOG_STATUS_KEY, NULL, 0,
      "Get machine check architecture (MCA) auxiliary log status information.", 56},
    { "get-ssif-interface-capabilities", GET_SSIF_INTERFACE_CAPABILITIES_KEY, NULL, 0,
      "Get SSIF interface capabilities.", 57},
    { "get-kcs-interface-capabilities", GET_KCS_INTERFACE_CAPABILITIES_KEY, NULL, 0,
      "Get KCS interface capabilities.", 58},
    { "get-bt-interface-capabilities", GET_BT_INTERFACE_CAPABILITIES_KEY, NULL, 0,
      "Get BT interface capabilities.", 59},
    { "get-bmc-global-enables", GET_BMC_GLOBAL_ENABLES_KEY, NULL, 0,
      "Get BMC Global Enables.", 60},
    { "set-system-firmware-version", SET_SYSTEM_FIRMWARE_VERSION_KEY, "STRING", 0,
      "Set System Firmware Version.", 61},
    { "set-system-name", SET_SYSTEM_NAME_KEY, "STRING", 0,
      "Set System Name.", 62},
    { "set-primary-operating-system-name", SET_PRIMARY_OPERATING_SYSTEM_NAME_KEY, "STRING", 0,
      "Set Primary Operating System Name.", 63},
    { "set-operating-system-name", SET_OPERATING_SYSTEM_NAME_KEY, "STRING", 0,
      "Set Operating System Name.", 64},
    { "set-present-os-version-number", SET_PRESENT_OS_VERSION_NUMBER_KEY, "STRING", 0,
      "Set Present Operating System Version Number.", 65},
    { "set-bmc-url", SET_BMC_URL_KEY, "STRING", 0,
      "Set BMC URL.", 66},
    { "set-base-os-hypervisor-url", SET_BASE_OS_HYPERVISOR_URL_KEY, "STRING", 0,
      "Set Base OS/Hypervisor URL.", 67},
    { "verbose", VERBOSE_KEY, 0, 0,
      "Increase verbosity in output.", 68},
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
  struct bmc_device_arguments *cmd_args;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case COLD_RESET_KEY:
      cmd_args->cold_reset++;
      break;
    case WARM_RESET_KEY:
      cmd_args->warm_reset++;
      break;
    case GET_SELF_TEST_RESULTS_KEY:
      cmd_args->get_self_test_results++;
      break;
    case GET_ACPI_POWER_STATE_KEY:
      cmd_args->get_acpi_power_state++;
      break;
    case SET_ACPI_POWER_STATE_KEY:
      cmd_args->set_acpi_power_state++;
      break;
    case SET_ACPI_SYSTEM_POWER_STATE_KEY:
      if (!strcasecmp (arg, "S0") /* acceptable here */
          || !strcasecmp (arg, "G0") /* acceptable here */
          || !strcasecmp (arg, "S0_G0")
          || !strcasecmp (arg, "S0/G0")) /* just in case */
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S0_G0;
      else if (!strcasecmp (arg, "S1"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S1;
      else if (!strcasecmp (arg, "S2"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S2;
      else if (!strcasecmp (arg, "S3"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S3;
      else if (!strcasecmp (arg, "S4"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S4;
      else if (!strcasecmp (arg, "G2") /* acceptable here */
               || !strcasecmp (arg, "S5_G2")
               || !strcasecmp (arg, "S5/G2")) /* just in case */
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S5_G2;
      else if (!strcasecmp (arg, "S4_S5")
               || !strcasecmp (arg, "S4/S5")) /* just in case */
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_S4_S5;
      else if (!strcasecmp (arg, "G3"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_G3;
      else if (!strcasecmp (arg, "SLEEPING"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_SLEEPING;
      else if (!strcasecmp (arg, "G1_SLEEPING"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_G1_SLEEPING;
      else if (!strcasecmp (arg, "OVERRIDE"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_OVERRIDE;
      else if (!strcasecmp (arg, "LEGACY_ON"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_LEGACY_ON;
      else if (!strcasecmp (arg, "LEGACY_OFF"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_LEGACY_OFF;
      else if (!strcasecmp (arg, "UNKNOWN"))
        cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_UNKNOWN;
      else
        {
          fprintf (stderr, "invalid value for system power state\n");
          exit (EXIT_FAILURE);
        }
      break;
    case SET_ACPI_DEVICE_POWER_STATE_KEY:
      if (!strcasecmp (arg, "D0"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D0;
      else if (!strcasecmp (arg, "D1"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D1;
      else if (!strcasecmp (arg, "D2"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D2;
      else if (!strcasecmp (arg, "D3"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_D3;
      else if (!strcasecmp (arg, "UNKNOWN"))
        cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_UNKNOWN;
      else
        {
          fprintf (stderr, "invalid value for device power state\n");
          exit (EXIT_FAILURE);
        }
      break;
    case GET_LAN_STATISTICS_KEY:
      cmd_args->get_lan_statistics++;
      break;
    case CLEAR_LAN_STATISTICS_KEY:
      cmd_args->clear_lan_statistics++;
      break;
    case REARM_SENSOR_KEY:
      cmd_args->rearm_sensor = 1;
      cmd_args->rearm_sensor_arg = arg;
      break;
    case GET_SDR_REPOSITORY_TIME_KEY:
      cmd_args->get_sdr_repository_time = 1;
      break;
    case SET_SDR_REPOSITORY_TIME_KEY:
      cmd_args->set_sdr_repository_time = 1;
      cmd_args->set_sdr_repository_time_arg = arg;
      break;
    case GET_SEL_TIME_KEY:
      cmd_args->get_sel_time = 1;
      break;
    case SET_SEL_TIME_KEY:
      cmd_args->set_sel_time = 1;
      cmd_args->set_sel_time_arg = arg;
      break;
    case GET_SEL_TIME_UTC_OFFSET_KEY:
      cmd_args->get_sel_time_utc_offset = 1;
      break;
    case SET_SEL_TIME_UTC_OFFSET_KEY:
      cmd_args->set_sel_time_utc_offset = 1;
      cmd_args->set_sel_time_utc_offset_arg = arg;
      break;
    case PLATFORM_EVENT_KEY:
      cmd_args->platform_event = 1;
      cmd_args->platform_event_arg = arg;
      break;
    case SET_SENSOR_READING_AND_EVENT_STATUS_KEY:
      cmd_args->set_sensor_reading_and_event_status = 1;
      cmd_args->set_sensor_reading_and_event_status_arg = arg;
      break;
    case GET_MCA_AUXILIARY_LOG_STATUS_KEY:
      cmd_args->get_mca_auxiliary_log_status = 1;
      break;
    case GET_SSIF_INTERFACE_CAPABILITIES_KEY:
      cmd_args->get_ssif_interface_capabilities = 1;
      break;
    case GET_KCS_INTERFACE_CAPABILITIES_KEY:
      cmd_args->get_kcs_interface_capabilities = 1;
      break;
    case GET_BT_INTERFACE_CAPABILITIES_KEY:
      cmd_args->get_bt_interface_capabilities = 1;
      break;
    case GET_BMC_GLOBAL_ENABLES_KEY:
      cmd_args->get_bmc_global_enables = 1;
      break;
    case SET_SYSTEM_FIRMWARE_VERSION_KEY:
      cmd_args->set_system_firmware_version = 1;
      cmd_args->set_system_firmware_version_arg = arg;
      break;
    case SET_SYSTEM_NAME_KEY:
      cmd_args->set_system_name = 1;
      cmd_args->set_system_name_arg = arg;
      break;
    case SET_PRIMARY_OPERATING_SYSTEM_NAME_KEY:
      cmd_args->set_primary_operating_system_name = 1;
      cmd_args->set_primary_operating_system_name_arg = arg;
      break;
    case SET_OPERATING_SYSTEM_NAME_KEY:
      cmd_args->set_operating_system_name = 1;
      cmd_args->set_operating_system_name_arg = arg;
      break;
    case SET_PRESENT_OS_VERSION_NUMBER_KEY:
      cmd_args->set_present_os_version_number = 1;
      cmd_args->set_present_os_version_number_arg = arg;
      break;
    case SET_BMC_URL_KEY:
      cmd_args->set_bmc_url = 1;
      cmd_args->set_bmc_url_arg = arg;
      break;
    case SET_BASE_OS_HYPERVISOR_URL_KEY:
      cmd_args->set_base_os_hypervisor_url = 1;
      cmd_args->set_base_os_hypervisor_url_arg = arg;
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
      return (common_parse_opt (key, arg, &(cmd_args->common_args)));
    }

  return (0);
}

static void
_bmc_device_config_file_parse (struct bmc_device_arguments *cmd_args)
{
  assert (cmd_args);

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_TIME | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_BMC_DEVICE,
                         NULL) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }
}

static void
_bmc_device_args_validate (struct bmc_device_arguments *cmd_args)
{
  assert (cmd_args);

  if (!cmd_args->common_args.flush_cache
      && !cmd_args->cold_reset
      && !cmd_args->warm_reset
      && !cmd_args->get_self_test_results
      && !cmd_args->get_acpi_power_state
      && !cmd_args->set_acpi_power_state
      && !cmd_args->get_lan_statistics
      && !cmd_args->clear_lan_statistics
      && !cmd_args->rearm_sensor
      && !cmd_args->get_sdr_repository_time
      && !cmd_args->set_sdr_repository_time
      && !cmd_args->get_sel_time
      && !cmd_args->set_sel_time
      && !cmd_args->get_sel_time_utc_offset
      && !cmd_args->set_sel_time_utc_offset
      && !cmd_args->platform_event
      && !cmd_args->set_sensor_reading_and_event_status
      && !cmd_args->get_mca_auxiliary_log_status
      && !cmd_args->get_ssif_interface_capabilities
      && !cmd_args->get_kcs_interface_capabilities
      && !cmd_args->get_bt_interface_capabilities
      && !cmd_args->get_bmc_global_enables
      && !cmd_args->set_system_firmware_version
      && !cmd_args->set_system_name
      && !cmd_args->set_primary_operating_system_name
      && !cmd_args->set_operating_system_name
      && !cmd_args->set_present_os_version_number
      && !cmd_args->set_bmc_url
      && !cmd_args->set_base_os_hypervisor_url)

    {
      fprintf (stderr,
               "No command specified.\n");
      exit (EXIT_FAILURE);
    }

  if ((cmd_args->common_args.flush_cache
       + cmd_args->cold_reset
       + cmd_args->warm_reset
       + cmd_args->get_self_test_results
       + cmd_args->get_acpi_power_state
       + cmd_args->set_acpi_power_state
       + cmd_args->get_lan_statistics
       + cmd_args->clear_lan_statistics
       + cmd_args->rearm_sensor
       + cmd_args->get_sdr_repository_time
       + cmd_args->set_sdr_repository_time
       + cmd_args->get_sel_time
       + cmd_args->set_sel_time
       + cmd_args->get_sel_time_utc_offset
       + cmd_args->set_sel_time_utc_offset
       + cmd_args->platform_event
       + cmd_args->set_sensor_reading_and_event_status
       + cmd_args->get_mca_auxiliary_log_status
       + cmd_args->get_ssif_interface_capabilities
       + cmd_args->get_kcs_interface_capabilities
       + cmd_args->get_bt_interface_capabilities
       + cmd_args->get_bmc_global_enables
       + cmd_args->set_system_firmware_version
       + cmd_args->set_system_name
       + cmd_args->set_primary_operating_system_name
       + cmd_args->set_operating_system_name
       + cmd_args->set_present_os_version_number
       + cmd_args->set_bmc_url
       + cmd_args->set_base_os_hypervisor_url) > 1)
    {
      fprintf (stderr,
               "Multiple commands specified.\n");
      exit (EXIT_FAILURE);
    }
  
  if (cmd_args->set_acpi_power_state
      && (cmd_args->set_acpi_power_state_args.system_power_state == IPMI_ACPI_SYSTEM_POWER_STATE_NO_CHANGE
          && cmd_args->set_acpi_power_state_args.device_power_state == IPMI_ACPI_DEVICE_POWER_STATE_NO_CHANGE))
    {
      fprintf (stderr,
               "No acpi power state configuration changes specified\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->set_system_firmware_version
      && strlen (cmd_args->set_system_firmware_version_arg) > IPMI_SYSTEM_INFO_STRING_LEN_MAX)
    {
      fprintf (stderr,
	       "system firmware version string too long\n");
      exit (EXIT_FAILURE);
    }
  
  if (cmd_args->set_system_name
      && strlen (cmd_args->set_system_name_arg) > IPMI_SYSTEM_INFO_STRING_LEN_MAX)
    {
      fprintf (stderr,
	       "system name string too long\n");
      exit (EXIT_FAILURE);
    }
  
  if (cmd_args->set_primary_operating_system_name
      && strlen (cmd_args->set_primary_operating_system_name_arg) > IPMI_SYSTEM_INFO_STRING_LEN_MAX)
    {
      fprintf (stderr,
	       "primary operating system name string too long\n");
      exit (EXIT_FAILURE);
    }
  
  if (cmd_args->set_operating_system_name
      && strlen (cmd_args->set_operating_system_name_arg) > IPMI_SYSTEM_INFO_STRING_LEN_MAX)
    {
      fprintf (stderr,
	       "operating system name string too long\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->set_present_os_version_number
      && strlen (cmd_args->set_present_os_version_number_arg) > IPMI_SYSTEM_INFO_STRING_LEN_MAX)
    {
      fprintf (stderr,
	       "present OS version number string too long\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->set_bmc_url
      && strlen (cmd_args->set_bmc_url_arg) > IPMI_SYSTEM_INFO_STRING_LEN_MAX)
    {
      fprintf (stderr,
	       "BMC URL string too long\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->set_base_os_hypervisor_url
      && strlen (cmd_args->set_base_os_hypervisor_url_arg) > IPMI_SYSTEM_INFO_STRING_LEN_MAX)
    {
      fprintf (stderr,
	       "Base OS/Hypervisor URL string too long\n");
      exit (EXIT_FAILURE);
    }
}

void
bmc_device_argp_parse (int argc, char **argv, struct bmc_device_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_admin (&(cmd_args->common_args));

  cmd_args->cold_reset = 0;
  cmd_args->warm_reset = 0;
  cmd_args->get_self_test_results = 0;
  cmd_args->get_acpi_power_state = 0;
  cmd_args->set_acpi_power_state = 0;
  cmd_args->set_acpi_power_state_args.system_power_state = IPMI_ACPI_SYSTEM_POWER_STATE_NO_CHANGE;
  cmd_args->set_acpi_power_state_args.device_power_state = IPMI_ACPI_DEVICE_POWER_STATE_NO_CHANGE;
  cmd_args->get_lan_statistics = 0;
  cmd_args->clear_lan_statistics = 0;
  cmd_args->rearm_sensor = 0;
  cmd_args->rearm_sensor_arg = NULL;
  cmd_args->get_sdr_repository_time = 0;
  cmd_args->set_sdr_repository_time = 0;
  cmd_args->set_sdr_repository_time_arg = NULL;
  cmd_args->get_sel_time = 0;
  cmd_args->set_sel_time = 0;
  cmd_args->set_sel_time_arg = NULL;
  cmd_args->get_sel_time_utc_offset = 0;
  cmd_args->set_sel_time_utc_offset = 0;
  cmd_args->set_sel_time_utc_offset_arg = NULL;
  cmd_args->platform_event = 0;
  cmd_args->platform_event_arg = NULL;
  cmd_args->set_sensor_reading_and_event_status = 0;
  cmd_args->set_sensor_reading_and_event_status_arg = NULL;
  cmd_args->get_mca_auxiliary_log_status = 0;
  cmd_args->get_ssif_interface_capabilities = 0;
  cmd_args->get_kcs_interface_capabilities = 0;
  cmd_args->get_bt_interface_capabilities = 0;
  cmd_args->get_bmc_global_enables = 0;
  cmd_args->set_system_firmware_version = 0;
  cmd_args->set_system_firmware_version_arg = NULL;
  cmd_args->set_system_name = 0;
  cmd_args->set_system_name_arg = NULL;
  cmd_args->set_primary_operating_system_name = 0;
  cmd_args->set_primary_operating_system_name_arg = NULL;
  cmd_args->set_operating_system_name = 0;
  cmd_args->set_operating_system_name_arg = NULL;
  cmd_args->set_present_os_version_number = 0;
  cmd_args->set_present_os_version_number_arg = NULL;
  cmd_args->set_bmc_url = 0;
  cmd_args->set_bmc_url_arg = NULL;
  cmd_args->set_base_os_hypervisor_url = 0;
  cmd_args->set_base_os_hypervisor_url_arg = NULL;

  cmd_args->verbose = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _bmc_device_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _bmc_device_args_validate (cmd_args);
}

