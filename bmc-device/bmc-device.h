/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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

#ifndef _BMC_DEVICE_H
#define _BMC_DEVICE_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

enum bmc_device_argp_option_keys
  {
    COLD_RESET_KEY = 160,
    WARM_RESET_KEY = 161,
    GET_SELF_TEST_RESULTS_KEY = 162,
    GET_ACPI_POWER_STATE_KEY = 163,
    SET_ACPI_POWER_STATE_KEY = 164,
    GET_LAN_STATISTICS_KEY = 165,
    CLEAR_LAN_STATISTICS_KEY = 166,
    REARM_SENSOR_KEY = 167,
    GET_SDR_REPOSITORY_TIME_KEY = 168,
    SET_SDR_REPOSITORY_TIME_KEY = 169,
    GET_SEL_TIME_KEY = 170,
    SET_SEL_TIME_KEY = 171,
    PLATFORM_EVENT_KEY = 172,
    SET_SENSOR_READING_AND_EVENT_STATUS_KEY = 173,
    GET_MCA_AUXILIARY_LOG_STATUS_KEY = 174,
    GET_SSIF_INTERFACE_CAPABILITIES_KEY = 175,
    GET_KCS_INTERFACE_CAPABILITIES_KEY = 176,
    GET_BT_INTERFACE_CAPABILITIES_KEY = 177,
    GET_BMC_GLOBAL_ENABLES_KEY = 178,
    SET_SYSTEM_FIRMWARE_VERSION_KEY=179,
    SET_SYSTEM_NAME_KEY=180,
    SET_PRIMARY_OPERATING_SYSTEM_NAME_KEY=181,
    SET_OPERATING_SYSTEM_NAME_KEY=182,
    VERBOSE_KEY = 183,
  };

enum bmc_device_set_acpi_power_state_options
  {
    SET_ACPI_SYSTEM_POWER_STATE_KEY = 190,
    SET_ACPI_DEVICE_POWER_STATE_KEY = 191,
  };

#define SYSTEM_INFO_STRING_MAX 255

struct bmc_device_set_acpi_power_state
{
  uint8_t system_power_state;
  uint8_t device_power_state;
};

struct bmc_device_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
  int cold_reset;
  int warm_reset;
  int get_self_test_results;
  int get_acpi_power_state;
  int set_acpi_power_state;
  struct bmc_device_set_acpi_power_state set_acpi_power_state_args;
  int get_lan_statistics;
  int clear_lan_statistics;
  int rearm_sensor;
  char *rearm_sensor_arg;
  int get_sdr_repository_time;
  int set_sdr_repository_time;
  char *set_sdr_repository_time_arg;
  int get_sel_time;
  int set_sel_time;
  char *set_sel_time_arg;
  int platform_event;
  char *platform_event_arg;
  int set_sensor_reading_and_event_status;
  char *set_sensor_reading_and_event_status_arg;
  int get_mca_auxiliary_log_status;
  int get_ssif_interface_capabilities;
  int get_kcs_interface_capabilities;
  int get_bt_interface_capabilities;
  int get_bmc_global_enables;
  int set_system_firmware_version;
  char *set_system_firmware_version_arg;
  int set_system_name;
  char *set_system_name_arg;
  int set_primary_operating_system_name;
  char *set_primary_operating_system_name_arg;
  int set_operating_system_name;
  char *set_operating_system_name_arg;
  int verbose;
};

typedef struct bmc_device_prog_data
{
  char *progname;
  struct bmc_device_arguments *args;
} bmc_device_prog_data_t;

typedef struct bmc_device_state_data
{
  bmc_device_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_cache_ctx_t sdr_cache_ctx;
  ipmi_sdr_parse_ctx_t sdr_parse_ctx;
} bmc_device_state_data_t;

#endif
