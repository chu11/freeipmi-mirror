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

#ifndef IPMI_SENSORS_H
#define IPMI_SENSORS_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "tool-oem-common.h"
#include "tool-sensor-common.h"
#include "pstdout.h"

enum ipmi_sensors_argp_option_keys
  {
    VERBOSE_KEY = 'v',
    SDR_INFO_KEY = 'i',
    QUIET_READINGS_KEY = 'q',
    SENSORS_KEY = 's',          /* legacy */
    RECORD_IDS_KEY = 'r',
    EXCLUDE_RECORD_IDS_KEY = 'R',
    GROUP_KEY = 160,              /* legacy */
    GROUPS_KEY = 'g',             /* legacy */
    EXCLUDE_GROUPS_KEY = 161,   /* legacy */
    LIST_GROUPS_KEY = 162,      /* legacy */
    SENSOR_TYPE_KEY = 163,
    SENSOR_TYPES_KEY = 't',
    EXCLUDE_SENSOR_TYPES_KEY = 'T',
    LIST_SENSOR_TYPES_KEY = 'L',
    BRIDGE_SENSORS_KEY = 'b',
    SHARED_SENSORS_KEY = 164,
    INTERPRET_OEM_DATA_KEY = 165,
    IGNORE_NOT_AVAILABLE_SENSORS_KEY = 166,
    IGNORE_UNRECOGNIZED_EVENTS_KEY = 167,
    OUTPUT_EVENT_BITMASK_KEY = 168,
    OUTPUT_SENSOR_STATE_KEY = 169,
    SENSOR_STATE_CONFIG_FILE_KEY = 170,
    ENTITY_SENSOR_NAMES_KEY = 171,
    OUTPUT_SENSOR_THRESHOLDS_KEY = 172,
    NO_SENSOR_TYPE_OUTPUT_KEY = 173,
    COMMA_SEPARATED_OUTPUT_KEY = 174,
    NO_HEADER_OUTPUT_KEY = 175,
    NON_ABBREVIATED_UNITS_KEY = 176,
    LEGACY_OUTPUT_KEY = 177,
    IPMIMONITORING_LEGACY_OUTPUT_KEY = 178,
  };

struct ipmi_sensors_arguments
{
  struct common_cmd_args common_args;
  unsigned int verbose_count;
  int sdr_info;
  int quiet_readings;
  unsigned int record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int record_ids_length;
  unsigned int exclude_record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int exclude_record_ids_length;
  char sensor_types[MAX_SENSOR_TYPES][MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int sensor_types_length;
  char exclude_sensor_types[MAX_SENSOR_TYPES][MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int exclude_sensor_types_length;
  int list_sensor_types;
  int bridge_sensors;
  int shared_sensors;
  int interpret_oem_data;
  int ignore_not_available_sensors;
  int ignore_unrecognized_events;
  int output_event_bitmask;
  int output_sensor_state;
  char *sensor_state_config_file;
  int entity_sensor_names;
  int output_sensor_thresholds;
  int no_sensor_type_output;
  int comma_separated_output;
  int no_header_output;
  int non_abbreviated_units;
  int legacy_output;
  int ipmimonitoring_legacy_output;
};

typedef struct ipmi_sensors_prog_data
{
  char *progname;
  struct ipmi_sensors_arguments *args;
} ipmi_sensors_prog_data_t;

struct ipmi_sensors_interpret_oem_data_intel_node_manager {
  int node_manager_data_found;
  uint8_t nm_health_event_sensor_number;
  uint8_t nm_exception_event_sensor_number;
  uint8_t nm_operational_capabilities_sensor_number;
  uint8_t nm_alert_threshold_exceeded_sensor_number;
};

typedef struct ipmi_sensors_state_data
{
  ipmi_sensors_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_ctx_t sdr_ctx;
  ipmi_sensor_read_ctx_t sensor_read_ctx;
  ipmi_interpret_ctx_t interpret_ctx;
  int output_headers;
  struct sensor_column_width column_width;
  struct ipmi_oem_data oem_data;
  struct ipmi_sensors_interpret_oem_data_intel_node_manager intel_node_manager;
} ipmi_sensors_state_data_t;

#endif /* IPMI_SENSORS_H */
