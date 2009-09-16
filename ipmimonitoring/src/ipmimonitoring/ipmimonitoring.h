/*
  Copyright (C) 2006 FreeIPMI Core Team

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

#ifndef _IPMIMONITORING_H
#define _IPMIMONITORING_H

#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"

#include "tool-cmdline-common.h"
#include "tool-sensor-common.h"
#include "pstdout.h"

enum ipmimonitoring_argp_option_keys
  {
    VERBOSE_KEY = 'v',
    QUIET_READINGS_KEY = 'q',
    SENSORS_KEY = 's',          /* legacy */
    RECORD_IDS_KEY = 'r',
    EXCLUDE_RECORD_IDS_KEY = 'R',
    GROUPS_KEY = 'g',           /* legacy */
    EXCLUDE_GROUPS_KEY = 160,   /* legacy */
    LIST_GROUPS_KEY = 161,      /* legacy */
    SENSOR_TYPES_KEY = 't',
    EXCLUDE_SENSOR_TYPES_KEY = 'T',
    LIST_SENSOR_TYPES_KEY = 'L',
    CACHE_DIR_KEY = 162,              /* legacy */
    BRIDGE_SENSORS_KEY = 'b',
    INTERPRET_OEM_DATA_KEY = 163,
    IGNORE_NON_INTERPRETABLE_SENSORS_KEY = 164,
    ENTITY_SENSOR_NAMES_KEY = 165,
    COMMA_SEPARATED_OUTPUT_KEY = 166,
    NON_ABBREVIATED_UNITS_KEY = 167,
    LEGACY_OUTPUT_KEY = 168,
    SENSOR_CONFIG_FILE_KEY = 169,
  };

struct ipmimonitoring_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
  int verbose_count;
  int regenerate_sdr_cache;
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
  int interpret_oem_data;
  int ignore_non_interpretable_sensors;
  int entity_sensor_names;
  int comma_separated_output;
  int non_abbreviated_units;
  int legacy_output;
  char *sensor_config_file;

  struct ipmi_monitoring_ipmi_config conf;
  int ipmimonitoring_flags;
  unsigned int ipmimonitoring_sensor_types[MAX_SENSOR_TYPES];
  unsigned int ipmimonitoring_sensor_types_length;
};

typedef struct ipmimonitoring_prog_data
{
  char *progname;
  struct ipmimonitoring_arguments *args;
} ipmimonitoring_prog_data_t;

typedef struct ipmimonitoring_state_data
{
  ipmimonitoring_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_cache_ctx_t sdr_cache_ctx;
  ipmi_sdr_parse_ctx_t sdr_parse_ctx;
  ipmi_monitoring_ctx_t ctx;
  int output_headers;
  struct sensor_entity_id_counts entity_id_counts;
  struct sensor_column_width column_width;
} ipmimonitoring_state_data_t;

#endif
