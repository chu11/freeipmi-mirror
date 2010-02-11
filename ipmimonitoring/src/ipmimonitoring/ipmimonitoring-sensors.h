/*****************************************************************************\
 *  $Id: ipmimonitoring-sensors.h,v 1.1.2.4 2010-02-11 18:35:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2010 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMIMONITORING_SENSORS_H
#define _IPMIMONITORING_SENSORS_H

#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"

#include "tool-cmdline-common.h"
#include "tool-sensor-common.h"
#include "pstdout.h"

enum ipmimonitoring_sensors_argp_option_keys
  {
    VERBOSE_KEY = 'v',
    RECORD_IDS_KEY = 'r',
    EXCLUDE_RECORD_IDS_KEY = 'R',
    SENSOR_TYPES_KEY = 't',
    EXCLUDE_SENSOR_TYPES_KEY = 'T',
    BRIDGE_SENSORS_KEY = 'b',
    SHARED_SENSORS_KEY = 161,
    INTERPRET_OEM_DATA_KEY = 162,
    IGNORE_NON_INTERPRETABLE_SENSORS_KEY = 163,
    SENSOR_CONFIG_FILE_KEY = 164,
  };

struct ipmimonitoring_sensors_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
  int verbose_count;
  int regenerate_sdr_cache;
  unsigned int record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int record_ids_length;
  unsigned int exclude_record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int exclude_record_ids_length;
  char sensor_types[MAX_SENSOR_TYPES][MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int sensor_types_length;
  char exclude_sensor_types[MAX_SENSOR_TYPES][MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int exclude_sensor_types_length;
  int bridge_sensors;
  int shared_sensors;
  int interpret_oem_data;
  int ignore_non_interpretable_sensors;
  char *sensor_config_file;

  struct ipmi_monitoring_ipmi_config conf;
  int ipmimonitoring_flags;
  unsigned int ipmimonitoring_sensor_types[MAX_SENSOR_TYPES];
  unsigned int ipmimonitoring_sensor_types_length;
};

typedef struct ipmimonitoring_sensors_prog_data
{
  char *progname;
  struct ipmimonitoring_sensors_arguments *args;
} ipmimonitoring_sensors_prog_data_t;

typedef struct ipmimonitoring_sensors_state_data
{
  ipmimonitoring_sensors_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_cache_ctx_t sdr_cache_ctx;
  ipmi_sdr_parse_ctx_t sdr_parse_ctx;
  ipmi_monitoring_ctx_t ctx;
  int output_headers;
  struct sensor_entity_id_counts entity_id_counts;
  struct sensor_column_width column_width;
} ipmimonitoring_sensors_state_data_t;

#endif
