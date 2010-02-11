/*****************************************************************************\
 *  $Id: ipmimonitoring-sensors.h,v 1.1.2.9 2010-02-11 21:57:22 chu11 Exp $
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

#include <ipmi_monitoring.h>

#include "tool-cmdline-common.h"

#define IPMI_MONITORING_SENSORS_MAX_RECORD_IDS                 128
#define IPMI_MONITORING_SENSORS_MAX_SENSOR_TYPES               16
#define IPMI_MONITORING_SENSORS_MAX_SENSOR_TYPES_STRING_LENGTH 256

enum ipmimonitoring_sensors_argp_option_keys
  {
    RECORD_IDS_KEY = 'r',
    SENSOR_TYPES_KEY = 't',
    REREAD_SDR_CACHE_KEY = 161,
    IGNORE_NON_INTERPRETABLE_SENSORS_KEY = 162,
    BRIDGE_SENSORS_KEY = 'b',
    INTERPRET_OEM_DATA_KEY = 163,
    SHARED_SENSORS_KEY = 164,
    SDR_CACHE_DIRECTORY_KEY = 165,
    SENSOR_CONFIG_FILE_KEY = 166,
  };

struct ipmimonitoring_sensors_arguments
{
  struct common_cmd_args common;
  int regenerate_sdr_cache;
  unsigned int record_ids[IPMI_MONITORING_SENSORS_MAX_RECORD_IDS];
  unsigned int record_ids_length;
  char sensor_types[IPMI_MONITORING_SENSORS_MAX_SENSOR_TYPES][IPMI_MONITORING_SENSORS_MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int sensor_types_length;
  int reread_sdr_cache;
  int ignore_non_interpretable_sensors;
  int bridge_sensors;
  int interpret_oem_data;
  int shared_sensors;
  char *sdr_cache_directory;
  char *sensor_config_file;

  struct ipmi_monitoring_ipmi_config conf;
  int ipmimonitoring_flags;
  unsigned int ipmimonitoring_sensor_types[IPMI_MONITORING_SENSORS_MAX_SENSOR_TYPES];
  unsigned int ipmimonitoring_sensor_types_length;
};

typedef struct ipmimonitoring_sensors_prog_data
{
  char *progname;
  struct ipmimonitoring_sensors_arguments *args;
} ipmimonitoring_sensors_prog_data_t;

#endif
