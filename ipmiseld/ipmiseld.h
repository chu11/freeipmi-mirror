/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMISELD_H
#define IPMISELD_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "tool-oem-common.h"
#include "tool-sensor-common.h"

#define IPMISELD_POLL_INTERVAL_DEFAULT       300
#define IPMISELD_POLL_ERROR_INTERVAL_DEFAULT 600

enum ipmiseld_argp_option_keys
  {
    IPMISELD_VERBOSE_KEY = 'v',
    IPMISELD_SENSOR_TYPES_KEY = 't',
    IPMISELD_EXCLUDE_SENSOR_TYPES_KEY = 'T',
    IPMISELD_SYSTEM_EVENT_ONLY_KEY = 160,
    IPMISELD_OEM_EVENT_ONLY_KEY = 161,
    IPMISELD_EVENT_STATE_CONFIG_FILE_KEY = 162,
    IPMISELD_INTERPRET_OEM_DATA_KEY = 163,
    IPMISELD_OUTPUT_OEM_EVENT_STRINGS_KEY = 164,
    IPMISELD_ENTITY_SENSOR_NAMES_KEY = 165,
    IPMISELD_NON_ABBREVIATED_UNITS_KEY = 166,
    IPMISELD_EVENT_STATE_FILTER_KEY = 167,
    IPMISELD_SYSTEM_EVENT_FORMAT_KEY = 168,
    IPMISELD_OEM_TIMESTAMPED_EVENT_FORMAT_KEY = 169,
    IPMISELD_OEM_NON_TIMESTAMPED_EVENT_FORMAT_KEY = 170,
    IPMISELD_POLL_INTERVAL_KEY = 171,
    IPMISELD_POLL_ERROR_INTERVAL_KEY = 172,
    IPMISELD_LOG_FACILITY_KEY = 173,
    IPMISELD_LOG_PRIORITY_KEY = 174,
    IPMISELD_CACHE_DIRECTORY_KEY = 175,
    IPMISELD_TEST_RUN_KEY = 176,
    IPMISELD_FOREGROUND_KEY = 177,
  };

struct ipmiseld_arguments
{
  struct common_cmd_args common_args;
  int verbose_count;
  char sensor_types[MAX_SENSOR_TYPES][MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int sensor_types_length;
  char exclude_sensor_types[MAX_SENSOR_TYPES][MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int exclude_sensor_types_length;
  int system_event_only;
  int oem_event_only;
  char *event_state_config_file;
  int interpret_oem_data;
  int output_oem_event_strings;
  int entity_sensor_names;
  int non_abbreviated_units;
  char *event_state_filter_str;
  char *system_event_format_str;
  char *oem_timestamped_event_format_str;
  char *oem_non_timestamped_event_format_str;
  unsigned int poll_interval; 
  unsigned int poll_error_interval; 
  char *log_facility_str;
  char *log_priority_str;
  char *cache_directory;
  int test_run;
  int foreground;
};

typedef struct ipmiseld_prog_data
{
  char *progname;
  int event_state_filter_mask;
  int log_facility;
  int log_priority;
  struct ipmiseld_arguments *args;
} ipmiseld_prog_data_t;

typedef struct ipmiseld_state_data
{
  ipmiseld_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  char *hostname;
  ipmi_sdr_ctx_t sdr_ctx;
  ipmi_sel_ctx_t sel_ctx;
  ipmi_interpret_ctx_t interpret_ctx;
  struct ipmi_oem_data oem_data;
} ipmiseld_state_data_t;

#endif /* IPMISELD_H */
