/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _TOOL_SENSOR_COMMON_H
#define _TOOL_SENSOR_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include <freeipmi/freeipmi.h>

#include "pstdout.h"

#define UNRECOGNIZED_SENSOR_GROUP           "Unrecognized"

#define SENSORS_HEADER_RECORD_ID_STR        "Record ID"
#define SENSORS_HEADER_NAME_STR             "Sensor Name"
#define SENSORS_HEADER_GROUP_STR            "Sensor Group"
#define SENSORS_HEADER_STATE_STR            "Sensor State"
#define SENSORS_HEADER_READING_STR          "Sensor Reading"
#define SENSORS_HEADER_UNITS_STR            "Sensor Units"
#define SENSORS_HEADER_EVENT_STR            "Sensor Event"

#define MAX_SENSOR_RECORD_IDS               256
#define MAX_SENSOR_GROUPS                   256
#define MAX_SENSOR_GROUPS_STRING_LENGTH     256

struct sensor_column_width
{
  int sensor_name;
  int sensor_group;
  int sensor_units;
};

const char * get_sensor_group_output_string (unsigned int sensor_type);

void get_sensor_group_cmdline_string (char *sensor_group);

int display_sensor_group_cmdline (pstdout_state_t pstate, 
                                  unsigned int sensor_type);

int display_string_cmdline (pstdout_state_t pstate, 
                            const char *str);

int sensor_group_strcmp (pstdout_state_t pstate,
                         const char *sensor_group_str_input,
                         unsigned int sensor_type);

int get_sensor_units_output_string (pstdout_state_t pstate,
                                    ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    char *sensor_units_buf,
                                    unsigned int sensor_units_buflen,
                                    unsigned int abbreviated_units_flag);

int is_sdr_sensor_group_listed (pstdout_state_t pstate,
                                ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                                uint8_t *sdr_record,
                                unsigned int sdr_record_len,
                                char groups[][MAX_SENSOR_GROUPS_STRING_LENGTH+1],
                                unsigned int groups_length);

void output_sensor_headers (pstdout_state_t pstate,
                            int quiet_readings,
                            int output_sensor_state,
                            struct sensor_column_width *column_width);

int calculate_column_widths (pstdout_state_t pstate,
                             ipmi_sdr_cache_ctx_t sdr_cache_ctx,
                             ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                             char groups[][MAX_SENSOR_GROUPS_STRING_LENGTH+1],
                             unsigned int groups_length,
                             unsigned int sensors[],
                             unsigned int sensors_length,
                             unsigned int abbreviated_units,
                             struct sensor_column_width *column_width);

#endif
