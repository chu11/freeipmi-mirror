/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef _TOOL_SENSOR_COMMON_H
#define _TOOL_SENSOR_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include <freeipmi/freeipmi.h>

#include "pstdout.h"

#define UNRECOGNIZED_SENSOR_TYPE            "Unrecognized"

#define SENSORS_HEADER_RECORD_ID_STR        "ID"
#define SENSORS_HEADER_NAME_STR             "Name"
#define SENSORS_HEADER_TYPE_STR             "Type"
#define SENSORS_HEADER_STATE_STR            "State"
#define SENSORS_HEADER_READING_STR          "Reading"
#define SENSORS_HEADER_UNITS_STR            "Units"
#define SENSORS_HEADER_EVENT_STR            "Event"

/* achu:
 *
 * The stack on cygwin is smaller than on unixes, and the ability for
 * me to get a bigger stack in cygwin is difficult.  The normal unix
 * ways (i.e. ulimit -s unlimited) don't work.  Some posts online
 * indicate it's b/c of the way windows works with stack limits.
 *
 * So we're just going to use smaller values to deal with the problem.
 */
#ifdef __CYGWIN__
#define MAX_SENSOR_RECORD_IDS               128
#define MAX_SENSOR_TYPES                    16
#else /* !__CYGWIN__ */
/* record id is 16 bits - 65536 */
#define MAX_SENSOR_RECORD_IDS               65536
#define MAX_SENSOR_TYPES                    256
#endif /* !__CYGWIN__ */
#define MAX_SENSOR_TYPES_STRING_LENGTH      256

#define SENSOR_PARSE_ALL_STRING             "all"
#define SENSOR_PARSE_NONE_STRING            "none"

/* entity_id is a 8 bit field */
#define MAX_ENTITY_IDS                      256

#define MAX_ENTITY_ID_SENSOR_NAME_STRING    256

struct sensor_column_width
{
  int record_id;
  int sensor_name;
  int sensor_type;
  int sensor_units;
};

struct sensor_entity_id_counts
{
  unsigned int count[MAX_ENTITY_IDS];
};

const char * get_sensor_type_output_string (unsigned int sensor_type);
const char * get_oem_sensor_type_output_string (uint8_t sensor_type, uint8_t event_reading_code, uint32_t manufacturer_id, uint16_t product_id);

int get_entity_sensor_name_string (pstdout_state_t pstate,
                                   ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                                   const void *sdr_record,
                                   unsigned int sdr_record_len,
                                   struct sensor_entity_id_counts *entity_id_counts,
                                   uint8_t *sensor_number,
                                   char *sensor_name_buf,
                                   unsigned int sensor_name_buf_len);

int list_sensor_types (pstdout_state_t pstate);

/* 1 if all valid, 0 if not, -1 on error */
int valid_sensor_types (pstdout_state_t pstate,
                        char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                        unsigned int sensor_types_length,
                        unsigned int allow_oem_reserved);

int get_sensor_units_output_string (pstdout_state_t pstate,
                                    ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                                    const void *sdr_record,
                                    unsigned int sdr_record_len,
                                    char *sensor_units_buf,
                                    unsigned int sensor_units_buflen,
                                    unsigned int abbreviated_units_flag);

int sensor_type_listed (pstdout_state_t pstate,
                        uint8_t sensor_type,
                        char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                        unsigned int sensor_types_length);

int sensor_type_listed_sdr (pstdout_state_t pstate,
                            ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                            const void *sdr_record,
                            unsigned int sdr_record_len,
                            char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                            unsigned int sensor_types_length);

int calculate_entity_id_counts (pstdout_state_t pstate,
                                ipmi_sdr_cache_ctx_t sdr_cache_ctx,
                                ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                                struct sensor_entity_id_counts *entity_id_counts);

/* use normal names, set entity_id_counts to NULL */
int calculate_column_widths (pstdout_state_t pstate,
                             ipmi_sdr_cache_ctx_t sdr_cache_ctx,
                             ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                             char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                             unsigned int sensor_types_length,
                             unsigned int record_ids[],
                             unsigned int record_ids_length,
                             unsigned int non_abbreviated_units,
                             unsigned int shared_sensors,
                             unsigned int count_event_only_records,
                             unsigned int count_device_locator_records,
                             unsigned int count_oem_records,
                             struct sensor_entity_id_counts *entity_id_counts,
                             struct sensor_column_width *column_width);

int calculate_column_widths_ignored_sdr_cache (unsigned int non_abbreviated_units,
					       struct sensor_column_width *column_width);
					       
#endif
