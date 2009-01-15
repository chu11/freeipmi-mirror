/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"

#include "ipmi-sensors.h"
#include "ipmi-sensors-display-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sensor-common.h"

static int
_output_verbose_header (ipmi_sensors_state_data_t *state_data,
                        uint8_t *sdr_record,
                        unsigned int sdr_record_len,
                        uint16_t record_id)
{
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  uint8_t sensor_owner_id_type, sensor_owner_id;
  uint8_t sensor_owner_lun, channel_number;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_sensor_number (state_data->pstate,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_number) < 0)
    return -1;

  if (sdr_cache_get_sensor_type (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_type) < 0)
    return -1;

  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    return -1;
  
  memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);

  if (sdr_cache_get_id_string (state_data->pstate,
                               sdr_record,
                               sdr_record_len,
                               id_string,
                               IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
    return -1;

  if (sdr_cache_get_sensor_owner_id (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &sensor_owner_id_type,
                                     &sensor_owner_id) < 0)
    return -1;

  if (sdr_cache_get_sensor_owner_lun (state_data->pstate,
                                      sdr_record,
                                      sdr_record_len,
                                      &sensor_owner_lun,
                                      &channel_number) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "ID String: %s\n", 
                  id_string);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n",
                  sensor_group (sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  sensor_number);
  if (sensor_owner_id_type)
    pstdout_printf (state_data->pstate,
                    "System Software ID: %Xh\n",
                    sensor_owner_id);
  else
    pstdout_printf (state_data->pstate,
                    "IPMB Slave Address: %Xh\n",
                    sensor_owner_id);
  pstdout_printf (state_data->pstate,
                  "Sensor Owner ID: %Xh\n",
                  (sensor_owner_id << 1) | sensor_owner_id_type);
  pstdout_printf (state_data->pstate,
                  "Sensor Owner LUN: %Xh\n",
                  sensor_owner_lun);
  pstdout_printf (state_data->pstate,
                  "Channel Number: %Xh\n",
                  channel_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  event_reading_type_code);

  return 0;
}

static int 
sensors_display_verbose_full_record (ipmi_sensors_state_data_t *state_data,
                                     uint8_t *sdr_record,
                                     unsigned int sdr_record_len,
                                     uint16_t record_id,
                                     double *reading,
                                     char **event_message_list,
                                     unsigned int event_message_list_len)
{
  uint8_t sensor_unit;
  uint8_t event_reading_type_code;
  int sensor_class;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_verbose_header (state_data,
                              sdr_record,
                              sdr_record_len,
                              record_id) < 0)
    return -1;

  if (sdr_cache_get_sensor_unit (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_unit) < 0)
    return -1;

  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    return -1;

  sensor_class = sensor_classify (event_reading_type_code);

  if (sensor_class == SENSOR_CLASS_THRESHOLD)
    {
      if (ipmi_sensors_output_verbose_thresholds (state_data,
                                                  sdr_record,
                                                  sdr_record_len) < 0)
        return -1;
      
      if (ipmi_sensors_output_verbose_sensor_reading_ranges (state_data,
                                                             sdr_record,
                                                             sdr_record_len) < 0)
        return -1;
    }

  if (ipmi_sensors_output_verbose_sensor_reading (state_data,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  reading) < 0)
    return -1;
  
  if (ipmi_sensors_output_verbose_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    return -1;
  
  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_verbose_compact_record (ipmi_sensors_state_data_t *state_data,
                                        uint8_t *sdr_record,
                                        unsigned int sdr_record_len,
                                        uint16_t record_id,
                                        char **event_message_list,
                                        unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_verbose_header (state_data,
                              sdr_record,
                              sdr_record_len,
                              record_id) < 0)
    return -1;

  if (ipmi_sensors_output_verbose_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    return -1;
  
  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_verbose_event_only_record (ipmi_sensors_state_data_t *state_data,
                                           uint8_t *sdr_record,
                                           unsigned int sdr_record_len,
                                           uint16_t record_id)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_verbose_header (state_data,
                              sdr_record,
                              sdr_record_len,
                              record_id) < 0)
    return -1;
  
  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

int
ipmi_sensors_display_verbose (ipmi_sensors_state_data_t *state_data,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len,
                              double *reading,
                              char **event_message_list,
                              unsigned int event_message_list_len)
{
  uint16_t record_id;
  uint8_t record_type;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_record_id_and_type(state_data->pstate,
                                       sdr_record,
                                       sdr_record_len,
                                       &record_id,
                                       &record_type) < 0)
    return -1;

  switch (record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return sensors_display_verbose_full_record (state_data,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  record_id,
                                                  reading,
                                                  event_message_list,
                                                  event_message_list_len);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_verbose_compact_record (state_data,
                                                     sdr_record,
                                                     sdr_record_len,
                                                     record_id,
                                                     event_message_list,
                                                     event_message_list_len);
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return sensors_display_verbose_event_only_record (state_data,
                                                        sdr_record,
                                                        sdr_record_len,
                                                        record_id);
    default:
      /* don't output any other types in verbose mode */
      break;
    }
  
  return (0);
}
