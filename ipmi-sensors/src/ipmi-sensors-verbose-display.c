/*
  Copyright (C) 2006 FreeIPMI Core Team

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

#include <stdio.h>

#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"

#include "ipmi-sensors.h"
#include "ipmi-sensors-util.h"

#include "pstdout.h"
#include "tool-sensor-common.h"

static int
_output_header (ipmi_sensors_state_data_t *state_data,
                uint8_t *sdr_record,
                unsigned int sdr_record_len,
                uint8_t record_id)
{
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;

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

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Sensor ID String: %s\n", 
                  id_string);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n",
                  sensor_group (sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  event_reading_type_code);

  return 0;
}

static int
_output_event_message_list (ipmi_sensors_state_data_t *state_data,
                            char **event_message_list,
                            unsigned int event_message_list_len)
{
  assert (state_data);

  pstdout_printf (state_data->pstate, 
                  "Sensor Status: ");
  
  if (ipmi_sensors_output_event_message_list (state_data,
                                              event_message_list,
                                              event_message_list_len) < 0)
    return -1;
  
  /* Extra \n in verbose output */
  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

static int 
sensors_display_verbose_full_record (ipmi_sensors_state_data_t *state_data,
                                     uint8_t *sdr_record,
                                     unsigned int sdr_record_len,
                                     uint8_t record_id,
                                     double *reading,
                                     char **event_message_list,
                                     unsigned int event_message_list_len)
{
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_header (state_data,
                      sdr_record,
                      sdr_record_len,
                      record_id) < 0)
    goto cleanup;

  if (record->readable_threshold_lower_critical_threshold)
    {
      pstdout_printf (state_data->pstate,
                      "Lower Critical Threshold: %f %s\n", 
                      record->lower_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_upper_critical_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Critical Threshold: %f %s\n", 
                      record->upper_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_lower_non_critical_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Critical Threshold: %f %s\n", 
                      record->lower_non_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_upper_non_critical_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Critical Threshold: %f %s\n", 
                      record->upper_non_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_lower_non_recoverable_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Recoverable Threshold: %f %s\n", 
                      record->lower_non_recoverable_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Recoverable Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_upper_non_recoverable_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Recoverable Threshold: %f %s\n", 
                      record->upper_non_recoverable_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Recoverable Threshold: %s\n", 
                      "NA");
    }
  pstdout_printf (state_data->pstate, 
                  "Sensor Min. Reading: %f %s\n", 
                  record->sensor_minimum_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Sensor Max. Reading: %f %s\n", 
                  record->sensor_maximum_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Normal Min.: %f %s\n", 
                  record->normal_minimum, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Normal Max.: %f %s\n", 
                  record->normal_maximum, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Nominal reading: %f %s\n", 
                  record->nominal_reading, 
                  ipmi_sensor_units[record->sensor_unit]);

  if (reading)
    pstdout_printf (state_data->pstate, 
                    "Sensor Reading: %f %s\n", 
                    *reading, 
                    ipmi_sensor_units[sensor_unit]);
  else
    pstdout_printf (state_data->pstate,
                    "Sensor Reading: %s\n",
                    "NA");
  
  if (_output_event_message_list (state_data,
                                  event_message_list,
                                  event_message_list_len) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return rv;
}

static int 
sensors_display_verbose_compact_record (ipmi_sensors_state_data_t *state_data,
                                        uint8_t *sdr_record,
                                        unsigned int sdr_record_len,
                                        uint8_t record_id,
                                        double *reading,
                                        char **event_message_list,
                                        unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_header (state_data,
                      sdr_record,
                      sdr_record_len,
                      record_id) < 0)
    return -1;

  if (_output_event_message_list (state_data,
                                  event_message_list,
                                  event_message_list_len) < 0)
    return -1;
  
  return 0;
}

static int 
sensors_display_verbose_event_only_record (ipmi_sensors_state_data_t *state_data,
                                           uint8_t *sdr_record,
                                           unsigned int sdr_record_len,
                                           uint8_t record_id,
                                           double *reading,
                                           char **event_message_list,
                                           unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_header (state_data,
                      sdr_record,
                      sdr_record_len,
                      record_id) < 0)
    return -1;
  
  if (_output_event_message_list (state_data,
                                  event_message_list,
                                  event_message_list_len) < 0)
    return -1;

  return 0;
}

int
ipmi_sensors_display_verbose (ipmi_sensors_state_data_t *state_data,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len,
                              double *reading,
                              char **event_message_list,
                              unsigned int event_message_list_len);
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
                                                     reading,
                                                     event_message_list,
                                                     event_message_list_len);
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return sensors_display_verbose_event_only_record (state_data,
                                                        sdr_record,
                                                        sdr_record_len,
                                                        record_id,
                                                        reading,
                                                        event_message_list,
                                                        event_message_list_len);
    default:
      /* don't output any other types in verbose mode */
      break;
    }
  
  return (0);
}
