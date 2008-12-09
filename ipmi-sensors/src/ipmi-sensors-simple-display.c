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

static double
round_double2 (double d)
{
  double r = 0.0;

  r = (d - (long) d) * 100.0;

  if ((r - (long) r) > 0.5)
    return ((long) d + (((long) r + 1) / 100.0));

  return ((long) d + ((long) r / 100.0));
}

static int
_output_simple_header (ipmi_sensors_state_data_t *state_data,
                       uint8_t *sdr_record,
                       unsigned int sdr_record_len,
                       uint16_t record_id)
{
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);
  
  if (sdr_cache_get_id_string (state_data->pstate,
                               sdr_record,
                               sdr_record_len,
                               id_string,
                               IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
    return -1;

  if (state_data->prog_data->args->quiet_readings)
    pstdout_printf (state_data->pstate,
                    "%d: %s: ", 
                    record_id, 
                    id_string);
  else
    {
      uint8_t sensor_type;

      if (sdr_cache_get_sensor_type (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &sensor_type) < 0)
        return -1;

      pstdout_printf (state_data->pstate,
                      "%d: %s (%s): ", 
                      record_id, 
                      id_string,
                      sensor_group (sensor_type));
    }

  return 0;
}

static int 
sensors_display_simple_full_record (ipmi_sensors_state_data_t *state_data,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint16_t record_id,
                                    double *reading,
                                    char **event_message_list,
                                    unsigned int event_message_list_len)
{
  uint8_t event_reading_type_code;
  double *lower_non_critical_threshold = NULL;
  double *upper_non_critical_threshold = NULL;
  double *lower_critical_threshold = NULL;
  double *upper_critical_threshold = NULL;
  double *lower_non_recoverable_threshold = NULL;
  double *upper_non_recoverable_threshold = NULL;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_simple_header (state_data,
                             sdr_record,
                             sdr_record_len,
                             record_id) < 0)
    goto cleanup;

  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup; 
 
  switch (sensor_classify (event_reading_type_code))
    {
    case SENSOR_CLASS_THRESHOLD:
      if (!state_data->prog_data->args->quiet_readings)
        {             
          uint8_t sensor_unit;
          double *lower_output_threshold = NULL;
          double *upper_output_threshold = NULL;

          if (sdr_cache_get_sensor_unit (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         &sensor_unit) < 0)
            goto cleanup;

          if (ipmi_sensors_get_thresholds (state_data,
                                           sdr_record,
                                           sdr_record_len,
                                           &lower_non_critical_threshold,
                                           &lower_critical_threshold,
                                           &lower_non_recoverable_threshold,
                                           &upper_non_critical_threshold,
                                           &upper_critical_threshold,
                                           &upper_non_recoverable_threshold) < 0)
            goto cleanup;

          if (reading)
            pstdout_printf (state_data->pstate,
                            "%.2f %s ", 
                            round_double2 (*reading), 
                            ipmi_sensor_units_abbreviated[sensor_unit]);
          else 
            pstdout_printf (state_data->pstate, "NA ");
          
          /* default output is critical thresholds, if those aren't
           * available, move to non-recoverable, and if those aren't
           * available, move on to non-critical.
           */

          if (lower_critical_threshold || upper_critical_threshold)
            {
              lower_output_threshold = lower_critical_threshold;
              upper_output_threshold = upper_critical_threshold;
            }
          else if (lower_non_recoverable_threshold || upper_non_recoverable_threshold)
            {
              lower_output_threshold = lower_non_recoverable_threshold;
              upper_output_threshold = upper_non_recoverable_threshold;
            }
          else if (lower_non_critical_threshold || upper_non_critical_threshold)
            {
              lower_output_threshold = lower_non_critical_threshold;
              upper_output_threshold = upper_non_critical_threshold;
            }

          if (lower_output_threshold)
            pstdout_printf (state_data->pstate,
                            "(%.2f/", 
                            round_double2 (*lower_output_threshold));
          else 
            pstdout_printf (state_data->pstate, "(NA/");

          if (upper_output_threshold)
            pstdout_printf (state_data->pstate,
                            "%.2f): ", 
                            round_double2 (*upper_output_threshold));
          else 
            pstdout_printf (state_data->pstate, "NA): ");
        }
      /* fall through and also output event messages */
    case SENSOR_CLASS_GENERIC_DISCRETE:
    case SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
    case SENSOR_CLASS_OEM:
    default:
      if (ipmi_sensors_output_event_message_list(state_data,
                                                 event_message_list,
                                                 event_message_list_len,
                                                 NULL,
                                                 0) < 0)
        goto cleanup;
      break;
    }

  rv = 0;
 cleanup:
  if (lower_non_critical_threshold)
    free(lower_non_critical_threshold);
  if (upper_non_critical_threshold)
    free(upper_non_critical_threshold);
  if (lower_critical_threshold)
    free(lower_critical_threshold);
  if (upper_critical_threshold)
    free(upper_critical_threshold);
  if (lower_non_recoverable_threshold)
    free(lower_non_recoverable_threshold);
  if (upper_non_recoverable_threshold)
    free(upper_non_recoverable_threshold);
  return rv;
}

static int 
sensors_display_simple_compact_record (ipmi_sensors_state_data_t *state_data,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint16_t record_id,
                                       char **event_message_list,
                                       unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_simple_header (state_data,
                             sdr_record,
                             sdr_record_len,
                             record_id) < 0)
    return -1;

  if (ipmi_sensors_output_event_message_list(state_data,
                                             event_message_list,
                                             event_message_list_len,
                                             NULL,
                                             0) < 0)
    return -1;
  
  return 0;
}

int 
ipmi_sensors_display_simple (ipmi_sensors_state_data_t *state_data,
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
      return sensors_display_simple_full_record (state_data,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 record_id, 
                                                 reading,
                                                 event_message_list,
                                                 event_message_list_len);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_simple_compact_record (state_data,
                                                    sdr_record,
                                                    sdr_record_len,
                                                    record_id, 
                                                    event_message_list,
                                                    event_message_list_len);
    default:
      /* don't output any other types in simple mode */
      break;
    }
  
  return (0);
}
