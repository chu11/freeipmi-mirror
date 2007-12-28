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
_output_header (ipmi_sensors_state_data_t *state_data,
                uint8_t *sdr_record,
                unsigned int sdr_record_len,
                uint8_t record_id)
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

  if (state_data->prog_data->args->quiet_readings_wanted)
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
                                    uint8_t record_id,
                                    double *reading,
                                    char **event_message_list,
                                    unsigned int event_message_list_len)
{
  uint8_t event_reading_type_code;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_header (state_data,
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
      if (!state_data->prog_data->args->quiet_readings_wanted)
        {             
          int8_t r_exponent, b_exponent;
          int16_t m, b;
          uint8_t linearization, analog_data_format;
          uint8_t sensor_number;
          uint8_t sensor_unit;
          uint64_t readable_threshold_lower_critical_threshold;
          uint64_t readable_threshold_upper_critical_threshold;
          double threshold;
          uint64_t val;

          if (sdr_cache_get_sensor_number (state_data->pstate,
                                           sdr_record,
                                           sdr_record_len,
                                           &sensor_number) < 0)
            goto cleanup;

          if (sdr_cache_get_sensor_unit (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         &sensor_unit) < 0)
            goto cleanup;

          if (sdr_cache_get_sensor_decoding_data(state_data->pstate,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 &r_exponent,
                                                 &b_exponent,
                                                 &m,
                                                 &b,
                                                 &linearization,
                                                 &analog_data_format) < 0)
            goto cleanup;

          /* achu: 
           *
           * I will admit I'm not entirely sure what the best way is
           * to get thresholds.  It seems the information is
           * stored/retrievable in multiple places.
           *
           * Since the readable_threshold_mask in the SDR record
           * indicates the mask is for the "Get Sensor Thresholds"
           * command, the best way is to get the values via that
           * command.  Sounds good to me.
           */

          _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sensor_thresholds_rs);

          if (ipmi_cmd_get_sensor_thresholds (state_data->ipmi_ctx,
                                              sensor_number,
                                              obj_cmd_rs) < 0)
            {
              pstdout_fprintf(state_data->pstate,
                              stderr,
                              "ipmi_cmd_get_sensor_thresholds: %s\n",
                              ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
              goto cleanup;
            }

          _FIID_OBJ_GET (obj_cmd_rs,
                         "readable_thresholds.lower_critical_threshold",
                         &readable_threshold_lower_critical_threshold);
          
          _FIID_OBJ_GET (obj_cmd_rs,
                         "readable_thresholds.upper_critical_threshold",
                         &readable_threshold_upper_critical_threshold);

          if (reading)
            pstdout_printf (state_data->pstate,
                            "%.2f %s ", 
                            round_double2 (*reading), 
                            ipmi_sensor_units_abbreviated[sensor_unit]);
          else 
            pstdout_printf (state_data->pstate, "NA");
          
          if (readable_threshold_lower_critical_threshold)
            {
              _FIID_OBJ_GET(obj_cmd_rs,
                            "lower_critical_threshold",
                            &val);

              if (ipmi_sensor_decode_value (r_exponent,
                                            b_exponent,
                                            m,
                                            b,
                                            linearization,
                                            analog_data_format,
                                            val,
                                            &threshold) < 0)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "ipmi_sensor_decode_value: %s\n",
                                   strerror(errno));
                  goto cleanup;
                }

              pstdout_printf (state_data->pstate,
                              "(%.2f/", 
                              round_double2 (threshold));
            }
          else 
            pstdout_printf (state_data->pstate, "(NA/");

          if (readable_threshold_upper_critical_threshold)
            {
              _FIID_OBJ_GET(obj_cmd_rs,
                            "upper_critical_threshold",
                            &val);

              if (ipmi_sensor_decode_value (r_exponent,
                                            b_exponent,
                                            m,
                                            b,
                                            linearization,
                                            analog_data_format,
                                            val,
                                            &threshold) < 0)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "ipmi_sensor_decode_value: %s\n",
                                   strerror(errno));
                  goto cleanup;
                }

              pstdout_printf (state_data->pstate,
                              "%.2f): ", 
                              round_double2 (threshold));
            }
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
                                                 event_message_list_len) < 0)
        goto cleanup;
      break;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

static int 
sensors_display_simple_compact_record (ipmi_sensors_state_data_t *state_data,
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
    goto cleanup;

  if (ipmi_sensors_output_event_message_list(state_data,
                                             event_message_list,
                                             event_message_list_len) < 0)
    goto cleanup;
  
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
                                                    reading,
                                                    event_message_list,
                                                    event_message_list_len);
    default:
      /* don't output any other types in simple mode */
      break;
    }
  
  return (0);
}
