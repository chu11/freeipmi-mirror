/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#include <freeipmi/freeipmi.h>

#include "ipmi-sensors.h"
#include "ipmi-sensors-output-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_SENSORS_FMT_BUFLEN 1024
 
int
ipmi_sensors_simple_output_setup (ipmi_sensors_state_data_t *state_data)
{
  assert (state_data);
  
  if (!state_data->prog_data->args->legacy_output
      && !state_data->prog_data->args->comma_separated_output)
    {
      if (calculate_column_widths (state_data->pstate,
                                   state_data->sdr_ctx,
                                   state_data->prog_data->args->sensor_types,
                                   state_data->prog_data->args->sensor_types_length,
                                   state_data->prog_data->args->record_ids,
                                   state_data->prog_data->args->record_ids_length,
                                   state_data->prog_data->args->non_abbreviated_units,
				   state_data->prog_data->args->shared_sensors,
                                   0, /* count_event_only_records */
                                   0, /* count_device_locator_records */
                                   0, /* count_oem_records */
				   state_data->prog_data->args->entity_sensor_names,
                                   &(state_data->column_width)) < 0)
        return (-1);
    }
  
  return (0);
}

static double
_round_double2 (double d)
{
  double r = 0.0;

  r = (d - (long) d) * 100.0;

  if ((r - (long) r) > 0.5)
    return ((long) d + (((long) r + 1) / 100.0));

  return ((long) d + ((long) r / 100.0));
}

static int
_legacy_simple_output_header (ipmi_sensors_state_data_t *state_data,
                              uint16_t record_id)
{
  char id_string[IPMI_SDR_MAX_ID_STRING_LENGTH + 1];

  assert (state_data);

  memset (id_string, '\0', IPMI_SDR_MAX_ID_STRING_LENGTH + 1);

  if (ipmi_sdr_parse_id_string (state_data->sdr_ctx,
				NULL,
				0,
                                id_string,
                                IPMI_SDR_MAX_ID_STRING_LENGTH) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_id_string: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  if (state_data->prog_data->args->quiet_readings)
    pstdout_printf (state_data->pstate,
                    "%u: %s: ",
                    record_id,
                    id_string);
  else
    {
      uint8_t sensor_type;
      uint8_t event_reading_type_code;
      const char * sensor_type_string = NULL;

      if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				      NULL,
				      0,
                                      &sensor_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_sensor_type: %s\n",
                           ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          return (-1);
        }

      if ((state_data->prog_data->args->interpret_oem_data)
          && (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
						      NULL,
						      0,
                                                      &event_reading_type_code) >= 0))
        sensor_type_string = get_oem_sensor_type_output_string (sensor_type,
                                                                event_reading_type_code,
                                                                state_data->oem_data.manufacturer_id,
                                                                state_data->oem_data.product_id);
      else 
        sensor_type_string = get_sensor_type_output_string (sensor_type);
      
      pstdout_printf (state_data->pstate,
                      "%u: %s (%s): ",
                      record_id,
                      id_string,
                      sensor_type_string);
    }

  return (0);
}

static int
_legacy_simple_output_full_record (ipmi_sensors_state_data_t *state_data,
                                   uint16_t record_id,
                                   double *sensor_reading,
                                   int event_message_output_type,
                                   uint16_t sensor_event_bitmask,
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

  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  if (_legacy_simple_output_header (state_data, record_id) < 0)
    goto cleanup;

  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  switch (ipmi_event_reading_type_code_class (event_reading_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      if (!state_data->prog_data->args->quiet_readings)
        {
          char sensor_units_buf[IPMI_SENSORS_UNITS_BUFLEN+1];
          double *lower_output_threshold = NULL;
          double *upper_output_threshold = NULL;

          memset (sensor_units_buf, '\0', IPMI_SENSORS_UNITS_BUFLEN+1);
          if (get_sensor_units_output_string (state_data->pstate,
                                              state_data->sdr_ctx,
                                              sensor_units_buf,
                                              IPMI_SENSORS_UNITS_BUFLEN,
                                              0) < 0)
            goto cleanup;

          if (ipmi_sensors_get_thresholds (state_data,
                                           &lower_non_critical_threshold,
                                           &lower_critical_threshold,
                                           &lower_non_recoverable_threshold,
                                           &upper_non_critical_threshold,
                                           &upper_critical_threshold,
                                           &upper_non_recoverable_threshold) < 0)
            goto cleanup;

          if (sensor_reading)
            pstdout_printf (state_data->pstate,
                            "%.2f %s ",
                            _round_double2 (*sensor_reading),
                            sensor_units_buf);
          else
            pstdout_printf (state_data->pstate,
                            "%s ",
                            IPMI_SENSORS_NA_STRING_LEGACY);
          
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
                            _round_double2 (*lower_output_threshold));
          else
            pstdout_printf (state_data->pstate, "(%s/", IPMI_SENSORS_NA_STRING_LEGACY);
          
          if (upper_output_threshold)
            pstdout_printf (state_data->pstate,
                            "%.2f): ",
                            _round_double2 (*upper_output_threshold));
          else
            pstdout_printf (state_data->pstate, "%s): ", IPMI_SENSORS_NA_STRING_LEGACY);
        }
      /* fall through and also output event messages */
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:
    default:
      if (ipmi_sensors_output_event_message_list (state_data,
                                                  event_message_output_type,
                                                  sensor_event_bitmask,
                                                  event_message_list,
                                                  event_message_list_len,
                                                  NULL,
                                                  0) < 0)
        goto cleanup;
      break;
    }

  rv = 0;
 cleanup:
  free (lower_non_critical_threshold);
  free (upper_non_critical_threshold);
  free (lower_critical_threshold);
  free (upper_critical_threshold);
  free (lower_non_recoverable_threshold);
  free (upper_non_recoverable_threshold);
  return (rv);
}

static int
_legacy_simple_output_compact_record (ipmi_sensors_state_data_t *state_data,
                                      uint16_t record_id,
                                      int event_message_output_type,
                                      uint16_t sensor_event_bitmask,
                                      char **event_message_list,
                                      unsigned int event_message_list_len)
{
  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  if (_legacy_simple_output_header (state_data, record_id) < 0)
    return (-1);

  if (ipmi_sensors_output_event_message_list (state_data,
                                              event_message_output_type,
                                              sensor_event_bitmask,
                                              event_message_list,
                                              event_message_list_len,
                                              NULL,
                                              0) < 0)
    return (-1);

  return (0);
}

static int
_ipmimonitoring_legacy_simple_output_header (ipmi_sensors_state_data_t *state_data,
                                             uint16_t record_id)
{
  char id_string[IPMI_SDR_MAX_ID_STRING_LENGTH + 1];
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  const char * sensor_type_string = NULL;

  assert (state_data);

  memset (id_string, '\0', IPMI_SDR_MAX_ID_STRING_LENGTH + 1);

  if (ipmi_sdr_parse_id_string (state_data->sdr_ctx,
				NULL,
				0,
                                id_string,
                                IPMI_SDR_MAX_ID_STRING_LENGTH) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_id_string: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				  NULL,
				  0,
                                  &sensor_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  if ((state_data->prog_data->args->interpret_oem_data)
      && (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
						  NULL,
						  0,
                                                  &event_reading_type_code) >= 0))
    sensor_type_string = get_oem_sensor_type_output_string (sensor_type,
                                                            event_reading_type_code,
                                                            state_data->oem_data.manufacturer_id,
                                                            state_data->oem_data.product_id);
  else 
    sensor_type_string = get_sensor_type_output_string (sensor_type);
      
  pstdout_printf (state_data->pstate,
                  "%u | %s | %s",
                  record_id,
                  id_string,
                  sensor_type_string);

  return (0);
}

static int
_ipmimonitoring_legacy_simple_output_full_record (ipmi_sensors_state_data_t *state_data,
                                                  uint16_t record_id,
                                                  double *sensor_reading,
                                                  int event_message_output_type,
                                                  uint16_t sensor_event_bitmask,
                                                  char **event_message_list,
                                                  unsigned int event_message_list_len)
{
  int rv = -1;

  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  if (_ipmimonitoring_legacy_simple_output_header (state_data, record_id) < 0)
    goto cleanup;

  if (state_data->prog_data->args->output_sensor_state)
    {
      char *sensor_state_str = NULL;
      
      if (ipmi_sensors_get_sensor_state (state_data,
                                         event_message_output_type,
                                         sensor_event_bitmask,
                                         &sensor_state_str) < 0)
        goto cleanup;
      
      pstdout_printf (state_data->pstate,
                      " | %s",
                      sensor_state_str);
    }

  if (!state_data->prog_data->args->quiet_readings)
    {
      uint8_t event_reading_type_code;

      if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
						  NULL,
						  0,
                                                  &event_reading_type_code) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_event_reading_type_code: %s\n",
                           ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          goto cleanup;
        }
      
      switch (ipmi_event_reading_type_code_class (event_reading_type_code))
        {
        case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
          {
            char sensor_units_buf[IPMI_SENSORS_UNITS_BUFLEN+1];
            
            memset (sensor_units_buf, '\0', IPMI_SENSORS_UNITS_BUFLEN+1);
            if (get_sensor_units_output_string (state_data->pstate,
                                                state_data->sdr_ctx,
                                                sensor_units_buf,
                                                IPMI_SENSORS_UNITS_BUFLEN,
                                                0) < 0)
              goto cleanup;
            
            
            if (sensor_reading)
              pstdout_printf (state_data->pstate,
                              " | %s | %f\n",
                              sensor_units_buf,
                              *sensor_reading);
            else
              pstdout_printf (state_data->pstate,
                              " | %s | %s\n",
                              sensor_units_buf,
                              IPMIMONITORING_NA_STRING_LEGACY);
          }

          break;
          
        case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
        case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:
        case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:
        default:
          /* sensor units */
          pstdout_printf (state_data->pstate,
                          " | %s | ",
                          IPMIMONITORING_NA_STRING_LEGACY);
          
          if (ipmi_sensors_output_event_message_list (state_data,
                                                      event_message_output_type,
                                                      sensor_event_bitmask,
                                                      event_message_list,
                                                      event_message_list_len,
                                                      NULL,
                                                      0) < 0)
            goto cleanup;
          
          break;
        }
    }
  else
    pstdout_printf (state_data->pstate,
                    "\n");

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmimonitoring_legacy_simple_output_compact_record (ipmi_sensors_state_data_t *state_data,
                                                     uint16_t record_id,
                                                     int event_message_output_type,
                                                     uint16_t sensor_event_bitmask,
                                                     char **event_message_list,
                                                     unsigned int event_message_list_len)
{
  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  if (_ipmimonitoring_legacy_simple_output_header (state_data, record_id) < 0)
    return (-1);

  if (state_data->prog_data->args->output_sensor_state)
    {
      char *sensor_state_str = NULL;
      
      if (ipmi_sensors_get_sensor_state (state_data,
					 event_message_output_type,
					 sensor_event_bitmask,
					 &sensor_state_str) < 0)
	return (-1);
      
      pstdout_printf (state_data->pstate,
		      " | %s",
		      sensor_state_str);
    }
  
  if (state_data->prog_data->args->quiet_readings)
    return (0);

  pstdout_printf (state_data->pstate,
		  " | %s | ",
		  IPMIMONITORING_NA_STRING_LEGACY);

  if (ipmi_sensors_output_event_message_list (state_data,
                                              event_message_output_type,
                                              sensor_event_bitmask,
                                              event_message_list,
                                              event_message_list_len,
                                              NULL,
                                              0) < 0)
    return (-1);

  return (0);
}

static int
_simple_output_header (ipmi_sensors_state_data_t *state_data,
                       uint16_t record_id,
                       uint8_t sensor_number,
                       int event_message_output_type,
                       uint16_t sensor_event_bitmask)
{
  char fmt[IPMI_SENSORS_FMT_BUFLEN + 1];
  char sensor_name[IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1];
  unsigned int sensor_name_flags = 0;
  const char *sensor_type_string;
  uint8_t event_reading_type_code;

  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  memset (sensor_name, '\0', IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1);
      
  if (!state_data->prog_data->args->shared_sensors)
    sensor_name_flags |= IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS;

  if (state_data->prog_data->args->entity_sensor_names)
    {
      if (ipmi_sdr_parse_entity_sensor_name (state_data->sdr_ctx,
					     NULL,
					     0,
					     sensor_number,
					     sensor_name_flags,
					     sensor_name,
					     IPMI_SDR_MAX_SENSOR_NAME_LENGTH) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_entity_sensor_name: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  return (-1);
	}
    }
  else
    {
      if (ipmi_sdr_parse_sensor_name (state_data->sdr_ctx,
				      NULL,
				      0,
				      sensor_number,
				      sensor_name_flags,
				      sensor_name,
				      IPMI_SDR_MAX_SENSOR_NAME_LENGTH) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_sensor_name: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  return (-1);
	}
    }
  
  memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);
  if (state_data->prog_data->args->no_sensor_type_output)
    {
      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%u,%%s");
      else
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%-%du | %%-%ds",
                  state_data->column_width.record_id,
                  state_data->column_width.sensor_name);
      
      pstdout_printf (state_data->pstate,
                      fmt,
                      record_id,
                      sensor_name);
    }
  else
    {     
      uint8_t sensor_type;

      if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				      NULL,
				      0,
                                      &sensor_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_sensor_type: %s\n",
                           ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          return (-1);
        }
      
      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%u,%%s,%%s");
      else
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%-%du | %%-%ds | %%-%ds",
                  state_data->column_width.record_id,
                  state_data->column_width.sensor_name,
                  state_data->column_width.sensor_type);
      
      if ((state_data->prog_data->args->interpret_oem_data)
          && (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
						      NULL,
						      0,
                                                      &event_reading_type_code) >= 0))
        sensor_type_string = get_oem_sensor_type_output_string (sensor_type,
                                                                event_reading_type_code,
                                                                state_data->oem_data.manufacturer_id,
                                                                state_data->oem_data.product_id);
      else 
        sensor_type_string = get_sensor_type_output_string (sensor_type);
      
      pstdout_printf (state_data->pstate,
                      fmt,
                      record_id,
                      sensor_name,
                      sensor_type_string);
    }

  if (state_data->prog_data->args->output_sensor_state)
    {
      char *sensor_state_str = NULL;

      if (ipmi_sensors_get_sensor_state (state_data,
                                         event_message_output_type,
                                         sensor_event_bitmask,
                                         &sensor_state_str) < 0)
        return (-1);

      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  ",%%s");
      else
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  " | %%-8s");

      pstdout_printf (state_data->pstate,
                      fmt,
                      sensor_state_str);
    }
      
  return (0);
}

static int
_simple_output_full_record (ipmi_sensors_state_data_t *state_data,
                            uint16_t record_id,
                            uint8_t sensor_number,
                            double *sensor_reading,
                            int event_message_output_type,
                            uint16_t sensor_event_bitmask,
                            char **event_message_list,
                            unsigned int event_message_list_len)
{
  char fmt[IPMI_SENSORS_FMT_BUFLEN + 1];
  uint8_t event_reading_type_code;
  double *lower_non_critical_threshold = NULL;
  double *upper_non_critical_threshold = NULL;
  double *lower_critical_threshold = NULL;
  double *upper_critical_threshold = NULL;
  double *lower_non_recoverable_threshold = NULL;
  double *upper_non_recoverable_threshold = NULL;
  int rv = -1;

  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  if (_simple_output_header (state_data,
                             record_id,
                             sensor_number,
                             event_message_output_type,
                             sensor_event_bitmask) < 0)
    goto cleanup;

  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  switch (ipmi_event_reading_type_code_class (event_reading_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      if (!state_data->prog_data->args->quiet_readings)
        {
          char sensor_units_buf[IPMI_SENSORS_UNITS_BUFLEN+1];

          memset (sensor_units_buf, '\0', IPMI_SENSORS_UNITS_BUFLEN+1);
          if (get_sensor_units_output_string (state_data->pstate,
                                              state_data->sdr_ctx,
                                              sensor_units_buf,
                                              IPMI_SENSORS_UNITS_BUFLEN,
                                              state_data->prog_data->args->non_abbreviated_units) < 0)
            goto cleanup;

          memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);

          if (sensor_reading)
            {
              if (state_data->prog_data->args->comma_separated_output)
                snprintf (fmt,
                          IPMI_SENSORS_FMT_BUFLEN,
                          ",%%.2f,%%s");
              else
                snprintf (fmt,
                          IPMI_SENSORS_FMT_BUFLEN,
                          " | %%-10.2f | %%-%ds",
                          state_data->column_width.sensor_units);
              
              pstdout_printf (state_data->pstate,
                              fmt,
                              _round_double2 (*sensor_reading),
                              sensor_units_buf);
            }
          else
            {
              if (state_data->prog_data->args->comma_separated_output)
                snprintf (fmt,
                          IPMI_SENSORS_FMT_BUFLEN,
                          ",%%s,%%s");
              else
                snprintf (fmt,
                          IPMI_SENSORS_FMT_BUFLEN,
                          " | %%-10s | %%-%ds",
                          state_data->column_width.sensor_units);
              
              pstdout_printf (state_data->pstate,
                              fmt,
                              IPMI_SENSORS_NA_STRING,
                              sensor_units_buf);
            }
        }

      if (state_data->prog_data->args->output_sensor_thresholds)
	{
	  char thresholdfmt[IPMI_SENSORS_FMT_BUFLEN + 1];
	  char nafmt[IPMI_SENSORS_FMT_BUFLEN + 1];

          if (ipmi_sensors_get_thresholds (state_data,
                                           &lower_non_critical_threshold,
                                           &lower_critical_threshold,
                                           &lower_non_recoverable_threshold,
                                           &upper_non_critical_threshold,
                                           &upper_critical_threshold,
                                           &upper_non_recoverable_threshold) < 0)
            goto cleanup;

          memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);

	  if (state_data->prog_data->args->comma_separated_output)
	    {
	      snprintf (thresholdfmt,
			IPMI_SENSORS_FMT_BUFLEN,
			",%%.2f");

	      snprintf (nafmt,
			IPMI_SENSORS_FMT_BUFLEN,
			",%%s");
	    }
	  else
	    {
	      snprintf (thresholdfmt,
			IPMI_SENSORS_FMT_BUFLEN,
			" | %%-10.2f");

	      snprintf (nafmt,
			IPMI_SENSORS_FMT_BUFLEN,
			" | %%-10s");
	    }

	  if (lower_non_recoverable_threshold)
	    pstdout_printf (state_data->pstate,
			    thresholdfmt,
			    *lower_non_recoverable_threshold);
	  else
	    pstdout_printf (state_data->pstate,
			    nafmt,
			    IPMI_SENSORS_NA_STRING);

	  if (lower_critical_threshold)
	    pstdout_printf (state_data->pstate,
			    thresholdfmt,
			    *lower_critical_threshold);
	  else
	    pstdout_printf (state_data->pstate,
			    nafmt,
			    IPMI_SENSORS_NA_STRING);

	  if (lower_non_critical_threshold)
	    pstdout_printf (state_data->pstate,
			    thresholdfmt,
			    *lower_non_critical_threshold);
	  else
	    pstdout_printf (state_data->pstate,
			    nafmt,
			    IPMI_SENSORS_NA_STRING);
			  
	  if (upper_non_critical_threshold)
	    pstdout_printf (state_data->pstate,
			    thresholdfmt,
			    *upper_non_critical_threshold);
	  else
	    pstdout_printf (state_data->pstate,
			    nafmt,
			    IPMI_SENSORS_NA_STRING);

	  if (upper_critical_threshold)
	    pstdout_printf (state_data->pstate,
			    thresholdfmt,
			    *upper_critical_threshold);
	  else
	    pstdout_printf (state_data->pstate,
			    nafmt,
			    IPMI_SENSORS_NA_STRING);

	  if (upper_non_recoverable_threshold)
	    pstdout_printf (state_data->pstate,
			    thresholdfmt,
			    *upper_non_recoverable_threshold);
	  else
	    pstdout_printf (state_data->pstate,
			    nafmt,
			    IPMI_SENSORS_NA_STRING);
	}

      if (state_data->prog_data->args->comma_separated_output)
        pstdout_printf (state_data->pstate, ",");
      else
        pstdout_printf (state_data->pstate, " | ");

      if (ipmi_sensors_output_event_message_list (state_data,
                                                  event_message_output_type,
                                                  sensor_event_bitmask,
                                                  event_message_list,
                                                  event_message_list_len,
                                                  NULL,
                                                  0) < 0)
        goto cleanup;

      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:
    default:
      if (!state_data->prog_data->args->quiet_readings)
        {
	  if (state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_DISCRETE_READING
	      && sensor_reading)
	    {
	      char sensor_units_buf[IPMI_SENSORS_UNITS_BUFLEN+1];

	      memset (sensor_units_buf, '\0', IPMI_SENSORS_UNITS_BUFLEN+1);
	      if (get_sensor_units_output_string (state_data->pstate,
						  state_data->sdr_ctx,
						  sensor_units_buf,
						  IPMI_SENSORS_UNITS_BUFLEN,
						  state_data->prog_data->args->non_abbreviated_units) < 0)
		goto cleanup;
	      
	      memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);

	      if (state_data->prog_data->args->comma_separated_output)
		snprintf (fmt,
			  IPMI_SENSORS_FMT_BUFLEN,
			  ",%%.2f,%%s");
	      else
		snprintf (fmt,
			  IPMI_SENSORS_FMT_BUFLEN,
			  " | %%-10.2f | %%-%ds",
			  state_data->column_width.sensor_units);
		  
	      pstdout_printf (state_data->pstate,
			      fmt,
			      _round_double2 (*sensor_reading),
			      sensor_units_buf);
	    }
	  else
	    {
	      memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);

	      if (state_data->prog_data->args->comma_separated_output)
		snprintf (fmt,
			  IPMI_SENSORS_FMT_BUFLEN,
			  ",%%s,%%s");
	      else
		snprintf (fmt,
			  IPMI_SENSORS_FMT_BUFLEN,
			  " | %%-10s | %%-%ds",
			  state_data->column_width.sensor_units);
	      
	      pstdout_printf (state_data->pstate,
			      fmt,
			      IPMI_SENSORS_NA_STRING,
			      IPMI_SENSORS_NA_STRING);
	    }
        }

      if (state_data->prog_data->args->output_sensor_thresholds)
	{
	  if (state_data->prog_data->args->comma_separated_output)
	    pstdout_printf (state_data->pstate,
			    ",%s,%s,%s,%s,%s,%s",
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING);
	  else
	    pstdout_printf (state_data->pstate,
			    " | %-10s | %-10s | %-10s | %-10s | %-10s | %-10s",
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING,
			    IPMI_SENSORS_NA_STRING);
	}
 
      if (state_data->prog_data->args->comma_separated_output)
	pstdout_printf (state_data->pstate, ",");
      else
	pstdout_printf (state_data->pstate, " | ");
      
      if (ipmi_sensors_output_event_message_list (state_data,
                                                  event_message_output_type,
                                                  sensor_event_bitmask,
                                                  event_message_list,
                                                  event_message_list_len,
                                                  NULL,
                                                  0) < 0)
        goto cleanup;
      break;
    }

  rv = 0;
 cleanup:
  free (lower_non_critical_threshold);
  free (upper_non_critical_threshold);
  free (lower_critical_threshold);
  free (upper_critical_threshold);
  free (lower_non_recoverable_threshold);
  free (upper_non_recoverable_threshold);
  return (rv);
}

static int
_simple_output_compact_record (ipmi_sensors_state_data_t *state_data,
                               uint16_t record_id,
                               uint8_t sensor_number,
                               int event_message_output_type,
                               uint16_t sensor_event_bitmask,                              
                               char **event_message_list,
                               unsigned int event_message_list_len)
{
  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  if (_simple_output_header (state_data,
                             record_id,
                             sensor_number,
                             event_message_output_type,
                             sensor_event_bitmask) < 0)
    return (-1);

  if (!state_data->prog_data->args->quiet_readings)
    {
      char fmt[IPMI_SENSORS_FMT_BUFLEN + 1];
  
      memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);

      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  ",%%s,%%s");
      else
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  " | %%-10s | %%-%ds",
                  state_data->column_width.sensor_units);

      pstdout_printf (state_data->pstate,
                      fmt,
                      IPMI_SENSORS_NA_STRING,
                      IPMI_SENSORS_NA_STRING);
    }

  if (state_data->prog_data->args->output_sensor_thresholds)
    {
      if (state_data->prog_data->args->comma_separated_output)
	pstdout_printf (state_data->pstate,
			",%s,%s,%s,%s,%s,%s",
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING);
      else
	pstdout_printf (state_data->pstate,
			" | %-10s | %-10s | %-10s | %-10s | %-10s | %-10s",
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING,
			IPMI_SENSORS_NA_STRING);
    }
 
  if (state_data->prog_data->args->comma_separated_output)
    pstdout_printf (state_data->pstate, ",");
  else
    pstdout_printf (state_data->pstate, " | ");

  if (ipmi_sensors_output_event_message_list (state_data,
                                              event_message_output_type,
                                              sensor_event_bitmask,
                                              event_message_list,
                                              event_message_list_len,
                                              NULL,
                                              0) < 0)
    return (-1);

  return (0);
}

static void
_output_headers (ipmi_sensors_state_data_t *state_data)
{
  char fmt[IPMI_SENSORS_FMT_BUFLEN + 1];

  assert (state_data);
  assert (!state_data->output_headers);

  if (state_data->prog_data->args->ipmimonitoring_legacy_output)
    {
      pstdout_printf (state_data->pstate,
                      "Record ID | Sensor Name | Sensor Group");
      
      if (state_data->prog_data->args->output_sensor_state)
        pstdout_printf (state_data->pstate,
                        " | Monitoring Status");
      
      if (!state_data->prog_data->args->quiet_readings)
        pstdout_printf (state_data->pstate,
                        " | Sensor Units | Sensor Reading");
      
      pstdout_printf (state_data->pstate,
                      "\n");
      
      return;
    }

  if (state_data->prog_data->args->legacy_output)
    return;

  if (state_data->prog_data->args->no_header_output)
    return;

  memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);
  if (state_data->prog_data->args->no_sensor_type_output)
    {
      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%s,%%s");
      else
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%-%ds | %%-%ds",
                  state_data->column_width.record_id,
                  state_data->column_width.sensor_name);
      
      pstdout_printf (state_data->pstate,
                      fmt,
                      SENSORS_HEADER_RECORD_ID_STR,
                      SENSORS_HEADER_NAME_STR);
    }
  else
    {
      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%s,%%s,%%s");
      else
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  "%%-%ds | %%-%ds | %%-%ds",
                  state_data->column_width.record_id,
                  state_data->column_width.sensor_name,
                  state_data->column_width.sensor_type);
      
      pstdout_printf (state_data->pstate,
                      fmt,
                      SENSORS_HEADER_RECORD_ID_STR,
                      SENSORS_HEADER_NAME_STR,
                      SENSORS_HEADER_TYPE_STR);
    }

  if (state_data->prog_data->args->output_sensor_state)
    {
      if (state_data->prog_data->args->comma_separated_output)
        pstdout_printf (state_data->pstate,
                        ",%s",
                        SENSORS_HEADER_STATE_STR);
      else
        pstdout_printf (state_data->pstate,
                        " | %s   ",
                        SENSORS_HEADER_STATE_STR);
    }
  
  if (!state_data->prog_data->args->quiet_readings)
    {
      if (state_data->prog_data->args->comma_separated_output)
        pstdout_printf (state_data->pstate,
                        ",%s",
                        SENSORS_HEADER_READING_STR);
      else
        pstdout_printf (state_data->pstate,
                        " | %s   ",
                        SENSORS_HEADER_READING_STR);
      
      memset (fmt, '\0', IPMI_SENSORS_FMT_BUFLEN + 1);
      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  ",%%s");
      else
        snprintf (fmt,
                  IPMI_SENSORS_FMT_BUFLEN,
                  " | %%-%ds",
                  state_data->column_width.sensor_units);
      
      pstdout_printf (state_data->pstate,
                      fmt,
                      SENSORS_HEADER_UNITS_STR);
    }

  if (state_data->prog_data->args->output_sensor_thresholds)
    {
      if (state_data->prog_data->args->comma_separated_output)
        pstdout_printf (state_data->pstate,
			",Lower NR,Lower C,Lower NC,Upper NC,Upper C,Upper NR");
      else
        pstdout_printf (state_data->pstate,
			" | Lower NR   | Lower C    | Lower NC   | Upper NC   | Upper C    | Upper NR  ");
    }

  if (state_data->prog_data->args->comma_separated_output)
    pstdout_printf (state_data->pstate,
                    ",%s\n",
                    SENSORS_HEADER_EVENT_STR);
  else
    pstdout_printf (state_data->pstate,
                    " | %s\n",
                    SENSORS_HEADER_EVENT_STR);
}

int
ipmi_sensors_simple_output (ipmi_sensors_state_data_t *state_data,
                            uint8_t sensor_number,
                            double *sensor_reading,
                            int event_message_output_type,
                            uint16_t sensor_event_bitmask,
                            char **event_message_list,
                            unsigned int event_message_list_len)
{
  uint16_t record_id;
  uint8_t record_type;

  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

  if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
					 NULL,
					 0,
                                         &record_id,
                                         &record_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  if (!state_data->output_headers)
    {
      _output_headers (state_data);
      state_data->output_headers++;
    }

  switch (record_type)
    {
    case IPMI_SDR_FORMAT_FULL_SENSOR_RECORD:
      if (state_data->prog_data->args->legacy_output)
        return (_legacy_simple_output_full_record (state_data,
                                                   record_id,
                                                   sensor_reading,
                                                   event_message_output_type,
                                                   sensor_event_bitmask,
                                                   event_message_list,
                                                   event_message_list_len));
      else if (state_data->prog_data->args->ipmimonitoring_legacy_output)
        return (_ipmimonitoring_legacy_simple_output_full_record (state_data,
                                                                  record_id,
                                                                  sensor_reading,
                                                                  event_message_output_type,
                                                                  sensor_event_bitmask,
                                                                  event_message_list,
                                                                  event_message_list_len));
      else
        return (_simple_output_full_record (state_data,
                                            record_id,
                                            sensor_number,
                                            sensor_reading,
                                            event_message_output_type,
                                            sensor_event_bitmask,
                                            event_message_list,
                                            event_message_list_len));
    case IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD:
      if (state_data->prog_data->args->legacy_output)
        return (_legacy_simple_output_compact_record (state_data,
                                                      record_id,
                                                      event_message_output_type,
                                                      sensor_event_bitmask,
                                                      event_message_list,
                                                      event_message_list_len));
      else if (state_data->prog_data->args->ipmimonitoring_legacy_output)
        return (_ipmimonitoring_legacy_simple_output_compact_record (state_data,
                                                                     record_id,
                                                                     event_message_output_type,
                                                                     sensor_event_bitmask,
                                                                     event_message_list,
                                                                     event_message_list_len));
      else
        return (_simple_output_compact_record (state_data,
                                               record_id,
                                               sensor_number,
                                               event_message_output_type,
                                               sensor_event_bitmask,
                                               event_message_list,
                                               event_message_list_len));
    default:
      /* don't output any other types in simple mode */
      break;
    }

  return (0);
}
