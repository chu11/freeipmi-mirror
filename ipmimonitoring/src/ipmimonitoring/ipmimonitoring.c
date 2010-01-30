/*****************************************************************************\
 *  $Id: ipmimonitoring.c,v 1.143 2010-01-30 01:13:46 chu11 Exp $
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
 *  Free Software Foundation; either version 2 of the License, or (at your
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <sys/resource.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/param.h>
#include <assert.h>
#include <errno.h>

#include "ipmi_monitoring.h"    /* lib .h file */

#include "ipmimonitoring.h"     /* tool .h file */
#include "ipmimonitoring-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "secure.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMIMONITORING_BUFLEN                 1024

#define IPMIMONITORING_NA_STRING              "N/A"

#define IPMIMONITORING_NO_EVENT_STRING        "OK"

#define IPMIMONITORING_UNRECOGNIZED_STATE     "Unrecognized State"

#define IPMIMONITORING_FMT_BUFLEN             1024

static int
_list_sensor_types (ipmimonitoring_state_data_t *state_data)
{
  assert (state_data);

  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_TEMPERATURE) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_VOLTAGE) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_CURRENT) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_FAN) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_PHYSICAL_SECURITY) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_PROCESSOR) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_POWER_SUPPLY) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_POWER_UNIT) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_MEMORY) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_DRIVE_SLOT) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_SYSTEM_EVENT) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_MODULE_BOARD) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_SLOT_CONNECTOR) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_WATCHDOG2) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_ENTITY_PRESENCE) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_BATTERY) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_FRU_STATE) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_CABLE_INTERCONNECT) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_BOOT_ERROR) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_BUTTON_SWITCH) < 0)
    return (-1);
  if (display_sensor_type_cmdline (state_data->pstate, IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE) < 0)
    return (-1);

  return (0);
}

static int
_flush_cache (ipmimonitoring_state_data_t *state_data)
{
  assert (state_data);

  if (sdr_cache_flush_cache (state_data->sdr_cache_ctx,
                             state_data->pstate,
                             state_data->prog_data->args->sdr.quiet_cache,
                             state_data->hostname,
                             state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return (-1);

  return (0);
}

static int
_setup_ipmimonitoring_lib (struct ipmimonitoring_arguments *args)
{
  int errnum;

  assert (args);

  if (ipmi_monitoring_init (args->ipmimonitoring_flags, &errnum) < 0)
    {
      fprintf (stderr,
               "ipmi_monitoring_init: %s\n",
               ipmi_monitoring_ctx_strerror (errnum));
      return (-1);
    }

  return (0);
}

static const char *
_get_sensor_type_string (ipmimonitoring_state_data_t *state_data, int sensor_type)
{
  assert (state_data);

  if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_TEMPERATURE]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_VOLTAGE)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_VOLTAGE]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_CURRENT)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_CURRENT]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_FAN)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_FAN]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_PHYSICAL_SECURITY)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_PHYSICAL_SECURITY]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_PROCESSOR)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_PROCESSOR]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_POWER_SUPPLY)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_POWER_SUPPLY]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_POWER_UNIT)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_POWER_UNIT]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_MEMORY)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_MEMORY]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_DRIVE_SLOT)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_DRIVE_SLOT]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_SYSTEM_EVENT)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_SYSTEM_EVENT]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_CRITICAL_INTERRUPT)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_MODULE_BOARD)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_MODULE_BOARD]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_SLOT_CONNECTOR)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_SLOT_CONNECTOR]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_WATCHDOG2)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_WATCHDOG2]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_ENTITY_PRESENCE)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_ENTITY_PRESENCE]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_BATTERY)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_BATTERY]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_FRU_STATE)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_FRU_STATE]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_CABLE_INTERCONNECT)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_CABLE_INTERCONNECT]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_BOOT_ERROR)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_BOOT_ERROR]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_BUTTON_SWITCH)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_BUTTON_SWITCH]);
  else if (sensor_type == IPMI_MONITORING_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
    return (ipmi_sensor_types[IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE]);

  return (IPMIMONITORING_NA_STRING);
}

static const char *
_get_sensor_state_string (ipmimonitoring_state_data_t *state_data, int sensor_state)
{
  assert (state_data);

  if (sensor_state == IPMI_MONITORING_SENSOR_STATE_NOMINAL)
    return "Nominal";
  else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_WARNING)
    return "Warning";
  else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_CRITICAL)
    return "Critical";

  return (IPMIMONITORING_NA_STRING);
}

static const char *
_get_sensor_units_string (ipmimonitoring_state_data_t *state_data,
                          uint8_t sensor_units)
{
  const char *sensor_units_str;
  char **sensor_units_ptr;

  assert (state_data);

  if (state_data->prog_data->args->non_abbreviated_units)
    sensor_units_ptr = (char **)ipmi_sensor_units;
  else
    sensor_units_ptr = (char **)ipmi_sensor_units_abbreviated;

  if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_CELSIUS)
    sensor_units_str = sensor_units_ptr[IPMI_SENSOR_UNIT_DEGREES_C];
  else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT)
    sensor_units_str = sensor_units_ptr[IPMI_SENSOR_UNIT_DEGREES_F];
  else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_VOLTS)
    sensor_units_str = sensor_units_ptr[IPMI_SENSOR_UNIT_VOLTS];
  else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_AMPS)
    sensor_units_str = sensor_units_ptr[IPMI_SENSOR_UNIT_AMPS];
  else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_RPM)
    sensor_units_str = sensor_units_ptr[IPMI_SENSOR_UNIT_RPM];
  else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_WATTS)
    sensor_units_str = sensor_units_ptr[IPMI_SENSOR_UNIT_WATTS];
  else
    sensor_units_str = IPMIMONITORING_NA_STRING;

  return (sensor_units_str);
}

static int
_store_sensor_units_column_width (ipmimonitoring_state_data_t *state_data)
{
  const char *sensor_units_str;
  int len;

  assert (state_data);

  /* Calculate units column width special since it's a limited bunch */

  state_data->column_width.sensor_units = 0;
  
  sensor_units_str = _get_sensor_units_string (state_data, IPMI_MONITORING_SENSOR_UNITS_CELSIUS);
  len = strlen (sensor_units_str);
  if (len > state_data->column_width.sensor_units)
    state_data->column_width.sensor_units = len;

  sensor_units_str = _get_sensor_units_string (state_data, IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT);
  len = strlen (sensor_units_str);
  if (len > state_data->column_width.sensor_units)
    state_data->column_width.sensor_units = len;

  sensor_units_str = _get_sensor_units_string (state_data, IPMI_MONITORING_SENSOR_UNITS_VOLTS);
  len = strlen (sensor_units_str);
  if (len > state_data->column_width.sensor_units)
    state_data->column_width.sensor_units = len;

  sensor_units_str = _get_sensor_units_string (state_data, IPMI_MONITORING_SENSOR_UNITS_AMPS);
  len = strlen (sensor_units_str);
  if (len > state_data->column_width.sensor_units)
    state_data->column_width.sensor_units = len;

  sensor_units_str = _get_sensor_units_string (state_data, IPMI_MONITORING_SENSOR_UNITS_RPM);
  len = strlen (sensor_units_str);
  if (len > state_data->column_width.sensor_units)
    state_data->column_width.sensor_units = len;

  sensor_units_str = _get_sensor_units_string (state_data, IPMI_MONITORING_SENSOR_UNITS_WATTS);
  len = strlen (sensor_units_str);
  if (len > state_data->column_width.sensor_units)
    state_data->column_width.sensor_units = len;

  if (state_data->column_width.sensor_units < strlen(SENSORS_HEADER_UNITS_STR))
    state_data->column_width.sensor_units = strlen(SENSORS_HEADER_UNITS_STR);

  return (0);
}

static int 
_output_setup (ipmimonitoring_state_data_t *state_data)
{
  assert (state_data);

  if (state_data->prog_data->args->entity_sensor_names)
    {
      if (calculate_entity_id_counts (state_data->pstate,
                                      state_data->sdr_cache_ctx,
                                      state_data->sdr_parse_ctx,
                                      &(state_data->entity_id_counts)) < 0)
        return (-1);
    }

  if (!state_data->prog_data->args->legacy_output
      && !state_data->prog_data->args->comma_separated_output)
    {
      struct sensor_entity_id_counts *entity_ptr = NULL;

      if (state_data->prog_data->args->entity_sensor_names)
        entity_ptr = &(state_data->entity_id_counts);

      if (calculate_column_widths (state_data->pstate,
                                   state_data->sdr_cache_ctx,
                                   state_data->sdr_parse_ctx,
                                   state_data->prog_data->args->sensor_types,
                                   state_data->prog_data->args->sensor_types_length,
                                   state_data->prog_data->args->record_ids,
                                   state_data->prog_data->args->record_ids_length,
                                   !state_data->prog_data->args->non_abbreviated_units,
				   state_data->prog_data->args->shared_sensors,
                                   0,
                                   0,
                                   entity_ptr,
                                   &(state_data->column_width)) < 0)
        return (-1);
      
      /* Calculate units column width special since it's a limited
       * bunch for ipmimonitoring 
       */
      if (_store_sensor_units_column_width (state_data) < 0)
        return (-1);
    }
  
  return (0);
}

static int
_calculate_record_ids (ipmimonitoring_state_data_t *state_data,
                       unsigned int output_record_ids[MAX_SENSOR_RECORD_IDS],
                       unsigned int *output_record_ids_length)
{
  struct ipmimonitoring_arguments *args = NULL;
  unsigned int tmp_record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int tmp_record_ids_length = 0;

  assert (state_data);
  assert (output_record_ids);
  assert (output_record_ids_length);

  args = state_data->prog_data->args;

  memset (output_record_ids, '\0', sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
  (*output_record_ids_length) = 0;

  memset (tmp_record_ids, '\0', sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
  
  if (calculate_record_ids (state_data->pstate,
                            state_data->sdr_cache_ctx,
                            state_data->sdr_parse_ctx,
                            args->sensor_types,
                            args->sensor_types_length,
                            args->exclude_sensor_types,
                            args->exclude_sensor_types_length,
                            args->record_ids,
                            args->record_ids_length,
                            args->exclude_record_ids,
                            args->exclude_record_ids_length,
                            tmp_record_ids,
                            &tmp_record_ids_length) < 0)
    return (-1);

  if (!args->verbose_count)
    {
      unsigned int i;

      for (i = 0; i < tmp_record_ids_length; i++)
        {
          uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
          int sdr_record_len = 0;
          uint16_t record_id;
          uint8_t record_type;

          if (ipmi_sdr_cache_search_record_id (state_data->sdr_cache_ctx,
                                               tmp_record_ids[i]) < 0)
            {
              /* at this point shouldn't have record id not found error */
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_cache_search_record_id: %s\n",
                               ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
              return (-1);
            }
          
          if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_cache_ctx,
                                                            sdr_record,
                                                            IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_cache_record_read: %s\n",
                               ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
              return (-1);
            }
          
          /* Shouldn't be possible */
          if (!sdr_record_len)
            continue;
          
          if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_parse_ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 &record_id,
                                                 &record_type) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_parse_record_id_and_type: %s\n",
                               ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
              return (-1);
            }
          
          if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
              || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
            {
              output_record_ids[(*output_record_ids_length)] = tmp_record_ids[i];
              (*output_record_ids_length)++;
            }
        }
    }
  else
    {
      memcpy (output_record_ids, tmp_record_ids, sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
      (*output_record_ids_length) = tmp_record_ids_length;
    }

  return (0);
}
  

static void
_output_sensor_units (ipmimonitoring_state_data_t *state_data,
                      int sensor_units)
{
  const char *sensor_units_str;

  assert (state_data);

  sensor_units_str = _get_sensor_units_string (state_data, sensor_units);

  if (state_data->prog_data->args->legacy_output)
    pstdout_printf (state_data->pstate,
                    " | %s",
                    sensor_units_str);
  else
    {
      char fmt[IPMIMONITORING_FMT_BUFLEN + 1];

      memset (fmt, '\0', IPMIMONITORING_FMT_BUFLEN + 1);
      
      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMIMONITORING_FMT_BUFLEN,
                  ",%%s");
      else
        snprintf (fmt,
                  IPMIMONITORING_FMT_BUFLEN,
                  " | %%-%ds",
                  state_data->column_width.sensor_units);
      
      pstdout_printf (state_data->pstate,
                      fmt,
                      sensor_units_str);
    }
}

static int
_output_sensor_bitmask_strings (ipmimonitoring_state_data_t *state_data,
                                int sensor_state,
                                char **sensor_bitmask_strings)
{
  assert (state_data);

  if (sensor_state != IPMI_MONITORING_SENSOR_STATE_UNKNOWN)
    {
      if (state_data->prog_data->args->comma_separated_output)
        pstdout_printf (state_data->pstate, ",");
      else
        pstdout_printf (state_data->pstate, " |");
      
      if (sensor_bitmask_strings)
        {
          unsigned int i = 0;
          
          while (sensor_bitmask_strings[i])
            {
              if (!state_data->prog_data->args->comma_separated_output)
                pstdout_printf (state_data->pstate, " ");
              
              pstdout_printf (state_data->pstate,
                              "'%s'",
                              sensor_bitmask_strings[i]);
              
              i++;
            }
        }
      else
        {
          if (!state_data->prog_data->args->comma_separated_output)
            pstdout_printf (state_data->pstate, " ");
          
          pstdout_printf (state_data->pstate,
                          "'%s'",
                          IPMIMONITORING_NO_EVENT_STRING);
        }
    }
  else
    {
      if (state_data->prog_data->args->comma_separated_output)
        pstdout_printf (state_data->pstate,
                        ",%s",
                        IPMIMONITORING_NA_STRING);
      else
        pstdout_printf (state_data->pstate,
                        " | %s",
                        IPMIMONITORING_NA_STRING);
    }

  return (0);
}

static int
_ipmimonitoring_callback (ipmi_monitoring_ctx_t c, void *callback_data)
{
  ipmimonitoring_state_data_t *state_data;
  struct ipmimonitoring_arguments *args;
  int record_id, sensor_type, sensor_state, sensor_units,
    sensor_reading_type, sensor_bitmask_type, sensor_bitmask;
  char **sensor_bitmask_strings = NULL;
  const char *sensor_type_str;
  const char *sensor_state_str;
  char *sensor_name = NULL;
  void *sensor_reading;
  char sensor_name_buf[MAX_ENTITY_ID_SENSOR_NAME_STRING + 1];
  char fmt[IPMIMONITORING_FMT_BUFLEN + 1];
  int rv = -1;

  assert (c);
  assert (callback_data);

  state_data = (ipmimonitoring_state_data_t *)callback_data;
  args = state_data->prog_data->args;

  if (!state_data->prog_data->args->no_header_output
      && !state_data->output_headers)
    {
      if (args->legacy_output)
        {
          pstdout_printf (state_data->pstate,
                          "Record ID | Sensor Name | Sensor Group | Monitoring Status");

          if (!args->quiet_readings)
            pstdout_printf (state_data->pstate,
                            " | Sensor Units | Sensor Reading");
          
          pstdout_printf (state_data->pstate,
                          "\n");
        }
      else
        output_sensor_headers (state_data->pstate,
                               args->quiet_readings,
                               1,
                               state_data->prog_data->args->comma_separated_output,
                               state_data->prog_data->args->no_sensor_type_output,
                               &(state_data->column_width));

      state_data->output_headers++;
    }

  if ((record_id = ipmi_monitoring_read_record_id (state_data->ctx)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_monitoring_read_record_id: %s\n",
                       ipmi_monitoring_ctx_errormsg (state_data->ctx));
      goto cleanup;
    }
  if ((sensor_type = ipmi_monitoring_read_sensor_type (state_data->ctx)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_monitoring_read_sensor_type: %s\n",
                       ipmi_monitoring_ctx_errormsg (state_data->ctx));
      goto cleanup;
    }
  /* get adjusted sensor name if necessary */
  if (state_data->prog_data->args->entity_sensor_names)
    {
      int sensor_number;
      uint8_t sensor_number_tmp;

      memset (sensor_name_buf, '\0', MAX_ENTITY_ID_SENSOR_NAME_STRING + 1);

      if ((sensor_number = ipmi_monitoring_read_sensor_number (state_data->ctx)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_monitoring_read_sensor_number: %s\n",
                           ipmi_monitoring_ctx_errormsg (state_data->ctx));
          goto cleanup;
        }
      
      sensor_number_tmp = sensor_number;
      if (get_entity_sensor_name_string_by_record_id (state_data->pstate,
                                                      state_data->sdr_parse_ctx,
                                                      state_data->sdr_cache_ctx,
                                                      (uint16_t)record_id,
                                                      &(state_data->entity_id_counts),
                                                      &sensor_number_tmp,
                                                      sensor_name_buf,
                                                      MAX_ENTITY_ID_SENSOR_NAME_STRING) < 0)
        return (-1);

      sensor_name = sensor_name_buf;
    }
  else
    {
      if (!(sensor_name = ipmi_monitoring_read_sensor_name (state_data->ctx)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                          "ipmi_monitoring_read_sensor_name: %s\n",
                           ipmi_monitoring_ctx_errormsg (state_data->ctx));
          goto cleanup;
        }
    }
  if ((sensor_state = ipmi_monitoring_read_sensor_state (state_data->ctx)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_monitoring_read_sensor_state: %s\n",
                       ipmi_monitoring_ctx_errormsg (state_data->ctx));
      goto cleanup;
    }
  if ((sensor_units = ipmi_monitoring_read_sensor_units (state_data->ctx)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_monitoring_read_sensor_units: %s\n",
                       ipmi_monitoring_ctx_errormsg (state_data->ctx));
      goto cleanup;
    }
  if ((sensor_reading_type = ipmi_monitoring_read_sensor_reading_type (state_data->ctx)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_monitoring_read_sensor_reading_type: %s\n",
                       ipmi_monitoring_ctx_errormsg (state_data->ctx));
      goto cleanup;
    }
  if ((sensor_bitmask_type = ipmi_monitoring_read_sensor_bitmask_type (state_data->ctx)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_monitoring_read_sensor_bitmask_type: %s\n",
                       ipmi_monitoring_ctx_errormsg (state_data->ctx));
      goto cleanup;
    }
  if ((sensor_bitmask = ipmi_monitoring_read_sensor_bitmask (state_data->ctx)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_monitoring_read_sensor_bitmask: %s\n",
                       ipmi_monitoring_ctx_errormsg (state_data->ctx));
      goto cleanup;
    }

  sensor_bitmask_strings = ipmi_monitoring_read_sensor_bitmask_strings (state_data->ctx);

  sensor_reading = ipmi_monitoring_read_sensor_reading (state_data->ctx);

  if (!sensor_name
      || !strlen (sensor_name))
    sensor_name = IPMIMONITORING_NA_STRING;

  sensor_type_str = _get_sensor_type_string (state_data, sensor_type);

  sensor_state_str = _get_sensor_state_string (state_data, sensor_state);

  if (args->legacy_output)
    pstdout_printf (state_data->pstate,
                    "%u | %s | %s | %s",
                    record_id,
                    sensor_name,
                    sensor_type_str,
                    sensor_state_str);
  else
    {
      memset (fmt, '\0', IPMIMONITORING_FMT_BUFLEN + 1);

      if (args->no_sensor_type_output)
        {
          if (args->comma_separated_output)
            snprintf (fmt,
                      IPMIMONITORING_FMT_BUFLEN,
                      "%%u,%%s,%%s");
          else
            snprintf (fmt,
                      IPMIMONITORING_FMT_BUFLEN,
                      "%%-%du | %%-%ds | %%-8s",
                      state_data->column_width.record_id,
                      state_data->column_width.sensor_name);
          
          pstdout_printf (state_data->pstate,
                          fmt,
                          record_id,
                          sensor_name,
                          sensor_state_str);
        }
      else
        {
          if (args->comma_separated_output)
            snprintf (fmt,
                      IPMIMONITORING_FMT_BUFLEN,
                      "%%u,%%s,%%s,%%s");
          else
            snprintf (fmt,
                      IPMIMONITORING_FMT_BUFLEN,
                      "%%-%du | %%-%ds | %%-%ds | %%-8s",
                      state_data->column_width.record_id,
                      state_data->column_width.sensor_name,
                      state_data->column_width.sensor_type);
          
          pstdout_printf (state_data->pstate,
                          fmt,
                          record_id,
                          sensor_name,
                          sensor_type_str,
                          sensor_state_str);
        }
    }

  if (!args->quiet_readings && sensor_reading)
    {
      if (args->legacy_output)
        _output_sensor_units (state_data, sensor_units);

      if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
        {
          if (args->legacy_output)
            pstdout_printf (state_data->pstate,
                            " | %s",
                            (*((uint8_t *)sensor_reading) ? "true" : "false"));
          else
            {
              if (args->comma_separated_output)
                pstdout_printf (state_data->pstate,
                                ",%s",
                                (*((uint8_t *)sensor_reading) ? "true" : "false"));
              else
                pstdout_printf (state_data->pstate,
                                " | %-10s",
                                (*((uint8_t *)sensor_reading) ? "true" : "false"));
            }
        }
      else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
        {
          if (args->legacy_output)
            pstdout_printf (state_data->pstate,
                            " | %u",
                            *((uint32_t *)sensor_reading));
          else
            {
              if (args->comma_separated_output)
                pstdout_printf (state_data->pstate,
                                ",%u",
                                *((uint32_t *)sensor_reading));
              else
                pstdout_printf (state_data->pstate,
                                " | %-10u",
                                *((uint32_t *)sensor_reading));
            }
        }
      else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
        {
          if (args->legacy_output)
            pstdout_printf (state_data->pstate,
                            " | %f",
                            *((double *)sensor_reading));
          else
            {
              if (args->comma_separated_output)
                pstdout_printf (state_data->pstate,
                                ",%.2f",
                                *((double *)sensor_reading));
              else
                pstdout_printf (state_data->pstate,
                                " | %-10.2f",
                                *((double *)sensor_reading));
            }
        }
      else if (args->legacy_output)
        {
          /* legacy output has the bitmask as the "sensor reading" */
          if (_output_sensor_bitmask_strings (state_data,
                                              sensor_state,
                                              sensor_bitmask_strings) < 0)
            goto cleanup;
        }
      else
        {
          if (args->legacy_output)
            pstdout_printf (state_data->pstate,
                            " | %s",
                            IPMIMONITORING_NA_STRING);
          else
            {
              if (args->comma_separated_output)
                pstdout_printf (state_data->pstate,
                                ",%s",
                                IPMIMONITORING_NA_STRING);
              else
                pstdout_printf (state_data->pstate,
                                " | %-10s",
                                IPMIMONITORING_NA_STRING);
            }
        }

      if (!args->legacy_output)
        {
          _output_sensor_units (state_data, sensor_units);

          if (_output_sensor_bitmask_strings (state_data,
                                              sensor_state,
                                              sensor_bitmask_strings) < 0)
            goto cleanup;
        }
    }
  else if (!args->quiet_readings && !sensor_reading)
    {
      if (args->legacy_output)
        pstdout_printf (state_data->pstate,
                        " | %s",
                        IPMIMONITORING_NA_STRING);
      else
        {
          char fmt[IPMIMONITORING_FMT_BUFLEN + 1];
          
          memset (fmt, '\0', IPMIMONITORING_FMT_BUFLEN + 1);
          if (args->comma_separated_output)
            snprintf (fmt,
                      IPMIMONITORING_FMT_BUFLEN,
                      ",%%s,%%s");
          else
            snprintf (fmt,
                      IPMIMONITORING_FMT_BUFLEN,
                      " | %%-10s | %%-%ds",
                      state_data->column_width.sensor_units);
          
          pstdout_printf (state_data->pstate,
                          fmt,
                          IPMIMONITORING_NA_STRING,
                          IPMIMONITORING_NA_STRING);
        }

      if (_output_sensor_bitmask_strings (state_data,
                                          sensor_state,
                                          sensor_bitmask_strings) < 0)
        goto cleanup;
    }
  else if (args->quiet_readings && !args->legacy_output)
    {
      if (_output_sensor_bitmask_strings (state_data,
                                          sensor_state,
                                          sensor_bitmask_strings) < 0)
        goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "\n");

  rv = 0;
 cleanup:
  return (rv);
}

static int
run_cmd_args (ipmimonitoring_state_data_t *state_data)
{
  struct ipmi_monitoring_ipmi_config conf;
  struct ipmimonitoring_arguments *args;
  unsigned int sensor_reading_flags = 0;
  unsigned int output_record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int output_record_ids_length = 0;
  unsigned int *output_record_ids_ptr = NULL;
  char sdr_cache_directory[MAXPATHLEN+1];

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->list_sensor_types)
    return (_list_sensor_types (state_data));

  if (args->sdr.flush_cache)
    return (_flush_cache (state_data));

  /* Force use of same directory used for other FreeIPMI tools.
   *
   * call sdr_cache_create_directory() to create it first, otherwise
   * lib will say directory doesn't exist.
   */

  if (sdr_cache_create_directory (NULL, args->sdr.sdr_cache_directory) < 0)
    return (-1);

  memset (sdr_cache_directory, '\0', MAXPATHLEN + 1);

  if (sdr_cache_get_cache_directory (NULL,
                                     args->sdr.sdr_cache_directory,
                                     sdr_cache_directory,
                                     MAXPATHLEN) < 0)
    return (-1);

  if (ipmi_monitoring_ctx_sdr_cache_directory (state_data->ctx,
                                               sdr_cache_directory) < 0)
    {
      fprintf (stderr,
               "ipmi_monitoring_ctx_sdr_cache_directory: %s\n",
               ipmi_monitoring_ctx_errormsg (state_data->ctx));
      return (-1);
    }

  /* Force use of same filename format used for other FreeIPMI tools.
   */
  if (ipmi_monitoring_ctx_sdr_cache_filenames (state_data->ctx,
                                               "sdr-cache-%L.%H") < 0)
    {
      fprintf (stderr,
               "ipmi_monitoring_ctx_sdr_cache_filename: %s\n",
               ipmi_monitoring_ctx_errormsg (state_data->ctx));
      return (-1);
    }

  if (ipmi_monitoring_ctx_sensor_config_file (state_data->ctx,
                                              args->sensor_config_file) < 0)
    {
      fprintf (stderr,
               "ipmi_monitoring_ctx_sensor_config_file: %s\n",
               ipmi_monitoring_ctx_errormsg (state_data->ctx));
      return (-1);
    }

  /* libipmimonitoring SDR creation/loading on its own.  However we do
   * it here so the ipmimonitoring tool and resemble other FreeIPMI
   * tools more closely.
   */

  if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 args->sdr.quiet_cache,
                                 args->sdr.sdr_cache_recreate,
                                 state_data->hostname,
                                 args->sdr.sdr_cache_directory) < 0)
    return (-1);

  if (_output_setup (state_data) < 0)
    return (-1);

  if (!args->legacy_output)
    {
      /* achu: for consistent output to ipmi-sensors, calculate
       * appropriate record IDs.
       */
      if (_calculate_record_ids (state_data,
                                 output_record_ids,
                                 &output_record_ids_length) < 0)
        return (-1);
          
      /* special case, nothing to output */
      if (!output_record_ids_length)
        return (0);

      output_record_ids_ptr = output_record_ids;
    }
  else
    {
      if (!args->verbose_count)
        sensor_reading_flags = IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS;
    }

  /* At this point in time we no longer need ipmi_ctx b/c
   * libipmimonitoring will open its own copy.  Although no BMC should
   * be dumb enough to not handle multiple connections at the same
   * time, it would be prudent to err on the side of safety and close
   * this connection.
   */
  ipmi_ctx_close (state_data->ipmi_ctx);
  ipmi_ctx_destroy (state_data->ipmi_ctx);
  state_data->ipmi_ctx = NULL;

  if (args->ignore_non_interpretable_sensors)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS;
  
  if (args->bridge_sensors)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_BRIDGE_SENSORS;

  if (args->shared_sensors)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS;

  if (args->interpret_oem_data)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_INTERPRET_OEM_DATA;

  memset (&conf, '\0', sizeof (struct ipmi_monitoring_ipmi_config));
  memcpy (&conf, &(args->conf), sizeof (struct ipmi_monitoring_ipmi_config));

  /* 
   * achu: configure per-situation workaround flags.  Can't be done
   * below in _convert_to_ipmimonitoring_options() b/c user could have
   * input localhost/127.0.0.1.
   */
  if (state_data->hostname
      && strcasecmp (state_data->hostname, "localhost")
      && strcmp (state_data->hostname, "127.0.0.1"))
    {
      if (conf.protocol_version == IPMI_MONITORING_PROTOCOL_VERSION_2_0)
        {
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;
        }
      else
        {
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_INTEL_2_0_SESSION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_SUN_2_0_SESSION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE)
            conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE;
        }
    }
  /* else - no inband workaround flags yet */

  if ((!args->record_ids_length && !args->ipmimonitoring_sensor_types_length)
      || output_record_ids_ptr)
    {
      if (ipmi_monitoring_sensor_readings_by_record_id (state_data->ctx,
                                                        state_data->hostname,
                                                        &conf,
                                                        sensor_reading_flags,
                                                        output_record_ids_ptr,
                                                        output_record_ids_length,
                                                        _ipmimonitoring_callback,
                                                        (void *)state_data) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_monitoring_sensor_readings_by_record_id: %s\n",
                           ipmi_monitoring_ctx_errormsg (state_data->ctx));
          return (-1);
        }
    }
  else if (args->record_ids_length)
    {
      if (ipmi_monitoring_sensor_readings_by_record_id (state_data->ctx,
                                                        state_data->hostname,
                                                        &conf,
                                                        sensor_reading_flags,
                                                        args->record_ids,
                                                        args->record_ids_length,
                                                        _ipmimonitoring_callback,
                                                        (void *)state_data) < 0)
        {
          /* special case error message */
          /* this error message is left for --legacy-output, new output 
           * should catch bad record ids in _calculate_record_ids().
           */
          if (ipmi_monitoring_ctx_errnum (state_data->ctx) == IPMI_MONITORING_ERR_SENSOR_NOT_FOUND)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "invalid record id specified\n");
          else
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_monitoring_sensor_readings_by_record_id: %s\n",
                             ipmi_monitoring_ctx_errormsg (state_data->ctx));
          return (-1);
        }
    }
  else
    {
      if (ipmi_monitoring_sensor_readings_by_sensor_type (state_data->ctx,
                                                          state_data->hostname,
                                                          &conf,
                                                          sensor_reading_flags,
                                                          args->ipmimonitoring_sensor_types,
                                                          args->ipmimonitoring_sensor_types_length,
                                                          _ipmimonitoring_callback,
                                                          (void *)state_data) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_monitoring_sensor_readings_by_sensor_type: %s\n",
                           ipmi_monitoring_ctx_errormsg (state_data->ctx));
          return (-1);
        }
    }

  return (0);
}

static int
_ipmimonitoring (pstdout_state_t pstate,
                 const char *hostname,
                 void *arg)
{
  ipmimonitoring_state_data_t state_data;
  ipmimonitoring_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code;

  prog_data = (ipmimonitoring_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmimonitoring_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* libipmimonitoring does an IPMI connection and SDR creation.
   * However we open up an IPMI connection to do the SDR cache
   * creation outside of libipmimonitoring so ipmimonitoring (the
   * tool) can resemble the other FreeIPMI tools closely.
   */

  /* Special case, just flush, don't do an IPMI connection */
  /* Special case, just list types, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache
      && !prog_data->args->list_sensor_types)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                             hostname,
                                             &(prog_data->args->common),
                                             errmsg,
                                             IPMI_OPEN_ERRMSGLEN)))
        {
          pstdout_fprintf (pstate,
                           stderr,
                           "%s\n",
                           errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (!(state_data.sdr_cache_ctx = ipmi_sdr_cache_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_parse_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));
    }

  if (!(state_data.ctx = ipmi_monitoring_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_monitoring_ctx_create:");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.ctx)
    ipmi_monitoring_ctx_destroy (state_data.ctx);
  if (state_data.sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy (state_data.sdr_cache_ctx);
  if (state_data.sdr_parse_ctx)
    ipmi_sdr_parse_ctx_destroy (state_data.sdr_parse_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return (exit_code);
}

static int
_convert_to_ipmimonitoring_sensor_type (const char *sensor_type)
{
  int n;

  assert (sensor_type);

  if (sensor_type_strcmp (NULL,
                          sensor_type,
                          IPMI_SENSOR_TYPE_TEMPERATURE) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_VOLTAGE) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_VOLTAGE;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_CURRENT) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_CURRENT;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_FAN) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_FAN;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_PHYSICAL_SECURITY) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_PHYSICAL_SECURITY;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_PROCESSOR) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_PROCESSOR;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_POWER_SUPPLY) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_POWER_SUPPLY;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_POWER_UNIT) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_POWER_UNIT;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_MEMORY) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_MEMORY;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_DRIVE_SLOT) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_DRIVE_SLOT;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_EVENT_LOGGING_DISABLED;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_SYSTEM_EVENT) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_SYSTEM_EVENT;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_CRITICAL_INTERRUPT;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_MODULE_BOARD) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_MODULE_BOARD;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_SLOT_CONNECTOR) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_SLOT_CONNECTOR;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_WATCHDOG2) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_WATCHDOG2;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_ENTITY_PRESENCE) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_ENTITY_PRESENCE;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_BATTERY) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_BATTERY;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_FRU_STATE) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_FRU_STATE;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_CABLE_INTERCONNECT) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_CABLE_INTERCONNECT;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_BOOT_ERROR) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_BOOT_ERROR;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_BUTTON_SWITCH) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_BUTTON_SWITCH;
  else if (sensor_type_strcmp (NULL,
                               sensor_type,
                               IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE) == 1)
    n = IPMI_MONITORING_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE;
  else
    {
      fprintf (stderr, "invalid sensor type '%s'\n", sensor_type);
      exit (1);
    }

  return (n);
}

/* For some ipmimonitoring library functions, we need to convert
 * cmd_args struct into the ipmimonitoring library equivalent
 * structs.
 */
static void
_convert_to_ipmimonitoring_options (struct ipmimonitoring_arguments *cmd_args)
{
  unsigned int i;

  assert (cmd_args);

  if (cmd_args->common.driver_type != IPMI_DEVICE_UNKNOWN)
    {
      if (cmd_args->common.driver_type == IPMI_DEVICE_LAN)
        cmd_args->conf.protocol_version = IPMI_MONITORING_PROTOCOL_VERSION_1_5;
      else if (cmd_args->common.driver_type == IPMI_DEVICE_LAN_2_0)
        cmd_args->conf.protocol_version = IPMI_MONITORING_PROTOCOL_VERSION_2_0;
      else if (cmd_args->common.driver_type == IPMI_DEVICE_KCS)
        cmd_args->conf.driver_type = IPMI_MONITORING_DRIVER_TYPE_KCS;
      else if (cmd_args->common.driver_type == IPMI_DEVICE_SSIF)
        cmd_args->conf.driver_type = IPMI_MONITORING_DRIVER_TYPE_SSIF;
      else if (cmd_args->common.driver_type == IPMI_DEVICE_OPENIPMI)
        cmd_args->conf.driver_type = IPMI_MONITORING_DRIVER_TYPE_OPENIPMI;
      else if (cmd_args->common.driver_type == IPMI_DEVICE_SUNBMC)
        cmd_args->conf.driver_type = IPMI_MONITORING_DRIVER_TYPE_SUNBMC;
    }
  else
    {
      cmd_args->conf.driver_type = -1;
      cmd_args->conf.protocol_version = -1;
    }

  cmd_args->conf.disable_auto_probe = cmd_args->common.disable_auto_probe;
  cmd_args->conf.driver_address = cmd_args->common.driver_address;
  cmd_args->conf.register_spacing = cmd_args->common.register_spacing;
  cmd_args->conf.driver_device = cmd_args->common.driver_device;

  cmd_args->conf.username = cmd_args->common.username;
  cmd_args->conf.password = cmd_args->common.password;
  cmd_args->conf.k_g = cmd_args->common.k_g;
  cmd_args->conf.k_g_len = cmd_args->common.k_g_len;

  if (cmd_args->common.privilege_level == IPMI_PRIVILEGE_LEVEL_USER)
    cmd_args->conf.privilege_level = IPMI_MONITORING_PRIVILEGE_LEVEL_USER;
  else if (cmd_args->common.privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR)
    cmd_args->conf.privilege_level = IPMI_MONITORING_PRIVILEGE_LEVEL_OPERATOR;
  else if (cmd_args->common.privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN)
    cmd_args->conf.privilege_level = IPMI_MONITORING_PRIVILEGE_LEVEL_ADMIN;
  else
    cmd_args->conf.privilege_level = IPMI_MONITORING_PRIVILEGE_LEVEL_USER;

  if (cmd_args->common.authentication_type == IPMI_AUTHENTICATION_TYPE_NONE)
    cmd_args->conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_NONE;
  else if (cmd_args->common.authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
    cmd_args->conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
  else if (cmd_args->common.authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
    cmd_args->conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_MD2;
  else if (cmd_args->common.authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
    cmd_args->conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_MD5;
  else
    cmd_args->conf.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_MD5;

  cmd_args->conf.cipher_suite_id = cmd_args->common.cipher_suite_id;

  cmd_args->conf.session_timeout_len = cmd_args->common.session_timeout;
  cmd_args->conf.retransmission_timeout_len = cmd_args->common.retransmission_timeout;

  cmd_args->conf.workaround_flags = 0;
  /* calculate workaround flags later dependent on settings/inputs */

  if (cmd_args->common.debug)
    {
      cmd_args->ipmimonitoring_flags |= IPMI_MONITORING_FLAGS_DEBUG;
      cmd_args->ipmimonitoring_flags |= IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS;
    }

  for (i = 0; i < cmd_args->sensor_types_length; i++)
    {
      int n;

      n = _convert_to_ipmimonitoring_sensor_type (cmd_args->sensor_types[i]);
      cmd_args->ipmimonitoring_sensor_types[cmd_args->ipmimonitoring_sensor_types_length] = n;
      cmd_args->ipmimonitoring_sensor_types_length++;
    }
}

int
main (int argc, char **argv)
{
  ipmimonitoring_prog_data_t prog_data;
  struct ipmimonitoring_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmimonitoring_prog_data_t));
  prog_data.progname = argv[0];
  ipmimonitoring_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  _convert_to_ipmimonitoring_options (&cmd_args);

  if (_setup_ipmimonitoring_lib (&cmd_args) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((hosts_count = pstdout_setup (&(prog_data.args->common.hostname),
                                    prog_data.args->hostrange.buffer_output,
                                    prog_data.args->hostrange.consolidate_output,
                                    prog_data.args->hostrange.fanout,
                                    prog_data.args->hostrange.eliminate,
                                    prog_data.args->hostrange.always_prefix)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!hosts_count)
    {
      exit_code = EXIT_SUCCESS;
      goto cleanup;
    }

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->sdr.quiet_cache = 1;

  if ((rv = pstdout_launch (prog_data.args->common.hostname,
                            _ipmimonitoring,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}

