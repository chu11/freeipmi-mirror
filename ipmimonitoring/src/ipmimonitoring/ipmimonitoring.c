/*****************************************************************************\
 *  $Id: ipmimonitoring.c,v 1.64.2.2 2008-11-27 15:38:57 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMIMONITORING_BUFLEN         1024
   
static int
_list_groups(ipmimonitoring_state_data_t *state_data)
{
  assert(state_data);

  pstdout_printf (state_data->pstate, "%s\n", "temperature");
  pstdout_printf (state_data->pstate, "%s\n", "voltage");
  pstdout_printf (state_data->pstate, "%s\n", "current");
  pstdout_printf (state_data->pstate, "%s\n", "fan");
  pstdout_printf (state_data->pstate, "%s\n", "physical_security");
  pstdout_printf (state_data->pstate, "%s\n", "platform_security_violation_attempt");
  pstdout_printf (state_data->pstate, "%s\n", "processor");
  pstdout_printf (state_data->pstate, "%s\n", "power_supply");
  pstdout_printf (state_data->pstate, "%s\n", "power_unit");
  pstdout_printf (state_data->pstate, "%s\n", "memory");
  pstdout_printf (state_data->pstate, "%s\n", "drive_slot");
  pstdout_printf (state_data->pstate, "%s\n", "system_firmware_progress");
  pstdout_printf (state_data->pstate, "%s\n", "event_logging_disabled");
  pstdout_printf (state_data->pstate, "%s\n", "system_event");
  pstdout_printf (state_data->pstate, "%s\n", "critical_interrupt");
  pstdout_printf (state_data->pstate, "%s\n", "module_board");
  pstdout_printf (state_data->pstate, "%s\n", "slot_connector");
  pstdout_printf (state_data->pstate, "%s\n", "watchdog2");
  pstdout_printf (state_data->pstate, "%s\n", "entity_presence");
  pstdout_printf (state_data->pstate, "%s\n", "management_subsystem_health");
  pstdout_printf (state_data->pstate, "%s\n", "battery");
  pstdout_printf (state_data->pstate, "%s\n", "fru_state");
  pstdout_printf (state_data->pstate, "%s\n", "cable_interconnect");
  pstdout_printf (state_data->pstate, "%s\n", "boot_error");
  pstdout_printf (state_data->pstate, "%s\n", "button_switch");
  pstdout_printf (state_data->pstate, "%s\n", "system_acpi_power_state");
  
  return 0;
}

static int
_flush_cache (ipmimonitoring_state_data_t *state_data)
{
  assert(state_data);

  if (sdr_cache_flush_cache(state_data->ipmi_sdr_cache_ctx,
                            state_data->pstate,
                            state_data->prog_data->args->sdr.quiet_cache,
                            state_data->hostname,
                            state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return -1;
  
  return 0;
}

static int
_setup_ipmimonitoring_lib (struct ipmimonitoring_arguments *args)
{
  char sdr_cache_directory[MAXPATHLEN+1];
  int errnum;

  assert(args);

  /* Force use of same directory used for other FreeIPMI tools. 
   * 
   * call sdr_cache_create_directory() to create it first, otherwise
   * lib will say directory doesn't exist.
   */

  if (sdr_cache_create_directory (NULL, args->sdr.sdr_cache_directory) < 0)
    return -1;

  if (sdr_cache_get_cache_directory(NULL,
                                    args->sdr.sdr_cache_directory,
                                    sdr_cache_directory,
                                    MAXPATHLEN) < 0)
    return -1;

  if (ipmi_monitoring_sdr_cache_directory(sdr_cache_directory, &errnum) < 0)
    {
      fprintf(stderr, 
              "ipmi_monitoring_sdr_cache_directory: %s\n", 
              ipmi_monitoring_ctx_strerror(errnum));
      return -1;
    }
  
  /* Force use of same filename format used for other FreeIPMI tools.
   */
  if (ipmi_monitoring_sdr_cache_filenames("sdr-cache-%L.%H", &errnum) < 0)
    {
      fprintf(stderr, 
              "ipmi_monitoring_sdr_cache_filename: %s\n", 
              ipmi_monitoring_ctx_strerror(errnum));
      return -1;
    }

  if (args->sensor_config_file)
    {
      if (ipmi_monitoring_sensor_config_file(args->sensor_config_file, &errnum) < 0)
        {
          fprintf(stderr, 
                  "ipmi_monitoring_sensor_config_file: %s\n", 
                  ipmi_monitoring_ctx_strerror(errnum));
          return -1;
        }
    }
  
  if (ipmi_monitoring_init(args->ipmimonitoring_flags, &errnum) < 0)
    {
      fprintf(stderr, "ipmi_monitoring_init: %s\n", ipmi_monitoring_ctx_strerror(errnum));
      return -1;
    }

  return 0;
}

int
run_cmd_args (ipmimonitoring_state_data_t *state_data)
{
  struct ipmimonitoring_arguments *args;
  unsigned int sensor_reading_flags;
  int i, num;
  
  assert(state_data);

  args = state_data->prog_data->args;

  if (args->list_groups)
    return _list_groups (state_data);

  if (args->sdr.flush_cache)
    return _flush_cache (state_data);

  /* libipmimonitoring SDR creation/loading on its own.  However we do
   * it here so the ipmimonitoring tool and resemble other FreeIPMI
   * tools more closely.
   */

  if (sdr_cache_create_and_load(state_data->ipmi_sdr_cache_ctx,
                                state_data->pstate,
                                state_data->ipmi_ctx,
                                args->sdr.quiet_cache,
                                args->sdr.sdr_cache_recreate,
                                state_data->hostname,
                                args->sdr.sdr_cache_directory) < 0)
    return -1;

  /* At this point in time we no longer need sdr_cache b/c
   * libipmimonitoring will open its own copy.
   */
  ipmi_sdr_cache_close(state_data->ipmi_sdr_cache_ctx);
  ipmi_sdr_cache_ctx_destroy(state_data->ipmi_sdr_cache_ctx);
  state_data->ipmi_sdr_cache_ctx = NULL;

  /* At this point in time we no longer need ipmi_ctx b/c
   * libipmimonitoring will open its own copy.  Although no BMC should
   * be dumb enough to not handle multiple connections at the same
   * time, it would be prudent to err on the side of safety and close
   * this connection.
   */
  ipmi_ctx_close(state_data->ipmi_ctx);
  ipmi_ctx_destroy(state_data->ipmi_ctx);
  state_data->ipmi_ctx = NULL;
  
  if (args->verbose)
    sensor_reading_flags = 0;
  else
    sensor_reading_flags = IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS;

  if (args->regenerate_sdr_cache)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_REREAD_SDR_CACHE;

  if (args->bridge_sensors)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_BRIDGE_SENSORS;
  
  if (!args->sensors_length && !args->ipmimonitoring_groups_length)
    {
      if ((num = ipmi_monitoring_sensor_readings_by_record_id(state_data->ctx,
                                                              state_data->hostname,
                                                              &(args->conf),
                                                              sensor_reading_flags,
                                                              NULL,
                                                              0)) < 0)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_monitoring_sensor_readings_by_record_id: %s\n",
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
        }
    }
  else if (args->sensors_length)
    {
      if ((num = ipmi_monitoring_sensor_readings_by_record_id(state_data->ctx,
                                                              state_data->hostname,
                                                              &(args->conf),
                                                              sensor_reading_flags,
                                                              args->sensors,
                                                              args->sensors_length)) < 0)
        {
          /* special case error message */
          if (ipmi_monitoring_ctx_errnum(state_data->ctx) == IPMI_MONITORING_ERR_SENSOR_NOT_FOUND)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "invalid record id specified\n");
          else
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_monitoring_sensor_readings_by_record_id: %s\n",
                            ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
        }
    }
  else 
    {
      if ((num = ipmi_monitoring_sensor_readings_by_sensor_group(state_data->ctx,
                                                                 state_data->hostname,
                                                                 &(args->conf),
                                                                 sensor_reading_flags,
                                                                 args->ipmimonitoring_groups,
                                                                 args->ipmimonitoring_groups_length)) < 0)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_monitoring_sensor_readings_by_sensor_group: %s\n",
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
        }
    }
    

  pstdout_printf(state_data->pstate, 
                 "Record_ID | Sensor Name | Sensor Group | Monitoring Status");
  if (!args->quiet_readings)
    pstdout_printf(state_data->pstate, 
                   "| Sensor Units | Sensor Reading");
  pstdout_printf(state_data->pstate, 
                 "\n");

  for (i = 0; i < num; i++, ipmi_monitoring_iterator_next(state_data->ctx))
    {
      int record_id, sensor_group, sensor_state, sensor_units, sensor_reading_type;
      char *sensor_group_str, *sensor_state_str, *sensor_units_str;
      char *sensor_name;
      void *sensor_reading;

      if ((record_id = ipmi_monitoring_iterator_record_id(state_data->ctx)) < 0)
	{
	  pstdout_fprintf(state_data->pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_record_id: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
	}
      if ((sensor_group = ipmi_monitoring_iterator_sensor_group(state_data->ctx)) < 0)
        {
	  pstdout_fprintf(state_data->pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_group: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
        }
      if (!(sensor_name = ipmi_monitoring_iterator_sensor_name(state_data->ctx)))
	{
	  pstdout_fprintf(state_data->pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_name: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
	}
      if ((sensor_state = ipmi_monitoring_iterator_sensor_state(state_data->ctx)) < 0)
	{
	  pstdout_fprintf(state_data->pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_state: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
	}
      if ((sensor_units = ipmi_monitoring_iterator_sensor_units(state_data->ctx)) < 0)
	{
	  pstdout_fprintf(state_data->pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_units: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
	}
      if ((sensor_reading_type = ipmi_monitoring_iterator_sensor_reading_type(state_data->ctx)) < 0)
	{
	  pstdout_fprintf(state_data->pstate, 
                          stderr, 
                          "ipmi_monitoring_iterator_sensor_reading_type: %s\n", 
                          ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
          return -1;
	}
      sensor_reading = ipmi_monitoring_iterator_sensor_reading(state_data->ctx);

      if (!sensor_name
          || !strlen(sensor_name))
        sensor_name = "N/A";

      if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_TEMPERATURE)
        sensor_group_str = "Temperature";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_VOLTAGE)
        sensor_group_str = "Voltage";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_CURRENT)
        sensor_group_str = "Current";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_FAN)
        sensor_group_str = "Fan";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_PHYSICAL_SECURITY)
        sensor_group_str = "Physical Security";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
        sensor_group_str = "Security Violation Attempt";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_PROCESSOR)
        sensor_group_str = "Group Processor";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_POWER_SUPPLY)
        sensor_group_str = "Power Supply";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_POWER_UNIT)
        sensor_group_str = "Power Unit";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_MEMORY)
        sensor_group_str = "Memory";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_DRIVE_SLOT)
        sensor_group_str = "Drive Slot";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_SYSTEM_FIRMWARE_PROGRESS)
        sensor_group_str = "System Firmware Progress";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_EVENT_LOGGING_DISABLED)
        sensor_group_str = "Event Logging Disabled";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_SYSTEM_EVENT)
        sensor_group_str = "System Event";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_CRITICAL_INTERRUPT)
        sensor_group_str = "Critical Interrupt";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_MODULE_BOARD)
        sensor_group_str = "Module Board";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_SLOT_CONNECTOR)
        sensor_group_str = "Slot Connector";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_WATCHDOG2)
        sensor_group_str = "Watchdog2";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_ENTITY_PRESENCE)
        sensor_group_str = "Entity Presence";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_MANAGEMENT_SUBSYSTEM_HEALTH)
        sensor_group_str = "Management Subsystem Health";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_BATTERY)
        sensor_group_str = "Battery";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_FRU_STATE)
        sensor_group_str = "FRU State";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_CABLE_INTERCONNECT)
        sensor_group_str = "Cable Interconnect";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_BOOT_ERROR)
        sensor_group_str = "Boot Error";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_BUTTON_SWITCH)
        sensor_group_str = "Button Switch";
      else if (sensor_group == IPMI_MONITORING_SENSOR_GROUP_SYSTEM_ACPI_POWER_STATE)
        sensor_group_str = "System ACPI Power State";
      else 
        sensor_group_str = "N/A";

      if (sensor_state == IPMI_MONITORING_SENSOR_STATE_NOMINAL)
	sensor_state_str = "Nominal";
      else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_WARNING)
	sensor_state_str = "Warning";
      else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_CRITICAL)
	sensor_state_str = "Critical";
      else
	sensor_state_str = "N/A";

      pstdout_printf(state_data->pstate,
                     "%d | %s | %s | %s", 
                     record_id, 
                     sensor_name, 
                     sensor_group_str,
                     sensor_state_str);
      
      if (!args->quiet_readings && sensor_reading)
        {
          if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_CELSIUS)
            sensor_units_str = "C";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT)
            sensor_units_str = "F";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_VOLTS)
            sensor_units_str = "V";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_AMPS)
            sensor_units_str = "A";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_RPM)
            sensor_units_str = "RPM";
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_WATTS)
            sensor_units_str = "W";
          else
            sensor_units_str = "N/A";

          pstdout_printf(state_data->pstate,
                         " | %s", 
                         sensor_units_str);

          if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
            pstdout_printf(state_data->pstate,
                           " | %s ", 
                           (*((uint8_t *)sensor_reading) ? "true" : "false"));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
            pstdout_printf(state_data->pstate,
                           " | %d ", 
                           *((uint32_t *)sensor_reading));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
            pstdout_printf(state_data->pstate,
                           " | %f ", 
                           *((double *)sensor_reading));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER16_BITMASK)
            {
              int bitmask_type;

              if ((bitmask_type = ipmi_monitoring_iterator_sensor_bitmask_type(state_data->ctx)) < 0)
                {
                  pstdout_fprintf(state_data->pstate, 
                                  stderr, 
                                  "ipmi_monitoring_iterator_sensor_bitmask_type: %s\n", 
                                  ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
                  return -1;
                }

              if (bitmask_type != IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN)
                {
                  uint16_t bitmask_value = *((uint16_t *)sensor_reading);
                  unsigned int output_count = 0;
                  int j;
                  
                  pstdout_printf(state_data->pstate,
                                 " |");

                  for (j = 0; j < 16; j++)
                    {
                      if (bitmask_value & (0x1 << j))
                        {
                          char buffer[IPMIMONITORING_BUFLEN+1];
                          
                          memset(buffer, '\0', IPMIMONITORING_BUFLEN+1);
                          if (ipmi_monitoring_bitmask_string(state_data->ctx,
                                                             bitmask_type,
                                                             (bitmask_value & (0x1 << j)),
                                                             buffer,
                                                             IPMIMONITORING_BUFLEN) < 0)
                            {
                              /* If parameters error, assume remote machine has given us some
                               * bogus offset.  We'll fall through and output an appropriate string.
                               */
                              if (ipmi_monitoring_ctx_errnum(state_data->ctx) != IPMI_MONITORING_ERR_PARAMETERS)
                                {
                                  pstdout_fprintf(state_data->pstate, 
                                                  stderr, 
                                                  "ipmi_monitoring_bitmask_string: %s\n", 
                                                  ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)));
                                  return -1;
                                }
                              
                              if (ipmi_monitoring_ctx_errnum(state_data->ctx) == IPMI_MONITORING_ERR_PARAMETERS
                                  && state_data->prog_data->args->common.debug)
                                pstdout_fprintf(state_data->pstate,
                                                stderr,
                                                "ipmi_monitoring_bitmask_string: %s: invalid bitmask likely: %X\n", 
                                                ipmi_monitoring_ctx_strerror(ipmi_monitoring_ctx_errnum(state_data->ctx)),
                                                *((uint16_t *)sensor_reading));

                              snprintf(buffer, IPMIMONITORING_BUFLEN, "Unrecognized State");
                            }
                      
                          pstdout_printf(state_data->pstate,
                                         " '%s'", 
                                         buffer);
                          output_count++;
                        }
                    }

                  if (!output_count)
                    pstdout_printf(state_data->pstate,
                                   " 'OK'");
                }
              else
                pstdout_printf(state_data->pstate,
                               " | 0x%X", 
                               *((uint16_t *)sensor_reading));
            }
          else
            pstdout_printf(state_data->pstate,
                           " | N/A");
        }
      else if (!args->quiet_readings && !sensor_reading)
        pstdout_printf(state_data->pstate,
                       " | N/A | N/A");

      pstdout_printf(state_data->pstate,
                     "\n");
    }
 
  return 0;
}

static int
_ipmimonitoring(pstdout_state_t pstate,
                const char *hostname,
                void *arg)
{
  ipmimonitoring_state_data_t state_data;
  ipmimonitoring_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code;

  prog_data = (ipmimonitoring_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(ipmimonitoring_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* libipmimonitoring does an IPMI connection and SDR creation.
   * However we open up an IPMI connection to do the SDR cache
   * creation outside of libipmimonitoring so ipmimonitoring (the
   * tool) can resemble the other FreeIPMI tools closely.
   */

  /* Special case, just flush, don't do an IPMI connection */
  /* Special case, just list groups, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache
      && !prog_data->args->list_groups)
    {
      if (!(state_data.ipmi_ctx = ipmi_open(prog_data->progname,
                                            hostname,
                                            &(prog_data->args->common),
                                            errmsg,
                                            IPMI_OPEN_ERRMSGLEN)))
        {
          pstdout_fprintf(pstate,
                          stderr,
                          "%s\n",
                          errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (!(state_data.ipmi_sdr_cache_ctx = ipmi_sdr_cache_ctx_create()))
    {
      pstdout_perror (pstate, "ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags(state_data.ipmi_sdr_cache_ctx,
                                       IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data.ipmi_sdr_cache_ctx)));
    }

  if (!(state_data.ctx = ipmi_monitoring_ctx_create()))
    {
      pstdout_perror(pstate, "ipmi_monitoring_ctx_create:");
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
    ipmi_monitoring_ctx_destroy(state_data.ctx);
  if (state_data.ipmi_sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy(state_data.ipmi_sdr_cache_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx); 
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return exit_code;
}

/* For some ipmimonitoring library functions, we need to convert
 * cmd_args struct into the ipmimonitoring library equivalent
 * structs.
 */
void
_grab_ipmimonitoring_options(struct ipmimonitoring_arguments *cmd_args)
{
  int i;

  assert(cmd_args);

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
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO;
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION;
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER;
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_INTEL_2_0_SESSION;
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION;
  if (cmd_args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION)
    cmd_args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_SUN_2_0_SESSION;

  if (cmd_args->common.debug)
    {
      cmd_args->ipmimonitoring_flags |= IPMI_MONITORING_FLAGS_DEBUG;
      cmd_args->ipmimonitoring_flags |= IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS;
    }

  for (i = 0; i < cmd_args->groups_length; i++)
    { 
      int n = -1;

      if (!strcasecmp(cmd_args->groups[i], "temperature"))
        n = IPMI_MONITORING_SENSOR_GROUP_TEMPERATURE;
      else if (!strcasecmp(cmd_args->groups[i], "voltage"))
        n = IPMI_MONITORING_SENSOR_GROUP_VOLTAGE;
      else if (!strcasecmp(cmd_args->groups[i], "current"))
        n = IPMI_MONITORING_SENSOR_GROUP_CURRENT;
      else if (!strcasecmp(cmd_args->groups[i], "fan"))
        n = IPMI_MONITORING_SENSOR_GROUP_FAN;
      else if (!strcasecmp(cmd_args->groups[i], "physical_security"))
        n = IPMI_MONITORING_SENSOR_GROUP_PHYSICAL_SECURITY;
      else if (!strcasecmp(cmd_args->groups[i], "platform_security_violation_attempt"))
        n = IPMI_MONITORING_SENSOR_GROUP_PLATFORM_SECURITY_VIOLATION_ATTEMPT;
      else if (!strcasecmp(cmd_args->groups[i], "processor"))
        n = IPMI_MONITORING_SENSOR_GROUP_PROCESSOR;
      else if (!strcasecmp(cmd_args->groups[i], "power_supply"))
        n = IPMI_MONITORING_SENSOR_GROUP_POWER_SUPPLY;
      else if (!strcasecmp(cmd_args->groups[i], "power_unit"))
        n = IPMI_MONITORING_SENSOR_GROUP_POWER_UNIT;
      else if (!strcasecmp(cmd_args->groups[i], "memory"))
        n = IPMI_MONITORING_SENSOR_GROUP_MEMORY;
      else if (!strcasecmp(cmd_args->groups[i], "drive_slot"))
        n = IPMI_MONITORING_SENSOR_GROUP_DRIVE_SLOT;
      else if (!strcasecmp(cmd_args->groups[i], "system_firmware_progress"))
        n = IPMI_MONITORING_SENSOR_GROUP_SYSTEM_FIRMWARE_PROGRESS;
      else if (!strcasecmp(cmd_args->groups[i], "event_logging_disabled"))
        n = IPMI_MONITORING_SENSOR_GROUP_EVENT_LOGGING_DISABLED;
      else if (!strcasecmp(cmd_args->groups[i], "system_event"))
        n = IPMI_MONITORING_SENSOR_GROUP_SYSTEM_EVENT;
      else if (!strcasecmp(cmd_args->groups[i], "critical_interrupt"))
        n = IPMI_MONITORING_SENSOR_GROUP_CRITICAL_INTERRUPT;
      else if (!strcasecmp(cmd_args->groups[i], "module_board"))
        n = IPMI_MONITORING_SENSOR_GROUP_MODULE_BOARD;
      else if (!strcasecmp(cmd_args->groups[i], "slot_connector"))
        n = IPMI_MONITORING_SENSOR_GROUP_SLOT_CONNECTOR;
      else if (!strcasecmp(cmd_args->groups[i], "watchdog2"))
        n = IPMI_MONITORING_SENSOR_GROUP_WATCHDOG2;
      else if (!strcasecmp(cmd_args->groups[i], "entity_presence"))
        n = IPMI_MONITORING_SENSOR_GROUP_ENTITY_PRESENCE;
      else if (!strcasecmp(cmd_args->groups[i], "management_subsystem_health"))
        n = IPMI_MONITORING_SENSOR_GROUP_MANAGEMENT_SUBSYSTEM_HEALTH;
      else if (!strcasecmp(cmd_args->groups[i], "battery"))
        n = IPMI_MONITORING_SENSOR_GROUP_BATTERY;
      else if (!strcasecmp(cmd_args->groups[i], "fru_state"))
        n = IPMI_MONITORING_SENSOR_GROUP_FRU_STATE;
      else if (!strcasecmp(cmd_args->groups[i], "cable_interconnect"))
        n = IPMI_MONITORING_SENSOR_GROUP_CABLE_INTERCONNECT;
      else if (!strcasecmp(cmd_args->groups[i], "boot_error"))
        n = IPMI_MONITORING_SENSOR_GROUP_BOOT_ERROR;
      else if (!strcasecmp(cmd_args->groups[i], "button_switch"))
        n = IPMI_MONITORING_SENSOR_GROUP_BUTTON_SWITCH;
      else if (!strcasecmp(cmd_args->groups[i], "system_acpi_power_state"))
        n = IPMI_MONITORING_SENSOR_GROUP_SYSTEM_ACPI_POWER_STATE;
      else
        {
          fprintf(stderr, "invalid sensor group '%s'\n", cmd_args->groups[i]);
          exit(1);
        }
      
      if (n >= 0)
        {
          cmd_args->ipmimonitoring_groups[cmd_args->ipmimonitoring_groups_length] = n;
          cmd_args->ipmimonitoring_groups_length++;
        }
    }
}

int
main(int argc, char **argv)
{
  ipmimonitoring_prog_data_t prog_data;
  struct ipmimonitoring_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump();

  memset(&prog_data, '\0', sizeof(ipmimonitoring_prog_data_t));
  prog_data.progname = argv[0];
  ipmimonitoring_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  _grab_ipmimonitoring_options(&cmd_args);

  if (_setup_ipmimonitoring_lib (&cmd_args) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((hosts_count = pstdout_setup(&(prog_data.args->common.hostname),
                                   prog_data.args->hostrange.buffer_output,
                                   prog_data.args->hostrange.consolidate_output,
                                   prog_data.args->hostrange.fanout,
                                   prog_data.args->hostrange.eliminate,
                                   prog_data.args->hostrange.always_prefix)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->sdr.quiet_cache = 1;

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
                           _ipmimonitoring,
                           &prog_data)) < 0)
    {
      fprintf(stderr,
              "pstdout_launch: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = rv;
 cleanup:
  return (exit_code);
}

