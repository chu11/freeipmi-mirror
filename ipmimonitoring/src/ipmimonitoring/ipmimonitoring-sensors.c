/*****************************************************************************\
 *  $Id: ipmimonitoring-sensors.c,v 1.1.2.8 2010-02-11 21:43:59 chu11 Exp $
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#include <assert.h>
#include <errno.h>

#include "ipmi_monitoring.h"

#include "ipmimonitoring-sensors.h"
#include "ipmimonitoring-sensors-argp.h"

#include "freeipmi-portability.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"

static const char *
_get_sensor_type_string (int sensor_type)
{
  const char *sensor_type_str;

  /* make sure libfreeipmi API is consistent to libipmimonitoring */
  assert (IPMI_SENSOR_TYPE_TEMPERATURE == IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE);
  assert (IPMI_SENSOR_TYPE_FRU_STATE == IPMI_MONITORING_SENSOR_TYPE_FRU_STATE);

  if ((sensor_type_str = ipmi_get_sensor_type_string (sensor_type)))
    return (sensor_type_str);

  return ("Unrecognized");
}
 
static int
_ipmimonitoring (ipmimonitoring_sensors_prog_data_t *prog_data)
{
  struct ipmimonitoring_sensors_arguments *args;
  ipmi_monitoring_ctx_t ctx = NULL;
  unsigned int sensor_reading_flags = 0;
  unsigned int i;
  int sensor_count;
  int errnum;
  int exit_code;

  args = prog_data->args;

  if (ipmi_monitoring_init (args->ipmimonitoring_flags, &errnum) < 0)
    {
      fprintf (stderr,
               "ipmi_monitoring_init: %s\n",
               ipmi_monitoring_ctx_strerror (errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!(ctx = ipmi_monitoring_ctx_create ()))
    {
      perror ("ipmi_monitoring_ctx_create:");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (args->sdr_cache_directory)
    {
      if (ipmi_monitoring_ctx_sdr_cache_directory (ctx,
                                                   args->sdr_cache_directory) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_ctx_sdr_cache_directory: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (args->sensor_config_file)
    {
      if (ipmi_monitoring_ctx_sensor_config_file (ctx,
                                                  args->sensor_config_file) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_ctx_sensor_config_file: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
  
  if (args->reread_sdr_cache)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_REREAD_SDR_CACHE;

  if (args->ignore_non_interpretable_sensors)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS;
  
  if (args->bridge_sensors)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_BRIDGE_SENSORS;

  if (args->interpret_oem_data)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_INTERPRET_OEM_DATA;

  if (args->shared_sensors)
    sensor_reading_flags |= IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS;

  /* 
   * achu: configure per-situation workaround flags.  Can't be done
   * below in _convert_to_ipmimonitoring_options() b/c user could have
   * input localhost/127.0.0.1.
   */
  if (prog_data->args->common.hostname
      && strcasecmp (prog_data->args->common.hostname, "localhost")
      && strcmp (prog_data->args->common.hostname, "127.0.0.1"))
    {
      if (args->conf.protocol_version == IPMI_MONITORING_PROTOCOL_VERSION_2_0)
        {
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;
        }
      else
        {
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_INTEL_2_0_SESSION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_SUN_2_0_SESSION;
          if (args->common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE)
            args->conf.workaround_flags |= IPMI_MONITORING_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE;
        }
    }
  /* else - no inband workaround flags yet */

  if (!args->record_ids_length && !args->ipmimonitoring_sensor_types_length)
    {
      if ((sensor_count = ipmi_monitoring_sensor_readings_by_record_id (ctx,
                                                                        prog_data->args->common.hostname,
                                                                        &args->conf,
                                                                        sensor_reading_flags,
                                                                        NULL,
                                                                        0,
                                                                        NULL,
                                                                        NULL)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_readings_by_record_id: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
  else if (args->record_ids_length)
    {
      if ((sensor_count = ipmi_monitoring_sensor_readings_by_record_id (ctx,
                                                                        prog_data->args->common.hostname,
                                                                        &args->conf,
                                                                        sensor_reading_flags,
                                                                        args->record_ids,
                                                                        args->record_ids_length,
                                                                        NULL,
                                                                        NULL)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_readings_by_record_id: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
  else
    {
      if ((sensor_count = ipmi_monitoring_sensor_readings_by_sensor_type (ctx,
                                                                          prog_data->args->common.hostname,
                                                                          &args->conf,
                                                                          sensor_reading_flags,
                                                                          args->ipmimonitoring_sensor_types,
                                                                          args->ipmimonitoring_sensor_types_length,
                                                                          NULL,
                                                                          NULL)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_readings_by_sensor_type: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  printf ("%s, %s, %s, %s, %s, %s, %s, %s\n",
          "Record ID",
          "Sensor Name",
          "Sensor Type",
          "Sensor State",
          "Sensor Reading",
          "Sensor Units",
          "Sensor Event Bitmask",
          "Sensor Event String");

  for (i = 0; i < sensor_count; i++, ipmi_monitoring_sensor_iterator_next (ctx))
    {
      int record_id, sensor_type, sensor_state, sensor_units,
        sensor_reading_type, sensor_bitmask_type, sensor_bitmask;
      char **sensor_bitmask_strings = NULL;
      const char *sensor_type_str;
      const char *sensor_state_str;
      char *sensor_name = NULL;
      void *sensor_reading;

      if ((record_id = ipmi_monitoring_sensor_read_record_id (ctx)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_record_id: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if ((sensor_type = ipmi_monitoring_sensor_read_sensor_type (ctx)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_sensor_type: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (!(sensor_name = ipmi_monitoring_sensor_read_sensor_name (ctx)))
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_sensor_name: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if ((sensor_state = ipmi_monitoring_sensor_read_sensor_state (ctx)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_sensor_state: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if ((sensor_units = ipmi_monitoring_sensor_read_sensor_units (ctx)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_sensor_units: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if ((sensor_bitmask_type = ipmi_monitoring_sensor_read_sensor_bitmask_type (ctx)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_sensor_bitmask_type: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if ((sensor_bitmask = ipmi_monitoring_sensor_read_sensor_bitmask (ctx)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_sensor_bitmask: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      sensor_bitmask_strings = ipmi_monitoring_sensor_read_sensor_bitmask_strings (ctx);

      if ((sensor_reading_type = ipmi_monitoring_sensor_read_sensor_reading_type (ctx)) < 0)
        {
          fprintf (stderr,
                   "ipmi_monitoring_sensor_read_sensor_reading_type: %s\n",
                   ipmi_monitoring_ctx_errormsg (ctx));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      sensor_reading = ipmi_monitoring_sensor_read_sensor_reading (ctx);

      if (!sensor_name
          || !strlen (sensor_name))
        sensor_name = "N/A";

      sensor_type_str = _get_sensor_type_string (sensor_type);

      printf ("%u, %s, %s",
              record_id,
              sensor_name,
              sensor_type_str);

      if (sensor_state == IPMI_MONITORING_SENSOR_STATE_NOMINAL)
        sensor_state_str = "Nominal";
      else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_WARNING)
        sensor_state_str = "Warning";
      else if (sensor_state == IPMI_MONITORING_SENSOR_STATE_CRITICAL)
        sensor_state_str = "Critical";
      else
        sensor_state_str = "N/A";

      printf (", %s", sensor_state_str);

      if (sensor_reading)
        {
          const char *sensor_units_str;

          if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
            printf (", %s",
                    (*((uint8_t *)sensor_reading) ? "true" : "false"));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
            printf (", %u",
                    *((uint32_t *)sensor_reading));
          else if (sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
            printf (", %.2f",
                    *((double *)sensor_reading));
          else
            printf (", N/A");

          if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_CELSIUS)
            sensor_units_str = ipmi_sensor_units_abbreviated[IPMI_SENSOR_UNIT_DEGREES_C];
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT)
            sensor_units_str = ipmi_sensor_units_abbreviated[IPMI_SENSOR_UNIT_DEGREES_F];
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_VOLTS)
            sensor_units_str = ipmi_sensor_units_abbreviated[IPMI_SENSOR_UNIT_VOLTS];
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_AMPS)
            sensor_units_str = ipmi_sensor_units_abbreviated[IPMI_SENSOR_UNIT_AMPS];
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_RPM)
            sensor_units_str = ipmi_sensor_units_abbreviated[IPMI_SENSOR_UNIT_RPM];
          else if (sensor_units == IPMI_MONITORING_SENSOR_UNITS_WATTS)
            sensor_units_str = ipmi_sensor_units_abbreviated[IPMI_SENSOR_UNIT_WATTS];
          else
            sensor_units_str = "N/A";

          printf (", %s", sensor_units_str);
        }
      else
        printf (", N/A, N/A");
  
      if (sensor_bitmask_type != IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN)
        printf (", %Xh", sensor_bitmask);
      else
        printf (", N/A");
  
      if (sensor_bitmask_type != IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN)
        {     
          if (sensor_bitmask_strings)
            {
              unsigned int i = 0;
          
              printf (",");
      
              while (sensor_bitmask_strings[i])
                {
                  printf (" ");
              
                  printf ("'%s'",
                          sensor_bitmask_strings[i]);
              
                  i++;
                }
            }
          else
            printf (", 'OK'");
        }
      else
        printf (", N/A");

      printf ("\n");
    }

  exit_code = 0;
 cleanup:
  if (ctx)
    ipmi_monitoring_ctx_destroy (ctx);
  return (exit_code);
}

#if 0
static int
_convert_to_ipmimonitoring_sensor_type_str (const char *sensor_type_str)
{
  unsigned int i;

  assert (sensor_type_str);

  /* make sure API is consistent to libipmimonitoring */
  assert (IPMI_SENSOR_TYPE_TEMPERATURE == IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE);
  assert (IPMI_SENSOR_TYPE_FRU_STATE == IPMI_MONITORING_SENSOR_TYPE_FRU_STATE);

  for (i = IPMI_SENSOR_TYPE_TEMPERATURE; i <= IPMI_SENSOR_TYPE_FRU_STATE; i++)
    {
      if (sensor_type_strcmp (NULL,
			      sensor_type_str,
			      i) == 1)
	return (i);
    }
  
  fprintf (stderr, "invalid sensor type '%s'\n", sensor_type_str);
  exit (1);
}
#endif

/* For some ipmimonitoring library functions, we need to convert
 * cmd_args struct into the ipmimonitoring library equivalent
 * structs.
 */
static void
_convert_to_ipmimonitoring_options (struct ipmimonitoring_sensors_arguments *cmd_args)
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

#if 0
  for (i = 0; i < cmd_args->sensor_types_length; i++)
    {
      int n;

      n = _convert_to_ipmimonitoring_sensor_type_str (cmd_args->sensor_types[i]);
      cmd_args->ipmimonitoring_sensor_types[cmd_args->ipmimonitoring_sensor_types_length] = n;
      cmd_args->ipmimonitoring_sensor_types_length++;
    }
#endif
}

int
main (int argc, char **argv)
{
  ipmimonitoring_sensors_prog_data_t prog_data;
  struct ipmimonitoring_sensors_arguments cmd_args;
  int exit_code;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmimonitoring_sensors_prog_data_t));
  prog_data.progname = argv[0];
  ipmimonitoring_sensors_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  _convert_to_ipmimonitoring_options (&cmd_args);

  exit_code = _ipmimonitoring (&prog_data);

  return (exit_code);
}

