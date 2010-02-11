/*****************************************************************************\
 *  $Id: ipmimonitoring-sensors-argp.c,v 1.1.2.5 2010-02-11 19:31:56 chu11 Exp $
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmimonitoring-sensors.h"
#include "ipmimonitoring-sensors-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmimonitoring-sensors - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2010 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2006-2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmimonitoring-sensors - test utility for libipmimonitoring sensors monitoring";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "record-ids",     RECORD_IDS_KEY, "RECORD-IDS-LIST", 0,
      "Show specific sensors by record id.  Accepts space or comma separated lists", 30},
    { "sensor-types",   SENSOR_TYPES_KEY,  "SENSOR-TYPE-LIST", 0,
      "Show sensors of a specific type.", 31},
    { "reread-sdr-cache", REREAD_SDR_CACHE_KEY, NULL, 0,
      "Re-Read SDR cache.", 32},
    { "bridge-sensors", BRIDGE_SENSORS_KEY, NULL, 0,
      "Bridge addresses to read non-BMC owned sensors.", 33},
    { "shared-sensors", SHARED_SENSORS_KEY, NULL, 0,
      "Iterate over shared sensors in a single record.", 34},
    { "interpret-oem-data", INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 35},
    { "ignore-non-interpretable-sensors", IGNORE_NON_INTERPRETABLE_SENSORS_KEY, NULL, 0,
      "Ignore non-interpretable sensors in output.", 36},
    { "sensor-config-file", SENSOR_CONFIG_FILE_KEY, "FILE", 0,
      "Specify an alternate sensor configuration file.", 37},
    { NULL, 0, NULL, 0, NULL, 0}
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc };

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmimonitoring_sensors_arguments *cmd_args = state->input;
  char *ptr;
  char *tok;
  int value;
  error_t ret;

  switch (key)
    {
    case RECORD_IDS_KEY:
      tok = strtok (arg, " ,");
      while (tok && cmd_args->record_ids_length < MAX_SENSOR_RECORD_IDS)
        {
          value = 0;
          ptr = NULL;
          errno = 0;

          value = strtol (tok, &ptr, 10);

          if (errno
              || ptr[0] != '\0'
              || value < 0
              || value < IPMI_SDR_RECORD_ID_FIRST
              || value > IPMI_SDR_RECORD_ID_LAST)
            {
              fprintf (stderr, "invalid sensor record id: %d\n", value);
              exit (1);
            }

          cmd_args->record_ids[cmd_args->record_ids_length] = value;
          cmd_args->record_ids_length++;
          tok = strtok (NULL, " ,");
        }
      break;
    case SENSOR_TYPES_KEY:
      tok = strtok (arg, " ,");
      while (tok && cmd_args->sensor_types_length < MAX_SENSOR_TYPES)
        {
          strncpy (cmd_args->sensor_types[cmd_args->sensor_types_length],
                   tok,
                   MAX_SENSOR_TYPES_STRING_LENGTH);
          cmd_args->sensor_types_length++;
          tok = strtok (NULL, " ,");
        }
      break;
    case REREAD_SDR_CACHE_KEY:
      cmd_args->reread_sdr_cache = 1;
      break;
    case IGNORE_NON_INTERPRETABLE_SENSORS_KEY:
      cmd_args->ignore_non_interpretable_sensors = 1;
      break;
    case BRIDGE_SENSORS_KEY:
      cmd_args->bridge_sensors = 1;
      break;
    case INTERPRET_OEM_DATA_KEY:
      cmd_args->interpret_oem_data = 1;
      break;
    case SHARED_SENSORS_KEY:
      cmd_args->shared_sensors = 1;
      break;
    case SENSOR_CONFIG_FILE_KEY:
      if (cmd_args->sensor_config_file)
        free (cmd_args->sensor_config_file);
      if (!(cmd_args->sensor_config_file = strdup (arg)))
        {
          perror ("strdup");
          exit (1);
        }
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      ret = common_parse_opt (key, arg, &(cmd_args->common));
      return (ret);
    }

  return (0);
}

void
ipmimonitoring_sensors_argp_parse (int argc, char **argv, struct ipmimonitoring_sensors_arguments *cmd_args)
{
  unsigned int i;

  init_common_cmd_args_operator (&(cmd_args->common));

  memset (cmd_args->record_ids,
          '\0',
          sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
  cmd_args->record_ids_length = 0;

  for (i = 0; i < MAX_SENSOR_TYPES; i++)
    memset (cmd_args->sensor_types[i],
            '\0',
            MAX_SENSOR_TYPES_STRING_LENGTH+1);
  cmd_args->sensor_types_length = 0;

  cmd_args->reread_sdr_cache = 0;
  cmd_args->bridge_sensors = 0;
  cmd_args->shared_sensors = 0;
  cmd_args->interpret_oem_data = 0;
  cmd_args->ignore_non_interpretable_sensors = 0;
  cmd_args->sensor_config_file = NULL;

  memset (&(cmd_args->conf), '\0', sizeof (struct ipmi_monitoring_ipmi_config));
  cmd_args->ipmimonitoring_flags = 0;

  memset (cmd_args->ipmimonitoring_sensor_types,
          '\0',
          sizeof (unsigned int) * MAX_SENSOR_TYPES);
  cmd_args->ipmimonitoring_sensor_types_length = 0;

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common));
}


