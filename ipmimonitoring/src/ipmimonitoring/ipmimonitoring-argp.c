/*****************************************************************************\
 *  $Id: ipmimonitoring-argp.c,v 1.40 2009-03-17 20:12:59 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
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

#include "ipmimonitoring.h"
#include "ipmimonitoring-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmimonitoring - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2006-2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmimonitoring - IPMI monitoring utility";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_OPERATOR,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "verbose", VERBOSE_KEY, 0, 0,
      "Increase verbosity in output.", 30},
    /* maintain "cache-dir" for backwards compatability */
    { "cache-dir", CACHE_DIR_KEY, "DIRECTORY", OPTION_HIDDEN,
      "Specify an alternate directory to read and write SDR caches..", 31},
    { "quiet-readings", QUIET_READINGS_KEY,  0, 0,
      "Do not output sensor readings, only states.", 32},
    { "list-groups",    LIST_GROUPS_KEY,    0, 0,
      "List sensor groups.", 33},
    { "groups",         GROUPS_KEY,       "GROUP-LIST", 0,
      "Show sensors belonging to a specific group.", 34},
    /* for backwards compatability */
    { "sensors",        SENSORS_KEY, "SENSORS-LIST", OPTION_HIDDEN,
      "Show sensors by record id.  Accepts space or comma separated lists", 35},
    { "record-ids",     RECORD_IDS_KEY, "RECORD-IDS-LIST", 0,
      "Show sensors by record id.  Accepts space or comma separated lists", 36},
    { "bridge-sensors", BRIDGE_SENSORS_KEY, NULL, 0,
      "Bridge addresses to read non-BMC owned sensors.", 37},
    { "comma-separated-output", COMMA_SEPARATED_OUTPUT_KEY, 0, 0,
      "Output fields in comma separated format.", 38},
    { "non-abbreviated-units", NON_ABBREVIATED_UNITS_KEY, 0, 0,
      "Output non-abbreviated units (i.e. 'Amps' instead of 'A').", 39},
    { "legacy-output", LEGACY_OUTPUT_KEY, 0, 0,
      "Output in legacy format.", 40},
    { "sensor-config-file", SENSOR_CONFIG_FILE_KEY, "FILE", 0,
      "Specify an alternate sensor configuration file.", 41},
    { 0 }
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc };

static struct argp cmdline_config_file_argp = { cmdline_options,
                                                cmdline_config_file_parse,
                                                cmdline_args_doc,
                                                cmdline_doc };

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmimonitoring_arguments *cmd_args = state->input;
  char *ptr;
  char *tok;
  int value;
  error_t ret;

  switch (key)
    {
    case VERBOSE_KEY:
      cmd_args->verbose = 1;
      break;
      /* legacy option */
    case CACHE_DIR_KEY:
      return (sdr_parse_opt (ARGP_SDR_CACHE_DIRECTORY_KEY,
                             arg,
                             state,
                             &(cmd_args->sdr)));
      break;
    case QUIET_READINGS_KEY:
      cmd_args->quiet_readings = 1;
      break;
    case LIST_GROUPS_KEY:
      cmd_args->list_groups = 1;
      break;
    case GROUPS_KEY:
      tok = strtok (arg, " ,");
      while (tok && cmd_args->groups_length < MAX_SENSOR_GROUPS)
        {
          strncpy (cmd_args->groups[cmd_args->groups_length],
                   tok,
                   MAX_SENSOR_GROUPS_STRING_LENGTH);
          cmd_args->groups_length++;
          tok = strtok (NULL, " ,");
        }
      break;
    case SENSORS_KEY:
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
    case BRIDGE_SENSORS_KEY:
      cmd_args->bridge_sensors = 1;
      break;
    case COMMA_SEPARATED_OUTPUT_KEY:
      cmd_args->comma_separated_output = 1;
      break;
    case NON_ABBREVIATED_UNITS_KEY:
      cmd_args->non_abbreviated_units = 1;
      break;
    case LEGACY_OUTPUT_KEY:
      cmd_args->legacy_output = 1;
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
      ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = sdr_parse_opt (key, arg, state, &(cmd_args->sdr));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->hostrange));
      return (ret);
    }

  return (0);
}

static void
_ipmimonitoring_config_file_parse (struct ipmimonitoring_arguments *cmd_args)
{
  struct config_file_data_ipmimonitoring config_file_data;

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmimonitoring));

  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         &(cmd_args->sdr),
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMIMONITORING,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (1);
    }

  if (config_file_data.quiet_readings_count)
    cmd_args->quiet_readings = config_file_data.quiet_readings;
  if (config_file_data.groups_count && config_file_data.groups_length)
    {
      int i;

      assert(MAX_SENSOR_GROUPS == CONFIG_FILE_MAX_SENSOR_GROUPS);
      assert(MAX_SENSOR_GROUPS_STRING_LENGTH == CONFIG_FILE_MAX_SENSOR_GROUPS_STRING_LENGTH);

      for (i = 0; i < config_file_data.groups_length; i++)
        strncpy (cmd_args->groups[i],
                 config_file_data.groups[i],
                 MAX_SENSOR_GROUPS_STRING_LENGTH);
      cmd_args->groups_length = config_file_data.groups_length;
    }
  if (config_file_data.record_ids_count && config_file_data.record_ids_count)
    {
      int i;

      assert (MAX_SENSOR_RECORD_IDS == CONFIG_FILE_MAX_SENSOR_RECORD_IDS);

      for (i = 0; i < config_file_data.record_ids_length; i++)
        cmd_args->record_ids[i] = config_file_data.record_ids[i];
      cmd_args->record_ids_length = config_file_data.record_ids_length;
    }
  if (config_file_data.bridge_sensors_count)
    cmd_args->bridge_sensors = config_file_data.bridge_sensors;
  if (config_file_data.comma_separated_output_count)
    cmd_args->comma_separated_output = config_file_data.comma_separated_output;
  if (config_file_data.non_abbreviated_units_count)
    cmd_args->non_abbreviated_units = config_file_data.non_abbreviated_units;
  if (config_file_data.legacy_output_count)
    cmd_args->legacy_output = config_file_data.legacy_output;
  if (config_file_data.sensor_config_file_count)
    cmd_args->sensor_config_file = config_file_data.sensor_config_file;
}

void
ipmimonitoring_argp_parse (int argc, char **argv, struct ipmimonitoring_arguments *cmd_args)
{
  int i;

  init_common_cmd_args_operator (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->verbose = 0;
  cmd_args->quiet_readings = 0;
  cmd_args->list_groups = 0;
  for (i = 0; i < MAX_SENSOR_GROUPS; i++)
    memset (cmd_args->groups[i],
            '\0',
            MAX_SENSOR_GROUPS_STRING_LENGTH+1);
  cmd_args->groups_length = 0;
  memset (cmd_args->record_ids,
          '\0',
          sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
  cmd_args->record_ids_length = 0;
  cmd_args->bridge_sensors = 0;
  cmd_args->comma_separated_output = 0;
  cmd_args->non_abbreviated_units = 0;
  cmd_args->legacy_output = 0;
  cmd_args->sensor_config_file = NULL;

  memset (&(cmd_args->conf), '\0', sizeof (struct ipmi_monitoring_ipmi_config));
  cmd_args->ipmimonitoring_flags = 0;
  memset (cmd_args->ipmimonitoring_groups,
          '\0',
          sizeof (unsigned int) * MAX_SENSOR_GROUPS);

  cmd_args->ipmimonitoring_groups_length = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common));

  _ipmimonitoring_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common));
  verify_sdr_cmd_args (&(cmd_args->sdr));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}


