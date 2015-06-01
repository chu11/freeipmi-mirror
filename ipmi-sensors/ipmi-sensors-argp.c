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

#include "ipmi-sensors.h"
#include "ipmi-sensors-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"
#include "tool-sensor-common.h"

const char *argp_program_version =
  "ipmi-sensors - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2014 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-sensors - displays IPMI sensor information";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_CACHE_OPTIONS,
    ARGP_COMMON_SDR_CACHE_OPTIONS_FILE_DIRECTORY,
    ARGP_COMMON_TIME_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "verbose",        VERBOSE_KEY,        0, 0,
      "Increase verbosity in output.  May be specified multiple times.", 40},
    { "sdr-info",       SDR_INFO_KEY,       0, 0,
      "Show sendor data repository (SDR) information.", 41},
    { "quiet-readings", QUIET_READINGS_KEY,  0, 0,
      "Do not output sensor readings or thresholds on simple output.", 42},
    /* for backwards compatability */
    { "sensors",        SENSORS_KEY, "SENSORS-LIST", OPTION_HIDDEN,
      "Show sensors by record id.  Accepts space or comma separated lists", 43},
    { "record-ids",     RECORD_IDS_KEY, "RECORD-IDS-LIST", 0,
      "Show specific sensors by record id.  Accepts space or comma separated lists", 44},
    { "exclude-record-ids", EXCLUDE_RECORD_IDS_KEY, "RECORD-IDS-LIST", 0,
      "Do not show specific sensors by record id.  Accepts space or comma separated lists", 45},
    /* maintain "group" options for backwards compatability */
    { "group",          GROUP_KEY,        "GROUP-NAME", OPTION_HIDDEN,
      "Show sensors belonging to a specific group.", 46},
    /* maintain "group" options for backwards compatability */
    { "groups",         GROUPS_KEY,       "GROUPS-LIST", OPTION_HIDDEN,
      "Show sensors belonging to a specific group.", 47},
    /* maintain "group" options for backwards compatability */
    { "exclude-groups", EXCLUDE_GROUPS_KEY, "GROUPS-LIST", OPTION_HIDDEN,
      "Do not show sensors belonging to a specific group.", 48},
    /* maintain "group" options for backwards compatability */
    { "list-groups",    LIST_GROUPS_KEY, 0, OPTION_HIDDEN,
      "List sensor groups.", 49},
    /* for backwards compatability */
    { "sensor-type",    SENSOR_TYPE_KEY,        "SENSOR-TYPE-NAME", OPTION_HIDDEN,
      "Show sensors of a specific type.", 50},
    { "sensor-types",   SENSOR_TYPES_KEY,       "SENSOR-TYPES-LIST", 0,
      "Show sensors of a specific type.", 51},
    { "exclude-sensor-types", EXCLUDE_SENSOR_TYPES_KEY, "SENSOR-TYPES-LIST", 0,
      "Do not show sensors of a specific type.", 52},
    { "list-sensor-types",    LIST_SENSOR_TYPES_KEY, 0, 0,
      "List sensor types.", 53},
    { "bridge-sensors", BRIDGE_SENSORS_KEY, NULL, 0,
      "Bridge addresses to read non-BMC owned sensors.", 54},
    { "shared-sensors", SHARED_SENSORS_KEY, NULL, 0,
      "Iterate over shared sensors in a single record.", 55},
    { "interpret-oem-data", INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 56},
    { "ignore-not-available-sensors", IGNORE_NOT_AVAILABLE_SENSORS_KEY, NULL, 0,
      "Ignore not-available (i.e. N/A) sensors.", 57},
    { "ignore-unrecognized-events", IGNORE_UNRECOGNIZED_EVENTS_KEY, NULL, 0,
      "Ignore unrecognized events (i.e. 'Unrecognized Event') in sensors output.", 58},
    { "output-event-bitmask", OUTPUT_EVENT_BITMASK_KEY, NULL, 0,
      "Output event bitmask value instead of the string representation.", 59},
    { "output-sensor-state", OUTPUT_SENSOR_STATE_KEY, NULL, 0,
      "Output sensor state in output.", 60},
    { "sensor-state-config-file", SENSOR_STATE_CONFIG_FILE_KEY, "FILE", 0,
      "Specify an alternate sensor state configuration file.", 61},
    /* ipmimonitoring legacy support */
    { "sensor-config-file", SENSOR_STATE_CONFIG_FILE_KEY, "FILE", OPTION_HIDDEN,
      "Specify an alternate sensor state configuration  file.", 62},
    { "entity-sensor-names", ENTITY_SENSOR_NAMES_KEY, NULL, 0,
      "Output sensor names with entity ids and instances.", 63},
    { "output-sensor-thresholds", OUTPUT_SENSOR_THRESHOLDS_KEY, NULL, 0,
      "Output sensor thresholds in output.", 64},
    { "no-sensor-type-output", NO_SENSOR_TYPE_OUTPUT_KEY, 0, 0,
      "Do not show sensor type output.", 65},
    { "comma-separated-output", COMMA_SEPARATED_OUTPUT_KEY, 0, 0,
      "Output fields in comma separated format.", 66},
    { "no-header-output", NO_HEADER_OUTPUT_KEY, 0, 0,
      "Do not output column headers.", 67},
    { "non-abbreviated-units", NON_ABBREVIATED_UNITS_KEY, 0, 0,
      "Output non-abbreviated units (e.g. 'Amps' insetead of 'A').", 68},
    { "legacy-output", LEGACY_OUTPUT_KEY, 0, 0,
      "Output in legacy format.", 69},
    /* ipmimonitoring legacy support */
    { "ipmimonitoring-legacy-output", IPMIMONITORING_LEGACY_OUTPUT_KEY, 0, 0,
      "Output in ipmimonitoring legacy format.", 70},
    { NULL, 0, NULL, 0, NULL, 0}
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
  struct ipmi_sensors_arguments *cmd_args;
  char *endptr;
  char *tok;
  int value;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
    case SDR_INFO_KEY:
      cmd_args->sdr_info = 1;
      break;
      /* maintain -s and "--sensors" for backwards compatability */
    case QUIET_READINGS_KEY:
      cmd_args->quiet_readings = 1;
      break;
    case SENSORS_KEY:
    case RECORD_IDS_KEY:
      tok = strtok (arg, " ,");
      while (tok && cmd_args->record_ids_length < MAX_SENSOR_RECORD_IDS)
        {
          if (!strcasecmp (tok, SENSOR_PARSE_ALL_STRING))
            {
              cmd_args->record_ids_length = 0;
              break;
            }

          errno = 0;
          value = strtol (tok, &endptr, 10);

          if (errno
              || endptr[0] != '\0'
              || value < 0
              || value < IPMI_SDR_RECORD_ID_FIRST
              || value > IPMI_SDR_RECORD_ID_LAST)
            {
              fprintf (stderr, "invalid sensor record id: %d\n", value);
              exit (EXIT_FAILURE);
            }

          cmd_args->record_ids[cmd_args->record_ids_length] = value;
          cmd_args->record_ids_length++;
          tok = strtok (NULL, " ,");
        }
      break;
    case EXCLUDE_RECORD_IDS_KEY:
      tok = strtok (arg, " ,");
      while (tok && cmd_args->exclude_record_ids_length < MAX_SENSOR_RECORD_IDS)
        {
          if (!strcasecmp (tok, SENSOR_PARSE_NONE_STRING))
            {
              cmd_args->exclude_record_ids_length = 0;
              break;
            }

          errno = 0;
          value = strtol (tok, &endptr, 10);

          if (errno
              || endptr[0] != '\0'
              || value < 0
              || value < IPMI_SDR_RECORD_ID_FIRST
              || value > IPMI_SDR_RECORD_ID_LAST)
            {
              fprintf (stderr, "invalid sensor record id: %d\n", value);
              exit (EXIT_FAILURE);
            }

          cmd_args->exclude_record_ids[cmd_args->exclude_record_ids_length] = value;
          cmd_args->exclude_record_ids_length++;
          tok = strtok (NULL, " ,");
        }
      break;
    case GROUP_KEY:             /* legacy */
    case SENSOR_TYPE_KEY:
      strncpy (cmd_args->sensor_types[cmd_args->sensor_types_length],
               arg,
               MAX_SENSOR_TYPES_STRING_LENGTH);
      cmd_args->sensor_types_length++;
      break;
    case GROUPS_KEY:            /* legacy */
    case SENSOR_TYPES_KEY:
      if (parse_sensor_types (SENSOR_PARSE_ALL_STRING,
			      cmd_args->sensor_types,
			      &(cmd_args->sensor_types_length),
			      arg) < 0)
	exit (EXIT_FAILURE);
      break;
    case EXCLUDE_GROUPS_KEY:    /* legacy */
    case EXCLUDE_SENSOR_TYPES_KEY:
      if (parse_sensor_types (SENSOR_PARSE_NONE_STRING,
			      cmd_args->exclude_sensor_types,
			      &(cmd_args->exclude_sensor_types_length),
			      arg) < 0)
	exit (EXIT_FAILURE);
      break;
    case LIST_GROUPS_KEY:       /* legacy */
    case LIST_SENSOR_TYPES_KEY:
      cmd_args->list_sensor_types = 1;
      break;
    case BRIDGE_SENSORS_KEY:
      cmd_args->bridge_sensors = 1;
      break;
    case SHARED_SENSORS_KEY:
      cmd_args->shared_sensors = 1;
      break;
    case INTERPRET_OEM_DATA_KEY:
      cmd_args->interpret_oem_data = 1;
      break;
    case IGNORE_NOT_AVAILABLE_SENSORS_KEY:
      cmd_args->ignore_not_available_sensors = 1;
      break;
    case IGNORE_UNRECOGNIZED_EVENTS_KEY:
      cmd_args->ignore_unrecognized_events = 1;
      break;
    case OUTPUT_EVENT_BITMASK_KEY:
      cmd_args->output_event_bitmask = 1;
      break;
    case OUTPUT_SENSOR_STATE_KEY:
      cmd_args->output_sensor_state = 1;
      break;
    case SENSOR_STATE_CONFIG_FILE_KEY:
      if (!(cmd_args->sensor_state_config_file = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case ENTITY_SENSOR_NAMES_KEY:
      cmd_args->entity_sensor_names = 1;
      break;
    case OUTPUT_SENSOR_THRESHOLDS_KEY:
      cmd_args->output_sensor_thresholds = 1;
      break;
    case NO_SENSOR_TYPE_OUTPUT_KEY:
      cmd_args->no_sensor_type_output = 1;
      break;
    case COMMA_SEPARATED_OUTPUT_KEY:
      cmd_args->comma_separated_output = 1;
      break;
    case NO_HEADER_OUTPUT_KEY:
      cmd_args->no_header_output = 1;
      break;
    case NON_ABBREVIATED_UNITS_KEY:
      cmd_args->non_abbreviated_units = 1;
      break;
    case LEGACY_OUTPUT_KEY:
      cmd_args->legacy_output = 1;
      break;
    case IPMIMONITORING_LEGACY_OUTPUT_KEY:
      cmd_args->ipmimonitoring_legacy_output = 1;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return (common_parse_opt (key, arg, &(cmd_args->common_args)));
    }

  return (0);
}

static void
_ipmi_sensors_config_file_parse (struct ipmi_sensors_arguments *cmd_args)
{
  struct config_file_data_ipmi_sensors config_file_data;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_sensors));

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_TIME | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_SENSORS,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->verbose_count = config_file_data.verbose_count;
  if (config_file_data.quiet_readings_count)
    cmd_args->quiet_readings = config_file_data.quiet_readings;
  if (config_file_data.record_ids_count && config_file_data.record_ids_length)
    {
      unsigned int i;

      assert (MAX_SENSOR_RECORD_IDS == CONFIG_FILE_MAX_SENSOR_RECORD_IDS);

      for (i = 0; i < config_file_data.record_ids_length; i++)
        cmd_args->record_ids[i] = config_file_data.record_ids[i];
      cmd_args->record_ids_length = config_file_data.record_ids_length;
    }
  if (config_file_data.exclude_record_ids_count && config_file_data.exclude_record_ids_length)
    {
      unsigned int i;

      assert (MAX_SENSOR_RECORD_IDS == CONFIG_FILE_MAX_SENSOR_RECORD_IDS);

      for (i = 0; i < config_file_data.exclude_record_ids_length; i++)
        cmd_args->exclude_record_ids[i] = config_file_data.exclude_record_ids[i];
      cmd_args->exclude_record_ids_length = config_file_data.exclude_record_ids_length;
    }
  if (config_file_data.sensor_types_count && config_file_data.sensor_types_length)
    {
      unsigned int i;

      assert(MAX_SENSOR_TYPES == CONFIG_FILE_MAX_SENSOR_TYPES);
      assert(MAX_SENSOR_TYPES_STRING_LENGTH == CONFIG_FILE_MAX_SENSOR_TYPES_STRING_LENGTH);

      for (i = 0; i < config_file_data.sensor_types_length; i++)
        strncpy (cmd_args->sensor_types[i],
                 config_file_data.sensor_types[i],
                 MAX_SENSOR_TYPES_STRING_LENGTH);
      cmd_args->sensor_types_length = config_file_data.sensor_types_length;
    }
  if (config_file_data.exclude_sensor_types_count && config_file_data.exclude_sensor_types_length)
    {
      unsigned int i;

      assert(MAX_SENSOR_TYPES == CONFIG_FILE_MAX_SENSOR_TYPES);
      assert(MAX_SENSOR_TYPES_STRING_LENGTH == CONFIG_FILE_MAX_SENSOR_TYPES_STRING_LENGTH);

      for (i = 0; i < config_file_data.exclude_sensor_types_length; i++)
        strncpy (cmd_args->exclude_sensor_types[i],
                 config_file_data.exclude_sensor_types[i],
                 MAX_SENSOR_TYPES_STRING_LENGTH);
      cmd_args->exclude_sensor_types_length = config_file_data.exclude_sensor_types_length;
    }
  if (config_file_data.bridge_sensors_count)
    cmd_args->bridge_sensors = config_file_data.bridge_sensors;
  if (config_file_data.shared_sensors_count)
    cmd_args->shared_sensors = config_file_data.shared_sensors;
  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
  if (config_file_data.ignore_not_available_sensors_count)
    cmd_args->ignore_not_available_sensors = config_file_data.ignore_not_available_sensors;
  if (config_file_data.ignore_unrecognized_events_count)
    cmd_args->ignore_unrecognized_events = config_file_data.ignore_unrecognized_events;
  if (config_file_data.output_event_bitmask_count)
    cmd_args->output_event_bitmask = config_file_data.output_event_bitmask;
  if (config_file_data.output_sensor_state_count)
    cmd_args->output_sensor_state = config_file_data.output_sensor_state;
  if (config_file_data.sensor_state_config_file_count)
    cmd_args->sensor_state_config_file = config_file_data.sensor_state_config_file;
  if (config_file_data.entity_sensor_names_count)
    cmd_args->entity_sensor_names = config_file_data.entity_sensor_names;
  if (config_file_data.output_sensor_thresholds_count)
    cmd_args->output_sensor_thresholds = config_file_data.output_sensor_thresholds;
  if (config_file_data.no_sensor_type_output_count)
    cmd_args->no_sensor_type_output = config_file_data.no_sensor_type_output;
  if (config_file_data.comma_separated_output_count)
    cmd_args->comma_separated_output = config_file_data.comma_separated_output;
  if (config_file_data.no_header_output_count)
    cmd_args->no_header_output = config_file_data.no_header_output;
  if (config_file_data.non_abbreviated_units_count)
    cmd_args->non_abbreviated_units = config_file_data.non_abbreviated_units;
  if (config_file_data.legacy_output_count)
    cmd_args->legacy_output = config_file_data.legacy_output;
  if (config_file_data.ipmimonitoring_legacy_output_count)
    cmd_args->ipmimonitoring_legacy_output = config_file_data.ipmimonitoring_legacy_output;
}

static void
_ipmi_sensors_args_validate (struct ipmi_sensors_arguments *cmd_args)
{
  assert (cmd_args);

  if (cmd_args->sensor_types_length)
    {
      if (valid_sensor_types (cmd_args->sensor_types,
                              cmd_args->sensor_types_length) < 0)
        exit (EXIT_FAILURE);
    }

  if (cmd_args->exclude_sensor_types_length)
    {
      if (valid_sensor_types (cmd_args->exclude_sensor_types,
                              cmd_args->exclude_sensor_types_length) < 0)
        exit (EXIT_FAILURE);
    }
}

void
ipmi_sensors_argp_parse (int argc, char **argv, struct ipmi_sensors_arguments *cmd_args)
{
  unsigned int i;

  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_operator (&(cmd_args->common_args));

  cmd_args->verbose_count = 0;
  cmd_args->sdr_info = 0;
  cmd_args->quiet_readings = 0;

  memset (cmd_args->record_ids,
          '\0',
          sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
  cmd_args->record_ids_length = 0;

  memset (cmd_args->exclude_record_ids,
          '\0',
          sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
  cmd_args->exclude_record_ids_length = 0;

  for (i = 0; i < MAX_SENSOR_TYPES; i++)
    memset (cmd_args->sensor_types[i],
            '\0',
            MAX_SENSOR_TYPES_STRING_LENGTH+1);
  cmd_args->sensor_types_length = 0;

  for (i = 0; i < MAX_SENSOR_TYPES; i++)
    memset (cmd_args->exclude_sensor_types[i],
            '\0',
            MAX_SENSOR_TYPES_STRING_LENGTH+1);
  cmd_args->exclude_sensor_types_length = 0;

  cmd_args->list_sensor_types = 0;

  cmd_args->bridge_sensors = 0;
  cmd_args->shared_sensors = 0;
  cmd_args->interpret_oem_data = 0;
  cmd_args->ignore_not_available_sensors = 0;
  cmd_args->ignore_unrecognized_events = 0;
  cmd_args->output_event_bitmask = 0;
  cmd_args->output_sensor_state = 0;
  cmd_args->sensor_state_config_file = NULL;
  cmd_args->entity_sensor_names = 0;
  cmd_args->output_sensor_thresholds = 0;
  cmd_args->no_sensor_type_output = 0;
  cmd_args->comma_separated_output = 0;
  cmd_args->no_header_output = 0;
  cmd_args->non_abbreviated_units = 0;
  cmd_args->legacy_output = 0;
  cmd_args->ipmimonitoring_legacy_output = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _ipmi_sensors_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _ipmi_sensors_args_validate (cmd_args);
}
