/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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

#include "ipmi-sel.h"
#include "ipmi-sel-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-sel - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2010 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-sel - display SEL entries";

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
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_IGNORE_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "verbose",    VERBOSE_KEY,    0, 0,
      "Increase verbosity in output.", 30},
    { "info",       INFO_KEY,       0, 0,
      "Show general information about the SEL.", 31},
    { "display",     DISPLAY_KEY,     "REC-LIST", 0,
      "Display SEL records by record ids.", 32},
    { "exclude-display", EXCLUDE_DISPLAY_KEY, "REC-LIST", 0,
      "Exclude display of SEL records by record ids.", 33},
    { "display-range", DISPLAY_RANGE_KEY, "START-END", 0,
      "Display SEL records from record id START to END.", 34},
    { "exclude-display-range", EXCLUDE_DISPLAY_RANGE_KEY, "START-END", 0,
      "Exclude display of SEL records from record id START to END.", 35},
    { "tail", TAIL_KEY, "COUNT", 0,
      "Display approximately the last count SEL records.", 36},
    { "clear", CLEAR_KEY, 0, 0,
      "Clear SEL.", 37},
    /* legacy */
    { "delete-all", DELETE_ALL_KEY, 0, OPTION_HIDDEN,
      "Delete all SEL records.", 38},
    { "delete",     DELETE_KEY,     "REC-LIST", 0,
      "Delete SEL records by record ids.", 39},
    { "delete-range", DELETE_RANGE_KEY, "START-END", 0,
      "Delete record ids from START to END in the SEL.", 40},
    { "system-event-only", SYSTEM_EVENT_ONLY_KEY, 0, 0,
      "Output only system event records (i.e. don't output OEM records).", 41},
    { "oem-event-only", OEM_EVENT_ONLY_KEY, 0, 0,
      "Output only OEM event records.", 42},
    { "hex-dump",   HEX_DUMP_KEY, 0, 0,
      "Hex-dump SEL records.", 43},
    { "assume-system-event-records", ASSUME_SYSTEM_EVENT_RECORDS_KEY, 0, 0,
      "Assume invalid record types are system event records.", 44},
    { "interpret-oem-data", INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 45},
    { "entity-sensor-names", ENTITY_SENSOR_NAMES_KEY, NULL, 0,
      "Output sensor names with entity ids and instances.", 46},
    { "no-sensor-type-output", NO_SENSOR_TYPE_OUTPUT_KEY, 0, 0,
      "Do not show sensor type output.", 47},
    { "comma-separated-output", COMMA_SEPARATED_OUTPUT_KEY, 0, 0,
      "Output fields in comma separated format.", 48},
    { "no-header-output", NO_HEADER_OUTPUT_KEY, 0, 0,
      "Do not output column headers.", 49},
    { "non-abbreviated-units", NON_ABBREVIATED_UNITS_KEY, 0, 0,
      "Output non-abbreviated units (e.g. 'Amps' instead of 'A').", 50},
    { "legacy-output", LEGACY_OUTPUT_KEY, 0, 0,
      "Output in legacy format.", 51},
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

static void
_read_record_list (int *flag,
                   uint16_t *record_list,
                   unsigned int *record_list_length,
                   char *arg)
{
  char *ptr;
  char *tok;
  int value;

  assert (flag);
  assert (record_list);
  assert (record_list_length);
  assert (arg);

  (*flag) = 1;
  tok = strtok (arg, " ,");
  while (tok && (*record_list_length) < IPMI_SEL_MAX_RECORD)
    {
      value = 0;
      ptr = NULL;
      errno = 0;

      value = strtol (tok, &ptr, 10);

      if (errno
          || ptr[0] != '\0'
          || value < 0
          || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
          || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
        {
          fprintf (stderr, "invalid record number: %d\n", value);
          exit (1);
        }

      record_list[(*record_list_length)] = value;
      (*record_list_length)++;
      tok = strtok (NULL, " ,");
    }
}

static void
_read_range (int *flag,
             uint16_t *range1,
             uint16_t *range2,
             char *arg)
{
  char *ptr;
  char *range_str = NULL;
  char *start_ptr = NULL;
  char *range1_str = NULL;
  char *range2_str = NULL;
  int value = 0;

  assert (flag);
  assert (range1);
  assert (range2);
  assert (arg);

  (*flag) = 1;

  if (!(range_str = strdup (arg)))
    {
      perror ("strdup");
      exit (1);
    }
  if (!(start_ptr = strchr (range_str, '-')))
    {
      /* invalid input */
      fprintf (stderr, "invalid range input\n");
      exit (1);
    }
  if (!(range2_str = strdup (start_ptr + 1)))
    {
      perror ("strdup");
      exit (1);
    }
  *start_ptr = '\0';
  range1_str = range_str;

  value = 0;
  ptr = NULL;
  errno = 0;
  value = strtol (range1_str, &ptr, 10);

  if (errno
      || ptr[0] != '\0'
      || value < 0
      || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
      || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    {
      fprintf (stderr, "invalid range record number: %d\n", value);
      exit (1);
    }

  (*range1) = value;

  value = 0;
  ptr = NULL;
  errno = 0;
  value = strtol (range2_str, &ptr, 10);

  if (errno
      || ptr[0] != '\0'
      || value < 0
      || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
      || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    {
      fprintf (stderr, "invalid range record number: %d\n", value);
      exit (1);
    }

  (*range2) = value;

  if ((*range2) < (*range1))
    {
      fprintf (stderr, "invalid END range\n");
      exit (1);
    }

  free (range1_str);
  free (range2_str);
}

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmi_sel_arguments *cmd_args = state->input;
  error_t ret;
  char *ptr;
  int value;

  switch (key)
    {
    case VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
    case INFO_KEY:
      cmd_args->info = 1;
      break;
    case DISPLAY_KEY:
      _read_record_list (&(cmd_args->display),
                         cmd_args->display_record_list,
                         &(cmd_args->display_record_list_length),
                         arg);
      break;
    case EXCLUDE_DISPLAY_KEY:
      _read_record_list (&(cmd_args->exclude_display),
                         cmd_args->exclude_display_record_list,
                         &(cmd_args->exclude_display_record_list_length),
                         arg);
      break;
    case DISPLAY_RANGE_KEY:
      _read_range (&(cmd_args->display_range),
                   &(cmd_args->display_range1),
                   &(cmd_args->display_range2),
                   arg);
      break;
    case EXCLUDE_DISPLAY_RANGE_KEY:
      _read_range (&(cmd_args->exclude_display_range),
                   &(cmd_args->exclude_display_range1),
                   &(cmd_args->exclude_display_range2),
                   arg);
      break;
    case TAIL_KEY:
      value = 0;
      ptr = NULL;
      errno = 0;
      value = strtol (arg, &ptr, 10);
      
      if (errno
          || ptr[0] != '\0'
          || value <= 0
          || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
          || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
        {
          fprintf (stderr, "invalid record count: %d\n", value);
          exit (1);
        }

      cmd_args->tail = 1;
      cmd_args->tail_count = value;
      break;
    case CLEAR_KEY:
    case DELETE_ALL_KEY:        /* legacy */
      cmd_args->clear = 1;
      break;
    case DELETE_KEY:
      _read_record_list (&(cmd_args->delete),
                         cmd_args->delete_record_list,
                         &(cmd_args->delete_record_list_length),
                         arg);
      break;
    case DELETE_RANGE_KEY:
      _read_range (&(cmd_args->delete_range),
                   &(cmd_args->delete_range1),
                   &(cmd_args->delete_range2),
                   arg);
      break;
    case SYSTEM_EVENT_ONLY_KEY:
      cmd_args->system_event_only = 1;
      break;
    case OEM_EVENT_ONLY_KEY:
      cmd_args->oem_event_only = 1;
      break;
    case HEX_DUMP_KEY:
      cmd_args->hex_dump = 1;
      break;
    case ASSUME_SYSTEM_EVENT_RECORDS_KEY:
      cmd_args->assume_system_event_records = 1;
      break;
    case INTERPRET_OEM_DATA_KEY:
      cmd_args->interpret_oem_data = 1;
      break;
    case ENTITY_SENSOR_NAMES_KEY:
      cmd_args->entity_sensor_names = 1;
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
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      ret = common_parse_opt (key, arg, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = sdr_parse_opt (key, arg, &(cmd_args->sdr));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, &(cmd_args->hostrange));
      return (ret);
    }

  return (0);
}

static void
_ipmi_sel_config_file_parse (struct ipmi_sel_arguments *cmd_args)
{
  struct config_file_data_ipmi_sel config_file_data;

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_sel));

  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         &(cmd_args->sdr),
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_SEL,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (1);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->verbose_count = config_file_data.verbose_count;
  if (config_file_data.system_event_only_count)
    cmd_args->system_event_only = config_file_data.system_event_only;
  if (config_file_data.oem_event_only_count)
    cmd_args->oem_event_only = config_file_data.oem_event_only;
  if (config_file_data.assume_system_event_records_count)
    cmd_args->assume_system_event_records = config_file_data.assume_system_event_records;
  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
  if (config_file_data.entity_sensor_names_count)
    cmd_args->entity_sensor_names = config_file_data.entity_sensor_names;
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
}


void
ipmi_sel_argp_parse (int argc, char **argv, struct ipmi_sel_arguments *cmd_args)
{
  init_common_cmd_args_operator (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->verbose_count = 0;
  cmd_args->info = 0;
  cmd_args->display = 0;
  memset (cmd_args->display_record_list,
          '\0',
          sizeof (uint16_t) * IPMI_SEL_MAX_RECORD);
  cmd_args->display_record_list_length = 0;
  cmd_args->exclude_display = 0;
  memset (cmd_args->exclude_display_record_list,
          '\0',
          sizeof (uint16_t) * IPMI_SEL_MAX_RECORD);
  cmd_args->exclude_display_record_list_length = 0;
  cmd_args->display_range = 0;
  cmd_args->display_range1 = 0;
  cmd_args->display_range2 = 0;
  cmd_args->exclude_display_range = 0;
  cmd_args->exclude_display_range1 = 0;
  cmd_args->exclude_display_range2 = 0;
  cmd_args->tail = 0;
  cmd_args->tail_count = 0;
  cmd_args->clear = 0;
  cmd_args->delete = 0;
  memset (cmd_args->delete_record_list,
          '\0',
          sizeof (uint16_t) * IPMI_SEL_MAX_RECORD);
  cmd_args->delete_record_list_length = 0;
  cmd_args->delete_range = 0;
  cmd_args->delete_range1 = 0;
  cmd_args->delete_range2 = 0;
  cmd_args->system_event_only = 0;
  cmd_args->oem_event_only = 0;
  cmd_args->hex_dump = 0;
  cmd_args->assume_system_event_records = 0;
  cmd_args->interpret_oem_data = 0;
  cmd_args->entity_sensor_names = 0;
  cmd_args->no_sensor_type_output = 0;
  cmd_args->comma_separated_output = 0;
  cmd_args->no_header_output = 0;
  cmd_args->non_abbreviated_units = 0;
  cmd_args->legacy_output = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common));

  _ipmi_sel_config_file_parse (cmd_args);

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
