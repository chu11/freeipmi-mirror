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
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
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

#include "ipmi-sel_.h"
#include "ipmi-sel-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-sel - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2014 FreeIPMI Core Team\n"
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
    ARGP_COMMON_SDR_CACHE_OPTIONS,
    ARGP_COMMON_SDR_CACHE_OPTIONS_FILE_DIRECTORY,
    ARGP_COMMON_SDR_CACHE_OPTIONS_IGNORE,
    ARGP_COMMON_TIME_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "verbose",    VERBOSE_KEY,    0, 0,
      "Increase verbosity in output.", 40},
    { "info",       INFO_KEY,       0, 0,
      "Show general information about the SEL.", 41},
    { "display",     DISPLAY_KEY,     "RECORD-IDS-LIST", 0,
      "Display SEL records by record ids.", 42},
    { "exclude-display", EXCLUDE_DISPLAY_KEY, "RECORD-IDS-LIST", 0,
      "Exclude display of SEL records by record ids.", 43},
    { "display-range", DISPLAY_RANGE_KEY, "START-END", 0,
      "Display SEL records from record id START to END.", 44},
    { "exclude-display-range", EXCLUDE_DISPLAY_RANGE_KEY, "START-END", 0,
      "Exclude display of SEL records from record id START to END.", 45},
    { "date-range", DATE_RANGE_KEY, "MM/DD/YYYY-MM/DD/YYYY", 0,
      "Display SEL records in the specified date range.", 46},
    { "exclude-date-range", EXCLUDE_DATE_RANGE_KEY, "MM/DD/YYYY-MM/DD/YYYY", 0,
      "Exclude display of SEL records in the specified date range.", 47},
    { "sensor-types",   SENSOR_TYPES_KEY,       "SENSOR-TYPES-LIST", 0,
      "Show sensors of a specific type.", 46},
    { "exclude-sensor-types", EXCLUDE_SENSOR_TYPES_KEY, "SENSOR-TYPES-LIST", 0,
      "Do not show sensors of a specific type.", 47},
    { "list-sensor-types",    LIST_SENSOR_TYPES_KEY, 0, 0,
      "List sensor types.", 48},
    { "tail", TAIL_KEY, "COUNT", 0,
      "Display approximately the last count SEL records.", 49},
    { "clear", CLEAR_KEY, 0, 0,
      "Clear SEL.", 50},
    { "post-clear", POST_CLEAR_KEY, 0, 0,
      "Clear SEL after displaying SEL records.", 51},
    /* legacy */
    { "delete-all", DELETE_ALL_KEY, 0, OPTION_HIDDEN,
      "Delete all SEL records.", 52},
    { "delete",     DELETE_KEY,     "RECORD-IDS-LIST", 0,
      "Delete SEL records by record ids.", 53},
    { "delete-range", DELETE_RANGE_KEY, "START-END", 0,
      "Delete record ids from START to END in the SEL.", 54},
    { "system-event-only", SYSTEM_EVENT_ONLY_KEY, 0, 0,
      "Output only system event records (i.e. don't output OEM records).", 55},
    { "oem-event-only", OEM_EVENT_ONLY_KEY, 0, 0,
      "Output only OEM event records.", 56},
    { "output-manufacturer-id", OUTPUT_MANUFACTURER_ID_KEY, 0, 0,
      "Output manufacturer ID on OEM event records when available.", 57},
    { "output-event-state", OUTPUT_EVENT_STATE_KEY, 0, 0,
      "Output event state in output.", 58},
    { "event-state-config-file", EVENT_STATE_CONFIG_FILE_KEY, "FILE", 0,
      "Specify an alternate event state configuration file.", 59},
    { "hex-dump",   HEX_DUMP_KEY, 0, 0,
      "Hex-dump SEL records.", 60},
    /* legacy */
    { "assume-system-event-records", ASSUME_SYSTEM_EVENT_RECORDS_KEY, 0, OPTION_HIDDEN,
      "Assume invalid record types are system event records.", 61},
    { "interpret-oem-data", INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 62},
    { "output-oem-event-strings", OUTPUT_OEM_EVENT_STRINGS_KEY, NULL, 0,
      "Attempt to output OEM event strings.", 63},
    { "entity-sensor-names", ENTITY_SENSOR_NAMES_KEY, NULL, 0,
      "Output sensor names with entity ids and instances.", 64},
    { "no-sensor-type-output", NO_SENSOR_TYPE_OUTPUT_KEY, 0, 0,
      "Do not show sensor type output.", 65},
    { "comma-separated-output", COMMA_SEPARATED_OUTPUT_KEY, 0, 0,
      "Output fields in comma separated format.", 66},
    { "no-header-output", NO_HEADER_OUTPUT_KEY, 0, 0,
      "Do not output column headers.", 67},
    { "non-abbreviated-units", NON_ABBREVIATED_UNITS_KEY, 0, 0,
      "Output non-abbreviated units (e.g. 'Amps' instead of 'A').", 68},
    { "legacy-output", LEGACY_OUTPUT_KEY, 0, 0,
      "Output in legacy format.", 69},
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
  char *endptr;
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
      errno = 0;
      value = strtol (tok, &endptr, 10);

      if (errno
          || endptr[0] != '\0'
          || value < 0
          || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
          || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
        {
          fprintf (stderr, "invalid record number: %d\n", value);
          exit (EXIT_FAILURE);
        }

      record_list[(*record_list_length)] = value;
      (*record_list_length)++;
      tok = strtok (NULL, " ,");
    }
}

static void
_read_record_id_range (int *flag,
                       uint16_t *range1,
                       uint16_t *range2,
                       char *arg)
{
  char *endptr;
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
      exit (EXIT_FAILURE);
    }
  if (!(start_ptr = strchr (range_str, '-')))
    {
      /* invalid input */
      fprintf (stderr, "invalid range input\n");
      exit (EXIT_FAILURE);
    }
  if (!(range2_str = strdup (start_ptr + 1)))
    {
      perror ("strdup");
      exit (EXIT_FAILURE);
    }
  *start_ptr = '\0';
  range1_str = range_str;

  errno = 0;
  value = strtol (range1_str, &endptr, 10);

  if (errno
      || endptr[0] != '\0'
      || value < 0
      || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
      || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    {
      fprintf (stderr, "invalid range record number: %d\n", value);
      exit (EXIT_FAILURE);
    }

  (*range1) = value;

  errno = 0;
  value = strtol (range2_str, &endptr, 10);

  if (errno
      || endptr[0] != '\0'
      || value < 0
      || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
      || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    {
      fprintf (stderr, "invalid range record number: %d\n", value);
      exit (EXIT_FAILURE);
    }

  (*range2) = value;

  if ((*range2) < (*range1))
    {
      fprintf (stderr, "invalid END range\n");
      exit (EXIT_FAILURE);
    }

  free (range1_str);
  free (range2_str);
}

static void
_read_date_range (int *flag,
                  uint32_t *range1,
                  uint32_t *range2,
                  char *arg)
{
  char *range_str = NULL;
  char *split_ptr = NULL;
  char *range1_str = NULL;
  char *range2_str = NULL;
  char *ptr = NULL;
  unsigned int dash_count = 0;
  time_t t;
  struct tm tm;

  assert (flag);
  assert (range1);
  assert (range2);
  assert (arg);

  (*flag) = 1;

  if (!(range_str = strdup (arg)))
    {
      perror ("strdup");
      exit (EXIT_FAILURE);
    }
  
  /* Count number of dashes, to see what format user input */
  ptr = range_str;
  do {
    ptr = strchr (ptr, '-');
    if (ptr)
      {
        ptr++;
        dash_count++;
      }
  } while (ptr);

  if (dash_count == 1)
    split_ptr = strchr (range_str, '-');
  else if (dash_count == 3)
    {
      /* one date input w/ dashes, one with slashes */
      char *ptr1, *ptr2;

      ptr1 = strchr (range_str, '/');
      ptr2 = strchr (range_str, '-');

      /* determine if MM/DD/YYYY is first or second date listed */
      if (ptr1 < ptr2)
        split_ptr = strchr (range_str, '-');
      else
        {
          ptr = range_str;
          ptr = strchr (ptr, '-');
          ptr++;
          ptr = strchr (ptr, '-');
          ptr++;
          split_ptr = strchr (ptr, '-');
        }
    }
  else if (dash_count == 5)
    {
      /* find the middle dash */
      ptr = range_str;
      ptr = strchr (ptr, '-');
      ptr++;
      ptr = strchr (ptr, '-');
      ptr++;
      split_ptr = strchr (ptr, '-');
    }
  else
    {
      fprintf (stderr, "invalid range input\n");
      exit (EXIT_FAILURE);
    }

  if (*(split_ptr + 1) == '\0')
    {
      fprintf (stderr, "invalid range input\n");
      exit (EXIT_FAILURE);
    }

  if (!(range2_str = strdup (split_ptr + 1)))
    {
      perror ("strdup");
      exit (EXIT_FAILURE);
    }
  *split_ptr = '\0';
  range1_str = range_str;

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tm, '\0', sizeof (struct tm));

  if (!strcasecmp (range1_str, "now"))
    t = time (NULL);
  else
    {
      if (!strptime (range1_str, "%m/%d/%Y", &tm))
        {
          if (!strptime (range1_str, "%b/%d/%Y", &tm))
            {
              if (!strptime (range1_str, "%m-%d-%Y", &tm))
                {
                  if (!strptime (range1_str, "%b-%d-%Y", &tm))
                    {
                      fprintf (stderr,
                               "Invalid time specification '%s'.\n",
                               range1_str);
                      exit (EXIT_FAILURE);
                    }
                }
            }
        }

      /* strptime() does not set tm_isdst.  Set so mktime() will not
       * adjust for daylight savings time.
       */
      tm.tm_isdst = -1;

      if ((t = mktime (&tm)) == (time_t)-1)
        {
          fprintf (stderr,
                   "Time specification '%s' cannot be represented.\n",
                   range1_str);
          exit (EXIT_FAILURE);
        }
    }

  (*range1) = (uint32_t)t;

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tm, '\0', sizeof (struct tm));

  if (!strcasecmp (range2_str, "now"))
    t = time (NULL);
  else
    {
      if (!strptime (range2_str, "%m/%d/%Y", &tm))
        {
          if (!strptime (range2_str, "%b/%d/%Y", &tm))
            {
              if (!strptime (range2_str, "%m-%d-%Y", &tm))
                {
                  if (!strptime (range2_str, "%b-%d-%Y", &tm))
                    {
                      fprintf (stderr,
                               "Invalid time specification '%s'.\n",
                               range2_str);
                      exit (EXIT_FAILURE);
                    }
                }
            }
        }

      /* strptime() does not set tm_isdst.  Set so mktime() will not
       * adjust for daylight savings time.
       */
      tm.tm_isdst = -1;

      if ((t = mktime (&tm)) == (time_t)-1)
        {
          fprintf (stderr,
                   "Time specification '%s' cannot be represented.\n",
                   range2_str);
          exit (EXIT_FAILURE);
        }
    }

  (*range2) = (uint32_t)t;

  if ((*range2) < (*range1))
    {
      fprintf (stderr, "invalid range\n");
      exit (EXIT_FAILURE);
    }

  /* Date range input means beginning of range1 date to end of range2 date
   * so we might need to add seconds to the end of the range2 date.
   */
  if (strcasecmp (range2_str, "now"))
    (*range2) = (*range2) + (24 * 60 * 60);

  free (range1_str);
  free (range2_str);
}

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmi_sel_arguments *cmd_args;
  char *endptr;
  int value;

  assert (state);
  
  cmd_args = state->input;

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
      _read_record_id_range (&(cmd_args->display_range),
                             &(cmd_args->display_range1),
                             &(cmd_args->display_range2),
                             arg);
      break;
    case EXCLUDE_DISPLAY_RANGE_KEY:
      _read_record_id_range (&(cmd_args->exclude_display_range),
                             &(cmd_args->exclude_display_range1),
                             &(cmd_args->exclude_display_range2),
                             arg);
      break;
    case DATE_RANGE_KEY:
      _read_date_range (&(cmd_args->date_range),
                        &(cmd_args->date_range1),
                        &(cmd_args->date_range2),
                        arg);
      break;
    case EXCLUDE_DATE_RANGE_KEY:
      _read_date_range (&(cmd_args->exclude_date_range),
                        &(cmd_args->exclude_date_range1),
                        &(cmd_args->exclude_date_range2),
                        arg);
      break;
    case SENSOR_TYPES_KEY:
      if (parse_sensor_types (SENSOR_PARSE_ALL_STRING,
			      cmd_args->sensor_types,
			      &(cmd_args->sensor_types_length),
			      arg) < 0)
	exit (EXIT_FAILURE);
      break;
    case EXCLUDE_SENSOR_TYPES_KEY:
      if (parse_sensor_types (SENSOR_PARSE_NONE_STRING,
			      cmd_args->exclude_sensor_types,
			      &(cmd_args->exclude_sensor_types_length),
			      arg) < 0)
	exit (EXIT_FAILURE);
      break;
    case LIST_SENSOR_TYPES_KEY:
      cmd_args->list_sensor_types = 1;
      break;
    case TAIL_KEY:
      errno = 0;
      value = strtol (arg, &endptr, 10);
      
      if (errno
          || endptr[0] != '\0'
          || value <= 0
          || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
          || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
        {
          fprintf (stderr, "invalid record count: %d\n", value);
          exit (EXIT_FAILURE);
        }

      cmd_args->tail = 1;
      cmd_args->tail_count = value;
      break;
    case CLEAR_KEY:
    case DELETE_ALL_KEY:        /* legacy */
      cmd_args->clear = 1;
      break;
    case POST_CLEAR_KEY:
      cmd_args->post_clear = 1;
      break;
    case DELETE_KEY:
      _read_record_list (&(cmd_args->delete),
                         cmd_args->delete_record_list,
                         &(cmd_args->delete_record_list_length),
                         arg);
      break;
    case DELETE_RANGE_KEY:
      _read_record_id_range (&(cmd_args->delete_range),
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
    case OUTPUT_MANUFACTURER_ID_KEY:
      cmd_args->output_manufacturer_id = 1;
      break;
    case OUTPUT_EVENT_STATE_KEY:
      cmd_args->output_event_state = 1;
      break;
    case EVENT_STATE_CONFIG_FILE_KEY:
      if (!(cmd_args->event_state_config_file = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case HEX_DUMP_KEY:
      cmd_args->hex_dump = 1;
      break;
      /* legacy */
    case ASSUME_SYSTEM_EVENT_RECORDS_KEY:
      cmd_args->assume_system_event_records = 1;
      break;
    case INTERPRET_OEM_DATA_KEY:
      cmd_args->interpret_oem_data = 1;
      break;
    case OUTPUT_OEM_EVENT_STRINGS_KEY:
      cmd_args->output_oem_event_strings = 1;
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
      return (common_parse_opt (key, arg, &(cmd_args->common_args)));
    }

  return (0);
}

static void
_ipmi_sel_config_file_parse (struct ipmi_sel_arguments *cmd_args)
{
  struct config_file_data_ipmi_sel config_file_data;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_sel));

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_TIME | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_SEL,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->verbose_count = config_file_data.verbose_count;
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
  if (config_file_data.system_event_only_count)
    cmd_args->system_event_only = config_file_data.system_event_only;
  if (config_file_data.oem_event_only_count)
    cmd_args->oem_event_only = config_file_data.oem_event_only;
  if (config_file_data.output_manufacturer_id_count)
    cmd_args->output_manufacturer_id = config_file_data.output_manufacturer_id;
  if (config_file_data.output_event_state_count)
    cmd_args->output_event_state = config_file_data.output_event_state;
  if (config_file_data.event_state_config_file_count)
    cmd_args->event_state_config_file = config_file_data.event_state_config_file;
  /* legacy */
  if (config_file_data.assume_system_event_records_count)
    cmd_args->assume_system_event_records = config_file_data.assume_system_event_records;
  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
  if (config_file_data.output_oem_event_strings_count)
    cmd_args->output_oem_event_strings = config_file_data.output_oem_event_strings;
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

static void
_ipmi_sel_args_validate (struct ipmi_sel_arguments *cmd_args)
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
ipmi_sel_argp_parse (int argc, char **argv, struct ipmi_sel_arguments *cmd_args)
{
  unsigned int i;

  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_operator (&(cmd_args->common_args));

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
  cmd_args->date_range = 0;
  cmd_args->date_range1 = 0;
  cmd_args->date_range2 = 0;
  cmd_args->exclude_date_range = 0;
  cmd_args->exclude_date_range1 = 0;
  cmd_args->exclude_date_range2 = 0;

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

  cmd_args->tail = 0;
  cmd_args->tail_count = 0;
  cmd_args->clear = 0;
  cmd_args->post_clear = 0;
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
  cmd_args->output_manufacturer_id = 0;
  cmd_args->output_event_state = 0;
  cmd_args->event_state_config_file = NULL;
  cmd_args->hex_dump = 0;
  /* legacy */
  cmd_args->assume_system_event_records = 0;
  cmd_args->interpret_oem_data = 0;
  cmd_args->output_oem_event_strings = 0;
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
              &(cmd_args->common_args));

  _ipmi_sel_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _ipmi_sel_args_validate (cmd_args);
}
