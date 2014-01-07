/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */
#include <assert.h>

#include "ipmiseld.h"
#include "ipmiseld-argp.h"
#include "ipmiseld-common.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"
#include "error.h"

const char *argp_program_version =
  "ipmiseld - " PACKAGE_VERSION "\n"
  "Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmiseld - IPMI SEL syslog logging daemon";

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
    ARGP_COMMON_OPTIONS_DEBUG,
    { "verbose", IPMISELD_VERBOSE_KEY, 0, 0,
      "Increase verbosity in output.", 40},
    { "sensor-types", IPMISELD_SENSOR_TYPES_KEY, "SENSOR-TYPES-LIST", 0,
      "Show sensors of a specific type.", 41},
    { "exclude-sensor-types", IPMISELD_EXCLUDE_SENSOR_TYPES_KEY, "SENSOR-TYPES-LIST", 0,
      "Do not show sensors of a specific type.", 42},
    { "system-event-only", IPMISELD_SYSTEM_EVENT_ONLY_KEY, 0, 0,
      "Output only system event records (i.e. don't output OEM records).", 43},
    { "oem-event-only", IPMISELD_OEM_EVENT_ONLY_KEY, 0, 0,
      "Output only OEM event records.", 44},
    { "event-state-config-file", IPMISELD_EVENT_STATE_CONFIG_FILE_KEY, "FILE", 0,
      "Specify an alternate event state configuration file.", 45},
    { "interpret-oem-data", IPMISELD_INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 46},
    { "output-oem-event-strings", IPMISELD_OUTPUT_OEM_EVENT_STRINGS_KEY, NULL, 0,
      "Attempt to output OEM event strings.", 47},
    { "entity-sensor-names", IPMISELD_ENTITY_SENSOR_NAMES_KEY, NULL, 0,
      "Output sensor names with entity ids and instances.", 48},
    { "non-abbreviated-units", IPMISELD_NON_ABBREVIATED_UNITS_KEY, 0, 0,
      "Output non-abbreviated units (e.g. 'Amps' instead of 'A').", 49},
    { "event-state-filter", IPMISELD_EVENT_STATE_FILTER_KEY, "FILTERSTRING", 0,
      "Specify event states to filter out and not log.", 50}, 
    { "warning-threshold", IPMISELD_WARNING_THRESHOLD_KEY, "PERCENTINT", 0,
      "Specify SEL fullness warning threshold as an integer percentage.", 51},
    { "clear-threshold", IPMISELD_CLEAR_THRESHOLD_KEY, "PERCENTINT", 0,
      "Specify SEL fullness clear threshold as an integer percentage.", 52},
    { "system-event-format", IPMISELD_SYSTEM_EVENT_FORMAT_KEY, "FORMATSTRING", 0,
      "Specify format for system event outputs.", 53},
    { "oem-timestamped-event-format", IPMISELD_OEM_TIMESTAMPED_EVENT_FORMAT_KEY, "FORMATSTRING", 0,
      "Specify format for oem timestamped event outputs.", 54},
    { "oem-non-timestamped-event-format", IPMISELD_OEM_NON_TIMESTAMPED_EVENT_FORMAT_KEY, "FORMATSTRING", 0,
      "Specify format for oem non-timestamped event outputs.", 55},
    { "poll-interval", IPMISELD_POLL_INTERVAL_KEY, "SECONDS", 0,
      "Specify poll interval to check the SEL for new events.", 56},
    { "log-facility", IPMISELD_LOG_FACILITY_KEY, "STRING", 0,
      "Specify syslog log facility.", 57},
    { "log-priority", IPMISELD_LOG_PRIORITY_KEY, "STRING", 0,
      "Specify syslog log priority.", 58},
    { "cache-directory", IPMISELD_CACHE_DIRECTORY_KEY, "DIRECTORY", 0,
      "Specify alternate cache directory.", 59},
    { "ignore-sdr", IPMISELD_IGNORE_SDR_KEY, 0, 0,
      "Ignore SDR related processing.", 60},
    { "re-download-sdr", IPMISELD_RE_DOWNLOAD_SDR_KEY, 0, 0,
      "Re-download the SDR even if it is not out of date.", 61},
    { "clear-sel", IPMISELD_CLEAR_SEL_KEY, 0, 0,
      "Clear SEL on startup.", 62},
    { "threadpool-count", IPMISELD_THREADPOOL_COUNT_KEY, "NUM", 0,
      "Specify threadpool count for parallel SEL polling.", 63},
    { "test-run", IPMISELD_TEST_RUN_KEY, 0, 0,
      "Do not daemonize, output current SEL as test of current settings.", 64},
    { "foreground", IPMISELD_FOREGROUND_KEY, 0, 0,
      "Run daemon in foreground.", 65},
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
  struct ipmiseld_arguments *cmd_args;
  char *endptr;
  int tmp;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case IPMISELD_VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
    case IPMISELD_SENSOR_TYPES_KEY:
      if (parse_sensor_types (SENSOR_PARSE_ALL_STRING,
                              cmd_args->sensor_types,
                              &(cmd_args->sensor_types_length),
                              arg) < 0)
        exit (EXIT_FAILURE);
      break;
    case IPMISELD_EXCLUDE_SENSOR_TYPES_KEY:
      if (parse_sensor_types (SENSOR_PARSE_NONE_STRING,
                              cmd_args->exclude_sensor_types,
                              &(cmd_args->exclude_sensor_types_length),
                              arg) < 0)
        exit (EXIT_FAILURE);
      break;
    case IPMISELD_SYSTEM_EVENT_ONLY_KEY:
      cmd_args->system_event_only = 1;
      break;
    case IPMISELD_OEM_EVENT_ONLY_KEY:
      cmd_args->oem_event_only = 1;
      break;
    case IPMISELD_EVENT_STATE_CONFIG_FILE_KEY:
      if (!(cmd_args->event_state_config_file = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case IPMISELD_INTERPRET_OEM_DATA_KEY:
      cmd_args->interpret_oem_data = 1;
      break;
    case IPMISELD_OUTPUT_OEM_EVENT_STRINGS_KEY:
      cmd_args->output_oem_event_strings = 1;
      break;
    case IPMISELD_ENTITY_SENSOR_NAMES_KEY:
      cmd_args->entity_sensor_names = 1;
      break;
    case IPMISELD_NON_ABBREVIATED_UNITS_KEY:
      cmd_args->non_abbreviated_units = 1;
      break;
    case IPMISELD_EVENT_STATE_FILTER_KEY:
      if (!(cmd_args->event_state_filter_str = strdup (arg)))
	{
	  perror ("strdup");
	  exit (EXIT_FAILURE);
	}
      break;
    case IPMISELD_WARNING_THRESHOLD_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
          || endptr[0] != '\0'
	  || tmp < 0
	  || tmp > 100) 
        {
          fprintf (stderr, "invalid warning threshold\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->warning_threshold = tmp;
      break;
    case IPMISELD_CLEAR_THRESHOLD_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
          || endptr[0] != '\0'
	  || tmp < 0
	  || tmp > 100) 
        {
          fprintf (stderr, "invalid clear threshold\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->clear_threshold = tmp;
      break;
    case IPMISELD_SYSTEM_EVENT_FORMAT_KEY:
      if (!(cmd_args->system_event_format_str = strdup (arg)))
	{
	  perror ("strdup");
	  exit (EXIT_FAILURE);
	}
      break;
    case IPMISELD_OEM_TIMESTAMPED_EVENT_FORMAT_KEY:
      if (!(cmd_args->oem_timestamped_event_format_str = strdup (arg)))
	{
	  perror ("strdup");
	  exit (EXIT_FAILURE);
	}
      break;
    case IPMISELD_OEM_NON_TIMESTAMPED_EVENT_FORMAT_KEY:
      if (!(cmd_args->oem_non_timestamped_event_format_str = strdup (arg)))
	{
	  perror ("strdup");
	  exit (EXIT_FAILURE);
	}
      break;
    case IPMISELD_POLL_INTERVAL_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
          || endptr[0] != '\0'
	  || tmp <= 0) 
        {
          fprintf (stderr, "invalid poll interval\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->poll_interval = tmp;
      break;
    case IPMISELD_LOG_FACILITY_KEY:
      if (!(cmd_args->log_facility_str = strdup (arg)))
	{
	  perror ("strdup");
	  exit (EXIT_FAILURE);
	}
      break;
    case IPMISELD_LOG_PRIORITY_KEY:
      if (!(cmd_args->log_priority_str = strdup (arg)))
	{
	  perror ("strdup");
	  exit (EXIT_FAILURE);
	}
      break;
    case IPMISELD_CACHE_DIRECTORY_KEY:
      if (!(cmd_args->cache_directory = strdup (arg)))
	{
	  perror ("strdup");
	  exit (EXIT_FAILURE);
	}
      break;
    case IPMISELD_IGNORE_SDR_KEY:
      cmd_args->ignore_sdr = 1;
      break;
    case IPMISELD_RE_DOWNLOAD_SDR_KEY:
      cmd_args->re_download_sdr = 1;
      break;
    case IPMISELD_CLEAR_SEL_KEY:
      cmd_args->clear_sel = 1;
      break;
    case IPMISELD_THREADPOOL_COUNT_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
          || endptr[0] != '\0'
	  || tmp <= 0) 
        {
          fprintf (stderr, "invalid threadpool count\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->threadpool_count = tmp;
      break;
    case IPMISELD_TEST_RUN_KEY:
      cmd_args->test_run = 1;
      break;
    case IPMISELD_FOREGROUND_KEY:
      cmd_args->foreground = 1;
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
_ipmiseld_config_file_parse (struct ipmiseld_arguments *cmd_args)
{
  struct config_file_data_ipmiseld config_file_data;
  char *filename;
  int no_error_if_not_found;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmiseld));

  /* Force use of ipmiseld config file, we don't want the standard freeipmi one */
  if (cmd_args->common_args.config_file)
    {
      filename = cmd_args->common_args.config_file;
      no_error_if_not_found = 0;  
    }
  else
    {
      filename = IPMISELD_CONFIG_FILE_DEFAULT;
      no_error_if_not_found = 1;
    }

  /* if returns < 0, file not found
   * will exit on fatal error within func
   */
  if (config_file_parse (filename,
                         no_error_if_not_found,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND,
                         CONFIG_FILE_TOOL_IPMISELD,
                         &config_file_data) < 0)
    return;

  /* normally require user to input on command line, but because this
   * is a daemon, we allow hostname config */
  if (config_file_data.hostname_count)
    cmd_args->common_args.hostname = config_file_data.hostname;
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
  if (config_file_data.event_state_config_file_count)
    cmd_args->event_state_config_file = config_file_data.event_state_config_file;
  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
  if (config_file_data.output_oem_event_strings_count)
    cmd_args->output_oem_event_strings = config_file_data.output_oem_event_strings;
  if (config_file_data.entity_sensor_names_count)
    cmd_args->entity_sensor_names = config_file_data.entity_sensor_names;
  if (config_file_data.non_abbreviated_units_count)
    cmd_args->non_abbreviated_units = config_file_data.non_abbreviated_units;
  if (config_file_data.event_state_filter_str_count)
    cmd_args->event_state_filter_str = config_file_data.event_state_filter_str;
  if (config_file_data.warning_threshold_count)
    cmd_args->warning_threshold = config_file_data.warning_threshold;
  if (config_file_data.clear_threshold_count)
    cmd_args->clear_threshold = config_file_data.clear_threshold;
  if (config_file_data.system_event_format_str_count)
    cmd_args->system_event_format_str = config_file_data.system_event_format_str;
  if (config_file_data.oem_timestamped_event_format_str_count)
    cmd_args->oem_timestamped_event_format_str = config_file_data.oem_timestamped_event_format_str;
  if (config_file_data.oem_non_timestamped_event_format_str_count)
    cmd_args->oem_non_timestamped_event_format_str = config_file_data.oem_non_timestamped_event_format_str;
  if (config_file_data.poll_interval_count)
    cmd_args->poll_interval = config_file_data.poll_interval;
  if (config_file_data.log_facility_str_count)
    cmd_args->log_facility_str = config_file_data.log_facility_str;
  if (config_file_data.log_priority_str_count)
    cmd_args->log_priority_str = config_file_data.log_priority_str;
  if (config_file_data.cache_directory_count)
    cmd_args->cache_directory = config_file_data.cache_directory;
  if (config_file_data.ignore_sdr_count)
    cmd_args->ignore_sdr = config_file_data.ignore_sdr;
  if (config_file_data.re_download_sdr_count)
    cmd_args->re_download_sdr = config_file_data.re_download_sdr;
  if (config_file_data.clear_sel_count)
    cmd_args->clear_sel = config_file_data.clear_sel;
  if (config_file_data.threadpool_count_count)
    cmd_args->threadpool_count = config_file_data.threadpool_count;
}

static void
_ipmiseld_args_validate (struct ipmiseld_arguments *cmd_args)
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

  if (cmd_args->event_state_filter_str)
    {
      if (ipmiseld_event_state_filter_parse (cmd_args->event_state_filter_str) < 0)
	err_exit ("Invalid event state filter specified\n");
    }

  if (cmd_args->log_facility_str)
    {
      if (ipmiseld_log_facility_parse (cmd_args->log_facility_str) < 0)
	err_exit ("Invalid log facility specified\n");
    }

  if (cmd_args->log_priority_str)
    {
      if (ipmiseld_log_priority_parse (cmd_args->log_priority_str) < 0)
	err_exit ("Invalid log priority specified\n");
    }

  if (cmd_args->cache_directory)
    {
      if (access (cmd_args->cache_directory, R_OK|W_OK|X_OK) < 0)
        {
          err_exit ("insufficient permission on cache directory '%s'",
		    cmd_args->cache_directory);
          exit (EXIT_FAILURE);
        }
    }
}

void
ipmiseld_argp_parse (int argc, char **argv, struct ipmiseld_arguments *cmd_args)
{
  unsigned int i;

  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);
  
  init_common_cmd_args_operator (&(cmd_args->common_args));

  cmd_args->verbose_count = 0;

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

  cmd_args->system_event_only = 0;
  cmd_args->oem_event_only = 0;
  cmd_args->event_state_config_file = NULL;
  cmd_args->interpret_oem_data = 0;
  cmd_args->output_oem_event_strings = 0;
  cmd_args->entity_sensor_names = 0;
  cmd_args->non_abbreviated_units = 0;
  cmd_args->event_state_filter_str = NULL;
  cmd_args->warning_threshold = IPMISELD_WARNING_THRESHOLD_DEFAULT;
  cmd_args->clear_threshold = IPMISELD_CLEAR_THRESHOLD_DEFAULT;
  cmd_args->system_event_format_str = NULL;
  cmd_args->oem_timestamped_event_format_str = NULL;
  cmd_args->oem_non_timestamped_event_format_str = NULL;
  cmd_args->poll_interval = IPMISELD_POLL_INTERVAL_DEFAULT;
  cmd_args->log_facility_str = NULL;
  cmd_args->log_priority_str = NULL;
  cmd_args->cache_directory = NULL;
  cmd_args->ignore_sdr = 0;
  cmd_args->re_download_sdr = 0;
  cmd_args->clear_sel = 0;
  cmd_args->threadpool_count = IPMISELD_THREADPOOL_COUNT;
  cmd_args->test_run = 0;
  cmd_args->foreground = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _ipmiseld_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _ipmiseld_args_validate (cmd_args);
}
