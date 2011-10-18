/*
 * Copyright (C) 2011 FreeIPMI Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
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
#include <limits.h>
#include <errno.h>

#include "ipmi-pet.h"
#include "ipmi-pet-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-pet - " PACKAGE_VERSION "\n"
  "Copyright (C) 2011 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-pet - IPMI Platform Event Trap Interpreter";

static char cmdline_args_doc[] = "[SPECIFIC TRAP] [VARIABLE BINDING HEX BYTES ...]";

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
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "verbose",    VERBOSE_KEY,    0, 0,
      "Increase verbosity in output.", 30},
    { "file", CMD_FILE_KEY, "CMD-FILE", 0,
      "Specify a file to read PET bytes from.", 31},
    { "output-event-state", OUTPUT_EVENT_STATE_KEY, 0, 0,
      "Output event state in output.", 32},
    { "event-state-config-file", EVENT_STATE_CONFIG_FILE_KEY, "FILE", 0,
      "Specify an alternate event state configuration file.", 33},
    { "interpret-oem-data", INTERPRET_OEM_DATA_KEY, NULL, 0,
      "Attempt to interpret OEM data.", 34},
    { "entity-sensor-names", ENTITY_SENSOR_NAMES_KEY, NULL, 0,
      "Output sensor names with entity ids and instances.", 35},
    { "no-sensor-type-output", NO_SENSOR_TYPE_OUTPUT_KEY, 0, 0,
      "Do not show sensor type output.", 36},
    { "comma-separated-output", COMMA_SEPARATED_OUTPUT_KEY, 0, 0,
      "Output fields in comma separated format.", 37},
    { "no-header-output", NO_HEADER_OUTPUT_KEY, 0, 0,
      "Do not output column headers.", 38},
    { "non-abbreviated-units", NON_ABBREVIATED_UNITS_KEY, 0, 0,
      "Output non-abbreviated units (e.g. 'Amps' instead of 'A').", 39},
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
  struct ipmi_pet_arguments *cmd_args = state->input;

  switch (key)
    {
    case VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
    case CMD_FILE_KEY:
      if (!(cmd_args->cmd_file = strdup (arg)))
        {
          perror ("strdup");
          exit (1);
        }
      break;
    case OUTPUT_EVENT_STATE_KEY:
      cmd_args->output_event_state = 1;
      break;
    case EVENT_STATE_CONFIG_FILE_KEY:
      if (!(cmd_args->event_state_config_file = strdup (arg)))
        {
          perror ("strdup");
          exit (1);
        }
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
    case ARGP_KEY_ARG:
      {
        unsigned int i;
        long value;
        
        if (strlen (arg) >= 2)
          {
            if (strncmp (arg, "0x", 2) == 0)
              arg+=2;
          }
        
        if (*arg == '\0')
          {
            fprintf (stderr, "invalid hex byte argument\n");
            exit (1);
          }
        
        for (i = 0; arg[i] != '\0'; i++)
          {
            if (i >= 2)
              {
                fprintf (stderr, "invalid hex byte argument\n");
                exit (1);
              }
            
            if (!isxdigit (arg[i]))
              {
                fprintf (stderr, "invalid hex byte argument\n");
                exit (1);
              }
          }
        
        value = strtol (arg, (char **) NULL, 16);
        cmd_args->cmd[cmd_args->cmd_length++] = (uint8_t) value;
        
        break;
      }
    case ARGP_KEY_END:
      break;
    default:
      return (ARGP_ERR_UNKNOWN);
    }

  return (0);
}

static void
_ipmi_pet_config_file_parse (struct ipmi_pet_arguments *cmd_args)
{
  struct config_file_data_ipmi_pet config_file_data;

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_pet));

  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         &(cmd_args->sdr),
                         NULL,
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR,
                         CONFIG_FILE_TOOL_IPMI_PET,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (1);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->verbose_count = config_file_data.verbose_count;
  if (config_file_data.output_event_state_count)
    cmd_args->output_event_state = config_file_data.output_event_state;
  if (config_file_data.event_state_config_file_count)
    cmd_args->event_state_config_file = config_file_data.event_state_config_file;
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
}

void
ipmi_pet_argp_parse (int argc, char **argv, struct ipmi_pet_arguments *cmd_args)
{
  init_common_cmd_args_operator (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  cmd_args->verbose_count = 0;
  cmd_args->cmd_file = NULL;
  cmd_args->output_event_state = 0;
  cmd_args->event_state_config_file = NULL;
  cmd_args->interpret_oem_data = 0;
  cmd_args->entity_sensor_names = 0;
  cmd_args->no_sensor_type_output = 0;
  cmd_args->comma_separated_output = 0;
  cmd_args->no_header_output = 0;
  cmd_args->non_abbreviated_units = 0;
  errno = 0;
  if ((cmd_args->arg_max = sysconf (_SC_ARG_MAX)) <= 0)
    {
      if (errno)
        {
          perror ("sysconf");
          exit (1);
        }
      cmd_args->arg_max = LONG_MAX;
    }
  if (!(cmd_args->cmd = calloc (cmd_args->arg_max, sizeof (uint8_t))))
    {
      perror ("calloc");
      exit (1);
    }
  cmd_args->cmd_length = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common));
  
  _ipmi_pet_config_file_parse (cmd_args);
  
  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common));
  verify_sdr_cmd_args (&(cmd_args->sdr));
}

