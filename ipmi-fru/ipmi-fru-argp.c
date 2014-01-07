/***************************************************************************** \
 *  $Id: ipmi-fru-argp.c,v 1.33 2010-02-08 22:20:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
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

#include "ipmi-fru_.h"
#include "ipmi-fru-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-fru - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-fru - display FRU information.";

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
    { "device-id", DEVICE_ID_KEY, "DEVICE_ID", 0,
      "Specify a specific FRU device ID.", 40},
    { "verbose", VERBOSE_KEY, 0, 0,
      "Increase verbosity in output.", 41},
    /* legacy */
    { "skip-checks", SKIP_CHECKS_KEY, 0, OPTION_HIDDEN,
      "Skip FRU checksum checks", 42},
    { "bridge-fru", BRIDGE_FRU_KEY, 0, 0,
      "Bridge to read FRU entries on other controllers", 43},
    { "interpret-oem-data", INTERPRET_OEM_DATA, NULL, 0,
      "Attempt to interpret OEM data.", 44},
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
  struct ipmi_fru_arguments *cmd_args;
  char *endptr;
  int tmp;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case DEVICE_ID_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
	  || endptr[0] != '\0')
        {
          fprintf (stderr, "invalid device id\n");
          exit (EXIT_FAILURE);
        }
      
      if (tmp == IPMI_FRU_DEVICE_ID_RESERVED
          || tmp < IPMI_FRU_DEVICE_ID_MIN
          || tmp > IPMI_FRU_DEVICE_ID_MAX)
        {
          fprintf (stderr, "invalid device id\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->device_id = tmp;
      cmd_args->device_id_set++;
      break;
    case VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
      /* legacy */
    case SKIP_CHECKS_KEY:
      cmd_args->skip_checks = 1;
      break;
    case BRIDGE_FRU_KEY:
      cmd_args->bridge_fru = 1;
      break;
    case INTERPRET_OEM_DATA:
      cmd_args->interpret_oem_data = 1;
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
_ipmi_fru_config_file_parse (struct ipmi_fru_arguments *cmd_args)
{
  struct config_file_data_ipmi_fru config_file_data;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_fru));

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_TIME | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_FRU,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->verbose_count = config_file_data.verbose_count;
  /* legacy */
  if (config_file_data.skip_checks_count)
    cmd_args->skip_checks = config_file_data.skip_checks;
  if (config_file_data.bridge_fru_count)
    cmd_args->bridge_fru = config_file_data.bridge_fru;
  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
}

void
ipmi_fru_argp_parse (int argc, char **argv, struct ipmi_fru_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_user (&(cmd_args->common_args));

  cmd_args->device_id = 0;
  cmd_args->device_id_set = 0;
  cmd_args->verbose_count = 0;
  /* legacy */
  cmd_args->skip_checks = 0;
  cmd_args->bridge_fru = 0;
  cmd_args->interpret_oem_data = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER, NULL,
              &(cmd_args->common_args));

  _ipmi_fru_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
}
