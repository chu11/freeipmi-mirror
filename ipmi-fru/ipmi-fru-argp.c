/***************************************************************************** \
 *  $Id: ipmi-fru-argp.c,v 1.33 2010-02-08 22:20:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
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

#include "ipmi-fru.h"
#include "ipmi-fru-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-fru - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.\n"
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
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_SDR_DIRECTORY_OPTIONS,
    ARGP_COMMON_IGNORE_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "device-id", DEVICE_ID_KEY, "DEVICE_ID", 0,
      "Specify a specific FRU device ID.", 30},
    { "verbose", VERBOSE_KEY, 0, 0,
      "Increase verbosity in output.", 31},
    { "skip-checks", SKIP_CHECKS_KEY, 0, 0,
      "Skip FRU checksum checks", 32},
    { "interpret-oem-data", INTERPRET_OEM_DATA, NULL, 0,
      "Attempt to interpret OEM data.", 33},
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
  error_t ret;
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
          exit (1);
        }
      
      if (tmp == IPMI_FRU_DEVICE_ID_RESERVED
          || tmp < IPMI_FRU_DEVICE_ID_MIN
          || tmp > IPMI_FRU_DEVICE_ID_MAX)
        {
          fprintf (stderr, "invalid device id\n");
          exit(1);
        }
      cmd_args->device_id = tmp;
      cmd_args->device_id_set++;
      break;
    case VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
    case SKIP_CHECKS_KEY:
      cmd_args->skip_checks = 1;
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
_ipmi_fru_config_file_parse (struct ipmi_fru_arguments *cmd_args)
{
  struct config_file_data_ipmi_fru config_file_data;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_fru));

  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         &(cmd_args->sdr),
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_FRU,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (1);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->verbose_count = config_file_data.verbose_count;
  if (config_file_data.skip_checks_count)
    cmd_args->skip_checks = config_file_data.skip_checks;
  if (config_file_data.interpret_oem_data_count)
    cmd_args->interpret_oem_data = config_file_data.interpret_oem_data;
}

void
ipmi_fru_argp_parse (int argc, char **argv, struct ipmi_fru_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_user (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));

  cmd_args->device_id = 0;
  cmd_args->device_id_set = 0;
  cmd_args->verbose_count = 0;
  cmd_args->skip_checks = 0;
  cmd_args->interpret_oem_data = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER, NULL,
              &(cmd_args->common));

  _ipmi_fru_config_file_parse (cmd_args);

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
