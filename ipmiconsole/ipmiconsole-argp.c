/*****************************************************************************\
 *  $Id: ipmiconsole-argp.c,v 1.33 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
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
#include <sys/param.h>
#include <assert.h>
#include <errno.h>

#include <ipmiconsole.h>        /* lib ipmiconsole.h */

#include "ipmiconsole_.h"       /* tool ipmiconsole.h */
#include "ipmiconsole-argp.h"

#include "freeipmi-portability.h"
#include "conffile.h"
#include "secure.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmiconsole - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2006-2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmiconsole - IPMI console utility";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "escape-char", ESCAPE_CHAR_KEY, "CHAR", 0,
      "Specify an alternate escape character (default char '&')", 40},
    { "dont-steal", DONT_STEAL_KEY, 0, 0,
      "Do not steal an SOL session if one is already detected as being in use.", 41},
    { "deactivate", DEACTIVATE_KEY, 0, 0,
      "Deactivate a SOL session if one is detected as being in use and exit.", 42},
    { "serial-keepalive", SERIAL_KEEPALIVE_KEY, 0, 0,
      "Occasionally send NUL characters to detect inactive serial connections.", 43},
    { "serial-keepalive-empty", SERIAL_KEEPALIVE_EMPTY_KEY, 0, 0,
      "Occasionally send empty SOL packets to detect inactive serial connections.", 44},
    { "sol-payload-instance", SOL_PAYLOAD_INSTANCE_KEY, "NUM", 0,
      "Specify SOL payload instance number.", 45},
    { "deactivate-all-instances", DEACTIVATE_ALL_INSTANCES_KEY, 0, 0,
      "Deactivate all payload instances instead of just the configured payload instance.", 46},
    { "lock-memory", LOCK_MEMORY_KEY, 0, 0,
      "Lock sensitive information (such as usernames and passwords) in memory.", 47},
#ifndef NDEBUG
    { "debugfile", DEBUGFILE_KEY, 0, 0,
      "Output debugging to the debugfile rather than to standard output.", 48},
    { "noraw", NORAW_KEY, 0, 0,
      "Don't enter terminal raw mode.", 49},
#endif
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
  struct ipmiconsole_arguments *cmd_args;
  char *endptr;
  int tmp;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case ESCAPE_CHAR_KEY:          /* --escape-char */
      cmd_args->escape_char = *arg;
      break;
    case DONT_STEAL_KEY:       /* --dont-steal */
      cmd_args->dont_steal++;
      break;
    case DEACTIVATE_KEY:       /* --deactivate */
      cmd_args->deactivate++;
      break;
    case SERIAL_KEEPALIVE_KEY:       /* --serial-keepalive */
      cmd_args->serial_keepalive++;
      break;
    case SERIAL_KEEPALIVE_EMPTY_KEY:       /* --serial-keepalive-empty */
      cmd_args->serial_keepalive_empty++;
      break;
    case SOL_PAYLOAD_INSTANCE_KEY: /* --sol-payload-instance */
      errno = 0;
      tmp = strtol (arg, &endptr, 0);
      if (errno
          || endptr[0] != '\0'
	  || !IPMI_PAYLOAD_INSTANCE_VALID (tmp))
        {
	  fprintf (stderr, "invalid sol payload instance\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->sol_payload_instance = tmp;
      break;
    case DEACTIVATE_ALL_INSTANCES_KEY: /* --deactivate-all-instances */
      cmd_args->deactivate_all_instances++;
      break;
    case LOCK_MEMORY_KEY:       /* --lock-memory */
      cmd_args->lock_memory++;
      break;
#ifndef NDEBUG
    case DEBUGFILE_KEY: /* --debugfile */
      cmd_args->debugfile++;
      break;
    case NORAW_KEY:     /* --noraw */
      cmd_args->noraw++;
      break;
#endif /* NDEBUG */
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
_ipmiconsole_config_file_parse (struct ipmiconsole_arguments *cmd_args)
{
  struct config_file_data_ipmiconsole config_file_data;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmiconsole));

  if (!cmd_args->common_args.config_file)
    {
      /* try legacy file first */
      if (!config_file_parse (IPMICONSOLE_CONFIG_FILE_LEGACY,
                              1,         /* do not exit if file not found */
                              &(cmd_args->common_args),
                              CONFIG_FILE_OUTOFBAND,
                              CONFIG_FILE_TOOL_IPMICONSOLE,
                              &config_file_data))
        goto out;
    }

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_OUTOFBAND,
                         CONFIG_FILE_TOOL_IPMICONSOLE,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

 out:
  if (config_file_data.escape_char_count)
    cmd_args->escape_char = config_file_data.escape_char;
  if (config_file_data.dont_steal_count)
    cmd_args->dont_steal = config_file_data.dont_steal;
  if (config_file_data.serial_keepalive_count)
    cmd_args->serial_keepalive = config_file_data.serial_keepalive;
  if (config_file_data.serial_keepalive_empty_count)
    cmd_args->serial_keepalive_empty = config_file_data.serial_keepalive_empty;
  if (config_file_data.lock_memory_count)
    cmd_args->lock_memory = config_file_data.lock_memory;
}

static void
_ipmiconsole_args_validate (struct ipmiconsole_arguments *cmd_args)
{
  assert (cmd_args);

  if (!cmd_args->common_args.hostname)
    {
      fprintf (stderr, "hostname input required\n");
      exit (EXIT_FAILURE);
    }
}

void
ipmiconsole_argp_parse (int argc, char **argv, struct ipmiconsole_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_admin (&(cmd_args->common_args));

  /* ipmiconsole differences */
  cmd_args->common_args.driver_type = IPMI_DEVICE_LAN_2_0;
  cmd_args->common_args.session_timeout = 60000;
  cmd_args->common_args.retransmission_timeout = 500;

  cmd_args->escape_char = '&';
  cmd_args->dont_steal = 0;
  cmd_args->deactivate = 0;
  cmd_args->serial_keepalive = 0;
  cmd_args->serial_keepalive_empty = 0;
  cmd_args->sol_payload_instance = 0;
  cmd_args->deactivate_all_instances = 0;
  cmd_args->lock_memory = 0;
#ifndef NDEBUG
  cmd_args->debugfile = 0;
  cmd_args->noraw = 0;
#endif /* NDEBUG */

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _ipmiconsole_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _ipmiconsole_args_validate (cmd_args);
}

