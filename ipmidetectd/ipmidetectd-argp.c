/*****************************************************************************\
 *  $Id: ipmidetectd.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-228523
 *
 *  This file is part of Ipmidetect, tools and libraries for detecting
 *  IPMI nodes in a cluster. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmidetect is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmidetect is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmidetect.  If not, see <http://www.gnu.org/licenses/>.
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
#include <assert.h>

#include "ipmidetectd.h"
#include "ipmidetectd-argp.h"

#include "freeipmi-portability.h"
#include "error.h"

const char *argp_program_version =
  "ipmidetectd - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmidetectd - IPMI node detection daemon";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    { "config-file", IPMIDETECTD_CONFIG_FILE_KEY, "FILE", 0,
      "Specify alternate config file", 1},
    { "config_file", IPMIDETECTD_LEGACY_CONFIG_FILE_KEY, "FILE", OPTION_HIDDEN,
      "Specify alternate config file", 2},
    { "debug", IPMIDETECTD_DEBUG_KEY, 0, 0,
      "Turn on debugging and run daemon in foreground", 3},
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
  struct ipmidetectd_arguments *cmd_args;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case IPMIDETECTD_CONFIG_FILE_KEY:
    case IPMIDETECTD_LEGACY_CONFIG_FILE_KEY:
      if (!(cmd_args->config_file = strdup (arg)))
	err_exit ("strdup: %s", strerror (errno));
      break;
    case IPMIDETECTD_DEBUG_KEY:
      cmd_args->debug++;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return (ARGP_ERR_UNKNOWN);
    }

  return (0);
}

void
ipmidetectd_argp_parse (int argc, char **argv, struct ipmidetectd_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  cmd_args->debug = 0;
  cmd_args->config_file = NULL;

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);
}
