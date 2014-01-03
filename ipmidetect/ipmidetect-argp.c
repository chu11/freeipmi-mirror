/*****************************************************************************\
 *  $Id: ipmidetect.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
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
#include <errno.h>

#include "ipmidetect_.h"
#include "ipmidetect-argp.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "fd.h"
#include "hostlist.h"

const char *argp_program_version =
  "ipmidetect - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmidetect - IPMI node detection client";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    { "hostname", IPMIDETECT_HOSTNAME_KEY, "HOST", 0,
      "Specify alternate server hostname", 1},
    { "hostname_legacy", IPMIDETECT_LEGACY_HOSTNAME_KEY, "HOST", OPTION_HIDDEN,
      "Specify alternate server hostname", 2},
    { "port", IPMIDETECT_PORT_KEY, "PORT", 0,
      "Specify alternate server port", 3},
    { "detected", IPMIDETECT_DETECTED_KEY, 0, 0,
      "Output only detected nodes", 4},
    { "undetected", IPMIDETECT_UNDETECTED_KEY, 0, 0,
      "Output only undetected nodes", 5},
    { "hostrange", IPMIDETECT_HOSTRANGE_KEY, 0, 0,
      "Output in hostrange format", 6},
    { "comma", IPMIDETECT_COMMA_KEY, 0, 0,
      "Output in comma separated format", 7},
    { "newline", IPMIDETECT_NEWLINE_KEY, 0, 0,
      "Output in newline separated format", 8},
    { "space", IPMIDETECT_SPACE_KEY, 0, 0,
      "Output in space separated format", 9},
    { NULL, 0, NULL, 0, NULL, 0}
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc };

#define IPMIDETECT_STDIN_BUFFERLEN 65536

static void
_push_inputted_nodes (struct ipmidetect_arguments *cmd_args,
		      const char *nodes)
{
  assert (cmd_args);
  assert (nodes);

  /* Error if nodes aren't short hostnames */
  if (strchr (nodes, '.'))
    err_exit ("nodes must be listed in short hostname format");

  if (!hostlist_push (cmd_args->inputted_nodes, nodes))
    err_exit ("nodes improperly formatted");
}

static void
_read_nodes_from_stdin (struct ipmidetect_arguments *cmd_args)
{
  char buf[IPMIDETECT_STDIN_BUFFERLEN];
  int n;

  assert (cmd_args);

  memset (buf, '\0', IPMIDETECT_STDIN_BUFFERLEN);

  if ((n = fd_read_n (STDIN_FILENO, buf, IPMIDETECT_STDIN_BUFFERLEN)) < 0)
    err_exit ("error reading from standard input: %s", strerror (errno));

  if (n == IPMIDETECT_STDIN_BUFFERLEN)
    err_exit ("overflow in standard input buffer");

  if (n > 0)
    {
      char *ptr = strtok (buf, " \t\n\0");
      while (ptr)
        {
          _push_inputted_nodes (cmd_args, ptr);
          ptr = strtok (NULL, " \t\n\0");
        }
    }
}

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmidetect_arguments *cmd_args;
  char *endptr;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case IPMIDETECT_HOSTNAME_KEY:
    case IPMIDETECT_LEGACY_HOSTNAME_KEY:
      if (!(cmd_args->hostname = strdup (arg)))
	err_exit ("strdup: %s", strerror (errno));
      break;
    case IPMIDETECT_PORT_KEY:
      errno = 0;
      cmd_args->port = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0')
	err_exit ("invalid port specified");
      break;
    case IPMIDETECT_DETECTED_KEY:
      cmd_args->output_type = IPMIDETECT_DETECTED_NODES;
      break;
    case IPMIDETECT_UNDETECTED_KEY:
      cmd_args->output_type = IPMIDETECT_UNDETECTED_NODES;
      break;
    case IPMIDETECT_HOSTRANGE_KEY:
      cmd_args->output_format = 0;
      break;
    case IPMIDETECT_COMMA_KEY:
      cmd_args->output_format = ',';
      break;
    case IPMIDETECT_NEWLINE_KEY:
      cmd_args->output_format = '\n';
      break;
    case IPMIDETECT_SPACE_KEY:
      cmd_args->output_format = ' ';
      break;
    case ARGP_KEY_ARG:
      if (!strcmp (arg, "-"))
        _read_nodes_from_stdin (cmd_args);
      else
	_push_inputted_nodes (cmd_args, arg);
      hostlist_uniq (cmd_args->inputted_nodes);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return (ARGP_ERR_UNKNOWN);
    }

  return (0);
}

void
ipmidetect_argp_parse (int argc, char **argv, struct ipmidetect_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  cmd_args->hostname = NULL;
  cmd_args->port = 0;
  cmd_args->output_type = IPMIDETECT_DETECTED_AND_UNDETECTED_NODES;
  cmd_args->output_format = 0;
  
  if (!(cmd_args->inputted_nodes = hostlist_create (NULL)))
    err_exit ("hostlist_create");

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);
}
