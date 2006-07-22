/* 

   bmc-config - tool to configure bmc

   Copyright (C) 2006 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <argp.h>
#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#ifndef __FreeBSD__
#include <sys/io.h>
#endif
#include <syslog.h>
#include <assert.h>
#include <stdarg.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <errno.h>
#if HAVE_GETOPT_H
#include <getopt.h>
#endif
#include <stdint.h>
#include <sys/stat.h>
#include <sys/select.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else  /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */

#include "argp-common.h"

#include "bmc-types.h"
#include "bmc-sections.h"

#include "freeipmi-portability.h"

const char *argp_program_version = PACKAGE_VERSION;
const char *argp_program_bug_address = "<" PACKAGE_BUGREPORT ">";
/* Program documentation. */
static char doc[] =  "GNU FreeIPMI (bmc-config) -- BMC config tool";
/* A description of the arguments we accept. */
static char args_doc[] = "";


/* The options we understand. */
static struct argp_option options[] = {
  /* bmc-config should have a default privilege of ADMIN 
   * so we cannot use ARGP_COMMON_OPTIONS
   */
  ARGP_COMMON_OPTIONS_INBAND,
  ARGP_COMMON_OPTIONS_OUTOFBAND,
  ARGP_COMMON_OPTIONS_AUTHTYPE,
  {"priv-level",     PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0,
   "Use this PRIVILEGE-LEVEL instead of ADMIN.  "
   "Allowed values are CALLBACK, USER, OPERATOR, ADMIN and OEM.", 11},

  {"checkout", 'o', 0, 0, 
   "Action is to GET the BMC configuration", 12},
  {"commit", 'i', 0, 0, 
   "Action is to UPDATE the BMC configuration", 13},
  {"diff", 'd', 0, 0, 
   "Action is to SHOW THE DIFFERENCES with BMC", 14},

  {"filename", 'f', "FILENAME", 0, 
   "use FILENAME in checkout, commit or diff", 15},
  {"key-pair", 'k', "KEY-PAIR", 0, 
   "use KEY-PAIR in commit or diff", 16},

  {"verbose",   'v', 0, 0,  "Produce verbose output", 17},
  {"quiet",     'q', 0, 0,  "Do not produce any output", 18 },
#ifndef NDEBUG
  {"debug",     'B', 0, 0,  "Print debug output", 19},
#endif /* NDEBUG */

  { 0, }
};



static int
args_validate (struct arguments *args)
{
  int ret = 0;

  // action is non 0 and -1
  if (! args->action || args->action == -1) 
    {
      fprintf (stderr, 
               "Exactly one of --checkout, --commit or --diff MUST be given\n");
      return -1;
    }

  // filename and keypair both not given
  if (args->filename && args->keypair) 
    {
      fprintf (stderr, 
               "Both --filename or --keypair cannot be used\n");
      return -1;
    }

  // filename is readable if commit, writable/creatable if checkout

  if (args->filename) 
    {
      switch (args->action) 
        {
        case BMC_ACTION_COMMIT: case BMC_ACTION_DIFF:
          if (access (args->filename, R_OK) != 0) 
            {
              perror (args->filename);
              return -1;
            }
          break;
        case BMC_ACTION_CHECKOUT:
          if (access (args->filename, F_OK) == 0) 
            {
              if (access (args->filename, W_OK) != 0) 
                {
                  perror (args->filename);
                  return -1;
                }
            } 
          else 
            {
              int fd;
              fd = open (args->filename, O_CREAT);
              if (fd == -1) 
                {
                  perror (args->filename);
                  return -1;
                } 
              else 
                {
                  close (fd);
                  unlink (args->filename);
                }
            }
        }
    }
  
  return ret;
}

static void
display (const struct arguments *args)
{
  return;
}

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{

  struct arguments *arguments = state->input;

  switch (key)
    {
    case 'q': 
    case 's':
      arguments->silent = 1;
      break;
    case 'v':
      arguments->verbose = 1;
      break;
#ifndef NDEBUG
    case 'B':
      arguments->debug = 1;
      break;
#endif /* NDEBUG */

    case 'f':
      if (arguments->filename) /* If specified more than once */
	free (arguments->filename);
      arguments->filename = strdup (arg);
      break;

    case 'k':
      if (arguments->keypair) /* if specified more than once */
	free (arguments->keypair);
      arguments->keypair = strdup (arg);
      break;

    case 'o':
      if (! arguments->action)
	arguments->action = BMC_ACTION_CHECKOUT;
      else
	arguments->action = -1;
      break;

    case 'i':
      if (! arguments->action)
	arguments->action = BMC_ACTION_COMMIT;
      else
	arguments->action = -1;
      break;

    case 'd':
      if (! arguments->action)
	arguments->action = BMC_ACTION_DIFF;
      else
	arguments->action = -1;
      break;

    case ARGP_KEY_ARG:
      argp_usage (state);
      break;

    default:
      return common_parse_opt (key, arg, state, &arguments->common);
    }
  return 0;
}

/* Our argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc};

int
bmc_argp (int argc, char *argv[], struct arguments *arguments)
{
  
  argp_parse (&argp, argc, argv, 0, 0, arguments);

  if (args_validate (arguments) == -1)
    return (1);

  if (arguments->verbose)
    display (arguments);

  return (0);
}
