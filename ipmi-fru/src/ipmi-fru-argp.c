/*****************************************************************************\
 *  $Id: ipmi-fru-argp.c,v 1.1.2.5 2007-07-11 17:50:29 chu11 Exp $
 *****************************************************************************
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
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <argp.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */


#include "argp-common.h"
#include "ipmi-fru.h"
#include "ipmi-fru-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI FRU Information [ipmi-fru-" PACKAGE_VERSION "]\n";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI FRU - display FRU information.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHTYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVLEVEL_USER,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
#ifndef NDEBUG
    ARGP_COMMON_OPTIONS_DEBUG,
#endif
    {"device-id", DEVICE_ID_KEY, "DEVICE_ID", 0,
     "Specify FRU device ID.", 25},
    {"verbose", VERBOSE_KEY, 0, 0,
     "Increase verbosity in output.", 26},
    {"skip-checks", SKIP_CHECKS_KEY, 0, 0,
     "Skip FRU version and checksum checks", 27},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmi_fru_arguments *cmd_args = state->input;
  error_t ret;
  char *ptr;

  switch (key)
    {
    case DEVICE_ID_KEY:
      cmd_args->device_id_wanted++;
      cmd_args->device_id = strtoul(arg, &ptr, 0);
      if (ptr != (arg + strlen(arg)))
        {
          fprintf (stderr, "invalid device id\n");
          argp_usage (state);
        }
      break;
    case VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
    case SKIP_CHECKS_KEY:
      cmd_args->skip_checks_wanted = 1;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = sdr_parse_opt (key, arg, state, &(cmd_args->sdr));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->hostrange));
      return ret;
    }
  
  return 0;
}

void 
ipmi_fru_argp_parse (int argc, char **argv, struct ipmi_fru_arguments *cmd_args)
{
  init_common_cmd_args (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->device_id_wanted = 0;
  cmd_args->device_id = 0;
  cmd_args->verbose_count = 0;
  cmd_args->skip_checks_wanted = 0;

  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_sdr_cmd_args (&(cmd_args->sdr));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}
