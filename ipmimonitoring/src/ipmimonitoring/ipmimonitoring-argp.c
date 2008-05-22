/*****************************************************************************\
 *  $Id: ipmimonitoring-argp.c,v 1.12 2008-05-22 17:37:04 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <argp.h>
#include <errno.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>

#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"
#include "ipmimonitoring.h"
#include "ipmimonitoring-argp.h"

#include "freeipmi-portability.h"

const char *argp_program_version = 
  "ipmimonitoring - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2006-2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = 
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] = 
  "ipmimonitoring - IPMI monitoring utility";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_USER,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    /* maintain "regenerate-sdr-cache" for backwards compatability */
    {"regenerate-sdr-cache", REGENERATE_SDR_CACHE_KEY, 0, OPTION_HIDDEN,
     "Regenerate the SDR cache.", 30},
    /* maintain "cache-dir" for backwards compatability */
    {"cache-dir", CACHE_DIR_KEY, "DIRECTORY", OPTION_HIDDEN,
     "Specify an alternate directory to read and write SDR caches..", 31},
    {"quiet-readings", QUIET_READINGS_KEY,  0, 0,
     "Do not output sensor readings, only states.", 32},
    {"list-groups",    LIST_GROUPS_KEY,    0, 0, 
     "List sensor groups.", 33}, 
    {"groups",         GROUPS_KEY,       "GROUP-NAME", 0, 
     "Show sensors belonging to a specific group.", 34}, 
    {"sensors",        SENSORS_LIST_KEY, "SENSORS-LIST", 0, 
     "Show sensors by record id.  Accepts space or comma separated lists", 35}, 
    { 0 }
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
  struct ipmimonitoring_arguments *cmd_args = state->input;
  char *ptr;
  char *tok;
  error_t ret;
  
  switch (key)
    {
      /* legacy option */
    case REGENERATE_SDR_CACHE_KEY:
      cmd_args->regenerate_sdr_cache = 1;
      break;
      /* legacy option */
    case CACHE_DIR_KEY:
      return sdr_parse_opt (ARGP_SDR_CACHE_DIRECTORY_KEY, 
                            arg, 
                            state, 
                            &(cmd_args->sdr));
      break;
    case QUIET_READINGS_KEY:
      cmd_args->quiet_readings = 1;
      break;
    case LIST_GROUPS_KEY:
      cmd_args->list_groups = 1;
      break;
    case GROUPS_KEY:
      cmd_args->groups_list_wanted = 1;
      tok = strtok(arg, " ,");
      while (tok && cmd_args->groups_list_length < IPMIMONITORING_MAX_GROUPS)
        {
          strncpy(cmd_args->groups_list[cmd_args->groups_list_length],
                  tok,
                  IPMIMONITORING_MAX_GROUPS_STRING_LENGTH);
          cmd_args->groups_list_length++;
          tok = strtok(NULL, " ,");
        }
      break;
    case SENSORS_LIST_KEY:
      cmd_args->sensors_list_wanted = 1;
      tok = strtok(arg, " ,");
      while (tok && cmd_args->sensors_list_length < IPMIMONITORING_MAX_RECORD_IDS)
        {
          unsigned int n = strtoul(tok, &ptr, 10);
          if (ptr != (tok + strlen(tok)))
            {
              fprintf (stderr, "invalid sensor record id\n");
              exit(1);
            }
          cmd_args->sensors_list[cmd_args->sensors_list_length] = n;
          cmd_args->sensors_list_length++;
          tok = strtok(NULL, " ,");
        }
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

static void
_ipmimonitoring_config_file_parse(struct ipmimonitoring_arguments *cmd_args)
{
  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         NULL,
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_HOSTRANGE,
                         0,
                         NULL) < 0)
    {
      fprintf(stderr, "config_file_parse: %s\n", strerror(errno));
      exit(1);
    }
}

void 
ipmimonitoring_argp_parse (int argc, char **argv, struct ipmimonitoring_arguments *cmd_args)
{
  int i;

  init_common_cmd_args_user (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->regenerate_sdr_cache = 0;
  cmd_args->quiet_readings = 0;
  cmd_args->list_groups = 0;
  cmd_args->groups_list_wanted = 0;
  for (i = 0; i < IPMIMONITORING_MAX_GROUPS; i++)
    memset(cmd_args->groups_list[i],
           '\0',
           IPMIMONITORING_MAX_GROUPS_STRING_LENGTH+1);
  cmd_args->groups_list_length = 0;
  cmd_args->sensors_list_wanted = 0;
  memset(cmd_args->sensors_list, 
         '\0', 
         sizeof(unsigned int)*IPMIMONITORING_MAX_RECORD_IDS);
  cmd_args->sensors_list_length = 0;

  memset(&(cmd_args->conf), '\0', sizeof(struct ipmi_monitoring_ipmi_config));
  cmd_args->ipmimonitoring_flags = 0;
  memset(cmd_args->ipmimonitoring_groups,
         '\0',
         sizeof(unsigned int)*IPMIMONITORING_MAX_GROUPS);

  cmd_args->ipmimonitoring_groups_length = 0;

  argp_parse (&cmdline_config_file_argp, argc, argv, ARGP_IN_ORDER, NULL, &(cmd_args->common));

  _ipmimonitoring_config_file_parse(cmd_args);

  argp_parse (&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_sdr_cmd_args (&(cmd_args->sdr));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}


