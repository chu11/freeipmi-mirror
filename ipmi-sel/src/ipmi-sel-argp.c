/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

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

#include "ipmi-sel.h"
#include "ipmi-sel-argp.h"

#include "freeipmi-portability.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI SEL [ipmi-sel-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2008 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI SEL - Used to view and delete SEL entries.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_USER,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_IGNORE_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about the SEL.", 30},
    {"get-time",   GET_TIME_KEY,   0, 0,
     "Get SEL time.", 31},
    {"set-time",   SET_TIME_KEY,   "TIME", 0,
     "Set SEL time.  Input format = \"MM/DD/YYYY - HH:MM:SS\" or \"now\".", 32},
    {"delete",     DELETE_KEY,     "REC-LIST", 0, 
     "Delete SEL records by record ids.", 33},
    {"delete-all", DELETE_ALL_KEY, 0, 0, 
     "Delete all SEL records.", 34},
    {"delete-range", DELETE_RANGE_KEY, "START-END", 0, 
     "Delete record ids from START to END in the SEL.", 35},
    {"hex-dump",   HEX_DUMP_KEY,   "FILE", OPTION_ARG_OPTIONAL, 
     "Hex-dump SEL records optionally into a FILE.", 36},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmi_sel_arguments *cmd_args = state->input;
  char *ptr;
  char *tok;
  error_t ret;

  switch (key)
    {
    case INFO_KEY:
      cmd_args->info_wanted = 1;
      break;
    case GET_TIME_KEY:
      cmd_args->get_time_wanted = 1;
      break;
    case SET_TIME_KEY:
      cmd_args->set_time_wanted = 1;
      cmd_args->set_time_arg = arg;
      break;
    case DELETE_ALL_KEY:
      cmd_args->delete_all_wanted = 1;
      break;
    case DELETE_KEY:
      cmd_args->delete_wanted = 1;
      tok = strtok(arg, " ,");
      while (tok && cmd_args->delete_record_list_length < IPMI_SEL_MAX_DELETE_RECORD)
        {
          unsigned int n = strtoul(tok, &ptr, 10);
          if (ptr != (tok + strlen(tok)))
            {
              fprintf (stderr, "invalid delete record number\n");
              argp_usage (state);
              break;
            }
          cmd_args->delete_record_list[cmd_args->delete_record_list_length] = n;
          cmd_args->delete_record_list_length++;
          tok = strtok(NULL, " ,");
        }
      break;
    case DELETE_RANGE_KEY:
      cmd_args->delete_range_wanted = 1;
      {
	char *range_str = NULL;
	char *start_ptr = NULL;
	char *range1_str = NULL;
	char *range2_str = NULL;
	int value = 0;
	int errnum = 0;
	char *tail = NULL;
	
	range_str = strdupa (arg);
	if (!(start_ptr = strchr (range_str, '-')))
          {
            /* invalid input */
	    fprintf (stderr, "invalid delete range\n");
	    argp_usage (state);
	    break;
          }
	range2_str = strdupa (start_ptr + 1);
	*start_ptr = '\0';
	range1_str = range_str;
	
	errno = 0;
	value = strtol (range1_str, &tail, 10);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid delete range\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid delete range\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid delete range\n");
	    argp_usage (state);
	    break;
	  }
	
	cmd_args->delete_range1 = value;
	
	value = 0;
	errnum = 0;
	tail = NULL;
	
	errno = 0;
	value = strtol (range2_str, &tail, 10);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid delete range\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid delete range\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid delete range\n");
	    argp_usage (state);
	    break;
	  }
	
	cmd_args->delete_range2 = value;
	
	if (cmd_args->delete_range2 < cmd_args->delete_range1)
	  {
	    fprintf (stderr, "invalid END range\n");
	    argp_usage (state);
	    break;
	  }
      }
      break;
    case HEX_DUMP_KEY:
      cmd_args->hex_dump_wanted = 1;
      if (arg)
	{
	  if (cmd_args->hex_dump_filename)
	    free (cmd_args->hex_dump_filename);
	  cmd_args->hex_dump_filename = strdup (arg);
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

void 
ipmi_sel_argp_parse (int argc, char **argv, struct ipmi_sel_arguments *cmd_args)
{
  init_common_cmd_args_operator (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->info_wanted = 0;
  cmd_args->get_time_wanted = 0;
  cmd_args->set_time_wanted = 0;
  cmd_args->set_time_arg = NULL;
  cmd_args->delete_all_wanted = 0;
  cmd_args->delete_wanted = 0;
  memset(cmd_args->delete_record_list,
         '\0',
         sizeof(int)*IPMI_SEL_MAX_DELETE_RECORD);
   cmd_args->delete_record_list_length = 0;
  cmd_args->delete_range_wanted = 0;
  cmd_args->delete_range1 = 0;
  cmd_args->delete_range2 = 0;
  cmd_args->hex_dump_wanted = 0;
  cmd_args->hex_dump_filename = NULL;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_sdr_cmd_args (&(cmd_args->sdr));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}
