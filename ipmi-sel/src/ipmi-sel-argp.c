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

#include "ipmi-sel.h"
#include "ipmi-sel-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version = 
  "ipmi-sel - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2008 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = 
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] = 
  "ipmi-sel - display SEL entries";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_USER,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_IGNORE_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about the SEL.", 30},
    {"delete",     DELETE_KEY,     "REC-LIST", 0, 
     "Delete SEL records by record ids.", 33},
    {"delete-all", DELETE_ALL_KEY, 0, 0, 
     "Delete all SEL records.", 34},
    {"delete-range", DELETE_RANGE_KEY, "START-END", 0, 
     "Delete record ids from START to END in the SEL.", 35},
    {"hex-dump",   HEX_DUMP_KEY,   "FILE", OPTION_ARG_OPTIONAL, 
     "Hex-dump SEL records optionally into a FILE.", 36},
    {"assume-system-event-records", ASSUME_SYSTEM_EVENT_RECORDS_KEY, 0, 0,
     "Assume invalid record types are system event records.", 37},
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
  struct ipmi_sel_arguments *cmd_args = state->input;
  char *ptr;
  char *tok;
  int value;
  error_t ret;

  switch (key)
    {
    case INFO_KEY:
      cmd_args->info = 1;
      break;
    case DELETE_ALL_KEY:
      cmd_args->delete_all = 1;
      break;
    case DELETE_KEY:
      cmd_args->delete = 1;
      tok = strtok(arg, " ,");
      while (tok && cmd_args->delete_record_list_length < IPMI_SEL_MAX_DELETE_RECORD)
        {
          value = 0;
          ptr = NULL;	
          errno = 0;

          value = strtol(tok, &ptr, 10);

          if (errno 
              || ptr[0] != '\0'
              || value < 0
              || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
              || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
            {
              fprintf (stderr, "invalid delete record number: %d\n", value);
              exit(1);
            }

          cmd_args->delete_record_list[cmd_args->delete_record_list_length] = value;
          cmd_args->delete_record_list_length++;
          tok = strtok(NULL, " ,");
        }
      break;
    case DELETE_RANGE_KEY:
      cmd_args->delete_range = 1;
      {
	char *range_str = NULL;
	char *start_ptr = NULL;
	char *range1_str = NULL;
	char *range2_str = NULL;
	int value = 0;
	
	range_str = strdupa (arg);
	if (!(start_ptr = strchr (range_str, '-')))
          {
            /* invalid input */
	    fprintf (stderr, "invalid delete range\n");
            exit(1);
          }
	range2_str = strdupa (start_ptr + 1);
	*start_ptr = '\0';
	range1_str = range_str;
	
	value = 0;
	ptr = NULL;	
	errno = 0;
	value = strtol (range1_str, &ptr, 10);

        if (errno 
            || ptr[0] != '\0'
            || value < 0
            || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
            || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
	  {
	    fprintf (stderr, "invalid delete range record number: %d\n", value);
            exit(1);
	  }
	
	cmd_args->delete_range1 = value;
	
	value = 0;
	ptr = NULL;	
	errno = 0;
	value = strtol (range2_str, &ptr, 10);

        if (errno 
            || ptr[0] != '\0'
            || value < 0
            || value <= IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
            || value >= IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
	  {
	    fprintf (stderr, "invalid delete range record number: %d\n", value);
            exit(1);
	  }

	cmd_args->delete_range2 = value;	
        
	if (cmd_args->delete_range2 < cmd_args->delete_range1)
	  {
	    fprintf (stderr, "invalid delete END range\n");
            exit(1);
	  }
      }
      break;
    case HEX_DUMP_KEY:
      cmd_args->hex_dump = 1;
      if (arg)
	{
	  if (cmd_args->hex_dump_filename)
	    free (cmd_args->hex_dump_filename);
	  cmd_args->hex_dump_filename = strdup (arg);
	}
      break;
    case ASSUME_SYSTEM_EVENT_RECORDS_KEY:
      cmd_args->assume_system_event_records = 1;
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
_ipmi_sel_config_file_parse(struct ipmi_sel_arguments *cmd_args)
{
  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         &(cmd_args->sdr),
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_SDR | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_SEL,
                         NULL) < 0)
    {
      fprintf(stderr, "config_file_parse: %s\n", strerror(errno));
      exit(1);
    }
}


void 
ipmi_sel_argp_parse (int argc, char **argv, struct ipmi_sel_arguments *cmd_args)
{
  init_common_cmd_args_operator (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->info = 0;
  cmd_args->delete_all = 0;
  cmd_args->delete = 0;
  memset(cmd_args->delete_record_list,
         '\0',
         sizeof(int)*IPMI_SEL_MAX_DELETE_RECORD);
   cmd_args->delete_record_list_length = 0;
  cmd_args->delete_range = 0;
  cmd_args->delete_range1 = 0;
  cmd_args->delete_range2 = 0;
  cmd_args->hex_dump = 0;
  cmd_args->hex_dump_filename = NULL;
  cmd_args->assume_system_event_records = 0;
  
  argp_parse (&cmdline_config_file_argp, argc, argv, ARGP_IN_ORDER, NULL, &(cmd_args->common));

  _ipmi_sel_config_file_parse(cmd_args);

  argp_parse (&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_sdr_cmd_args (&(cmd_args->sdr));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}
