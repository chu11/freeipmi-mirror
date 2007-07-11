/* 
   $Id: ipmi-sel-argp.c,v 1.18.4.3 2007-07-11 17:50:30 chu11 Exp $ 
   
   ipmi-sel-argp.c - System Event Logger utility.
   
   Copyright (C) 2005 FreeIPMI Core Team
   
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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
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
#include <ctype.h>
#include <errno.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "argp-common.h"
#include "ipmi-sel.h"
#include "ipmi-sel-argp.h"

#include "freeipmi-portability.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI SEL [ipmi-sel-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
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
    ARGP_COMMON_OPTIONS_AUTHTYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVLEVEL_USER,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_SDR_OPTIONS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
#ifndef NDEBUG
    ARGP_COMMON_OPTIONS_DEBUG,
#endif
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about SEL.", 25},
    {"delete",     DELETE_KEY,     "REC-LIST", 0, 
     "Delete given SEL records entry.", 26},
    {"delete-all", DELETE_ALL_KEY, 0, 0, 
     "Delete all SEL entries.", 27},
    {"delete-range", DELETE_RANGE_KEY, "START-END", 0, 
     "Delete records from START to END in SEL.", 28},
    {"hex-dump",   HEX_DUMP_KEY,   "FILE", OPTION_ARG_OPTIONAL, 
     "Hex-dump SEL entries optionally to FILE.", 29},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };


static char *
stripwhite (char *string)
{
  register char *s, *t;

  for (s = string; isspace (*s); s++)
    ;

  if (*s == 0)
    return s;

  t = s + strlen (s) - 1;
  while (t > s && isspace (*t))
    t--;
  *++t = '\0';

  return s;
}

static char *
get_token (char **line)
{
  char *command;
  while (1)
    {
      command = (char *) strsep (line, " ,");
      if (!command)
        break;
      if (*(command))
        break;
    }
  
  if (command)
    return strdup (stripwhite (command));
  return command;
}

static int 
validate_delete_list_string (char *delete_list_string)
{
  char *dlist = NULL;
  
  if (delete_list_string == NULL)
    return (-1);
  
  dlist = strdupa (delete_list_string);
  while (delete_list_string)
    {
      int value = 0;
      char *token = NULL;
      char *str = NULL;
      char *tail = NULL;
      int errnum = 0;
      
      token = get_token (&dlist);
      if (token == NULL)
	break;
      str = strdupa (token);
      free (token);
      
      errno = 0;
      value = strtol (str, &tail, 10);
      errnum = errno;
      
      if (errnum)
	return (-1); // overflow
      
      if (tail[0] != '\0')
	return (-1); // invalid integer format
      
      if (value < 0)
	return (-1); // negative number
    }
  
  return 0;
}

static int 
get_delete_record_count (char *delete_list_string)
{
  int count = 0;
  char *dlist = NULL;
  
  if (delete_list_string == NULL)
    return (-1);
  
  dlist = strdupa (delete_list_string);
  while (delete_list_string)
    {
      char *str = NULL;
      
      str = get_token (&dlist);
      if (str == NULL)
	break;
      free (str);
      
      count++;
    }
  
  return count;
}

static int 
get_delete_record_list (char *delete_list_string, int *records, int count)
{
  int i = 0;
  char *dlist = NULL;
  
  if (delete_list_string == NULL || 
      records == NULL)
    return (-1);
  
  dlist = strdupa (delete_list_string);
  for (i = 0; i < count; i++)
    {
      int value = 0;
      char *str = NULL;
      
      str = get_token (&dlist);
      if (str == NULL)
	break;
      
      value = strtol (str, NULL, 10);
      records[i] = value;
      
      free (str);
    }
  
  return 0;
}

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmi_sel_arguments *cmd_args = state->input;
  error_t ret;

  switch (key)
    {
    case INFO_KEY:
      cmd_args->info_wanted = 1;
      break;
    case DELETE_KEY:
      cmd_args->delete_wanted = 1;
      {
	char *delete_arg = strdupa (arg);
	
	if (validate_delete_list_string (delete_arg) == -1)
	  {
	    fprintf (stderr, "invalid integer in delete list\n");
	    argp_usage (state);
	    break;
	  }
	
	if (cmd_args->delete_record_list)
	  free (cmd_args->delete_record_list);
	
	cmd_args->delete_record_list_length = 
	  get_delete_record_count (delete_arg);
	cmd_args->delete_record_list = 
	  calloc (cmd_args->delete_record_list_length, sizeof (int));
	get_delete_record_list (delete_arg, 
				cmd_args->delete_record_list, 
				cmd_args->delete_record_list_length);
      }
      break;
    case DELETE_ALL_KEY:
      cmd_args->delete_all_wanted = 1;
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
	start_ptr = strchr (range_str, '-');
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
  init_common_cmd_args (&(cmd_args->common));
  init_sdr_cmd_args (&(cmd_args->sdr));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->info_wanted = 0;
  cmd_args->delete_wanted = 0;
  cmd_args->delete_record_list = NULL;
  cmd_args->delete_record_list_length = 0;
  cmd_args->delete_all_wanted = 0;
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
