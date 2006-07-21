/* 
   $Id: ipmi-raw-argp.c,v 1.9 2006-07-21 23:31:18 chu11 Exp $ 
   
   ipmi-raw-argp.c - ipmi-raw command line argument parser.
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
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

#include "argp-common.h"
#include "ipmi-raw-argp.h"

static struct arguments cmd_args;

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"FreeIPMI Raw [ipmi-raw-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI Raw - executes IPMI commands by hex values.";

static char args_doc[] = "[COMMAND-HEX-BYTES]";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS, 
    {"file", CMD_FILE_KEY, "CMD-FILE", 0, 
     "Read command requests from CMD-FILE.", 12}, 
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct arguments *cmd_args = state->input;
  
  switch (key)
    {
    case CMD_FILE_KEY:
      cmd_args->cmd_file = strdup (arg);
      break;
    case ARGP_KEY_ARG:
      {
	int i;
	long value;
	
	for (i = 0; arg[i] != (char) NULL; i++)
	  {
	    if (i > 2)
	      {
		fprintf (stderr, "%s: invalid hex byte argument\n", 
			 program_invocation_short_name);
		argp_usage (state);
		return (-1);
	      }
	    
	    if (isxdigit (arg[i]) == 0)
	      {
		fprintf (stderr, "%s: invalid hex byte argument\n", 
			 program_invocation_short_name);
		argp_usage (state);
		return (-1);
	      }
	  }
	
	value = strtol (arg, (char **) NULL, 16);
	cmd_args->cmd[cmd_args->cmd_length++] = (uint8_t) value;
	
	break;
      }
    case ARGP_KEY_END:
      break;
    default:
      return common_parse_opt (key, arg, state, 
			       &(cmd_args->common));
    }
  
  return 0;
}

void 
ipmi_raw_argp_parse (int argc, char **argv)
{
  init_common_cmd_args (&(cmd_args.common));
  cmd_args.cmd_file = NULL;
  memset (cmd_args.cmd, 0, ARG_MAX);
  cmd_args.cmd_length = 0;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, &cmd_args);
}

struct arguments *
ipmi_raw_get_arguments ()
{
  return &cmd_args;
}

