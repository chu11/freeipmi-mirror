/* 
   $Id: ipmi-sel-argp.c,v 1.1 2006-06-27 18:26:49 balamurugan Exp $ 
   
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

#include "argp-common.h"
#include "ipmi-sel-argp.h"

static struct arguments cmd_args;

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
    ARGP_COMMON_OPTIONS, 
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about SEL.", 9},
    {"delete",     DELETE_KEY,     "REC-LIST", 0, 
     "Delete given SEL records entry."},
    {"delete-all", DELETE_ALL_KEY, 0, 0, 
     "Delete all SEL entries."},
    {"hex-dump",   HEX_DUMP_KEY,   "FILE", OPTION_ARG_OPTIONAL, 
     "Hex-dump SEL entries optionally to FILE."},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct arguments *cmd_args = state->input;
  
  switch (key)
    {
    case INFO_KEY:
      cmd_args->info_wanted = 1;
      break;
    case DELETE_KEY:
      cmd_args->delete_wanted = 1;
      printf ("delete_list: [%s]\n", arg);
      break;
    case DELETE_ALL_KEY:
      cmd_args->delete_all_wanted = 1;
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
      return common_parse_opt (key, arg, state, 
			       &(cmd_args->common));
    }
  
  return 0;
}

void 
ipmi_sel_argp_parse (int argc, char **argv)
{
  init_common_cmd_args (&(cmd_args.common));
  cmd_args.info_wanted = 0;
  cmd_args.delete_wanted = 0;
  cmd_args.delete_record_list = NULL;
  cmd_args.delete_record_list_length = 0;
  cmd_args.delete_all_wanted = 0;
  cmd_args.hex_dump_wanted = 0;
  cmd_args.hex_dump_filename = NULL;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, &cmd_args);
}

struct arguments *
ipmi_sel_get_arguments ()
{
  return &cmd_args;
}

