/* 
   $Id: bmc-info-argp.c,v 1.7 2007-01-22 21:51:25 chu11 Exp $ 
   
   bmc-info-argp.c - displays BMC information.
   
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

#include <argp.h>

#include "argp-common.h"
#include "bmc-info-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"BMC Information [bmc-info-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "BMC Info - display BMC information.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS, 
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct bmc_info_arguments *cmd_args = state->input;
  
  switch (key)
    {
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
bmc_info_argp_parse (int argc, char **argv, struct bmc_info_arguments *cmd_args)
{
  init_common_cmd_args (&(cmd_args->common));
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
}
