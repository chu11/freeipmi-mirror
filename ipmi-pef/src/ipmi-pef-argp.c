/* 
   $Id: ipmi-pef-argp.c,v 1.5 2007-05-15 18:34:17 balamurugan Exp $ 
   
   ipmi-pef-argp.c - Platform Event Filtering utility.
   
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

#include "argp-common.h"
#include "ipmi-pef-argp.h"

#include "freeipmi-portability.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI PEF [ipmi-pef-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI PEF - PEF configuration utility.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS, 
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about PEF.", 13},
    {"checkout",   CHECKOUT_KEY,   "FILE", OPTION_ARG_OPTIONAL,
     "Action is to GET the PEF event filter tables", 14},
    {"commit",     COMMIT_KEY,     "FILE", 0,
     "Action is to UPDATE the PEF event filter tables", 15},
    {"alert-policy-table", ALERT_POLICY_TABLE_KEY, 0, 0, 
     "Do checkout/commit of Alert Policy Table.", 16},
    {"lan-conf", LAN_ALERT_DESTINATION_KEY, 0, 0, 
     "Do checkout/commit of PEF specific LAN configuration.", 17},
    {"community-string", COMMUNITY_STRING_KEY, "STRING", OPTION_ARG_OPTIONAL, 
     "Do checkout/commit of Community String", 18},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmi_pef_arguments *cmd_args = state->input;
  
  switch (key)
    {
    case INFO_KEY:
      cmd_args->info_wanted = 1;
      break;
    case CHECKOUT_KEY:
      cmd_args->checkout_wanted = 1;
      if (arg)
	cmd_args->checkout_filename = strdup (arg);
      break;
    case COMMIT_KEY:
      cmd_args->commit_wanted = 1;
      cmd_args->commit_filename = strdup (arg);
      break;
    case ALERT_POLICY_TABLE_KEY:
      cmd_args->alert_policy_table_wanted = 1;
      break;
    case LAN_ALERT_DESTINATION_KEY:
      cmd_args->lan_alert_destination_wanted = 1;
      break;
    case COMMUNITY_STRING_KEY:
      cmd_args->community_string_wanted = 1;
      if (arg)
	cmd_args->community_string = strdup (arg);
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
ipmi_pef_argp_parse (int argc, char **argv, struct ipmi_pef_arguments *cmd_args)
{
  init_common_cmd_args (&(cmd_args->common));
  cmd_args->info_wanted = 0;
  cmd_args->checkout_wanted = 0;
  cmd_args->checkout_filename = NULL;
  cmd_args->commit_wanted = 0;
  cmd_args->commit_filename = NULL;
  cmd_args->alert_policy_table_wanted = 0;
  cmd_args->lan_alert_destination_wanted = 0;
  cmd_args->community_string_wanted = 0;
  cmd_args->community_string = NULL;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
}

