/* 
   ipmi-chassis-argp.c - IPMI Chassis ARGP parser
   
   Copyright (C) 2007 FreeIPMI Core Team
   
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

#include <stdlib.h>
#include <argp.h>

#include "argp-common.h"
#include "ipmi-chassis.h"
#include "ipmi-chassis-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI Chassis [ipmi-chassis-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI Chassis - Control IPMI Chassis.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS,
    {"get-capabilities", 'c', NULL, 0, "Get the chassis capabilities", 13},
    {"get-status", 's', NULL, 0, "Get the chassis status", 14},
    {"chassis-control", 'C', NULL, 0, "Control the chassis", 15},
    {"chassis-reset", 'R', NULL, 0, "Reset the chassis (Recommended to use --chassis-control)", 16},
    {"chassis-identify", 'I', NULL, 0, "Chassis Identification", 17},
    {"set-capabilities", 'S', NULL, 0, "Set the chassis capabilities", 18},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmi_chassis_arguments *cmd_args = state->input;
  
  switch (key)
    {
    case 'c':
      if (cmd_args->cmd != -1) {
	fprintf (stderr, "Error: Only one command at a time\n");
	return -1;
      }
      cmd_args->cmd = IPMI_CMD_GET_CHASSIS_CAPABILITIES;
      break;
    case 's':
      if (cmd_args->cmd != -1) {
	fprintf (stderr, "Error: Only one command at a time\n");
	return -1;
      }
      cmd_args->cmd = IPMI_CMD_GET_CHASSIS_STATUS;
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
ipmi_chassis_argp_parse (int argc, char **argv,
			 struct ipmi_chassis_arguments *cmd_args)
{
  error_t err;
  init_common_cmd_args (&(cmd_args->common));
  cmd_args->cmd = -1;
  err = argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);

  if (err != 0)
    exit (EINVAL);
}
