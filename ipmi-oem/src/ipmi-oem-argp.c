/* 
   Copyright (C) 2008 FreeIPMI Core Team
   
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

#include "tool-cmdline-common.h"
#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"

#include "freeipmi-portability.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"FreeIPMI Oem [ipmi-oem-" PACKAGE_VERSION "]\n"
"Copyright (C) 2008 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI OEM - executes OEM specific IPMI commands.";

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
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmi_oem_arguments *cmd_args = state->input;
  error_t ret;
  
  switch (key)
    {
    case ARGP_KEY_ARG:
      {
        if (!cmd_args->oem_id)
          {
            if (!(cmd_args->oem_id = strdup(arg)))
              {
                perror("strdup");
                exit(1);
              }
            break;
          }
        else if (!cmd_args->oem_cmd)
          {
            if (!(cmd_args->oem_cmd = strdup(arg)))
              {
                perror("strdup");
                exit(1);
              }
            break;
          }
        else 
          {
            if (cmd_args->oem_options_count < ARG_MAX)
              {
                if (!(cmd_args->oem_options[cmd_args->oem_options_count] = strdup(arg)))
                  {
                    perror("strdup");
                    exit(1);
                  }
                cmd_args->oem_options_count++;
                break;
              }
          }
	break;
      }
    case ARGP_KEY_END:
      break;
    default:
      ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->hostrange));
      return ret;
    }
  
  return 0;
}

void 
ipmi_oem_argp_parse (int argc, char **argv, struct ipmi_oem_arguments *cmd_args)
{
  init_common_cmd_args (&(cmd_args->common));
  init_hostrange_cmd_args (&(cmd_args->hostrange));

  cmd_args->oem_id = NULL;
  cmd_args->oem_cmd = NULL;
  memset (cmd_args->oem_options, 0, sizeof(cmd_args->oem_options));
  cmd_args->oem_options_count = 0;

  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}


