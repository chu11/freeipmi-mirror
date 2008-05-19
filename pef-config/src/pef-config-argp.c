/* 
   Copyright (C) 2007-2008 FreeIPMI Core Team
   
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

#include "tool-cmdline-common.h"
#include "pef-config.h"
#include "pef-config-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI PEF [pef-config-" PACKAGE_VERSION "]\n"
"Copyright (C) 2007-2008 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "PEF-CONFIG - PEF configuration utility.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about PEF configuration.", 30},
    {"checkout", CHECKOUT_KEY, 0, 0,
     "Fetch configuration information.", 31},
    {"commit", COMMIT_KEY, 0, 0,
     "Update configuration information from a config file or key pairs.", 32},
    {"diff", DIFF_KEY, 0, 0,
     "Show differences between stored information and a config file or key pairs.", 33},
    {"filename", FILENAME_KEY, "FILENAME", 0,
     "Specify a config file for checkout/commit/diff.", 34},   
    /* legacy short-option */
    {"foobar", FILENAME_KEY_LEGACY, "FILENAME", OPTION_HIDDEN,
     "Specify a config file for checkout/commit/diff.", 35},
    {"key-pair", KEYPAIR_KEY, "KEY-PAIR", 0,
     "Specify KEY=VALUE pairs for checkout/commit/diff.", 36},
    {"section", SECTIONS_KEY, "SECTION", 0,
     "Specify a SECTION for checkout.", 37},
    {"listsections", LIST_SECTIONS_KEY, 0, 0,
     "List available sections for checkout.", 38},
    {"verbose", VERBOSE_KEY, 0, 0,
     "Print additional detailed information.", 39},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct pef_config_arguments *cmd_args = state->input;
  struct config_keypair *kp = NULL;
  struct config_section_str *sstr = NULL;
  char *section_name = NULL;
  char *key_name = NULL;
  char *value = NULL;
  
  switch (key)
    {
    case INFO_KEY:
      if (!cmd_args->config_args.action)
        cmd_args->config_args.action = CONFIG_ACTION_INFO;
      else
        cmd_args->config_args.action = -1;
      break;
    case CHECKOUT_KEY:
      if (!cmd_args->config_args.action)
        cmd_args->config_args.action = CONFIG_ACTION_CHECKOUT;
      else
        cmd_args->config_args.action = -1;
      break;
    case COMMIT_KEY:
      if (!cmd_args->config_args.action)
        cmd_args->config_args.action = CONFIG_ACTION_COMMIT;
      else
        cmd_args->config_args.action = -1;
      break;
    case DIFF_KEY:
      if (!cmd_args->config_args.action)
        cmd_args->config_args.action = CONFIG_ACTION_DIFF;
      else
        cmd_args->config_args.action = -1;
      break;
    case FILENAME_KEY:
    case FILENAME_KEY_LEGACY:
      if (cmd_args->config_args.filename) /* If specified more than once */
        free (cmd_args->config_args.filename);
      if (!(cmd_args->config_args.filename = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case KEYPAIR_KEY:
      if (config_keypair_parse_string(arg,
                                      &section_name,
                                      &key_name,
                                      &value) < 0)
        {
          /* error printed in function call */
          exit(1);
        }
      if (!(kp = config_keypair_create(section_name,
                                       key_name,
                                       value)))
        {
          fprintf(stderr,
                  "config_keypair_create error\n");
          exit(1);
        }
      if (config_keypair_append(&(cmd_args->config_args.keypairs),
                                kp) < 0)
        {
          /* error printed in function call */
          exit(1);
        }
      if (section_name)
        free(section_name);
      section_name = NULL;
      if (key_name)
        free(key_name);
      key_name = NULL;
      if (value)
        free(value);
      value = NULL;
      kp = NULL;
      break;
    case SECTIONS_KEY:
      if (!(sstr = config_section_str_create(arg)))
        {
          fprintf(stderr,
                  "config_section_str_create error\n");
          exit(1);
        }
      if (config_section_str_append(&(cmd_args->config_args.section_strs),
                                    sstr) < 0)
        {
          /* error printed in function call */
          exit(1);
        }
      sstr = NULL;
      break;
    case LIST_SECTIONS_KEY:
      if (!cmd_args->config_args.action)
        cmd_args->config_args.action = CONFIG_ACTION_LIST_SECTIONS;
      else
        cmd_args->config_args.action = -1;
      break;
    case VERBOSE_KEY:
      cmd_args->config_args.verbose = 1;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return common_parse_opt (key, arg, state, 
			       &(cmd_args->config_args.common));
    }
  
  return 0;
}

void
_pef_config_args_validate (struct pef_config_arguments *cmd_args)
{
  // action is non 0 and -1
  if (! cmd_args->config_args.action || cmd_args->config_args.action == -1)
    {
      fprintf (stderr,
               "Exactly one of --info, --checkout, --commit, --diff, or --listsections MUST be given\n");
      exit(1);
    }
  
  config_args_validate(&(cmd_args->config_args));
}

void 
pef_config_argp_parse (int argc, char **argv, struct pef_config_arguments *cmd_args)
{
  init_common_cmd_args_admin (&(cmd_args->config_args.common));
  cmd_args->config_args.action = 0;
  cmd_args->config_args.verbose = 0;
  cmd_args->config_args.filename = NULL;
  cmd_args->config_args.keypairs = NULL;
  cmd_args->config_args.section_strs = NULL;

  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->config_args.common));
  _pef_config_args_validate (cmd_args);
}

