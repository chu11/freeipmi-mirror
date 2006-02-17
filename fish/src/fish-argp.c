/* 
   $Id: fish-argp.c,v 1.1.2.4 2006-02-17 23:59:49 chu11 Exp $ 
   
   fish-argp.c - fish command line argument parser.
   
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

#include "common.h"

static struct arguments cmd_args;
static int script_arg_start_index = 0;
static int script_argc = 0;
static char **script_argv = NULL;

static error_t parse_opt (int key, char *arg, struct argp_state *state);

static char version_doc[] = 
"FreeIPMI Shell [fish-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

void (*argp_program_version_hook) (FILE *, struct argp_state *) = fi_show_version;
const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "Free IPMI SHell - an extensible console based shell "
"for managing large number of IPMI compatible systems.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS, 
    {"script-file", SCRIPT_FILE_KEY, "SCRIPT-FILE", 0, 
     "Load and execute given SCRIPT-FILE."}, 
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct arguments *args = state->input;
  
  switch (key)
    {
    case SCRIPT_FILE_KEY:
      args->script_file = strdup (arg);
      script_arg_start_index = state->next;
      /* consume rest of args */
      state->next = state->argc;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return common_parse_opt (key, arg, state,
                               &(args->common));
    }
  
  return 0;
}

static char **
make_script_argv (int start_arg_index, int len, char **argv)
{
  int i;
  char **sargv = NULL;
  
  sargv = (char **) malloc (sizeof (char *) * len);
  
  for (i = 0; i < len; i++)
    {
      if (i == 0)
	{
	  char *script_name = NULL;
	  script_name = strchr (argv[start_arg_index + i], '=');
	  if (script_name != NULL)
	    {
	      sargv[i] = script_name + 1;
	      continue;
	    }
	}
      sargv[i] = argv[start_arg_index + i];
    }
  
  return sargv;
}

void 
fi_show_version (FILE *stream, struct argp_state *state)
{
  (void) state;
  
  fprintf (stream, "%s\n", version_doc);
}

void 
fi_argp_parse (int argc, char **argv)
{
  init_common_cmd_args (&(cmd_args.common));
  cmd_args.script_file = NULL;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, &cmd_args);
  
  if (script_arg_start_index != 0)
    {
      if (script_arg_start_index == argc)
	script_argc = 1;
      else 
	script_argc = argc - script_arg_start_index + 1;
      script_argv = make_script_argv (script_arg_start_index - 1, 
				      script_argc, argv);
    }
}

struct arguments *
fi_get_arguments ()
{
  return &cmd_args;
}

int 
fi_set_arguments (struct arguments *args)
{
  cmd_args.common.disable_auto_probe = args->common.disable_auto_probe;
  cmd_args.common.driver_type = args->common.driver_type;
  cmd_args.common.driver_address = args->common.driver_address;
  
  if (cmd_args.common.driver_device)
    xfree (cmd_args.common.driver_device);
  if (args->common.driver_device)
    cmd_args.common.driver_device = strdup (args->common.driver_device);
  else 
    cmd_args.common.driver_device = NULL;
  
  if (cmd_args.common.host)
    xfree (cmd_args.common.host);
  if (args->common.host)
    cmd_args.common.host = strdup (args->common.host);
  else 
    cmd_args.common.host = NULL;
  
  if (cmd_args.common.username)
    xfree (cmd_args.common.username);
  if (args->common.username)
    cmd_args.common.username = strdup (args->common.username);
  else 
    cmd_args.common.username = NULL;
  
  if (cmd_args.common.password)
    xfree (cmd_args.common.password);
  if (args->common.password)
    cmd_args.common.password = strdup (args->common.password);
  else 
    cmd_args.common.password = NULL;
  
  cmd_args.common.authentication_type = args->common.authentication_type;
  cmd_args.common.privilege_level = args->common.privilege_level;
  
  if (cmd_args.script_file)
    xfree (cmd_args.script_file);
  if (args->script_file)
    cmd_args.script_file = strdup (args->script_file);
  else 
    cmd_args.script_file = NULL;
  
  return (0);
}

void 
fi_get_script_args (int *argc, char ***argv)
{
  *argc = script_argc;
  *argv = script_argv;
}

int 
get_script_argc ()
{
  return script_argc;
}

char **
get_script_argv ()
{
  return script_argv;
}

