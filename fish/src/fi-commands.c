/* 
   fi-commands.c: fish commands registered to the interpreter
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include <stdlib.h>
#include <assert.h>
#include <guile/gh.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <freeipmi.h>
#include <error.h>

#include "fish.h"
#include "fi-utils.h"
#include "fi-commands.h"
#include "interpreter.h"
#include "ipmi-wrapper.h"

extern int errno;

void
command_eval_scheme_str (char *cmd_line)
{
  gh_eval_str_with_stack_saving_handler (cmd_line);
  scm_force_output (scm_current_output_port ());
}

void
command_show_help (char *cmd_line)
{
  char *command;
  int all = 0;
  
  command = get_token (&cmd_line);
  if (command == (char *) NULL)
    all = 1;
  
  if (all || (strcasecmp (command, "eval") == 0))
    {
      printf ("eval SCHEME-CODE\n"
	      "\t- evaluates the SCHEME-CODE\n");
    }
  if (all || (strcasecmp (command, "load") == 0))
    {
      printf ("load SCHEME-FILE\n"
	      "\t- loads and evaluates the SCHEME-FILE\n");
    }
  if (all || (strcasecmp (command, "toggle") == 0))
    {
      printf ("toggle VARIABLE\n"
	      "\t- toggles the state of VARIABLE to ON and OFF\n"
	      "\tVariables:\n"
	      "\t\tbell    - OFF / ON\n");
    }
  if (all || (strcasecmp (command, "quit") == 0))
    {
      printf ("quit\n"
	      "\t- quit the shell\n");
    }
  if (all || (strcasecmp (command, "help") == 0))
    {
      printf ("help [COMMAND]\n"
	      "\t- display elaborate help on this COMMAND\n");
    }
  /* adds help facility to your new command
     if (all || (strcasecmp (command, "my_command") == 0))
     {
     printf("my_command [optional argument]\n"
     "\t- my_command really freaks ;-)\n");
     }
  */
  dynamic_help_handler (command, all);
}


void
command_load_scheme_file (char *cmd_line)
{
  gh_eval_file_with_standard_handler (cmd_line);
  scm_force_output (scm_current_output_port ());
}

void
command_toggle (char *cmd_line)
{
  char *toggle_arg = NULL;

  toggle_arg = get_token (&cmd_line);
  if (toggle_arg == (char *) NULL)
    {
      fprintf (stderr,
	       "Invalid argument to \"toggle\", see \"help toggle\""
	       "for more info\n");
      return;
    }

  if (strcasecmp (toggle_arg, "bell") == 0)
    {
      command_bell ();
      return;
    }

  /* Example: status mode */
  /* if (strcasecmp (toggle_arg, "status") == 0)
    {
      toggle_status ();
      if (get_status_mode ())
	  puts ("Buddy status notifications - [SHOW]");
      else
	  puts ("Buddy status notifications - [HIDE]");
    }
  */
}

void
command_bell ()
{
  toggle_bell ();

  if (get_bell_state ())
    {
      puts ("Bell sound  - [ON]");
#if defined (HAVE_RL_DING)
      rl_ding ();
#else
#if defined (HAVE_DING)
      ding ();
#else
#error "Fish(PANIC): strange readline library, no compatible "rl_ding" call, please submit BUG-REPORT"
#endif
#endif
    }
  else
    {
      puts ("Bell sound - [OFF]");
    }
}
