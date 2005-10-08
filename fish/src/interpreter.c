/*
   interpreter.c: file contains read-eval-print loop and
   supportive functions

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

#include "common.h"

char *commands[] = { "eval", "help", "load", "quit", "reset", "status", "toggle", NULL};

char quit_flag = 0;
SCM dynamic_commands = SCM_UNSPECIFIED;
char *fish_prompt = NULL;

unsigned char toggle_bell_state = 0;	// bell - OFF / ON

unsigned char
get_bell_state (void)
{
  return toggle_bell_state;
}

void set_fish_prompt (char *prompt)
{
  fish_prompt = prompt;
}

char * get_fish_prompt ()
{
  if (fish_prompt)
    return fish_prompt;
  else
    return (FI_DEFAULT_PROMPT_STRING);
}

void
toggle_bell (void)
{
  toggle_bell_state = ~toggle_bell_state;
}

char
fi_quit ()
{
  quit_flag = 1;
  return (quit_flag);
}

void
register_command (SCM command)
{
  if (gh_list_p (dynamic_commands) != 1)
    {// not a list
      dynamic_commands = gh_list (command, SCM_UNDEFINED);
      gh_define (EX_DYNAMIC_COMMANDS, dynamic_commands);
    }
  else
    {
      dynamic_commands =
        gh_append2 (dynamic_commands,
                    gh_list (command, SCM_UNDEFINED));
      gh_define (EX_DYNAMIC_COMMANDS, dynamic_commands);
    }
}

void
unregister_command (SCM command)
{
  SCM tmp_scm;
  char *dynamic_cmd = NULL;
  char *dynamic_command = NULL;
  int dynamic_command_index = 0;
  int dynamic_commands_count = 0;

  if (gh_list_p (dynamic_commands) != 1)
    return;
  
  dynamic_command = gh_scm2newstr (command, NULL);

  if (dynamic_command == NULL)
    return;

  dynamic_commands_count = gh_length (dynamic_commands);

  while (dynamic_command_index < dynamic_commands_count)
    {
      tmp_scm = gh_list_ref (dynamic_commands, gh_ulong2scm (dynamic_command_index++));
      dynamic_cmd = gh_scm2newstr (gh_list_ref (tmp_scm,
						 gh_ulong2scm (0)),
				    NULL);
      if (dynamic_cmd && strcasecmp (dynamic_cmd, dynamic_command) == 0)
	{
	  dynamic_commands = scm_delete (tmp_scm, dynamic_commands);
	  gh_define (EX_DYNAMIC_COMMANDS, dynamic_commands);
	  return;
	}
    }
}

char *
command_generator (const char *text, int stat)
{
  static int i, len;
  static int toggle_index;
  static unsigned long dynamic_cmd_index;
  unsigned long dynamic_commands_count;
  char *dynamic_command, *command;

  if (!stat)
    {
      len = strlen (text);
      i = 0;
      toggle_index  = 0;
      dynamic_cmd_index = 0;
    }

  /* FIXME: see TODO or ask <ab@gnu.org.in> 
  // if your command takes node_name as argument then you
  // will have to add ur command in this check list
  if ((strncmp (rl_line_buffer, "who ", 4) == 0)
      || (strncmp (rl_line_buffer, "poweroff ", 9) == 0)
      || (strncmp (rl_line_buffer, "poweron ", 8) == 0)
      || (strncmp (rl_line_buffer, "status ", 7) == 0))
    {
      struct ipmi_node *temp_node;
      char *node_hostname;
      while((temp_node = (struct ipmi_node *) get_nth_node (i)))
	{
	  i++;
	  node_hostname = temp_node->hostname;
	  if (!node_hostname || (node_hostname[0] == 0))
	    continue;

	  if (strncasecmp (node_hostname, text, len) == 0)
	    return strdup (node_hostname);
	}
      return NULL;
    }
  */
  // ?toggle argument tabs
  if (strncmp (rl_line_buffer, "?toggle ", 8) == 0)
    {
      switch (toggle_index)
	{
	case 0:
	  toggle_index++;
	  if (strncasecmp ("bell", text, len) == 0)
	    return strdup ("bell");
	default:
	  return NULL;
	}
    }

  // for dynamic commands
  if (gh_list_p (dynamic_commands))
    {
      dynamic_commands_count = gh_length (dynamic_commands);
      while (dynamic_cmd_index < dynamic_commands_count)
	{
	  dynamic_command = 
	    gh_scm2newstr (gh_list_ref (gh_list_ref (dynamic_commands, 
					gh_ulong2scm (dynamic_cmd_index++)),
					gh_ulong2scm (0)),
			   NULL);
	  if (dynamic_command && strncasecmp (dynamic_command, text, len) == 0)
	    return strdup (dynamic_command);
	}
    }
  
  // for normal command
  while ((command = commands[i]))
    {
      i++;
      if (strncasecmp (command, text, len) == 0)
	return (char *) strdup (command);
    }
  return (char *) NULL;
}

char **
complete_text (char *text, int start, int end)
{
  char **matches;
  matches = (char **) NULL;

#if defined (HAVE_RL_COMPLETION_MATCHES)
  matches = (char **) rl_completion_matches (text, command_generator);
#elif defined (HAVE_COMPLETION_MATCHES)
  matches = (char **) completion_matches (text, command_generator);
#else
#error "Fish(PANIC): strange readline library, no compatible "rl_completion_matches" call, please submit BUG-REPORT"
#endif
  return matches;
}

void
read_eval_print_loop (void)
{
  char *line, *cmd_line;

  /* register readline auto-completion callback handler 
   */
  rl_attempted_completion_function = (CPPFunction *) complete_text;

  do {
    line = readline (get_fish_prompt ());

    if (!line)
      {
	fi_quit ();
	return;
      }
  
    cmd_line = stripwhite (line);
    if (strlen (cmd_line) == 0)
      continue;
    
    add_history (cmd_line);
    interpreter (cmd_line);
    
    if (line)
      free (line);
  } while (!quit_flag);
}

void
syntax_error (char *command)
{
  fprintf (stderr, "Improper syntax, Try \"help %s\"\n", command);
}

void
interpreter (char *cmd_line)
{
  char *command = NULL;
  
  command = get_token (&cmd_line);
  
  if (cmd_line)
         cmd_line = stripwhite (cmd_line);
  
  if (strcasecmp (command, "eval") == 0)
    {
      if (cmd_line && *cmd_line)
	command_eval_scheme_str (cmd_line);
      else
	syntax_error (command);
    }
  else if (strcasecmp (command, "help") == 0)
    {
      // help arguments are optional
      command_show_help (cmd_line);
    }
  else if (strcasecmp (command, "load") == 0)
    {
      if (cmd_line && *cmd_line)
	command_load_scheme_file (cmd_line);
      else
	syntax_error (command);
    }
  else if (strcasecmp (command, "quit") == 0)
    {
      fi_quit ();
    }
  else if (strcasecmp (command, "toggle") == 0)
    {
      if (cmd_line && *cmd_line)
	command_toggle (cmd_line);
      else
	syntax_error (command);
    }
  else if (gh_list_p (dynamic_commands) == 1)
    {
      int i = 0;
      char *dynamic_command = NULL;

      for (i = 0; i < gh_length (dynamic_commands); i++)
	{
	  dynamic_command = gh_scm2newstr (gh_list_ref (gh_list_ref (dynamic_commands, 
								     gh_ulong2scm (i)), 
							gh_ulong2scm (0)),
					   NULL);
	  if (dynamic_command && strcasecmp (dynamic_command, command) == 0)
	    {
	      dynamic_command_handler (dynamic_command, cmd_line);
	      break;
	    }
	  free (dynamic_command);
	}
      
      if (i == gh_length (dynamic_commands))
	{
	  unknown_command_handler (command);
	  return;
	}
    }
  return;
}

void 
dynamic_command_handler (char *dynamic_command, char *cmd_line)
{
  /*
    (and (procedure? 'dynamic_command) 
         (dynamic_command '(arg1 arg2 ... argN)))
  */
  
  char *eval_string = NULL;
  
  if (dynamic_command == NULL)
    return;
  
  asprintf (&eval_string, 
	    "(and (procedure? %s) (%s '(%s)))", 
	    dynamic_command, 
	    dynamic_command, 
	    (cmd_line ? cmd_line : ""));
  
  command_eval_scheme_str (eval_string);
  
  free (eval_string);
}

void
dynamic_help_handler (char *dynamic_command, int all)
{
  char *command = NULL;
  if (gh_list_p (dynamic_commands) == 1)
    {
      int i;
      SCM tmp_scm;
      char *dynamic_syntax = NULL;
      
      for (i=0; i < gh_length (dynamic_commands); i++)
	{
	  dynamic_syntax = NULL;
	  tmp_scm = 
	    gh_list_ref (dynamic_commands, gh_ulong2scm (i));
	  command = 
	    gh_scm2newstr (gh_list_ref (tmp_scm, gh_ulong2scm (0)), NULL);
	  if (all || (strcasecmp (command, dynamic_command) == 0))
	    {
	      dynamic_syntax = 
		gh_scm2newstr (gh_list_ref (tmp_scm, gh_ulong2scm (1)),
			       NULL);
	      if (dynamic_syntax != NULL)
		printf ("%s\n", dynamic_syntax);
	    }
	}
    }
}

void
unknown_command_handler (char *command)
{
  fprintf (stderr, "Undefined command: \"%s\".  Try \"help\".\n", command);
}

