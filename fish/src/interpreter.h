/* 
   interpreter.h: file contains read-eval-print loop and
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

#ifndef _INTERPRETER_H
#define _INTERPRETER_H

#define EX_DYNAMIC_COMMANDS "fi-commands"

unsigned char get_bell_state (void);

void toggle_bell (void);
char fi_quit ();
void set_fish_prompt (char *prompt);
char * get_fish_prompt ();

void register_command (SCM command);
void unregister_command (SCM command);
char *command_generator (const char *text, int stat);
char **complete_text (char *text, int start, int end);
void read_eval_print_loop ();
void syntax_error (char *command);
void interpreter (char *cmd_line);
void dynamic_command_handler (char *dynamic_command, char *cmd_line);
void dynamic_help_handler (char *dynamic_command, int all);
void unknown_command_handler (char *command);

#endif
