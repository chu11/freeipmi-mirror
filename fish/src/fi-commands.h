/* 
   fi-commands.h: fish commands registered to the interpreter
   
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


#ifndef _FI_COMMANDS_H
#define _FI_COMMANDS_H

int command_ipmi_ping (char *cmd_line);
void command_show_help (char *cmd_line);
void command_default_handler (char *command, char *cmd_line);
void command_eval_scheme_str (char *cmd_line);
void command_load_scheme_file (char *cmd_line);
void command_toggle (char *cmd_line);
void command_bell ();

#endif
