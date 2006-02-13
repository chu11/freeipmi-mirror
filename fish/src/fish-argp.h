/* 
   $Id: fish-argp.h,v 1.3.2.2 2006-02-13 22:10:06 chu11 Exp $ 
   
   fish-argp.h - fish command line argument parser.
   
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

#ifndef _FISH_ARGP_H
#define _FISH_ARGP_H

#define SCRIPT_FILE_KEY     's'

struct arguments 
{
  struct common_cmd_args common;
  char *script_file;
};

void fi_show_version (FILE *stream, struct argp_state *state);
void fi_argp_parse (int argc, char **argv);
struct arguments *fi_get_arguments ();
int fi_set_arguments (struct arguments *args);
void fi_get_script_args (int *argc, char ***argv);

int get_script_argc ();
char **get_script_argv ();

#endif
