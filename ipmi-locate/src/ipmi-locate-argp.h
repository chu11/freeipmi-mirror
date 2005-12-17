/* 
   $Id: ipmi-locate-argp.h,v 1.1 2005-12-17 21:28:09 balamurugan Exp $ 
   
   ipmi-locate-argp.h - command line argument parser.
   
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

#ifndef _FISH_ARGP_H
#define _FISH_ARGP_H

enum argp_option_keys
  { 
    QUIET_KEY = 'q', 
    VERBOSE_KEY = 'v', 
    DUMMY_KEY = 129, 
    BRIEF_KEY
  };

struct arguments
{
  int quiet;
  int brief;
  int verbose;
};

void fi_show_version (FILE *stream, struct argp_state *state);
void fi_argp_parse (int argc, char **argv);
struct arguments *fi_get_arguments ();
int fi_set_arguments (struct arguments *args);

#endif
