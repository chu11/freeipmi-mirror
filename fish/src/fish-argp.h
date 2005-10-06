/* 
   $Id: fish-argp.h,v 1.1 2005-10-06 10:41:09 balamurugan Exp $ 
   
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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifndef _FISH_ARGP_H
#define _FISH_ARGP_H

enum argp_option_keys
  { 
    QUIET_KEY = 'q', 
    VERBOSE_KEY = 'v', 
    SCRIPT_FILE_KEY = 's', 
    DUMMY_KEY = 129, 
    BRIEF_KEY, 
    POLL_INTERVAL_KEY, 
    SMS_IO_BASE_KEY, 
    HOST_KEY = 'h', 
    USERNAME_KEY = 'u', 
    PASSWORD_KEY = 'p', 
    AUTH_TYPE_KEY = 'a', 
    PRIV_LEVEL_KEY = 'l'
  };

struct arguments
{
  int quiet;
  int brief;
  int verbose;
  char *script_file;
  int poll_interval;
  unsigned int sms_io_base;
  char *host;
  char *username;
  char *password;
  int auth_type;
  int priv_level;
};

void fi_show_version (FILE *stream, struct argp_state *state);
void fi_argp_parse (int argc, char **argv);
struct arguments *fi_get_arguments ();
void fi_get_script_args (int *argc, char ***argv);

int get_script_argc ();
char **get_script_argv ();
void fi_set_sms_io_base (unsigned int io_base);
void set_driver_poll_interval (int driver_poll_interval_value);

#endif
