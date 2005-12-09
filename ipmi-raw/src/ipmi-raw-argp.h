/* 
   $Id: ipmi-raw-argp.h,v 1.1 2005-12-09 01:11:32 balamurugan Exp $ 
   
   ipmi-raw-argp.h - ipmi-raw command line argument parser.
   
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

#ifndef _IPMI_RAW_ARGP_H
#define _IPMI_RAW_ARGP_H

enum argp_option_keys
  { 
    QUIET_KEY = 'q', 
    VERBOSE_KEY = 'v', 
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

#endif
