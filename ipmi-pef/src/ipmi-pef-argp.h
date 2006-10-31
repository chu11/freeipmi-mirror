/* 
   $Id: ipmi-pef-argp.h,v 1.2 2006-10-31 19:31:39 balamurugan Exp $ 
   
   ipmi-pef-argp.h - Platform Event Filtering utility.
   
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

#ifndef _IPMI_PEF_ARGP_H
#define _IPMI_PEF_ARGP_H

enum argp_option_keys
  { 
    INFO_KEY = 'i', 
    CHECKOUT_KEY = 'o', 
    COMMIT_KEY = 'c'
  };

struct arguments
{
  struct common_cmd_args common;
  int info_wanted;
  int checkout_wanted;
  char *checkout_filename;
  int commit_wanted;
  char *commit_filename;
};

void ipmi_pef_argp_parse (int argc, char **argv);
struct arguments *ipmi_pef_get_arguments ();

#endif
