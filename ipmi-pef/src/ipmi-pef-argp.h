/* 
   $Id: ipmi-pef-argp.h,v 1.1 2006-10-06 14:18:04 balamurugan Exp $ 
   
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
    INFO_KEY = 'i'
  };

struct arguments
{
  struct common_cmd_args common;
  int info_wanted;
};

void ipmi_pef_argp_parse (int argc, char **argv);
struct arguments *ipmi_pef_get_arguments ();

#endif
