/* 
   $Id: ipmi-sensors-argp.h,v 1.2 2006-08-07 20:56:33 chu11 Exp $ 
   
   ipmi-sensors-argp.h - IPMI Sensors utility.
   
   Copyright (C) 2006 FreeIPMI Core Team
   
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

#ifndef _IPMI_SENSORS_ARGP_H
#define _IPMI_SENSORS_ARGP_H

enum argp_option_keys
  { 
    VERBOSE_KEY = 'v', 
    SDR_INFO_KEY = 'i', 
    FLUSH_CACHE_KEY = 'f', 
    LIST_GROUPS_KEY = 'L', 
    GROUP_KEY = 'g', 
    SENSORS_LIST_KEY = 's'
  };

struct arguments
{
  struct common_cmd_args common;
  int verbose_wanted;
  int verbose_count;
  int sdr_info_wanted;
  int flush_cache_wanted;
  int list_groups_wanted;
  int group_wanted;
  char *group;
  int sensors_list_wanted;
  unsigned int *sensors_list;
  unsigned int sensors_list_length;
};

void ipmi_sensors_argp_parse (int argc, char **argv);
struct arguments *ipmi_sensors_get_arguments ();

#endif
