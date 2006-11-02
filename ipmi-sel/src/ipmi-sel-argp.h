/* 
   $Id: ipmi-sel-argp.h,v 1.5 2006-11-02 17:35:55 balamurugan Exp $ 
   
   ipmi-sel-argp.h - System Event Logger utility.
   
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

#ifndef _IPMI_SEL_ARGP_H
#define _IPMI_SEL_ARGP_H

enum argp_option_keys
  { 
    INFO_KEY = 'i', 
    DELETE_KEY = 'd', 
    DELETE_ALL_KEY = 'c', 
    HEX_DUMP_KEY = 'x', 
    DELETE_RANGE_KEY = 'R',
    FLUSH_CACHE_KEY = 'f', 
    SDR_CACHE_DIR_KEY = 200
  };

struct arguments
{
  struct common_cmd_args common;
  int info_wanted;
  int delete_wanted;
  int *delete_record_list;
  int delete_record_list_length;
  int delete_all_wanted;
  int delete_range_wanted;
  int delete_range1;
  int delete_range2;
  int hex_dump_wanted;
  char *hex_dump_filename;
  int flush_cache_wanted;
  int sdr_cache_dir_wanted;
  char *sdr_cache_dir;
};

void ipmi_sel_argp_parse (int argc, char **argv);
struct arguments *ipmi_sel_get_arguments ();

#endif
