/* 
   $Id: ipmi-sel.h,v 1.6 2007-06-01 20:56:17 chu11 Exp $ 
   
   ipmi-sel.h - System Event Logger utility.
   
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

#ifndef _IPMI_SEL_H
#define _IPMI_SEL_H

#include <freeipmi/freeipmi.h>

#include "argp-common.h"
#include "ipmi-sdr-cache.h"
#include "pstdout.h"

enum ipmi_sel_argp_option_keys
  { 
    INFO_KEY = 'i', 
    DELETE_KEY = 'd', 
    DELETE_ALL_KEY = 'c', 
    HEX_DUMP_KEY = 'x', 
    DELETE_RANGE_KEY = 'R'
  };

struct ipmi_sel_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
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
};

typedef struct ipmi_sel_prog_data
{
  char *progname;
  struct ipmi_sel_arguments *args;
  uint32_t debug_flags;
} ipmi_sel_prog_data_t;

typedef struct ipmi_sel_state_data
{
  ipmi_sel_prog_data_t *prog_data;
  ipmi_device_t dev;
  pstdout_state_t pstate;
  char *hostname;
  sdr_cache_ctx_t sdr_cache_ctx;
  sdr_record_t *sdr_record_list;
  unsigned int sdr_record_count;
} ipmi_sel_state_data_t;

#endif
