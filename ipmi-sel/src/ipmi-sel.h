/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifndef _IPMI_SEL_H
#define _IPMI_SEL_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "tool-sdr-cache-common.h"
#include "pstdout.h"

#define IPMI_SEL_MAX_RECORD 4096

enum ipmi_sel_argp_option_keys
  { 
    VERBOSE_KEY = 'v',
    INFO_KEY = 'i', 
    DISPLAY_KEY = 'o', 
    DISPLAY_RANGE_KEY = 'r',
    DELETE_ALL_KEY = 'c', 
    DELETE_KEY = 'd', 
    DELETE_RANGE_KEY = 'R',
    SYSTEM_EVENT_ONLY_KEY = 'N',
    OEM_EVENT_ONLY_KEY = 'O',
    HEX_DUMP_KEY = 'x', 
    LEGACY_OUTPUT_KEY = 'L',
  };

struct ipmi_sel_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
  int verbose;
  int verbose_count;
  int info;
  int display;
  unsigned int display_record_list[IPMI_SEL_MAX_RECORD];
  unsigned int display_record_list_length;
  int display_range;
  unsigned int display_range1;
  unsigned int display_range2;
  int delete_all;
  int delete;
  unsigned int delete_record_list[IPMI_SEL_MAX_RECORD];
  unsigned int delete_record_list_length;
  int delete_range;
  unsigned int delete_range1;
  unsigned int delete_range2;
  int system_event_only;
  int oem_event_only;
  int hex_dump;
  int legacy_output;
};

typedef struct ipmi_sel_prog_data
{
  char *progname;
  struct ipmi_sel_arguments *args;
} ipmi_sel_prog_data_t;

typedef struct ipmi_sel_state_data
{
  ipmi_sel_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_cache_ctx_t ipmi_sdr_cache_ctx;
  ipmi_sel_parse_ctx_t ipmi_sel_parse_ctx;
  int output_headers;
} ipmi_sel_state_data_t;

#endif
