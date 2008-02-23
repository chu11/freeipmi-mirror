/*****************************************************************************\
 *  $Id: ipmi-fru.h,v 1.8 2008-02-23 15:34:12 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_FRU_H
#define _IPMI_FRU_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

#define FRU_BUF_LEN 2048

typedef enum
  {
    FRU_ERR_FATAL_ERROR = -2,
    FRU_ERR_NON_FATAL_ERROR = -1,
    FRU_ERR_SUCCESS = 0,
  } fru_err_t;

enum ipmi_sel_argp_option_keys
  {
    DEVICE_ID_KEY = 'e',
    VERBOSE_KEY = 'v',
    SKIP_CHECKS_KEY = 's'
  };

struct ipmi_fru_arguments
{
  struct common_cmd_args common;
  struct sdr_cmd_args sdr;
  struct hostrange_cmd_args hostrange;
  int device_id_wanted;
  int device_id;
  int verbose_count;
  int skip_checks_wanted;
};

typedef struct ipmi_fru_prog_data
{
  char *progname;
  struct ipmi_fru_arguments *args;
} ipmi_fru_prog_data_t;

typedef struct ipmi_fru_state_data
{
  ipmi_fru_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  char *hostname;
  ipmi_sdr_cache_ctx_t ipmi_sdr_cache_ctx;
  uint64_t fru_inventory_area_size;
} ipmi_fru_state_data_t;

#endif
