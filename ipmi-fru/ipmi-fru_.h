/*****************************************************************************\
 *  $Id: ipmi-fru_.h,v 1.17 2010-02-08 22:20:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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

/* file is named ipmi-fru_.h to differentiate itself from the
 * library ipmi-fru_.h.
 *
 * I am scared of the portability of the #include_next directive, so
 * that's why I'm doing it this way.
 */

/* file is "ipmi-fru_.h", so double underscore */

#ifndef IPMI_FRU__H
#define IPMI_FRU__H

#include <stdint.h>
#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "tool-oem-common.h"
#include "pstdout.h"

enum ipmi_fru_argp_option_keys
  {
    DEVICE_ID_KEY = 'e',
    VERBOSE_KEY = 'v',
    /* legacy */
    SKIP_CHECKS_KEY = 's',
    BRIDGE_FRU_KEY = 160,
    INTERPRET_OEM_DATA = 161,
  };

struct ipmi_fru_arguments
{
  struct common_cmd_args common_args;
  uint8_t device_id;
  int device_id_set;
  unsigned int verbose_count;
  /* legacy */
  int skip_checks;
  int bridge_fru;
  int interpret_oem_data;
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
  ipmi_fru_ctx_t fru_ctx;
  ipmi_sdr_ctx_t sdr_ctx;
  struct ipmi_oem_data oem_data;
} ipmi_fru_state_data_t;

#endif /* IPMI_FRU__H */
