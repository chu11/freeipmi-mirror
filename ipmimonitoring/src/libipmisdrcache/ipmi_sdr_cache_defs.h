/*****************************************************************************\
 *  $Id: ipmi_sdr_cache_defs.h,v 1.1.2.2 2007-12-20 23:25:41 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_SDR_CACHE_DEFS_H
#define _IPMI_SDR_CACHE_DEFS_H

#include <stdint.h>
#include <sys/param.h>

#include "ipmi_sdr_cache.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SDR_CACHE_MAGIC        0xABCD9876

#define IPMI_SDR_CACHE_FILE_MAGIC_0 0xEF
#define IPMI_SDR_CACHE_FILE_MAGIC_1 0xE7
#define IPMI_SDR_CACHE_FILE_MAGIC_2 0x35
#define IPMI_SDR_CACHE_FILE_MAGIC_3 0x7C

#define IPMI_SDR_CACHE_OPERATION_UNINITIALIZED 0
#define IPMI_SDR_CACHE_OPERATION_READ_CACHE    1

struct ipmi_sdr_cache_ctx {
  uint32_t magic;
  unsigned int errnum;
  unsigned int operation;

  int fd;
  char filename[MAXPATHLEN+1];
  uint8_t version;
  uint16_t record_count;

  /* Cache Creation Vars */
  unsigned int record_count_written;
  unsigned int total_bytes_written;
  int validation_flags;
  uint16_t *record_ids;
  unsigned int record_ids_count;
  uint8_t *sensor_numbers;
  unsigned int sensor_numbers_count;

  /* Cache Reading Vars */
  off_t file_size;
  off_t records_start_offset;
  uint8_t *sdr_cache;
  off_t current_offset;
};

#endif /* _IPMI_SDR_CACHE_DEFS_H */
