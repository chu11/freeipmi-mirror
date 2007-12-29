/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-defs.h,v 1.3 2007-12-29 21:11:34 chu11 Exp $
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

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SDR_CACHE_MAGIC        0xABCD9876

#if 0
/* Original - sdr cache version 1.0 - keep for documentation history */
#define IPMI_SDR_CACHE_FILE_MAGIC_0 0xEF
#define IPMI_SDR_CACHE_FILE_MAGIC_1 0xE7
#define IPMI_SDR_CACHE_FILE_MAGIC_2 0x35
#define IPMI_SDR_CACHE_FILE_MAGIC_3 0x7C
#endif

#define IPMI_SDR_CACHE_FILE_MAGIC_0 0x72
#define IPMI_SDR_CACHE_FILE_MAGIC_1 0x8C
#define IPMI_SDR_CACHE_FILE_MAGIC_2 0x9D
#define IPMI_SDR_CACHE_FILE_MAGIC_3 0x1F

#define IPMI_SDR_CACHE_FILE_VERSION_0 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_1 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_2 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_3 0x01

#define IPMI_SDR_CACHE_OPERATION_UNINITIALIZED 0
#define IPMI_SDR_CACHE_OPERATION_READ_CACHE    1

struct ipmi_sdr_cache_ctx {
  uint32_t magic;
  unsigned int errnum;
  unsigned int operation;
  unsigned int flags;

  uint8_t sdr_version;
  uint16_t record_count;
  uint32_t most_recent_addition_timestamp;
  uint32_t most_recent_erase_timestamp;

  /* Cache Reading Vars */
  int fd;
  off_t file_size;
  off_t records_start_offset;
  uint8_t *sdr_cache;
  off_t current_offset;
};

#endif /* _IPMI_SDR_CACHE_DEFS_H */
