/*****************************************************************************\
 *  $Id: ipmi-sdr-defs.h,v 1.13 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef IPMI_SDR_DEFS_H
#define IPMI_SDR_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <sys/types.h>          /* off_t */
#include <sys/param.h>
#if HAVE_UNISTD_H
#include <unistd.h>             /* off_t */
#endif /* HAVE_UNISTD_H */

#include "freeipmi/sdr/ipmi-sdr.h"

#include "list.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SDR_CTX_MAGIC        0xABCD9876

#define IPMI_SDR_OPERATION_UNINITIALIZED  0
#define IPMI_SDR_OPERATION_CREATE_CACHE   1
#define IPMI_SDR_OPERATION_READ_CACHE     2
#define IPMI_SDR_OPERATION_DELETE_CACHE   3

/* Why use indexes instead of fiid templates?  B/c that's how it was
 * written before libipmimonitoring's libipmisdrcache was written before
 * it was included in freeipmi.
 */
#define IPMI_SDR_RECORD_HEADER_LENGTH               5
#define IPMI_SDR_RECORD_LENGTH_INDEX                4
#define IPMI_SDR_RECORD_ID_INDEX_LS                 0
#define IPMI_SDR_RECORD_ID_INDEX_MS                 1
#define IPMI_SDR_RECORD_TYPE_INDEX                  3
#define IPMI_SDR_RECORD_SENSOR_OWNER_ID_INDEX       5
#define IPMI_SDR_RECORD_SENSOR_NUMBER_INDEX         7
#define IPMI_SDR_RECORD_COMPACT_SHARE_COUNT         23
#define IPMI_SDR_RECORD_COMPACT_SHARE_COUNT_BITMASK 0x0F
#define IPMI_SDR_RECORD_COMPACT_SHARE_COUNT_SHIFT   0
#define IPMI_SDR_RECORD_EVENT_SHARE_COUNT           12
#define IPMI_SDR_RECORD_EVENT_SHARE_COUNT_BITMASK   0x0F
#define IPMI_SDR_RECORD_EVENT_SHARE_COUNT_SHIFT     0

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

/* Cache Version 0.1 format
 *
 * magic bytes (4 bytes)
 * version bytes (4)
 * sdr version (1)
 * record count (2)
 * most recent addition timestamp (4)
 * most recent erase timestamp (4)
 * records (variable)
 */
#define IPMI_SDR_CACHE_FILE_VERSION_1_0 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_1_1 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_1_2 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_1_3 0x01

/* Cache Version 1.2 format
 *
 * magic bytes (4 bytes)
 * version bytes (4)
 * sdr version (1)
 * record count (2)
 * most recent addition timestamp (4)
 * most recent erase timestamp (4)
 * header checksum (1) [all bytes above]
 * records (variable)
 * total bytes of file (4)
 * trailer checksum (1) [records + total bytes of file]
 */

#define IPMI_SDR_CACHE_FILE_VERSION_1_2_0 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_1_2_1 0x01
#define IPMI_SDR_CACHE_FILE_VERSION_1_2_2 0x00
#define IPMI_SDR_CACHE_FILE_VERSION_1_2_3 0x02

#define IPMI_MAX_ENTITY_IDS          256
#define IPMI_MAX_ENTITY_ID_INSTANCES 256

struct ipmi_sdr_offset {
  off_t offset;
  int offset_dumped;
};

struct ipmi_sdr_entity_count {
  uint8_t entity_instances[IPMI_MAX_ENTITY_ID_INSTANCES];
  unsigned int entity_instances_count;
};

struct ipmi_sdr_ctx {
  uint32_t magic;
  int errnum;
  unsigned int operation;
  unsigned int flags;
  char *debug_prefix;

  uint8_t sdr_version;
  uint16_t record_count;
  uint32_t most_recent_addition_timestamp;
  uint32_t most_recent_erase_timestamp;

  /* Cache Reading Vars */
  int fd;
  off_t file_size;
  off_t records_start_offset;
  off_t records_end_offset;
  uint8_t *sdr_cache;
  struct ipmi_sdr_offset current_offset;
  int callback_lock;

  /* for saving/reset */
  List saved_offsets;

  /* Stats */
  int stats_compiled;
  struct ipmi_sdr_entity_count entity_counts[IPMI_MAX_ENTITY_IDS];
};

#endif /* IPMI_SDR_DEFS_H */
