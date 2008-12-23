/*****************************************************************************\
 *  $Id: ipmi-sel-parse-defs.h,v 1.1.2.5 2008-12-23 18:46:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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

#ifndef _IPMI_SEL_PARSE_DEFS_H
#define _IPMI_SEL_PARSE_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <sys/types.h>          /* off_t */
#include <sys/param.h>
#if HAVE_UNISTD_H
#include <unistd.h>             /* off_t */
#endif /* HAVE_UNISTD_H */

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "list.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SEL_PARSE_MAGIC 0xAECD1846

/* Table 21-1 */
#define IPMI_SEL_RECORD_LENGTH               16
#define IPMI_SEL_RECORD_HEADER_LENGTH         3

#define IPMI_SEL_PARSE_DEBUG_BUFLEN         256

#define IPMI_SEL_PARSE_RESERVATION_ID_RETRY   4

#define IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD        0x1
#define IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD     0x2
#define IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD 0x3
#define IPMI_SEL_RECORD_TYPE_CLASS_UNKNOWN_RECORD             0x4

struct ipmi_sel_parse_entry {
  uint8_t sel_event_record[IPMI_SEL_RECORD_LENGTH];
  unsigned int sel_event_record_len; /* should always be 16, but just in case */
};

struct ipmi_sel_parse_ctx {
  uint32_t magic;
  unsigned int errnum;
  unsigned int flags;
  char *debug_prefix;

  ipmi_ctx_t ipmi_ctx;
  ipmi_sdr_cache_ctx_t sdr_cache_ctx;

  List sel_entries;
  ListIterator sel_entries_itr;
  struct ipmi_sel_parse_entry *current_sel_entry;

  struct ipmi_sel_parse_entry *callback_sel_entry;
};

#endif /* _IPMI_SEL_PARSE_DEFS_H */
