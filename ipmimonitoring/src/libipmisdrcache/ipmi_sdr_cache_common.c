/*****************************************************************************\
 *  $Id: ipmi_sdr_cache_common.c,v 1.1.2.1 2007-12-20 22:59:42 chu11 Exp $
 *****************************************************************************
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi_sdr_cache.h"
#include "ipmi_sdr_cache_common.h"
#include "ipmi_sdr_cache_defs.h"

void
ipmi_sdr_cache_init_ctx(ipmi_sdr_cache_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_SDR_CACHE_MAGIC);

  c->operation = IPMI_SDR_CACHE_OPERATION_UNINITIALIZED;

  c->fd = -1;
  memset(c->filename, '\0', MAXPATHLEN+1);
  c->version = 0;
  c->record_count = 0;

  c->record_count_written = 0;
  c->total_bytes_written = 0;
  c->validation_flags = 0;
  c->record_ids = NULL;
  c->record_ids_count = 0;
  c->sensor_numbers = NULL;
  c->sensor_numbers_count = 0;

  c->file_size = 0;
  c->records_start_offset = 0;
  c->sdr_cache = NULL;
}

