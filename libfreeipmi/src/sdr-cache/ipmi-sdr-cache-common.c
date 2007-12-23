/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-common.c,v 1.1.2.3 2007-12-23 22:36:49 chu11 Exp $
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

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"
#include "freeipmi/api/ipmi-sdr-repository-cmds-api.h"
#include "freeipmi/cmds/ipmi-sdr-repository-cmds.h"

#include "ipmi-sdr-cache-common.h"
#include "ipmi-sdr-cache-defs.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

void
ipmi_sdr_cache_init_ctx(ipmi_sdr_cache_ctx_t ctx)
{
  assert(ctx);
  assert(ctx->magic == IPMI_SDR_CACHE_MAGIC);

  ctx->operation = IPMI_SDR_CACHE_OPERATION_UNINITIALIZED;

  ctx->sdr_version = 0;
  ctx->record_count = 0;
  ctx->most_recent_addition_timestamp = 0;
  ctx->most_recent_erase_timestamp = 0;

  ctx->fd = -1;
  ctx->file_size = 0;
  ctx->records_start_offset = 0;
  ctx->sdr_cache = NULL;
}

int
ipmi_sdr_cache_info(ipmi_sdr_cache_ctx_t ctx,
                    ipmi_ctx_t ipmi_ctx,
                    uint8_t *sdr_version,
                    uint16_t *record_count,
                    uint32_t *most_recent_addition_timestamp,
                    uint32_t *most_recent_erase_timestamp)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == IPMI_SDR_CACHE_MAGIC);
  assert(ipmi_ctx);
  assert(sdr_version);
  assert(record_count);
  assert(most_recent_addition_timestamp);
  assert(most_recent_erase_timestamp);
  
  SDR_CACHE_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, 
                                    tmpl_cmd_get_sdr_repository_info_rs);

  SDR_CACHE_ERR_IPMI_ERROR_CLEANUP(!(ipmi_cmd_get_sdr_repository_info (ipmi_ctx, 
                                                                       obj_cmd_rs) < 0));

  *sdr_version = 0;
  SDR_CACHE_FIID_OBJ_GET_CLEANUP(obj_cmd_rs,
                                 "sdr_version_minor",
                                 &val);
  *sdr_version = val;

  SDR_CACHE_FIID_OBJ_GET_CLEANUP(obj_cmd_rs,
                                 "sdr_version_major",
                                 &val);
  *sdr_version |= (val << 4);

  *record_count = 0;
  SDR_CACHE_FIID_OBJ_GET_CLEANUP(obj_cmd_rs,
                                 "record_count",
                                 &val);
  *record_count = val;

  *most_recent_addition_timestamp = 0;
  SDR_CACHE_FIID_OBJ_GET_CLEANUP(obj_cmd_rs,
                                 "most_recent_addition_timestamp",
                                 &val);
  *most_recent_addition_timestamp = val;

  *most_recent_erase_timestamp = 0;
  SDR_CACHE_FIID_OBJ_GET_CLEANUP(obj_cmd_rs,
                                 "most_recent_erase_timestamp",
                                 &val);
  *most_recent_erase_timestamp = val;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return rv;
}
