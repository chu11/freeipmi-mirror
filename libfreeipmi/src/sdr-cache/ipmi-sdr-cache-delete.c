/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-delete.c,v 1.3.6.1 2008-06-07 15:21:17 chu11 Exp $
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"

#include "ipmi-sdr-cache-defs.h"

#include "libcommon/ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

int 
ipmi_sdr_cache_delete(ipmi_sdr_cache_ctx_t ctx, char *filename)
{
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);
  
  SDR_CACHE_ERR_PARAMETERS(filename);

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
        SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_DELETE_CTX_SET_TO_READ);
      else
        SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR);
      return -1;
    }

  if (unlink(filename) < 0)
    {
      /* If there is no file (ENOENT), its ok */
      if (errno != ENOENT)
        SDR_CACHE_ERR(0);
    }
  
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}
