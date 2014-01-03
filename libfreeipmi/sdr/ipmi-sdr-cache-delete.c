/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-delete.c,v 1.11 2010-02-08 22:09:40 chu11 Exp $
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

#include "freeipmi/sdr/ipmi-sdr.h"

#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "freeipmi-portability.h"

int
ipmi_sdr_cache_delete (ipmi_sdr_ctx_t ctx, const char *filename)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!filename)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_UNINITIALIZED)
    {
      if (ctx->operation == IPMI_SDR_OPERATION_READ_CACHE)
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CONTEXT_PERFORMING_OTHER_OPERATION);
      else
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
      return (-1);
    }

  ctx->operation = IPMI_SDR_OPERATION_DELETE_CACHE;

  if (unlink (filename) < 0)
    {
      /* If there is no file (ENOENT), its ok */
      if (errno != ENOENT)
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
	  goto cleanup;
        }
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  ctx->operation = IPMI_SDR_OPERATION_UNINITIALIZED;
  return (rv);
}
