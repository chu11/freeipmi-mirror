/*****************************************************************************\
 *  $Id: ipmi-sdr.c,v 1.20 2010-02-08 22:09:40 chu11 Exp $
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
#include <sys/mman.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/sdr/ipmi-sdr.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "freeipmi-portability.h"

static char *ipmi_sdr_cache_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "filename invalid",
    "file system error",
    "filename path permission error",
    "context performing other operation",
    "SDR cache exists",
    "SDR record with an identical record id already written",
    "SDR record length invalid",
    "SDR record count invalid",
    "SDR cache reading initialization already called",
    "cache reading not initialized",
    "SDR cache does not exist",
    "SDR cache invalid",
    "SDR cache out of date",
    "stats not compiled",
    "invalid sdr record",
    "incomplete sdr record",
    "cannot parse or calculate",
    "error returned in callback",
    "not found",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sdr_ctx_t
ipmi_sdr_ctx_create (void)
{
  struct ipmi_sdr_ctx *ctx = NULL;

  if (!(ctx = (ipmi_sdr_ctx_t)malloc (sizeof (struct ipmi_sdr_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_sdr_ctx));
  ctx->magic = IPMI_SDR_CTX_MAGIC;
  ctx->flags = IPMI_SDR_FLAGS_DEFAULT;
  ctx->debug_prefix = NULL;

  if (!(ctx->saved_offsets = list_create ((ListDelF)free)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  sdr_init_ctx (ctx);
  return (ctx);

 cleanup:
  if (ctx)
    {
      if (ctx->saved_offsets)
	list_destroy (ctx->saved_offsets);
      free (ctx);
    }
  return (NULL);
}

void
ipmi_sdr_ctx_destroy (ipmi_sdr_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    return;

  if (ctx->callback_lock)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CONTEXT_PERFORMING_OTHER_OPERATION);
      return;
    }

  /* ignore potential error, destroy path */
  if (ctx->fd >= 0)
    close (ctx->fd);
  /* ignore potential error, void return func */
  if (ctx->sdr_cache)
    munmap (ctx->sdr_cache, ctx->file_size);

  list_destroy (ctx->saved_offsets);

  ctx->magic = ~IPMI_SDR_CTX_MAGIC;
  ctx->operation = IPMI_SDR_OPERATION_UNINITIALIZED;
  free (ctx->debug_prefix);
  free (ctx);
}

int
ipmi_sdr_ctx_errnum (ipmi_sdr_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_SDR_ERR_CONTEXT_NULL);
  else if (ctx->magic != IPMI_SDR_CTX_MAGIC)
    return (IPMI_SDR_ERR_CONTEXT_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_sdr_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_SDR_ERR_SUCCESS && errnum <= IPMI_SDR_ERR_ERRNUMRANGE)
    return (ipmi_sdr_cache_errmsgs[errnum]);
  else
    return (ipmi_sdr_cache_errmsgs[IPMI_SDR_ERR_ERRNUMRANGE]);
}

char *
ipmi_sdr_ctx_errormsg (ipmi_sdr_ctx_t ctx)
{
  return (ipmi_sdr_ctx_strerror (ipmi_sdr_ctx_errnum (ctx)));
}

int
ipmi_sdr_ctx_get_flags (ipmi_sdr_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_ctx_set_flags (ipmi_sdr_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_SDR_FLAGS_DEBUG_DUMP)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

char *
ipmi_sdr_ctx_get_debug_prefix (ipmi_sdr_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (NULL);
    }

  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (ctx)->debug_prefix;
}

int
ipmi_sdr_ctx_set_debug_prefix (ipmi_sdr_ctx_t ctx, const char *debug_prefix)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  free (ctx->debug_prefix);
  ctx->debug_prefix = NULL;

  if (debug_prefix)
    {
      if (!(ctx->debug_prefix = strdup (debug_prefix)))
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_OUT_OF_MEMORY);
          return (-1);
        }
    }

  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}
