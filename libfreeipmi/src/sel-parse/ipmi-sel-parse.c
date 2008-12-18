/*****************************************************************************\
 *  $Id: ipmi-sel-parse.c,v 1.1.2.2 2008-12-18 18:55:46 chu11 Exp $
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
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-defs.h"

#include "libcommon/ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

static char *ipmi_sel_parse_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "sdr cache permission error",
    "sdr cache filesystem error",
    "sdr cache error",
    "no sel entries available",
    "not found",
    "callback error",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sel_parse_ctx_t
ipmi_sel_parse_ctx_create(ipmi_ctx_t ipmi_ctx, ipmi_sdr_cache_ctx_t sdr_cache_ctx)
{
  struct ipmi_sel_parse_ctx *ctx = NULL;

  ERR_CLEANUP((ctx = (ipmi_sel_parse_ctx_t)malloc(sizeof(struct ipmi_sel_parse_ctx))));
  memset(ctx, '\0', sizeof(struct ipmi_sel_parse_ctx));
  ctx->magic = IPMI_SEL_PARSE_MAGIC;
  ctx->flags = IPMI_SEL_PARSE_FLAGS_DEFAULT;
  ctx->debug_prefix = NULL;

  ctx->ipmi_ctx = ipmi_ctx;
  ctx->sdr_cache_ctx = sdr_cache_ctx;

  if (!(c->sel_entries = list_create((ListDelF)free)))
    goto cleanup;

  return ctx;

 cleanup:
  if (ctx)
    free(ctx);
  return NULL;
}

void
ipmi_sel_parse_ctx_destroy(ipmi_sel_parse_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_PARSE_MAGIC)
    return;

  ctx->magic = ~IPMI_SEL_PARSE_MAGIC;
  if (ctx->debug_prefix)
    free(ctx->debug_prefix);
  free(ctx);
}

int 
ipmi_sel_parse_ctx_errnum(ipmi_sel_parse_ctx_t ctx)
{
  if (!ctx)
    return IPMI_SEL_PARSE_CTX_ERR_CONTEXT_NULL;
  else if (ctx->magic != IPMI_SEL_PARSE_MAGIC)
    return IPMI_SEL_PARSE_CTX_ERR_CONTEXT_INVALID;
  else
    return ctx->errnum;
}

char *
ipmi_sel_parse_ctx_strerror(int errnum)
{
  if (errnum >= IPMI_SEL_PARSE_CTX_ERR_SUCCESS && errnum <= IPMI_SEL_PARSE_CTX_ERR_ERRNUMRANGE)
    return ipmi_sel_parse_errmsgs[errnum];
  else
    return ipmi_sel_parse_errmsgs[IPMI_SEL_PARSE_CTX_ERR_ERRNUMRANGE];
}

int
ipmi_sel_parse_ctx_get_flags(ipmi_sel_parse_ctx_t ctx, unsigned int *flags)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(flags);

  *flags = ctx->flags;
  return 0;
}

int
ipmi_sel_parse_ctx_set_flags(ipmi_sel_parse_ctx_t ctx, unsigned int flags)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(!(flags & ~IPMI_SEL_PARSE_FLAGS_DEBUG_DUMP));

  ctx->flags = flags;
  return 0;
}

char *
ipmi_sel_parse_ctx_get_debug_prefix(ipmi_sel_parse_ctx_t ctx)
{
  ERR_NULL_RETURN(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  return ctx->debug_prefix;
}

int
ipmi_sel_parse_ctx_set_debug_prefix(ipmi_sel_parse_ctx_t ctx, const char *prefix)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  if (ctx->debug_prefix)
    {
      free(ctx->debug_prefix);
      ctx->debug_prefix = NULL;
    }

  if (prefix)
    SEL_PARSE_ERR_OUT_OF_MEMORY((ctx->debug_prefix = strdup(prefix)));

  return 0;
}
