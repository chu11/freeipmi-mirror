/* 
   Copyright (C) 2003-2009 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

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
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "freeipmi/api/ipmi-sel-cmds-api.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sensor-read-defs.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

static char *ipmi_sensor_read_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "sensor reading unavailable",
    "sensor scanning disabled",
    "sensor non-analog",
    "sensor non-linear",
    "sensor reading cannot be obtained",
    "sensor not owned by BMC",
    "sensor is system software sensor",
    "node busy",
    "invalid sdr record type",
    "sdr cache error",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sensor_read_ctx_t
ipmi_sensor_read_ctx_create(ipmi_ctx_t ipmi_ctx, ipmi_sdr_cache_ctx_t sdr_cache_ctx)
{
  struct ipmi_sensor_read_ctx *ctx = NULL;
  
  ERR_EINVAL_NULL_RETURN(ipmi_ctx);
  ERR_EINVAL_NULL_RETURN(sdr_cache_ctx);

  ERR_CLEANUP((ctx = (ipmi_sensor_read_ctx_t)malloc(sizeof(struct ipmi_sensor_read_ctx))));
  memset(ctx, '\0', sizeof(struct ipmi_sensor_read_ctx));
  ctx->magic = IPMI_SENSOR_READ_MAGIC;
  ctx->flags = IPMI_SENSOR_READ_FLAGS_DEFAULT;
  ctx->debug_prefix = NULL;

  ctx->ipmi_ctx = ipmi_ctx;
  ctx->sdr_cache_ctx = sdr_cache_ctx;

  return ctx;

 cleanup:
  if (ctx)
    free(ctx);
  return NULL;
}

void
ipmi_sensor_read_ctx_destroy(ipmi_sensor_read_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SENSOR_READ_MAGIC)
    return;

  if (ctx->debug_prefix)
    free(ctx->debug_prefix);
  ctx->magic = ~IPMI_SENSOR_READ_MAGIC;
  free(ctx);
}

int 
ipmi_sensor_read_ctx_errnum(ipmi_sensor_read_ctx_t ctx)
{
  if (!ctx)
    return IPMI_SENSOR_READ_CTX_ERR_CONTEXT_NULL;
  else if (ctx->magic != IPMI_SENSOR_READ_MAGIC)
    return IPMI_SENSOR_READ_CTX_ERR_CONTEXT_INVALID;
  else
    return ctx->errnum;
}

char *
ipmi_sensor_read_ctx_strerror(int errnum)
{
  if (errnum >= IPMI_SENSOR_READ_CTX_ERR_SUCCESS && errnum <= IPMI_SENSOR_READ_CTX_ERR_ERRNUMRANGE)
    return ipmi_sensor_read_errmsgs[errnum];
  else
    return ipmi_sensor_read_errmsgs[IPMI_SENSOR_READ_CTX_ERR_ERRNUMRANGE];
}

char *
ipmi_sensor_read_ctx_errormsg(ipmi_sensor_read_ctx_t ctx)
{
  return ipmi_sensor_read_ctx_strerror(ipmi_sensor_read_ctx_errnum(ctx));
}

int
ipmi_sensor_read_ctx_get_flags(ipmi_sensor_read_ctx_t ctx, unsigned int *flags)
{
  ERR(ctx && ctx->magic == IPMI_SENSOR_READ_MAGIC);

  SENSOR_READ_ERR_PARAMETERS(flags);

  *flags = ctx->flags;
  return 0;
}

int
ipmi_sensor_read_ctx_set_flags(ipmi_sensor_read_ctx_t ctx, unsigned int flags)
{
  ERR(ctx && ctx->magic == IPMI_SENSOR_READ_MAGIC);

  SENSOR_READ_ERR_PARAMETERS(!(flags & ~IPMI_SENSOR_READ_FLAGS_MASK));

  ctx->flags = flags;
  return 0;
}

char *
ipmi_sensor_read_ctx_get_debug_prefix(ipmi_sensor_read_ctx_t ctx)
{
  ERR_NULL_RETURN(ctx && ctx->magic == IPMI_SENSOR_READ_MAGIC);

  return ctx->debug_prefix;
}

int
ipmi_sensor_read_ctx_set_debug_prefix(ipmi_sensor_read_ctx_t ctx, const char *prefix)
{
  ERR(ctx && ctx->magic == IPMI_SENSOR_READ_MAGIC);

  if (ctx->debug_prefix)
    {
      free(ctx->debug_prefix);
      ctx->debug_prefix = NULL;
    }

  if (prefix)
    SENSOR_READ_ERR_OUT_OF_MEMORY((ctx->debug_prefix = strdup(prefix)));

  return 0;
}
