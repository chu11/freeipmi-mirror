/*
  Copyright (C) 2003-2010 FreeIPMI Core Team

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
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"

#include "freeipmi/record-format/ipmi-sdr-record-format.h"

#include "ipmi-interpret-defs.h"
#include "ipmi-interpret-trace.h"
#include "ipmi-interpret-sensor-config.h"
#include "ipmi-interpret-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

static char *ipmi_interpret_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "permission denied",
    "sensor config file does not exist",
    "sensor config file parse error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_interpret_ctx_t
ipmi_interpret_ctx_create (void)
{
  struct ipmi_interpret_ctx *ctx = NULL;

  if (!(ctx = (ipmi_interpret_ctx_t)malloc (sizeof (struct ipmi_interpret_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_interpret_ctx));
  ctx->magic = IPMI_INTERPRET_CTX_MAGIC;
  ctx->flags = IPMI_INTERPRET_FLAGS_DEFAULT;

  if (!(ctx->sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (ipmi_interpret_sensors_init (ctx) < 0)
    goto cleanup;

  return (ctx);

 cleanup:
  if (ctx)
    {
      if (ctx->sdr_parse_ctx)
        ipmi_sdr_parse_ctx_destroy (ctx->sdr_parse_ctx);
      ipmi_interpret_sensors_destroy (ctx);
      free (ctx);
    }
  return (NULL);
}

void
ipmi_interpret_ctx_destroy (ipmi_interpret_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    return;

  ctx->magic = ~IPMI_INTERPRET_CTX_MAGIC;
  ipmi_sdr_parse_ctx_destroy (ctx->sdr_parse_ctx);
  free (ctx);
}

int
ipmi_interpret_ctx_errnum (ipmi_interpret_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_INTERPRET_ERR_CONTEXT_NULL);
  else if (ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    return (IPMI_INTERPRET_ERR_CONTEXT_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_interpret_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_INTERPRET_ERR_SUCCESS && errnum <= IPMI_INTERPRET_ERR_ERRNUMRANGE)
    return (ipmi_interpret_errmsgs[errnum]);
  else
    return (ipmi_interpret_errmsgs[IPMI_INTERPRET_ERR_ERRNUMRANGE]);
}

char *
ipmi_interpret_ctx_errormsg (ipmi_interpret_ctx_t ctx)
{
  return (ipmi_interpret_ctx_strerror (ipmi_interpret_ctx_errnum (ctx)));
}

int
ipmi_interpret_ctx_get_flags (ipmi_interpret_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_ctx_set_flags (ipmi_interpret_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_INTERPRET_FLAGS_MASK)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_load_sensor_config (ipmi_interpret_ctx_t ctx,
                                   const char *sensor_config_file)
{
  struct stat buf;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (sensor_config_file)
    {
      if (stat (sensor_config_file, &buf) < 0)
        {
          if (errno == EACCES || errno == EPERM)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PERMISSION);
          else if (errno == ENOENT)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST);
          else
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
          goto cleanup;
        }
    }

  if (ipmi_interpret_sensor_config_parse (ctx, sensor_config_file) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_interpret_sensor (ipmi_interpret_ctx_t ctx,
                       const void *sdr_record,
                       unsigned int sdr_record_len,
                       uint8_t shared_sensor_number_offset,
                       uint16_t sensor_event_bitmask)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (!sdr_record
      || !sdr_record_len)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

 cleanup:
  return (rv);
}
