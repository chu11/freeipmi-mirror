/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  Based on ipmitool.c provided by Amitoj Singh <amitoj@fnal.gov> and
  Don Holmgren <djholm@fnal.gov>

  Under GNU/Linux, requires i2c-dev, i2c-i801, i2c-core drivers version >= 2.8.7

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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-kcs-interface.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-ssif-driver-api.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

fiid_template_t tmpl_ssif_raw =
  {
    { 8192, "raw_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

static void
_ipmi_ssif_dump (ipmi_ctx_t ctx,
                 const void *pkt,
                 unsigned int pkt_len,
                 uint8_t cmd,
                 uint8_t net_fn,
                 unsigned int debug_direction,
                 fiid_template_t tmpl_cmd)
{
  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && pkt
          && pkt_len
          && (debug_direction == DEBUG_UTIL_DIRECTION_REQUEST
              || debug_direction == DEBUG_UTIL_DIRECTION_RESPONSE));

  /* Don't cleanup/return an error here.  It's just debug code. */

  debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                 debug_direction,
                 net_fn,
                 cmd,
                 hdrbuf,
                 DEBUG_UTIL_HDR_BUFLEN);
  
  ipmi_dump_ssif_packet (STDERR_FILENO,
                         NULL,
                         hdrbuf,
                         NULL,
                         pkt,
                         pkt_len,
                         tmpl_cmd);
}

static void
_ipmi_ssif_dump_rq (ipmi_ctx_t ctx,
                    const void *pkt,
                    unsigned int pkt_len,
                    uint8_t cmd,
                    uint8_t net_fn,
                    fiid_obj_t obj_cmd_rq)
{
  fiid_field_t *tmpl_cmd = NULL;

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rq)))
    {
      _ipmi_ssif_dump (ctx,
                       pkt,
                       pkt_len,
                       cmd,
                       net_fn,
                       DEBUG_UTIL_DIRECTION_REQUEST,
                       tmpl_cmd);
      fiid_template_free (tmpl_cmd);
    }
}

static void
_ipmi_ssif_dump_rs (ipmi_ctx_t ctx,
                    const void *pkt,
                    unsigned int pkt_len,
                    uint8_t cmd,
                    uint8_t net_fn,
                    fiid_obj_t obj_cmd_rs)
{
  fiid_field_t *tmpl_cmd = NULL;

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rs)))
    {
      _ipmi_ssif_dump (ctx,
                       pkt,
                       pkt_len,
                       cmd,
                       net_fn,
                       DEBUG_UTIL_DIRECTION_RESPONSE,
                       tmpl_cmd);
      fiid_template_free (tmpl_cmd);
    }
}

static void
_ipmi_ssif_dump_raw_rq (ipmi_ctx_t ctx,
                        const void *pkt,
                        unsigned int pkt_len,
                        uint8_t cmd,
                        uint8_t net_fn)
{
  _ipmi_ssif_dump (ctx,
                   pkt,
                   pkt_len,
                   cmd,
                   net_fn,
                   DEBUG_UTIL_DIRECTION_REQUEST,
                   tmpl_ssif_raw);
}

static void
_ipmi_ssif_dump_raw_rs (ipmi_ctx_t ctx,
                        const void *pkt,
                        unsigned int pkt_len,
                        uint8_t cmd,
                        uint8_t net_fn)
{
  _ipmi_ssif_dump (ctx,
                   pkt,
                   pkt_len,
                   cmd,
                   net_fn,
                   DEBUG_UTIL_DIRECTION_RESPONSE,
                   tmpl_ssif_raw);
}

static int
_ssif_cmd_write (ipmi_ctx_t ctx, uint8_t cmd, fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  int hdr_len, cmd_len, send_len;
  int rv = -1;
  
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && fiid_obj_valid (obj_cmd_rq));
  
  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if ((cmd_len = fiid_obj_len_bytes (obj_cmd_rq)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }
  
  pkt_len = hdr_len + cmd_len;
  
  if (!(pkt = malloc (pkt_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (pkt, '\0', pkt_len);
  
  if (fill_hdr_ipmi_kcs (ctx->lun,
                         ctx->net_fn,
                         ctx->io.inband.rq.obj_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if ((send_len = assemble_ipmi_kcs_pkt (ctx->io.inband.rq.obj_hdr,
                                         obj_cmd_rq,
                                         pkt,
                                         pkt_len)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && send_len)
    _ipmi_ssif_dump_rq (ctx, pkt, send_len, cmd, ctx->net_fn, obj_cmd_rq);

  if (ipmi_ssif_write (ctx->io.inband.ssif_ctx, pkt, send_len) < 0)
    {
      API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  if (pkt)
    free (pkt);
  return (rv);
}

static int
_ssif_cmd_read (ipmi_ctx_t ctx, uint8_t cmd, fiid_obj_t obj_cmd_rs)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  int hdr_len, cmd_len, read_len;
  fiid_field_t *tmpl = NULL;
  int rv = -1;
  
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && fiid_obj_valid (obj_cmd_rs));

  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (!(tmpl = fiid_obj_template (obj_cmd_rs)))
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  
  if ((cmd_len = fiid_template_len_bytes (tmpl)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  pkt_len = hdr_len + cmd_len;
  
  if (!(pkt = malloc (pkt_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (pkt, '\0', pkt_len);
  
  if ((read_len = ipmi_ssif_read (ctx->io.inband.ssif_ctx, pkt, pkt_len)) < 0)
    {
      API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
      goto cleanup;
    }

  if (!read_len)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && read_len)
    _ipmi_ssif_dump_rs (ctx, pkt, read_len, cmd, ctx->net_fn, obj_cmd_rs);

  if (unassemble_ipmi_kcs_pkt (pkt,
                               read_len,
                               ctx->io.inband.rs.obj_hdr,
                               obj_cmd_rs) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  if (pkt)
    free (pkt);
  fiid_template_free (tmpl);
  return (rv);
}

int
ipmi_ssif_cmd_api (ipmi_ctx_t ctx,
                   fiid_obj_t obj_cmd_rq,
                   fiid_obj_t obj_cmd_rs)
{
  uint8_t cmd = 0;             /* used for debugging */
  uint64_t val;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_PACKET_VALID (obj_cmd_rq) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_SSIF)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* ignore error, continue on */
      if (FIID_OBJ_GET (obj_cmd_rq, "cmd", &val) < 0)
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      else
        cmd = val;
    }

  if (_ssif_cmd_write (ctx, cmd, obj_cmd_rq) < 0)
    return (-1);

  if (_ssif_cmd_read (ctx, cmd, obj_cmd_rs) < 0)
    return (-1);

  return (0);
}

int
ipmi_ssif_cmd_raw_api (ipmi_ctx_t ctx,
                       const void *buf_rq,
                       unsigned int buf_rq_len,
                       void *buf_rs,
                       unsigned int buf_rs_len)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  uint8_t *readbuf = NULL;
  int bytes_read = 0;
  int hdr_len, rv = -1;
  uint8_t cmd = 0;             /* used for debugging */

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!buf_rq
      || !buf_rq_len
      || !buf_rs
      || !buf_rs_len)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_SSIF)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    cmd = ((uint8_t *)buf_rq)[0];

  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  pkt_len = hdr_len + buf_rq_len;

  if (!(pkt = malloc (pkt_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(readbuf = malloc (buf_rs_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_hdr_ipmi_kcs (ctx->lun,
                         ctx->net_fn,
                         ctx->io.inband.rq.obj_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_get_all (ctx->io.inband.rq.obj_hdr, pkt, pkt_len) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.inband.rq.obj_hdr);
      goto cleanup;
    }
  memcpy (pkt + hdr_len, buf_rq, buf_rq_len);

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && pkt_len)
    _ipmi_ssif_dump_raw_rq (ctx, pkt, pkt_len, cmd, ctx->net_fn);

  /* Request Block */
  if (ipmi_ssif_write (ctx->io.inband.ssif_ctx, pkt, pkt_len) < 0)
    {
      API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
      goto cleanup;
    }

  /* Response Block */
  if ((bytes_read = ipmi_ssif_read (ctx->io.inband.ssif_ctx,
                                    readbuf, buf_rs_len)) < 0)
    {
      API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
      goto cleanup;
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && bytes_read)
    _ipmi_ssif_dump_raw_rs (ctx, readbuf, bytes_read, cmd, ctx->net_fn);

  if (!bytes_read)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  if ((bytes_read - hdr_len) > 0)
    {
      memcpy (buf_rs, readbuf + hdr_len, bytes_read - hdr_len);
      rv = bytes_read - hdr_len;
    }
  else
    rv = 0;

 cleanup:
  if (pkt)
    free (pkt);
  if (readbuf)
    free (readbuf);
  return (rv);
}
