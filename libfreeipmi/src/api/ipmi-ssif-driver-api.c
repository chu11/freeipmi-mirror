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
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
#include <errno.h>

#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/interface/ipmi-kcs-interface.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-ssif-driver-api.h"

#include "freeipmi-portability.h"

int
ipmi_ssif_cmd_api (ipmi_ctx_t ctx,
		   fiid_obj_t obj_cmd_rq,
		   fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (!fiid_obj_valid(obj_cmd_rq)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (api_fiid_obj_packet_valid(ctx, obj_cmd_rq) < 0)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_SSIF)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  {
    uint8_t *pkt;
    uint32_t pkt_len;
    int32_t hdr_len, cmd_len, send_len;

    if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        return (-1);
      }

    if ((cmd_len = fiid_obj_len_bytes (obj_cmd_rq)) < 0)
      {
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM(ctx, obj_cmd_rq);
        return (-1);
      }

    pkt_len = hdr_len + cmd_len;

    pkt = alloca (pkt_len);
    if (!pkt)
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        return (-1);
      }
    memset (pkt, 0, pkt_len);

    if (fill_hdr_ipmi_kcs (ctx->lun,
                           ctx->net_fn,
                           ctx->io.inband.rq.obj_hdr) < 0)
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        return (-1);
      }

    if ((send_len = assemble_ipmi_kcs_pkt (ctx->io.inband.rq.obj_hdr,
                                           obj_cmd_rq,
                                           pkt,
                                           pkt_len)) < 0)
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        return (-1);
      }

    if (ipmi_ssif_write (ctx->io.inband.ssif_ctx, pkt, send_len) < 0)
      {
        API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
        return (-1);
      }
  }

  {
    uint8_t *pkt;
    uint32_t pkt_len;
    ssize_t read_len;
    int32_t hdr_len, cmd_len;
    fiid_field_t *tmpl = NULL;
    int8_t rv = -1;

    if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        goto cleanup;
      }

    if (!(tmpl = fiid_obj_template(obj_cmd_rs)))
      {
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM(ctx, obj_cmd_rs);
        goto cleanup;
      }

    if ((cmd_len = fiid_template_len_bytes (tmpl)) < 0)
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        goto cleanup;
      }

    pkt_len = hdr_len + cmd_len;

    if (!(pkt = alloca (pkt_len)))
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        goto cleanup;
      }
    memset (pkt, 0, pkt_len);

    if ((read_len = ipmi_ssif_read (ctx->io.inband.ssif_ctx, pkt, pkt_len)) < 0)
      {
        API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
        goto cleanup;
      }

    if (!read_len)
      {
        API_SET_ERRNUM(ctx, IPMI_ERR_SYSTEM_ERROR);
        goto cleanup;
      }

    if (unassemble_ipmi_kcs_pkt (pkt,
                                 read_len,
                                 ctx->io.inband.rs.obj_hdr,
                                 obj_cmd_rs) < 0)
      {
        API_ERRNO_TO_API_ERRNUM(ctx, errno);
        goto cleanup;
      }

    rv = 0;
  cleanup:
    FIID_TEMPLATE_FREE(tmpl);
    if (rv < 0)
      return (rv);
  }

  return (0);
}

int32_t
ipmi_ssif_cmd_raw_api (ipmi_ctx_t ctx,
		       uint8_t *buf_rq,
		       size_t buf_rq_len,
		       uint8_t *buf_rs,
		       size_t buf_rs_len)
{
  uint8_t *pkt = NULL;
  uint32_t pkt_len;
  uint8_t *readbuf = NULL;
  int32_t bytes_read = 0;
  int32_t hdr_len;
  int32_t rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (!buf_rq 
      || !buf_rq_len
      || !buf_rs 
      || !buf_rs_len)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_SSIF)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }
  pkt_len = hdr_len + buf_rq_len;

  pkt = alloca(pkt_len);
  if (!pkt)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }

  readbuf = alloca(buf_rs_len);
  if (!readbuf)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }

  if (fill_hdr_ipmi_kcs (ctx->lun,
                         ctx->net_fn,
                         ctx->io.inband.rq.obj_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }
  
  if (fiid_obj_get_all(ctx->io.inband.rq.obj_hdr, pkt, pkt_len) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM(ctx, ctx->io.inband.rq.obj_hdr);
      return (-1);
    }
  memcpy(pkt + hdr_len, buf_rq, buf_rq_len);
  
  /* Request Block */
  if (ipmi_ssif_write (ctx->io.inband.ssif_ctx, pkt, pkt_len) < 0)
    {
      API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
      return (-1);
    }
  
  /* Response Block */
  if ((bytes_read = ipmi_ssif_read (ctx->io.inband.ssif_ctx,
                                    readbuf, buf_rs_len)) < 0)
    {
      API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
      return (-1);
    }

  if (!bytes_read)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_SYSTEM_ERROR);
      return -1;
    }

  if ((bytes_read - hdr_len) > 0)
    {
      memcpy(buf_rs, readbuf + hdr_len, bytes_read - hdr_len);
      rv = bytes_read - hdr_len;
    }
  else
    rv = 0;

  return (rv);
}
