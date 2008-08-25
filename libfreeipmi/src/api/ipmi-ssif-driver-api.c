/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"
#include "ipmi-ssif-driver-api.h"

#include "freeipmi-portability.h"

int
ipmi_ssif_cmd_api (ipmi_ctx_t ctx,
		   fiid_obj_t obj_cmd_rq,
		   fiid_obj_t obj_cmd_rs)
{
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rq)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_PACKET_VALID(obj_cmd_rq);

  API_ERR_INTERNAL_ERROR(ctx->type == IPMI_DEVICE_SSIF);

  {
    uint8_t *pkt;
    uint32_t pkt_len;
    int32_t hdr_len, cmd_len;

    API_FIID_TEMPLATE_LEN_BYTES(hdr_len, tmpl_hdr_kcs);
    API_FIID_OBJ_LEN_BYTES (cmd_len, obj_cmd_rq);
    pkt_len = hdr_len + cmd_len;

    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    API_ERR (pkt);

    API_ERR (fill_hdr_ipmi_kcs (ctx->lun,
				ctx->net_fn,
				ctx->io.inband.rq.obj_hdr) == 0);
    API_ERR (assemble_ipmi_kcs_pkt (ctx->io.inband.rq.obj_hdr,
				    obj_cmd_rq,
				    pkt,
				    pkt_len) > 0);

    API_ERR_SSIF (ipmi_ssif_write (ctx->io.inband.ssif_ctx, pkt, pkt_len) != -1);
  }

  {
    uint8_t *pkt;
    uint32_t pkt_len;
    ssize_t read_len;
    int32_t hdr_len, cmd_len;
    fiid_field_t *tmpl = NULL;
    int8_t rv = -1;

    API_FIID_TEMPLATE_LEN_BYTES_CLEANUP(hdr_len, tmpl_hdr_kcs);
    API_FIID_OBJ_TEMPLATE_CLEANUP(tmpl, obj_cmd_rs);
    API_FIID_TEMPLATE_LEN_BYTES_CLEANUP(cmd_len, tmpl);
    pkt_len = hdr_len + cmd_len;

    API_ERR_CLEANUP ((pkt = alloca (pkt_len)));
    memset (pkt, 0, pkt_len);

    API_ERR_SSIF_CLEANUP (!((read_len = ipmi_ssif_read (ctx->io.inband.ssif_ctx, pkt, pkt_len)) < 0));

    if (!read_len)
      API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_SYSTEM_ERROR);

    API_ERR_CLEANUP (!(unassemble_ipmi_kcs_pkt (pkt,
						read_len,
						ctx->io.inband.rs.obj_hdr,
						obj_cmd_rs) < 0));

    rv = 0;
  cleanup:
    API_FIID_TEMPLATE_FREE(tmpl);
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

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (buf_rq 
                      && buf_rq_len > 0
                      && buf_rs 
                      && buf_rs_len > 0);

  API_ERR_INTERNAL_ERROR(ctx->type == IPMI_DEVICE_SSIF);

  API_FIID_TEMPLATE_LEN_BYTES(hdr_len, tmpl_hdr_kcs);
  pkt_len = hdr_len + buf_rq_len;

  pkt = alloca(pkt_len);
  API_ERR (pkt);
  readbuf = alloca(buf_rs_len);
  API_ERR (readbuf);

  API_ERR (fill_hdr_ipmi_kcs (ctx->lun,
			      ctx->net_fn,
			      ctx->io.inband.rq.obj_hdr) == 0);
  
  API_FIID_OBJ_GET_ALL(ctx->io.inband.rq.obj_hdr, pkt, pkt_len);
  memcpy(pkt + hdr_len, buf_rq, buf_rq_len);
  
  /* Request Block */
  API_ERR_SSIF (ipmi_ssif_write (ctx->io.inband.ssif_ctx, pkt, pkt_len) != -1);
  
  /* Response Block */
  API_ERR_SSIF ((bytes_read = ipmi_ssif_read (ctx->io.inband.ssif_ctx,
					      readbuf, buf_rs_len)) != -1);

  if (!bytes_read)
    {
      API_ERR_SET_ERRNUM(IPMI_ERR_SYSTEM_ERROR);
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
