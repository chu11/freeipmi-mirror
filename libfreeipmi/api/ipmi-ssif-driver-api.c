/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/interface/ipmi-kcs-interface.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-ssif-driver-api.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

fiid_template_t tmpl_ssif_raw =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8192, "raw_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

static void
_api_ssif_dump (ipmi_ctx_t ctx,
		const void *pkt,
		unsigned int pkt_len,
		uint8_t cmd,
		uint8_t net_fn,
		uint8_t group_extension,
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
		 group_extension,
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
_api_ssif_dump_rq (ipmi_ctx_t ctx,
		   const void *pkt,
		   unsigned int pkt_len,
		   uint8_t cmd,
		   uint8_t net_fn,
		   uint8_t group_extension,
		   fiid_obj_t obj_cmd_rq)
{
  fiid_field_t *tmpl_cmd = NULL;

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rq)))
    {
      _api_ssif_dump (ctx,
		      pkt,
		      pkt_len,
		      cmd,
		      net_fn,
		      group_extension,
		      DEBUG_UTIL_DIRECTION_REQUEST,
		      tmpl_cmd);
      fiid_template_free (tmpl_cmd);
    }
}

static void
_api_ssif_dump_rs (ipmi_ctx_t ctx,
		   const void *pkt,
		   unsigned int pkt_len,
		   uint8_t cmd,
		   uint8_t net_fn,
		   uint8_t group_extension,
		   fiid_obj_t obj_cmd_rs)
{
  fiid_field_t *tmpl_cmd = NULL;

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rs)))
    {
      _api_ssif_dump (ctx,
		      pkt,
		      pkt_len,
		      cmd,
		      net_fn,
		      group_extension,
		      DEBUG_UTIL_DIRECTION_RESPONSE,
		      tmpl_cmd);
      fiid_template_free (tmpl_cmd);
    }
}

static int
_ssif_cmd_write (ipmi_ctx_t ctx,
		 uint8_t cmd,
		 uint8_t group_extension,
		 fiid_obj_t obj_cmd_rq)
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
  
  if (fill_hdr_ipmi_kcs (ctx->target.lun,
                         ctx->target.net_fn,
                         ctx->io.inband.rq.obj_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if ((send_len = assemble_ipmi_kcs_pkt (ctx->io.inband.rq.obj_hdr,
                                         obj_cmd_rq,
                                         pkt,
                                         pkt_len,
					 IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && send_len)
    _api_ssif_dump_rq (ctx,
		       pkt,
		       send_len,
		       cmd,
		       ctx->target.net_fn,
		       group_extension,
		       obj_cmd_rq);

  if (ipmi_ssif_write (ctx->io.inband.ssif_ctx, pkt, send_len) < 0)
    {
      API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  free (pkt);
  return (rv);
}

static int
_ssif_cmd_read (ipmi_ctx_t ctx,
		uint8_t cmd,
		uint8_t group_extension,
		fiid_obj_t obj_cmd_rs)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  int hdr_len, cmd_len, read_len;
  fiid_field_t *tmpl = NULL;
  int ret, rv = -1;
  unsigned int intf_flags = IPMI_INTERFACE_FLAGS_DEFAULT;
  
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    intf_flags |= IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK;

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
    _api_ssif_dump_rs (ctx,
		       pkt,
		       read_len,
		       cmd,
		       ctx->target.net_fn,
		       group_extension,
		       obj_cmd_rs);

  if ((ret = unassemble_ipmi_kcs_pkt (pkt,
                                      read_len,
                                      ctx->io.inband.rs.obj_hdr,
                                      obj_cmd_rs,
				      intf_flags)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* IPMI didn't return enough data back to you */
  if (!ret)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_IPMI_ERROR);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  free (pkt);
  fiid_template_free (tmpl);
  return (rv);
}

int
api_ssif_cmd (ipmi_ctx_t ctx,
	      fiid_obj_t obj_cmd_rq,
	      fiid_obj_t obj_cmd_rs)
{
  uint8_t cmd = 0;             /* used for debugging */
  uint8_t group_extension = 0; /* used for debugging */
  uint64_t val;
  struct timespec request, remain;
  uint8_t retry = IPMI_SSIF_RETRY_DEFAULT;
  
  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && ctx->type == IPMI_DEVICE_SSIF
	  && fiid_obj_valid (obj_cmd_rq)
	  && fiid_obj_packet_valid (obj_cmd_rq) == 1
	  && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* ignore error, continue on */
      if (FIID_OBJ_GET (obj_cmd_rq, "cmd", &val) < 0)
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      else
        cmd = val;

      if (IPMI_NET_FN_GROUP_EXTENSION (ctx->target.net_fn))
        {
          /* ignore error, continue on */
          if (FIID_OBJ_GET (obj_cmd_rq,
                            "group_extension_identification",
                            &val) < 0)
            API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
          else
            group_extension = val;
        }
    }

  if (_ssif_cmd_write (ctx, cmd, group_extension, obj_cmd_rq) < 0)
    return (-1);

  /******************************************************************************
    12.9 SMBus NACKs and Error Recovery:
    ====================================
    The BMC can NACK the SMBus host controller if it is not ready to accept a new 
    transaction. Typically, this will be exhibited by the BMC NACK'ing its slave 
    address. 
    
    If the BMC NACKs a single part transaction, software can simply retry it. 
    If a 'middle' or 'end' transaction is NACK'd, software should not retry the 
    particular but should restart the multi-part read or write from the beginning
    Start transaction for the transfer.
  *******************************************************************************/
  if (_ssif_cmd_read (ctx, cmd, group_extension, obj_cmd_rs) < 0)
    {
      while (1)
        {
          request.tv_sec = 0; 
          request.tv_nsec = IPMI_SSIF_TIMEOUT_DEFAULT;
          if (nanosleep (&request, &remain) < 0 )
            return (-1);

          if (_ssif_cmd_read (ctx, cmd, group_extension, obj_cmd_rs) < 0)
            {
              if (retry == 0)
                return (-1);
        
              retry--;        
            }
            else
              break;
        }
    }

  return (0);
}

int
api_ssif_cmd_raw (ipmi_ctx_t ctx,
		  const void *buf_rq,
		  unsigned int buf_rq_len,
		  void *buf_rs,
		  unsigned int buf_rs_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int len, rv = -1;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && ctx->type == IPMI_DEVICE_SSIF
	  && buf_rq
	  && buf_rq_len
	  && buf_rs
	  && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_ssif_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_ssif_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_cmd_rq,
                        buf_rq,
                        buf_rq_len) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }

  if (api_ssif_cmd (ctx,
		    obj_cmd_rq,
		    obj_cmd_rs) < 0)
    goto cleanup;

  if ((len = fiid_obj_get_all (obj_cmd_rs,
                               buf_rs,
                               buf_rs_len)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  rv = len;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
