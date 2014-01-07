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
#include <assert.h>
#include <errno.h>

#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/fiid/fiid.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-openipmi-driver-api.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_openipmi_raw =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8192, "raw_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

int
api_openipmi_cmd (ipmi_ctx_t ctx,
		  fiid_obj_t obj_cmd_rq,
		  fiid_obj_t obj_cmd_rs)
{
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_OPENIPMI
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));
  
  if (ipmi_openipmi_cmd (ctx->io.inband.openipmi_ctx,
                         ctx->target.lun,
                         ctx->target.net_fn,
                         obj_cmd_rq,
                         obj_cmd_rs) < 0)
    {
      API_OPENIPMI_ERRNUM_TO_API_ERRNUM (ctx, ipmi_openipmi_ctx_errnum (ctx->io.inband.openipmi_ctx));
      return (-1);
    }

  return (0);
}

int
api_openipmi_cmd_ipmb (ipmi_ctx_t ctx,
		       fiid_obj_t obj_cmd_rq,
		       fiid_obj_t obj_cmd_rs)
{
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_OPENIPMI
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  if (ipmi_openipmi_cmd_ipmb (ctx->io.inband.openipmi_ctx,
                              ctx->target.channel_number,
                              ctx->target.rs_addr,
                              ctx->target.lun,
                              ctx->target.net_fn,
                              obj_cmd_rq,
                              obj_cmd_rs) < 0)
    {
      API_OPENIPMI_ERRNUM_TO_API_ERRNUM (ctx, ipmi_openipmi_ctx_errnum (ctx->io.inband.openipmi_ctx));
      return (-1);
    }

  return (0);
}

int
api_openipmi_cmd_raw (ipmi_ctx_t ctx,
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
          && ctx->type == IPMI_DEVICE_OPENIPMI
          && buf_rq
          && buf_rq_len
          && buf_rs
          && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_openipmi_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_openipmi_raw)))
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

  if (api_openipmi_cmd (ctx,
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

int
api_openipmi_cmd_raw_ipmb (ipmi_ctx_t ctx,
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
          && ctx->type == IPMI_DEVICE_OPENIPMI
          && buf_rq
          && buf_rq_len
          && buf_rs
          && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_openipmi_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_openipmi_raw)))
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

  if (api_openipmi_cmd_ipmb (ctx,
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
