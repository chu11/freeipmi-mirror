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

#include <sys/types.h>

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

#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-lan-session-common.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_lan_raw =
  {
    { 8192, "raw_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

int
api_lan_cmd (ipmi_ctx_t ctx,
	     fiid_obj_t obj_cmd_rq,
	     fiid_obj_t obj_cmd_rs)
{
  uint8_t authentication_type;
  unsigned int internal_workaround_flags = 0;
  int ret;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN
	  && ctx->io.outofband.sockfd
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  api_lan_cmd_get_session_parameters (ctx,
				      &authentication_type,
				      &internal_workaround_flags);

  if (ctx->flags & IPMI_FLAGS_NOSESSION)
    ret = api_lan_cmd_wrapper (ctx,
			       internal_workaround_flags,
			       ctx->target.lun,
			       ctx->target.net_fn,
			       IPMI_AUTHENTICATION_TYPE_NONE,
			       0,
			       NULL,
			       0,
			       &(ctx->io.outofband.rq_seq),
			       NULL,
			       0,
			       obj_cmd_rq,
			       obj_cmd_rs);
  else
    /* if auth type NONE, still pass password.  Needed for
     * check_unexpected_authcode workaround
     */
    ret = api_lan_cmd_wrapper (ctx,
			       internal_workaround_flags,
			       ctx->target.lun,
			       ctx->target.net_fn,
			       authentication_type,
			       1,
			       &(ctx->io.outofband.session_sequence_number),
			       ctx->io.outofband.session_id,
			       &(ctx->io.outofband.rq_seq),
			       ctx->io.outofband.password,
			       IPMI_1_5_MAX_PASSWORD_LENGTH,
			       obj_cmd_rq,
			       obj_cmd_rs);

  return (ret);
}

int
api_lan_cmd_ipmb (ipmi_ctx_t ctx,
		  fiid_obj_t obj_cmd_rq,
		  fiid_obj_t obj_cmd_rs)
{
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN
	  && ctx->io.outofband.sockfd
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  /* if auth type NONE, still pass password.  Needed for
   * check_unexpected_authcode workaround
   */
  return (api_lan_cmd_wrapper_ipmb (ctx,
				    obj_cmd_rq,
				    obj_cmd_rs));
}

int
api_lan_cmd_raw (ipmi_ctx_t ctx,
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
          && ctx->type == IPMI_DEVICE_LAN
	  && ctx->io.outofband.sockfd
	  && buf_rq
          && buf_rq_len
          && buf_rs
          && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_lan_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_lan_raw)))
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

  if (api_lan_cmd (ctx,
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
api_lan_cmd_raw_ipmb (ipmi_ctx_t ctx,
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
          && ctx->type == IPMI_DEVICE_LAN
	  && ctx->io.outofband.sockfd
	  && buf_rq
          && buf_rq_len
          && buf_rs
          && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_lan_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_lan_raw)))
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

  if (api_lan_cmd_ipmb (ctx,
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
api_lan_2_0_cmd (ipmi_ctx_t ctx,
		 fiid_obj_t obj_cmd_rq,
		 fiid_obj_t obj_cmd_rs)
{
  uint8_t payload_authenticated;
  uint8_t payload_encrypted;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0
	  && ctx->io.outofband.sockfd
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  api_lan_2_0_cmd_get_session_parameters (ctx,
					  &payload_authenticated,
					  &payload_encrypted);

  return (api_lan_2_0_cmd_wrapper (ctx,
				   0,
				   ctx->target.lun,
				   ctx->target.net_fn,
				   IPMI_PAYLOAD_TYPE_IPMI,
				   payload_authenticated,
				   payload_encrypted,
				   NULL,
				   &(ctx->io.outofband.session_sequence_number),
				   ctx->io.outofband.managed_system_session_id,
				   &(ctx->io.outofband.rq_seq),
				   ctx->io.outofband.authentication_algorithm,
				   ctx->io.outofband.integrity_algorithm,
				   ctx->io.outofband.confidentiality_algorithm,
				   ctx->io.outofband.integrity_key_ptr,
				   ctx->io.outofband.integrity_key_len,
				   ctx->io.outofband.confidentiality_key_ptr,
				   ctx->io.outofband.confidentiality_key_len,
				   strlen (ctx->io.outofband.password) ? ctx->io.outofband.password : NULL,
				   strlen (ctx->io.outofband.password),
				   obj_cmd_rq,
				   obj_cmd_rs));
}

int
api_lan_2_0_cmd_ipmb (ipmi_ctx_t ctx,
		      fiid_obj_t obj_cmd_rq,
		      fiid_obj_t obj_cmd_rs)
{
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0
	  && ctx->io.outofband.sockfd
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  return (api_lan_2_0_cmd_wrapper_ipmb (ctx,
					obj_cmd_rq,
					obj_cmd_rs));
}

int
api_lan_2_0_cmd_raw (ipmi_ctx_t ctx,
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
          && ctx->type == IPMI_DEVICE_LAN_2_0
	  && ctx->io.outofband.sockfd
	  && buf_rq
          && buf_rq_len
          && buf_rs
          && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_lan_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_lan_raw)))
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

  if (api_lan_2_0_cmd (ctx,
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
api_lan_2_0_cmd_raw_ipmb (ipmi_ctx_t ctx,
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
          && ctx->type == IPMI_DEVICE_LAN_2_0
	  && ctx->io.outofband.sockfd
	  && buf_rq
          && buf_rq_len
          && buf_rs
          && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_lan_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_lan_raw)))
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

  if (api_lan_2_0_cmd_ipmb (ctx,
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
