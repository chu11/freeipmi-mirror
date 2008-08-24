/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"
#include "ipmi-lan-session-common.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_lan_raw =
  {
    {8192, "raw_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

int8_t 
ipmi_lan_cmd (ipmi_ctx_t ctx, 
	      fiid_obj_t obj_cmd_rq,
	      fiid_obj_t obj_cmd_rs)
{
  uint8_t authentication_type;
  uint32_t internal_workaround_flags = 0;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_DEVICE_NOT_OPEN(ctx->type == IPMI_DEVICE_LAN);
  API_ERR_DEVICE_NOT_OPEN(ctx->io.outofband.sockfd); 

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rq)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_PACKET_VALID(obj_cmd_rq);
  
  API_ERR_INTERNAL_ERROR(ctx->type == IPMI_DEVICE_LAN);

  ipmi_lan_cmd_get_session_parameters (ctx,
				       &authentication_type,
				       &internal_workaround_flags);

  /* if auth type NONE, still pass password.  Needed for
   * check_unexpected_authcode workaround 
   */
  return ipmi_lan_cmd_wrapper (ctx, 
                               internal_workaround_flags,
                               ctx->lun,
                               ctx->net_fn,
                               authentication_type,
                               &(ctx->io.outofband.session_sequence_number),
                               ctx->io.outofband.session_id,
                               &(ctx->io.outofband.rq_seq),
                               ctx->io.outofband.password,
                               IPMI_1_5_MAX_PASSWORD_LENGTH,
                               obj_cmd_rq,
                               obj_cmd_rs);
}

int32_t 
ipmi_lan_cmd_raw (ipmi_ctx_t ctx, 
		  uint8_t *buf_rq, 
		  size_t buf_rq_len, 
		  uint8_t *buf_rs, 
		  size_t buf_rs_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int retval = -1;
  int32_t len;
  uint8_t authentication_type;
  uint32_t internal_workaround_flags = 0;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_DEVICE_NOT_OPEN(ctx->type == IPMI_DEVICE_LAN);
  API_ERR_DEVICE_NOT_OPEN(ctx->io.outofband.sockfd); 

  API_ERR_PARAMETERS (buf_rq
                      && buf_rq_len > 0
                      && buf_rs
                      && buf_rs_len > 0);

  API_ERR_INTERNAL_ERROR(ctx->type == IPMI_DEVICE_LAN);

  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rq, tmpl_lan_raw);
  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_lan_raw);

  API_FIID_OBJ_SET_ALL_CLEANUP (obj_cmd_rq, buf_rq, buf_rq_len);

  ipmi_lan_cmd_get_session_parameters (ctx,
				       &authentication_type,
				       &internal_workaround_flags);

  if (ipmi_lan_cmd_wrapper (ctx, 
                            internal_workaround_flags,
                            ctx->lun,
                            ctx->net_fn,
                            authentication_type,
                            &(ctx->io.outofband.session_sequence_number),
                            ctx->io.outofband.session_id,
                            &(ctx->io.outofband.rq_seq),
                            ctx->io.outofband.password,
                            IPMI_1_5_MAX_PASSWORD_LENGTH,
                            obj_cmd_rq,
                            obj_cmd_rs) < 0)
    goto cleanup;

  API_FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_cmd_rs, buf_rs, buf_rs_len);
  retval = len;

 cleanup:
  API_FIID_OBJ_DESTROY (obj_cmd_rq);
  API_FIID_OBJ_DESTROY (obj_cmd_rs);
  return (retval);
}

int8_t 
ipmi_lan_2_0_cmd (ipmi_ctx_t ctx, 
                  fiid_obj_t obj_cmd_rq,
                  fiid_obj_t obj_cmd_rs)
{
  uint8_t payload_authenticated;
  uint8_t payload_encrypted;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_DEVICE_NOT_OPEN(ctx->type == IPMI_DEVICE_LAN_2_0);
  API_ERR_DEVICE_NOT_OPEN(ctx->io.outofband.sockfd); 

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rq)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_PACKET_VALID(obj_cmd_rq);
  
  API_ERR_INTERNAL_ERROR(ctx->type == IPMI_DEVICE_LAN_2_0);

  ipmi_lan_2_0_cmd_get_session_parameters (ctx,
					   &payload_authenticated,
					   &payload_encrypted);

  return ipmi_lan_2_0_cmd_wrapper (ctx, 
                                   ctx->lun,
                                   ctx->net_fn,
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
                                   strlen(ctx->io.outofband.password) ? ctx->io.outofband.password : NULL,
                                   strlen(ctx->io.outofband.password),
                                   obj_cmd_rq,
                                   obj_cmd_rs);
}

int32_t 
ipmi_lan_2_0_cmd_raw (ipmi_ctx_t ctx, 
                      uint8_t *buf_rq, 
                      size_t buf_rq_len, 
                      uint8_t *buf_rs, 
                      size_t buf_rs_len)
{
  uint8_t payload_authenticated;
  uint8_t payload_encrypted;
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int retval = -1;
  int32_t len;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_DEVICE_NOT_OPEN(ctx->type == IPMI_DEVICE_LAN_2_0);
  API_ERR_DEVICE_NOT_OPEN(ctx->io.outofband.sockfd); 

  API_ERR_PARAMETERS (buf_rq
                      && buf_rq_len > 0
                      && buf_rs
                      && buf_rs_len > 0);

  API_ERR_INTERNAL_ERROR(ctx->type == IPMI_DEVICE_LAN_2_0);

  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rq, tmpl_lan_raw);
  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_lan_raw);

  API_FIID_OBJ_SET_ALL_CLEANUP (obj_cmd_rq, buf_rq, buf_rq_len);

  ipmi_lan_2_0_cmd_get_session_parameters (ctx,
					   &payload_authenticated,
					   &payload_encrypted);

  if (ipmi_lan_2_0_cmd_wrapper (ctx, 
                                ctx->lun,
                                ctx->net_fn,
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
                                strlen(ctx->io.outofband.password) ? ctx->io.outofband.password : NULL,
                                strlen(ctx->io.outofband.password),
                                obj_cmd_rq,
                                obj_cmd_rs) < 0)
    goto cleanup;

  API_FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_cmd_rs, buf_rs, buf_rs_len);
  retval = len;

 cleanup:
  API_FIID_OBJ_DESTROY (obj_cmd_rq);
  API_FIID_OBJ_DESTROY (obj_cmd_rs);
  return (retval);
}
