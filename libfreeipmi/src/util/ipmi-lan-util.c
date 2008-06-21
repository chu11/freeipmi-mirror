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

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
#include <errno.h>

#include "freeipmi/util/ipmi-lan-util.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"
#include "libcommon/ipmi-md2.h"
#include "libcommon/ipmi-md5.h"

#include "freeipmi-portability.h"
#include "secure.h"

int8_t 
ipmi_lan_check_session_sequence_number (fiid_obj_t obj_lan_session_hdr, uint32_t session_sequence_number)
{
  uint64_t session_sequence_number_recv;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_lan_session_hdr));

  FIID_OBJ_FIELD_LOOKUP (obj_lan_session_hdr, "session_sequence_number");

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, "session_sequence_number");
  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_lan_session_hdr, "session_sequence_number", &session_sequence_number_recv);

  return ((((uint32_t)session_sequence_number_recv) == session_sequence_number) ? 1 : 0);
}

int8_t 
ipmi_lan_check_session_id (fiid_obj_t obj_lan_session_hdr, uint32_t session_id)
{
  uint64_t session_id_recv;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_lan_session_hdr));

  FIID_OBJ_FIELD_LOOKUP (obj_lan_session_hdr, "session_id");

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, "session_id");
  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_lan_session_hdr, "session_id", &session_id_recv);

  return ((((uint32_t)session_id_recv) == session_id) ? 1 : 0);
}

int8_t 
ipmi_lan_check_session_authentication_code (fiid_obj_t obj_lan_session_hdr_rs,
                                            fiid_obj_t obj_lan_msg_hdr_rs,
                                            fiid_obj_t obj_cmd,
                                            fiid_obj_t obj_lan_msg_trlr_rs,
                                            uint8_t authentication_type, 
                                            uint8_t *authentication_code_data, 
                                            uint32_t authentication_code_data_len)
{
  uint8_t authentication_code_recv[IPMI_1_5_MAX_PASSWORD_LENGTH];
  uint8_t authentication_code_calc[IPMI_1_5_MAX_PASSWORD_LENGTH];
  int32_t authentication_code_recv_len;
  uint8_t pwbuf[IPMI_1_5_MAX_PASSWORD_LENGTH];
  uint64_t val;
  int rv = -1;

  ERR_EINVAL (fiid_obj_valid(obj_lan_session_hdr_rs)
              && fiid_obj_valid(obj_lan_msg_hdr_rs)
              && fiid_obj_valid(obj_cmd)
              && fiid_obj_valid(obj_lan_msg_trlr_rs)
	      && (authentication_type == IPMI_AUTHENTICATION_TYPE_NONE
                  || authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
                  || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
                  || authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
	      && !(authentication_code_data && authentication_code_data_len > IPMI_1_5_MAX_PASSWORD_LENGTH));

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_session_hdr_rs, tmpl_lan_session_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr_rs, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr_rs, tmpl_lan_msg_trlr);

  FIID_OBJ_PACKET_VALID(obj_lan_session_hdr_rs);
  FIID_OBJ_PACKET_VALID(obj_lan_msg_hdr_rs);
  FIID_OBJ_PACKET_VALID(obj_lan_msg_trlr_rs);

  FIID_OBJ_GET(obj_lan_session_hdr_rs, "authentication_type", &val);

  if (authentication_type != val)
    return (0);

  memset(authentication_code_recv, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  FIID_OBJ_GET_DATA_LEN(authentication_code_recv_len,
                        obj_lan_session_hdr_rs,
                        "authentication_code",
                        authentication_code_recv,
                        IPMI_1_5_MAX_PASSWORD_LENGTH);
  
  if (authentication_type == IPMI_AUTHENTICATION_TYPE_NONE)
    {
      if (authentication_code_recv_len)
	rv = 0;
      else
	rv = 1;
      goto cleanup;
    }

  memset(authentication_code_calc, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  if (authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
    memcpy(authentication_code_calc, authentication_code_data, authentication_code_data_len);
  else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
           || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
    {
      int32_t obj_lan_msg_hdr_len, obj_cmd_len, obj_lan_msg_trlr_len, obj_len, len;
      uint8_t session_id_buf[1024];
      uint8_t session_sequence_number_buf[1024];
      int32_t session_id_len, session_sequence_number_len;
      uint8_t *buf;
      uint32_t buflen;

      FIID_OBJ_GET_DATA_LEN_CLEANUP (session_id_len,
				     obj_lan_session_hdr_rs,
				     "session_id",
				     session_id_buf,
				     1024);

      FIID_OBJ_GET_DATA_LEN_CLEANUP (session_sequence_number_len,
				     obj_lan_session_hdr_rs,
				     "session_sequence_number",
				     session_sequence_number_buf,
				     1024);
      
      /* Must zero extend password.  No null termination is required.
       * Also, must memcpy instead of strcpy, password need not be
       * 1 word
       */
      memset(pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
      memcpy(pwbuf, authentication_code_data, authentication_code_data_len);

      FIID_OBJ_LEN_BYTES_CLEANUP (obj_lan_msg_hdr_len, obj_lan_msg_hdr_rs);
      FIID_OBJ_LEN_BYTES_CLEANUP (obj_cmd_len, obj_cmd);
      FIID_OBJ_LEN_BYTES_CLEANUP (obj_lan_msg_trlr_len, obj_lan_msg_trlr_rs);
      
      buflen = obj_lan_msg_hdr_len + obj_cmd_len + obj_lan_msg_trlr_len;
      ERR ((buf = (uint8_t *)alloca(buflen)));
      
      len = 0;
      FIID_OBJ_GET_ALL_LEN_CLEANUP (obj_len, obj_lan_msg_hdr_rs, buf + len, buflen - len);
      len += obj_len;
      
      FIID_OBJ_GET_ALL_LEN_CLEANUP (obj_len, obj_cmd, buf + len, buflen - len);
      len += obj_len;
      
      FIID_OBJ_GET_ALL_LEN_CLEANUP (obj_len, obj_lan_msg_trlr_rs, buf + len, buflen - len);
      len += obj_len;

      if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
        {
          md2_t ctx;
          
          ERR_EXIT(IPMI_1_5_MAX_PASSWORD_LENGTH == MD2_DIGEST_LENGTH);
          
          md2_init(&ctx);
          md2_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
          md2_update_data(&ctx, session_id_buf, session_id_len);
          md2_update_data(&ctx, buf, len);
          md2_update_data(&ctx, session_sequence_number_buf, session_sequence_number_len);
          md2_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
          md2_finish(&ctx, authentication_code_calc, IPMI_1_5_MAX_PASSWORD_LENGTH);
          md2_init(&ctx);
        }
      else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
        {
          md5_t ctx;

          ERR_EXIT(IPMI_1_5_MAX_PASSWORD_LENGTH == MD5_DIGEST_LENGTH);

          md5_init(&ctx);
          md5_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
          md5_update_data(&ctx, session_id_buf, session_id_len);
          md5_update_data(&ctx, buf, len);
          md5_update_data(&ctx, session_sequence_number_buf, session_sequence_number_len);
          md5_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
          md5_finish(&ctx, authentication_code_calc, IPMI_1_5_MAX_PASSWORD_LENGTH);
          md5_init(&ctx);
        }
    }

  /* Must memcmp instead of strcmp, password need not be 1 word */
  if (memcmp(authentication_code_recv, authentication_code_calc, IPMI_1_5_MAX_PASSWORD_LENGTH) == 0)
    rv = 1;
  else
    rv = 0;

 cleanup:
  secure_memset(authentication_code_recv, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  secure_memset(authentication_code_calc, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  secure_memset(pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  return rv;
}

int8_t 
ipmi_lan_check_packet_session_authentication_code (uint8_t *pkt, uint64_t pkt_len, uint8_t authentication_type, uint8_t *authentication_code_data, uint32_t authentication_code_data_len)
{
  uint8_t authentication_type_recv;
  int32_t rmcp_hdr_len, authentication_type_index, authentication_code_index;
  uint32_t authentication_type_offset, authentication_code_offset;
  uint8_t authentication_code_buf[IPMI_1_5_MAX_PASSWORD_LENGTH];
  uint8_t pwbuf[IPMI_1_5_MAX_PASSWORD_LENGTH];
  int rv = -1;

  ERR_EINVAL (pkt 
	      && (authentication_type == IPMI_AUTHENTICATION_TYPE_NONE
                  || authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
                  || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
                  || authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
	      && !(authentication_code_data && authentication_code_data_len > IPMI_1_5_MAX_PASSWORD_LENGTH));

  FIID_TEMPLATE_LEN_BYTES (rmcp_hdr_len, tmpl_rmcp_hdr);
  FIID_TEMPLATE_FIELD_START_BYTES (authentication_type_index, 
				   tmpl_lan_session_hdr, 
				   "authentication_type");
  authentication_type_offset = rmcp_hdr_len + authentication_type_index;

  if (pkt_len < authentication_type_offset)
    return 0;

  authentication_type_recv = pkt[authentication_type_offset];

  /* authentication code check fails if authentication types do not match */
  if (authentication_type != authentication_type_recv)
    return 0;

  /* Automatically return 1 if auth type is none */
  if (authentication_type_recv == IPMI_AUTHENTICATION_TYPE_NONE)
    return 1;

  FIID_TEMPLATE_FIELD_START_BYTES (authentication_code_index, 
				   tmpl_lan_session_hdr, 
				   "authentication_code");
  authentication_code_offset = rmcp_hdr_len + authentication_code_index;

  if (pkt_len < (authentication_code_offset + IPMI_1_5_MAX_PASSWORD_LENGTH))
    return 0;

  memset(authentication_code_buf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
    {
      int32_t session_id_index, session_sequence_number_index, data_index;
      uint32_t session_id_offset, session_sequence_number_offset, data_offset;
      int32_t session_id_len, session_sequence_number_len;
      
      FIID_TEMPLATE_FIELD_START_BYTES_CLEANUP (session_id_index, 
					       tmpl_lan_session_hdr, 
					       "session_id");
      FIID_TEMPLATE_FIELD_START_BYTES_CLEANUP (session_sequence_number_index, 
					       tmpl_lan_session_hdr, 
					       "session_sequence_number");
      FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP (session_id_len,
					     tmpl_lan_session_hdr,
					     "session_id");
      FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP (session_sequence_number_len, 
					     tmpl_lan_session_hdr, 
					     "session_sequence_number");
      FIID_TEMPLATE_LEN_BYTES_CLEANUP (data_index, tmpl_lan_session_hdr);
      
      session_id_offset = rmcp_hdr_len + session_id_index;
      session_sequence_number_offset = rmcp_hdr_len + session_sequence_number_index;
      data_offset = rmcp_hdr_len + data_index;
      
      if (pkt_len < (session_id_offset + session_id_len)
	  || pkt_len < (session_sequence_number_offset + session_sequence_number_len)
	  || pkt_len < data_offset)
	{
	  rv = 0;
	  goto cleanup;
	}
      
      /* Must zero extend password.  No null termination is required.
       * Also, must memcpy instead of strcpy, password need not be
       * 1 word
       */
      memset(pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
      memcpy(pwbuf, authentication_code_data, authentication_code_data_len);
      if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
	{
	  md2_t ctx;
	  
	  ERR_EXIT(IPMI_1_5_MAX_PASSWORD_LENGTH == MD2_DIGEST_LENGTH);
	  
	  md2_init(&ctx);
	  md2_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
	  md2_update_data(&ctx,
			  pkt + session_id_offset,
			  session_id_len);
	  md2_update_data(&ctx,
			  pkt + data_offset,
			  pkt_len - data_offset);
	  md2_update_data(&ctx,
			  pkt + session_sequence_number_offset,
			  session_sequence_number_len);
	  md2_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
	  md2_finish(&ctx, authentication_code_buf, IPMI_1_5_MAX_PASSWORD_LENGTH);
	  md2_init(&ctx);
	}
      else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
	{
	  md5_t ctx;
	  
	  ERR_EXIT(IPMI_1_5_MAX_PASSWORD_LENGTH == MD5_DIGEST_LENGTH);
	  
	  md5_init(&ctx);
	  md5_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
	  md5_update_data(&ctx,
			  pkt + session_id_offset,
			  session_id_len);
	  md5_update_data(&ctx,
			  pkt + data_offset,
			  pkt_len - data_offset);
	  md5_update_data(&ctx,
			  pkt + session_sequence_number_offset,
			  session_sequence_number_len);
	  md5_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
	  md5_finish(&ctx, authentication_code_buf, IPMI_1_5_MAX_PASSWORD_LENGTH);
	  md5_init(&ctx);
	}
    }
  else /* authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
	  || authentication_type == IPMI_AUTHENTICATION_TYPE_OEM_PROP */
    {
      if (authentication_code_data)
	memcpy(authentication_code_buf, authentication_code_data, authentication_code_data_len);
    }

  /* Must memcmp instead of strcmp, password need not be 1 word */
  if (memcmp(authentication_code_buf, pkt + authentication_code_offset, IPMI_1_5_MAX_PASSWORD_LENGTH) == 0)
    rv = 1;
  else
    rv = 0;

 cleanup:
  secure_memset(authentication_code_buf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  secure_memset(pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  return rv;
}

int8_t 
ipmi_lan_check_net_fn (fiid_obj_t obj_lan_msg_hdr, uint8_t net_fn)
{
  uint64_t net_fn_recv;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_lan_msg_hdr)
	      && IPMI_NET_FN_VALID(net_fn));

  FIID_OBJ_FIELD_LOOKUP (obj_lan_msg_hdr, "net_fn");

  FIID_OBJ_FIELD_LEN (len, obj_lan_msg_hdr, "net_fn");
  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_lan_msg_hdr, "net_fn", &net_fn_recv);

  return ((((uint8_t)net_fn_recv) == net_fn) ? 1 : 0);
}

int8_t 
ipmi_lan_check_rq_seq (fiid_obj_t obj_lan_msg_hdr, uint8_t rq_seq)
{
  uint64_t rq_seq_recv;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_lan_msg_hdr));

  FIID_OBJ_FIELD_LOOKUP (obj_lan_msg_hdr, "rq_seq");

  FIID_OBJ_FIELD_LEN (len, obj_lan_msg_hdr, "rq_seq");
  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_lan_msg_hdr, "rq_seq", &rq_seq_recv);

  return ((((uint8_t)rq_seq_recv) == rq_seq) ? 1 : 0);
}

int8_t
ipmi_lan_check_checksum (fiid_obj_t obj_lan_msg_hdr,
                         fiid_obj_t obj_cmd,
                         fiid_obj_t obj_lan_msg_trlr)
{
  int32_t obj_lan_msg_hdr_len, obj_cmd_len, obj_len, len, req_len;
  uint8_t checksum1_recv, checksum1_calc, checksum2_recv, checksum2_calc;
  uint8_t *buf = NULL;
  uint32_t buflen;
  uint64_t val;
  
  ERR_EINVAL (fiid_obj_valid(obj_lan_msg_hdr)
              && fiid_obj_valid(obj_cmd)
              && fiid_obj_valid(obj_lan_msg_trlr));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  FIID_OBJ_FIELD_LEN (len, obj_lan_msg_hdr, "checksum1");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_msg_hdr_rs, "checksum1");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_lan_msg_trlr, "checksum2");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_msg_trlr, "checksum2");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_LEN_BYTES (obj_lan_msg_hdr_len, obj_lan_msg_hdr);
  FIID_OBJ_LEN_BYTES (obj_cmd_len, obj_cmd);

  FIID_OBJ_GET (obj_lan_msg_hdr, "checksum1", &val);
  checksum1_recv = val;
  ERR ((buf = (uint8_t *)alloca(obj_lan_msg_hdr_len)));
  FIID_OBJ_GET_BLOCK_LEN(len, obj_lan_msg_hdr, "rq_addr", "net_fn", buf, obj_lan_msg_hdr_len);
  checksum1_calc = ipmi_checksum(buf, len);

  if (checksum1_recv != checksum1_calc)
    return (0);

  FIID_OBJ_GET (obj_lan_msg_trlr, "checksum2", &val);
  checksum2_recv = val;

  buflen = obj_lan_msg_hdr_len + obj_cmd_len;
  ERR ((buf = (uint8_t *)alloca(buflen)));

  len = 0;
  FIID_OBJ_GET_BLOCK_LEN(obj_len, obj_lan_msg_hdr, "rs_addr", "rq_seq", buf, buflen - len);
  len += obj_len;
  FIID_OBJ_GET_ALL_LEN(obj_len, obj_cmd, buf + len, buflen - len);
  len += obj_len;

  checksum2_calc = ipmi_checksum(buf, len);
  if (checksum2_recv != checksum2_calc)
    return (0);

  return (1);
}

int8_t 
ipmi_lan_check_packet_checksum (uint8_t *pkt, uint64_t pkt_len)
{
  uint8_t authentication_type;
  uint32_t authentication_type_offset;
  int32_t rmcp_hdr_len, msg_hdr_len1, msg_hdr_len2, authentication_code_len;
  int32_t authentication_type_start_bytes;
  int32_t checksum1_block_index, checksum1_block_len, 
    checksum2_block_index, checksum2_block_len;
  uint8_t checksum1_recv, checksum1_calc, checksum2_recv, checksum2_calc;

  ERR_EINVAL (pkt && pkt_len);
  
  FIID_TEMPLATE_LEN_BYTES (rmcp_hdr_len, tmpl_rmcp_hdr);

  FIID_TEMPLATE_FIELD_START_BYTES (authentication_type_start_bytes, 
				   tmpl_lan_session_hdr, 
				   "authentication_type");
  
  authentication_type_offset = rmcp_hdr_len + authentication_type_start_bytes;
  authentication_type = pkt[authentication_type_offset];

  FIID_TEMPLATE_BLOCK_LEN_BYTES (msg_hdr_len1,
				 tmpl_lan_session_hdr,
				 "authentication_type",
				 "session_id");

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    authentication_code_len = IPMI_1_5_MAX_PASSWORD_LENGTH;
  else
    authentication_code_len = 0;

  FIID_TEMPLATE_FIELD_LEN_BYTES(msg_hdr_len2,
				tmpl_lan_session_hdr,
				"ipmi_msg_len");

  checksum1_block_index = rmcp_hdr_len + msg_hdr_len1 + authentication_code_len + msg_hdr_len2;

  FIID_TEMPLATE_BLOCK_LEN_BYTES(checksum1_block_len,
				tmpl_lan_msg_hdr_rs,
				"rq_addr",
				"net_fn");

  if (pkt_len < (checksum1_block_index + checksum1_block_len + 1))
    return (0);

  checksum1_calc = ipmi_checksum(pkt + checksum1_block_index, checksum1_block_len);
  checksum1_recv = pkt[checksum1_block_index + checksum1_block_len];

  if (checksum1_calc != checksum1_recv)
    return (0);

  checksum2_block_index = checksum1_block_index + checksum1_block_len + 1;

  if (pkt_len <= (checksum2_block_index + 1))
    return (0);

  checksum2_block_len = pkt_len - checksum2_block_index - 1;
  
  checksum2_calc = ipmi_checksum(pkt + checksum2_block_index, checksum2_block_len);
  checksum2_recv = pkt[checksum2_block_index + checksum2_block_len];

  if (checksum2_calc != checksum2_recv)
    return (0);

  return (1);
}
