/* 
   ipmi-lan.c - IPMI LAN Utils

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-lan-utils.h"
#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-lan.h"
#include "freeipmi/ipmi-netfn-spec.h"
#include "freeipmi/ipmi-utils.h"
#include "freeipmi/rmcp.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "md2.h"
#include "md5.h"

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
ipmi_lan_check_session_authentication_code (uint8_t *pkt, uint64_t pkt_len, uint8_t authentication_type, uint8_t *authentication_code_data, uint32_t authentication_code_data_len)
{
  uint8_t authentication_type_recv;
  int32_t rmcp_hdr_len, authentication_type_index, authentication_code_index;
  uint32_t authentication_type_offset, authentication_code_offset;
  uint8_t authentication_code_buf[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];

  ERR_EINVAL (pkt 
	      && IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
	      && !(authentication_code_data && authentication_code_data_len > IPMI_MAX_AUTHENTICATION_CODE_LENGTH));

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

  if (pkt_len < (authentication_code_offset + IPMI_MAX_AUTHENTICATION_CODE_LENGTH))
    return 0;

  memset(authentication_code_buf, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH);

  if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
    {
      uint8_t pwbuf[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];
      int32_t session_id_index, session_sequence_number_index, data_index;
      uint32_t session_id_offset, session_sequence_number_offset, data_offset;
      int32_t session_id_len, session_sequence_number_len;
      
      FIID_TEMPLATE_FIELD_START_BYTES (session_id_index, 
				       tmpl_lan_session_hdr, 
				       "session_id");
      FIID_TEMPLATE_FIELD_START_BYTES (session_sequence_number_index, 
				       tmpl_lan_session_hdr, 
				       "session_sequence_number");
      FIID_TEMPLATE_FIELD_LEN_BYTES (session_id_len,
                                     tmpl_lan_session_hdr,
                                     "session_id");
      FIID_TEMPLATE_FIELD_LEN_BYTES (session_sequence_number_len, 
				     tmpl_lan_session_hdr, 
				     "session_sequence_number");
      FIID_TEMPLATE_LEN_BYTES (data_index, tmpl_lan_session_hdr);
      
      session_id_offset = rmcp_hdr_len + session_id_index;
      session_sequence_number_offset = rmcp_hdr_len + session_sequence_number_index;
      data_offset = rmcp_hdr_len + data_index;
      
      if (pkt_len < (session_id_offset + session_id_len)
	  || pkt_len < (session_sequence_number_offset + session_sequence_number_len)
	  || pkt_len < data_offset)
	return 0;
      
      /* Must zero extend password.  No null termination is required.
       * Also, must memcpy instead of strcpy, password need not be
       * 1 word
       */
      memset(pwbuf, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
      memcpy(pwbuf, authentication_code_data, authentication_code_data_len);
      if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
	{
	  md2_t ctx;
	  
	  ERR_EXIT(IPMI_MAX_AUTHENTICATION_CODE_LENGTH == MD2_DIGEST_LENGTH);
	  
	  md2_init(&ctx);
	  md2_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	  md2_update_data(&ctx,
			       pkt + session_id_offset,
			       session_id_len);
	  md2_update_data(&ctx,
			       pkt + data_offset,
			       pkt_len - data_offset);
	  md2_update_data(&ctx,
			       pkt + session_sequence_number_offset,
			       session_sequence_number_len);
	  md2_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	  md2_finish(&ctx, authentication_code_buf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	}
      else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
	{
	  md5_t ctx;
	  
	  ERR_EXIT(IPMI_MAX_AUTHENTICATION_CODE_LENGTH == MD5_DIGEST_LENGTH);
	  
	  md5_init(&ctx);
	  md5_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	  md5_update_data(&ctx,
			       pkt + session_id_offset,
			       session_id_len);
	  md5_update_data(&ctx,
			       pkt + data_offset,
			       pkt_len - data_offset);
	  md5_update_data(&ctx,
			       pkt + session_sequence_number_offset,
			       session_sequence_number_len);
	  md5_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	  md5_finish(&ctx, authentication_code_buf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	  
	}
    }
  else /* authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
	  || authentication_type == IPMI_AUTHENTICATION_TYPE_OEM_PROP */
    {
      if (authentication_code_data)
	memcpy(authentication_code_buf, authentication_code_data, authentication_code_data_len);
    }

  /* Must memcmp instead of strcmp, password need not be 1 word */
  if (memcmp(authentication_code_buf, pkt + authentication_code_offset, IPMI_MAX_AUTHENTICATION_CODE_LENGTH) == 0)
    return 1;
  else
    return 0;
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
ipmi_lan_check_checksum (uint8_t *pkt, uint64_t pkt_len)
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
    authentication_code_len = IPMI_MAX_AUTHENTICATION_CODE_LENGTH;
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
