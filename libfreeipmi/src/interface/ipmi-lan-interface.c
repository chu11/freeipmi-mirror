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
#include <assert.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"
#include "libcommon/ipmi-md2.h"
#include "libcommon/ipmi-md5.h"

#include "freeipmi-portability.h"
#include "secure.h"

#define IPMI_LAN_PKT_PAD_SIZE   1

fiid_template_t tmpl_lan_session_hdr =
  {
    {8,   "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "session_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "ipmi_msg_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0,   "", 0}
  };

/* IPMI LAN Message Request Header */
fiid_template_t tmpl_lan_msg_hdr_rq =
  {
    {8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* IPMI LAN Message Response Header */
fiid_template_t tmpl_lan_msg_hdr_rs =
  {
    {8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* IPMI LAN Message Trailer */
fiid_template_t tmpl_lan_msg_trlr = 
  {
    {8, "checksum2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
fill_lan_session_hdr  (uint8_t authentication_type, 
		       uint32_t session_sequence_number, 
		       uint32_t session_id, 
		       fiid_obj_t obj_lan_session_hdr)
{
  ERR_EINVAL (IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
	      && fiid_obj_valid(obj_lan_session_hdr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_session_hdr, tmpl_lan_session_hdr);

  FIID_OBJ_CLEAR (obj_lan_session_hdr);
  FIID_OBJ_SET (obj_lan_session_hdr, "authentication_type", authentication_type);
  FIID_OBJ_SET (obj_lan_session_hdr, "session_sequence_number", session_sequence_number);
  FIID_OBJ_SET (obj_lan_session_hdr, "session_id", session_id);

  /* authentication_code_data calculated in assemble_ipmi_lan_pkt */
  /* ipmi_msg_len calculated in assemble_ipmi_lan_pkt */

  return (0);
}

int8_t 
fill_lan_msg_hdr (uint8_t rs_addr,
                  uint8_t net_fn, 
		  uint8_t rs_lun, 
		  uint8_t rq_seq, 
		  fiid_obj_t obj_lan_msg_hdr)
{
  uint8_t checksum_buf[1024];
  int32_t checksum_len;
  uint8_t checksum;

  ERR_EINVAL (IPMI_NET_FN_VALID(net_fn)
	      && IPMI_BMC_LUN_VALID(rs_lun)
	      && !(rq_seq > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
	      && fiid_obj_valid(obj_lan_msg_hdr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq);

  FIID_OBJ_CLEAR (obj_lan_msg_hdr);
  FIID_OBJ_SET (obj_lan_msg_hdr, "rs_addr", rs_addr);
  FIID_OBJ_SET (obj_lan_msg_hdr, "net_fn", net_fn);
  FIID_OBJ_SET (obj_lan_msg_hdr, "rs_lun", rs_lun);
  
  FIID_OBJ_GET_BLOCK_LEN (checksum_len,
			  obj_lan_msg_hdr, 
			  "rs_addr", 
			  "net_fn", 
			  checksum_buf, 
			  1024);

  checksum = ipmi_checksum(checksum_buf, checksum_len);
  FIID_OBJ_SET (obj_lan_msg_hdr, "checksum1", checksum);
  FIID_OBJ_SET (obj_lan_msg_hdr, "rq_addr", IPMI_LAN_SOFTWARE_ID_REMOTE_CONSOLE_SOFTWARE);
  FIID_OBJ_SET (obj_lan_msg_hdr, "rq_lun", IPMI_BMC_IPMB_LUN_BMC);
  FIID_OBJ_SET (obj_lan_msg_hdr, "rq_seq", rq_seq);

  return (0);
}

static int32_t
_ipmi_lan_pkt_rq_min_size(uint8_t authentication_type, fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;

  assert(IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type) && fiid_obj_valid(obj_cmd));

  FIID_TEMPLATE_LEN_BYTES (len, tmpl_rmcp_hdr);
  msg_len += len;
  FIID_TEMPLATE_LEN_BYTES (len, tmpl_lan_msg_hdr_rq);
  msg_len += len;
  FIID_TEMPLATE_LEN_BYTES (len, tmpl_lan_msg_trlr);
  msg_len += len;
  FIID_TEMPLATE_BLOCK_LEN_BYTES (len,
				 tmpl_lan_session_hdr,
				 "authentication_type",
				 "session_id");
  msg_len += len;
  
  if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
      || authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
      || authentication_type == IPMI_AUTHENTICATION_TYPE_OEM_PROP) 
    msg_len += IPMI_1_5_MAX_PASSWORD_LENGTH;
  
  FIID_TEMPLATE_FIELD_LEN_BYTES (len,
				 tmpl_lan_session_hdr,
				 "ipmi_msg_len");
  msg_len += len;

  FIID_OBJ_LEN_BYTES (len, obj_cmd);
  msg_len += len;

  return msg_len;
}
                       
/*
  Complete IPMI LAN Request Packet
  +----------------------+
  |  RMCP                |
  |  Session             |
  |  Message             |
  |  Command             |
  |    Data              |
  |  Checksum            |
  +----------------------+
*/

int32_t 
assemble_ipmi_lan_pkt (fiid_obj_t obj_rmcp_hdr, 
		       fiid_obj_t obj_lan_session_hdr, 
		       fiid_obj_t obj_lan_msg_hdr, 
		       fiid_obj_t obj_cmd, 
		       uint8_t *authentication_code_data,
		       uint32_t authentication_code_data_len,
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  uint64_t authentication_type;
  uint32_t indx, required_len;
  uint8_t *authentication_code_field_ptr = NULL;
  uint8_t *checksum_data_ptr = NULL;
  uint8_t *msg_data_ptr = NULL;
  uint8_t *ipmi_msg_len_ptr = NULL;
  uint32_t msg_data_count = 0;
  uint32_t checksum_data_count = 0;
  int32_t len, req_len;
  uint8_t ipmi_msg_len;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  uint8_t pwbuf[IPMI_1_5_MAX_PASSWORD_LENGTH];
  uint8_t checksum;
  int32_t rv = -1;

  ERR_EINVAL (fiid_obj_valid(obj_rmcp_hdr)
	      && fiid_obj_valid(obj_lan_session_hdr) 
	      && fiid_obj_valid(obj_lan_msg_hdr) 
	      && fiid_obj_valid(obj_cmd) 
	      && !(authentication_code_data && authentication_code_data_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
	      && pkt);
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_session_hdr, tmpl_lan_session_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq);
  FIID_OBJ_PACKET_VALID(obj_rmcp_hdr);

  /* 
   * ipmi_msg_len is calculted in this function, so we can't use
   * FIID_OBJ_PACKET_VALID() b/c ipmi_msg_len is probably not set yet.
   */

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, "authentication_type");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_session_hdr, "authentication_type");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, "session_sequence_number");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_session_hdr, "session_sequence_number");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, "session_id");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_session_hdr, "session_id");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_PACKET_VALID(obj_lan_msg_hdr);
  FIID_OBJ_PACKET_VALID(obj_cmd);

  FIID_OBJ_GET (obj_lan_session_hdr, "authentication_type", &authentication_type);
  ERR_EINVAL (authentication_type == IPMI_AUTHENTICATION_TYPE_NONE
	      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
	      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
	      || authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY);
  
  required_len = _ipmi_lan_pkt_rq_min_size((uint8_t)authentication_type, obj_cmd);
  ERR_EMSGSIZE (!(pkt_len < required_len));

  memset (pkt, 0, pkt_len);

  indx = 0;

  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += len;

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len, 
				 obj_lan_session_hdr,
				 "authentication_type",
				 "session_id",
				 pkt + indx,
				 pkt_len - indx);
  indx += len;

  /* authentication_code generated last.  Save pointers for later calculation */
  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      authentication_code_field_ptr = (pkt + indx); 
      indx += IPMI_1_5_MAX_PASSWORD_LENGTH;
    }
  
  ipmi_msg_len_ptr = (pkt + indx);
  FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP(len,
					tmpl_lan_session_hdr,
					"ipmi_msg_len");
  ERR_CLEANUP (len == 1);
  indx += len;

  msg_data_ptr = (pkt + indx);

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
				 obj_lan_msg_hdr,
				 "rs_addr",
				 "checksum1",
				 pkt + indx,
				 pkt_len - indx);
  indx += len;
  msg_data_count += len;

  checksum_data_ptr = (pkt + indx);

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
				 obj_lan_msg_hdr,
				 "rq_addr",
				 "rq_seq",
				 pkt + indx,
				 pkt_len - indx);
  indx += len;
  msg_data_count += len;
  checksum_data_count += len;

  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_cmd, pkt + indx, pkt_len - indx);
  indx += len;
  msg_data_count += len;
  checksum_data_count += len;

  FIID_OBJ_CREATE_CLEANUP(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  checksum = ipmi_checksum (checksum_data_ptr, checksum_data_count);
  
  FIID_OBJ_SET_ALL_CLEANUP (obj_lan_msg_trlr, &checksum, sizeof(checksum));
  
  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_lan_msg_trlr, pkt + indx, pkt_len - indx);
  indx += len;
  msg_data_count += len;

  /* ipmi_msg_len done after message length is computed */
  ipmi_msg_len = msg_data_count;
  memcpy (ipmi_msg_len_ptr,
	  &ipmi_msg_len,
	  sizeof(ipmi_msg_len));
  
  /* Auth code must be done last, some authentication like md2 and md5
   * require all fields, including checksums, to be calculated
   * beforehand
   */
  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {     
      int32_t authentication_len;
      
      memset(pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
	  
      FIID_OBJ_FIELD_LEN_BYTES_CLEANUP(authentication_len,
				       obj_lan_session_hdr,
				       "authentication_code");
      
      if (authentication_len)
	{
	  FIID_OBJ_GET_DATA_CLEANUP (obj_lan_session_hdr, 
				     "authentication_code",
				     pwbuf,
				     IPMI_1_5_MAX_PASSWORD_LENGTH);

          memcpy (authentication_code_field_ptr,
		  pwbuf,
		  IPMI_1_5_MAX_PASSWORD_LENGTH);
	}
      else
	{
	  if (authentication_code_data)
	    memcpy(pwbuf,
		   authentication_code_data,
		   authentication_code_data_len);
	  
	  if (authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
	    {	 
	      memcpy (authentication_code_field_ptr,
		      pwbuf,
		      IPMI_1_5_MAX_PASSWORD_LENGTH);
	    }
	  else /* IPMI_AUTHENTICATION_TYPE_MD2 || IPMI_AUTHENTICATION_TYPE_MD5 */
	    {
	      uint8_t session_id_buf[1024];
	      uint8_t session_sequence_number_buf[1024];
	      int32_t session_id_len, session_sequence_number_len;
	      
	      FIID_OBJ_GET_DATA_LEN_CLEANUP (session_id_len,
					     obj_lan_session_hdr,
					     "session_id",
					     session_id_buf,
					     1024);
	      
	      FIID_OBJ_GET_DATA_LEN_CLEANUP (session_sequence_number_len,
					     obj_lan_session_hdr,
					     "session_sequence_number",
					     session_sequence_number_buf,
					     1024);

	      if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
		{
		  md2_t ctx;
		  uint8_t digest[MD2_DIGEST_LENGTH];
		  
		  ERR_EXIT(IPMI_1_5_MAX_PASSWORD_LENGTH == MD2_DIGEST_LENGTH);
		  
		  md2_init(&ctx);
		  md2_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
		  md2_update_data(&ctx, session_id_buf, session_id_len);
		  md2_update_data(&ctx, msg_data_ptr, msg_data_count);
		  md2_update_data(&ctx, session_sequence_number_buf, session_sequence_number_len);
		  md2_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
		  md2_finish(&ctx, digest, MD2_DIGEST_LENGTH);
		  md2_init(&ctx);
		  
		  memcpy (authentication_code_field_ptr, digest, IPMI_1_5_MAX_PASSWORD_LENGTH);
		  secure_memset(digest, '\0', MD2_DIGEST_LENGTH);
		}
	      else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
		{
		  md5_t ctx;
		  uint8_t digest[MD5_DIGEST_LENGTH];
		  
		  ERR_EXIT(IPMI_1_5_MAX_PASSWORD_LENGTH == MD5_DIGEST_LENGTH);
		  
		  md5_init(&ctx);
		  md5_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
		  md5_update_data(&ctx, session_id_buf, session_id_len);
		  md5_update_data(&ctx, msg_data_ptr, msg_data_count);
		  md5_update_data(&ctx, session_sequence_number_buf, session_sequence_number_len);
		  md5_update_data(&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
		  md5_finish(&ctx, digest, MD5_DIGEST_LENGTH);
		  md5_init(&ctx);
		  
		  memcpy (authentication_code_field_ptr, digest, IPMI_1_5_MAX_PASSWORD_LENGTH);
		  secure_memset(digest, '\0', MD5_DIGEST_LENGTH);
		}
	    }
	}
    }

  rv = indx;
 cleanup:
  if (rv < 0)
    secure_memset(pkt, '\0', pkt_len);
  FIID_OBJ_DESTROY(obj_lan_msg_trlr);
  secure_memset(pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  return rv;
}

/*
  Complete IPMI LAN Response Packet
  +----------------------+
  |  Session             |
  |  RMCP                |
  |  Message             |
  |  Command             |
  |    Completion Code   |
  |    Data              |
  |  Checksum            |
  +----------------------+
  Optional Arguments : (pass NULL to ignore)
  rmcp_hdr, session, msg, cmd and checksum
*/

int8_t 
unassemble_ipmi_lan_pkt (uint8_t *pkt, 
			 uint32_t pkt_len, 
			 fiid_obj_t obj_rmcp_hdr, 
			 fiid_obj_t obj_lan_session_hdr, 
			 fiid_obj_t obj_lan_msg_hdr, 
			 fiid_obj_t obj_cmd, 
			 fiid_obj_t obj_lan_msg_trlr)
{
  uint64_t authentication_type;
  uint32_t indx;
  uint32_t obj_cmd_len;
  int32_t obj_lan_msg_trlr_len;
  int32_t len;

  ERR_EINVAL (pkt
	      && fiid_obj_valid(obj_rmcp_hdr)
	      && fiid_obj_valid(obj_lan_session_hdr) 
	      && fiid_obj_valid(obj_lan_msg_hdr) 
	      && fiid_obj_valid(obj_cmd)
	      && fiid_obj_valid(obj_lan_msg_trlr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_session_hdr, tmpl_lan_session_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  indx = 0;
  FIID_OBJ_CLEAR(obj_rmcp_hdr);
  FIID_OBJ_SET_ALL_LEN(len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += len;

  if (pkt_len <= indx)
    return 0;

  FIID_OBJ_CLEAR (obj_lan_session_hdr);
  FIID_OBJ_SET_BLOCK_LEN (len,
			  obj_lan_session_hdr,
			  "authentication_type",
			  "session_id",
			  pkt + indx,
			  pkt_len - indx);
  indx += len;

  FIID_OBJ_GET (obj_lan_session_hdr, 
		"authentication_type", 
		&authentication_type);

  ERR_EINVAL (IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type));

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      FIID_OBJ_SET_DATA_LEN (len,
			     obj_lan_session_hdr,
			     "authentication_code",
			     pkt + indx,
			     pkt_len - indx);
      indx += len;

      if (pkt_len <= indx)
        return 0;
    }

  FIID_OBJ_SET_DATA_LEN(len,
			obj_lan_session_hdr,
			"ipmi_msg_len",
			pkt + indx,
			pkt_len - indx);
  indx += len;
  
  if (pkt_len <= indx)
    return 0;
  
  FIID_OBJ_CLEAR(obj_lan_msg_hdr);
  FIID_OBJ_SET_ALL_LEN(len, obj_lan_msg_hdr, pkt + indx, pkt_len - indx);
  indx += len;
  
  if (pkt_len <= indx)
    return 0;
  
  FIID_TEMPLATE_LEN_BYTES (obj_lan_msg_trlr_len, tmpl_lan_msg_trlr);
  
  if ((pkt_len - indx) >= obj_lan_msg_trlr_len)
    obj_cmd_len = (pkt_len - indx) - obj_lan_msg_trlr_len;
  else if ((pkt_len - indx) < obj_lan_msg_trlr_len)
    obj_cmd_len = 0;
  
  if (obj_cmd_len)
    {
      FIID_OBJ_CLEAR(obj_cmd);
      FIID_OBJ_SET_ALL_LEN(len, obj_cmd, pkt + indx, obj_cmd_len);
      indx += len;
      
      if (pkt_len <= indx)
	return 0;
    }
  
  FIID_OBJ_CLEAR(obj_lan_msg_trlr);
  FIID_OBJ_SET_ALL_LEN(len, obj_lan_msg_trlr, pkt + indx, pkt_len - indx);
  indx += len;
  
  return 0;
}

ssize_t 
ipmi_lan_sendto (int s, 
		 const void *buf, 
		 size_t len, 
		 int flags, 
		 const struct sockaddr *to, 
		 unsigned int tolen)
{
  void *_buf;
  ssize_t bytes_sent;
  size_t _len;
  size_t pad_len = 0;

  ERR_EINVAL (buf && len);

  /*
    Note from Table 12-8, RMCP Packet for IPMI via Ethernet footnote
    Some LAN adapter chips may have a problem where packets of overall
    lengths 56, 84, 112, 128, or 156 are not handled correctly. The
    PAD byte is added as necessary to avoid these overall
    lengths. Remote console software must use the PAD byte when
    formatting packets to any 10/100 Ethernet device that accepts RMCP
    packets. -- Anand Babu
  */
  _len = len;
  if (_len == 56  ||
      _len == 84  ||
      _len == 112 ||
      _len == 128 ||
      _len == 156)
    pad_len += IPMI_LAN_PKT_PAD_SIZE;

  _len += pad_len;
  _buf = alloca (_len);         
  memset (_buf, 0, _len);
  memcpy (_buf, buf, len);
  
  ERR (!((bytes_sent = sendto (s, _buf, _len, flags, to, tolen)) < 0));

  return (bytes_sent - pad_len);
}

ssize_t 
ipmi_lan_recvfrom (int s, 
		   void *buf, 
		   size_t len, 
		   int flags, 
		   struct sockaddr *from, 
		   unsigned int *fromlen)
{
  ssize_t bytes_recvd = 0;
  void *recv_buf;
  size_t recv_buf_len;

  ERR_EINVAL (buf && len);

  if (len < 1024)
    recv_buf_len = 1024;
  else
    recv_buf_len = len;
  
  recv_buf = alloca (recv_buf_len);
  
  ERR (!((bytes_recvd = recvfrom (s, recv_buf, recv_buf_len, flags, from, fromlen)) < 0));
  
  memcpy (buf, recv_buf, bytes_recvd);
  return (bytes_recvd);
}

