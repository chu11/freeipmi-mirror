/* 
   ipmi-lan-interface.c - IPMI LAN Interface

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
#include <sys/types.h>
#include <sys/socket.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/ipmi-lan-interface.h"
#include "freeipmi/ipmi-ipmb-interface.h"
#include "freeipmi/ipmi-messaging-support-cmds.h"
#include "freeipmi/ipmi-netfn-spec.h"
#include "freeipmi/ipmi-utils.h"
#include "freeipmi/rmcp.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

#include "md2.h"
#include "md5.h"

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
fill_lan_msg_hdr (uint8_t net_fn, 
		  uint8_t rs_lun, 
		  uint8_t rq_seq, 
		  fiid_obj_t obj_msg)
{
  uint8_t checksum_buf[1024];
  int32_t checksum_len;
  int8_t checksum;

  if (!IPMI_NET_FN_VALID(net_fn)
      || !IPMI_BMC_LUN_VALID(rs_lun)
      || (rq_seq > IPMI_LAN_SEQUENCE_NUMBER_MAX)
      || !fiid_obj_valid(obj_msg))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_msg, tmpl_lan_msg_hdr_rq);

  FIID_OBJ_SET (obj_msg, (uint8_t *)"rs_addr", IPMI_SLAVE_ADDR_BMC);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"net_fn", net_fn);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rs_lun", rs_lun);
  
  FIID_OBJ_GET_BLOCK_LEN (checksum_len,
			  obj_msg, 
			  (uint8_t *)"rs_addr", 
			  (uint8_t *)"net_fn", 
			  checksum_buf, 
			  1024);

  checksum = ipmi_checksum(checksum_buf, checksum_len);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"checksum1", checksum);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rq_addr", IPMI_SLAVE_ADDR_SWID);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rq_lun", IPMI_BMC_IPMB_LUN_BMC);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rq_seq", rq_seq);

  return (0);
}

int8_t
fill_lan_session_hdr  (uint8_t authentication_type, uint32_t inbound_sequence_number, uint32_t session_id, uint8_t *authentication_code_data, uint32_t authentication_code_data_len, fiid_obj_t obj_hdr)
{
  if (!IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
      || (authentication_code_data && authentication_code_data_len > IPMI_MAX_AUTHENTICATION_CODE_LENGTH)
      || !fiid_obj_valid(obj_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_hdr, tmpl_lan_session_hdr);

  FIID_OBJ_SET (obj_hdr, (uint8_t *)"authentication_type", authentication_type);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"session_sequence_number", inbound_sequence_number);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"session_id", session_id);
 
  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  FIID_OBJ_CLEAR_FIELD (obj_hdr, (uint8_t *)"authentication_code");
  
  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE 
      && authentication_code_data)
    {
      char buf[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];
	  
      memset(buf, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
      memcpy(buf, authentication_code_data, authentication_code_data_len);
      
     FIID_OBJ_SET_DATA (obj_hdr,
			(uint8_t *)"authentication_code",
			(uint8_t *)buf,
			IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
    }

  return (0);
}

static int32_t
_ipmi_lan_pkt_rq_min_size(uint8_t authentication_type, fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;

  assert(IPMI_AUTHENTICATION_TYPE_VALID(authentication_type) && fiid_obj_valid(obj_cmd));

  FIID_TEMPLATE_LEN_BYTES (len, tmpl_rmcp_hdr);
  msg_len += len;
  FIID_TEMPLATE_LEN_BYTES (len, tmpl_lan_msg_hdr_rq);
  msg_len += len;
  FIID_TEMPLATE_LEN_BYTES (len, tmpl_lan_msg_trlr);
  msg_len += len;
  FIID_TEMPLATE_BLOCK_LEN_BYTES (len,
				 tmpl_lan_session_hdr,
				 (uint8_t *)"authentication_type",
				 (uint8_t *)"session_id");
  msg_len += len;
  
  if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
      || authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
      || authentication_type == IPMI_AUTHENTICATION_TYPE_OEM_PROP) 
    msg_len += IPMI_MAX_AUTHENTICATION_CODE_LENGTH;
  
  FIID_TEMPLATE_FIELD_LEN_BYTES (len,
				 tmpl_lan_session_hdr,
				 (uint8_t *)"ipmi_msg_len");
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
  int8_t checksum;
  int32_t rv = -1;

  if (!fiid_obj_valid(obj_rmcp_hdr)
      || !fiid_obj_valid(obj_lan_session_hdr) 
      || !fiid_obj_valid(obj_lan_msg_hdr) 
      || !fiid_obj_valid(obj_cmd) 
      || (authentication_code_data && authentication_code_data_len > IPMI_MAX_AUTHENTICATION_CODE_LENGTH)
      || !pkt)
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_session_hdr, tmpl_lan_session_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq);
  FIID_OBJ_PACKET_VALID(obj_rmcp_hdr);

  /* 
   * ipmi_msg_len is calculted in this function, so we can't use
   * FIID_OBJ_PACKET_VALID() b/c ipmi_msg_len is probably not set yet.
   */

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, (uint8_t *)"authentication_type");

  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_session_hdr, (uint8_t *)"authentication_type");
  
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, (uint8_t *)"session_sequence_number");

  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_session_hdr, (uint8_t *)"session_sequence_number");
  
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, (uint8_t *)"session_id");

  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_lan_session_hdr, (uint8_t *)"session_id");
  
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_PACKET_VALID(obj_lan_msg_hdr);
  FIID_OBJ_PACKET_VALID(obj_cmd);

  FIID_OBJ_GET (obj_lan_session_hdr, (uint8_t *)"authentication_type", &authentication_type);

  if (!IPMI_AUTHENTICATION_TYPE_VALID(authentication_type))
    {
      errno = EINVAL;
      return -1;
    }
  
  required_len = _ipmi_lan_pkt_rq_min_size((uint8_t)authentication_type, obj_cmd);
  if (pkt_len < required_len) 
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset (pkt, 0, pkt_len);

  indx = 0;

  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += len;

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len, 
				 obj_lan_session_hdr,
				 (uint8_t *)"authentication_type",
				 (uint8_t *)"session_id",
				 pkt + indx,
				 pkt_len - indx);
  indx += len;

  /* authentication_code generated last.  Save pointers for later calculation */
  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      authentication_code_field_ptr = (pkt + indx); 
      indx += IPMI_MAX_AUTHENTICATION_CODE_LENGTH;
    }
  
  ipmi_msg_len_ptr = (pkt + indx);
  FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP(len,
					tmpl_lan_session_hdr,
					(uint8_t *)"ipmi_msg_len");
  ERR_CLEANUP (len == 1);
  indx += len;

  msg_data_ptr = (pkt + indx);

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
				 obj_lan_msg_hdr,
				 (uint8_t *)"rs_addr",
				 (uint8_t *)"checksum1",
				 pkt + indx,
				 pkt_len - indx);
  indx += len;
  msg_data_count += len;

  checksum_data_ptr = (pkt + indx);

  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
				 obj_lan_msg_hdr,
				 (uint8_t *)"rq_addr",
				 (uint8_t *)"rq_seq",
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
  
  FIID_OBJ_SET_ALL_CLEANUP (obj_lan_msg_trlr, (uint8_t *)&checksum, sizeof(checksum));
  
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
      uint8_t pwbuf[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];
      int32_t authentication_len;
      
      memset(pwbuf, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	  
      FIID_OBJ_FIELD_LEN_BYTES_CLEANUP(authentication_len,
				       obj_lan_session_hdr,
				       (uint8_t *)"authentication_code");
      
      if (authentication_len)
	{
	  FIID_OBJ_GET_DATA_CLEANUP (obj_lan_session_hdr, 
				     (uint8_t *)"authentication_code",
				     pwbuf,
				     IPMI_MAX_AUTHENTICATION_CODE_LENGTH);

          memcpy (authentication_code_field_ptr,
		  pwbuf,
		  IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
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
		      IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
	    }
	  else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
		   || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
	    {
	      uint8_t session_id_buf[1024];
	      uint8_t session_sequence_number_buf[1024];
	      int32_t session_id_len, session_sequence_number_len;
	      
	      FIID_OBJ_GET_DATA_LEN_CLEANUP (session_id_len,
					     obj_lan_session_hdr,
					     (uint8_t *)"session_id",
					     session_id_buf,
					     1024);
	      if (!session_id_len)
		{
		  errno = EINVAL;
		  goto cleanup;
		}
	      
	      FIID_OBJ_GET_DATA_LEN_CLEANUP (session_sequence_number_len,
					     obj_lan_session_hdr,
					     (uint8_t *)"session_sequence_number",
					     session_sequence_number_buf,
					     1024);
	      if (!session_sequence_number_len)
		{
		  errno = EINVAL;
		  goto cleanup;
		}

	      if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
		{
		  md2_t ctx;
		  uint8_t digest[MD2_DIGEST_LEN];
		  
		  ERR_EXIT(IPMI_MAX_AUTHENTICATION_CODE_LENGTH == MD2_DIGEST_LEN);
		  
		  md2_init(&ctx);
		  md2_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
		  md2_update_data(&ctx, session_id_buf, session_id_len);
		  md2_update_data(&ctx, msg_data_ptr, msg_data_count);
		  md2_update_data(&ctx, session_sequence_number_buf, session_sequence_number_len);
		  md2_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
		  md2_finish(&ctx, digest, MD2_DIGEST_LEN);
		  
		  memcpy (authentication_code_field_ptr, digest, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
		}
	      else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
		{
		  md5_t ctx;
		  uint8_t digest[MD5_DIGEST_LEN];
		  
		  ERR_EXIT(IPMI_MAX_AUTHENTICATION_CODE_LENGTH == MD5_DIGEST_LEN);
		  
		  md5_init(&ctx);
		  md5_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
		  md5_update_data(&ctx, session_id_buf, session_id_len);
		  md5_update_data(&ctx, msg_data_ptr, msg_data_count);
		  md5_update_data(&ctx, session_sequence_number_buf, session_sequence_number_len);
		  md5_update_data(&ctx, pwbuf, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
		  md5_finish(&ctx, digest, MD5_DIGEST_LEN);
		  
		  memcpy (authentication_code_field_ptr, digest, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
		}
	    }
	  else
	    {
	      /* Unsupported authentication type */
	      errno = EINVAL;
	      goto cleanup;
	    }
	}
    }

  rv = indx;
 cleanup:
  if (rv < 0)
    memset(pkt, '\0', pkt_len);
  FIID_OBJ_DESTROY_NO_RETURN(obj_lan_msg_trlr);
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
  uint32_t obj_cmd_len, obj_lan_msg_trlr_len;
  int32_t len;

  if (!pkt
      || !fiid_obj_valid(obj_rmcp_hdr)
      || !fiid_obj_valid(obj_lan_session_hdr) 
      || !fiid_obj_valid(obj_lan_msg_hdr) 
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_lan_msg_trlr))
    {
      errno = EINVAL;
      return -1;
    }

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
			  (uint8_t *)"authentication_type",
			  (uint8_t *)"session_id",
			  pkt + indx,
			  pkt_len - indx);
  indx += len;

  FIID_OBJ_GET (obj_lan_session_hdr, 
		(uint8_t *)"authentication_type", 
		&authentication_type);

  if (!IPMI_AUTHENTICATION_TYPE_VALID(authentication_type))
    {
      errno = EINVAL;
      return -1;
    }

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      FIID_OBJ_SET_DATA_LEN (len,
			     obj_lan_session_hdr,
			     (uint8_t *)"authentication_code",
			     pkt + indx,
			     pkt_len - indx);
      indx += len;

      if (pkt_len <= indx)
        return 0;
    }

  FIID_OBJ_SET_DATA_LEN(len,
			obj_lan_session_hdr,
			(uint8_t *)"ipmi_msg_len",
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
ipmi_lan_sendto (int sockfd, 
		 const void *pkt, 
		 size_t pkt_len, 
		 int flags, 
		 const struct sockaddr *to, 
		 unsigned int tolen)
{
  void *_pkt;
  ssize_t bytes_sent;
  size_t _pkt_len;
  size_t pad_len = 0;

  if (pkt == NULL || pkt_len < 0)
    {
      errno = EINVAL;
      return -1;
    }

  /*
    Note from Table 12-8, RMCP Packet for IPMI via Ethernet footnote
    Some LAN adapter chips may have a problem where packets of overall
    lengths 56, 84, 112, 128, or 156 are not handled correctly. The
    PAD byte is added as necessary to avoid these overall
    lengths. Remote console software must use the PAD byte when
    formatting packets to any 10/100 Ethernet device that accepts RMCP
    packets. -- Anand Babu
  */
  _pkt_len = pkt_len;
  if (_pkt_len == 56  ||
      _pkt_len == 84  ||
      _pkt_len == 112 ||
      _pkt_len == 128 ||
      _pkt_len == 156)
    {
      pad_len += IPMI_LAN_PKT_PAD_SIZE;
    }

  _pkt_len += pad_len;
  _pkt = alloca (_pkt_len);         
  memset (_pkt, 0, _pkt_len);
  memcpy (_pkt, pkt, pkt_len);
  
  bytes_sent = sendto (sockfd, _pkt, _pkt_len, flags, to, tolen);

  if (bytes_sent == -1)
    return -1;
  else
    return (bytes_sent - pad_len);
}

ssize_t 
ipmi_lan_recvfrom (int sockfd, 
		   void *pkt, 
		   size_t pkt_len, 
		   int flags, 
		   struct sockaddr *from, 
		   unsigned int *fromlen)
{
  ssize_t bytes_recvd = 0;
  void *recv_buf;
  size_t recv_buf_len;
  size_t pad_len = 0;

  if (pkt == NULL || pkt_len < 0)
    {
      errno = EINVAL;
      return -1;
    }

  if (pkt_len < 1024)
    recv_buf_len = 1024;
  else
    recv_buf_len = pkt_len;
  
  /* See comment in ipmi_lan_sendto */
  /* WILL LET THIS CHECK GO SOON --ab@gnu.org.in */
  if (recv_buf_len == 56  ||
      recv_buf_len == 84  ||
      recv_buf_len == 112 ||
      recv_buf_len == 128 ||
      recv_buf_len == 156)
    {
      pad_len = IPMI_LAN_PKT_PAD_SIZE;
    }

  recv_buf_len += pad_len;
  recv_buf = alloca (recv_buf_len);
  
  bytes_recvd = recvfrom (sockfd, recv_buf, recv_buf_len, flags, from, fromlen);
  if (bytes_recvd == -1)
    {
      /*  if (recv_buf) free (recv_buf); */
      return -1;
    }
  
  recv_buf_len = pad_len ? (bytes_recvd - pad_len) : bytes_recvd;
  memcpy (pkt, recv_buf, recv_buf_len);
  /* if (recv_buf) free (recv_buf); */
  return (recv_buf_len);
}

int8_t 
ipmi_lan_check_session_sequence_number (fiid_obj_t obj_lan_session_hdr, uint32_t session_sequence_number)
{
  uint64_t session_sequence_number_recv;
  int32_t len;

  if (!fiid_obj_valid(obj_lan_session_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LOOKUP (obj_lan_session_hdr, (uint8_t *)"session_sequence_number");

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, (uint8_t *)"session_sequence_number");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_lan_session_hdr, (uint8_t *)"session_sequence_number", &session_sequence_number_recv);

  return ((((uint32_t)session_sequence_number_recv) == session_sequence_number) ? 1 : 0);
}

int8_t 
ipmi_lan_check_session_id (fiid_obj_t obj_lan_session_hdr, uint32_t session_id)
{
  uint64_t session_id_recv;
  int32_t len;

  if (!fiid_obj_valid(obj_lan_session_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LOOKUP (obj_lan_session_hdr, (uint8_t *)"session_id");

  FIID_OBJ_FIELD_LEN (len, obj_lan_session_hdr, (uint8_t *)"session_id");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_lan_session_hdr, (uint8_t *)"session_id", &session_id_recv);

  return ((((uint32_t)session_id_recv) == session_id) ? 1 : 0);
}

int8_t 
ipmi_lan_check_session_authcode (uint8_t *pkt, uint64_t pkt_len, uint8_t authentication_type, uint8_t *authentication_code_data, uint32_t authentication_code_data_len)
{
  uint8_t authentication_type_recv;
  int32_t rmcp_hdr_len, authentication_type_index, authentication_code_index;
  uint32_t authentication_type_offset, authentication_code_offset;
  uint8_t authentication_code_buf[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];

  if (!pkt 
      || !IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
      || (authentication_code_data && authentication_code_data_len > IPMI_MAX_AUTHENTICATION_CODE_LENGTH))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_TEMPLATE_LEN_BYTES (rmcp_hdr_len, tmpl_rmcp_hdr);
  FIID_TEMPLATE_FIELD_START_BYTES (authentication_type_index, 
				   tmpl_lan_session_hdr, 
				   (uint8_t *)"authentication_type");
  authentication_type_offset = rmcp_hdr_len + authentication_type_index;

  if (pkt_len < authentication_type_offset)
    return 0;

  authentication_type_recv = pkt[authentication_type_offset];

  /* authcode check fails if authentication types do not match */
  if (authentication_type != authentication_type_recv)
    return 0;

  /* Automatically return 1 if auth type is none */
  if (authentication_type_recv == IPMI_AUTHENTICATION_TYPE_NONE)
    return 1;

  FIID_TEMPLATE_FIELD_START_BYTES (authentication_code_index, 
				   tmpl_lan_session_hdr, 
				   (uint8_t *)"authentication_code");
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
				       (uint8_t *)"session_id");
      FIID_TEMPLATE_FIELD_START_BYTES (session_sequence_number_index, 
				       tmpl_lan_session_hdr, 
				       (uint8_t *)"session_sequence_number");
      ERR(!((session_id_len = fiid_template_field_len_bytes (tmpl_lan_session_hdr, (uint8_t *)"session_id")) < 0));
      FIID_TEMPLATE_FIELD_LEN_BYTES (session_sequence_number_len, 
				     tmpl_lan_session_hdr, 
				     (uint8_t *)"session_sequence_number");
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
	  
	  ERR_EXIT(IPMI_MAX_AUTHENTICATION_CODE_LENGTH == MD2_DIGEST_LEN);
	  
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
	  
	  ERR_EXIT(IPMI_MAX_AUTHENTICATION_CODE_LENGTH == MD5_DIGEST_LEN);
	  
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

  if (!(obj_lan_msg_hdr && IPMI_NET_FN_VALID(net_fn)))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LOOKUP (obj_lan_msg_hdr, (uint8_t *)"net_fn");

  FIID_OBJ_FIELD_LEN (len, obj_lan_msg_hdr, (uint8_t *)"net_fn");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_lan_msg_hdr, (uint8_t *)"net_fn", &net_fn_recv);

  return ((((uint8_t)net_fn_recv) == net_fn) ? 1 : 0);
}

int8_t 
ipmi_lan_check_rq_seq (fiid_obj_t obj_lan_msg_hdr, uint8_t rq_seq)
{
  uint64_t rq_seq_recv;
  int32_t len;

  if (!fiid_obj_valid(obj_lan_msg_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LOOKUP (obj_lan_msg_hdr, (uint8_t *)"rq_seq");

  FIID_OBJ_FIELD_LEN (len, obj_lan_msg_hdr, (uint8_t *)"rq_seq");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_lan_msg_hdr, (uint8_t *)"rq_seq", &rq_seq_recv);

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
  int8_t checksum1_recv, checksum1_calc, checksum2_recv, checksum2_calc;

  if (pkt == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_TEMPLATE_LEN_BYTES (rmcp_hdr_len, tmpl_rmcp_hdr);

  FIID_TEMPLATE_FIELD_START_BYTES (authentication_type_start_bytes, 
				   tmpl_lan_session_hdr, 
				   (uint8_t *)"authentication_type");
  
  authentication_type_offset = rmcp_hdr_len + authentication_type_start_bytes;
  authentication_type = pkt[authentication_type_offset];

  FIID_TEMPLATE_BLOCK_LEN_BYTES (msg_hdr_len1,
				 tmpl_lan_session_hdr,
				 (uint8_t *)"authentication_type",
				 (uint8_t *)"session_id");

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    authentication_code_len = IPMI_MAX_AUTHENTICATION_CODE_LENGTH;
  else
    authentication_code_len = 0;

  FIID_TEMPLATE_FIELD_LEN_BYTES(msg_hdr_len2,
				tmpl_lan_session_hdr,
				(uint8_t *)"ipmi_msg_len");

  checksum1_block_index = rmcp_hdr_len + msg_hdr_len1 + authentication_code_len + msg_hdr_len2;

  FIID_TEMPLATE_BLOCK_LEN_BYTES(checksum1_block_len,
				tmpl_lan_msg_hdr_rs,
				(uint8_t *)"rq_addr",
				(uint8_t *)"net_fn");

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
