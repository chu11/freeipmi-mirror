/* 
   ipmi-lan.c - IPMI LAN 

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
#include <assert.h>
#include <errno.h>

#include "freeipmi/ipmi-lan.h"
#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-ipmb-interface.h"
#include "freeipmi/ipmi-netfn-spec.h"
#include "freeipmi/ipmi-utils.h"
#include "freeipmi/rmcp.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
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
  if (!IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type)
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

  assert(IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type) && fiid_obj_valid(obj_cmd));

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

  if (!IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type))
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

  if (!IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type))
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
