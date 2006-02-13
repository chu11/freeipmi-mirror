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

#include "freeipmi.h"

/* IPMI LAN Message Request Header */
fiid_template_t tmpl_lan_msg_hdr_rq =
  {
    {8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "chksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
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
    {8, "chksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* IPMI LAN Message Trailer */
fiid_template_t tmpl_lan_msg_trlr = 
  {
    {8, "chksum2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t 
fill_lan_msg_hdr (uint8_t net_fn, 
		  uint8_t rs_lun, 
		  uint8_t rq_seq, 
		  fiid_obj_t obj_msg)
{
  uint8_t chksum_buf[1024];
  int32_t chksum_len;
  ipmi_chksum_t chksum;
  int8_t rv;

  if (!IPMI_NET_FN_VALID(net_fn)
      || !IPMI_BMC_LUN_VALID(rs_lun)
      || (rq_seq > IPMI_LAN_SEQ_NUM_MAX)
      || !fiid_obj_valid(obj_msg))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_msg, tmpl_lan_msg_hdr_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_msg, (uint8_t *)"rs_addr", IPMI_SLAVE_ADDR_BMC);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"net_fn", net_fn);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rs_lun", rs_lun);
  
  if ((chksum_len = fiid_obj_get_block(obj_msg, 
                                       (uint8_t *)"rs_addr", 
                                       (uint8_t *)"net_fn", 
                                       chksum_buf, 
                                       1024)) < 0)
    return (-1);

  chksum = ipmi_chksum(chksum_buf, chksum_len);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"chksum1", chksum);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rq_addr", IPMI_SLAVE_ADDR_SWID);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rq_lun", IPMI_BMC_IPMB_LUN_BMC);
  FIID_OBJ_SET (obj_msg, (uint8_t *)"rq_seq", rq_seq);

  return (0);
}

static int32_t
_ipmi_lan_pkt_min_size(uint8_t auth_type,
                       fiid_template_t tmpl_lan_msg, 
                       fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;

  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  ERR(!((len = fiid_template_len_bytes (tmpl_hdr_rmcp)) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_len_bytes (tmpl_lan_msg)) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_block_len_bytes (tmpl_lan_session_hdr,
					      (uint8_t *)"auth_type",
					      (uint8_t *)"session_id")) < 0));
  msg_len += len;
  ERR(!((len = fiid_template_field_len_bytes (tmpl_lan_session_hdr,
					      (uint8_t *)"ipmi_msg_len")) < 0));
  msg_len += len;
  
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    msg_len += IPMI_SESSION_MAX_AUTH_CODE_LEN;
  
  return msg_len;
}
                       
static int32_t 
_ipmi_lan_pkt_size (uint8_t auth_type, 
		    fiid_template_t tmpl_lan_msg, 
		    fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;
  
  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  ERR(!((len = _ipmi_lan_pkt_min_size(auth_type, tmpl_lan_msg, obj_cmd)) < 0));
  msg_len += len;
  ERR(!((len = fiid_obj_len_bytes (obj_cmd)) < 0));
  msg_len += len;

  return msg_len;
}

static int32_t 
_ipmi_lan_pkt_rq_size (uint8_t auth_type, fiid_obj_t obj_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rq, obj_cmd);
}

static int32_t 
_ipmi_max_lan_pkt_size (uint8_t auth_type, 
                        fiid_template_t tmpl_lan_msg, 
                        fiid_obj_t obj_cmd)
{
  uint32_t msg_len = 0;
  int32_t len;
  fiid_field_t *tmpl = NULL;
  int32_t rv = -1;

  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && fiid_obj_valid(obj_cmd));

  if ((len = _ipmi_lan_pkt_min_size(auth_type, tmpl_lan_msg, obj_cmd)) < 0)
    goto cleanup;
  msg_len += len;
  
  if (!(tmpl = fiid_obj_template(obj_cmd)))
    goto cleanup;
  if ((len = fiid_template_len_bytes(tmpl)) < 0)
    goto cleanup;
  msg_len += len;
  
  rv = msg_len;
 cleanup:
  if (tmpl)
    fiid_template_free(tmpl);
  return (rv);
}

int32_t 
_ipmi_max_lan_pkt_rs_size (uint8_t auth_type, fiid_obj_t obj_cmd)
{
  return _ipmi_max_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rs, obj_cmd);
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
assemble_ipmi_lan_pkt (fiid_obj_t obj_hdr_rmcp, 
		       fiid_obj_t obj_hdr_session, 
		       fiid_obj_t obj_msg_hdr, 
		       fiid_obj_t obj_cmd, 
		       uint8_t *auth_code_data,
		       uint32_t auth_code_data_len,
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  uint64_t auth_type;
  uint32_t indx, required_len;
  uint8_t *auth_code_field_ptr = NULL;
  uint8_t *chksum_data_ptr = NULL;
  uint8_t *msg_data_ptr = NULL;
  uint8_t *ipmi_msg_len_ptr = NULL;
  uint32_t msg_data_count = 0;
  uint32_t chksum_data_count = 0;
  int32_t len, req_len;
  uint8_t ipmi_msg_len;
  fiid_obj_t obj_msg_trlr = NULL;
  ipmi_chksum_t chksum;
  int8_t rv;

  if (!fiid_obj_valid(obj_hdr_rmcp)
      || !fiid_obj_valid(obj_hdr_session) 
      || !fiid_obj_valid(obj_msg_hdr) 
      || !fiid_obj_valid(obj_cmd) 
      || (auth_code_data && auth_code_data_len > IPMI_SESSION_MAX_AUTH_CODE_LEN)
      || !pkt)
    {
      errno = EINVAL;
      return -1;
    }
  
  if ((rv = fiid_obj_template_compare(obj_hdr_rmcp, tmpl_hdr_rmcp)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_hdr_session, tmpl_lan_session_hdr)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_msg_hdr, tmpl_lan_msg_hdr_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_hdr_rmcp)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  /* 
   * ipmi_msg_len is calculted in this function, so we can't use
   * fiid_obj_packet_valid() b/c ipmi_msg_len is probably not set yet.
   */

  if ((len = fiid_obj_field_len(obj_hdr_session, (uint8_t *)"auth_type")) < 0)
    return (-1);

  if ((req_len = fiid_template_field_len(tmpl_lan_session_hdr, (uint8_t *)"auth_type")) < 0)
    return (-1);
  
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_field_len(obj_hdr_session, (uint8_t *)"session_seq_num")) < 0)
    return (-1);

  if ((req_len = fiid_template_field_len(tmpl_lan_session_hdr, (uint8_t *)"session_seq_num")) < 0)
    return (-1);
  
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_field_len(obj_hdr_session, (uint8_t *)"session_id")) < 0)
    return (-1);

  if ((req_len = fiid_template_field_len(tmpl_lan_session_hdr, (uint8_t *)"session_id")) < 0)
    return (-1);
  
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_msg_hdr)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_cmd)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_get(obj_hdr_session, (uint8_t *)"auth_type", &auth_type) < 0)
    return -1;

  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type))
    {
      errno = EINVAL;
      return -1;
    }
  
  required_len = _ipmi_lan_pkt_rq_size((uint8_t)auth_type, obj_cmd);
  if (pkt_len < required_len) 
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset (pkt, 0, pkt_len);

  indx = 0;

  if ((len = fiid_obj_get_all(obj_hdr_rmcp, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;

  if ((len = fiid_obj_get_block(obj_hdr_session,
				(uint8_t *)"auth_type",
				(uint8_t *)"session_id",
				pkt + indx,
				pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;

  /* auth_code generated last.  Save pointers for later calculation */
  if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
    {
      auth_code_field_ptr = (pkt + indx); 
      indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
    }
  
  ipmi_msg_len_ptr = (pkt + indx);
  if ((len = fiid_template_field_len_bytes(tmpl_lan_session_hdr,
                                           (uint8_t *)"ipmi_msg_len")) < 0)
    goto cleanup;
  if (len != 1)
    {
      errno = EINVAL;
      goto cleanup;
    }
  indx += len;

  msg_data_ptr = (pkt + indx);

  if ((len = fiid_obj_get_block(obj_msg_hdr,
				(uint8_t *)"rs_addr",
				(uint8_t *)"chksum1",
				pkt + indx,
				pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  msg_data_count += len;

  chksum_data_ptr = (pkt + indx);

  if ((len = fiid_obj_get_block(obj_msg_hdr,
				(uint8_t *)"rq_addr",
				(uint8_t *)"rq_seq",
				pkt + indx,
				pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  msg_data_count += len;
  chksum_data_count += len;

  if ((len = fiid_obj_get_all(obj_cmd,
			      pkt + indx,
			      pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  msg_data_count += len;
  chksum_data_count += len;

  if (!(obj_msg_trlr = fiid_obj_create(tmpl_lan_msg_trlr)))
    goto cleanup;

  chksum = ipmi_chksum (chksum_data_ptr, chksum_data_count);
  
  if (fiid_obj_set_all(obj_msg_trlr, 
		       (uint8_t *)&chksum, 
		       sizeof(chksum)) < 0)
    goto cleanup;
  
  if ((len = fiid_obj_get_all(obj_msg_trlr,
			      pkt + indx,
			      pkt_len - indx)) < 0)
    goto cleanup;
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
  if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
    {     
      uint8_t pwbuf[IPMI_SESSION_MAX_AUTH_CODE_LEN];
      int32_t auth_len;
      
      memset(pwbuf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);
	  
      if ((auth_len = fiid_obj_field_len_bytes(obj_hdr_session,
					       (uint8_t *)"auth_code")) < 0)
	goto cleanup;
      
      if (auth_len)
	{
	  if (fiid_obj_get_data(obj_hdr_session, 
				(uint8_t *)"auth_code",
				pwbuf,
				IPMI_SESSION_MAX_AUTH_CODE_LEN) < 0)
	    goto cleanup;

          memcpy (auth_code_field_ptr,
		  pwbuf,
		  IPMI_SESSION_MAX_AUTH_CODE_LEN);
	}
      else
	{
	  if (auth_code_data)
	    memcpy(pwbuf,
		   auth_code_data,
		   auth_code_data_len);
	  
	  if (auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
	    {	 
	      memcpy (auth_code_field_ptr,
		      pwbuf,
		      IPMI_SESSION_MAX_AUTH_CODE_LEN);
	    }
	  else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
		   || auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
	    {
	      uint8_t session_id_buf[1024];
	      uint8_t session_seq_num_buf[1024];
	      int32_t session_id_len, session_seq_num_len;
	      
	      if ((session_id_len = fiid_obj_get_data(obj_hdr_session,
						      (uint8_t *)"session_id",
						      session_id_buf,
						      1024)) < 0)
		goto cleanup;
	      
	      if (!session_id_len)
		{
		  errno = EINVAL;
		  goto cleanup;
		}
	      
	      if ((session_seq_num_len = fiid_obj_get_data(obj_hdr_session,
							   (uint8_t *)"session_seq_num",
							   session_seq_num_buf,
							   1024)) < 0)
		goto cleanup;
	      
	      if (!session_seq_num_len)
		{
		  errno = EINVAL;
		  goto cleanup;
		}

	      if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2)
		{
		  ipmi_md2_t ctx;
		  uint8_t digest[IPMI_MD2_DIGEST_LEN];
		  
		  ERR_EXIT(IPMI_SESSION_MAX_AUTH_CODE_LEN == IPMI_MD2_DIGEST_LEN);
		  
		  ipmi_md2_init(&ctx);
		  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
		  ipmi_md2_update_data(&ctx, session_id_buf, session_id_len);
		  ipmi_md2_update_data(&ctx, msg_data_ptr, msg_data_count);
		  ipmi_md2_update_data(&ctx, session_seq_num_buf, session_seq_num_len);
		  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
		  ipmi_md2_finish(&ctx, digest, IPMI_MD2_DIGEST_LEN);
		  
		  memcpy (auth_code_field_ptr, digest, IPMI_SESSION_MAX_AUTH_CODE_LEN);
		}
	      else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
		{
		  ipmi_md5_t ctx;
		  uint8_t digest[IPMI_MD5_DIGEST_LEN];
		  
		  ERR_EXIT(IPMI_SESSION_MAX_AUTH_CODE_LEN == IPMI_MD5_DIGEST_LEN);
		  
		  ipmi_md5_init(&ctx);
		  ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
		  ipmi_md5_update_data(&ctx, session_id_buf, session_id_len);
		  ipmi_md5_update_data(&ctx, msg_data_ptr, msg_data_count);
		  ipmi_md5_update_data(&ctx, session_seq_num_buf, session_seq_num_len);
		  ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
		  ipmi_md5_finish(&ctx, digest, IPMI_MD5_DIGEST_LEN);
		  
		  memcpy (auth_code_field_ptr, digest, IPMI_SESSION_MAX_AUTH_CODE_LEN);
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

  fiid_obj_destroy(obj_msg_trlr);
  return indx;
  
 cleanup:
  if (pkt)
    memset(pkt, '\0', pkt_len);
  if (obj_msg_trlr)
    fiid_obj_destroy(obj_msg_trlr);
  return -1;
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
  hdr_rmcp, session, msg, cmd and chksum
*/

int8_t 
unassemble_ipmi_lan_pkt (uint8_t *pkt, 
			 uint32_t pkt_len, 
			 fiid_obj_t obj_hdr_rmcp, 
			 fiid_obj_t obj_hdr_session, 
			 fiid_obj_t obj_msg_hdr, 
			 fiid_obj_t obj_cmd, 
			 fiid_obj_t obj_msg_trlr)
{
  uint64_t auth_type;
  uint32_t indx;
  uint32_t obj_cmd_len, obj_msg_trlr_len;
  int32_t len;
  int8_t rv;

  if (!pkt
      || !fiid_obj_valid(obj_hdr_rmcp)
      || !fiid_obj_valid(obj_hdr_session) 
      || !fiid_obj_valid(obj_msg_hdr) 
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_msg_trlr))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_hdr_rmcp, tmpl_hdr_rmcp)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_hdr_session, tmpl_lan_session_hdr)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_msg_hdr, tmpl_lan_msg_hdr_rs)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_msg_trlr, tmpl_lan_msg_trlr)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  indx = 0;
  ERR(!(fiid_obj_clear(obj_hdr_rmcp) < 0));
  ERR(!((len = fiid_obj_set_all(obj_hdr_rmcp, pkt + indx, pkt_len - indx)) < 0));
  indx += len;

  if (pkt_len <= indx)
    return 0;

  ERR(!(fiid_obj_clear(obj_hdr_session) < 0));
  ERR(!((len = fiid_obj_set_block(obj_hdr_session,
                                  (uint8_t *)"auth_type",
                                  (uint8_t *)"session_id",
                                  pkt + indx,
                                  pkt_len - indx)) < 0));
  indx += len;

  ERR(!(fiid_obj_get(obj_hdr_session, (uint8_t *)"auth_type", &auth_type) < 0));

  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type))
    {
      errno = EINVAL;
      return -1;
    }

  if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
    {
      ERR(!((len = fiid_obj_set_data(obj_hdr_session,
                                     (uint8_t *)"auth_code",
                                     pkt + indx,
				     pkt_len - indx)) < 0));
      indx += len;

      if (pkt_len <= indx)
        return 0;
    }

  if ((len = fiid_obj_set_data(obj_hdr_session,
			       (uint8_t *)"ipmi_msg_len",
			       pkt + indx,
			       pkt_len - indx)) < 0)
    return -1;
  indx += len;

  if (pkt_len <= indx)
    return 0;

  ERR(!(fiid_obj_clear(obj_msg_hdr) < 0));
  ERR(!((len = fiid_obj_set_all(obj_msg_hdr, pkt + indx, pkt_len - indx)) < 0));
  indx += len;

  if (pkt_len <= indx)
    return 0;

  ERR(!((obj_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));

  if ((pkt_len - indx) >= obj_msg_trlr_len)
    obj_cmd_len = (pkt_len - indx) - obj_msg_trlr_len;
  else if ((pkt_len - indx) < obj_msg_trlr_len)
    obj_cmd_len = 0;

  if (obj_cmd_len)
    {
      ERR(!(fiid_obj_clear(obj_cmd) < 0));
      ERR(!((len = fiid_obj_set_all(obj_cmd, pkt + indx, pkt_len - indx)) < 0));
      indx += len;
      
      if (pkt_len <= indx)
        return 0;
    }

  ERR(!(fiid_obj_clear(obj_msg_trlr) < 0));
  ERR(!((len = fiid_obj_set_all(obj_msg_trlr, pkt + indx, pkt_len - indx)) < 0));
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
ipmi_lan_cmd (uint32_t sockfd, 
	      struct sockaddr *hostaddr, 
	      size_t hostaddr_len, 
	      uint8_t auth_type, 
	      uint32_t session_seq_num, 
	      uint32_t session_id, 
	      uint8_t *auth_code_data, 
	      uint32_t auth_code_data_len, 
	      uint8_t net_fn, 
	      uint8_t lun, 
	      uint8_t rq_seq, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_obj_t obj_cmd_rs)
{
  int8_t rv;

  if (!(hostaddr 
	&& sockfd 
	&& hostaddr_len 
	&& fiid_obj_valid(obj_cmd_rq)
	&& fiid_obj_valid(obj_cmd_rs)))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_cmd_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  {
    fiid_obj_t obj_hdr_rmcp = NULL;
    fiid_obj_t obj_hdr_session = NULL;
    fiid_obj_t obj_msg_hdr = NULL;
    uint8_t *pkt;
    uint32_t pkt_len;
    int status = 0;
    
    rv = -1;

    if (!(obj_hdr_rmcp = fiid_obj_create (tmpl_hdr_rmcp)))
      goto cleanup1;
    if (!(obj_hdr_session = fiid_obj_create(tmpl_lan_session_hdr)))
      goto cleanup1;
    if (!(obj_msg_hdr = fiid_obj_create(tmpl_lan_msg_hdr_rq)))
      goto cleanup1;

    pkt_len = _ipmi_lan_pkt_rq_size(auth_type, obj_cmd_rq); 
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
   
    if (fill_hdr_rmcp_ipmi (obj_hdr_rmcp) < 0)
      goto cleanup1;
    
    if (fill_hdr_session (auth_type, 
			  session_seq_num, 
			  session_id, 
			  NULL, 
			  0,
			  obj_hdr_session) < 0)
      goto cleanup1;

    if (fill_lan_msg_hdr (net_fn, 
			  lun, 
			  rq_seq, 
			  obj_msg_hdr) < 0)
      goto cleanup1;

    if (assemble_ipmi_lan_pkt (obj_hdr_rmcp, 
			       obj_hdr_session, 
			       obj_msg_hdr, 
			       obj_cmd_rq, 
			       auth_code_data,
			       auth_code_data_len,
			       pkt, 
			       pkt_len) < 0)
      goto cleanup1;

    if ((status = ipmi_lan_sendto (sockfd, 
				   pkt,
				   pkt_len, 
				   0, 
				   hostaddr, 
				   hostaddr_len)) < 0)
      goto cleanup1;
    
    rv = 0;
  cleanup1:
    if (obj_hdr_rmcp)
      fiid_obj_destroy(obj_hdr_rmcp);
    if (obj_hdr_session)
      fiid_obj_destroy(obj_hdr_session);
    if (obj_msg_hdr)
      fiid_obj_destroy(obj_msg_hdr);
    if (rv < 0)
      return (rv);
  }

  {
    fiid_obj_t obj_hdr_rmcp = NULL;
    fiid_obj_t obj_hdr_session = NULL;
    fiid_obj_t obj_msg_hdr = NULL;
    fiid_obj_t obj_msg_trlr = NULL;
    struct sockaddr_in from;
    socklen_t fromlen;
    uint8_t *pkt;
    uint32_t _pkt_len = 1024;
    int32_t pkt_len;

    rv = -1;

    if (!(obj_hdr_rmcp = fiid_obj_create (tmpl_hdr_rmcp)))
      goto cleanup2;
    if (!(obj_hdr_session = fiid_obj_create(tmpl_lan_session_hdr)))
      goto cleanup2;
    if (!(obj_msg_hdr = fiid_obj_create(tmpl_lan_msg_hdr_rs)))
      goto cleanup2;
    if (!(obj_msg_trlr = fiid_obj_create(tmpl_lan_msg_trlr)))
      goto cleanup2;

    pkt_len = _ipmi_max_lan_pkt_rs_size (auth_type, obj_cmd_rs);
    pkt     = alloca (_pkt_len);
    memset (pkt, 0, _pkt_len);
    ERR (pkt);
    
    fromlen = sizeof(struct sockaddr_in);
    if ((pkt_len = ipmi_lan_recvfrom (sockfd, 
				      pkt, 
				      _pkt_len, 
				      0, 
				      (struct sockaddr *)&from, 
				      &fromlen)) < 0)
      goto cleanup2;

    if (ipmi_lan_check_chksum (pkt, pkt_len) != 1)
      goto cleanup2;

    if (unassemble_ipmi_lan_pkt (pkt, 
				 pkt_len, 
				 obj_hdr_rmcp,
				 obj_hdr_session, 
				 obj_msg_hdr,
				 obj_cmd_rs, 
				 obj_msg_trlr) < 0)
      goto cleanup2;

    rv = 0;
  cleanup2:
    if (obj_hdr_rmcp)
      fiid_obj_destroy(obj_hdr_rmcp);
    if (obj_hdr_session)
      fiid_obj_destroy(obj_hdr_session);
    if (obj_msg_hdr)
      fiid_obj_destroy(obj_msg_hdr);
    if (obj_msg_trlr)
      fiid_obj_destroy(obj_msg_trlr);
    if (rv < 0)
      return (rv);
  }

  return (0);
}

int8_t 
ipmi_lan_cmd2 (ipmi_device_t *dev, 
	       fiid_obj_t obj_cmd_rq, 
	       fiid_obj_t obj_cmd_rs)
{
  int8_t rv;

  if (!(dev 
        && dev->io.outofband.local_sockfd 
        && fiid_obj_valid(obj_cmd_rq)
        && fiid_obj_valid(obj_cmd_rs)))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((rv = fiid_obj_packet_valid(obj_cmd_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_rmcp) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_session) < 0)
     return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_hdr) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_trlr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_rmcp) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_session) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_hdr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_trlr) < 0) 
    return (-1);
  
  {
    uint8_t *pkt;
    int32_t pkt_len;
    int status = 0;
    
    if ((pkt_len = _ipmi_lan_pkt_rq_size (dev->io.outofband.auth_type, obj_cmd_rq)) < 0)
      return (-1);
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
    
    ERR (fill_hdr_rmcp_ipmi (dev->io.outofband.rq.obj_hdr_rmcp) != -1);
    ERR (fill_lan_msg_hdr (dev->net_fn,
                           dev->lun,
                           dev->io.outofband.rq_seq,
                           dev->io.outofband.rq.obj_msg_hdr) != -1);
    ERR (fill_hdr_session (dev->io.outofband.auth_type,
                           dev->io.outofband.session_seq_num,
                           dev->io.outofband.session_id,
                           NULL,
                           0,
                           dev->io.outofband.rq.obj_hdr_session) != -1);
    ERR (assemble_ipmi_lan_pkt (dev->io.outofband.rq.obj_hdr_rmcp,
                                dev->io.outofband.rq.obj_hdr_session,
                                dev->io.outofband.rq.obj_msg_hdr,
                                obj_cmd_rq,
                                dev->io.outofband.password,
                                IPMI_SESSION_MAX_AUTH_CODE_LEN,
                                pkt,
                                pkt_len) != -1);

#if 0
printf("DEBUGGING:\n");

	fiid_obj_dump_lan(STDERR_FILENO,
			  NULL,
			  NULL,
			  pkt,
			  pkt_len,
			  *(dev->io.outofband.rs.tmpl_msg_hdr_ptr));
#endif

    dev->io.outofband.session_seq_num++;
    IPMI_LAN_RQ_SEQ_INC (dev->io.outofband.rq_seq);
    
    status = ipmi_lan_sendto (dev->io.outofband.local_sockfd, pkt, pkt_len, 0, 
			      &(dev->io.outofband.remote_host), 
			      dev->io.outofband.remote_host_len);
    ERR (status != -1);
  }
  
  {
    struct sockaddr_in from;
    socklen_t fromlen = 0;
    
    uint8_t *pkt = NULL;
    uint32_t pkt_max_size = 1024;
    int32_t bytes_received = 0;
    int32_t max_pkt_len;
    
    if ((max_pkt_len = _ipmi_max_lan_pkt_rs_size (dev->io.outofband.auth_type, obj_cmd_rs)) < 0)
      return (-1);
    ERR (max_pkt_len <= pkt_max_size);
    
    pkt = alloca (pkt_max_size);
    ERR (pkt);
    memset (pkt, 0, pkt_max_size);
    ERR (!((bytes_received = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
                                                pkt, 
                                                pkt_max_size, 
                                                0, 
                                                (struct sockaddr *) &from, 
                                                &fromlen)) < 0));
    
    ERR (unassemble_ipmi_lan_pkt(pkt,
                                 bytes_received,
                                 dev->io.outofband.rs.obj_hdr_rmcp,
                                 dev->io.outofband.rs.obj_hdr_session,
                                 dev->io.outofband.rs.obj_msg_hdr,
                                 obj_cmd_rs,
                                 dev->io.outofband.rs.obj_msg_trlr) != -1);
    
    ERR (check_hdr_session_authcode (pkt, 
                                     bytes_received, 
                                     dev->io.outofband.auth_type,
                                     dev->io.outofband.password,
                                     IPMI_SESSION_MAX_AUTH_CODE_LEN) == 1);

  }
  
  return (0);
}

static int8_t 
ipmi_lan_cmd_raw_send (ipmi_device_t *dev, 
		       fiid_obj_t obj_cmd_rq)
{
  if (!dev || !fiid_obj_valid(obj_cmd_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_rmcp) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_hdr_session) < 0)
     return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_hdr) < 0)
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rq.obj_msg_trlr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_rmcp) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_hdr_session) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_hdr) < 0) 
    return (-1);
  if (fiid_obj_clear(dev->io.outofband.rs.obj_msg_trlr) < 0) 
    return (-1);
  
  {
    uint8_t *pkt;
    uint32_t pkt_len;
    int status = 0;
    
    if ((pkt_len = _ipmi_lan_pkt_rq_size (dev->io.outofband.auth_type, obj_cmd_rq)) < 0)
      return (-1);
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
    
    ERR (fill_hdr_rmcp_ipmi (dev->io.outofband.rq.obj_hdr_rmcp) != -1);
    ERR (fill_lan_msg_hdr (dev->net_fn,
                           dev->lun,
                           dev->io.outofband.rq_seq,
                           dev->io.outofband.rq.obj_msg_hdr) != -1);
    ERR (fill_hdr_session (dev->io.outofband.auth_type,
                           dev->io.outofband.session_seq_num,
                           dev->io.outofband.session_id,
                           NULL,
                           0,
                           dev->io.outofband.rq.obj_hdr_session) != -1);
    ERR (assemble_ipmi_lan_pkt (dev->io.outofband.rq.obj_hdr_rmcp,
                                dev->io.outofband.rq.obj_hdr_session,
                                dev->io.outofband.rq.obj_msg_hdr,
                                obj_cmd_rq,
                                dev->io.outofband.password,
                                IPMI_SESSION_MAX_AUTH_CODE_LEN,
                                pkt,
                                pkt_len) != -1);

    dev->io.outofband.session_seq_num++;
    IPMI_LAN_RQ_SEQ_INC (dev->io.outofband.rq_seq);
    
    status = ipmi_lan_sendto (dev->io.outofband.local_sockfd, pkt, pkt_len, 0, 
			      &(dev->io.outofband.remote_host), 
			      dev->io.outofband.remote_host_len);
    ERR (status != -1);
  }
  
  return (0);
}

int8_t 
ipmi_lan_cmd_raw2 (ipmi_device_t *dev, 
		   uint8_t *buf_rq, 
		   size_t buf_rq_len, 
		   uint8_t *buf_rs, 
		   size_t *buf_rs_len)
{
  size_t buf_rs_len_in;

  if (!(dev && 
	dev->io.outofband.local_sockfd && 
	buf_rq && 
	buf_rq_len >= 2 && 
	buf_rs && 
	buf_rs_len && 
	*buf_rs_len))
    {
      errno = EINVAL;
      return (-1);
    }
  
  buf_rs_len_in = *buf_rs_len;
  *buf_rs_len = 0;
  
  {
    uint64_t val = 0;
    fiid_obj_t obj_hdr;
    fiid_template_t tmpl_hdr_cmd = 
      {
	{2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	{6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	{0, "", 0}
      };
    
    if (!(obj_hdr = fiid_obj_create(tmpl_hdr_cmd)))
      return (-1);

    if (fiid_obj_set_all(obj_hdr,
                         buf_rq,
                         buf_rq_len) < 0)
      {
        fiid_obj_destroy(obj_hdr);
        return (-1);
      }

    if (fiid_obj_get (obj_hdr, (uint8_t *)"lun", &val) < 0)
      {
        fiid_obj_destroy(obj_hdr);
        return (-1);
      }
    dev->lun = val;

    if (fiid_obj_get (obj_hdr, (uint8_t *)"net_fn", &val) < 0)
      {
        fiid_obj_destroy(obj_hdr);
        return (-1);
      }
    dev->net_fn = val;

    fiid_obj_destroy(obj_hdr);
  }
  
  {
    fiid_obj_t obj_cmd_rq = NULL;
    size_t obj_cmd_rq_len = 0;
    fiid_field_t *tmpl_var_cmd_rq = NULL;
    int8_t retval = 0;

    obj_cmd_rq_len = buf_rq_len - 1;

    if (!(tmpl_var_cmd_rq = fiid_template_make ((obj_cmd_rq_len * 8), 
                                                (uint8_t *)"COMMAND_RQ_DATA",
                                                FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED)))
      return (-1);

    if (!(obj_cmd_rq = fiid_obj_create(tmpl_var_cmd_rq)))
      {
        fiid_template_free (tmpl_var_cmd_rq);
        return (-1);
      }
    
    if (fiid_obj_set_all(obj_cmd_rq,
                         buf_rq + 1,
                         buf_rq_len - 1) < 0)
      {
        fiid_obj_destroy(obj_cmd_rq);
        fiid_template_free (tmpl_var_cmd_rq);
        return (-1);
      }

    if ((retval = ipmi_lan_cmd_raw_send (dev, obj_cmd_rq)) < 0)
      {
        fiid_obj_destroy(obj_cmd_rq);
        fiid_template_free (tmpl_var_cmd_rq);
        return (-1);
      }

    fiid_obj_destroy(obj_cmd_rq);
    fiid_template_free (tmpl_var_cmd_rq);
  }
  
  {
    fiid_obj_t obj_cmd_rs = NULL;
    size_t obj_cmd_rs_len = 0; 
    fiid_field_t *tmpl_var_cmd_rs = NULL;
    
    struct sockaddr_in from;
    socklen_t fromlen = 0;
    
    uint8_t *pkt = NULL;
    uint32_t pkt_max_size = 1024;
    int32_t pkt_len;
    int32_t bytes_received;
    int32_t pkt_hdrs_size;
    int32_t rmcp_len, hdr_len, session_len, authcode_len, ipmi_msg_len, trlr_len;

    if ((rmcp_len = fiid_template_len_bytes (*(dev->io.outofband.rs.tmpl_hdr_rmcp_ptr))) < 0)
      return (-1);

    if ((hdr_len = fiid_template_len_bytes (*(dev->io.outofband.rs.tmpl_msg_hdr_ptr))) < 0)
      return (-1);

    if ((session_len = fiid_template_block_len_bytes (*(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr),
                                                      (uint8_t *)"auth_type",
                                                      (uint8_t *)"session_id")) < 0)
      return (-1);

    if (dev->io.outofband.auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
      authcode_len = IPMI_SESSION_MAX_AUTH_CODE_LEN;
    else
      authcode_len = 0;

    if ((ipmi_msg_len = fiid_template_field_len_bytes (*(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr),
                                                       (uint8_t *)"ipmi_msg_len")) < 0)
      return (-1);
                                                          
    if ((trlr_len = fiid_template_len_bytes (*(dev->io.outofband.rs.tmpl_msg_trlr_ptr))) < 0)
      return (-1);
    
    pkt_hdrs_size = rmcp_len + hdr_len + session_len + authcode_len + ipmi_msg_len + trlr_len;
    
    pkt_len = 1024 - pkt_hdrs_size - 1;
    ERR (pkt_len <= pkt_max_size);
    
    pkt = alloca (pkt_max_size);
    ERR (pkt);
    memset (pkt, 0, pkt_max_size);
    if ((bytes_received = ipmi_lan_recvfrom (dev->io.outofband.local_sockfd, 
                                             pkt, 
                                             pkt_max_size, 
                                             0, 
                                             (struct sockaddr *) &from, 
                                             &fromlen)) < 0)
      return (-1);

    ERR (bytes_received >= pkt_hdrs_size);
    
    obj_cmd_rs_len = (bytes_received - pkt_hdrs_size);

    if ((buf_rs_len_in - 1) < obj_cmd_rs_len)
      {
        errno = EINVAL;
        return (-1);
      }

    if (!(tmpl_var_cmd_rs = fiid_template_make ((obj_cmd_rs_len * 8), 
                                                (uint8_t *)"COMMAND_RS_DATA",
                                                FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED)))
      return (-1);

    if (!(obj_cmd_rs = fiid_obj_create(tmpl_var_cmd_rs)))
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
        return (-1);
      }

    if (unassemble_ipmi_lan_pkt(pkt,
                                bytes_received,
                                dev->io.outofband.rs.obj_hdr_rmcp,
                                dev->io.outofband.rs.obj_hdr_session,
                                dev->io.outofband.rs.obj_msg_hdr,
                                obj_cmd_rs,
                                dev->io.outofband.rs.obj_msg_trlr) < 0)
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
	return (-1);
      }

    if (check_hdr_session_authcode (pkt, 
                                    bytes_received, 
                                    dev->io.outofband.auth_type,
                                    dev->io.outofband.password,
                                    IPMI_SESSION_MAX_AUTH_CODE_LEN) != 1)
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
	return (-1);
      }

    if (fiid_obj_get_all(obj_cmd_rs,
                         buf_rs + 1,
                         buf_rs_len_in - 1) < 0)
      {
	fiid_template_free (tmpl_var_cmd_rs);
        fiid_obj_destroy(obj_cmd_rs);
	return (-1);
      }
    
    fiid_template_free (tmpl_var_cmd_rs);
    fiid_obj_destroy(obj_cmd_rs);
    
    {
      uint64_t val = 0;
      fiid_obj_t obj_hdr;
      fiid_template_t tmpl_hdr_cmd = 
	{
	  {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {0, "", 0}
	};
      
      if (!(obj_hdr = fiid_obj_create(tmpl_hdr_cmd)))
        return (-1);

      if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr, 
                        (uint8_t *)"rq_lun", &val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      if (fiid_obj_set (obj_hdr,
                        (uint8_t *)"lun",
                        val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      if (fiid_obj_get (dev->io.outofband.rs.obj_msg_hdr, 
                        (uint8_t *)"net_fn", &val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      if (fiid_obj_set(obj_hdr,
                       (uint8_t *)"net_fn", 
                       val) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }
        
      if (fiid_obj_get_all(obj_hdr,
                           buf_rs,
                           1) < 0)
        {
          fiid_obj_destroy(obj_hdr);
          return (-1);
        }

      fiid_obj_destroy(obj_hdr);
      *buf_rs_len = obj_cmd_rs_len + 1;
    }
  }
  
  return (0);
}

int8_t 
ipmi_lan_check_net_fn (fiid_obj_t obj_msg_hdr, uint8_t net_fn)
{
  uint64_t net_fn_recv;
  int32_t len;
  int8_t rv;

  if (!(obj_msg_hdr && IPMI_NET_FN_VALID(net_fn)))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_field_lookup (obj_msg_hdr, (uint8_t *)"net_fn")) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_field_len (obj_msg_hdr, (uint8_t *)"net_fn")) < 0)
    return (-1);

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, (uint8_t *)"net_fn", &net_fn_recv);

  return ((((uint8_t)net_fn_recv) == net_fn) ? 1 : 0);
}

int8_t 
ipmi_lan_check_rq_seq (fiid_obj_t obj_msg_hdr, uint8_t rq_seq)
{
  uint64_t rq_seq_recv;
  int32_t len;
  int8_t rv;

  if (!fiid_obj_valid(obj_msg_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_field_lookup (obj_msg_hdr, (uint8_t *)"rq_seq")) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_field_len (obj_msg_hdr, (uint8_t *)"rq_seq")) < 0)
    return (-1);

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, (uint8_t *)"rq_seq", &rq_seq_recv);

  return ((((uint8_t)rq_seq_recv) == rq_seq) ? 1 : 0);
}

int8_t 
ipmi_lan_check_chksum (uint8_t *pkt, uint64_t pkt_len)
{
  uint8_t auth_type;
  uint32_t auth_type_offset;
  int32_t rmcp_hdr_len, msg_hdr_len1, msg_hdr_len2, auth_code_len;
  int32_t auth_type_start_bytes;
  int32_t chksum1_block_index, chksum1_block_len, 
    chksum2_block_index, chksum2_block_len;
  ipmi_chksum_t chksum1_recv, chksum1_calc, chksum2_recv, chksum2_calc;

  if (pkt == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((rmcp_hdr_len = fiid_template_len_bytes (tmpl_hdr_rmcp)) < 0)
    return (-1);

  if ((auth_type_start_bytes = fiid_template_field_start_bytes (tmpl_lan_session_hdr, (uint8_t *)"auth_type")) < 0)
    return (-1);
  
  auth_type_offset = rmcp_hdr_len + auth_type_start_bytes;
  auth_type = pkt[auth_type_offset];

  if ((msg_hdr_len1 = fiid_template_block_len_bytes(tmpl_lan_session_hdr,
						    (uint8_t *)"auth_type",
						    (uint8_t *)"session_id")) < 0)
    return (-1);

  if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
    auth_code_len = IPMI_SESSION_MAX_AUTH_CODE_LEN;
  else
    auth_code_len = 0;

  if ((msg_hdr_len2 = fiid_template_field_len_bytes(tmpl_lan_session_hdr,
						    (uint8_t *)"ipmi_msg_len")) < 0)
    return (-1);

  chksum1_block_index = rmcp_hdr_len + msg_hdr_len1 + auth_code_len + msg_hdr_len2;

  if ((chksum1_block_len = fiid_template_block_len_bytes(tmpl_lan_msg_hdr_rs,
							 (uint8_t *)"rq_addr",
							 (uint8_t *)"net_fn")) < 0)
    return (-1);

  if (pkt_len < (chksum1_block_index + chksum1_block_len + 1))
    return (0);

  chksum1_calc = ipmi_chksum(pkt + chksum1_block_index, chksum1_block_len);
  chksum1_recv = pkt[chksum1_block_index + chksum1_block_len];

  if (chksum1_calc != chksum1_recv)
    return (0);

  chksum2_block_index = chksum1_block_index + chksum1_block_len + 1;

  if (pkt_len <= (chksum2_block_index + 1))
    return (0);

  chksum2_block_len = pkt_len - chksum2_block_index - 1;
  
  chksum2_calc = ipmi_chksum(pkt + chksum2_block_index, chksum2_block_len);
  chksum2_recv = pkt[chksum2_block_index + chksum2_block_len];

  if (chksum2_calc != chksum2_recv)
    return (0);

  return (1);
}

