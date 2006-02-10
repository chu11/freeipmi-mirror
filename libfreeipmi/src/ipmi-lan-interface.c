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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#include "freeipmi.h"

/* IPMI LAN Message Request Header */
fiid_template_t tmpl_lan_msg_hdr_rq =
  {
    {8, "rs_addr"},
    {2, "rs_lun"},
    {6, "net_fn"},
    {8, "chksum1"},
    {8, "rq_addr"},
    {2, "rq_lun"},
    {6, "rq_seq"},
    {0, ""}
  };

/* IPMI LAN Message Response Header */
fiid_template_t tmpl_lan_msg_hdr_rs =
  {
    {8, "rq_addr"},
    {2, "rq_lun"},
    {6, "net_fn"},
    {8, "chksum1"},
    {8, "rs_addr"},
    {2, "rs_lun"},
    {6, "rq_seq"},
    {0, ""}
  };

/* IPMI LAN Message Trailer */
fiid_template_t tmpl_lan_msg_trlr = 
  {
    {8, "chksum2"},
    {0, ""}
  };

int8_t 
fill_lan_msg_hdr (uint8_t net_fn, 
		  uint8_t rs_lun, 
		  uint8_t rq_seq, 
		  fiid_obj_t obj_msg)
{
  if (!IPMI_NET_FN_VALID(net_fn)
      || !IPMI_BMC_LUN_VALID(rs_lun)
      || (rq_seq > IPMI_LAN_SEQ_NUM_MAX)
      || (obj_msg == NULL))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rs_addr", IPMI_SLAVE_ADDR_BMC);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"net_fn", net_fn);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rs_lun", rs_lun);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"chksum1", 
		ipmi_chksum (obj_msg, IPMI_LAN_PKT_RQ_CHKSUM1_BLOCK_LEN));
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rq_addr", IPMI_SLAVE_ADDR_SWID);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rq_lun", IPMI_BMC_IPMB_LUN_BMC);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, (uint8_t *)"rq_seq", rq_seq);
  return (0);
}

static int32_t 
_ipmi_lan_pkt_size (uint8_t auth_type, 
		    fiid_template_t tmpl_lan_msg, 
		    fiid_template_t tmpl_cmd)
{
  uint32_t msg_len;
  
  assert(IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
         && tmpl_lan_msg
         && tmpl_cmd);

  msg_len = fiid_obj_len_bytes (tmpl_hdr_rmcp) +
    fiid_obj_len_bytes (tmpl_lan_msg) +
    fiid_obj_len_bytes (tmpl_cmd) +
    fiid_obj_len_bytes (tmpl_lan_msg_trlr);

  if (auth_type == IPMI_SESSION_AUTH_TYPE_NONE) 
    msg_len += fiid_obj_len_bytes(tmpl_hdr_session);
  else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
           || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
           || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
           || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    msg_len += fiid_obj_len_bytes(tmpl_hdr_session_auth);
  
  return msg_len;
}

static int32_t 
_ipmi_lan_pkt_rq_size (uint8_t auth_type, 
		       fiid_template_t tmpl_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rq, tmpl_cmd);
}

int32_t 
_ipmi_lan_pkt_rs_size (uint8_t auth_type, 
		       fiid_template_t tmpl_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rs, tmpl_cmd);
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
		       fiid_template_t tmpl_hdr_session, 
		       fiid_obj_t obj_msg_hdr, 
		       fiid_obj_t obj_cmd, 
		       fiid_template_t tmpl_cmd, 
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  uint64_t auth_type;
  uint32_t indx, required_len;
  uint8_t *auth_code_field_ptr = NULL;
  uint8_t *msg_data_ptr = NULL;
  uint32_t msg_data_count = 0;
  uint32_t obj_len;
  int8_t chksum;

  if (!(obj_hdr_rmcp && obj_hdr_session && tmpl_hdr_session && 
        obj_msg_hdr && obj_cmd && tmpl_cmd && pkt))
    {
      errno = EINVAL;
      return -1;
    }
  
  if (!fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"auth_type")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_seq_num")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_id")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"))
    {
      errno = EINVAL;
      return -1;
    }

  fiid_obj_get(obj_hdr_session, tmpl_hdr_session, (uint8_t *)"auth_type", &auth_type);
  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type))
    {
      errno = EINVAL;
      return -1;
    }
  
  required_len = _ipmi_lan_pkt_rq_size((uint8_t)auth_type, tmpl_cmd);
  if (pkt_len < required_len) 
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset (pkt, 0, pkt_len);

  indx = 0;
  obj_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);
  memcpy (pkt, obj_hdr_rmcp, obj_len);
  indx += obj_len;

  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type"), 
          obj_len);
  indx += obj_len;
  
  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num"), 
          obj_len);
  indx += obj_len;

  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_id");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_id"), 
          obj_len);
  indx += obj_len;

  /* auth_code generated last.  Save pointers for later calculation */
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    {
      auth_code_field_ptr = (pkt + indx); 
      indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
    }
    
  obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len");
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"), 
          obj_len);
  indx += obj_len;

  msg_data_ptr = (pkt + indx);
  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq);
  memcpy (pkt + indx, obj_msg_hdr, obj_len);
  indx += obj_len;
  msg_data_count += obj_len;

  obj_len = fiid_obj_len_bytes (tmpl_cmd);
  memcpy (pkt + indx, obj_cmd, obj_len);
  indx += obj_len;
  msg_data_count += obj_len;

  chksum = ipmi_chksum (pkt + IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_INDX (auth_type), 
                        IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_LEN (tmpl_cmd));
  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr);
  memcpy (pkt + indx, &chksum, obj_len);
  indx += obj_len;
  msg_data_count += obj_len;

  /* Auth code must be done last, some authentication like md2 and md5
   * require all fields, including checksums, to be calculated
   * beforehand
   */
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    {
      if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_code"))
        {
          ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_code") == IPMI_SESSION_MAX_AUTH_CODE_LEN);
          memcpy (auth_code_field_ptr,
                  obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_code"),
                  fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_code"));
        }
      else if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_calc_data"))
        {
          if (auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
            {
              ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data") >= IPMI_SESSION_MAX_AUTH_CODE_LEN);

              /* achu: Do not copy based on field length of "auth_calc_data" */
              memcpy (auth_code_field_ptr,
                      obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data"),
                      IPMI_SESSION_MAX_AUTH_CODE_LEN);
            }
          else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
                   || auth_type == IPMI_SESSION_AUTH_TYPE_MD5) 
            {
              uint8_t pwbuf[IPMI_SESSION_MAX_AUTH_CODE_LEN];

              ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data") >= IPMI_SESSION_MAX_AUTH_CODE_LEN);
              ERR_EXIT(IPMI_SESSION_MAX_AUTH_CODE_LEN == IPMI_MD2_DIGEST_LEN);

              /* Must zero extend password.  No null termination is required. 
	       * Also, must memcpy instead of strcpy, password need not be
	       * 1 word
	       */   
              memset(pwbuf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);
              memcpy(pwbuf, 
		     (obj_hdr_session + 
		      fiid_obj_field_start_bytes (tmpl_hdr_session, 
						  (uint8_t *)"auth_calc_data")), 
		     IPMI_SESSION_MAX_AUTH_CODE_LEN);

              if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2)
                {
                  ipmi_md2_t ctx;
                  uint8_t digest[IPMI_MD2_DIGEST_LEN];
             
                  ipmi_md2_init(&ctx);
                  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md2_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_id")), 
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_id"));
                  ipmi_md2_update_data(&ctx, msg_data_ptr, msg_data_count);
                  ipmi_md2_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_seq_num")),
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_seq_num"));
                  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md2_finish(&ctx, digest, IPMI_MD2_DIGEST_LEN);
                  
                  memcpy (auth_code_field_ptr, digest, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                }
              else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
                {
                  ipmi_md5_t ctx;
                  uint8_t digest[IPMI_MD5_DIGEST_LEN];
                              
                  ipmi_md5_init(&ctx);
                  ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md5_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_id")), 
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_id"));
                  ipmi_md5_update_data(&ctx, msg_data_ptr, msg_data_count);
                  ipmi_md5_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    (uint8_t *)"session_seq_num")),
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 (uint8_t *)"session_seq_num"));
                  ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md5_finish(&ctx, digest, IPMI_MD5_DIGEST_LEN);
                  
                  memcpy (auth_code_field_ptr, digest, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                }
            }
          else
            {
              /* tmpl_hdr_session_auth_calc does not support this
               * authentication type 
               */
              errno = EINVAL;
              return -1;
            }
        }
    }

  return indx;
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
			 fiid_template_t tmpl_hdr_session, 
			 fiid_template_t tmpl_cmd, 
			 fiid_obj_t obj_hdr_rmcp, 
			 fiid_obj_t obj_hdr_session, 
			 fiid_obj_t obj_msg_hdr, 
			 fiid_obj_t obj_cmd, 
			 fiid_obj_t obj_msg_trlr)
{
  uint8_t auth_type;
  uint32_t auth_type_offset, indx;
  uint32_t obj_cmd_len, obj_len, obj_rmcp_hdr_len, obj_msg_trlr_len;

  if (!(pkt && tmpl_hdr_session && tmpl_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  if (!fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"auth_type")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_seq_num")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"session_id")
      || !fiid_obj_field_lookup(tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"))
    {
      errno = EINVAL;
      return -1;
    }

  indx = 0;
  obj_rmcp_hdr_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);
  if (obj_hdr_rmcp)
    {
      memcpy (obj_hdr_rmcp, pkt + indx,
	      FREEIPMI_MIN(pkt_len - indx, obj_rmcp_hdr_len));
    }
  indx += obj_rmcp_hdr_len;

  if (pkt_len <= indx)
    return 0;

  if ((pkt_len - indx) < fiid_obj_field_end_bytes (tmpl_hdr_session, (uint8_t *)"auth_type"))
    {
      /* Special case, return after copying this */
      memcpy(obj_hdr_session, pkt + indx, (pkt_len - indx));
      return 0;
    }

  auth_type_offset = obj_rmcp_hdr_len + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
  auth_type = pkt[auth_type_offset];

  if (obj_hdr_session)
    {
      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
      memcpy (obj_hdr_session + 
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type"), 
	      pkt + indx, 
	      FREEIPMI_MIN ((pkt_len - indx), obj_len));
      indx += obj_len;
      
      if (pkt_len <= indx)
        return 0;

      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num");
      memcpy (obj_hdr_session + 
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num"), 
	      pkt + indx, 
	      FREEIPMI_MIN ((pkt_len - indx), obj_len));
      indx += obj_len;

      if (pkt_len <= indx)
        return 0;

      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_id");
      memcpy (obj_hdr_session + 
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_id"), 
	      pkt + indx,
	      FREEIPMI_MIN ((pkt_len - indx), obj_len));
      indx += obj_len;

      if (pkt_len <= indx)
        return 0;

      if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
        {
          if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_code")) 
            {
              obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_code");
	      ERR_EXIT(obj_len == IPMI_SESSION_MAX_AUTH_CODE_LEN);
              memcpy (obj_hdr_session + 
		      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_code"), 
		      pkt + indx, 
		      FREEIPMI_MIN ((pkt_len - indx), obj_len));
              indx += obj_len;
            }
	  else if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_calc_data"))
	    {
              obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data");
	      ERR_EXIT(obj_len >= IPMI_SESSION_MAX_AUTH_CODE_LEN);

	      /* Must copy IPMI_SESSION_MAX_AUTH_CODE_LEN,
	      auth_calc_data may be > IPMI_SESSION_MAX_AUTH_CODE_LEN
	      */
              memcpy (obj_hdr_session +
		      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_calc_data"),
                      pkt + indx, 
		      FREEIPMI_MIN ((pkt_len - indx), IPMI_SESSION_MAX_AUTH_CODE_LEN));
              indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
	    }
          else
            {
              /* achu: user passed invalid template and we cannot
               * store the authentication code anywhere.
               */ 
              errno = EINVAL;
              return -1;
            }
        }
      
      obj_len = fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len");
      memcpy (obj_hdr_session +
	      fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len"),
	      pkt + indx,
	      obj_len);
      indx += obj_len;
    }
  else
    {
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num");
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"session_id");
      if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
        indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"ipmi_msg_len");
    }

  if (pkt_len <= indx)
    return 0;

  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_hdr_rs);
  if (obj_msg_hdr)
    memcpy (obj_msg_hdr,
	    pkt + indx,
	    FREEIPMI_MIN((pkt_len - indx), obj_len));
  indx += obj_len;

  if (pkt_len <= indx)
    return 0;

  obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd);
  obj_msg_trlr_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr);

  if ((pkt_len - indx) <= obj_cmd_len)
    {
      if ((pkt_len - indx) > obj_msg_trlr_len)
        obj_cmd_len = (pkt_len - indx) - obj_msg_trlr_len;
      else
        obj_cmd_len = (pkt_len - indx);
    }

  if (obj_cmd)
    memcpy (obj_cmd,
            pkt + indx,
            FREEIPMI_MIN((pkt_len - indx), obj_cmd_len));
  indx += obj_cmd_len;

  if (pkt_len <= indx)
    return 0;

  obj_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr);
  if (obj_msg_trlr)
    memcpy (obj_msg_trlr,
	    pkt + indx,
	    FREEIPMI_MIN((pkt_len - indx), obj_msg_trlr_len));
  indx += obj_msg_trlr_len;
  
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
ipmi_lan_check_net_fn (fiid_template_t tmpl_msg_hdr, 
		       fiid_obj_t obj_msg_hdr, 
		       uint8_t net_fn)
{
  uint64_t net_fn_recv;

  if (!(obj_msg_hdr 
        && tmpl_msg_hdr 
        && IPMI_NET_FN_VALID(net_fn)))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_msg_hdr, (uint8_t *)"net_fn"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, tmpl_msg_hdr, (uint8_t *)"net_fn", &net_fn_recv);

  return ((((uint8_t)net_fn_recv) == net_fn) ? 1 : 0);
}

int8_t 
ipmi_lan_check_rq_seq (fiid_template_t tmpl_msg_hdr, 
		       fiid_obj_t obj_msg_hdr, 
		       uint8_t rq_seq)
{
  uint64_t rq_seq_recv;

  if (!(obj_msg_hdr && tmpl_msg_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_msg_hdr, (uint8_t *)"rq_seq"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, tmpl_msg_hdr, (uint8_t *)"rq_seq", &rq_seq_recv);

  return ((((uint8_t)rq_seq_recv) == rq_seq) ? 1 : 0);
}

int8_t 
ipmi_lan_check_chksum (uint8_t *pkt, uint64_t pkt_len)
{
  uint8_t auth_type;
  uint32_t auth_type_offset, required_len;

  if (pkt == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  if (pkt_len < (fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_end_bytes (tmpl_hdr_session, (uint8_t *)"auth_type")))
    return (0);

  auth_type_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
  auth_type = pkt[auth_type_offset];

  if (ipmi_chksum_test (pkt + IPMI_LAN_PKT_CHKSUM1_BLOCK_INDX (auth_type), 
			IPMI_LAN_PKT_RS_CHKSUM1_BLOCK_LEN + 1))
    {
      required_len = IPMI_LAN_PKT_CHKSUM1_BLOCK_INDX (auth_type) + IPMI_LAN_PKT_RS_CHKSUM1_BLOCK_LEN + 1;
      if (pkt_len <= required_len)
	return (0);

      if (ipmi_chksum_test (pkt + IPMI_LAN_PKT_RS_CHKSUM2_BLOCK_INDX (auth_type),
			    pkt_len - required_len))
	{
	  return (1);
	}
    }

  return (0);
}
