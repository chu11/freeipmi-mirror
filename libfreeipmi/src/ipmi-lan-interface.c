/* 
   ipmi-lan-interface.c - IPMI LAN Interface

   Copyright (C) 2003, 2004 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <errno.h>

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
    {8, "rs_addr"},
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
fill_lan_msg_hdr (u_int8_t net_fn, u_int8_t rs_lun, u_int8_t rq_seq, fiid_obj_t obj_msg)
{
  if ((net_fn > IPMI_NET_FN_TRANSPORT_RS)
      || (rs_lun > IPMI_BMC_IPMB_LUN_OEM_LUN2)
      || (rq_seq > IPMI_LAN_SEQ_NUM_MAX)
      || (obj_msg == NULL))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, "rs_addr", IPMI_SLAVE_ADDR_BMC);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, "net_fn", net_fn);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, "rs_lun", rs_lun);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, "chksum1", 
		ipmi_chksum (obj_msg, IPMI_LAN_PKT_RQ_CHKSUM1_BLOCK_LEN));
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, "rq_addr", IPMI_SLAVE_ADDR_SWID);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, "rq_lun", IPMI_BMC_IPMB_LUN_BMC);
  FIID_OBJ_SET (obj_msg, tmpl_lan_msg_hdr_rq, "rq_seq", rq_seq);
  return (0);
}

int32_t
_ipmi_lan_pkt_size(u_int8_t auth_type, fiid_template_t tmpl_lan_msg, fiid_template_t tmpl_cmd)
{
  u_int32_t msg_len;
  
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
  else 
    /* fatal error, library should not call this function with a bad auth_type */
    ERR_EXIT(0);
  
  return msg_len;
}

int32_t
_ipmi_lan_pkt_rq_size(u_int8_t auth_type, fiid_template_t tmpl_cmd)
{
  return _ipmi_lan_pkt_size(auth_type, tmpl_lan_msg_hdr_rq, tmpl_cmd);
}

int32_t
_ipmi_lan_pkt_rs_size(u_int8_t auth_type, fiid_template_t tmpl_cmd)
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
assemble_ipmi_lan_pkt (fiid_obj_t obj_hdr_rmcp, fiid_obj_t obj_hdr_session, fiid_template_t tmpl_hdr_session, fiid_obj_t obj_msg_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, u_int8_t *pkt, u_int32_t pkt_len)
{
  u_int64_t auth_type;
  u_int32_t indx, required_len;
  u_int8_t *auth_code_field_ptr = NULL;
  u_int8_t *msg_data_ptr = NULL;
  u_int32_t msg_data_count = 0;
  ipmi_chksum_t chksum;

  if (!(obj_hdr_rmcp && obj_hdr_session && tmpl_hdr_session && 
        obj_msg_hdr && obj_cmd && tmpl_cmd && pkt))
    {
      errno = EINVAL;
      return -1;
    }
  
  if (!fiid_obj_field_lookup(tmpl_hdr_session, "auth_type")
      || !fiid_obj_field_lookup(tmpl_hdr_session, "session_seq_num")
      || !fiid_obj_field_lookup(tmpl_hdr_session, "session_id")
      || !fiid_obj_field_lookup(tmpl_hdr_session, "ipmi_msg_len"))
    {
      errno = EINVAL;
      return -1;
    }

  fiid_obj_get(obj_hdr_session, tmpl_hdr_session, "auth_type", &auth_type);
  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type))
    {
      errno = EINVAL;
      return -1;
    }
  
  required_len = _ipmi_lan_pkt_rq_size((u_int8_t)auth_type, tmpl_cmd);
  if (pkt_len < required_len) 
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset (pkt, 0, pkt_len);

  indx = 0;
  memcpy (pkt, obj_hdr_rmcp, fiid_obj_len_bytes (tmpl_hdr_rmcp));
  indx += fiid_obj_len_bytes (tmpl_hdr_rmcp);

  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_type"), 
          fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_type"));
  indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_type");
  
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "session_seq_num"), 
          fiid_obj_field_len_bytes (tmpl_hdr_session, "session_seq_num"));
  indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "session_seq_num");

  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "session_id"), 
          fiid_obj_field_len_bytes (tmpl_hdr_session, "session_id"));
  indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "session_id");

  /* auth_code generated last.  Save pointers for later calculate */
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    {
      auth_code_field_ptr = (pkt + indx); 
      indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
    }
    
  memcpy (pkt + indx, 
          obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "ipmi_msg_len"), 
          fiid_obj_field_len_bytes (tmpl_hdr_session, "ipmi_msg_len"));
  indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "ipmi_msg_len");

  msg_data_ptr = (pkt + indx);
  memcpy (pkt + indx, obj_msg_hdr, fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq));
  indx += fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq);
  msg_data_count += fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq);

  memcpy (pkt + indx, obj_cmd, fiid_obj_len_bytes (tmpl_cmd));
  indx += fiid_obj_len_bytes (tmpl_cmd);
  msg_data_count += fiid_obj_len_bytes (tmpl_cmd);

  chksum = ipmi_chksum (pkt + IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_INDX (auth_type), 
                        IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_LEN (tmpl_cmd));
  memcpy (pkt + indx, &chksum, fiid_obj_len_bytes (tmpl_lan_msg_trlr));
  indx += fiid_obj_len_bytes (tmpl_lan_msg_trlr);
  msg_data_count += fiid_obj_len_bytes (tmpl_lan_msg_trlr);

  /* Auth type must be done last, some authentication like md2 and md5
   * require all fields, including checksums, to be calculated
   * beforehand
   */
  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
      || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
      || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
      || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP) 
    {
      if (fiid_obj_field_lookup (tmpl_hdr_session, "auth_code"))
        {
          ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_code") == IPMI_SESSION_MAX_AUTH_CODE_LEN);
          memcpy (auth_code_field_ptr,
                  obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_code"),
                  fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_code"));
        }
      else if (fiid_obj_field_lookup (tmpl_hdr_session, "auth_calc_data"))
        {
          if (auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
            {
              ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_calc_data") >= IPMI_SESSION_MAX_AUTH_CODE_LEN);

              /* achu: Do not copy based on field length of "auth_calc_data" */
              memcpy (auth_code_field_ptr,
                      obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_calc_data"),
                      IPMI_SESSION_MAX_AUTH_CODE_LEN);
            }
          else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
                   || auth_type == IPMI_SESSION_AUTH_TYPE_MD5) 
            {
              u_int8_t pwbuf[IPMI_SESSION_MAX_AUTH_CODE_LEN];

              ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_calc_data") >= IPMI_SESSION_MAX_AUTH_CODE_LEN);
              ERR_EXIT(IPMI_SESSION_MAX_AUTH_CODE_LEN == IPMI_MD2_DIGEST_LEN);

              /* Must zero extend password.  No null termination is required. 
	       * Also, must memcpy instead of strcpy, password need not be
	       * 1 word
	       */   
              memset(pwbuf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);
              memcpy(pwbuf, 
		     (obj_hdr_session + 
		      fiid_obj_field_start_bytes (tmpl_hdr_session, 
						  "auth_calc_data")), 
		     IPMI_SESSION_MAX_AUTH_CODE_LEN);

              if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2)
                {
                  ipmi_md2_t ctx;
                  u_int8_t digest[IPMI_MD2_DIGEST_LEN];
             
                  ipmi_md2_init(&ctx);
                  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md2_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    "session_id")), 
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 "session_id"));
                  ipmi_md2_update_data(&ctx, msg_data_ptr, msg_data_count);
                  ipmi_md2_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    "session_seq_num")),
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 "session_seq_num"));
                  ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md2_finish(&ctx, digest, IPMI_MD2_DIGEST_LEN);
                  
                  memcpy (auth_code_field_ptr, digest, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                }
              else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
                {
                  ipmi_md5_t ctx;
                  u_int8_t digest[IPMI_MD5_DIGEST_LEN];
                              
                  ipmi_md5_init(&ctx);
                  ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
                  ipmi_md5_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    "session_id")), 
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 "session_id"));
                  ipmi_md5_update_data(&ctx, msg_data_ptr, msg_data_count);
                  ipmi_md5_update_data(&ctx, 
                                       (obj_hdr_session +
                                        fiid_obj_field_start_bytes (tmpl_hdr_session,
                                                                    "session_seq_num")),
                                       fiid_obj_field_len_bytes (tmpl_hdr_session,
                                                                 "session_seq_num"));
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
unassemble_ipmi_lan_pkt (u_int8_t *pkt, u_int32_t pkt_len, fiid_template_t tmpl_hdr_session, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr_rmcp, fiid_obj_t obj_hdr_session, fiid_obj_t obj_msg_hdr, fiid_obj_t obj_cmd, fiid_obj_t obj_msg_trlr)
{
  u_int8_t auth_type;
  u_int32_t auth_type_offset, indx;

  if (!(pkt && tmpl_hdr_session && tmpl_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  if (!fiid_obj_field_lookup(tmpl_hdr_session, "auth_type")
      || !fiid_obj_field_lookup(tmpl_hdr_session, "session_seq_num")
      || !fiid_obj_field_lookup(tmpl_hdr_session, "session_id")
      || !fiid_obj_field_lookup(tmpl_hdr_session, "ipmi_msg_len"))
    {
      errno = EINVAL;
      return -1;
    }

  indx = 0;
  if (obj_hdr_rmcp)
    {
      memcpy (obj_hdr_rmcp, pkt + indx, FREEIPMI_MIN(pkt_len - indx, fiid_obj_len_bytes (tmpl_hdr_rmcp)));
    }
  indx += fiid_obj_len_bytes (tmpl_hdr_rmcp);

  if (pkt_len <= indx)
    return 0;

  if ((pkt_len - indx) < fiid_obj_field_end_bytes (tmpl_hdr_session, "auth_type"))
    {
      /* Special case, return after copying this */
      memcpy(obj_hdr_session, pkt + indx, (pkt_len - indx));
      return 0;
    }

  auth_type_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_type");
  auth_type = pkt[auth_type_offset];

  if (obj_hdr_session)
    {
      memcpy (obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_type"), 
              pkt + indx, FREEIPMI_MIN ((pkt_len - indx), fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_type")));
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_type");

      if (pkt_len <= indx)
        return 0;

      memcpy (obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "session_seq_num"), 
              pkt + indx, FREEIPMI_MIN ((pkt_len - indx), fiid_obj_field_len_bytes (tmpl_hdr_session, "session_seq_num")));
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "session_seq_num");

      if (pkt_len <= indx)
        return 0;

      memcpy (obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "session_id"), 
              pkt + indx, FREEIPMI_MIN ((pkt_len - indx), fiid_obj_field_len_bytes (tmpl_hdr_session, "session_id")));
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "session_id");

      if (pkt_len <= indx)
        return 0;

      if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
        {
          if (fiid_obj_field_lookup (tmpl_hdr_session, "auth_code")) 
            {
	      ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_code") == IPMI_SESSION_MAX_AUTH_CODE_LEN);
              memcpy (obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_code"),
                      pkt + indx, FREEIPMI_MIN ((pkt_len - indx), fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_code")));
              indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_code");
            }
	  else if (fiid_obj_field_lookup (tmpl_hdr_session, "auth_calc_data"))
	    {
	      ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_calc_data") >= IPMI_SESSION_MAX_AUTH_CODE_LEN);

	      /* Must copy IPMI_SESSION_MAX_AUTH_CODE_LEN, auth_calc_data may be > IPMI_SESSION_MAX_AUTH_CODE_LEN */
              memcpy (obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_calc_data"),
                      pkt + indx, FREEIPMI_MIN ((pkt_len - indx), IPMI_SESSION_MAX_AUTH_CODE_LEN));
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
      
      memcpy (obj_hdr_session + fiid_obj_field_start_bytes (tmpl_hdr_session, "ipmi_msg_len"), 
              pkt + indx, fiid_obj_field_len_bytes (tmpl_hdr_session, "ipmi_msg_len"));
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "ipmi_msg_len");
    }
  else
    {
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "auth_type");
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "session_seq_num");
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "session_id");
      if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
        indx += IPMI_SESSION_MAX_AUTH_CODE_LEN;
      indx += fiid_obj_field_len_bytes (tmpl_hdr_session, "ipmi_msg_len");
    }

  if (pkt_len <= indx)
    return 0;

  if (obj_msg_hdr)
    memcpy (obj_msg_hdr, pkt + indx, FREEIPMI_MIN((pkt_len - indx), fiid_obj_len_bytes (tmpl_lan_msg_hdr_rs)));
  indx += fiid_obj_len_bytes (tmpl_lan_msg_hdr_rs);

  if (pkt_len <= indx)
    return 0;

  if (obj_cmd)
    memcpy (obj_cmd, pkt + indx, FREEIPMI_MIN((pkt_len - indx), fiid_obj_len_bytes (tmpl_cmd)));
  indx += fiid_obj_len_bytes (tmpl_cmd);

  if (pkt_len <= indx)
    return 0;

  if (obj_msg_trlr)
    memcpy (obj_msg_trlr, pkt + indx, FREEIPMI_MIN((pkt_len - indx), fiid_obj_len_bytes (tmpl_lan_msg_trlr)));
  indx += fiid_obj_len_bytes (tmpl_lan_msg_trlr);
  
  return 0;
}

ssize_t ipmi_lan_sendto (int sockfd, const void *pkt, size_t pkt_len, int flags, const struct sockaddr *to, unsigned int tolen)
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
    packets.
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

/*   if (_pkt) free (_pkt); */

  if (bytes_sent == -1)
    return -1;
  else
    return (bytes_sent - pad_len);
}

ssize_t  ipmi_lan_recvfrom (int sockfd, void *pkt, size_t pkt_len, int flags, struct sockaddr *from, unsigned int *fromlen)
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
ipmi_lan_cmd (u_int32_t sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, u_int8_t auth_type, u_int32_t session_seq_num, u_int32_t session_id, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, u_int8_t net_fn, u_int8_t lun, u_int8_t rq_seq, fiid_obj_t obj_cmd_rq, fiid_template_t tmpl_cmd_rq, fiid_obj_t obj_cmd_rs, fiid_template_t tmpl_cmd_rs)
{
  fiid_template_t *tmpl_hdr_session_ptr;

  if (!(hostaddr && sockfd && hostaddr_len && tmpl_cmd_rq && obj_cmd_rq 
	&& tmpl_cmd_rs && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  if (auth_type == IPMI_SESSION_AUTH_TYPE_NONE)
    tmpl_hdr_session_ptr = &tmpl_hdr_session;
  else if (auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)
    tmpl_hdr_session_ptr = &tmpl_hdr_session_auth;
  else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
	   || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
	   || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
    tmpl_hdr_session_ptr = &tmpl_hdr_session_auth_calc;
  else
    {
      errno = EINVAL;
      return (-1);
    }

  {
    fiid_obj_t obj_hdr_rmcp;
    fiid_obj_t obj_hdr_session;
    fiid_obj_t obj_msg_hdr;
    u_int8_t *pkt;
    u_int32_t pkt_len;
    int status = 0;

    FIID_OBJ_ALLOCA (obj_hdr_rmcp, tmpl_hdr_rmcp);
    FIID_OBJ_ALLOCA (obj_hdr_session, *tmpl_hdr_session_ptr);
    FIID_OBJ_ALLOCA (obj_msg_hdr, tmpl_lan_msg_hdr_rq);

    pkt_len = _ipmi_lan_pkt_rq_size(auth_type, tmpl_cmd_rq); 
    pkt = alloca (pkt_len);
    ERR (pkt);
    memset (pkt, 0, pkt_len);
   
    ERR (fill_hdr_rmcp_ipmi (obj_hdr_rmcp) != -1);
    ERR (fill_hdr_session (*tmpl_hdr_session_ptr, auth_type, session_seq_num, 
			   session_id, auth_code_data, auth_code_data_len,
                           tmpl_cmd_rq, obj_hdr_session) != -1);
    ERR (fill_lan_msg_hdr (net_fn, lun, rq_seq, obj_msg_hdr) != -1);
    ERR (assemble_ipmi_lan_pkt (obj_hdr_rmcp, obj_hdr_session, *tmpl_hdr_session_ptr,
				obj_msg_hdr, obj_cmd_rq, tmpl_cmd_rq,
				pkt, pkt_len) != -1);
    /* __DEBUG >> */
/*     fiid_obj_dump (2, obj_hdr_rmcp, tmpl_hdr_rmcp); */
/*     fiid_obj_dump (2, obj_hdr_session, *tmpl_hdr_session_ptr); */
/*     fiid_obj_dump (2, obj_msg_hdr, tmpl_lan_msg_hdr_rq); */
/*     fiid_obj_dump (2, obj_cmd_rq, tmpl_cmd_rq); */
    /* __DEBUG >> */

    status = ipmi_lan_sendto (sockfd, pkt, pkt_len, 0, hostaddr, hostaddr_len);
    ERR (status != -1);
  }

  {
    struct sockaddr_in from;
    socklen_t fromlen;
    fiid_obj_t obj_hdr_session;
    u_int8_t *pkt;
    u_int32_t _pkt_len = 1024;
    int32_t pkt_len;

    FIID_OBJ_ALLOCA (obj_hdr_session, *tmpl_hdr_session_ptr);

    pkt_len = _ipmi_lan_pkt_rs_size (auth_type, tmpl_cmd_rs);
    pkt     = alloca (_pkt_len);
    memset (pkt, 0, _pkt_len);
    ERR (pkt);
    
    pkt_len = ipmi_lan_recvfrom (sockfd, pkt, _pkt_len, 0, (struct sockaddr *)&from, 
				&fromlen);
    ERR (pkt_len != -1);
    ERR (ipmi_lan_check_chksum (pkt, pkt_len) == 1);
    ERR (unassemble_ipmi_lan_pkt (pkt, pkt_len, *tmpl_hdr_session_ptr, tmpl_cmd_rs,
				  0, obj_hdr_session, 0, obj_cmd_rs, 0) != -1);

    /* __DEBUG__ >> */
/*     fiid_obj_dump (2, obj_cmd_rs, tmpl_cmd_rs); */
    /* __DEBUG__ << */

    ERR (ipmi_comp_test (obj_cmd_rs));

  }
  return (0);
}

int8_t
ipmi_lan_check_net_fn(fiid_template_t tmpl_msg_hdr, fiid_obj_t obj_msg_hdr, u_int8_t net_fn)
{
  u_int64_t net_fn_recv;

  if (!(obj_msg_hdr && tmpl_msg_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_msg_hdr, "net_fn"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, tmpl_msg_hdr, "net_fn", &net_fn_recv);

  return ((((int8_t)net_fn_recv) == net_fn) ? 1 : 0);
}

int8_t
ipmi_lan_check_rq_seq(fiid_template_t tmpl_msg_hdr, fiid_obj_t obj_msg_hdr, u_int8_t rq_seq)
{
  u_int64_t rq_seq_recv;

  if (!(obj_msg_hdr && tmpl_msg_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_msg_hdr, "rq_seq"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_msg_hdr, tmpl_msg_hdr, "rq_seq", &rq_seq_recv);

  return ((((int8_t)rq_seq_recv) == rq_seq) ? 1 : 0);
}

int8_t
ipmi_lan_check_chksum (u_int8_t *pkt, u_int64_t pkt_len)
{
  u_int8_t auth_type;
  u_int32_t auth_type_offset, required_len;

  if (pkt == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  if (pkt_len < (fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_end_bytes (tmpl_hdr_session, "auth_type")))
    return (0);

  auth_type_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session, "auth_type");
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
