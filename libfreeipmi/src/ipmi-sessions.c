/* 
   ipmi-sessions.c - IPMI Session Handler

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

fiid_template_t tmpl_hdr_session = 
  {
    {8,  "auth_type"},
    {32, "session_seq_num"},
    {32, "session_id"},
    {8,  "ipmi_msg_len"},
    {0, ""}
  };

fiid_template_t tmpl_hdr_session_auth =
  {
    {8,   "auth_type"},
    {32,  "session_seq_num"},
    {32,  "session_id"},
    {128, "auth_code"},
    {8,   "ipmi_msg_len"},
    {0,   ""}
  };

fiid_template_t tmpl_hdr_session_auth_calc =
  {
    {8,   "auth_type"},
    {32,  "session_seq_num"},
    {32,  "session_id"},
    {256, "auth_calc_data"},    /* up to 256 bits */
    {8,   "ipmi_msg_len"},
    {0,   ""}
  };


int8_t
fill_hdr_session  (fiid_template_t tmpl_session, uint8_t auth_type, uint32_t inbound_seq_num, uint32_t session_id, uint8_t *auth_code_data, uint32_t auth_code_data_len, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr)
{
  if (!IPMI_1_5_SESSION_AUTH_TYPE_VALID(auth_type)
      || !(tmpl_session && tmpl_cmd && obj_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_hdr, tmpl_session, (uint8_t *)"auth_type", auth_type);
  FIID_OBJ_SET (obj_hdr, tmpl_session, (uint8_t *)"session_seq_num", inbound_seq_num);
  FIID_OBJ_SET (obj_hdr, tmpl_session, (uint8_t *)"session_id", session_id);
  if (auth_code_data && auth_code_data_len > 0
      && (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
          || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
          || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
          || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP))
    {
      if (fiid_obj_field_lookup (tmpl_session, (uint8_t *)"auth_code")) 
        {
          /* achu: auth_code_data_len can be equal to
           * IPMI_SESSION_MAX_AUTH_CODE_LEN, null termination is not
           * required.
           */
          if (auth_code_data_len > IPMI_SESSION_MAX_AUTH_CODE_LEN)
            {
              errno = EINVAL;
              return (-1);
            }
          
          /* achu: The BMC may ignore any '\0' characters that indicate the
           * end of the string.  So we need to guarantee the buffer is
           * completely cleared before setting anything.
           */
          ERR_EXIT (fiid_obj_memset_field (obj_hdr, '\0', 
                                           tmpl_session, (uint8_t *)"auth_code") == 0);
          ERR_EXIT (fiid_obj_set_data (obj_hdr, 
                                       tmpl_session, 
                                       (uint8_t *)"auth_code", 
                                       auth_code_data, 
                                       auth_code_data_len) == 0);
        }
      else if (fiid_obj_field_lookup (tmpl_session, (uint8_t *)"auth_calc_data"))
        {
          /* tmpl_hdr_session_auth_calc does not support all
           * authentication types
           */
          if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE
              && auth_type != IPMI_SESSION_AUTH_TYPE_MD2
              && auth_type != IPMI_SESSION_AUTH_TYPE_MD5
              && auth_type != IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
            {
              errno = EINVAL;
              return (-1);
            }

          if (auth_code_data_len > fiid_obj_field_len_bytes (tmpl_session, (uint8_t *)"auth_calc_data"))
            {
              errno = EINVAL;
              return (-1);
            }
          
          /* achu: The BMC may ignore any '\0' characters that indicate the
           * end of the string.  So we need to guarantee the buffer is
           * completely cleared before setting anything.
           */
          ERR_EXIT (fiid_obj_memset_field (obj_hdr, '\0', 
                                           tmpl_session, (uint8_t *)"auth_calc_data") == 0);
          ERR_EXIT (fiid_obj_set_data (obj_hdr, 
                                       tmpl_session, 
                                       (uint8_t *)"auth_calc_data", 
                                       auth_code_data, 
                                       auth_code_data_len) == 0);
        }
      else
        {
          /* achu: user requested non-anonymous authentication without
           * passing in a template that supports authentication
           */ 
          errno = EINVAL;
          return (-1);
        }
    }

  FIID_OBJ_SET (obj_hdr, tmpl_session, (uint8_t *)"ipmi_msg_len", 
                (fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq) + 
                 fiid_obj_len_bytes (tmpl_cmd) + 
                 fiid_obj_len_bytes (tmpl_lan_msg_trlr)));
  
  return (0);
}

int8_t 
check_hdr_session_session_seq_num (fiid_template_t tmpl_hdr_session, fiid_obj_t obj_hdr_session, uint32_t session_seq_num)
{
  uint64_t session_seq_num_recv;

  if (!(tmpl_hdr_session && obj_hdr_session))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"session_seq_num"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_hdr_session, tmpl_hdr_session, (uint8_t *)"session_seq_num", &session_seq_num_recv);
  
  return ((((int32_t)session_seq_num_recv) == session_seq_num) ? 1 : 0);
}

int8_t 
check_hdr_session_session_id (fiid_template_t tmpl_hdr_session, fiid_obj_t obj_hdr_session, uint32_t session_id)
{
  uint64_t session_id_recv;

  if (!(tmpl_hdr_session && obj_hdr_session))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"session_id"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_hdr_session, tmpl_hdr_session, (uint8_t *)"session_id", &session_id_recv);

  return ((((int32_t)session_id_recv) == session_id) ? 1 : 0);
}

int8_t 
check_hdr_session_authcode (uint8_t *pkt, uint64_t pkt_len, fiid_template_t tmpl_hdr_session, uint8_t auth_type, uint8_t *auth_code_data, uint32_t auth_code_data_len)
{
  uint8_t auth_type_recv;
  uint32_t auth_type_offset, auth_code_offset;
  uint8_t auth_code_buf[IPMI_SESSION_MAX_AUTH_CODE_LEN];

  if (!(tmpl_hdr_session && pkt && auth_code_data_len <= IPMI_SESSION_MAX_AUTH_CODE_LEN))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_type")
      || !fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"session_id")
      || !fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"session_seq_num"))
    {
      errno = EINVAL;
      return (-1);
    }
  
  auth_type_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"auth_type");
  if (pkt_len < auth_type_offset)
    return 0;

  auth_type_recv = pkt[auth_type_offset];

  /* authcode check fails if authentication types do not match */
  if (auth_type != auth_type_recv)
    return 0;

  /* Automatically return 1 if auth type is none */
  if (auth_type_recv == IPMI_SESSION_AUTH_TYPE_NONE)
    return 1;

  /* Don't use passed in session template, use tmpl_hdr_session_auth */
  auth_code_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_code");
  if (pkt_len < (auth_code_offset + IPMI_SESSION_MAX_AUTH_CODE_LEN))
    return 0;

  memset(auth_code_buf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);

  if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_code"))
    {
      ERR_EXIT(fiid_obj_field_len_bytes (tmpl_hdr_session, (uint8_t *)"auth_code") == IPMI_SESSION_MAX_AUTH_CODE_LEN);

      if (auth_code_data) 
	memcpy(auth_code_buf, auth_code_data, auth_code_data_len);
    }
  else if (fiid_obj_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_calc_data"))
    {
      if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
	  || auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
	{
	  uint8_t pwbuf[IPMI_SESSION_MAX_AUTH_CODE_LEN];
	  uint32_t session_id_offset, session_seq_num_offset, data_offset;
          
          session_id_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_id");
          session_seq_num_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_hdr_session, (uint8_t *)"session_seq_num");
          data_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_len_bytes (tmpl_hdr_session_auth);

          if (pkt_len < session_id_offset
              || pkt_len < session_seq_num_offset
              || pkt_len < data_offset)
            return 0;

	  /* Must zero extend password.  No null termination is required. 
	   * Also, must memcpy instead of strcpy, password need not be
	   * 1 word
	   */   
	  memset(pwbuf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);
	  memcpy(pwbuf, auth_code_data, auth_code_data_len);
	  if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2)
	    {
	      ipmi_md2_t ctx;

	      ERR_EXIT(IPMI_SESSION_MAX_AUTH_CODE_LEN == IPMI_MD2_DIGEST_LEN);

	      ipmi_md2_init(&ctx);
	      ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
	      ipmi_md2_update_data(&ctx, 
				   pkt + session_id_offset,
				   fiid_obj_field_len_bytes (tmpl_hdr_session,
							     (uint8_t *)"session_id"));
	      ipmi_md2_update_data(&ctx, 
                                   pkt + data_offset, 
                                   pkt_len - data_offset);
	      ipmi_md2_update_data(&ctx, 
				   pkt + session_seq_num_offset,
				   fiid_obj_field_len_bytes (tmpl_hdr_session,
							     (uint8_t *)"session_seq_num"));
	      ipmi_md2_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
	      ipmi_md2_finish(&ctx, auth_code_buf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
	    }
	  else if (auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
	    {
	      ipmi_md5_t ctx;
	      
 	      ERR_EXIT(IPMI_SESSION_MAX_AUTH_CODE_LEN == IPMI_MD5_DIGEST_LEN);

	      ipmi_md5_init(&ctx);
	      ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
	      ipmi_md5_update_data(&ctx, 
				   pkt + session_id_offset,
				   fiid_obj_field_len_bytes (tmpl_hdr_session,
							     (uint8_t *)"session_id"));
	      ipmi_md5_update_data(&ctx, 
                                   pkt + data_offset, 
                                   pkt_len - data_offset);
	      ipmi_md5_update_data(&ctx, 
				   pkt + session_seq_num_offset,
				   fiid_obj_field_len_bytes (tmpl_hdr_session,
							     (uint8_t *)"session_seq_num"));
	      ipmi_md5_update_data(&ctx, pwbuf, IPMI_SESSION_MAX_AUTH_CODE_LEN);
	      ipmi_md5_finish(&ctx, auth_code_buf, IPMI_SESSION_MAX_AUTH_CODE_LEN);

	    }
	}
      else if (auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
	{
	  if (auth_code_data) 
	    memcpy(auth_code_buf, auth_code_data, auth_code_data_len);
	}
      else
	{
	  /* Unsupported auth type for calculations */
	  /* XXX is this the right errno type to return?? */
	  errno = EINVAL;
	  return (-1);
	}
    }
  else 
    {
      /* User passed in auth_type != none, but template does not support
       * authentication
       */
      errno = EINVAL;
      return (-1);
    }
  
  /* Must memcmp instead of strcmp, password need not be 1 word */
  if (memcmp(auth_code_buf, pkt + auth_code_offset, IPMI_SESSION_MAX_AUTH_CODE_LEN) == 0)
    return 1;
  else
    return 0;
}
