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
    {8,  "auth_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_seq_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "ipmi_msg_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0,  "", 0}
  };

fiid_template_t tmpl_hdr_session_auth =
  {
    {8,   "auth_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "session_seq_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "auth_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "ipmi_msg_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0,   "", 0}
  };

fiid_template_t tmpl_hdr_session_auth_calc =
  {
    {8,   "auth_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "session_seq_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {256, "auth_calc_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {8,   "ipmi_msg_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0,   "", 0}
  };


int8_t
fill_hdr_session  (uint8_t auth_type, uint32_t inbound_seq_num, uint32_t session_id, uint8_t *auth_code_data, uint32_t auth_code_data_len, fiid_obj_t obj_hdr)
{
  char *auth_field;
  int8_t rv;

  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
      || !fiid_obj_valid(obj_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_session)) < 0)
    return (-1);

  if (!rv)
    {
      if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_session_auth)) < 0)
	return (-1);
    }

  if (!rv)
    {
      if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_session_auth_calc)) < 0)
	return (-1);
    }

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_hdr, (uint8_t *)"auth_type", auth_type);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"session_seq_num", inbound_seq_num);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"session_id", session_id);

  if ((rv = fiid_obj_field_lookup (obj_hdr, (uint8_t *)"auth_code")) < 0)
    return -1;
  
  if (rv)
    auth_field = "auth_code";
  else 
    {
      if ((rv = fiid_obj_field_lookup (obj_hdr, (uint8_t *)"auth_calc_data")) < 0)
	return -1;
    }

  if (rv)
    auth_field = "auth_calc_data";
  else
    {
      /* achu: user requested non-anonymous authentication without
       * passing in a template that supports authentication
       */ 
      errno = EINVAL;
      return -1;
    }
  
  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  ERR_EXIT (fiid_obj_clear_field (obj_hdr, (uint8_t *)auth_field) == 0);
  
  if (auth_code_data && auth_code_data_len > 0
      && (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
          || auth_type == IPMI_SESSION_AUTH_TYPE_MD5
          || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
          || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP))
    {
      if (!strcmp(auth_field, "auth_code"))
        {
          char buf[IPMI_SESSION_MAX_AUTH_CODE_LEN];

          /* achu: auth_code_data_len can be equal to
           * IPMI_SESSION_MAX_AUTH_CODE_LEN, null termination is not
           * required.
           */
          if (auth_code_data_len > IPMI_SESSION_MAX_AUTH_CODE_LEN)
            {
              errno = EINVAL;
              return (-1);
            }

          memset(buf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);
          memcpy(buf, auth_code_data, auth_code_data_len);

          ERR_EXIT (!(fiid_obj_set_data (obj_hdr, 
                                         (uint8_t *)auth_field, 
                                         (uint8_t *)buf,
                                         IPMI_SESSION_MAX_AUTH_CODE_LEN) < 0));
        }
      else
        {
          /* we don't currently support OEM authentication */
          if (auth_type != IPMI_SESSION_AUTH_TYPE_MD2
              && auth_type != IPMI_SESSION_AUTH_TYPE_MD5
              && auth_type != IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY)
            {
              errno = EINVAL;
              return (-1);
            }
          
          if (auth_code_data_len > fiid_obj_max_field_len_bytes (obj_hdr, (uint8_t *)auth_field))
            {
              errno = EINVAL;
              return (-1);
            }

          ERR_EXIT (!(fiid_obj_set_data (obj_hdr, 
                                         (uint8_t *)auth_field, 
                                         auth_code_data, 
                                         auth_code_data_len) < 0));
        }     
    }

  return (0);
}

int8_t 
check_hdr_session_session_seq_num (fiid_obj_t obj_hdr_session, uint32_t session_seq_num)
{
  uint64_t session_seq_num_recv;
  int32_t len;
  int8_t rv;

  if (!fiid_obj_valid(obj_hdr_session))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_field_lookup (obj_hdr_session, (uint8_t *)"session_seq_num")) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_field_len (obj_hdr_session, (uint8_t *)"session_seq_num")) < 0)
    return (-1);

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_hdr_session, (uint8_t *)"session_seq_num", &session_seq_num_recv);

  return ((((uint32_t)session_seq_num_recv) == session_seq_num) ? 1 : 0);
}

int8_t 
check_hdr_session_session_id (fiid_obj_t obj_hdr_session, uint32_t session_id)
{
  uint64_t session_id_recv;
  int32_t len;
  int8_t rv;

  if (!fiid_obj_valid(obj_hdr_session))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_field_lookup (obj_hdr_session, (uint8_t *)"session_id")) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_field_len (obj_hdr_session, (uint8_t *)"session_id")) < 0)
    return (-1);

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_hdr_session, (uint8_t *)"session_id", &session_id_recv);

  return ((((uint32_t)session_id_recv) == session_id) ? 1 : 0);
}

int8_t 
check_hdr_session_authcode (uint8_t *pkt, uint64_t pkt_len, fiid_template_t tmpl_hdr_session, uint8_t auth_type, uint8_t *auth_code_data, uint32_t auth_code_data_len)
{
  uint8_t auth_type_recv;
  int32_t rmcp_hdr_len, auth_type_index, auth_code_index;
  uint32_t auth_type_offset, auth_code_offset;
  uint8_t auth_code_buf[IPMI_SESSION_MAX_AUTH_CODE_LEN];
  char *auth_field;
  int8_t rv;

  if (!pkt 
      || (auth_code_data && auth_code_data_len > IPMI_SESSION_MAX_AUTH_CODE_LEN))
    {
      errno = EINVAL;
      return (-1);
    }

  ERR(!((rmcp_hdr_len = fiid_template_len_bytes (tmpl_hdr_rmcp)) < 0));
  ERR(!((auth_type_index = fiid_template_field_start_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_type")) < 0));
  auth_type_offset = rmcp_hdr_len + auth_type_index;

  if (pkt_len < auth_type_offset)
    return 0;

  auth_type_recv = pkt[auth_type_offset];

  /* authcode check fails if authentication types do not match */
  if (auth_type != auth_type_recv)
    return 0;

  /* Automatically return 1 if auth type is none */
  if (auth_type_recv == IPMI_SESSION_AUTH_TYPE_NONE)
    return 1;

  /* Don't use passed in session template, use tmpl_hdr_session */
  ERR(!((auth_code_index = fiid_template_field_start_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_code")) < 0));
  auth_code_offset = rmcp_hdr_len + auth_code_index;

  if (pkt_len < (auth_code_offset + IPMI_SESSION_MAX_AUTH_CODE_LEN))
    return 0;

  memset(auth_code_buf, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN);

  if ((rv = fiid_template_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_code")) < 0)
    return -1;
  
  if (rv)
    auth_field = "auth_code";
  else 
    {
      if ((rv = fiid_template_field_lookup (tmpl_hdr_session, (uint8_t *)"auth_calc_data")) < 0)
	return -1;
    }
  
  if (rv)
    auth_field = "auth_calc_data";
  else
    {
      /* User passed in auth_type != none, but template does not
       * support authentication
       */
      errno = EINVAL;
      return -1;
    }

  if (!strcmp(auth_field, "auth_code"))
    {
      if (auth_code_data)
        memcpy(auth_code_buf, auth_code_data, auth_code_data_len);
    }
  else
    {
      if (auth_type == IPMI_SESSION_AUTH_TYPE_MD2
          || auth_type == IPMI_SESSION_AUTH_TYPE_MD5)
        {
          uint8_t pwbuf[IPMI_SESSION_MAX_AUTH_CODE_LEN];
          int32_t session_id_index, session_seq_num_index, data_index;
	  uint32_t session_id_offset, session_seq_num_offset, data_offset;
	  int32_t session_id_len, session_seq_num_len;
	  
          ERR(!((session_id_index = fiid_template_field_start_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_id")) < 0));
          ERR(!((session_seq_num_index = fiid_template_field_start_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_seq_num")) < 0));
	  ERR(!((session_id_len = fiid_template_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_id")) < 0));
	  ERR(!((session_seq_num_len = fiid_template_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_seq_num")) < 0));
	  ERR(!((data_index = fiid_template_len_bytes (tmpl_hdr_session_auth)) < 0));

	  session_id_offset = rmcp_hdr_len + session_id_index;
	  session_seq_num_offset = rmcp_hdr_len + session_seq_num_index;
	  data_offset = rmcp_hdr_len + data_index;

          if (pkt_len < (session_id_offset + session_id_len)
              || pkt_len < (session_seq_num_offset + session_seq_num_len)
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
				   session_id_len);
              ipmi_md2_update_data(&ctx,
                                   pkt + data_offset,
                                   pkt_len - data_offset);
              ipmi_md2_update_data(&ctx,
                                   pkt + session_seq_num_offset,
				   session_seq_num_len);
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
				   session_id_len);
              ipmi_md5_update_data(&ctx,
                                   pkt + data_offset,
                                   pkt_len - data_offset);
              ipmi_md5_update_data(&ctx,
                                   pkt + session_seq_num_offset,
				   session_seq_num_len);
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
	  errno = EINVAL;
	  return (-1);
	}
    }

  /* Must memcmp instead of strcmp, password need not be 1 word */
  if (memcmp(auth_code_buf, pkt + auth_code_offset, IPMI_SESSION_MAX_AUTH_CODE_LEN) == 0)
    return 1;
  else
    return 0;
}
