/* 
   ipmi-rmcpplus-interface.c - IPMI Session Handler

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

static int32_t
_dump_rmcpplus_hdr_session(int fd, 
                           char *prefix, 
                           char *session_hdr, 
                           uint8_t *pkt, 
                           uint32_t pkt_len,
                           uint64_t *payload_type,
                           uint64_t *payload_authenticated,
                           uint64_t *payload_encrypted,
                           uint64_t *session_id,
                           uint64_t *ipmi_payload_len)
{
  uint32_t session_hdr_len = 0;
  int32_t obj_len, obj_len_1, obj_len_2, obj_len_3, obj_field_start;
  fiid_obj_t obj_rmcpplus_hdr_session_temp;
  fiid_field_t *tmpl_rmcpplus_hdr_session_dump;

  if (!pkt
      || !payload_type
      || !payload_authenticated
      || !payload_encrypted
      || !session_id)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA(obj_rmcpplus_hdr_session_temp, tmpl_rmcpplus_hdr_session);

  /*
   * Extract auth_type and payload information
   */
  ERR_EXIT(!((obj_len = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
  ERR_EXIT(!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "auth_type")) < 0));
  ERR_EXIT(obj_len < IPMI_MAX_PAYLOAD_LEN);
  memcpy (obj_rmcpplus_hdr_session_temp + obj_field_start,
          pkt + session_hdr_len,
          FREEIPMI_MIN ((pkt_len - session_hdr_len), obj_len));
  session_hdr_len += FREEIPMI_MIN ((pkt_len - session_hdr_len), obj_len);

  if (pkt_len <= session_hdr_len)
    goto output;

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "payload_type",
                payload_type);
  
  /*
   * Extract OEM IANA and OEM Payload ID
   */
  if (*payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      ERR_EXIT(!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
      ERR_EXIT(!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_payload_id")) < 0));
      obj_len = obj_len_1 + obj_len_2;
      session_hdr_len += FREEIPMI_MIN ((pkt_len - session_hdr_len), obj_len);

      if (pkt_len <= session_hdr_len)
        goto output;
    }

  /*
   * Extract Session ID, Session Sequence Number, and Payload Length
   */
  ERR_EXIT(!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  ERR_EXIT(!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_seq_num")) < 0));
  ERR_EXIT(!((obj_len_3 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "ipmi_payload_len")) < 0));
  obj_len = obj_len_1 + obj_len_2 + obj_len_3;
  ERR_EXIT(!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  ERR_EXIT(obj_len < IPMI_MAX_PAYLOAD_LEN);

  memcpy (obj_rmcpplus_hdr_session_temp + obj_field_start,
          pkt + session_hdr_len,
          FREEIPMI_MIN ((pkt_len - session_hdr_len), obj_len));
  session_hdr_len += FREEIPMI_MIN ((pkt_len - session_hdr_len), obj_len);

  if (pkt_len <= session_hdr_len)
    goto output;

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "payload_type.authenticated",
                payload_authenticated);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "payload_type.encrypted",
                payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "session_id",
                session_id);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "ipmi_payload_len",
                ipmi_payload_len);

 output:
  if (*payload_type != IPMI_PAYLOAD_TYPE_OEM_EXPLICIT) 
    {
      if (!(tmpl_rmcpplus_hdr_session_dump = fiid_template_make(4,   "auth_type",
                                                                4,   "reserved",
                                                                6,   "payload_type",
                                                                1,   "payload_type.authenticated",
                                                                1,   "payload_type.encrypted",
                                                                32,  "session_id",
                                                                32,  "session_seq_num",
                                                                16,   "ipmi_payload_len")))
								
							       
        return (-1);
    }
  else
    tmpl_rmcpplus_hdr_session_dump = (fiid_field_t *)&tmpl_rmcpplus_hdr_session[0];

  ERR_OUT(fiid_obj_dump_perror (fd, prefix, session_hdr, NULL, pkt, tmpl_rmcpplus_hdr_session_dump) != -1);

  if (*payload_type != IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    fiid_template_free(tmpl_rmcpplus_hdr_session_dump);

  return (session_hdr_len);
}

static int32_t
_dump_rmcpplus_payload_object(int fd,
                              char *prefix,
                              char *payload_hdr,
                              fiid_obj_t obj_payload)
{
  fiid_field_t *tmpl_rmcpplus_payload_dump;
  uint64_t confidentiality_header_len, payload_data_len, confidentiality_trailer_len;
  uint8_t buf[IPMI_MAX_PAYLOAD_LEN];
  int32_t obj_field_start;
  int32_t buf_index = 0;

  if (!obj_payload)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload: Invalid parameters");
      return (-1);
    }
  
  FIID_OBJ_GET(obj_payload,
               tmpl_rmcpplus_payload,
               "confidentiality_header_len",
               &confidentiality_header_len);
  
  FIID_OBJ_GET(obj_payload,
               tmpl_rmcpplus_payload,
               "payload_data_len",
               &payload_data_len);

  FIID_OBJ_GET(obj_payload,
               tmpl_rmcpplus_payload,
               "confidentiality_trailer_len",
               &confidentiality_trailer_len);

  if (confidentiality_header_len && payload_data_len && confidentiality_trailer_len)
    {
      if (!(tmpl_rmcpplus_payload_dump = fiid_template_make((confidentiality_header_len * 8), "confidentiality_header",
                                                            (payload_data_len * 8), "payload_data",
                                                            (confidentiality_trailer_len * 8), "confidentiality_trailer")))
        return (-1);
    }
  else if (payload_data_len)
    {
      if (!(tmpl_rmcpplus_payload_dump = fiid_template_make((payload_data_len * 8), "payload_data")))
        return (-1);
    }
  else
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload: Invalid lengths in payload");
      return (-1);
    }
  
  if (confidentiality_header_len)
    {
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "confidentiality_header")) < 0));
      memcpy (buf + buf_index,
              obj_payload + obj_field_start,
              confidentiality_header_len);
      buf_index += confidentiality_header_len;
    }

  if (payload_data_len)
    {
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "payload_data")) < 0));
      memcpy (buf + buf_index,
              obj_payload + obj_field_start,
              payload_data_len);
      buf_index += payload_data_len;
    }

  if (confidentiality_trailer_len)
    {
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "confidentiality_trailer")) < 0));
      memcpy (buf + buf_index,
              obj_payload + obj_field_start,
              confidentiality_trailer_len);
      buf_index += confidentiality_trailer_len;
    }

  ERR_OUT(fiid_obj_dump_perror (fd, prefix, payload_hdr, NULL, buf, tmpl_rmcpplus_payload_dump) != -1);

  fiid_template_free(tmpl_rmcpplus_payload_dump);

  return (0);
}

static int32_t
_dump_rmcpplus_payload_data(int fd, 
                            char *prefix, 
                            char *msg_hdr,
                            char *cmd_hdr,
                            char *trlr_hdr,
                            fiid_template_t tmpl_msg_hdr,
                            fiid_template_t tmpl_cmd,
                            uint8_t *pkt,
                            uint32_t ipmi_payload_len)
{
  uint8_t buf[IPMI_MAX_PAYLOAD_LEN];
  int32_t pkt_index = 0;

  if (!tmpl_msg_hdr
      || !tmpl_cmd
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_data: Invalid parameters");
      return (-1);
    }
  
  /* Dump message header */

  if ((ipmi_payload_len - pkt_index) < fiid_obj_len_bytes (tmpl_msg_hdr))
    {
      ERR_EXIT((ipmi_payload_len - pkt_index) < IPMI_MAX_PAYLOAD_LEN);
      memset(buf, '\0', IPMI_MAX_PAYLOAD_LEN);
      memcpy(buf, pkt + pkt_index, (ipmi_payload_len - pkt_index));
      ERR_OUT(fiid_obj_dump_perror (fd, prefix, msg_hdr, NULL, buf, tmpl_msg_hdr) != -1);
    }
  else
    ERR_OUT(fiid_obj_dump_perror (fd, prefix, msg_hdr, NULL, pkt + pkt_index, tmpl_msg_hdr) != -1);
  pkt_index += fiid_obj_len_bytes (tmpl_msg_hdr);

  if (ipmi_payload_len <= pkt_index)
    return 0;

  /* Dump command data */

  if ((ipmi_payload_len - pkt_index) < fiid_obj_len_bytes (tmpl_cmd))
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_cmd) < IPMI_MAX_PAYLOAD_LEN);
      memset(buf, '\0', IPMI_MAX_PAYLOAD_LEN);
      memcpy(buf, pkt + pkt_index, (ipmi_payload_len - pkt_index));
      ERR_OUT(fiid_obj_dump_perror (fd, prefix, cmd_hdr, NULL, buf, tmpl_cmd) != -1);
    }
  else
    ERR_OUT(fiid_obj_dump_perror (fd, prefix, cmd_hdr, NULL, pkt + pkt_index, tmpl_cmd) != -1);
  pkt_index += fiid_obj_len_bytes (tmpl_cmd);

  if (ipmi_payload_len <= pkt_index)
    return 0;

  /* Dump trailer */

  if ((ipmi_payload_len - pkt_index) < fiid_obj_len_bytes (tmpl_lan_msg_trlr))
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_lan_msg_trlr) < IPMI_MAX_PAYLOAD_LEN);
      memset(buf, '\0', IPMI_MAX_PAYLOAD_LEN);
      memcpy(buf, pkt + pkt_index, (ipmi_payload_len- pkt_index));
      ERR_OUT(fiid_obj_dump_perror (fd, prefix, trlr_hdr, NULL, buf, tmpl_lan_msg_trlr) != -1);
    }
  else
    ERR_OUT(fiid_obj_dump_perror (fd, prefix, trlr_hdr, NULL, pkt + pkt_index, tmpl_lan_msg_trlr) != -1);
  pkt_index += fiid_obj_len_bytes (tmpl_lan_msg_trlr);

  return (0);
}

static int32_t
_dump_rmcpplus_payload_special(int fd,
                               char *prefix,
                               char *payload_hdr,
                               char *cmd_hdr,
                               uint8_t payload_type,
                               uint8_t authentication_algorithm,
                               fiid_template_t tmpl_msg_hdr,
                               fiid_template_t tmpl_cmd,
                               uint8_t *pkt,
                               uint32_t ipmi_payload_len)
{
  uint8_t buf[IPMI_MAX_PAYLOAD_LEN];
  fiid_obj_t obj_payload;
  int32_t cmd_index = 0;
  int32_t obj_data_len, obj_field_len;
  int32_t pkt_index = 0;
  
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !tmpl_msg_hdr
      || !tmpl_cmd
      || !obj_payload)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_special: Invalid parameters");
      return (-1);
    }

  /* XXX Need template checks? */
  if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
      || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    {
      /* These can all be treated the same b/c there are no variable length fields */
      FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);

      FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

      FIID_OBJ_SET_DATA(obj_payload, 
                        tmpl_rmcpplus_payload,
                        "payload_data",
                        pkt,
                        ipmi_payload_len);

      FIID_OBJ_SET(obj_payload,
                   tmpl_rmcpplus_payload,
                   "payload_data_len",
                   ipmi_payload_len);

      if (_dump_rmcpplus_payload_object(fd,
                                        prefix,
                                        payload_hdr,
                                        obj_payload) < 0)
        return (-1);

      /* There is no msg_hdr or msg_trlr, so we just dump this as the command */
      
      if (ipmi_payload_len < fiid_obj_len_bytes (tmpl_cmd))
        {
          ERR_EXIT(fiid_obj_len_bytes(tmpl_cmd) < IPMI_MAX_PAYLOAD_LEN);
          memset(buf, '\0', IPMI_MAX_PAYLOAD_LEN);
          memcpy(buf, pkt + pkt_index, ipmi_payload_len);
          ERR_OUT(fiid_obj_dump_perror (fd, prefix, cmd_hdr, NULL, buf, tmpl_cmd) != -1);
        }
      else
        ERR_OUT(fiid_obj_dump_perror (fd, prefix, cmd_hdr, NULL, pkt, tmpl_cmd) != -1);

      return (0);
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    {
      fiid_field_t *tmpl_rmcpplus_rakp_message_2_dump;

      if (!fiid_obj_field_lookup (tmpl_cmd, "message_tag")
          || !fiid_obj_field_lookup (tmpl_cmd, "rmcpplus_status_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "remote_console_session_id")
          || !fiid_obj_field_lookup (tmpl_cmd, "managed_system_random_number")
          || !fiid_obj_field_lookup (tmpl_cmd, "managed_system_guid")
          || !fiid_obj_field_lookup (tmpl_cmd, "key_exchange_authentication_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "key_exchange_authentication_code_len"))
        {
          errno = EINVAL;
          return (-1);
        }
      
      obj_field_len = 0;

      FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);

      FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

      FIID_OBJ_SET_DATA(obj_payload, 
                        tmpl_rmcpplus_payload,
                        "payload_data",
                        pkt,
                        ipmi_payload_len);

      FIID_OBJ_SET(obj_payload,
                   tmpl_rmcpplus_payload,
                   "payload_data_len",
                   ipmi_payload_len);

      if (_dump_rmcpplus_payload_object(fd,
                                        prefix,
                                        payload_hdr,
                                        obj_payload) < 0)
        return (-1);

      ERR_EXIT (!((obj_data_len = fiid_obj_field_end_bytes (tmpl_cmd, "managed_system_guid")) < 0));

      if (obj_data_len > ipmi_payload_len)
        obj_data_len = ipmi_payload_len;

      memset (buf, '\0', IPMI_MAX_PAYLOAD_LEN);
      memcpy (buf, pkt, obj_data_len);
      cmd_index += obj_data_len;

      if (ipmi_payload_len <= cmd_index)
        goto output_rakp_message_2;

      /* Whatever is left is the key authentication code */

      obj_field_len = ipmi_payload_len - cmd_index;
      memcpy (buf + cmd_index, pkt + cmd_index, obj_field_len);

    output_rakp_message_2:
      if (!(tmpl_rmcpplus_rakp_message_2_dump = fiid_template_make(8,   "message_tag",
                                                                   8,   "rmcpplus_status_code",
                                                                   16,  "reserved1",
                                                                   32,  "remote_console_session_id",
                                                                   128, "managed_system_random_number",
                                                                   128, "managed_system_guid",
                                                                   (obj_field_len * 8), "key_exchange_authentication_code")))
        return (-1);

      ERR_OUT(fiid_obj_dump_perror (fd, prefix, cmd_hdr, NULL, buf, tmpl_rmcpplus_rakp_message_2_dump) != -1);
      
      fiid_template_free(tmpl_rmcpplus_rakp_message_2_dump);
      return (0);
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      fiid_field_t *tmpl_rmcpplus_rakp_message_3_dump;

      if (!fiid_obj_field_lookup (tmpl_cmd, "message_tag")
          || !fiid_obj_field_lookup (tmpl_cmd, "rmcpplus_status_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "managed_system_session_id")
          || !fiid_obj_field_lookup (tmpl_cmd, "key_exchange_authentication_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "key_exchange_authentication_code_len"))
        {
          errno = EINVAL;
          return (-1);
        }
      
      obj_field_len = 0;

      FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);

      FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

      FIID_OBJ_SET_DATA(obj_payload, 
                        tmpl_rmcpplus_payload,
                        "payload_data",
                        pkt,
                        ipmi_payload_len);

      FIID_OBJ_SET(obj_payload,
                   tmpl_rmcpplus_payload,
                   "payload_data_len",
                   ipmi_payload_len);

      if (_dump_rmcpplus_payload_object(fd,
                                        prefix,
                                        payload_hdr,
                                        obj_payload) < 0)
        return (-1);

      ERR_EXIT (!((obj_data_len = fiid_obj_field_end_bytes (tmpl_cmd, "managed_system_session_id")) < 0));
      
      if (obj_data_len > ipmi_payload_len)
        obj_data_len = ipmi_payload_len;

      memset (buf, '\0', IPMI_MAX_PAYLOAD_LEN);
      memcpy (buf, pkt, obj_data_len);
      cmd_index += obj_data_len;

      if (ipmi_payload_len <= cmd_index)
        goto output_rakp_message_3;

      /* Whatever is left is the key authentication code */

      obj_field_len = ipmi_payload_len - cmd_index;
      memcpy (buf + cmd_index, pkt + cmd_index, obj_field_len);

    output_rakp_message_3:
      if (!(tmpl_rmcpplus_rakp_message_3_dump = fiid_template_make(8,   "message_tag",
                                                                   8,   "rmcpplus_status_code",
                                                                   16,  "reserved1",
                                                                   32, "managed_system_session_id",
                                                                   (obj_field_len * 8), "key_exchange_authentication_code")))
        return (-1);
      
      ERR_OUT(fiid_obj_dump_perror (fd, prefix, cmd_hdr, NULL, buf, tmpl_rmcpplus_rakp_message_3_dump) != -1);
      
      fiid_template_free(tmpl_rmcpplus_rakp_message_3_dump);
      return (0);
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      fiid_field_t *tmpl_rmcpplus_rakp_message_4_dump;

      if (!fiid_obj_field_lookup (tmpl_cmd, "message_tag")
          || !fiid_obj_field_lookup (tmpl_cmd, "rmcpplus_status_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "remote_console_session_id")
          || !fiid_obj_field_lookup (tmpl_cmd, "integrity_check_value")
          || !fiid_obj_field_lookup (tmpl_cmd, "integrity_check_value_len"))
        {
          errno = EINVAL;
          return (-1);
        }
      
      obj_field_len = 0;

      FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);

      FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

      FIID_OBJ_SET_DATA(obj_payload, 
                        tmpl_rmcpplus_payload,
                        "payload_data",
                        pkt,
                        ipmi_payload_len);

      FIID_OBJ_SET(obj_payload,
                   tmpl_rmcpplus_payload,
                   "payload_data_len",
                   ipmi_payload_len);

      if (_dump_rmcpplus_payload_object(fd,
                                        prefix,
                                        payload_hdr,
                                        obj_payload) < 0)
        return (-1);

      ERR_EXIT (!((obj_data_len = fiid_obj_field_end_bytes (tmpl_cmd, "remote_console_session_id")) < 0));
      
      if (obj_data_len > ipmi_payload_len)
        obj_data_len = ipmi_payload_len;

      memset (buf, '\0', IPMI_MAX_PAYLOAD_LEN);
      memcpy (buf, pkt, obj_data_len);
      cmd_index += obj_data_len;

      if (ipmi_payload_len <= cmd_index)
        goto output_rakp_message_4;

      /* Whatever is left is the integrity value */

      obj_field_len = ipmi_payload_len - cmd_index;
      memcpy (buf + cmd_index, pkt + cmd_index, obj_field_len);

    output_rakp_message_4:
      if (!(tmpl_rmcpplus_rakp_message_4_dump = fiid_template_make(8,   "message_tag",
                                                                   8,   "rmcpplus_status_code",
                                                                   16,  "reserved1",
                                                                   32, "remote_console_session_id",
                                                                   (obj_field_len * 8), "integrity_check_value")))
        return (-1);
      
      ERR_OUT(fiid_obj_dump_perror (fd, prefix, cmd_hdr, NULL, buf, tmpl_rmcpplus_rakp_message_4_dump) != -1);
      
      fiid_template_free(tmpl_rmcpplus_rakp_message_4_dump);
      return (0);
    }
  else
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_special: Invalid parameters");
      return (-1);
    }
      
  /* NOT REACHED */
  return (0);
}

static int32_t
_dump_rmcpplus_payload_confidentiality_none(int fd, 
                                            char *prefix, 
                                            char *payload_hdr, 
                                            char *msg_hdr,
                                            char *cmd_hdr,
                                            char *trlr_hdr,
                                            fiid_template_t tmpl_msg_hdr,
                                            fiid_template_t tmpl_cmd,
                                            uint8_t *pkt,
                                            uint32_t ipmi_payload_len)
{
  fiid_obj_t obj_payload;

  if (!tmpl_msg_hdr
      || !tmpl_cmd
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_none: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA(obj_payload, 
                    tmpl_rmcpplus_payload,
                    "payload_data",
                    pkt,
                    ipmi_payload_len);

  FIID_OBJ_SET(obj_payload,
               tmpl_rmcpplus_payload,
               "payload_data_len",
               ipmi_payload_len);

  if (_dump_rmcpplus_payload_object(fd,
                                    prefix,
                                    payload_hdr,
                                    obj_payload) < 0)
    return (-1);

  if (_dump_rmcpplus_payload_data(fd,
                                  prefix,
                                  msg_hdr,
                                  cmd_hdr,
                                  trlr_hdr,
                                  tmpl_msg_hdr,
                                  tmpl_cmd,
                                  pkt,
                                  ipmi_payload_len) < 0)
    return (-1);

  return (0);
}

/* XXX: crypted and uncrypted payload? */
static int32_t
_dump_rmcpplus_payload_confidentiality_aes_cbc_128(int fd,
                                                   char *prefix,
                                                   char *payload_hdr,
                                                   char *msg_hdr,
                                                   char *cmd_hdr,
                                                   char *trlr_hdr,
                                                   fiid_template_t tmpl_msg_hdr,
                                                   fiid_template_t tmpl_cmd,
                                                   uint8_t *confidentiality_key,
                                                   uint32_t confidentiality_key_len,
                                                   uint8_t *pkt,
                                                   int32_t ipmi_payload_len)
{
  uint8_t iv[IPMI_AES_CBC_128_IV_LEN];
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LEN];
  uint8_t pad_len;
  int cipher_keylen, cipher_blocklen;
  int32_t payload_data_len, decrypt_len, cmd_data_len, pkt_index = 0;
  fiid_obj_t obj_payload;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  if (!tmpl_msg_hdr
      || !tmpl_cmd
      || !confidentiality_key
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_aes_cbc_128: Invalid parameters");
      return (-1);
    }
  
  if ((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0)
    return (-1);
  
  ERR_EXIT (!(cipher_keylen < IPMI_AES_CBC_128_KEY_LEN));
  
  if (confidentiality_key_len < IPMI_AES_CBC_128_KEY_LEN)
    {
      errno = EINVAL;
      return (-1);
    }
  confidentiality_key_len = IPMI_AES_CBC_128_KEY_LEN;
  
  if ((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0)
    return (-1);
  
  ERR_EXIT (cipher_blocklen == IPMI_AES_CBC_128_BLOCK_LEN);
  
  if (ipmi_payload_len < IPMI_AES_CBC_128_BLOCK_LEN)
    {
      errno = EINVAL;
      return (-1);
    }

  payload_data_len = ipmi_payload_len - IPMI_AES_CBC_128_BLOCK_LEN;

  if (payload_data_len <= 0)
    {
      errno = EINVAL;
      return (-1);
    }

  memcpy(iv, pkt, IPMI_AES_CBC_128_BLOCK_LEN);
  pkt_index += IPMI_AES_CBC_128_BLOCK_LEN;
  memcpy(payload_buf, pkt + pkt_index, payload_data_len);

  FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA(obj_payload,
                    tmpl_rmcpplus_payload,
                    "confidentiality_header",
                    iv,
                    IPMI_AES_CBC_128_BLOCK_LEN);

  FIID_OBJ_SET(obj_payload,
               tmpl_rmcpplus_payload,
               "confidentiality_header_len",
               IPMI_AES_CBC_128_BLOCK_LEN);

  if ((decrypt_len = ipmi_crypt_cipher_decrypt(IPMI_CRYPT_CIPHER_AES,
                                               IPMI_CRYPT_CIPHER_MODE_CBC,
                                               confidentiality_key,
                                               confidentiality_key_len,
                                               iv,
					       IPMI_AES_CBC_128_BLOCK_LEN,
                                               payload_buf,
                                               payload_data_len)) < 0)
    return (-1);
  
  if (decrypt_len != payload_data_len)
    {
      ipmi_debug("ipmi_crypt_cipher_decrypt: Invalid decryption length");
      return (-1);
    }
  
  pad_len = payload_buf[payload_data_len - 1];
  if (pad_len > IPMI_AES_CBC_128_BLOCK_LEN)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_aes_cbc_128: invalid pad_len");
      return (-1);
    }
  
  cmd_data_len = payload_data_len - pad_len - 1;
  if (cmd_data_len <= 0)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_aes_cbc_128: invalid cmd_data_len");
      return (-1);
    }
  
  FIID_OBJ_SET_DATA(obj_payload,
		    tmpl_rmcpplus_payload,
		    "payload_data",
		    payload_buf,
		    cmd_data_len);

  FIID_OBJ_SET(obj_payload,
	       tmpl_rmcpplus_payload,
	       "payload_data_len",
	       cmd_data_len);

  FIID_OBJ_SET_DATA(obj_payload,
		    tmpl_rmcpplus_payload,
		    "confidentiality_trailer",
		    payload_buf + cmd_data_len,
		    pad_len + 1);

  FIID_OBJ_SET(obj_payload,
	       tmpl_rmcpplus_payload,
	       "confidentiality_trailer_len",
	       pad_len + 1);
  
  if (_dump_rmcpplus_payload_object(fd,
                                    prefix,
                                    payload_hdr,
                                    obj_payload) < 0)
    return (-1);
  
  if (_dump_rmcpplus_payload_data(fd,
                                  prefix,
                                  msg_hdr,
                                  cmd_hdr,
                                  trlr_hdr,
                                  tmpl_msg_hdr,
                                  tmpl_cmd,
                                  payload_buf,
                                  cmd_data_len) < 0)
    return (-1);

  return (0);  
}

static int32_t
_dump_rmcpplus_payload(int fd, 
                       char *prefix, 
                       char *payload_hdr, 
                       char *msg_hdr,
                       char *cmd_hdr,
                       char *trlr_hdr,
                       uint8_t payload_type,
                       uint8_t authentication_algorithm,
                       uint8_t confidentiality_algorithm,
                       fiid_template_t tmpl_msg_hdr, 
                       fiid_template_t tmpl_cmd,
                       uint8_t *confidentiality_key,
                       uint32_t confidentiality_key_len,
                       uint8_t *pkt, 
                       uint32_t ipmi_payload_len)
{
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !ipmi_payload_len
      || !tmpl_msg_hdr
      || !tmpl_cmd
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload: Invalid parameters");
      return (-1);
    }

  /* First determine if this is a special payload_type
   *
   * Note: We don't check consider RAKP1 or RAKP3 special b/c
   * they are requests, not responses
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
      || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      return _dump_rmcpplus_payload_special(fd,
                                            prefix,
                                            payload_hdr,
                                            cmd_hdr,
                                            payload_type,
                                            authentication_algorithm,
                                            tmpl_msg_hdr,
                                            tmpl_cmd,
                                            pkt,
                                            ipmi_payload_len);
    }
  else
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _dump_rmcpplus_payload_confidentiality_none(fd,
                                                           prefix,
                                                           payload_hdr,
                                                           msg_hdr,
                                                           cmd_hdr,
                                                           trlr_hdr,
                                                           tmpl_msg_hdr,
                                                           tmpl_cmd,
                                                           pkt,
                                                           ipmi_payload_len);
      else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
        return _dump_rmcpplus_payload_confidentiality_aes_cbc_128(fd,
                                                                  prefix,
                                                                  payload_hdr,
                                                                  msg_hdr,
                                                                  cmd_hdr,
                                                                  trlr_hdr,
                                                                  tmpl_msg_hdr,
                                                                  tmpl_cmd,
                                                                  confidentiality_key,
                                                                  confidentiality_key_len,
                                                                  pkt,
                                                                  ipmi_payload_len);
      else
        {
          /* achu: Even though the algorithm is legit, we don't support it yet :-( */
          errno = EINVAL;
          return (-1);
        }
    }

  /* NOT REACHED */
  return (-1);
}

static int32_t
_dump_rmcpplus_session_trlr(int fd,
                            char *prefix,
                            char *session_trlr_hdr,
                            uint8_t integrity_algorithm,
                            fiid_template_t tmpl_trlr_session,
                            uint8_t *pkt,
                            uint32_t pkt_len)
{
  int32_t pad_length_field_len, next_header_field_len, pad_length, authcode_len;
  uint8_t buf[IPMI_MAX_PAYLOAD_LEN];
  int32_t pkt_index = 0;
  char *auth_field;
  fiid_field_t *tmpl_rmcpplus_session_trlr_dump;

  if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !tmpl_trlr_session
      || !fiid_obj_field_lookup (tmpl_trlr_session, "integrity_pad")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "pad_length")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "next_header"))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    authcode_len = 0;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    authcode_len = IPMI_HMAC_SHA1_96_AUTHCODE_LEN;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    authcode_len = IPMI_HMAC_MD5_128_AUTHCODE_LEN;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    authcode_len = IPMI_MD5_128_AUTHCODE_LEN;
  else
    {
      /* achu: Even though the algorithm is legit, we don't support it yet :-( */
      errno = EINVAL;
      return (-1);
    }
  
  ERR_EXIT (!((pad_length_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "pad_length")) < 0));
  ERR_EXIT (!((next_header_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "next_header")) < 0));
  
  if (pkt_len < (pad_length_field_len))
    {
      next_header_field_len = 0;
      authcode_len = 0;
    }
  else if (pkt_len < (pad_length_field_len + next_header_field_len))
    authcode_len = 0;
  else if (pkt_len < (authcode_len + pad_length_field_len + next_header_field_len))
    authcode_len = pkt_len - pad_length_field_len - next_header_field_len;
  
  pad_length = pkt_len - pad_length_field_len - next_header_field_len - authcode_len;
  
  if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_code"))
    auth_field = "auth_code";
  else
    auth_field = "auth_calc_data";

  memset(buf, '\0', IPMI_MAX_PAYLOAD_LEN);
  if (pad_length)
    {
      memcpy(buf + pkt_index, pkt + pkt_index, pad_length);
      pkt_index += pad_length;
    }
  
  if (pad_length_field_len)
    {
      memcpy(buf + pkt_index, pkt + pkt_index, pad_length_field_len); 
      pkt_index += pad_length_field_len;
    }

  if (next_header_field_len)
    {
      memcpy(buf + pkt_index, pkt + pkt_index, next_header_field_len);
      pkt_index += next_header_field_len;
    }
  
  if (authcode_len)
    {
      memcpy(buf + pkt_index, pkt + pkt_index, authcode_len);
      pkt_index += authcode_len;
    }
  
  if (pad_length)
    {
      if (!(tmpl_rmcpplus_session_trlr_dump = fiid_template_make((pad_length *8),              "integrity_pad",
                                                                 (pad_length_field_len * 8),   "pad_length",
                                                                 (next_header_field_len * 8),  "next_header",
                                                                 (authcode_len * 8),           auth_field)))
        return (-1);
    }
  else
    {
      if (!(tmpl_rmcpplus_session_trlr_dump = fiid_template_make((pad_length_field_len * 8),   "pad_length",
                                                                 (next_header_field_len * 8),  "next_header",
                                                                 (authcode_len * 8),           auth_field)))
        return (-1);
    }
  
  ERR_OUT(fiid_obj_dump_perror (fd, prefix, session_trlr_hdr, NULL, buf, tmpl_rmcpplus_session_trlr_dump) != -1);

  fiid_template_free(tmpl_rmcpplus_session_trlr_dump);

  return (pkt_index);
}

int32_t
fiid_obj_dump_rmcpplus (int fd, 
                        char *prefix, 
                        char *hdr, 
                        uint8_t authentication_algorithm,
                        uint8_t integrity_algorithm,
                        uint8_t confidentiality_algorithm,
                        uint8_t *integrity_key,
                        uint32_t integrity_key_len,
                        uint8_t *confidentiality_key,
                        uint32_t confidentiality_key_len,
                        uint8_t *pkt, 
                        uint32_t pkt_len, 
                        fiid_template_t tmpl_msg_hdr, 
                        fiid_template_t tmpl_cmd,
                        fiid_template_t tmpl_trlr_session)
{
  uint32_t pkt_index = 0;
  int32_t obj_rmcp_hdr_len, obj_len;
  uint64_t payload_type, payload_authenticated, payload_encrypted, session_id, ipmi_payload_len;
  uint8_t buf[IPMI_MAX_PAYLOAD_LEN];
  char prefix_buf[IPMI_MAX_PAYLOAD_LEN];
  char *rmcp_hdr = 
    "RMCP Header:\n"
    "------------";
  char *session_hdr =
    "IPMI RMCPPLUS Session Header:\n"
    "-----------------------------";
  char *payload_hdr =
    "IPMI RMCPPLUS Payload:\n"
    "----------------------";
  char *msg_hdr =
    "IPMI Message Header:\n"
    "--------------------";
  char *cmd_hdr =
    "IPMI Command Data:\n"
    "------------------";
  char *trlr_hdr =
    "IPMI Trailer:\n"
    "-------------";
  char *session_trlr_hdr = 
    "IPMI RMCPPLUS Session Trailer:\n"
    "------------------------------";
  char *extra_hdr =
    "Unexpected Data:\n"
    "----------------";

  if (!pkt
      || !tmpl_msg_hdr
      || !tmpl_cmd
      || !tmpl_trlr_session)
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_MAX_PAYLOAD_LEN) < 0)
    return (-1);

  /* Dump rmcp header */

  obj_rmcp_hdr_len = fiid_obj_len_bytes (tmpl_hdr_rmcp);
  if ((pkt_len - pkt_index) < obj_len)
    {
      memset(buf, '\0', IPMI_MAX_PAYLOAD_LEN);
      memcpy(buf, pkt + pkt_index, (pkt_len - pkt_index)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, buf, tmpl_hdr_rmcp) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, pkt + pkt_index, tmpl_hdr_rmcp) != -1);
  pkt_index += obj_rmcp_hdr_len;

  if (pkt_len <= pkt_index)
    return 0;
  
  /* Dump rmcpplus session header */

  if ((obj_len = _dump_rmcpplus_hdr_session(fd,
                                            prefix_buf,
                                            session_hdr,
                                            pkt + pkt_index,
                                            pkt_len - pkt_index,
                                            &payload_type,
                                            &payload_authenticated,
                                            &payload_encrypted,
                                            &session_id,
                                            &ipmi_payload_len)) < 0)

    return (-1);
  pkt_index += obj_len;

  if (pkt_len <= pkt_index)
    return 0;

  /* Dump Payload */

  if (_dump_rmcpplus_payload(fd, 
                             prefix_buf, 
                             payload_hdr, 
                             msg_hdr,
                             cmd_hdr,
                             trlr_hdr,
                             payload_type,
                             authentication_algorithm,
                             confidentiality_algorithm,
                             tmpl_msg_hdr, 
                             tmpl_cmd,
                             confidentiality_key,
                             confidentiality_key_len,
                             pkt + pkt_index, 
                             ipmi_payload_len) < 0)
    return (-1);

  pkt_index += ipmi_payload_len;

  if (pkt_len <= pkt_index)
    return 0;

  /* Dump trailer */

  if (session_id && payload_authenticated)
    {
      if ((obj_len = _dump_rmcpplus_session_trlr(fd,
                                                 prefix_buf,
                                                 session_trlr_hdr,
                                                 integrity_algorithm,
                                                 tmpl_trlr_session,
                                                 pkt + pkt_index,
                                                 pkt_len - pkt_index)) < 0)
        return (-1);
      pkt_index += obj_len;

      if (pkt_len <= pkt_index)
        return 0;
    }


  /* Dump extra stuff if packet is longer than expected */
  if ((pkt_len - pkt_index) > 0)
    {
      fiid_field_t *tmpl_extra;

      if (!(tmpl_extra = fiid_template_make((pkt_len - pkt_index) * 8, "extra")))
        return (-1);

      ERR_OUT(fiid_obj_dump_perror(fd, prefix_buf, extra_hdr, NULL, pkt + pkt_index, tmpl_extra) != -1);

      fiid_template_free(tmpl_extra);
    }

  return 0;
}
