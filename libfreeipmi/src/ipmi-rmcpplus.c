/* 
   ipmi-rmcpplus.c - IPMI Session Handler

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

fiid_template_t tmpl_rmcpplus_hdr_session = 
  {
    {4,   "auth_type"},         /* 06h for rmcpplus */
    {4,   "reserved"},
    {6,   "payload_type"},
    {1,   "payload_type.authenticated"},
    {1,   "payload_type.encrypted"},
    {32,  "oem_iana"},          /* only if payload = 02h */
    {16,  "oem_payload_id"},    /* only if payload = 02h */
    {32,  "session_id"},        /* 0h outside of a session */
    {32,  "session_seq_num"},   /* 0h outside of a session, seperate #'s if authenticated or unauthenticated session */
    {16,   "ipmi_payload_len"},     /* apparently the length of just the payload */
    {0,   ""}
  };

/* doesn't exist if session_id = 0h */
fiid_template_t tmpl_rmcpplus_trlr_session = 
  {
    {32,  "integrity_pad"},     /* 0 to 32 bits */
    {8,   "pad_length"},
    {8,   "next_header"},
    {256, "auth_code"},         /* up to 256 bits */
    {32,  "auth_code_len"}, /* WORKAROUND */
    {0,   ""}
  };

/* doesn't exist if session_id = 0h */
fiid_template_t tmpl_rmcpplus_trlr_session_calc = 
  {
    {32,  "integrity_pad"},     /* 0 to 32 bits to pad integrity data to multiple of 4 bytes */
    {8,   "pad_length"},
    {8,   "next_header"},
    {256, "auth_calc_data"},    /* up to 256 bits */
    {32,  "auth_calc_data_len"}, /* WORKAROUND */
    {0,   ""}
  };
/* note: the ipmi spec wording is terrible.  Apparently, the integrity
 * pad is to ensure that the data passed to the HMAC is a multiple of 4,
 * not just the integrity field.  Sigh ... I duno, we'll see
 */

fiid_template_t tmpl_rmcpplus_payload = 
  {
    {512,    "confidentiality_header"},  /* up to 512 bits */
    {32,     "confidentiality_header_len"}, /* WORKAROUND */
    {524288, "payload_data"}, /* 65536 = 2^16 bytes, b/c ipmi_payload_len is 2 bytes */
    {32,     "payload_data_len"}, /* WORKAROUND */
    {512,    "confidentiality_trailer"}, /* up to 512 bits */
    {32,     "confidentiality_trailer_len"}, /* WORKAROUND */
    {0,   ""}
  };

fiid_template_t tmpl_rmcpplus_open_session_rq = 
  {
    {8,   "message_tag"},        
    {4,   "requested_maximum_privilege_level"},
    {4,   "reserved1"},
    {16,  "reserved2"},
    {32,  "remote_console_session_id"}, /* random num */
    {8,   "authentication_payload.payload_type"},
    {16,  "reserved3"},
    {8,   "authentication_payload.payload_length"}, /* 08h ?? */
    {6,   "authentication_payload.authentication_algorithm"}, /* 00h */
    {2,   "reserved4"},
    {24,  "reserved5"},
    {8,   "integrity_payload.payload_type"},
    {16,  "reserved6"},
    {8,   "integrity_payload.payload_length"}, /* 08h ?? */
    {6,   "integrity_payload.integrity_algorithm"}, /* 01h */
    {2,   "reserved7"},
    {24,  "reserved8"},
    {8,   "confidentiality_payload.payload_type"},
    {16,  "reserved9"},
    {8,   "confidentiality_payload.payload_length"}, /* 08h ?? */
    {6,   "confidentiality_payload.confidentiality_algorithm"}, /* 02h */
    {2,   "reserved10"},
    {24,  "reserved11"},
    {0,   ""}
  };

fiid_template_t tmpl_rmcpplus_open_session_rs = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {4,   "maximum_privilege_level"},
    {4,   "reserved1"},
    {8,   "reserved2"},
    {32,  "remote_console_session_id"},
    {32,  "managed_system_session_id"}, /* 0h not valid */
    {8,   "authentication_payload.payload_type"},
    {16,  "reserved3"},
    {8,   "authentication_payload.payload_length"}, /* 08h ?? */
    {6,   "authentication_payload.authentication_algorithm"}, /* 00h */
    {2,   "reserved4"},
    {24,  "reserved5"},
    {8,   "integrity_payload.payload_type"},
    {16,  "reserved6"},
    {8,   "integrity_payload.payload_length"}, /* 08h ?? */
    {6,   "integrity_payload.integrity_algorithm"}, /* 01h */
    {2,   "reserved7"},
    {24,  "reserved8"},
    {8,   "confidentiality_payload.payload_type"},
    {16,  "reserved9"},
    {8,   "confidentiality_payload.payload_length"}, /* 08h ?? */
    {6,   "confidentiality_payload.confidentiality_algorithm"}, /* 02h */
    {2,   "reserved10"},
    {24,  "reserved11"},
    {0,   ""}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_1 = 
  {
    {8,   "message_tag"},
    {24,  "reserved1"},
    {32,  "managed_system_session_id"},
    {128, "remote_console_random_number"},
    {4,   "requested_maximum_privilege_level"},
    {1,   "name_only_lookup"},
    {3,   "reserved2"},
    {16,  "reserved3"},
    {8,   "username_length"},
    {128, "username"},
    {0,   ""}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_2 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "remote_console_session_id"},
    {128, "managed_system_random_number"},
    {128, "managed_system_guid"},
    {512, "key_exchange_authentication_code"}, /* up to 64 bytes */
    {32,  "key_exchange_authentication_code_len"}, /* WORKAROUND */
    {0,   ""}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_3 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "managed_system_session_id"},
    {512, "key_exchange_authentication_code"}, /* up to 64 bytes */
    {32,  "key_exchange_authentication_code_len"}, /* WORKAROUND */
    {0,   ""}
  };

/* achu: The IPMI 2.0 Spec version 1.0 lists the 4th field as
 * "management_console_session_id", * not "managed_system_session_id"
 * or "remote_console_session_id".  I'm assuming this is a typo and
 * that "remote_console_session_id" is what is really meant.
 */
fiid_template_t tmpl_rmcpplus_rakp_message_4 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "remote_console_session_id"},
    {512, "integrity_check_value"}, /* up to 64 bytes */
    {32,  "integrity_check_value_len"}, /* WORKAROUND */
    {0,   ""}
  };


int8_t
fill_rmcpplus_hdr_session (uint8_t payload_type, 
                           uint8_t payload_authenticated, 
                           uint8_t payload_encrypted, 
                           uint32_t oem_iana, 
                           uint16_t oem_payload_id, 
                           uint32_t session_id, 
                           uint32_t session_seq_num, 
                           fiid_obj_t obj_hdr)
{
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID(payload_authenticated)
      || !IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_encrypted)
      || ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && (payload_authenticated || payload_encrypted || session_id || session_seq_num)))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_hdr, '\0', tmpl_rmcpplus_hdr_session);

  FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "auth_type", IPMI_SESSION_AUTH_TYPE_RMCPPLUS);
  FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "payload_type", payload_type);
  FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "payload_type.authenticated", payload_authenticated);
  FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "payload_type.encrypted", payload_encrypted);
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "oem_iana", oem_iana);
      FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "oem_payload_id", oem_payload_id);
    }
  FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "session_id", session_id);
  FIID_OBJ_SET (obj_hdr, tmpl_rmcpplus_hdr_session, "session_seq_num", session_seq_num);

  /* ipmi_payload_len will be calculated during packet assembly */

  return (0);
}

int8_t
fill_rmcpplus_trlr_session(fiid_template_t tmpl_trlr_session,
                           uint8_t *auth_code_data,
                           uint32_t auth_code_data_len,
                           fiid_obj_t obj_trlr)
{
  int32_t field_len;
  char *field_str, *field_str_len;

  if (!tmpl_trlr_session || !obj_trlr)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_trlr, '\0', tmpl_trlr_session);

  /* Unlike fill_hdr_session in IPMI 1.5, we only need to copy data.
   * No checking is required.  The difficult part of computing hashes
   * and checking for correct input is done during the packet
   * assembly.  Padding calculations will also be done during packet
   * assembly.
   */

  FIID_OBJ_SET (obj_trlr, tmpl_trlr_session, "next_header", IPMI_NEXT_HEADER);

  if (auth_code_data && auth_code_data_len > 0)
    {
      if (fiid_obj_field_lookup(tmpl_trlr_session, "auth_code"))
        {
          if (!fiid_obj_field_lookup(tmpl_trlr_session, "auth_code_len"))
            {
              errno = EINVAL;
              return (-1);
            }

          if ((field_len = fiid_obj_field_len_bytes(tmpl_trlr_session, "auth_code")) < 0)
            return (-1);

          field_str = "auth_code";
          field_str_len = "auth_code_len";
        }
      else if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_calc_data"))
        {
          if (!fiid_obj_field_lookup(tmpl_trlr_session, "auth_calc_data_len"))
            {
              errno = EINVAL;
              return (-1);
            }

          if ((field_len = fiid_obj_field_len_bytes(tmpl_trlr_session, "auth_calc_data")) < 0)
            return (-1);         

          field_str = "auth_calc_data";
          field_str_len = "auth_calc_data_len";
        }
      else
        {
          errno = EINVAL;
          return (-1);
        }

      if (auth_code_data_len > field_len)
        {
          errno = EINVAL;
          return (-1);
        }
          
      FIID_OBJ_SET_DATA (obj_trlr,
                         tmpl_trlr_session,
                         field_str,
                         auth_code_data,
                         auth_code_data_len);
      FIID_OBJ_SET (obj_trlr, tmpl_trlr_session, field_str_len, auth_code_data_len);

    }

  return (0);
}

int8_t
fill_rmcpplus_payload(uint8_t *confidentiality_header,
                      uint32_t confidentiality_header_len,
                      uint8_t *payload_data,
                      uint32_t payload_data_len,
                      uint8_t *confidentiality_trailer,
                      uint32_t confidentiality_trailer_len,
                      fiid_obj_t obj_cmd)
{
  if (!obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_cmd, '\0', tmpl_rmcpplus_payload);
  
  if (confidentiality_header)
    {
      int32_t max_len;

      ERR_EXIT (!((max_len = fiid_obj_field_len_bytes(tmpl_rmcpplus_payload, "confidentiality_header")) < 0));

      if (confidentiality_header_len > max_len)
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_SET_DATA(obj_cmd,
                        tmpl_rmcpplus_payload,
                        "confidentiality_header",
                        confidentiality_header,
                        confidentiality_header_len);
    }

  if (payload_data)
    {
      int32_t max_len;

      ERR_EXIT (!((max_len = fiid_obj_field_len_bytes(tmpl_rmcpplus_payload, "payload_data")) < 0));

      if (payload_data_len > max_len)
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_SET_DATA(obj_cmd,
                        tmpl_rmcpplus_payload,
                        "payload_data",
                        payload_data,
                        payload_data_len);
    }

  if (confidentiality_trailer)
    {
      int32_t max_len;

      ERR_EXIT (!((max_len = fiid_obj_field_len_bytes(tmpl_rmcpplus_payload, "confidentiality_trailer")) < 0));

      if (confidentiality_trailer_len > max_len)
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_SET_DATA(obj_cmd,
                        tmpl_rmcpplus_payload,
                        "confidentiality_trailer",
                        confidentiality_trailer,
                        confidentiality_trailer_len);
    }

  return (0);
}
                             
int8_t
fill_rmcpplus_open_session (uint8_t message_tag,
                            uint8_t requested_maximum_privilege_level,
                            uint32_t remote_console_session_id,
                            uint8_t authentication_algorithm,
                            uint8_t integrity_algorithm,
                            uint8_t confidentiality_algorithm,
                            fiid_obj_t obj_cmd)
{
  if (!obj_cmd
      || !IPMI_PRIV_LEVEL_VALID(requested_maximum_privilege_level)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_MEMSET (obj_cmd, '\0', tmpl_rmcpplus_open_session_rq);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_open_session_rq, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_open_session_rq, 
                "requested_maximum_privilege_level", 
                requested_maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq, 
                "remote_console_session_id",
                remote_console_session_id);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq,
                "authentication_payload.payload_type",
                IPMI_AUTHENTICATION_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq,
                "authentication_payload.payload_length",
                IPMI_AUTHENTICATION_PAYLOAD_LEN);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq, 
                "authentication_payload.authentication_algorithm",
                authentication_algorithm);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq,
                "integrity_payload.payload_type",
                IPMI_INTEGRITY_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq,
                "integrity_payload.payload_length",
                IPMI_INTEGRITY_PAYLOAD_LEN);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq, 
                "integrity_payload.integrity_algorithm",
                integrity_algorithm);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq,
                "confidentiality_payload.payload_type",
                IPMI_CONFIDENTIALITY_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq,
                "confidentiality_payload.payload_length",
                IPMI_CONFIDENTIALITY_PAYLOAD_LEN);
  FIID_OBJ_SET (obj_cmd,
                tmpl_rmcpplus_open_session_rq, 
                "confidentiality_payload.confidentiality_algorithm",
                confidentiality_algorithm);

  return (0);
}

int8_t
fill_rmcpplus_rakp_message_1(uint8_t message_tag,
                             uint32_t managed_system_session_id,
                             uint8_t *remote_console_random_number,
                             uint32_t remote_console_random_number_len,
                             uint8_t requested_maximum_privilege_level,
                             uint8_t name_only_lookup_flag,
                             uint8_t *username,
                             uint32_t username_len,
                             fiid_obj_t obj_cmd)
{
  if (!obj_cmd
      || !remote_console_random_number
      || remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LEN
      || !IPMI_PRIV_LEVEL_VALID(requested_maximum_privilege_level)
      || !IPMI_USERNAME_LOOKUP_VALID(name_only_lookup_flag)
      || (username && username_len > IPMI_SESSION_MAX_USERNAME_LEN))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_cmd, '\0', tmpl_rmcpplus_rakp_message_1);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_rakp_message_1, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_rakp_message_1, 
                "managed_system_session_id", 
                managed_system_session_id);
  FIID_OBJ_SET_DATA (obj_cmd,
                     tmpl_rmcpplus_rakp_message_1, 
                     "remote_console_random_number",
                     remote_console_random_number,
                     remote_console_random_number_len);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_rakp_message_1, 
                "requested_maximum_privilege_level", 
                requested_maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_rakp_message_1, 
                "name_only_lookup", 
                name_only_lookup_flag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_rakp_message_1, 
                "username_length", 
                username_len);
  if (username)
    FIID_OBJ_SET_DATA (obj_cmd,
		       tmpl_rmcpplus_rakp_message_1, 
		       "username",
		       username,
		       username_len);
  
  return (0);
}

int8_t
fill_rmcpplus_rakp_message_3(uint8_t message_tag,
                             uint8_t rmcpplus_status_code,
                             uint32_t managed_system_session_id,
                             uint8_t *key_exchange_authentication_code,
                             uint32_t key_exchange_authentication_code_len,
                             fiid_obj_t obj_cmd)
{
  if (!obj_cmd || !RMCPPLUS_STATUS_VALID(rmcpplus_status_code))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_cmd, '\0', tmpl_rmcpplus_rakp_message_3);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_rakp_message_3, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_rmcpplus_rakp_message_3, 
                "managed_system_session_id", 
                managed_system_session_id);

  if (key_exchange_authentication_code && key_exchange_authentication_code_len > 0)
    {
      uint32_t field_len;

      if ((field_len = fiid_obj_field_len_bytes(tmpl_rmcpplus_rakp_message_3, 
                                                "key_exchange_authentication_code")) < 0)
        return (-1);
      
      if (key_exchange_authentication_code_len > field_len)
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_SET_DATA (obj_cmd,
                         tmpl_rmcpplus_rakp_message_3,
                         "key_exchange_authentication_code",
                         key_exchange_authentication_code,
                         key_exchange_authentication_code_len);
      FIID_OBJ_SET (obj_cmd,
                    tmpl_rmcpplus_rakp_message_3,
                    "key_exchange_authentication_code_len",
                    key_exchange_authentication_code_len);
    }
  else
    FIID_OBJ_SET (obj_cmd,
                  tmpl_rmcpplus_rakp_message_3,
                  "key_exchange_authentication_code_len",
                  0);

  return (0);
}
