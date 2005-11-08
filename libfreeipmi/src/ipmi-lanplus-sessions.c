/* 
   ipmi-lanplus-sessions.c - IPMI Session Handler

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

fiid_template_t tmpl_lanplus_hdr_session = 
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
    {16,   "ipmi_msg_len"},     /* len = payload or len = payload + trailer??, I dunno, the spec sucks */
    {0,   ""}
  };

/* doesn't exist if session_id = 0h */
fiid_template_t tmpl_lanplus_trlr_session = 
  {
    {32,  "integrity_pad"},     /* 0 to 32 bits */
    {8,   "pad_length"},
    {8,   "next_header"},
    {256, "auth_code"},         /* up to 256 bits */
    {32,  "auth_code_len"}, /* XXX not in IPMI 2.0 spec */
    {0,   ""}
  };

/* doesn't exist if session_id = 0h */
fiid_template_t tmpl_lanplus_trlr_session_calc = 
  {
    {32,  "integrity_pad"},     /* 0 to 32 bits to pad integrity data to multiple of 4 bytes */
    {8,   "pad_length"},
    {8,   "next_header"},
    {256, "auth_calc_data"},    /* up to 256 bits */
    {32,  "auth_calc_data_len"}, /* XXX not in IPMI 2.0 spec */
    {0,   ""}
  };
/* note: the ipmi spec wording is terrible.  Apparently, the integrity
 * pad is to ensure that the entire ipmi packet is a multiple of 4,
 * not just the integrity field.  Sigh ... I duno, we'll see
 */

fiid_template_t tmpl_lanplus_open_session_rq = 
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

fiid_template_t tmpl_lanplus_open_session_rs = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {4,   "maximum_privilege_level"},
    {4,   "reserved1"},
    {8,   "reserved2"},
    {32,  "remote_console_session_id"},
    {32,  "managed_system_session_id"}, /* 0h not valid */
    {64,  "authentication_payload"},
    {64,  "integrity_payload"},
    {64,  "confidentiality_payload"},
    {0,   ""}
  };

fiid_template_t tmpl_lanplus_rakp_message_1 = 
  {
    {8,   "message_tag"},
    {24,  "reserved1"},
    {32,  "managed_system_session_id"},
    {128, "remote_console_random_number"},
    {4,   "requested_maximum_privilege_level"},
    {1,   "username_lookup"},
    {3,   "reserved2"},
    {16,  "reserved3"},
    {8,   "username_length"},
    {128, "username"},
    {0,   ""}
  };

fiid_template_t tmpl_lanplus_rakp_message_2 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "remote_console_session_id"},
    {128, "managed_system_random_number"},
    {128, "managed_system_guid"},
    {512, "key_exchange_authentication_code"}, /* up to 64 bytes */
    {0,   ""}
  };

fiid_template_t tmpl_lanplus_rakp_message_3 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "managed_system_session_id"},
    {512, "key_exchange_authentication_code"}, /* up to 64 bytes */
    {0,   ""}
  };

fiid_template_t tmpl_lanplus_rakp_message_4 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "management_console_session_id"},
    {512, "integrity_check_value"}, /* up to 64 bytes */
    {0,   ""}
  };

int8_t
fill_lanplus_hdr_session (fiid_template_t tmpl_session, u_int8_t auth_type, u_int8_t payload_type, u_int8_t payload_authenticated, u_int8_t payload_encrypted, u_int32_t oem_iana, u_int16_t oem_payload_id, u_int32_t session_id, u_int32_t session_seq_num, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr)
{
  if (!IPMI_2_0_SESSION_AUTH_TYPE_VALID(auth_type)
      || !IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID(payload_authenticated)
      || !IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_encrypted)
      || !(tmpl_session && tmpl_cmd && obj_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_hdr, '\0', tmpl_session);

  FIID_OBJ_SET (obj_hdr, tmpl_session, "auth_type", auth_type);
  FIID_OBJ_SET (obj_hdr, tmpl_session, "payload_type", payload_type);
  FIID_OBJ_SET (obj_hdr, tmpl_session, "payload_type.authenticated", payload_authenticated);
  FIID_OBJ_SET (obj_hdr, tmpl_session, "payload_type.encrypted", payload_encrypted);
  FIID_OBJ_SET (obj_hdr, tmpl_session, "oem_iana", oem_iana);
  FIID_OBJ_SET (obj_hdr, tmpl_session, "oem_payload_id", oem_payload_id);
  FIID_OBJ_SET (obj_hdr, tmpl_session, "session_id", session_id);
  FIID_OBJ_SET (obj_hdr, tmpl_session, "session_seq_num", session_seq_num);

  /* ipmi_msg_len will be calculated during packet assembly */

  return (0);
}

int8_t
fill_lanplus_trlr_session(fiid_template_t tmpl_trlr,
                          u_int8_t *auth_code_data,
                          u_int32_t auth_code_data_len,
                          fiid_obj_t obj_trlr)
{
  int32_t field_len;
  char *field_str, *field_str_len;

  if (!(tmpl_trlr && obj_trlr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_trlr, '\0', tmpl_trlr);

  /* Unlike fill_hdr_session in IPMI 1.5, we only need to copy data.
   * No checking is required.  The difficult part of computing hashes
   * and checking for correct input is done during the packet
   * assembly.  Padding will calculations will also be done during
   * packet assembly.
   */

  FIID_OBJ_SET (obj_trlr, tmpl_trlr, "next_header", IPMI_NEXT_HEADER);

  if (auth_code_data && auth_code_data_len > 0)
    {
      if (fiid_obj_field_lookup(tmpl_trlr, "auth_code"))
        {
          if (!fiid_obj_field_lookup(tmpl_trlr, "auth_code_len"))
            {
              errno = EINVAL;
              return (-1);
            }

          if ((field_len = fiid_obj_field_len_bytes(tmpl_trlr, "auth_code")) < 0)
            return (-1);

          field_str = "auth_code";
          field_str_len = "auth_code_len";
        }
      else if (fiid_obj_field_lookup (tmpl_trlr, "auth_calc_data"))
        {
          if (!fiid_obj_field_lookup(tmpl_trlr, "auth_calc_data_len"))
            {
              errno = EINVAL;
              return (-1);
            }

          if ((field_len = fiid_obj_field_len_bytes(tmpl_trlr, "auth_calc_data")) < 0)
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
          
      ERR_EXIT (fiid_obj_set_data (obj_trlr,
                                   tmpl_trlr,
                                   field_str,
                                   auth_code_data,
                                   auth_code_data_len) == 0);
      FIID_OBJ_SET (obj_trlr, tmpl_trlr, field_str_len, auth_code_data_len);

    }

  return (0);
}

int8_t
fill_lanplus_open_session (u_int8_t message_tag,
                           u_int8_t requested_maximum_privilege_level,
                           u_int32_t remote_console_session_id,
                           u_int8_t authentication_payload_type,
                           u_int8_t authentication_payload_length,
                           u_int8_t authentication_payload_algorithm,
                           u_int8_t integrity_payload_type,
                           u_int8_t integrity_payload_length,
                           u_int8_t integrity_payload_algorithm,
                           u_int8_t confidentiality_payload_type,
                           u_int8_t confidentiality_payload_length,
                           u_int8_t confidentiality_payload_algorithm,
                           fiid_obj_t obj_cmd)
{
  if (!(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  

  return (0);
}

