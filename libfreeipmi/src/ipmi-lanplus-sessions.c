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

#include <gcrypt.h>

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
    {16,   "ipmi_payload_len"},     /* apparently the length of just the payload */
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
 * pad is to ensure that the data passed to the HMAC is a multiple of 4,
 * not just the integrity field.  Sigh ... I duno, we'll see
 */

fiid_template_t tmpl_lanplus_payload = 
  {
    {512,    "confidentiality_header"},  /* up to 512 bits */
    {32,     "confidentiality_header_len"}, /* XXX not in IPMI 2.0 spec */
    {524288, "payload_data"}, /* 65536 = 2^16 bytes, b/c ipmi_payload_len is 2 bytes */
    {32,     "payload_data_len"}, /* XXX not in IPMI 2.0 spec */
    {512,    "confidentiality_trailer"}, /* up to 512 bits */
    {32,     "confidentiality_trailer_len"}, /* XXX not in IPMI 2.0 spec */
    {0,   ""}
  };

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

fiid_template_t tmpl_lanplus_rakp_message_1 = 
  {
    {8,   "message_tag"},
    {24,  "reserved1"},
    {32,  "managed_system_session_id"},
    {128, "remote_console_random_number"},
    {4,   "requested_maximum_privilege_level"},
    {1,   "nameonly_lookup"},
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
    {32,  "key_exchange_authentication_code_len"}, /* XXX not in IPMI 2.0 spec */
    {0,   ""}
  };

fiid_template_t tmpl_lanplus_rakp_message_3 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "managed_system_session_id"},
    {512, "key_exchange_authentication_code"}, /* up to 64 bytes */
    {32,  "key_exchange_authentication_code_len"}, /* XXX not in IPMI 2.0 spec */
    {0,   ""}
  };

fiid_template_t tmpl_lanplus_rakp_message_4 = 
  {
    {8,   "message_tag"},
    {8,   "rmcpplus_status_code"},
    {16,  "reserved1"},
    {32,  "management_console_session_id"},
    {512, "integrity_check_value"}, /* up to 64 bytes */
    {32,  "integrity_check_value_len"}, /* XXX not in IPMI 2.0 spec */
    {0,   ""}
  };

static int __gcrypt_initialized = 0;

/* 
 * XXX: Check for thread issues in general later on, gcrypt maybe needs
 * some stuff done.
 *
 */
int8_t
__ipmi_init_gcrypt(void)
{
  gcry_error_t e;

  if (__gcrypt_initialized)
    return (0);

  if (!gcry_check_version(GCRYPT_VERSION))
    {
      ipmi_debug("gcry_check_version");
      return (-1);
    }

  /* XXX: We copy digests to insecure memory, so not an issue for now */
  if ((e = gcry_control(GCRYCTL_DISABLE_SECMEM, 0)) != GPG_ERR_NO_ERROR)
    {
      ipmi_debug("gcry_control: %s", gcry_strerror(e));
      return (-1);
    }

  if ((e = gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0)) != GPG_ERR_NO_ERROR)
    {
      ipmi_debug("gcry_control: %s", gcry_strerror(e));
      return (-1);
    }

  __gcrypt_initialized++;
  return (0);
}

int32_t 
ipmi_calculate_sik(u_int8_t authentication_algorithm,
                   u_int8_t *key,
                   u_int32_t key_len,
                   u_int8_t *remote_console_random_number,
                   u_int32_t remote_console_random_number_len,
                   u_int8_t *managed_system_random_number,
                   u_int32_t managed_system_random_number_len,
                   u_int8_t requested_privilege_level,
                   u_int8_t *username,
                   u_int8_t username_len,
                   u_int8_t *sik,
                   u_int32_t sik_len)
{
  /* key can be NULL, indicating a empty key */
  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || (key && !key_len)
      || !remote_console_random_number
      || remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LEN
      || !managed_system_random_number
      || managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LEN
      || !IPMI_PRIV_LEVEL_VALID(requested_privilege_level)
      || (!username && username_len != 0)
      || !sik
      || !sik_len)
    {
      errno = EINVAL;
      return (-1);
    }

  /* XXX: Should I check for a key length? I dunno */

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      /* XXX: achu: Ummm, I don't think there is a SIK?? I'm confused */
      return (0);
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
           || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      gcry_md_hd_t h;
      gcry_error_t e;
      u_int8_t *digestPtr;
      u_int8_t gcry_hash_algorithm;
      u_int32_t digest_len;

      if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
        {
          gcry_hash_algorithm = GCRY_MD_SHA1;
          digest_len = IPMI_HMAC_SHA1_DIGEST_LEN;
        }
      else
        {
          gcry_hash_algorithm = GCRY_MD_MD5;
          digest_len = IPMI_HMAC_MD5_DIGEST_LEN;
        }
      
      if (sik_len < digest_len)
        {
          errno = EINVAL;
          return (-1);
        }

      if (__ipmi_init_gcrypt() < 0)
        return (-1);

      ERR_EXIT (gcry_md_get_algo_dlen(gcry_hash_algorithm) != digest_len);

      /* XXX in secure memory? */
      if ((e = gcry_md_open(&h, gcry_hash_algorithm, GCRY_MD_FLAG_HMAC)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_md_open: %s", gcry_strerror(e));
          return (-1);
        }
      
      if (!h)
        {
          ipmi_debug("gcry_md_open: NULL handle return");
          return (-1);
        }

      if ((e = gcry_md_setkey(h, key, key_len)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_md_setkey: %s", gcry_strerror(e));
          return (-1);
        }

      gcry_md_write(h, (void *)remote_console_random_number, remote_console_random_number_len);
      gcry_md_write(h, (void *)managed_system_random_number, managed_system_random_number_len);
      gcry_md_write(h, (void *)&requested_privilege_level, sizeof(u_int8_t));
      gcry_md_write(h, (void *)&username_len, sizeof(u_int8_t));
      if (username && username_len > 0)
        gcry_md_write(h, (void *)username, username_len);

      gcry_md_final(h);

      if (!(digestPtr = gcry_md_read(h, gcry_hash_algorithm)))
        {
          ipmi_debug("gcry_md_read: NULL data return");
          return (-1);
        }

      memcpy(sik, digestPtr, digest_len);
      gcry_md_close(h);
      return (digest_len);
    }
  else
    {
      /* achu: Even though the algorithm is legit, we don't support it yet :-( */
      errno = EINVAL;
      return (-1);
    }

  /* NOT REACHED */
  return (0);
}

int32_t
__ipmi_calculate_k(u_int8_t authentication_algorithm,
                   u_int8_t *sik_key,
                   u_int32_t sik_key_len,
                   u_int8_t *k,
                   u_int32_t k_len,
                   u_int8_t *constant,
                   u_int32_t constant_len)
{
  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      /* XXX: achu: I think this is right, I'm confused, the spec stinks */
      if (k_len < IPMI_KEY_CONSTANT_LEN
          || constant_len < IPMI_KEY_CONSTANT_LEN)
        {
          errno = EINVAL;
          return (-1);
        }
      
      memcpy(k, constant, IPMI_KEY_CONSTANT_LEN);
      return (IPMI_KEY_CONSTANT_LEN);
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
           || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      gcry_md_hd_t h;
      gcry_error_t e;
      u_int8_t *digestPtr;
      u_int8_t gcry_hash_algorithm;
      u_int32_t digest_len;

      if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
        {
          gcry_hash_algorithm = GCRY_MD_SHA1;
          digest_len = IPMI_HMAC_SHA1_DIGEST_LEN;
        }
      else
        {
          gcry_hash_algorithm = GCRY_MD_MD5;
          digest_len = IPMI_HMAC_MD5_DIGEST_LEN;
        }
      
      if (k_len < digest_len || constant_len < digest_len || sik_key_len < digest_len)
        {
          errno = EINVAL;
          return (-1);
        }

      if (__ipmi_init_gcrypt() < 0)
        return (-1);

      ERR_EXIT (gcry_md_get_algo_dlen(gcry_hash_algorithm) != digest_len);
      
      /* XXX in secure memory? */
      if ((e = gcry_md_open(&h, gcry_hash_algorithm, GCRY_MD_FLAG_HMAC)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_md_open: %s", gcry_strerror(e));
          return (-1);
        }
      
      if (!h)
        {
          ipmi_debug("gcry_md_open: NULL handle return");
          return (-1);
        }

      if ((e = gcry_md_setkey(h, sik_key, digest_len)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_md_setkey: %s", gcry_strerror(e));
          return (-1);
        }

      /* XXX: achu, I believe the constant you pass in is the
       * digest_len, atleast according to 13.32, "constants are
       * constructed using a hexadecimal octet value repeated up to
       * the HMAC block size in length starting with the constant
       * 01h", So I hope that I'm right.
       */
      gcry_md_write(h, (void *)constant, digest_len);

      gcry_md_final(h);

      if (!(digestPtr = gcry_md_read(h, gcry_hash_algorithm)))
        {
          ipmi_debug("gcry_md_read: NULL data return");
          return (-1);
        }

      memcpy(k, digestPtr, digest_len);
      gcry_md_close(h);
      return (digest_len);
    }
  else
    {
      /* achu: Even though the algorithm is legit, we don't support it yet :-( */
      errno = EINVAL;
      return (-1);
    }

  /* NOT REACHED */
  return (0);
}

int32_t
ipmi_calculate_k1(u_int8_t authentication_algorithm,
                  u_int8_t *sik_key,
                  u_int32_t sik_key_len,
                  u_int8_t *k1,
                  u_int32_t k1_len)

{
  u_int8_t constant[IPMI_KEY_CONSTANT_LEN] = { 0x01, 0x01, 0x01, 0x01, 0x01, 
                                               0x01, 0x01, 0x01, 0x01, 0x01, 
                                               0x01, 0x01, 0x01, 0x01, 0x01, 
                                               0x01, 0x01, 0x01, 0x01, 0x01}; 
  
  return __ipmi_calculate_k(authentication_algorithm,
                            sik_key,
                            sik_key_len,
                            k1,
                            k1_len,
                            constant,
                            IPMI_KEY_CONSTANT_LEN);                          
}

int32_t
ipmi_calculate_k2(u_int8_t authentication_algorithm,
                  u_int8_t *sik_key,
                  u_int32_t sik_key_len,
                  u_int8_t *k1,
                  u_int32_t k1_len)

{
  u_int8_t constant[IPMI_KEY_CONSTANT_LEN] = { 0x02, 0x02, 0x02, 0x02, 0x02, 
                                               0x02, 0x02, 0x02, 0x02, 0x02, 
                                               0x02, 0x02, 0x02, 0x02, 0x02, 
                                               0x02, 0x02, 0x02, 0x02, 0x02}; 
  return __ipmi_calculate_k(authentication_algorithm,
                            sik_key,
                            sik_key_len,
                            k1,
                            k1_len,
                            constant,
                            IPMI_KEY_CONSTANT_LEN);  
}

int8_t
fill_lanplus_hdr_session (fiid_template_t tmpl_session, 
                          u_int8_t auth_type, 
                          u_int8_t payload_type, 
                          u_int8_t payload_authenticated, 
                          u_int8_t payload_encrypted, 
                          u_int32_t oem_iana, 
                          u_int16_t oem_payload_id, 
                          u_int32_t session_id, 
                          u_int32_t session_seq_num, 
                          fiid_template_t tmpl_cmd, 
                          fiid_obj_t obj_hdr)
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

  /* ipmi_payload_len will be calculated during packet assembly */

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

  if (!tmpl_trlr || !obj_trlr)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_trlr, '\0', tmpl_trlr);

  /* Unlike fill_hdr_session in IPMI 1.5, we only need to copy data.
   * No checking is required.  The difficult part of computing hashes
   * and checking for correct input is done during the packet
   * assembly.  Padding calculations will also be done during packet
   * assembly.
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
          
      FIID_OBJ_SET_DATA (obj_trlr,
                         tmpl_trlr,
                         field_str,
                         auth_code_data,
                         auth_code_data_len);
      FIID_OBJ_SET (obj_trlr, tmpl_trlr, field_str_len, auth_code_data_len);

    }

  return (0);
}

int8_t
fill_lanplus_open_session (u_int8_t message_tag,
                           u_int8_t requested_maximum_privilege_level,
                           u_int32_t remote_console_session_id,
                           u_int8_t authentication_algorithm,
                           u_int8_t integrity_algorithm,
                           u_int8_t confidentiality_algorithm,
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
  
  FIID_OBJ_MEMSET (obj_cmd, '\0', tmpl_lanplus_open_session_rq);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_open_session_rq, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_open_session_rq, 
                "requested_maximum_privilege_level", 
                requested_maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq, 
                "remote_console_session_id",
                remote_console_session_id);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq,
                "authentication_payload.payload_type",
                IPMI_AUTHENTICATION_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq,
                "authentication_payload.payload_length",
                IPMI_AUTHENTICATION_PAYLOAD_LEN);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq, 
                "authentication_payload.authentication_algorithm",
                authentication_algorithm);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq,
                "integrity_payload.payload_type",
                IPMI_INTEGRITY_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq,
                "integrity_payload.payload_length",
                IPMI_INTEGRITY_PAYLOAD_LEN);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq, 
                "integrity_payload.integrity_algorithm",
                integrity_algorithm);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq,
                "confidentiality_payload.payload_type",
                IPMI_CONFIDENTIALITY_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq,
                "confidentiality_payload.payload_length",
                IPMI_CONFIDENTIALITY_PAYLOAD_LEN);
  FIID_OBJ_SET (obj_cmd,
                tmpl_lanplus_open_session_rq, 
                "confidentiality_payload.confidentiality_algorithm",
                confidentiality_algorithm);

  return (0);
}

int8_t
fill_lanplus_rakp_message_1(u_int8_t message_tag,
                            u_int32_t managed_system_session_id,
                            u_int8_t *remote_console_random_number,
                            u_int32_t remote_console_random_number_len,
                            u_int8_t requested_maximum_privilege_level,
                            u_int8_t nameonly_lookup_flag,
                            u_int8_t *username,
                            u_int32_t username_len,
                            fiid_obj_t obj_cmd)
{
  if (!obj_cmd
      || !remote_console_random_number
      || remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LEN
      || !IPMI_PRIV_LEVEL_VALID(requested_maximum_privilege_level)
      || !IPMI_USERNAME_LOOKUP_VALID(nameonly_lookup_flag)
      || (username && username_len > IPMI_SESSION_MAX_USERNAME_LEN))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_cmd, '\0', tmpl_lanplus_rakp_message_1);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_rakp_message_1, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_rakp_message_1, 
                "managed_system_session_id", 
                managed_system_session_id);
  FIID_OBJ_SET_DATA (obj_cmd,
                     tmpl_lanplus_rakp_message_1, 
                     "remote_console_random_number",
                     remote_console_random_number,
                     remote_console_random_number_len);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_rakp_message_1, 
                "requested_maximum_privilege_level", 
                requested_maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_rakp_message_1, 
                "nameonly_lookup", 
                nameonly_lookup_flag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_rakp_message_1, 
                "username_length", 
                username_len);
  FIID_OBJ_SET_DATA (obj_cmd,
                     tmpl_lanplus_rakp_message_1, 
                     "username",
                     username,
                     username_len);
  
  return (0);
}

int8_t
fill_lanplus_rakp_message_3(u_int8_t message_tag,
                            u_int8_t rmcpplus_status_code,
                            u_int32_t managed_system_session_id,
                            u_int8_t *key_exchange_authentication_code,
                            u_int32_t key_exchange_authentication_code_len,
                            fiid_obj_t obj_cmd)
{
  if (!obj_cmd || !RMCPPLUS_STATUS_VALID(rmcpplus_status_code))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_cmd, '\0', tmpl_lanplus_rakp_message_3);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_rakp_message_3, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd, 
                tmpl_lanplus_rakp_message_3, 
                "managed_system_session_id", 
                managed_system_session_id);

  if (key_exchange_authentication_code && key_exchange_authentication_code_len > 0)
    {
      u_int32_t field_len;

      if ((field_len = fiid_obj_field_len_bytes(tmpl_lanplus_rakp_message_3, 
                                                "key_exchange_authentication_code")) < 0)
        return (-1);
      
      if (key_exchange_authentication_code_len > field_len)
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_SET_DATA (obj_cmd,
                         tmpl_lanplus_rakp_message_3,
                         "key_exchange_authentication_code",
                         key_exchange_authentication_code,
                         key_exchange_authentication_code_len);
      FIID_OBJ_SET (obj_cmd,
                    tmpl_lanplus_rakp_message_3,
                    "key_exchange_authentication_code_len",
                    key_exchange_authentication_code_len);
    }
  else
    FIID_OBJ_SET (obj_cmd,
                  tmpl_lanplus_rakp_message_3,
                  "key_exchange_authentication_code_len",
                  0);

  return (0);
}

static int8_t
_construct_payload(u_int8_t confidentiality_algorithm,
                   fiid_obj_t obj_cmd,
                   fiid_template_t tmpl_cmd,
                   u_int8_t *key,
                   u_int32_t key_len,
                   fiid_obj_t obj_payload)
{
  if (!IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !obj_cmd
      || !tmpl_cmd
      || obj_payload)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_payload, '\0', tmpl_lanplus_payload);

  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    {
      u_int32_t obj_cmd_len;

      FIID_OBJ_SET (obj_payload, 
                    tmpl_lanplus_payload, 
                    "confidentiality_header_len",
                    0);

      obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd);

      FIID_OBJ_SET_DATA (obj_payload,
                         tmpl_lanplus_payload,
                         "payload_data",
                         obj_cmd,
                         obj_cmd_len);
      
      FIID_OBJ_SET (obj_payload,
                    tmpl_lanplus_payload,
                    "payload_data_len",
                    obj_cmd_len);

      FIID_OBJ_SET (obj_payload, 
                    tmpl_lanplus_payload, 
                    "confidentiality_trailer_len",
                    0);
    }
  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
    {
      /* achu: Confidentiality Key for AES_CBS_128 is K2 */
      gcry_cipher_hd_t h;
      gcry_error_t e;
      u_int8_t iv[IPMI_AES_CBC_128_IV_LEN];
      int32_t iv_len;
      u_int8_t obj_cmd_cpy[IPMI_MAX_MSG_LEN];
      u_int32_t obj_cmd_len;
      u_int8_t pad_len;
      size_t cipher_keylen, cipher_blklen;

      if (!key || key_len < IPMI_AES_CBC_128_KEY_LEN)
        {
          errno = EINVAL;
          return (-1);
        }

      if ((e = gcry_cipher_algo_info(GCRY_CIPHER_AES, 
                                     GCRYCTL_GET_KEYLEN,
                                     NULL,
                                     &cipher_keylen)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_algo_info: %s", gcry_strerror(e));
          return (-1);
        }

      ERR_EXIT (cipher_keylen < IPMI_AES_CBC_128_KEY_LEN);

      if ((e = gcry_cipher_algo_info(GCRY_CIPHER_AES, 
                                     GCRYCTL_GET_BLKLEN,
                                     NULL,
                                     &cipher_blklen)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_algo_info: %s", gcry_strerror(e));
          return (-1);
        }

      ERR_EXIT (cipher_blklen != IPMI_AES_CBC_128_BLOCK_LEN);
     
      if ((iv_len = ipmi_get_random((char *)iv, IPMI_AES_CBC_128_IV_LEN)) < 0)
        {
          ipmi_debug("ipmi_get_random: %s", strerror(errno));
          return (-1);
        }

      if (iv_len != IPMI_AES_CBC_128_IV_LEN)
        {
          ipmi_debug("ipmi_get_random: Invalid bytes returned: %d", iv_len);
          return (-1);
        }

      if ((e = gcry_cipher_open(&h, 
                                GCRY_CIPHER_AES,
                                GCRY_CIPHER_MODE_CBC,
                                0) != GPG_ERR_NO_ERROR))
        {
          ipmi_debug("gcry_cipher_open: %s", gcry_strerror(e));
          return (-1);
        }

      if ((e = gcry_cipher_setkey(h, 
                                  (void *)key, 
                                  IPMI_AES_CBC_128_KEY_LEN)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_setkey: %s", gcry_strerror(e));
          return (-1);
        }

      if ((e = gcry_cipher_setiv(h, (void *)iv, IPMI_AES_CBC_128_IV_LEN)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_setiv: %s", gcry_strerror(e));
          return (-1);
        }
  
      ERR_EXIT ((obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd)) > IPMI_MAX_MSG_LEN);
      memcpy(obj_cmd_cpy, obj_cmd, obj_cmd_len);

      /* Pad the data appropriately */

      pad_len = IPMI_AES_CBC_128_BLOCK_LEN - ((obj_cmd_len + 1) % IPMI_AES_CBC_128_BLOCK_LEN);
      
      ERR_EXIT ((obj_cmd_len + pad_len + 1) > IPMI_MAX_MSG_LEN);

      if (pad_len)
        {
          int i;
          for (i = 0; i < pad_len; i++)
            obj_cmd_cpy[obj_cmd_len + i] = i + 1;
          obj_cmd_cpy[obj_cmd_len + pad_len] = pad_len;
        }

      if ((e = gcry_cipher_encrypt(h, 
                                   (void *)obj_cmd_cpy,
                                   obj_cmd_len + pad_len + 1,
                                   NULL,
                                   0)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_encrypt: %s", gcry_strerror(e));
          return (-1);
        }
                                  
      gcry_cipher_close(h);

      FIID_OBJ_SET_DATA (obj_payload,
                         tmpl_lanplus_payload,
                         "confidentiality_header",
                         iv,
                         IPMI_AES_CBC_128_IV_LEN);
      
      FIID_OBJ_SET (obj_payload, 
                    tmpl_lanplus_payload, 
                    "confidentiality_header_len",
                    IPMI_AES_CBC_128_IV_LEN);
      
      FIID_OBJ_SET_DATA (obj_payload,
                         tmpl_lanplus_payload,
                         "payload_data",
                         obj_cmd_cpy,
                         obj_cmd_len);
      
      FIID_OBJ_SET (obj_payload,
                    tmpl_lanplus_payload,
                    "payload_data_len",
                    obj_cmd_len);

      FIID_OBJ_SET_DATA (obj_payload,
                         tmpl_lanplus_payload,
                         "confidentiality_trailer",
                         obj_cmd_cpy + obj_cmd_len,
                         pad_len + 1);
      
      FIID_OBJ_SET (obj_payload, 
                    tmpl_lanplus_payload, 
                    "confidentiality_trailer_len",
                    pad_len + 1);
    }
  else
    {
      /* achu: Even though the algorithm is legit, we don't support it yet :-( */
      errno = EINVAL;
      return (-1);
    }

  /* NOT REACHED */
  return (0);
}

int32_t
assemble_ipmi_lanplus_pkt (u_int8_t authentication_algorithm,
                           u_int8_t integrity_algorithm,
                           u_int8_t confidentiality_algorithm,
                           u_int8_t *integrity_key,
                           u_int32_t integrity_key_len,
                           u_int8_t *confidentiality_key,
                           u_int32_t confidentiality_key_len,
                           fiid_obj_t obj_hdr_rmcp,
                           fiid_obj_t obj_lanplus_hdr_session,
                           fiid_obj_t obj_cmd,
                           fiid_template_t tmpl_cmd,
                           fiid_obj_t obj_lanplus_trlr_session,
                           fiid_template_t tmpl_trlr_session,
                           u_int8_t *pkt,
                           u_int32_t pkt_len)
{
  u_int32_t msg_len = 0;
  int32_t obj_rmcp_hdr_len, obj_len, obj_field_start;
  u_int64_t payload_type, payload_authenticated, payload_encrypted, field_len, session_id;
  int32_t payload_len;
  fiid_obj_t obj_payload = NULL;
  fiid_obj_t obj_hdr_session_temp = NULL;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !obj_hdr_rmcp
      || !obj_lanplus_hdr_session
      || !obj_cmd
      || !tmpl_cmd
      || !pkt
      || !pkt_len)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_GET (obj_lanplus_hdr_session, 
                tmpl_lanplus_hdr_session,
                "payload_type",
                &payload_type);

  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET (obj_lanplus_hdr_session,
                tmpl_lanplus_hdr_session,
                "session_id",
                &session_id);

  FIID_OBJ_GET (obj_lanplus_hdr_session,
                tmpl_lanplus_hdr_session,
                "payload_type.authenticated",
                &payload_authenticated);

  FIID_OBJ_GET (obj_lanplus_hdr_session,
                tmpl_lanplus_hdr_session,
                "payload_type.encrypted",
                &payload_encrypted);

  if ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
       || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
       || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
       || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
       || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
       || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
      && (payload_authenticated || payload_encrypted))
    {
      errno = EINVAL;
      return (-1);
    }

  memset(pkt, '\0', pkt_len);

  msg_len = 0;
  ERR_EXIT (!((obj_rmcp_hdr_len = fiid_obj_len_bytes(tmpl_hdr_rmcp)) < 0));
  if (obj_rmcp_hdr_len > (pkt_len - msg_len))
    {
      errno = ENOSPC;
      return (-1);
    }
  memcpy (pkt, obj_hdr_rmcp, obj_rmcp_hdr_len);
  msg_len += obj_rmcp_hdr_len;
  
  /* Copy over Auth Type and Payload Type
   *
   * Determine length by determining the start of the OEM IANA
   */
  ERR_EXIT (!((obj_len = fiid_obj_field_start_bytes (tmpl_lanplus_hdr_session, "oem_iana")) < 0));
  if (obj_len > (pkt_len - msg_len))
    {
      errno = ENOSPC;
      return (-1);
    }
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_hdr_session, "auth_type")) < 0));
  memcpy (pkt + msg_len, 
          obj_lanplus_hdr_session + obj_field_start,
          obj_len);
  msg_len += obj_len;  

  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      /* Copy over OEM IANA and OEM Payload ID */

      ERR_EXIT (!((obj_len = fiid_obj_field_len_bytes (tmpl_lanplus_hdr_session, "oem_iana")) < 0));
      if (obj_len > (pkt_len - msg_len))
        {
          errno = ENOSPC;
          return (-1);
        }
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_hdr_session, "oem_iana")) < 0));
      memcpy (pkt + msg_len,
              obj_lanplus_hdr_session + obj_field_start,
              obj_len);
      msg_len += obj_len;

      ERR_EXIT (!((obj_len = fiid_obj_field_len_bytes (tmpl_lanplus_hdr_session, "oem_payload_id")) < 0));
      if (obj_len > (pkt_len - msg_len))
        {
          errno = ENOSPC;
          return (-1);
        }
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_hdr_session, "oem_payload_id")) < 0));
      memcpy (pkt + msg_len,
              obj_lanplus_hdr_session + obj_field_start,
              obj_len);
      msg_len += obj_len;
    }

  /* Copy over Session ID and Session Sequence Number */
  ERR_EXIT(!((obj_len = fiid_obj_field_len_bytes (tmpl_lanplus_hdr_session, "session_id")) < 0));
  if (obj_len > (pkt_len - msg_len))
    {
      errno = ENOSPC;
      return (-1);
    }
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_hdr_session, "session_id")) < 0));
  memcpy (pkt + msg_len,
          obj_lanplus_hdr_session + obj_field_start,
          obj_len);
  msg_len += obj_len;

  ERR_EXIT(!((obj_len = fiid_obj_field_len_bytes (tmpl_lanplus_hdr_session, "session_seq_num")) < 0));
  if (obj_len > (pkt_len - msg_len))
    {
      errno = ENOSPC;
      return (-1);
    }
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_hdr_session, "session_seq_num")) < 0));
  memcpy (pkt + msg_len,
          obj_lanplus_hdr_session + obj_field_start,
          obj_len);
  msg_len += obj_len;

  FIID_OBJ_ALLOCA (obj_payload, tmpl_lanplus_payload);

  if ((payload_len = _construct_payload(confidentiality_algorithm,
                                        obj_cmd,
                                        tmpl_cmd,
                                        confidentiality_key,
                                        confidentiality_key_len,
                                        obj_payload)) < 0)
    {
      ipmi_debug("fill_lanplus_payload: %s", strerror(errno));
      return (-1);
    }
  
  /* Set payload length */
  fiid_obj_alloca(obj_hdr_session_temp, tmpl_lanplus_hdr_session);
  if (!obj_hdr_session_temp)
    {
      errno = ENOMEM;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_hdr_session_temp, 
                tmpl_lanplus_hdr_session,
                "ipmi_payload_len",
                payload_len);

  ERR_EXIT (!((obj_len = fiid_obj_field_len_bytes (tmpl_lanplus_hdr_session, "ipmi_payload_len")) < 0));
  if (obj_len > (pkt_len - msg_len))
    {
      errno = ENOSPC;
      return (-1);
    }
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_hdr_session, "ipmi_payload_len")) < 0));
  memcpy (pkt + msg_len,
          obj_hdr_session_temp + obj_field_start,
          obj_len);
  msg_len += obj_len;

  FIID_OBJ_GET (obj_payload,
                tmpl_lanplus_payload,
                "confidentiality_header_len",
                &field_len);
  if (field_len)
    {
      if (field_len > (pkt_len - msg_len))
        {
          errno = ENOSPC;
          return (-1);
        }
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_payload, "confidentiality_header")) < 0));
      memcpy (pkt + msg_len,
              obj_payload + obj_field_start,
              field_len);
      msg_len += field_len;
    }

  FIID_OBJ_GET (obj_payload,
                tmpl_lanplus_payload,
                "payload_len",
                &field_len);
  if (field_len)
    {
      if (field_len > (pkt_len - msg_len))
        {
          errno = ENOSPC;
          return (-1);
        }
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_payload, "payload")) < 0));
      memcpy (pkt + msg_len,
              obj_payload + obj_field_start,
              field_len);
      msg_len += field_len;
    }

  FIID_OBJ_GET (obj_payload,
                tmpl_lanplus_payload,
                "confidentiality_trailer_len",
                &field_len);
  if (field_len)
    {
      if (field_len > (pkt_len - msg_len))
        {
          errno = ENOSPC;
          return (-1);
        }
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_payload, "confidentiality_trailer")) < 0));
      memcpy (pkt + msg_len,
              obj_payload + obj_field_start,
              field_len);
      msg_len += field_len;
    }

  /* no trailer if session_id == 0 or payload unauthenticated 
   *
   * Remember De Morgan's law
   *
   * !(!session_id || !payload_authenticated)
   */
  if (session_id && payload_authenticated)
    {
      u_int64_t obj_auth_len;
      u_int8_t pad_len;
      u_int32_t pad_length_field_len, next_header_field_len;
      char *auth_field;
      char *auth_field_len;
      u_int8_t pad_bytes[IPMI_INTEGRITY_PAD_MULTIPLE] = {IPMI_INTEGRITY_PAD_DATA,
                                                         IPMI_INTEGRITY_PAD_DATA,
                                                         IPMI_INTEGRITY_PAD_DATA,
                                                         IPMI_INTEGRITY_PAD_DATA};

      if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
          || !obj_lanplus_trlr_session 
          || !tmpl_trlr_session)
        {
          errno = EINVAL;
          return (-1);
        }

      if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_code"))
        {
          auth_field = "auth_code";
          auth_field_len = "auth_code_len";
        }
      else if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_calc"))
        {
          auth_field = "auth_calc";
          auth_field_len = "auth_calc_len";
        }
      else
        {
          /* Bad template */
          errno = EINVAL;
          return (-1);
        }

      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE) 
        FIID_OBJ_GET (obj_lanplus_trlr_session,
                      tmpl_lanplus_trlr_session,
                      auth_field_len,
                      &obj_auth_len);
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        obj_auth_len = IPMI_HMAC_SHA1_96_AUTHCODE_LEN;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        obj_auth_len = IPMI_HMAC_MD5_128_AUTHCODE_LEN;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        obj_auth_len = IPMI_MD5_128_AUTHCODE_LEN;
      else
        {
          /* Legit integrity algorithm, but not supported */
          errno = EINVAL;
          return (-1);
        }

      if ((pad_length_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "pad_length")) < 0)
        {
          errno = EINVAL;
          return (-1);
        }
  
      if ((next_header_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "next_header")) < 0)
        {
          errno = EINVAL;
          return (-1);
        }
      
      pad_len = IPMI_INTEGRITY_PAD_MULTIPLE - (((msg_len - obj_rmcp_hdr_len) + pad_length_field_len + next_header_field_len + obj_auth_len) % IPMI_INTEGRITY_PAD_MULTIPLE);
      
      if (pad_len)
        {
          if (pad_len > (pkt_len - msg_len))
            {
              errno = ENOSPC;
              return (-1);
            }
          ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "integrity_pad")) < 0));
          ERR_EXIT (pad_len < IPMI_INTEGRITY_PAD_MULTIPLE);
          memcpy (pkt + msg_len, pad_bytes, pad_len);
          msg_len += pad_len;
        }

      if (pad_length_field_len > (pkt_len - msg_len))
        {
          errno = ENOSPC;
          return (-1);
        }
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "pad_length")) < 0));
      memcpy (pkt + msg_len,
              obj_lanplus_trlr_session + obj_field_start,
              pad_length_field_len);
      msg_len += pad_length_field_len;

      if (next_header_field_len > (pkt_len - msg_len))
        {
          errno = ENOSPC;
          return (-1);
        }
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "next_header")) < 0));
      memcpy (pkt + msg_len,
              obj_lanplus_trlr_session + obj_field_start,
              next_header_field_len);
      msg_len += next_header_field_len;

      if (!strcmp(auth_field, "auth_code"))
        {
          /* XXX: achu: i don't know if this is right, do we copy in
           * just the length of the auth_code, like if the password is
           * "foo", do we just copy in 3 bytes? Or do we copy in 16
           * bytes.  The IPMI spec doesn't say.
           */
          FIID_OBJ_GET (obj_lanplus_trlr_session,
                        tmpl_trlr_session,
                        "auth_code_len",
                        &field_len);
          if (field_len)
            {
              if (field_len > (pkt_len - msg_len))
                {
                  errno = ENOSPC;
                  return (-1);
                }
              ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_payload, "auth_code")) < 0));
              memcpy (pkt + msg_len,
                      obj_lanplus_trlr_session + obj_field_start,
                      field_len);
              msg_len += field_len;
            }
        }
      else
        {
          if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE) 
            {
              /* XXX: achu: i don't know if this is right, do we copy in
               * just the length of the auth_code, like if the password is
               * "foo", do we just copy in 3 bytes? Or do we copy in 16
               * bytes.  The IPMI spec doesn't say.
               */
              if (obj_auth_len)
                {
                  if (obj_auth_len > (pkt_len - msg_len))
                    {
                      errno = ENOSPC;
                      return (-1);
                    }
                  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_lanplus_payload, "auth_calc")) < 0));
                  memcpy (pkt + msg_len,
                          obj_lanplus_trlr_session + obj_field_start,
                          obj_auth_len);
                  msg_len += obj_auth_len;
                }
            }
          else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
                   || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
                   || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
            {
              gcry_md_hd_t h;
              gcry_error_t e;
              u_int8_t *digestPtr;
              u_int8_t gcry_hash_algorithm;
              u_int8_t gcry_flags;
              u_int32_t digest_len;

              if (!integrity_key || !integrity_key_len)
                {
                  errno = EINVAL;
                  return (-1);
                }

              if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
                {
                  gcry_hash_algorithm = GCRY_MD_SHA1;
                  gcry_flags = GCRY_MD_FLAG_HMAC;
                  digest_len = IPMI_HMAC_SHA1_DIGEST_LEN;
                }
              else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
                {
                  gcry_hash_algorithm = GCRY_MD_MD5;
                  gcry_flags = GCRY_MD_FLAG_HMAC;
                  digest_len = IPMI_HMAC_MD5_DIGEST_LEN;
                }
              else
                {
                  gcry_hash_algorithm = GCRY_MD_MD5;
                  gcry_flags = 0;
                  digest_len = IPMI_MD5_DIGEST_LEN;
                }

              if (__ipmi_init_gcrypt() < 0)
                return (-1);

              ERR_EXIT (gcry_md_get_algo_dlen(gcry_hash_algorithm) != digest_len);

              if ((e = gcry_md_open(&h, gcry_hash_algorithm, GCRY_MD_FLAG_HMAC)) != GPG_ERR_NO_ERROR)
                {
                  ipmi_debug("gcry_md_open: %s", gcry_strerror(e));
                  return (-1);
                }

              if (!h)
                {
                  ipmi_debug("gcry_md_open: NULL handle return");
                  return (-1);
                }

              if (gcry_flags & GCRY_MD_FLAG_HMAC)
                {
                  if (integrity_key_len < digest_len)
                    {
                      errno = EINVAL;
                      return (-1);
                    }

                  if ((e = gcry_md_setkey(h, integrity_key, digest_len)) != GPG_ERR_NO_ERROR)
                    {
                      ipmi_debug("gcry_md_setkey: %s", gcry_strerror(e));
                      return (-1);
                    }
                }

              FIID_OBJ_GET (obj_lanplus_trlr_session,
                            tmpl_trlr_session,
                            "auth_calc_len",
                            &field_len);
              if (field_len)
                {
                  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "auth_calc")) < 0));
                  gcry_md_write(h, (void *)(obj_lanplus_trlr_session + obj_field_start), field_len);
                }

              gcry_md_write(h, (void *)(pkt + obj_rmcp_hdr_len), msg_len - obj_rmcp_hdr_len);

              if (field_len)
                gcry_md_write(h, (void *)(obj_lanplus_trlr_session + obj_field_start), field_len);
              
              gcry_md_final(h);

              if (!(digestPtr = gcry_md_read(h, gcry_hash_algorithm)))
                {
                  ipmi_debug("gcry_md_read: NULL data return");
                  return (-1);
                }

              if (digest_len > (pkt_len - msg_len))
                {
                  errno = ENOSPC;
                  return (-1);
                }

              memcpy(pkt + msg_len, 
                     digestPtr, 
                     digest_len);
              msg_len += digest_len;
             
              gcry_md_close(h);
            }
        }
    }

  return (msg_len);
}
