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
    {16,   "ipmi_msg_len"},     /* len = payload + trailer, even though the spec only calls it the "payload length", sigh */
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
    {524288, "payload_data"}, /* 65536 = 2^16 bytes, b/c ipmi_msg_len is 2 bytes */
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
      /* XXX: need a logging mechanism */
      return (-1);
    }

  /* XXX: We copy digests to insecure memory, so not an issue for now */
  if ((e = gcry_control(GCRYCTL_DISABLE_SECMEM, 0)))
    {
      /* XXX: gcry_strerror(e) */
      return (-1);
    }

  if ((e = gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0)))
    {
      /* XXX: gcry_strerror(e) */
      return (-1);
    }

  __gcrypt_initialized++;
  return (0);
}

int32_t 
ipmi_generate_sik(u_int8_t authentication_algorithm,
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
          digest_len = IPMI_RAKP_HMAC_SHA1_DIGEST_LEN;
        }
      else
        {
          gcry_hash_algorithm = GCRY_MD_MD5;
          digest_len = IPMI_RAKP_HMAC_MD5_DIGEST_LEN;
        }
      
      if (sik_len < digest_len)
        {
          errno = EINVAL;
          return (-1);
        }

      if (__ipmi_init_gcrypt() < 0)
        return (-1);

      if (gcry_md_get_algo_dlen(gcry_hash_algorithm) != digest_len)
        {
          /* achu: Oh crap, something is really wrong */
          /* XXX: need a logging mechanism */
          return (-1);
        }

      if ((e = gcry_md_open(&h, gcry_hash_algorithm, GCRY_MD_FLAG_HMAC)))
        {
          /* XXX: gcry_strerror(e) */
          return (-1);
        }
      
      if (!h)
        {
          /* XXX: need a logging mechanism */
          return (-1);
        }

      if ((e = gcry_md_setkey(h, key, key_len)))
        {
          /* XXX: gcry_strerror(e) */
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
          /* XXX: need a logging mechanism */
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
__ipmi_generate_k(u_int8_t authentication_algorithm,
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
          digest_len = IPMI_RAKP_HMAC_SHA1_DIGEST_LEN;
        }
      else
        {
          gcry_hash_algorithm = GCRY_MD_MD5;
          digest_len = IPMI_RAKP_HMAC_MD5_DIGEST_LEN;
        }
      
      if (k_len < digest_len || constant_len < digest_len)
        {
          errno = EINVAL;
          return (-1);
        }

      if (__ipmi_init_gcrypt() < 0)
        return (-1);

      if (gcry_md_get_algo_dlen(gcry_hash_algorithm) != digest_len)
        {
          /* achu: Oh crap, something is really wrong */
          /* XXX: need a logging mechanism */
          return (-1);
        }
      
      if ((e = gcry_md_open(&h, gcry_hash_algorithm, GCRY_MD_FLAG_HMAC)))
        {
          /* XXX: gcry_strerror(e) */
          return (-1);
        }
      
      if (!h)
        {
          /* XXX: need a logging mechanism */
          return (-1);
        }

      if ((e = gcry_md_setkey(h, sik_key, sik_key_len)))
        {
          /* XXX: gcry_strerror(e) */
          return (-1);
        }

      /* XXX: achu, I believe the constant you pass in is the
       * digest_len, atleast according to 13.32, "constants are
       * constructed using a hexadecimal octet value repeated up to
       * teh HMAC block size in length starting with the constant
       * 01h", but this spec is k00ky, who knows.
       */
      gcry_md_write(h, (void *)constant, digest_len);

      gcry_md_final(h);

      if (!(digestPtr = gcry_md_read(h, gcry_hash_algorithm)))
        {
          /* XXX: need a logging mechanism */
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
ipmi_generate_k1(u_int8_t authentication_algorithm,
                 u_int8_t *sik_key,
                 u_int32_t sik_key_len,
                 u_int8_t *k1,
                 u_int32_t k1_len)

{
  u_int8_t constant[IPMI_KEY_CONSTANT_LEN] = { 0x01, 0x01, 0x01, 0x01, 0x01, 
                                               0x01, 0x01, 0x01, 0x01, 0x01, 
                                               0x01, 0x01, 0x01, 0x01, 0x01, 
                                               0x01, 0x01, 0x01, 0x01, 0x01}; 
  
  return __ipmi_generate_k(authentication_algorithm,
                           sik_key,
                           sik_key_len,
                           k1,
                           k1_len,
                           constant,
                           IPMI_KEY_CONSTANT_LEN);                          
}

int32_t
ipmi_generate_k2(u_int8_t authentication_algorithm,
                 u_int8_t *sik_key,
                 u_int32_t sik_key_len,
                 u_int8_t *k1,
                 u_int32_t k1_len)

{
  u_int8_t constant[IPMI_KEY_CONSTANT_LEN] = { 0x02, 0x02, 0x02, 0x02, 0x02, 
                                               0x02, 0x02, 0x02, 0x02, 0x02, 
                                               0x02, 0x02, 0x02, 0x02, 0x02, 
                                               0x02, 0x02, 0x02, 0x02, 0x02}; 
  return __ipmi_generate_k(authentication_algorithm,
                           sik_key,
                           sik_key_len,
                           k1,
                           k1_len,
                           constant,
                           IPMI_KEY_CONSTANT_LEN);  
}

int32_t
ipmi_generate_k3(u_int8_t authentication_algorithm,
                 u_int8_t *sik_key,
                 u_int32_t sik_key_len,
                 u_int8_t *k1,
                 u_int32_t k1_len)

{
  u_int8_t constant[IPMI_KEY_CONSTANT_LEN] = { 0x03, 0x03, 0x03, 0x03, 0x03, 
                                               0x03, 0x03, 0x03, 0x03, 0x03, 
                                               0x03, 0x03, 0x03, 0x03, 0x03, 
                                               0x03, 0x03, 0x03, 0x03, 0x03}; 
  return __ipmi_generate_k(authentication_algorithm,
                           sik_key,
                           sik_key_len,
                           k1,
                           k1_len,
                           constant,
                           IPMI_KEY_CONSTANT_LEN);  
}

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

#if 0
int8_t
fill_lanplus_payload(fiid_obj_t obj_cmd,
                     fiid_template_t tmpl_cmd,
                     u_int8_t confidentiality_algorithm,
                     u_int8_t *confidentiality_key,
                     u_int32_t confidentiality_key_len,
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
      /* achu: Confidentiality Key for AES_CBS_128 is  */
    }
  else
    {
      /* achu: Even though the algorithm is legit, we don't support it yet :-( */
      errno = EINVAL;
      return (-1);
    }

  return (0);
}
#endif /* 0 */

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

#if 0
int32_t
assemble_ipmi_lanplus_pkt (fiid_obj_t obj_hdr_rmcp,
                           fiid_obj_t obj_lanplus_hdr_session,
                           fiid_obj_t obj_lanplus_payload,
                           fiid_obj_t obj_lanplus_trlr_session,
                           u_int8_t *pkt,
                           u_int32_t pkt_len)
{
  if (!obj_hdr_rmcp
      || !obj_lanplus_hdr_session
      || !obj_lanplus_payload
      || !obj_lanplus_trlr_session
      || !pkt
      || !pkt_len)
    {
      errno = EINVAL;
      return -1;
    }

  
  return (0);
}
#endif
