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

#define _BUF_SPACE_CHECK(__copy_in_len, __buf_space_left) \
do { \
   if ((__copy_in_len) > (__buf_space_left)) \
     { \
       errno = ENOSPC; \
       return (-1); \
     } \
}  while (0) 

static int32_t
_construct_payload_buf(fiid_obj_t obj_msg_hdr,
                       u_int8_t *obj_cmd,
                       u_int32_t obj_cmd_len,
                       u_int8_t *payload_buf,
                       u_int32_t payload_buf_len)
{
  int32_t obj_msg_hdr_len, obj_msg_trlr_len, chksum_start_offset, chksum_block_len;
  u_int32_t payload_len;
  ipmi_chksum_t chksum;

  if (!obj_msg_hdr
      || !obj_cmd
      || !obj_cmd_len
      || !payload_buf
      || !payload_buf_len)
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_buf: Invalid parameters");
      return (-1);
    }

  memset(payload_buf, '\0', payload_buf_len);

  ERR_EXIT (!((obj_msg_hdr_len = fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq)) < 0));
  ERR_EXIT (!((obj_msg_trlr_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr)) < 0));

  payload_len = obj_msg_hdr_len + obj_cmd_len + obj_msg_trlr_len;

  ERR_EXIT (!(payload_len > IPMI_MAX_PAYLOAD_LEN));

  if (payload_len > payload_buf_len)
    {
      errno = ENOSPC;
      ipmi_debug("_construct_payload_buf: buffer short");
      return (-1);
    }

  ERR_EXIT (!((chksum_start_offset = fiid_obj_field_end_bytes (tmpl_lan_msg_hdr_rq, "chksum1")) < 0));
  chksum_block_len = obj_msg_hdr_len - chksum_start_offset + obj_cmd_len;
  
  memcpy(payload_buf, obj_msg_hdr, obj_msg_hdr_len);
  memcpy(payload_buf + obj_msg_hdr_len, obj_cmd, obj_cmd_len);

  chksum = ipmi_chksum (payload_buf + chksum_start_offset, chksum_block_len);

  memcpy(payload_buf + obj_msg_hdr_len + obj_cmd_len, &chksum, obj_msg_trlr_len);
  
  return payload_len;
}

static int32_t
_construct_payload_buf_cmd_buf(fiid_obj_t obj_msg_hdr,
                               fiid_obj_t obj_cmd,
                               u_int32_t obj_cmd_len,
                               u_int8_t *payload_buf,
                               u_int32_t payload_buf_len)
{
  return _construct_payload_buf(obj_msg_hdr,
                                (u_int8_t *)obj_cmd,
                                obj_cmd_len,
                                payload_buf,
                                payload_buf_len);
}

static int32_t
_construct_payload_buf_cmd_tmpl(fiid_obj_t obj_msg_hdr,
                                fiid_obj_t obj_cmd,
                                fiid_template_t tmpl_cmd,
                                u_int8_t *payload_buf,
                                u_int32_t payload_buf_len)
{
  int32_t obj_cmd_len;

  if (!tmpl_cmd)
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_buf_cmd_tmpl: Invalid parameters");
      return (-1);
    }

  ERR_EXIT (!((obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd)) < 0));
  
  return _construct_payload_buf(obj_msg_hdr,
                                obj_cmd,
                                obj_cmd_len,
                                payload_buf,
                                payload_buf_len);
                                
}

static int32_t
_construct_payload_special(u_int8_t payload_type,
                           u_int8_t authentication_algorithm,
                           fiid_obj_t obj_msg_hdr,
                           fiid_obj_t obj_cmd,
                           fiid_template_t tmpl_cmd,
                           fiid_obj_t obj_payload)
{
  u_int8_t obj_cmd_buf[IPMI_MAX_PAYLOAD_LEN];
  u_int8_t payload_buf[IPMI_MAX_PAYLOAD_LEN];
  int32_t obj_cmd_len, payload_len;
  u_int64_t obj_field_len;
  int32_t obj_data_len, obj_field_start;

  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !obj_msg_hdr
      || !obj_cmd
      || !tmpl_cmd
      || !obj_payload)
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_special: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_payload, '\0', tmpl_rmcpplus_payload);

  if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      if (!fiid_obj_field_lookup (tmpl_cmd, "message_tag")
          || !fiid_obj_field_lookup (tmpl_cmd, "rmcpplus_status_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "managed_system_session_id")
          || !fiid_obj_field_lookup (tmpl_cmd, "key_exchange_authentication_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "key_exchange_authentication_code_len"))
        {
          errno = EINVAL;
          return (-1);
        }

      obj_cmd_len = 0;

      memset(obj_cmd_buf, '\0', IPMI_MAX_PAYLOAD_LEN);

      ERR_EXIT (!((obj_data_len = fiid_obj_field_end_bytes (tmpl_cmd, "managed_system_session_id")) < 0));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_cmd, "key_exchange_authentication_code")) < 0));
      
      FIID_OBJ_GET (obj_cmd,
                    tmpl_cmd,
                    "key_exchange_authentication_code_len",
                    &obj_field_len);

      obj_cmd_len = obj_data_len + obj_field_len;
      if (obj_cmd_len > IPMI_MAX_PAYLOAD_LEN)
        {
          errno = ENOSPC;
          ipmi_debug("_construct_payload_special: buffer short");
          return (-1);
        }

      memcpy(obj_cmd_buf, obj_cmd, obj_data_len);
      memcpy(obj_cmd_buf + obj_data_len, obj_cmd + obj_field_start, obj_field_len);

      if ((payload_len = _construct_payload_buf_cmd_buf(obj_msg_hdr,
                                                        obj_cmd_buf,
                                                        obj_cmd_len,
                                                        payload_buf,
                                                        IPMI_MAX_PAYLOAD_LEN)) < 0)
        return (-1);
      
      FIID_OBJ_SET (obj_payload, 
                    tmpl_rmcpplus_payload, 
                    "confidentiality_header_len",
                    0);
      
      FIID_OBJ_SET_DATA (obj_payload,
                         tmpl_rmcpplus_payload,
                         "payload_data",
                         payload_buf,
                         payload_len);
      
      FIID_OBJ_SET (obj_payload,
                    tmpl_rmcpplus_payload,
                    "payload_data_len",
                    payload_len);
      
      FIID_OBJ_SET (obj_payload, 
                    tmpl_rmcpplus_payload, 
                    "confidentiality_trailer_len",
                    0);
    }
  else
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_special: Invalid parameters");
      return (-1);
    }

  /* NOT REACHED */
  return (0);
}

static int32_t
_construct_payload_confidentiality_none(fiid_obj_t obj_msg_hdr,
                                        fiid_obj_t obj_cmd,
                                        fiid_template_t tmpl_cmd,
                                        fiid_obj_t obj_payload)
{
  u_int8_t payload_buf[IPMI_MAX_PAYLOAD_LEN];
  int32_t payload_len;

  if (!obj_msg_hdr
      || !obj_cmd
      || !tmpl_cmd
      || !obj_payload)
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_confidentiality_none: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_payload, '\0', tmpl_rmcpplus_payload);

  FIID_OBJ_SET (obj_payload, 
                tmpl_rmcpplus_payload, 
                "confidentiality_header_len",
                0);

  if ((payload_len = _construct_payload_buf_cmd_tmpl(obj_msg_hdr, 
                                                     obj_cmd, 
                                                     tmpl_cmd, 
                                                     payload_buf, 
                                                     IPMI_MAX_PAYLOAD_LEN)) < 0)
    return (-1);

  FIID_OBJ_SET_DATA (obj_payload,
                     tmpl_rmcpplus_payload,
                     "payload_data",
                     payload_buf,
                     payload_len);
  
  FIID_OBJ_SET (obj_payload,
                tmpl_rmcpplus_payload,
                "payload_data_len",
                payload_len);

  FIID_OBJ_SET (obj_payload, 
                tmpl_rmcpplus_payload, 
                "confidentiality_trailer_len",
                0);

  return payload_len;
}

static int32_t
_construct_payload_confidentiality_aes_cbc_128(fiid_obj_t obj_msg_hdr,
                                               fiid_obj_t obj_cmd,
                                               fiid_template_t tmpl_cmd,
                                               u_int8_t *confidentiality_key,
                                               u_int32_t confidentiality_key_len,
                                               fiid_obj_t obj_payload)
{
  u_int8_t iv[IPMI_AES_CBC_128_IV_LEN];
  int32_t iv_len;
  u_int8_t payload_buf[IPMI_MAX_PAYLOAD_LEN];
  u_int8_t pad_len;
  u_int32_t payload_len;
  int cipher_keylen, cipher_blocklen, encrypt_len;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  if (!obj_msg_hdr
      || !obj_cmd
      || !tmpl_cmd
      || !confidentiality_key
      || !obj_payload)
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_confidentiality_aes_cbc_128: Invalid parameters");
      return (-1);
    }

  if ((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0)
    return (-1);

  ERR_EXIT (cipher_keylen < IPMI_AES_CBC_128_KEY_LEN);

  if (confidentiality_key_len < IPMI_AES_CBC_128_KEY_LEN)
    {
      errno = EINVAL;
      return (-1);
    }
  confidentiality_key_len = IPMI_AES_CBC_128_KEY_LEN;

  if ((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0)
    return (-1);
   
  ERR_EXIT (cipher_blocklen == IPMI_AES_CBC_128_BLOCK_LEN);
     
  if ((iv_len = ipmi_get_random((char *)iv, IPMI_AES_CBC_128_IV_LEN)) < 0)
    {
      ipmi_debug("ipmi_get_random: %s", strerror(errno));
      return (-1);
    }
  
  if (iv_len != IPMI_AES_CBC_128_IV_LEN)
    {
      errno = EINVAL;
      ipmi_debug("ipmi_get_random: Invalid bytes returned: %d", iv_len);
      return (-1);
    }
    
  if ((payload_len = _construct_payload_buf_cmd_tmpl(obj_msg_hdr, 
                                                     obj_cmd, 
                                                     tmpl_cmd, 
                                                     payload_buf, 
                                                     IPMI_MAX_PAYLOAD_LEN)) < 0)
    return (-1);

  /* Pad the data appropriately */

  /* +1 is for the pad length field */
  pad_len = IPMI_AES_CBC_128_BLOCK_LEN - ((payload_len + 1) % IPMI_AES_CBC_128_BLOCK_LEN);
      
  ERR_EXIT ((payload_len + pad_len + 1) > IPMI_MAX_PAYLOAD_LEN);
  
  if (pad_len)
    {
      int i;
      for (i = 0; i < pad_len; i++)
        payload_buf[payload_len + i] = i + 1;
      payload_buf[payload_len + pad_len] = pad_len;
    }

  if ((encrypt_len = ipmi_crypt_cipher_encrypt(IPMI_CRYPT_CIPHER_AES,
                                               IPMI_CRYPT_CIPHER_MODE_CBC,
                                               confidentiality_key,
                                               confidentiality_key_len,
                                               iv,
                                               iv_len,
                                               payload_buf,
                                               payload_len + pad_len + 1)) < 0) /* +1 for pad length field */
       return (-1);

  if (encrypt_len != (payload_len + pad_len + 1))
    {
      ipmi_debug("ipmi_crypt_cipher_encrypt: Invalid encryption length");
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_payload, '\0', tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA (obj_payload,
                     tmpl_rmcpplus_payload,
                     "confidentiality_header",
                     iv,
                     IPMI_AES_CBC_128_IV_LEN);
      
  FIID_OBJ_SET (obj_payload, 
                tmpl_rmcpplus_payload, 
                "confidentiality_header_len",
                iv_len);
  
  FIID_OBJ_SET_DATA (obj_payload,
                     tmpl_rmcpplus_payload,
                     "payload_data",
                     payload_buf,
                     payload_len);
      
  FIID_OBJ_SET (obj_payload,
                tmpl_rmcpplus_payload,
                "payload_data_len",
                payload_len);

  FIID_OBJ_SET_DATA (obj_payload,
                     tmpl_rmcpplus_payload,
                     "confidentiality_trailer",
                     payload_buf + payload_len,
                     pad_len + 1);
      
  FIID_OBJ_SET (obj_payload, 
                tmpl_rmcpplus_payload, 
                "confidentiality_trailer_len",
                pad_len + 1);
  
  return (iv_len + payload_len + pad_len + 1);
}

static int32_t
_construct_payload(u_int8_t payload_type,
                   u_int8_t authentication_algorithm,
                   u_int8_t confidentiality_algorithm,
                   fiid_obj_t obj_msg_hdr,
                   fiid_obj_t obj_cmd,
                   fiid_template_t tmpl_cmd,
                   u_int8_t *confidentiality_key,
                   u_int32_t confidentiality_key_len,
                   fiid_obj_t obj_payload)
{
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !obj_msg_hdr
      || !obj_cmd
      || !tmpl_cmd
      || !obj_payload)
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_MEMSET (obj_payload, '\0', tmpl_rmcpplus_payload);

  /* First determine if this is a special payload_type 
   * 
   * Note: We don't check consider RAKP2 or RAKP4 special b/c
   * they are responses, not requests.  RAKP1 is not special b/c 
   * there are no optional or variable length fields.  It can be
   * treated like any other ipmi command.
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      return _construct_payload_special(payload_type,
                                        authentication_algorithm,
                                        obj_msg_hdr,
                                        obj_cmd,
                                        tmpl_cmd,
                                        obj_payload);
    }
  else
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _construct_payload_confidentiality_none(obj_msg_hdr,
                                                       obj_cmd,
                                                       tmpl_cmd,
                                                       obj_payload);
      else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
        return _construct_payload_confidentiality_aes_cbc_128(obj_msg_hdr,
                                                              obj_cmd,
                                                              tmpl_cmd,
                                                              confidentiality_key,
                                                              confidentiality_key_len,
                                                              obj_payload);
      else
        {
          /* achu: Even though the algorithm is legit, we don't support it yet :-( */
          errno = EINVAL;
          return (-1);
        }
    }

  /* NOT REACHED */
  return (0);
}

static int32_t
_calculate_auth_code_len(u_int8_t integrity_algorithm, 
                         fiid_obj_t obj_rmcpplus_trlr_session,
                         fiid_template_t tmpl_trlr_session)
{
  u_int64_t auth_code_len;

  if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !tmpl_trlr_session)
    {
      errno = EINVAL;
      ipmi_debug("_auth_code_len: Invalid parameters");
      return (-1);
    }
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE) 
    {
      char *auth_field_len;

      if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_code"))
        auth_field_len = "auth_code_len";
      else if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_calc"))
        auth_field_len = "auth_calc_len";
      else
        {
          /* Bad template */
          errno = EINVAL;
          return (-1);
        }
      
      FIID_OBJ_GET (obj_rmcpplus_trlr_session,
                    tmpl_rmcpplus_trlr_session,
                    auth_field_len,
                    &auth_code_len);
    }
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    auth_code_len = IPMI_HMAC_SHA1_96_AUTHCODE_LEN;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    auth_code_len = IPMI_HMAC_MD5_128_AUTHCODE_LEN;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    auth_code_len = IPMI_MD5_128_AUTHCODE_LEN;
  else
    {
      /* achu: Even though the algorithm is legit, we don't support it yet :-( */
      errno = EINVAL;
      return (-1);
    }
  
  return ((int32_t)auth_code_len);
}

static int8_t
_construct_trlr_session_pad(u_int8_t integrity_algorithm,
                            u_int32_t ipmi_msg_len,
                            int8_t *pad_len_ptr,
                            int8_t *pad_length_field_len_ptr,
                            int8_t *next_header_field_len_ptr,
                            fiid_obj_t obj_rmcpplus_trlr_session,
                            fiid_template_t tmpl_trlr_session)
{
  int32_t auth_code_len;
  int8_t pad_len, pad_length_field_len, next_header_field_len;
  u_int8_t pad_bytes[IPMI_INTEGRITY_PAD_MULTIPLE] = {IPMI_INTEGRITY_PAD_DATA,
                                                     IPMI_INTEGRITY_PAD_DATA,
                                                     IPMI_INTEGRITY_PAD_DATA,
                                                     IPMI_INTEGRITY_PAD_DATA};

  if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !pad_len_ptr
      || !pad_length_field_len_ptr
      || !next_header_field_len_ptr
      || !obj_rmcpplus_trlr_session
      || !tmpl_trlr_session 
      || !fiid_obj_field_lookup (tmpl_trlr_session, "integrity_pad")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "pad_length")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "next_header"))
    {
      errno = EINVAL;
      ipmi_debug("_construct_trlr_session_pad: Invalid parameters");
      return (-1);
    }

  if ((auth_code_len = _calculate_auth_code_len(integrity_algorithm,
                                                obj_rmcpplus_trlr_session,
                                                tmpl_trlr_session)) < 0)
    return (-1);
  
  ERR_EXIT (!((pad_length_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "pad_length")) < 0)); 
  ERR_EXIT (!((next_header_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "next_header")) < 0));
  
  pad_len = IPMI_INTEGRITY_PAD_MULTIPLE - ((ipmi_msg_len + pad_length_field_len + next_header_field_len + auth_code_len) % IPMI_INTEGRITY_PAD_MULTIPLE);

  ERR_EXIT (!((fiid_obj_memset_field(obj_rmcpplus_trlr_session, '\0', tmpl_trlr_session, "integrity_pad")) < 0));

  if (pad_len)
    FIID_OBJ_SET_DATA (obj_rmcpplus_trlr_session,
                       tmpl_trlr_session,
                       "integrity_pad",
                       pad_bytes,
                       pad_len);
  
  FIID_OBJ_SET (obj_rmcpplus_trlr_session,
                tmpl_trlr_session,
                "pad_length",
                pad_len);
  
  *pad_len_ptr = pad_len;
  *pad_length_field_len_ptr = pad_length_field_len;
  *next_header_field_len_ptr = next_header_field_len;

  return (0);
}

static int32_t
_construct_trlr_session_auth_code(u_int8_t integrity_algorithm,
                                  u_int8_t *integrity_key,
                                  u_int32_t integrity_key_len,
                                  fiid_obj_t obj_rmcpplus_trlr_session,
                                  fiid_template_t tmpl_trlr_session,
                                  u_int8_t *pkt_data,
                                  u_int32_t pkt_data_len,
                                  u_int8_t *auth_code_buf,
                                  u_int32_t auth_code_buf_len)
{
  int32_t obj_field_start, auth_code_len;
  u_int64_t obj_field_len;

  if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !obj_rmcpplus_trlr_session
      || !tmpl_trlr_session 
      || !auth_code_buf
      || !auth_code_buf_len
      || !fiid_obj_field_lookup (tmpl_trlr_session, "integrity_pad")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "pad_length")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "next_header"))
    {
      errno = EINVAL;
      ipmi_debug("_construct_trlr_session_auth_code: Invalid parameters");
      return -1;
    }

  if ((auth_code_len = _calculate_auth_code_len(integrity_algorithm,
                                                obj_rmcpplus_trlr_session,
                                                tmpl_trlr_session)) < 0)
    return (-1);

  memset(auth_code_buf, '\0', auth_code_buf_len);

  if (fiid_obj_field_lookup(tmpl_trlr_session, "auth_code"))
    {
      FIID_OBJ_GET (obj_rmcpplus_trlr_session,
                    tmpl_trlr_session,
                    "auth_code_len",
                    &obj_field_len);

      if (auth_code_len > obj_field_len)
        {
          errno = EINVAL;
          return (-1);
        }

      if (obj_field_len > auth_code_buf_len)
        {
          errno = ENOSPC;
          ipmi_debug("_construct_trlr_session_auth_code: buffer short");
          return (-1);
        }
     
      if (obj_field_len)
        {
          ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "auth_code")) < 0));
          memcpy(auth_code_buf, obj_rmcpplus_trlr_session + obj_field_start, auth_code_len);
        }

      return (auth_code_len);
    }
  else
    {
      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE) 
        {
          /* XXX: achu: Do we zero pad the auth_code?  The spec doesn't
           * say.
           */
          if (auth_code_len)
            {
              ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "auth_calc")) < 0));
              memcpy (auth_code_buf, obj_rmcpplus_trlr_session + obj_field_start, auth_code_len);
            }
          
          return (auth_code_len);
        }
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        {
          int hash_algorithm, hash_flags, crypt_digest_len;
          unsigned int expected_digest_len, hash_data_len, integrity_digest_len;
          u_int8_t hash_data[IPMI_MAX_PAYLOAD_LEN];
          u_int64_t auth_calc_len = 0;
          u_int32_t auth_calc_field_start = 0;
          u_int8_t integrity_digest[IPMI_MAX_PAYLOAD_LEN];
          
          /* Note: Integrity Key for HMAC_SHA1_95 and HMAC_MD5_128 is K1 */

          if (!pkt_data || !pkt_data_len)
            {
              errno = EINVAL;
              return (-1);
            }

          if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
            {
              hash_algorithm = IPMI_CRYPT_HASH_SHA1;
              hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
              expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LEN;
            }
          else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
            {
              hash_algorithm = IPMI_CRYPT_HASH_MD5;
              hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
              expected_digest_len = IPMI_HMAC_MD5_DIGEST_LEN;
            }
          else
            {
              hash_algorithm = IPMI_CRYPT_HASH_MD5;
              hash_flags = 0;
              expected_digest_len = IPMI_MD5_DIGEST_LEN;
            }
          
          if ((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0)
            return (-1);

          ERR_EXIT (crypt_digest_len == expected_digest_len);
         
          if ((hash_flags & IPMI_CRYPT_HASH_FLAGS_HMAC) && (integrity_key_len < crypt_digest_len))
            {
              errno = EINVAL;
              return (-1);
            }

          if (integrity_key_len > crypt_digest_len)
            integrity_key_len = crypt_digest_len;

          memset(hash_data, '\0', IPMI_MAX_PAYLOAD_LEN);
          
          hash_data_len = 0;

          if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
            {
              /* XXX: achu: Do we zero pad?  I don't know. */
              FIID_OBJ_GET (obj_rmcpplus_trlr_session,
                            tmpl_trlr_session,
                            "auth_calc_len",
                            &auth_calc_len);

              if (auth_calc_len)
                {
                  ERR_EXIT (!((auth_calc_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "auth_calc")) < 0));
                  memcpy(hash_data + hash_data_len, 
                         (void *)(obj_rmcpplus_trlr_session + auth_calc_field_start), 
                         auth_calc_len);
                  hash_data_len += auth_calc_len;
                }
            }
          
          memcpy(hash_data + hash_data_len, pkt_data, pkt_data_len);
          hash_data_len += pkt_data_len;
          
          if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128 && auth_calc_len)
            {
              /* XXX: achu: Do we zero pad?  I don't know. */
              memcpy(hash_data + hash_data_len, 
                     (void *)(obj_rmcpplus_trlr_session + auth_calc_field_start), 
                     auth_calc_len);
              hash_data_len += auth_calc_len;
            }
          
          if ((integrity_digest_len = ipmi_crypt_hash(hash_algorithm,
                                                      hash_flags,
                                                      integrity_key,
                                                      integrity_key_len,
                                                      hash_data,
                                                      hash_data_len,
                                                      integrity_digest,
                                                      IPMI_MAX_PAYLOAD_LEN)) < 0)
            {
              ipmi_debug("ipmi_crypt_hash: %s", strerror(errno));
              return (-1);
            }
          
          if (integrity_digest_len != crypt_digest_len)
            {
              ipmi_debug("ipmi_crypt_hash: invalid digest length returned");
              return (-1);
            }

          if (integrity_digest_len > auth_code_buf_len)
            {
              errno = ENOSPC;
              ipmi_debug("_construct_trlr_session_auth_code: buffer short");
              return (-1);
            }

          memcpy(auth_code_buf, integrity_digest, integrity_digest_len);
          
          return (integrity_digest_len);
        }
    }

  /* NOT REACHED */
  return (0);
}

int32_t
assemble_ipmi_rmcpplus_pkt (u_int8_t authentication_algorithm,
                            u_int8_t integrity_algorithm,
                            u_int8_t confidentiality_algorithm,
                            u_int8_t *integrity_key,
                            u_int32_t integrity_key_len,
                            u_int8_t *confidentiality_key,
                            u_int32_t confidentiality_key_len,
                            fiid_obj_t obj_hdr_rmcp,
                            fiid_obj_t obj_rmcpplus_hdr_session,
                            fiid_obj_t obj_msg_hdr,
                            fiid_obj_t obj_cmd,
                            fiid_template_t tmpl_cmd,
                            fiid_obj_t obj_rmcpplus_trlr_session,
                            fiid_template_t tmpl_trlr_session,
                            u_int8_t *pkt,
                            u_int32_t pkt_len)
{
  unsigned int pkt_msg_len = 0;
  int32_t obj_rmcp_hdr_len, obj_len, obj_len_1, obj_len_2, obj_field_start;
  u_int64_t obj_field_len, payload_type, payload_authenticated, payload_encrypted, session_id, session_seq_num;
  int32_t payload_len;
  fiid_obj_t obj_payload = NULL;
  fiid_obj_t obj_hdr_session_temp = NULL;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !obj_hdr_rmcp
      || !obj_rmcpplus_hdr_session
      || !obj_msg_hdr
      || !obj_cmd
      || !tmpl_cmd
      || !pkt
      || !pkt_len)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_GET (obj_rmcpplus_hdr_session, 
                tmpl_rmcpplus_hdr_session,
                "payload_type",
                &payload_type);

  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "payload_type.authenticated",
                &payload_authenticated);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "payload_type.encrypted",
                &payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "session_id",
                &session_id);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "session_seq_num",
                &session_seq_num);

  if (((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
        || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
       && (payload_authenticated || payload_encrypted || session_id || session_seq_num))
      || (session_id 
          && payload_authenticated
          && (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
              || !obj_rmcpplus_trlr_session 
              || !tmpl_trlr_session)))
    {
      errno = EINVAL;
      return (-1);
    }

  memset(pkt, '\0', pkt_len);

  /* 
   * Copy RMCP header into packet
   */
  pkt_msg_len = 0;
  ERR_EXIT (!((obj_rmcp_hdr_len = fiid_obj_len_bytes(tmpl_hdr_rmcp)) < 0));
  _BUF_SPACE_CHECK(obj_rmcp_hdr_len, (pkt_len - pkt_msg_len));
  memcpy (pkt, obj_hdr_rmcp, obj_rmcp_hdr_len);
  pkt_msg_len += obj_rmcp_hdr_len;
  
  /* 
   * Copy Auth Type and Payload Type into packet
   *
   * Determine length by determining the start of the OEM IANA
   */
  ERR_EXIT (!((obj_len = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
  _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "auth_type")) < 0));
  memcpy (pkt + pkt_msg_len, 
          obj_rmcpplus_hdr_session + obj_field_start,
          obj_len);
  pkt_msg_len += obj_len;  

  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      /* 
       * Copy OEM IANA and OEM Payload ID into packet
       */

      ERR_EXIT (!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
      ERR_EXIT (!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_payload_id")) < 0));
      obj_len = obj_len_1 + obj_len_2;
      _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_rmcpplus_hdr_session + obj_field_start,
              obj_len);
      pkt_msg_len += obj_len;
    }

  /* 
   * Copy Session ID and Session Sequence Number into packet 
   */
  ERR_EXIT(!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  ERR_EXIT(!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_seq_num")) < 0));
  obj_len = obj_len_1 + obj_len_2;
  _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  memcpy (pkt + pkt_msg_len,
          obj_rmcpplus_hdr_session + obj_field_start,
          obj_len);
  pkt_msg_len += obj_len;

  /* 
   * Construct/Encrypt Payload and copy into packet
   */
  FIID_OBJ_ALLOCA (obj_payload, tmpl_rmcpplus_payload);

  if ((payload_len = _construct_payload(payload_type,
                                        authentication_algorithm,
                                        confidentiality_algorithm,
                                        obj_msg_hdr,
                                        obj_cmd,
                                        tmpl_cmd,
                                        confidentiality_key,
                                        confidentiality_key_len,
                                        obj_payload)) < 0)
    {
      ipmi_debug("_construct_payload: %s", strerror(errno));
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_hdr_session_temp, tmpl_rmcpplus_hdr_session);
  if (!obj_hdr_session_temp)
    {
      errno = ENOMEM;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_hdr_session_temp, 
                tmpl_rmcpplus_hdr_session,
                "ipmi_payload_len",
                payload_len);
  
  ERR_EXIT (!((obj_len = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "ipmi_payload_len")) < 0));
  _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "ipmi_payload_len")) < 0));
  memcpy (pkt + pkt_msg_len,
          obj_hdr_session_temp + obj_field_start,
          obj_len);
  pkt_msg_len += obj_len;

  FIID_OBJ_GET (obj_payload,
                tmpl_rmcpplus_payload,
                "confidentiality_header_len",
                &obj_field_len);
  if (obj_field_len)
    {
      _BUF_SPACE_CHECK(obj_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "confidentiality_header")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_payload + obj_field_start,
              obj_field_len);
      pkt_msg_len += obj_field_len;
    }

  FIID_OBJ_GET (obj_payload,
                tmpl_rmcpplus_payload,
                "payload_len",
                &obj_field_len);
  if (obj_field_len)
    {
      _BUF_SPACE_CHECK(obj_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "payload")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_payload + obj_field_start,
              obj_field_len);
      pkt_msg_len += obj_field_len;
    }

  FIID_OBJ_GET (obj_payload,
                tmpl_rmcpplus_payload,
                "confidentiality_trailer_len",
                &obj_field_len);
  if (obj_field_len)
    {
      _BUF_SPACE_CHECK(obj_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "confidentiality_trailer")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_payload + obj_field_start,
              obj_field_len);
      pkt_msg_len += obj_field_len;
    }

  if (session_id && payload_authenticated)
    {
      int32_t rmcpplus_trlr_session_len;
      u_int8_t pad_len, pad_length_field_len, next_header_field_len;
      fiid_obj_t obj_rmcpplus_trlr_session_temp;
      u_int8_t auth_code_buf[IPMI_MAX_PAYLOAD_LEN];
      int32_t auth_code_len;

      if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
          || !fiid_obj_field_lookup (tmpl_trlr_session, "integrity_pad")
          || !fiid_obj_field_lookup (tmpl_trlr_session, "pad_length")
          || !fiid_obj_field_lookup (tmpl_trlr_session, "next_header"))
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_ALLOCA (obj_rmcpplus_trlr_session_temp, tmpl_trlr_session);
      ERR_EXIT (!((rmcpplus_trlr_session_len = fiid_obj_len_bytes (tmpl_trlr_session)) < 0));
      memcpy(obj_rmcpplus_trlr_session_temp, obj_rmcpplus_trlr_session, rmcpplus_trlr_session_len);

      if (_construct_trlr_session_pad(integrity_algorithm,
                                      (pkt_msg_len - obj_rmcp_hdr_len),
                                      &pad_len,
                                      &pad_length_field_len,
                                      &next_header_field_len,
                                      obj_rmcpplus_trlr_session_temp,
                                      tmpl_trlr_session) < 0)
        return (-1);

      /* 
       * Copy pad into packet
       */
      if (pad_len)
        {
          _BUF_SPACE_CHECK(pad_len, (pkt_len - pkt_msg_len));
          ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "integrity_pad")) < 0));
          ERR_EXIT (pad_len < IPMI_INTEGRITY_PAD_MULTIPLE);
          memcpy (pkt + pkt_msg_len, obj_rmcpplus_trlr_session_temp + obj_field_start, pad_len);
          pkt_msg_len += pad_len;
        }

      /* 
       * Copy pad length into packet
       */
      _BUF_SPACE_CHECK(pad_length_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "pad_length")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_rmcpplus_trlr_session + obj_field_start,
              pad_length_field_len);
      pkt_msg_len += pad_length_field_len;

      /* 
       * Copy next header field into packet
       */
      _BUF_SPACE_CHECK(next_header_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "next_header")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_rmcpplus_trlr_session + obj_field_start,
              next_header_field_len);
      pkt_msg_len += next_header_field_len;

      /* achu: Note that the integrity code is all data prior to the authcode, so this 
       * call must be done after the pad, pad length, and next header are copied into 
       * the pkt buffer.
       */
      if ((auth_code_len = _construct_trlr_session_auth_code(integrity_algorithm,
                                                             integrity_key,
                                                             integrity_key_len,
                                                             obj_rmcpplus_trlr_session,
                                                             tmpl_trlr_session,
                                                             pkt + obj_rmcp_hdr_len,
                                                             pkt_msg_len - obj_rmcp_hdr_len,
                                                             auth_code_buf,
                                                             IPMI_MAX_PAYLOAD_LEN)) < 0)
        return (-1);

      if (auth_code_len)
        {
          _BUF_SPACE_CHECK(auth_code_len, (pkt_len - pkt_msg_len));
          memcpy(pkt + pkt_msg_len, auth_code_buf, auth_code_len);
          pkt_msg_len += auth_code_len;
        }
    }

  return (pkt_msg_len);
}

int32_t
assemble_ipmi_rmcpplus_pkt_with_payload (u_int8_t authentication_algorithm,
                                         u_int8_t integrity_algorithm,
                                         u_int8_t confidentiality_algorithm,
                                         u_int8_t *integrity_key,
                                         u_int32_t integrity_key_len,
                                         u_int8_t *confidentiality_key,
                                         u_int32_t confidentiality_key_len,
                                         fiid_obj_t obj_hdr_rmcp,
                                         fiid_obj_t obj_rmcpplus_hdr_session,
                                         fiid_obj_t obj_msg_hdr,
                                         fiid_obj_t obj_cmd,
                                         fiid_template_t tmpl_cmd,
                                         fiid_obj_t obj_rmcpplus_trlr_session,
                                         fiid_template_t tmpl_trlr_session,
                                         u_int8_t *pkt,
                                         u_int32_t pkt_len)
{
  unsigned int pkt_msg_len = 0;
  int32_t obj_rmcp_hdr_len, obj_len, obj_len_1, obj_len_2, obj_field_start;
  u_int64_t obj_field_len, payload_type, payload_authenticated, payload_encrypted, session_id, session_seq_num;
  int32_t payload_len;
  fiid_obj_t obj_payload = NULL;
  fiid_obj_t obj_hdr_session_temp = NULL;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !obj_hdr_rmcp
      || !obj_rmcpplus_hdr_session
      || !obj_msg_hdr
      || !obj_cmd
      || !tmpl_cmd
      || !pkt
      || !pkt_len)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_GET (obj_rmcpplus_hdr_session, 
                tmpl_rmcpplus_hdr_session,
                "payload_type",
                &payload_type);

  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "payload_type.authenticated",
                &payload_authenticated);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "payload_type.encrypted",
                &payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "session_id",
                &session_id);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session,
                tmpl_rmcpplus_hdr_session,
                "session_seq_num",
                &session_seq_num);

  if (((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
        || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
       && (payload_authenticated || payload_encrypted || session_id || session_seq_num))
      || (session_id 
          && payload_authenticated
          && (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
              || !obj_rmcpplus_trlr_session 
              || !tmpl_trlr_session)))
    {
      errno = EINVAL;
      return (-1);
    }

  memset(pkt, '\0', pkt_len);

  /* 
   * Copy RMCP header into packet
   */
  pkt_msg_len = 0;
  ERR_EXIT (!((obj_rmcp_hdr_len = fiid_obj_len_bytes(tmpl_hdr_rmcp)) < 0));
  _BUF_SPACE_CHECK(obj_rmcp_hdr_len, (pkt_len - pkt_msg_len));
  memcpy (pkt, obj_hdr_rmcp, obj_rmcp_hdr_len);
  pkt_msg_len += obj_rmcp_hdr_len;
  
  /* 
   * Copy Auth Type and Payload Type into packet
   *
   * Determine length by determining the start of the OEM IANA
   */
  ERR_EXIT (!((obj_len = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
  _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "auth_type")) < 0));
  memcpy (pkt + pkt_msg_len, 
          obj_rmcpplus_hdr_session + obj_field_start,
          obj_len);
  pkt_msg_len += obj_len;  

  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      /* 
       * Copy OEM IANA and OEM Payload ID into packet
       */

      ERR_EXIT (!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
      ERR_EXIT (!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_payload_id")) < 0));
      obj_len = obj_len_1 + obj_len_2;
      _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_rmcpplus_hdr_session + obj_field_start,
              obj_len);
      pkt_msg_len += obj_len;
    }

  /* 
   * Copy Session ID and Session Sequence Number into packet 
   */
  ERR_EXIT(!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  ERR_EXIT(!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_seq_num")) < 0));
  obj_len = obj_len_1 + obj_len_2;
  _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  memcpy (pkt + pkt_msg_len,
          obj_rmcpplus_hdr_session + obj_field_start,
          obj_len);
  pkt_msg_len += obj_len;

  /* 
   * Construct/Encrypt Payload and copy into packet
   */
  FIID_OBJ_ALLOCA (obj_payload, tmpl_rmcpplus_payload);

  if ((payload_len = _construct_payload(payload_type,
                                        authentication_algorithm,
                                        confidentiality_algorithm,
                                        obj_msg_hdr,
                                        obj_cmd,
                                        tmpl_cmd,
                                        confidentiality_key,
                                        confidentiality_key_len,
                                        obj_payload)) < 0)
    {
      ipmi_debug("_construct_payload: %s", strerror(errno));
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_hdr_session_temp, tmpl_rmcpplus_hdr_session);
  if (!obj_hdr_session_temp)
    {
      errno = ENOMEM;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_hdr_session_temp, 
                tmpl_rmcpplus_hdr_session,
                "ipmi_payload_len",
                payload_len);
  
  ERR_EXIT (!((obj_len = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "ipmi_payload_len")) < 0));
  _BUF_SPACE_CHECK(obj_len, (pkt_len - pkt_msg_len));
  ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "ipmi_payload_len")) < 0));
  memcpy (pkt + pkt_msg_len,
          obj_hdr_session_temp + obj_field_start,
          obj_len);
  pkt_msg_len += obj_len;

  FIID_OBJ_GET (obj_payload,
                tmpl_rmcpplus_payload,
                "confidentiality_header_len",
                &obj_field_len);
  if (obj_field_len)
    {
      _BUF_SPACE_CHECK(obj_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "confidentiality_header")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_payload + obj_field_start,
              obj_field_len);
      pkt_msg_len += obj_field_len;
    }

  FIID_OBJ_GET (obj_payload,
                tmpl_rmcpplus_payload,
                "payload_len",
                &obj_field_len);
  if (obj_field_len)
    {
      _BUF_SPACE_CHECK(obj_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "payload")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_payload + obj_field_start,
              obj_field_len);
      pkt_msg_len += obj_field_len;
    }

  FIID_OBJ_GET (obj_payload,
                tmpl_rmcpplus_payload,
                "confidentiality_trailer_len",
                &obj_field_len);
  if (obj_field_len)
    {
      _BUF_SPACE_CHECK(obj_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_payload, "confidentiality_trailer")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_payload + obj_field_start,
              obj_field_len);
      pkt_msg_len += obj_field_len;
    }

  if (session_id && payload_authenticated)
    {
      int32_t rmcpplus_trlr_session_len;
      u_int8_t pad_len, pad_length_field_len, next_header_field_len;
      fiid_obj_t obj_rmcpplus_trlr_session_temp;
      u_int8_t auth_code_buf[IPMI_MAX_PAYLOAD_LEN];
      int32_t auth_code_len;

      if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
          || !fiid_obj_field_lookup (tmpl_trlr_session, "integrity_pad")
          || !fiid_obj_field_lookup (tmpl_trlr_session, "pad_length")
          || !fiid_obj_field_lookup (tmpl_trlr_session, "next_header"))
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_ALLOCA (obj_rmcpplus_trlr_session_temp, tmpl_trlr_session);
      ERR_EXIT (!((rmcpplus_trlr_session_len = fiid_obj_len_bytes (tmpl_trlr_session)) < 0));
      memcpy(obj_rmcpplus_trlr_session_temp, obj_rmcpplus_trlr_session, rmcpplus_trlr_session_len);

      if (_construct_trlr_session_pad(integrity_algorithm,
                                      (pkt_msg_len - obj_rmcp_hdr_len),
                                      &pad_len,
                                      &pad_length_field_len,
                                      &next_header_field_len,
                                      obj_rmcpplus_trlr_session_temp,
                                      tmpl_trlr_session) < 0)
        return (-1);

      /* 
       * Copy pad into packet
       */
      if (pad_len)
        {
          _BUF_SPACE_CHECK(pad_len, (pkt_len - pkt_msg_len));
          ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "integrity_pad")) < 0));
          ERR_EXIT (pad_len < IPMI_INTEGRITY_PAD_MULTIPLE);
          memcpy (pkt + pkt_msg_len, obj_rmcpplus_trlr_session_temp + obj_field_start, pad_len);
          pkt_msg_len += pad_len;
        }

      /* 
       * Copy pad length into packet
       */
      _BUF_SPACE_CHECK(pad_length_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "pad_length")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_rmcpplus_trlr_session + obj_field_start,
              pad_length_field_len);
      pkt_msg_len += pad_length_field_len;

      /* 
       * Copy next header field into packet
       */
      _BUF_SPACE_CHECK(next_header_field_len, (pkt_len - pkt_msg_len));
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_trlr_session, "next_header")) < 0));
      memcpy (pkt + pkt_msg_len,
              obj_rmcpplus_trlr_session + obj_field_start,
              next_header_field_len);
      pkt_msg_len += next_header_field_len;

      /* achu: Note that the integrity code is all data prior to the authcode, so this 
       * call must be done after the pad, pad length, and next header are copied into 
       * the pkt buffer.
       */
      if ((auth_code_len = _construct_trlr_session_auth_code(integrity_algorithm,
                                                             integrity_key,
                                                             integrity_key_len,
                                                             obj_rmcpplus_trlr_session,
                                                             tmpl_trlr_session,
                                                             pkt + obj_rmcp_hdr_len,
                                                             pkt_msg_len - obj_rmcp_hdr_len,
                                                             auth_code_buf,
                                                             IPMI_MAX_PAYLOAD_LEN)) < 0)
        return (-1);

      if (auth_code_len)
        {
          _BUF_SPACE_CHECK(auth_code_len, (pkt_len - pkt_msg_len));
          memcpy(pkt + pkt_msg_len, auth_code_buf, auth_code_len);
          pkt_msg_len += auth_code_len;
        }
    }

  return (pkt_msg_len);
}

static int32_t
_deconstruct_payload_buf(fiid_obj_t obj_msg_hdr,
                         u_int8_t *obj_cmd_buf,
                         u_int32_t obj_cmd_buf_len,
                         fiid_obj_t obj_msg_trlr,
                         u_int8_t *pkt,
                         u_int32_t lan_msg_len)
{
  int32_t obj_msg_hdr_len, obj_msg_trlr_len, obj_cmd_len;
  unsigned int pkt_index = 0;

  if (!pkt || !lan_msg_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_buf: Invalid parameters");
      return (-1);
    }

  obj_cmd_len = 0;

  ERR_EXIT (!((obj_msg_hdr_len = fiid_obj_len_bytes (tmpl_lan_msg_hdr_rs)) < 0));
  ERR_EXIT (!((obj_msg_trlr_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr)) < 0));

  if (obj_msg_hdr)
    {
      FIID_OBJ_MEMSET (obj_msg_hdr, '\0', tmpl_lan_msg_hdr_rs);
      memcpy (obj_msg_hdr,
              pkt + pkt_index,
              FREEIPMI_MIN ((lan_msg_len - pkt_index), obj_msg_hdr_len));
    }
  pkt_index += obj_msg_hdr_len;

  if (pkt_index <= lan_msg_len)
    return 0;
  
  /* achu: Whatever is in between the header and the trailer is the
   * command data
   */

  if ((lan_msg_len - pkt_index) > obj_msg_trlr_len)
    obj_cmd_len = (lan_msg_len - pkt_index) - obj_msg_trlr_len;
  else
    obj_cmd_len = (lan_msg_len - pkt_index);

  if (obj_cmd_len > obj_cmd_buf_len)
    {
      errno = ENOSPC;
      ipmi_debug("_deconstruct_payload_buf: buffer short");
      return (-1);
    }

  memcpy(obj_cmd_buf, pkt + pkt_index, obj_cmd_len);
  pkt_index += obj_cmd_len;

  if (pkt_index <= lan_msg_len)
    return obj_cmd_len;

  memcpy(obj_msg_trlr, pkt + pkt_index, obj_msg_trlr_len);

  return obj_cmd_len;
}

static int32_t
_deconstruct_payload_buf_cmd_buf(fiid_obj_t obj_msg_hdr,
                                 fiid_obj_t obj_cmd_buf,
                                 u_int32_t obj_cmd_buf_len,
                                 fiid_obj_t obj_msg_trlr,
                                 u_int8_t *pkt,
                                 u_int32_t lan_msg_len)
{
  return _deconstruct_payload_buf(obj_msg_hdr,
                                  (u_int8_t *)obj_cmd_buf,
                                  obj_cmd_buf_len,
                                  obj_msg_trlr,
                                  pkt,
                                  lan_msg_len);
}

static int32_t
_deconstruct_payload_buf_cmd_tmpl(fiid_obj_t obj_msg_hdr,
                                  fiid_obj_t obj_cmd_buf,
                                  fiid_template_t tmpl_cmd,
                                  fiid_obj_t obj_msg_trlr,
                                  u_int8_t *pkt,
                                  u_int32_t lan_msg_len)
{
  int32_t obj_cmd_buf_len;

  if ((obj_cmd_buf && !tmpl_cmd)
      || !pkt
      || !lan_msg_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_buf_cmd_tmpl: Invalid parameters");
      return (-1);
    }

  ERR_EXIT (!((obj_cmd_buf_len = fiid_obj_len_bytes (tmpl_cmd)) < 0));
  
  return _deconstruct_payload_buf(obj_msg_hdr,
                                  (u_int8_t *)obj_cmd_buf,
                                  obj_cmd_buf_len,
                                  obj_msg_trlr,
                                  pkt,
                                  lan_msg_len);
}

static int32_t
_deconstruct_payload_special(u_int8_t payload_type,
                             u_int8_t authentication_algorithm,
                             fiid_obj_t obj_msg_hdr,
                             fiid_obj_t obj_cmd,
                             fiid_obj_t obj_msg_trlr,
                             fiid_template_t tmpl_cmd,
                             u_int8_t *pkt,
                             u_int32_t ipmi_payload_len)
{
  u_int8_t obj_cmd_buf[IPMI_MAX_PAYLOAD_LEN];
  int32_t obj_len, obj_cmd_len, cmd_index = 0;
  u_int64_t obj_field_len;
  int32_t obj_data_len, obj_field_start;
  
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || (obj_cmd && !tmpl_cmd)
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_special: Invalid parameters");
      return (-1);
    }

  if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    {
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

      memset(obj_cmd_buf, '\0', IPMI_MAX_PAYLOAD_LEN);

      /* achu: RAKP2 is not encrypted, so the payload length is the length of
       * the msg_hdr, cmd, and msg_trlr.
       */
      if ((obj_cmd_len = _deconstruct_payload_buf_cmd_buf(obj_msg_hdr,
                                                          obj_cmd_buf,
                                                          IPMI_MAX_PAYLOAD_LEN,
                                                          obj_msg_trlr,
                                                          pkt,
                                                          ipmi_payload_len)) < 0)
        return (-1);

      if (!obj_cmd)
        return (0);

      ERR_EXIT (!((obj_data_len = fiid_obj_field_end_bytes (tmpl_cmd, "managed_system_guid")) < 0));

      if (obj_cmd_len < obj_data_len)
        obj_data_len = obj_cmd_len;

        memcpy (obj_cmd, obj_cmd_buf, obj_data_len);
      cmd_index += obj_data_len;
      
      if (ipmi_payload_len <= cmd_index)
        return (0);

      /* Whatever is left is the key authentication code */
      
      obj_field_len = ipmi_payload_len - cmd_index;
      
      ERR_EXIT (!((obj_len = fiid_obj_field_len_bytes(tmpl_cmd, "key_exchange_authentication_code")) < 0));

      if (obj_field_len > obj_len)
        {
          errno = ENOSPC;
          return (-1);
        }
      
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_cmd, "key_exchange_authentication_code")) < 0));
      
      memcpy(obj_cmd + obj_field_start,
             pkt + cmd_index,
             obj_field_len);
      
      FIID_OBJ_SET (obj_cmd,
                    tmpl_cmd,
                    "key_exchange_authentication_code_len",
                    obj_field_len);

      return (0);
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      if (!fiid_obj_field_lookup (tmpl_cmd, "message_tag")
          || !fiid_obj_field_lookup (tmpl_cmd, "rmcpplus_status_code")
          || !fiid_obj_field_lookup (tmpl_cmd, "management_console_session_id")
          || !fiid_obj_field_lookup (tmpl_cmd, "integrity_check_value")
          || !fiid_obj_field_lookup (tmpl_cmd, "integrity_check_value_len"))
        {
          errno = EINVAL;
          return (-1);
        }

      memset(obj_cmd_buf, '\0', IPMI_MAX_PAYLOAD_LEN);

      /* achu: RAKP4 is not encrypted, so the payload length is the length of
       * the msg_hdr, cmd, and msg_trlr.
       */
      if ((obj_cmd_len = _deconstruct_payload_buf_cmd_buf(obj_msg_hdr,
                                                          obj_cmd_buf,
                                                          IPMI_MAX_PAYLOAD_LEN,
                                                          obj_msg_trlr,
                                                          pkt,
                                                          ipmi_payload_len)) < 0)
        return (-1);

      if (!obj_cmd)
        return (0);

      ERR_EXIT (!((obj_data_len = fiid_obj_field_end_bytes (tmpl_cmd, "management_console_session_id")) < 0));

      if (obj_cmd_len < obj_data_len)
        obj_data_len = obj_cmd_len;

      memcpy (obj_cmd, obj_cmd_buf, obj_data_len);
      cmd_index += obj_data_len;
      
      if (ipmi_payload_len <= cmd_index)
        return (0);

      /* Whatever is left is the integrity check value */
      
      obj_field_len = ipmi_payload_len - cmd_index;
      
      ERR_EXIT (!((obj_len = fiid_obj_field_len_bytes(tmpl_cmd, "integrity_check_value")) < 0));

      if (obj_field_len > obj_len)
        {
          errno = ENOSPC;
          return (-1);
        }
      
      ERR_EXIT (!((obj_field_start = fiid_obj_field_start_bytes (tmpl_cmd, "integrity_check_value")) < 0));
      
      memcpy(obj_cmd + obj_field_start,
             pkt + cmd_index,
             obj_field_len);
      
      FIID_OBJ_SET (obj_cmd,
                    tmpl_cmd,
                    "integrity_check_value_len",
                    obj_field_len);

      return (0);
    }
  else
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_special: Invalid parameters");
      return (-1);
    }

  /* NOT REACHED */
  return (0);
}

static int32_t
_deconstruct_payload_confidentiality_none(fiid_obj_t obj_msg_hdr,
                                          fiid_obj_t obj_cmd,
                                          fiid_obj_t obj_msg_trlr,
                                          fiid_template_t tmpl_cmd,
                                          u_int8_t *pkt,
                                          u_int32_t ipmi_payload_len)
{
  int32_t obj_cmd_len;

  if ((obj_cmd && !tmpl_cmd)
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_confidentiality_none: Invalid parameters");
      return (-1);
    }
  
  /* achu: No encryption, so ipmi_payload_len is the length of 
   * the msg_hdr, cmd, and msg_trlr.
   */
  if ((obj_cmd_len = _deconstruct_payload_buf_cmd_tmpl(obj_msg_hdr,
                                                       obj_cmd,
                                                       tmpl_cmd,
                                                       obj_msg_trlr,
                                                       pkt,
                                                       ipmi_payload_len)) < 0)
    return (-1);
  
  return (0);
}

static int32_t
_deconstruct_payload_confidentiality_aes_cbc_128(fiid_obj_t obj_msg_hdr,
                                                 fiid_obj_t obj_cmd,
                                                 fiid_obj_t obj_msg_trlr,
                                                 fiid_template_t tmpl_cmd,
                                                 u_int8_t *confidentiality_key,
                                                 u_int32_t confidentiality_key_len,
                                                 u_int8_t *pkt,
                                                 u_int32_t ipmi_payload_len)
{
  u_int8_t iv[IPMI_AES_CBC_128_IV_LEN];
  int32_t iv_len;
  u_int8_t payload_buf[IPMI_MAX_PAYLOAD_LEN];
  u_int8_t pad_len;
  int cipher_keylen, cipher_blocklen, i;
  int32_t payload_data_len, decrypt_len, cmd_data_len, obj_cmd_len, pkt_index = 0;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  if ((obj_cmd && !tmpl_cmd)
      || !confidentiality_key
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_confidentiality_aes_cbc_128: Invalid parameters");
      return (-1);
    }

  if ((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0)
    return (-1);

  ERR_EXIT (cipher_keylen < IPMI_AES_CBC_128_KEY_LEN);

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

  if (!payload_data_len)
    {
      errno = EINVAL;
      return (-1);
    }

  memcpy(iv, pkt, IPMI_AES_CBC_128_BLOCK_LEN);
  pkt_index += IPMI_AES_CBC_128_BLOCK_LEN;

  memcpy(payload_buf, pkt + pkt_index, payload_data_len);

  if ((decrypt_len = ipmi_crypt_cipher_decrypt(IPMI_CRYPT_CIPHER_AES,
                                               IPMI_CRYPT_CIPHER_MODE_CBC,
                                               confidentiality_key,
                                               confidentiality_key_len,
                                               iv,
                                               iv_len,
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
      ipmi_debug("_deconstruct_payload_confidentiality_aes_cbc_128: invalid pad_len");
      return (-1);
    }

  cmd_data_len = payload_data_len - pad_len - 1;
  if (cmd_data_len <= 0)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_confidentiality_aes_cbc_128: invalid cmd_data_len");
      return (-1);
    }

  /* XXX: Is this right to check and return -1 here? */
  for (i = 0; i < pad_len; i++)
    {
      if (payload_buf[cmd_data_len + i] != i + 1)
        {
          errno = EINVAL;
          ipmi_debug("_deconstruct_payload_confidentiality_aes_cbc_128: invalid pad data");
          return (-1);
        }
    }
  
  if ((obj_cmd_len = _deconstruct_payload_buf_cmd_tmpl(obj_msg_hdr, 
                                                       obj_cmd, 
                                                       tmpl_cmd, 
                                                       obj_msg_trlr,
                                                       payload_buf,
                                                       cmd_data_len)) < 0)
    return (-1);

  return (0);
}

static int32_t
_deconstruct_payload(u_int8_t payload_type,
                     u_int8_t authentication_algorithm,
                     u_int8_t confidentiality_algorithm,
                     fiid_obj_t obj_msg_hdr,
                     fiid_obj_t obj_cmd,
                     fiid_obj_t obj_msg_trlr,
                     fiid_template_t tmpl_cmd,
                     u_int8_t *confidentiality_key,
                     u_int32_t confidentiality_key_len,
                     u_int8_t *pkt,
                     u_int32_t ipmi_payload_len)
{
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload: Invalid parameters");
      return (-1);
    }

  /* First determine if this is a special payload_type 
   * 
   * Note: We don't check consider RAKP1 or RAKP3 special b/c
   * they are requests, not responses
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      return _deconstruct_payload_special(payload_type,
                                          authentication_algorithm,
                                          obj_msg_hdr,
                                          obj_cmd,
                                          obj_msg_trlr,
                                          tmpl_cmd,
                                          pkt,
                                          ipmi_payload_len);
    }
  else
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _deconstruct_payload_confidentiality_none(obj_msg_hdr,
                                                         obj_cmd,
                                                         obj_msg_trlr,
                                                         tmpl_cmd,
                                                         pkt,
                                                         ipmi_payload_len);
      else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
        return _deconstruct_payload_confidentiality_aes_cbc_128(obj_msg_hdr,
                                                                obj_cmd,
                                                                obj_msg_trlr,
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
  return (0);
}

/* XXX: How check correctness of payload? */
int32_t
unassemble_ipmi_rmcpplus_pkt (u_int8_t authentication_algorithm,
                              u_int8_t integrity_algorithm,
                              u_int8_t confidentiality_algorithm,
                              u_int8_t *integrity_key,
                              u_int32_t integrity_key_len,
                              u_int8_t *confidentiality_key,
                              u_int32_t confidentiality_key_len,
                              u_int8_t *pkt,
                              u_int32_t pkt_len,
                              fiid_obj_t obj_hdr_rmcp,
                              fiid_obj_t obj_rmcpplus_hdr_session,
                              fiid_obj_t obj_msg_hdr,
                              fiid_obj_t obj_cmd,
                              fiid_obj_t obj_msg_trlr,
                              fiid_template_t tmpl_cmd,
                              fiid_obj_t obj_rmcpplus_trlr_session,
                              fiid_template_t tmpl_trlr_session)
{
  unsigned int pkt_index = 0;
  int32_t obj_rmcp_hdr_len, obj_len, obj_len_1, obj_len_2, obj_len_3, obj_field_start;
  u_int64_t payload_type, payload_authenticated, payload_encrypted, session_id, session_seq_num, ipmi_payload_len;
  fiid_obj_t obj_rmcpplus_hdr_session_temp;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !pkt
      || (obj_cmd && !tmpl_cmd)
      || (obj_rmcpplus_trlr_session && !tmpl_trlr_session))
    {
      errno = EINVAL;
      return -1;
    }

  ERR_EXIT (!((obj_rmcp_hdr_len = fiid_obj_len_bytes (tmpl_hdr_rmcp)) < 0));
  if (obj_hdr_rmcp)
    {
      FIID_OBJ_MEMSET (obj_hdr_rmcp, '\0', tmpl_hdr_rmcp);
      memcpy (obj_hdr_rmcp, 
              pkt + pkt_index,
              FREEIPMI_MIN((pkt_len - pkt_index), obj_rmcp_hdr_len));
    }
  pkt_index += obj_rmcp_hdr_len;

  if (pkt_len <= pkt_index)
    return 0;

  FIID_OBJ_ALLOCA(obj_rmcpplus_hdr_session_temp, tmpl_rmcpplus_hdr_session);

  /*
   * Extract auth_type and payload information 
   */
  ERR_EXIT(!((obj_len = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
  ERR_EXIT(!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "auth_type")) < 0));
  memcpy (obj_rmcpplus_hdr_session_temp + obj_field_start,
          pkt + pkt_index,
          FREEIPMI_MIN ((pkt_len - pkt_index), obj_len));
  pkt_index += obj_len;

  if (pkt_len <= pkt_index)
    return 0;

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp, 
                tmpl_rmcpplus_hdr_session,
                "payload_type",
                &payload_type);
  
  /* XXX: Is this right to check and return -1 here? */
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type))
    {
      errno = EINVAL;
      return (-1);
    }
   
  /* 
   * Extract OEM IANA and OEM Payload ID
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      ERR_EXIT(!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
      ERR_EXIT(!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "oem_payload_id")) < 0));
      obj_len = obj_len_1 + obj_len_2;
      ERR_EXIT(!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "oem_iana")) < 0));
      memcpy (obj_rmcpplus_hdr_session_temp + obj_field_start,
              pkt + pkt_index,
              FREEIPMI_MIN ((pkt_len - pkt_index), obj_len));
      pkt_index += obj_len;
      
      if (pkt_len <= pkt_index)
        return 0;
    }

  /* 
   * Extract Session ID, Session Sequence Number, and Payload Length
   */
  ERR_EXIT(!((obj_len_1 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  ERR_EXIT(!((obj_len_2 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "session_seq_num")) < 0));
  ERR_EXIT(!((obj_len_3 = fiid_obj_field_len_bytes (tmpl_rmcpplus_hdr_session, "ipmi_payload_len")) < 0));
  obj_len = obj_len_1 + obj_len_2 + obj_len_3;
  ERR_EXIT(!((obj_field_start = fiid_obj_field_start_bytes (tmpl_rmcpplus_hdr_session, "session_id")) < 0));
  memcpy (obj_rmcpplus_hdr_session_temp + obj_field_start,
          pkt + pkt_index,
          FREEIPMI_MIN ((pkt_len - pkt_index), obj_len));
  pkt_index += obj_len;
      
  if (pkt_len <= pkt_index)
    return 0;
      
  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "payload_type.authenticated",
                &payload_authenticated);
  
  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "payload_type.encrypted",
                &payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "session_id",
                &session_id);
  
  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "session_seq_num",
                &session_seq_num);

  FIID_OBJ_GET (obj_rmcpplus_hdr_session_temp,
                tmpl_rmcpplus_hdr_session,
                "ipmi_payload_len",
                &ipmi_payload_len);

  /* XXX: Is this right to check and return -1 here? */
  if (((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
        || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
       && (payload_authenticated || payload_encrypted || session_id || session_seq_num))
      || (session_id 
          && payload_authenticated
          && (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
              || !obj_rmcpplus_trlr_session 
              || !tmpl_trlr_session))
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if (obj_rmcpplus_hdr_session)
    {
      int32_t obj_rmcpplus_hdr_session_len;
      ERR_EXIT (!((obj_rmcpplus_hdr_session_len = fiid_obj_len_bytes(tmpl_rmcpplus_hdr_session)) < 0));
      FIID_OBJ_MEMSET (obj_rmcpplus_hdr_session, '\0', tmpl_rmcpplus_hdr_session);
      memcpy(obj_rmcpplus_hdr_session, obj_rmcpplus_hdr_session_temp, obj_rmcpplus_hdr_session_len);
    }  

  /* 
   * Deconstruct/Decrypt Payload
   */
  if (_deconstruct_payload(payload_type,
                           authentication_algorithm,
                           confidentiality_algorithm,
                           obj_msg_hdr,
                           obj_cmd,
                           obj_msg_trlr,
                           tmpl_cmd,
                           confidentiality_key,
                           confidentiality_key_len,
                           pkt,
                           FREEIPMI_MIN ((pkt_len - pkt_index), ipmi_payload_len)) < 0)
    {
      ipmi_debug("_deconstruct_payload: %s", strerror(errno));
      return (-1);
    } 
  pkt_index += ipmi_payload_len;

  if (pkt_len <= pkt_index)
    return 0;
  
  if (session_id && payload_authenticated && obj_rmcpplus_trlr_session)
    {
      int32_t pad_length_field_len, next_header_field_len;
      u_int32_t authcode_len;
      u_int64_t pad_length;
      char *auth_field, *auth_field_len;

      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        {
          /* achu: There shouldn't be any integrity data */
          errno = EINVAL;
          return (-1);
        }
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
      
      if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
          || !fiid_obj_field_lookup (tmpl_trlr_session, "integrity_pad")
          || !fiid_obj_field_lookup (tmpl_trlr_session, "pad_length")
          || !fiid_obj_field_lookup (tmpl_trlr_session, "next_header"))
        {
          errno = EINVAL;
          return (-1);
        }

      ERR_EXIT (!((pad_length_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "pad_length")) < 0)); 
      ERR_EXIT (!((next_header_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "next_header")) < 0));

      /* achu: There needs to be atleast the next_header and pad_len fields */
      if (pkt_len < (authcode_len + pad_length_field_len + next_header_field_len))
        {
          errno = EINVAL;
          return (-1);
        }


      if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_code"))
        {
          auth_field = "auth_code";
          auth_field_len = "auth_code_len";
        }
      else
        {
          auth_field = "auth_calc";
          auth_field = "auth_calc_len";
        }

      if (fiid_obj_set_data(obj_rmcpplus_trlr_session,
                            tmpl_trlr_session,
                            auth_field,
                            pkt + pkt_index + ((pkt_len - pkt_index) - authcode_len),
                            authcode_len) < 0)
        return (-1);

      if (fiid_obj_set(obj_rmcpplus_trlr_session,
                       tmpl_trlr_session,
                       auth_field_len,
                       authcode_len) < 0)
        return (-1);
                        
      if (fiid_obj_set_data(obj_rmcpplus_trlr_session,
                            tmpl_trlr_session,
                            "next_header",
                            pkt + pkt_index + ((pkt_len - pkt_index) - authcode_len - next_header_field_len),
                            next_header_field_len) < 0)
        return (-1);
      
      if (fiid_obj_set_data(obj_rmcpplus_trlr_session,
                            tmpl_trlr_session,
                            "pad_length",
                            pkt + pkt_index + ((pkt_len - pkt_index) - authcode_len - next_header_field_len - pad_length_field_len),
                            pad_length_field_len) < 0)
        return (-1);
      
      if (fiid_obj_get(obj_rmcpplus_trlr_session,
                       tmpl_trlr_session,
                       "pad_length",
                       &pad_length) < 0)
        return (-1);

      if (pad_length != (pkt_index + authcode_len + pad_length_field_len + next_header_field_len))
        {
          errno = EINVAL;
          return (-1);
        }

      if (fiid_obj_set_data(obj_rmcpplus_trlr_session,
                            tmpl_trlr_session,
                            "integrity_pad",
                            pkt + pkt_index,
                            pad_length) < 0)
        return (-1);
    }

  return (0);
}

