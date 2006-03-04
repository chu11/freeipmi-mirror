/* 
   ipmi-rmcpplus-interface.c - IPMI RMCPPLUS Interface

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-rmcpplus-interface.h"
#include "freeipmi/ipmi-rmcpplus.h"
#include "freeipmi/ipmi-rmcpplus-utils.h"
#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-debug.h"
#include "freeipmi/ipmi-messaging-support-cmds.h" /* XXX  - only for IPMI_MAX_USER_NAME_LENGTH */
#include "freeipmi/ipmi-privilege-level-spec.h"
#include "freeipmi/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/rmcp.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "md5.h"

#define _BUF_SPACE_CHECK(__copy_in_len, __buf_space_left) \
do { \
   if ((__copy_in_len) > (__buf_space_left)) \
     { \
       errno = ENOSPC; \
       goto cleanup; \
     } \
}  while (0) 

static int32_t
_construct_payload_buf(fiid_obj_t obj_lan_msg_hdr,
                       fiid_obj_t obj_cmd,
                       uint8_t *payload_buf,
                       uint32_t payload_buf_len)
{
  int32_t obj_lan_msg_hdr_len, obj_lan_msg_trlr_len, checksum_start_offset, checksum_block_len;
  uint32_t payload_len;
  uint8_t checksum;
  int32_t obj_cmd_len;
  int32_t len, indx = 0, rv = -1;
  fiid_obj_t obj_lan_msg_trlr = NULL;

  if (!fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !payload_buf
      || !payload_buf_len)
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_buf: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq);
  FIID_OBJ_PACKET_VALID(obj_lan_msg_hdr);

  memset(payload_buf, '\0', payload_buf_len);

  ERR_EXIT (!((obj_lan_msg_hdr_len = fiid_obj_len_bytes (obj_lan_msg_hdr)) < 0));
  ERR_EXIT (!((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));
  ERR_CLEANUP (!((obj_cmd_len = fiid_obj_len_bytes (obj_cmd)) < 0));

  payload_len = obj_lan_msg_hdr_len + obj_cmd_len + obj_lan_msg_trlr_len;

  ERR_EXIT (!(payload_len > IPMI_MAX_PAYLOAD_LENGTH));

  if (payload_len > payload_buf_len)
    {
      errno = ENOSPC;
      ipmi_debug("_construct_payload_buf: buffer short");
      goto cleanup;
    }
   
  FIID_OBJ_GET_ALL_LEN_CLEANUP(len, obj_lan_msg_hdr, payload_buf + indx, payload_buf_len - indx);
  indx += len;
    
  FIID_OBJ_GET_ALL_LEN_CLEANUP(len, obj_cmd, payload_buf + indx, payload_buf_len - indx);
  indx += len;

  FIID_OBJ_CREATE_CLEANUP(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  checksum = ipmi_checksum (payload_buf + checksum_start_offset, checksum_block_len);

  ERR_EXIT (!((checksum_start_offset = fiid_template_field_end_bytes (tmpl_lan_msg_hdr_rq, "checksum1")) < 0));
  checksum_block_len = obj_lan_msg_hdr_len - checksum_start_offset + obj_cmd_len;

  FIID_OBJ_SET_ALL_CLEANUP (obj_lan_msg_trlr, (uint8_t *)&checksum, sizeof(checksum));

  FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_lan_msg_trlr, payload_buf + indx, payload_buf_len - indx);
  len += indx;
  
  rv = indx;
 cleanup:
  if (rv < 0)
    memset(payload_buf, '\0', payload_buf_len);
  FIID_OBJ_DESTROY_NO_RETURN(obj_lan_msg_trlr);
  return (rv);
}

static int32_t
_construct_payload_confidentiality_none(fiid_obj_t obj_lan_msg_hdr,
                                        fiid_obj_t obj_cmd,
                                        fiid_obj_t obj_payload)
{
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  int32_t payload_len;

  if (!fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_payload))
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_confidentiality_none: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_CLEAR (obj_payload);

  ERR (!((payload_len = _construct_payload_buf(obj_lan_msg_hdr, 
                                               obj_cmd, 
                                               payload_buf, 
                                               IPMI_MAX_PAYLOAD_LENGTH)) < 0));

  FIID_OBJ_SET_DATA (obj_payload,
                     "payload_data",
                     payload_buf,
                     payload_len);

  return payload_len;
}

static int32_t
_construct_payload_confidentiality_aes_cbc_128(uint8_t payload_encrypted,
                                               fiid_obj_t obj_lan_msg_hdr,
                                               fiid_obj_t obj_cmd,
                                               uint8_t *confidentiality_key,
                                               uint32_t confidentiality_key_len,
                                               fiid_obj_t obj_payload)
{
  uint8_t iv[IPMI_AES_CBC_128_IV_LENGTH];
  int32_t iv_len;
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_len;
  uint32_t payload_len;
  int cipher_keylen, cipher_blocklen, encrypt_len;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  if (!payload_encrypted
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !confidentiality_key
      || !fiid_obj_valid(obj_payload))
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_confidentiality_aes_cbc_128: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);

  ERR (!((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (!(cipher_keylen < IPMI_AES_CBC_128_KEY_LENGTH));

  if (confidentiality_key_len < IPMI_AES_CBC_128_KEY_LENGTH)
    {
      errno = EINVAL;
      return (-1);
    }
  confidentiality_key_len = IPMI_AES_CBC_128_KEY_LENGTH;

  ERR (!((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (cipher_blocklen == IPMI_AES_CBC_128_BLOCK_LENGTH);
     
  ERR (!((iv_len = ipmi_get_random((char *)iv, IPMI_AES_CBC_128_IV_LENGTH)) < 0));
  ERR (!(iv_len != IPMI_AES_CBC_128_IV_LENGTH));
    
  ERR (!((payload_len = _construct_payload_buf(obj_lan_msg_hdr, 
                                               obj_cmd, 
                                               payload_buf, 
                                               IPMI_MAX_PAYLOAD_LENGTH)) < 0));

  /* Pad the data appropriately */

  /* +1 is for the pad length field */
  pad_len = IPMI_AES_CBC_128_BLOCK_LENGTH - ((payload_len + 1) % IPMI_AES_CBC_128_BLOCK_LENGTH);
      
  if (!((payload_len + pad_len + 1) > IPMI_MAX_PAYLOAD_LENGTH))
    {
      errno = ENOSPC;
      ipmi_debug("ipmi_get_random: buffer short");
      return (-1);
    }
  
  if (pad_len)
    {
      int i;
      for (i = 0; i < pad_len; i++)
        payload_buf[payload_len + i] = i + 1;
      payload_buf[payload_len + pad_len] = pad_len;
    }

  /* +1 for pad length field */
  ERR (!((encrypt_len = ipmi_crypt_cipher_encrypt(IPMI_CRYPT_CIPHER_AES,
                                                  IPMI_CRYPT_CIPHER_MODE_CBC,
                                                  confidentiality_key,
                                                  confidentiality_key_len,
                                                  iv,
                                                  iv_len,
                                                  payload_buf,
                                                  payload_len + pad_len + 1)) < 0));
  ERR (!(encrypt_len != (payload_len + pad_len + 1)));

  FIID_OBJ_CLEAR (obj_payload);

  FIID_OBJ_SET_DATA (obj_payload,
                     "confidentiality_header",
                     iv,
                     IPMI_AES_CBC_128_IV_LENGTH);
      
  FIID_OBJ_SET_DATA (obj_payload,
                     "payload_data",
                     payload_buf,
                     payload_len);
      
  FIID_OBJ_SET_DATA (obj_payload,
                     "confidentiality_trailer",
                     payload_buf + payload_len,
                     pad_len + 1);
      
  return (iv_len + payload_len + pad_len + 1);
}

static int32_t
_construct_payload_special(uint8_t payload_type,
                           uint8_t authentication_algorithm,
                           fiid_obj_t obj_cmd,
                           fiid_obj_t obj_payload)
{
  uint8_t obj_cmd_buf[IPMI_MAX_PAYLOAD_LENGTH];
  int32_t obj_cmd_len = 0;

  if ((payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
      || (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
          && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_rq) != 1)
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
          && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_1) != 1)
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
          && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_3) != 1)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_payload))
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload_special: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_PACKET_VALID(obj_cmd);
  FIID_OBJ_CLEAR (obj_payload);

  FIID_OBJ_GET_ALL_LEN (obj_cmd_len, obj_cmd, obj_cmd_buf, IPMI_MAX_PAYLOAD_LENGTH);

  FIID_OBJ_SET_DATA (obj_payload,
                     "payload_data",
                     obj_cmd_buf,
                     obj_cmd_len);
  
  return (obj_cmd_len);
}

static int32_t
_construct_payload(uint8_t payload_type,
                   uint8_t payload_encrypted,
                   uint8_t authentication_algorithm,
                   uint8_t confidentiality_algorithm,
                   fiid_obj_t obj_lan_msg_hdr,
                   fiid_obj_t obj_cmd,
                   uint8_t *confidentiality_key,
                   uint32_t confidentiality_key_len,
                   fiid_obj_t obj_payload)
{
  /* Note: We don't check consider OPEN_SESSION_RESPONSE, RAKP2 or
   * RAKP4 payload types b/c they are responses, not requests.
   */
  if ((payload_type != IPMI_PAYLOAD_TYPE_IPMI
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || (confidentiality_algorithm != IPMI_CONFIDENTIALITY_ALGORITHM_NONE
          && confidentiality_algorithm != IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_payload))
    {
      errno = EINVAL;
      ipmi_debug("_construct_payload: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq);
  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _construct_payload_confidentiality_none(obj_lan_msg_hdr,
                                                       obj_cmd,
                                                       obj_payload);
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return _construct_payload_confidentiality_aes_cbc_128(payload_encrypted,
                                                              obj_lan_msg_hdr,
                                                              obj_cmd,
                                                              confidentiality_key,
                                                              confidentiality_key_len,
                                                              obj_payload);
    }
  else
    return _construct_payload_special(payload_type,
                                      authentication_algorithm,
                                      obj_cmd,
                                      obj_payload);

  /* NOT REACHED */
  return (0);
}

static int32_t
_calculate_authentication_code_len(uint8_t integrity_algorithm)
{
  int32_t authentication_code_len;

  if ((integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_NONE
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_MD5_128))
    {
      errno = EINVAL;
      ipmi_debug("_authentication_code_len: Invalid parameters");
      return (-1);
    }
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE) 
    authentication_code_len = 0;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
  else /* IPMI_INTEGRITY_ALGORITHM_MD5_128 */
    authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
  
  return (authentication_code_len);
}

static int8_t
_construct_session_trlr_pad(uint8_t integrity_algorithm,
                            uint32_t ipmi_msg_len,
                            fiid_obj_t obj_rmcpplus_session_trlr)
{
  int32_t authentication_code_len;
  int8_t pad_len, pad_length_field_len, next_header_field_len;
  uint8_t pad_bytes[IPMI_INTEGRITY_PAD_MULTIPLE] = {IPMI_INTEGRITY_PAD_DATA,
                                                    IPMI_INTEGRITY_PAD_DATA,
                                                    IPMI_INTEGRITY_PAD_DATA,
                                                    IPMI_INTEGRITY_PAD_DATA};

  if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !fiid_obj_valid(obj_rmcpplus_session_trlr))
    {
      errno = EINVAL;
      ipmi_debug("_construct_session_trlr_pad: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  ERR (!((authentication_code_len = _calculate_authentication_code_len(integrity_algorithm)) < 0));
  
  ERR_EXIT (!((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "pad_length")) < 0)); 
  ERR_EXIT (!((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "next_header")) < 0));
  
  pad_len = IPMI_INTEGRITY_PAD_MULTIPLE - ((ipmi_msg_len + pad_length_field_len + next_header_field_len + authentication_code_len) % IPMI_INTEGRITY_PAD_MULTIPLE);

  FIID_OBJ_CLEAR_FIELD (obj_rmcpplus_session_trlr, "integrity_pad");

  if (pad_len)
    FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                       "integrity_pad",
                       pad_bytes,
                       pad_len);
  
  return (0);
}

static int32_t
_construct_session_trlr_authentication_code(uint8_t integrity_algorithm,
                                            uint8_t *integrity_key,
                                            uint32_t integrity_key_len,
                                            uint8_t *authentication_code_data,
                                            uint32_t authentication_code_data_len,
                                            fiid_obj_t obj_rmcpplus_session_trlr,
                                            uint8_t *pkt_data,
                                            uint32_t pkt_data_len,
                                            uint8_t *authentication_code_buf,
                                            uint32_t authentication_code_buf_len)
{
  int hash_algorithm, hash_flags, crypt_digest_len;
  unsigned int expected_digest_len, copy_digest_len, hash_data_len, integrity_digest_len;
  uint8_t hash_data[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t integrity_digest[IPMI_MAX_PAYLOAD_LENGTH];
  int32_t len, authentication_code_len;
  
  if ((integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_MD5_128)
      || (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
          && authentication_code_data 
          && authentication_code_data_len > IPMI_MAX_PASSOWRD_LENGTH)
      || !fiid_obj_valid(obj_rmcpplus_session_trlr)
      || !pkt_data
      || !pkt_data_len
      || !authentication_code_buf
      || !authentication_code_buf_len)
    {
      errno = EINVAL;
      ipmi_debug("_construct_session_trlr_authentication_code: Invalid parameters");
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  /* Check if the user provided an authentication code, if so, use it */

  memset(authentication_code_buf, '\0', authentication_code_buf_len);

  FIID_OBJ_FIELD_LEN_BYTES(len,
                           obj_rmcpplus_session_trlr,
                           (uint8_t *)"authentication_code");

  if (len)
    {
      FIID_OBJ_GET_DATA_LEN(len,
                            obj_rmcpplus_session_trlr,
                            (uint8_t *)"authentication_code",
                            authentication_code_buf,
                            authentication_code_buf_len);
      return (len);
    }

  ERR (!((authentication_code_len = _calculate_authentication_code_len(integrity_algorithm)) < 0));

  /* Note: Integrity Key for HMAC_SHA1_95 and HMAC_MD5_128 is K1 */
           
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
      copy_digest_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
    }
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      copy_digest_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  else
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = 0;
      expected_digest_len = MD5_DIGEST_LEN;
      copy_digest_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
      
  ERR (!((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0));
      
  ERR_EXIT (crypt_digest_len == expected_digest_len);
      
  memset(hash_data, '\0', IPMI_MAX_PAYLOAD_LENGTH);
      
  hash_data_len = 0;
      
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
      && authentication_code_data
      && authentication_code_data_len)
    {
      /* SPEC: achu: Zero Pad is not specified.  And even if we were
       * supposed to, we don't know if we should Zero pad to 16 or 20 bytes.
       *
       * For now, we'll assume no zero pad.  Therefore, if it is a NULL
       * password, we won't be be including a password in the hash.
       */

      memcpy(hash_data + hash_data_len, 
             authentication_code_data, 
             authentication_code_data_len);
      hash_data_len += authentication_code_data_len;
    }
  
  memcpy(hash_data + hash_data_len, pkt_data, pkt_data_len);
  hash_data_len += pkt_data_len;
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
      && authentication_code_data
      && authentication_code_data_len)
    {
      memcpy(hash_data + hash_data_len, 
             authentication_code_data, 
             authentication_code_data_len);
      hash_data_len += authentication_code_data_len;
    }
  
  if ((integrity_digest_len = ipmi_crypt_hash(hash_algorithm,
                                              hash_flags,
                                              integrity_key,
                                              integrity_key_len,
                                              hash_data,
                                              hash_data_len,
                                              integrity_digest,
                                              IPMI_MAX_PAYLOAD_LENGTH)) < 0)
    {
      ipmi_debug("ipmi_crypt_hash: %s", strerror(errno));
      return (-1);
    }
  
  if (integrity_digest_len != crypt_digest_len)
    {
      ipmi_debug("ipmi_crypt_hash: invalid digest length returned");
      return (-1);
    }
  
  if (integrity_digest_len > authentication_code_buf_len)
    {
      errno = ENOSPC;
      ipmi_debug("_construct_session_trlr_authentication_code: buffer short");
      return (-1);
    }
  
  memcpy(authentication_code_buf, integrity_digest, copy_digest_len);
      
  return (copy_digest_len);
}

int32_t
assemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm,
                            uint8_t integrity_algorithm,
                            uint8_t confidentiality_algorithm,
                            uint8_t *integrity_key,
                            uint32_t integrity_key_len,
                            uint8_t *confidentiality_key,
                            uint32_t confidentiality_key_len,
                            uint8_t *authentication_code_data,
                            uint32_t authentication_code_data_len,
                            fiid_obj_t obj_rmcp_hdr,
                            fiid_obj_t obj_rmcpplus_session_hdr,
                            fiid_obj_t obj_lan_msg_hdr,
                            fiid_obj_t obj_cmd,
                            fiid_obj_t obj_rmcpplus_session_trlr,
                            uint8_t *pkt,
                            uint32_t pkt_len)
{
  unsigned int indx = 0;
  int32_t obj_rmcp_hdr_len;
  uint64_t payload_type, payload_authenticated, payload_encrypted, session_id, session_sequence_number;
  int32_t payload_len;
  fiid_obj_t obj_payload = NULL;
  fiid_obj_t obj_session_hdr_temp = NULL;
  fiid_obj_t obj_rmcpplus_session_trlr_temp = NULL;
  int32_t req_len, obj_len, len;
  int32_t oem_iana_len, oem_payload_id_len;
  int32_t rv = -1;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !fiid_obj_valid(obj_rmcp_hdr)
      || !fiid_obj_valid(obj_rmcpplus_session_hdr)
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_rmcpplus_session_trlr)
      || !pkt
      || !pkt_len)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq);

  FIID_OBJ_PACKET_VALID(obj_rmcp_hdr);
  FIID_OBJ_PACKET_VALID(obj_lan_msg_hdr);
  FIID_OBJ_PACKET_VALID(obj_cmd);

  /*
   * Can't use FIID_OBJ_PACKET_VALID() on obj_rmcpplus_session_hdr b/c
   * a ipmi_payload_len is required but may not be set yet.
   */

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"authentication_type");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"authentication_type");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"reserved");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"reserved");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"payload_type");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"payload_type");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"payload_type.authenticated");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"payload_type.authenticated");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"payload_type.encrypted");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"payload_type.encrypted");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"session_id");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"session_id");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"session_sequence_number");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"session_sequence_number");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  /*
   * Can't use FIID_OBJ_PACKET_VALID() on obj_rmcpplus_session_trlr b/c
   * integrity pad, pad length, and authentication code may not be set.
   */
  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_trlr, (uint8_t *)"next_header");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_trlr, (uint8_t *)"next_header");
  if (len != req_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET (obj_rmcpplus_session_hdr, 
                "payload_type",
                &payload_type);

  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.authenticated",
                &payload_authenticated);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.encrypted",
                &payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_id",
                &session_id);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_sequence_number",
                &session_sequence_number);

  if (((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
        || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
       && (payload_authenticated 
	   || payload_encrypted 
	   || session_id 
	   || session_sequence_number))
      || (session_id 
          && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED
          && (integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
              && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
              && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_MD5_128))
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
          && payload_encrypted))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN(oem_iana_len, obj_rmcpplus_session_hdr, (uint8_t *)"oem_iana");
  FIID_OBJ_FIELD_LEN(oem_payload_id_len, obj_rmcpplus_session_hdr, (uint8_t *)"oem_payload_id");
  
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      int32_t oem_iana_req_len, oem_payload_id_req_len;

      FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"oem_iana");
      FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, (uint8_t *)"oem_payload_id");

      if (oem_iana_len != oem_iana_req_len
          || oem_payload_id_len != oem_payload_id_req_len)
        {
          errno = EINVAL;
          return (-1);
        }
    }
  else
    {
      if (oem_iana_len || oem_payload_id_len)
        {
          errno = EINVAL;
          return (-1);
        }
    }

  memset(pkt, '\0', pkt_len);

  /* 
   * Copy RMCP header into packet
   */
  indx = 0;
  FIID_OBJ_LEN_BYTES_CLEANUP(obj_rmcp_hdr_len, obj_rmcp_hdr);
  _BUF_SPACE_CHECK(obj_rmcp_hdr_len, (pkt_len - indx));
  FIID_OBJ_GET_ALL_LEN_CLEANUP(obj_rmcp_hdr_len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += obj_rmcp_hdr_len;
  
  /* 
   * Copy Session Header into packet
   */
  FIID_OBJ_BLOCK_LEN_BYTES_CLEANUP(len,
                                   obj_rmcpplus_session_hdr,
                                   (uint8_t *)"authentication_type",
                                   (uint8_t *)"session_sequence_number");
  _BUF_SPACE_CHECK(len, (pkt_len - indx));
  FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
                                 obj_rmcpplus_session_hdr,
                                 (uint8_t *)"authentication_type",
                                 (uint8_t *)"session_sequence_number",
                                 pkt + indx,
                                 pkt_len - indx);
  indx += len;

  /* 
   * Construct/Encrypt Payload and copy into packet
   */
  FIID_OBJ_CREATE_CLEANUP (obj_payload, tmpl_rmcpplus_payload);

  ERR (!((payload_len = _construct_payload(payload_type,
                                           payload_encrypted,
                                           authentication_algorithm,
                                           confidentiality_algorithm,
                                           obj_lan_msg_hdr,
                                           obj_cmd,
                                           confidentiality_key,
                                           confidentiality_key_len,
                                           obj_payload)) < 0));
  
  /* 
   * Copy IPMI Payload Length into packet
   */

  FIID_OBJ_CREATE (obj_session_hdr_temp, tmpl_rmcpplus_session_hdr);  
  FIID_OBJ_CLEAR (obj_session_hdr_temp);

  FIID_OBJ_SET (obj_session_hdr_temp, 
                "ipmi_payload_len",
                payload_len);
  
  FIID_OBJ_LEN_BYTES_CLEANUP(obj_len, obj_session_hdr_temp);
  _BUF_SPACE_CHECK(obj_len, (pkt_len - indx));
  FIID_OBJ_GET_ALL_LEN_CLEANUP(obj_len, obj_session_hdr_temp, pkt + indx, pkt_len - indx);
  indx += obj_len;

  /* 
   * Copy IPMI Payload into packet
   */

  FIID_OBJ_LEN_BYTES_CLEANUP(obj_len, obj_payload);
  _BUF_SPACE_CHECK(obj_len, (pkt_len - indx));
  FIID_OBJ_GET_ALL_LEN_CLEANUP(obj_len, obj_payload, pkt + indx, pkt_len - indx);
  indx += obj_len;

  if (session_id)
    {
      uint8_t authentication_code_buf[IPMI_MAX_PAYLOAD_LENGTH];
      int32_t authentication_code_len;

      if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm))
        {
          errno = EINVAL;
          return (-1);
        }

      FIID_OBJ_TEMPLATE_COMPARE_CLEANUP(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

      FIID_OBJ_DUP_CLEANUP (obj_rmcpplus_session_trlr_temp, obj_rmcpplus_session_trlr);

      ERR (!(_construct_session_trlr_pad(integrity_algorithm,
                                         (indx - obj_rmcp_hdr_len),
                                         obj_rmcpplus_session_trlr_temp) < 0));

      FIID_OBJ_BLOCK_LEN_BYTES_CLEANUP(len,
                                       obj_rmcpplus_session_trlr_temp,
                                       (uint8_t *)"integrity_pad",
                                       (uint8_t *)"next_header");
      _BUF_SPACE_CHECK(len, (pkt_len - indx));
      FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
                                     obj_rmcpplus_session_trlr_temp,
                                     (uint8_t *)"integrity_pad",
                                     (uint8_t *)"next_header",
                                     pkt + indx,
                                     pkt_len - indx);
      indx += len;

      if (payload_authenticated)
        {
          /* achu: Note that the integrity code is all data prior to the authentication_code, so this 
           * call must be done after the pad, pad length, and next header are copied into 
           * the pkt buffer.
           */
          ERR (!((authentication_code_len = _construct_session_trlr_authentication_code(integrity_algorithm,
                                                                                        integrity_key,
                                                                                        integrity_key_len,
                                                                                        authentication_code_data,
                                                                                        authentication_code_data_len,
                                                                                        obj_rmcpplus_session_trlr_temp,
                                                                                        pkt + obj_rmcp_hdr_len,
                                                                                        indx - obj_rmcp_hdr_len,
                                                                                        authentication_code_buf,
                                                                                        IPMI_MAX_PAYLOAD_LENGTH)) < 0));
          
          if (authentication_code_len)
            {
              _BUF_SPACE_CHECK(authentication_code_len, (pkt_len - indx));
              memcpy(pkt + indx, authentication_code_buf, authentication_code_len);
              indx += authentication_code_len;
            }
        }
    }

  rv = indx;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_payload);
  FIID_OBJ_DESTROY_NO_RETURN(obj_session_hdr_temp);
  FIID_OBJ_DESTROY_NO_RETURN(obj_rmcpplus_session_trlr_temp);
  return (rv);
}

static int32_t
_deconstruct_payload_buf(fiid_obj_t obj_lan_msg_hdr,
                         fiid_obj_t obj_cmd,
                         fiid_obj_t obj_lan_msg_trlr,
                         uint8_t *pkt,
                         uint32_t lan_msg_len)
{
  int32_t obj_lan_msg_trlr_len, obj_cmd_len;
  int32_t len;
  unsigned int indx = 0;

  if (!fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_lan_msg_trlr)
      || !pkt 
      || !lan_msg_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_buf: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  ERR_EXIT (!((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));

  FIID_OBJ_CLEAR (obj_lan_msg_hdr);
  FIID_OBJ_SET_ALL_LEN(len, 
                       obj_lan_msg_hdr,
                       pkt + indx,
                       lan_msg_len - indx);
  indx += len;

  if (indx >= lan_msg_len)
    return 0;
  
  /* achu: Whatever is in between the header and the trailer is the
   * command data
   */

  if ((lan_msg_len - indx) >= obj_lan_msg_trlr_len)
    obj_cmd_len = (lan_msg_len - indx) - obj_lan_msg_trlr_len;
  else
    obj_cmd_len = 0;

  if (obj_cmd_len)
    {
      FIID_OBJ_CLEAR (obj_cmd);
      FIID_OBJ_SET_ALL_LEN (len,
                            obj_cmd,
                            pkt + indx,
                            obj_cmd_len);
      indx += len;

      if (indx >= lan_msg_len)
        return 0;
    }

  FIID_OBJ_CLEAR (obj_lan_msg_trlr);
  FIID_OBJ_SET_ALL_LEN (len,
                        obj_lan_msg_trlr,
                        pkt + indx,
                        lan_msg_len - indx);
                    
  return (0);
}

static int32_t
_deconstruct_payload_confidentiality_none(fiid_obj_t obj_payload,
                                          fiid_obj_t obj_lan_msg_hdr,
                                          fiid_obj_t obj_cmd,
                                          fiid_obj_t obj_lan_msg_trlr,
                                          uint8_t *pkt,
                                          uint32_t ipmi_payload_len)
{
  if (!fiid_obj_valid(obj_payload)
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_lan_msg_trlr)
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_confidentiality_none: Invalid parameters");
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  /* achu: No encryption, so ipmi_payload_len is the length of 
   * the msg_hdr, cmd, and msg_trlr.
   */
  ERR (!(_deconstruct_payload_buf(obj_lan_msg_hdr,
                                  obj_cmd,
                                  obj_lan_msg_trlr,
                                  pkt,
                                  ipmi_payload_len) < 0));

  FIID_OBJ_CLEAR(obj_payload);
      
  FIID_OBJ_SET_DATA(obj_payload,
                    "payload_data",
                    pkt,
                    ipmi_payload_len);

  return (0);
}

static int32_t
_deconstruct_payload_confidentiality_aes_cbc_128(uint8_t payload_encrypted,
                                                 fiid_obj_t obj_payload,
                                                 fiid_obj_t obj_lan_msg_hdr,
                                                 fiid_obj_t obj_cmd,
                                                 fiid_obj_t obj_lan_msg_trlr,
                                                 uint8_t *confidentiality_key,
                                                 uint32_t confidentiality_key_len,
                                                 uint8_t *pkt,
                                                 uint32_t ipmi_payload_len)
{
  uint8_t iv[IPMI_AES_CBC_128_IV_LENGTH];
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_len;
  int cipher_keylen, cipher_blocklen;
  int32_t payload_data_len, decrypt_len, cmd_data_len, indx = 0;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  if (payload_encrypted != IPMI_PAYLOAD_FLAG_ENCRYPTED
      || !fiid_obj_valid(obj_payload)
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_lan_msg_trlr)
      || !confidentiality_key
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_confidentiality_aes_cbc_128: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  ERR (!((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (!(cipher_keylen < IPMI_AES_CBC_128_KEY_LENGTH));

  if (confidentiality_key_len < IPMI_AES_CBC_128_KEY_LENGTH)
    {
      errno = EINVAL;
      return (-1);
    }
  confidentiality_key_len = IPMI_AES_CBC_128_KEY_LENGTH;

  ERR (!((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (cipher_blocklen == IPMI_AES_CBC_128_BLOCK_LENGTH);
     
  if (ipmi_payload_len < IPMI_AES_CBC_128_BLOCK_LENGTH)
    {
      errno = EINVAL;
      return (-1);
    }

  payload_data_len = ipmi_payload_len - IPMI_AES_CBC_128_BLOCK_LENGTH;

  if (payload_data_len <= 0)
    {
      errno = EINVAL;
      return (-1);
    }

  memcpy(iv, pkt, IPMI_AES_CBC_128_BLOCK_LENGTH);
  indx += IPMI_AES_CBC_128_BLOCK_LENGTH;
  memcpy(payload_buf, pkt + indx, payload_data_len);

  FIID_OBJ_CLEAR (obj_payload);
  FIID_OBJ_SET_DATA(obj_payload,
                    "confidentiality_header",
                    iv,
                    IPMI_AES_CBC_128_BLOCK_LENGTH);

  ERR (!((decrypt_len = ipmi_crypt_cipher_decrypt(IPMI_CRYPT_CIPHER_AES,
                                                  IPMI_CRYPT_CIPHER_MODE_CBC,
                                                  confidentiality_key,
                                                  confidentiality_key_len,
                                                  iv,
                                                  IPMI_AES_CBC_128_BLOCK_LENGTH,
                                                  payload_buf,
                                                  payload_data_len)) < 0));
  ERR (!((decrypt_len != payload_data_len)));

  pad_len = payload_buf[payload_data_len - 1];
  if (pad_len > IPMI_AES_CBC_128_BLOCK_LENGTH)
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

  FIID_OBJ_SET_DATA(obj_payload,
                    "payload_data",
                    payload_buf,
                    cmd_data_len);

  FIID_OBJ_SET_DATA(obj_payload,
                    "confidentiality_trailer",
                    payload_buf + cmd_data_len,
                    pad_len + 1);
  
  /* achu: User is responsible for checking if padding is not corrupt  */
  
  ERR (!(_deconstruct_payload_buf(obj_lan_msg_hdr, 
                                  obj_cmd, 
                                  obj_lan_msg_trlr,
                                  payload_buf,
                                  cmd_data_len) < 0));

  return (0);
}

static int32_t
_deconstruct_payload_special(uint8_t payload_type,
                             uint8_t authentication_algorithm,
                             fiid_obj_t obj_payload,
                             fiid_obj_t obj_lan_msg_hdr,
                             fiid_obj_t obj_cmd,
                             fiid_obj_t obj_lan_msg_trlr,
                             uint8_t *pkt,
                             uint32_t ipmi_payload_len)
{
  if ((payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
      || (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
          && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_rs) != 1)
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
          && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) != 1)
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4
          && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) != 1)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !fiid_obj_valid(obj_payload)
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_lan_msg_trlr)
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload_special: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  FIID_OBJ_CLEAR(obj_payload);
  FIID_OBJ_SET_DATA(obj_payload,
                    "payload_data",
                    pkt,
                    ipmi_payload_len);
  
  FIID_OBJ_CLEAR(obj_lan_msg_trlr);

  FIID_OBJ_CLEAR(obj_cmd);
  FIID_OBJ_SET_ALL(obj_cmd,
                   pkt,
                   ipmi_payload_len);
  
  FIID_OBJ_CLEAR(obj_lan_msg_hdr);

  return (0);
}

static int32_t
_deconstruct_payload(uint8_t payload_type,
                     uint8_t payload_encrypted,
                     uint8_t authentication_algorithm,
                     uint8_t confidentiality_algorithm,
                     fiid_obj_t obj_payload,
                     fiid_obj_t obj_lan_msg_hdr,
                     fiid_obj_t obj_cmd,
                     fiid_obj_t obj_lan_msg_trlr,
                     uint8_t *confidentiality_key,
                     uint32_t confidentiality_key_len,
                     uint8_t *pkt,
                     uint32_t ipmi_payload_len)
{
  if ((payload_type != IPMI_PAYLOAD_TYPE_IPMI
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
      || !IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_encrypted)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || (confidentiality_algorithm != IPMI_CONFIDENTIALITY_ALGORITHM_NONE
          && confidentiality_algorithm != IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
      || !fiid_obj_valid(obj_payload)
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_lan_msg_trlr)
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_deconstruct_payload: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

  /* First determine if this is a special payload_type 
   * 
   * Note: We don't check consider OPEN_SESSION_REQUEST, RAKP1 or
   * RAKP3 special b/c they are requests, not responses
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _deconstruct_payload_confidentiality_none(obj_payload,
                                                         obj_lan_msg_hdr,
                                                         obj_cmd,
                                                         obj_lan_msg_trlr,
                                                         pkt,
                                                         ipmi_payload_len);
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return _deconstruct_payload_confidentiality_aes_cbc_128(payload_encrypted,
                                                                obj_payload,
                                                                obj_lan_msg_hdr,
                                                                obj_cmd,
                                                                obj_lan_msg_trlr,
                                                                confidentiality_key,
                                                                confidentiality_key_len,
                                                                pkt,
                                                                ipmi_payload_len);
    }
  else
    return _deconstruct_payload_special(payload_type,
                                        authentication_algorithm,
                                        obj_payload,
                                        obj_lan_msg_hdr,
                                        obj_cmd,
                                        obj_lan_msg_trlr,
                                        pkt,
                                        ipmi_payload_len);

  /* NOT REACHED */
  return (0);
}

/* XXX: Should bother with obj_payload?  How else do you check the confidentiality pad? */
int32_t
unassemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm,
                              uint8_t integrity_algorithm,
                              uint8_t confidentiality_algorithm,
                              uint8_t *integrity_key,
                              uint32_t integrity_key_len,
                              uint8_t *confidentiality_key,
                              uint32_t confidentiality_key_len,
                              uint8_t *pkt,
                              uint32_t pkt_len,
                              fiid_obj_t obj_rmcp_hdr,
                              fiid_obj_t obj_rmcpplus_session_hdr,
                              fiid_obj_t obj_payload,
                              fiid_obj_t obj_lan_msg_hdr,
                              fiid_obj_t obj_cmd,
                              fiid_obj_t obj_lan_msg_trlr,
                              fiid_obj_t obj_rmcpplus_session_trlr)
{
  unsigned int indx = 0;
  int32_t obj_rmcp_hdr_len, obj_len;
  uint64_t payload_type, payload_authenticated, payload_encrypted, 
    session_id, session_sequence_number, ipmi_payload_len;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !pkt
      || !fiid_obj_valid(obj_rmcp_hdr)
      || !fiid_obj_valid(obj_rmcpplus_session_hdr)
      || !fiid_obj_valid(obj_payload)
      || !fiid_obj_valid(obj_lan_msg_hdr)
      || !fiid_obj_valid(obj_cmd)
      || !fiid_obj_valid(obj_lan_msg_trlr)
      || !fiid_obj_valid(obj_rmcpplus_session_trlr))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
  FIID_OBJ_TEMPLATE_COMPARE(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  /*
   * Extract RMCP header
   */
  FIID_OBJ_CLEAR(obj_rmcp_hdr);
  FIID_OBJ_SET_ALL_LEN(obj_rmcp_hdr_len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += obj_rmcp_hdr_len;

  if (pkt_len <= indx)
    return 0;

  /*
   * Extract auth_type and payload information 
   */
  FIID_OBJ_CLEAR(obj_rmcpplus_session_hdr);
  FIID_OBJ_SET_BLOCK_LEN(obj_len, 
                         obj_rmcpplus_session_hdr, 
                         (uint8_t *)"authentication_type",
                         (uint8_t *)"payload_type.encrypted",
                         pkt + indx, 
                         pkt_len - indx);
  indx += obj_len;

  if (pkt_len <= indx)
    return 0;

  FIID_OBJ_GET (obj_rmcpplus_session_hdr, "payload_type", &payload_type);
  
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
      FIID_OBJ_SET_BLOCK_LEN(obj_len,
                             obj_rmcpplus_session_hdr,
                             (uint8_t *)"oem_iana",
                             (uint8_t *)"oem_payload_id",
                             pkt + indx,
                             pkt_len - indx);
      indx += obj_len;
      
      if (pkt_len <= indx)
        return 0;
    }

  /* 
   * Extract Session ID, Session Sequence Number, and Payload Length
   */
  FIID_OBJ_SET_BLOCK_LEN(obj_len,
                         obj_rmcpplus_session_hdr,
                         (uint8_t *)"session_id",
                         (uint8_t *)"ipmi_payload_len",
                         pkt + indx,
                         pkt_len - indx);
  indx += obj_len;
  
  if (pkt_len <= indx)
    return 0;

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.authenticated",
                &payload_authenticated);
  
  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.encrypted",
                &payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_id",
                &session_id);
  
  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_sequence_number",
                &session_sequence_number);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "ipmi_payload_len",
                &ipmi_payload_len);

  if (((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
        || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
        || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
       && (payload_authenticated 
	   || payload_encrypted 
	   || session_id 
	   || session_sequence_number))
      || (session_id 
          && payload_authenticated
          && (integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
              && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
              && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_MD5_128))
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
          && payload_encrypted)
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if (pkt_len - indx < ipmi_payload_len)
    {
      ipmi_debug("unassemble_ipmi_rmcpplus_pkt: Shorten payload length");
      ipmi_payload_len = pkt_len - indx;
    }

  /* 
   * Deconstruct/Decrypt Payload
   */
  ERR (!(_deconstruct_payload(payload_type,
                              payload_encrypted,
                              authentication_algorithm,
                              confidentiality_algorithm,
                              obj_payload,
                              obj_lan_msg_hdr,
                              obj_cmd,
                              obj_lan_msg_trlr,
                              confidentiality_key,
                              confidentiality_key_len,
                              pkt + indx,
                              ipmi_payload_len) < 0));
  indx += ipmi_payload_len;

  if (pkt_len <= indx)
    return 0;
  
  FIID_OBJ_CLEAR (obj_rmcpplus_session_trlr);

  if (session_id)
    {
      int32_t pad_length_field_len, next_header_field_len;
      uint32_t authentication_code_len;
      uint64_t pad_length;

      if (payload_authenticated)
        {
          if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
            authentication_code_len = 0;
          else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
            authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
          else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
            authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
          else /* IPMI_INTEGRITY_ALGORITHM_MD5_128 */
            authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
        }
      else
        authentication_code_len = 0;

      ERR_EXIT (!((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "pad_length")) < 0)); 
      ERR_EXIT (!((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "next_header")) < 0));
      
      /* achu: There needs to be atleast the next_header and pad_len fields */
      if ((pkt_len - indx) < (authentication_code_len + pad_length_field_len + next_header_field_len))
        return 0;

      if (authentication_code_len)
        FIID_OBJ_SET_DATA(obj_rmcpplus_session_trlr,
                          (uint8_t *)"authentication_code",
                          pkt + indx + ((pkt_len - indx) - authentication_code_len),
                          authentication_code_len);

      FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                         "next_header",
                         pkt + indx + ((pkt_len - indx) - authentication_code_len - next_header_field_len),
                         next_header_field_len);
      
      FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                         "pad_length",
                         pkt + indx + ((pkt_len - indx) - authentication_code_len - next_header_field_len - pad_length_field_len),
                         pad_length_field_len);
      
      FIID_OBJ_GET (obj_rmcpplus_session_trlr,
                    "pad_length",
                    &pad_length);

      if (pad_length > IPMI_INTEGRITY_PAD_MULTIPLE)
	{
	  errno = EINVAL;
	  return (-1);
	}
      
      if (pad_length >= (pkt_len - indx - authentication_code_len - pad_length_field_len - next_header_field_len))
        pad_length = (pkt_len - indx - authentication_code_len - pad_length_field_len - next_header_field_len);
      
      FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                         "integrity_pad",
                         pkt + indx,
                         pad_length);
    }

  return (0);
}
