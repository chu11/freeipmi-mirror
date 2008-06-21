/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/util/ipmi-rmcpplus-util.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"

#include "libcommon/ipmi-crypt.h"
#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"
#include "secure.h"

#define IPMI_KEY_CONSTANT_LENGTH                          20
#define IPMI_MAX_K1_LENGTH                                32
#define IPMI_MAX_K2_LENGTH                                32
#define IPMI_MAX_SIK_KEY_LENGTH                           32
#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH  32
#define IPMI_MAX_INTEGRITY_CHECK_VALUE_LENGTH             64
#define IPMI_MAX_KEY_DATA_LENGTH                          1024
#define IPMI_MAX_INTEGRITY_PAD_LENGTH                     8
#define IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH           64
#define IPMI_MAX_INTEGRITY_DATA_LENGTH                    32

int32_t 
ipmi_calculate_sik(uint8_t authentication_algorithm,
                   uint8_t *k_g,
                   uint32_t k_g_len,
                   uint8_t *remote_console_random_number,
                   uint32_t remote_console_random_number_len,
                   uint8_t *managed_system_random_number,
                   uint32_t managed_system_random_number_len,
                   uint8_t name_only_lookup, 
                   uint8_t requested_privilege_level,
                   char *user_name,
                   uint8_t user_name_len,
                   uint8_t *sik,
                   uint32_t sik_len)
{
  int hash_algorithm, hash_flags, expected_digest_len, crypt_digest_len, 
    computed_digest_len;
  unsigned int hash_data_len;
  uint8_t hash_data[IPMI_MAX_KEY_DATA_LENGTH];
  uint8_t priv_byte = 0;
  int32_t rv = -1;

  /* k_g can be NULL, indicating a empty k_g */
  ERR_EINVAL ((authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
	      && !(k_g && !k_g_len)
	      && !(k_g && k_g_len > IPMI_MAX_K_G_LENGTH)
	      && remote_console_random_number
	      && !(remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
	      && managed_system_random_number
	      && !(managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
              && IPMI_USER_NAME_LOOKUP_VALID(name_only_lookup)
	      && IPMI_PRIVILEGE_LEVEL_VALID(requested_privilege_level)
	      && !(user_name && !user_name_len)
	      && sik
	      && sik_len);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
    }
  else /* IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 */
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
    }

  ERR_CLEANUP (!((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0));

  ERR_EXIT (crypt_digest_len == expected_digest_len);

  ERR_EINVAL_CLEANUP (!(sik_len < expected_digest_len));

  k_g_len = (k_g_len > IPMI_MAX_SIK_KEY_LENGTH) ? IPMI_MAX_SIK_KEY_LENGTH : k_g_len;
  
  memset(hash_data, '\0', IPMI_MAX_KEY_DATA_LENGTH);

  /* 
   * Build up data for hashing.
   */
  
  hash_data_len = 0;
  memcpy(hash_data + hash_data_len, 
	 (void *)remote_console_random_number, 
	 remote_console_random_number_len);
  hash_data_len += remote_console_random_number_len;
  
  memcpy(hash_data + hash_data_len, 
	 (void *)managed_system_random_number,
	 managed_system_random_number_len);
  hash_data_len += managed_system_random_number_len;

  /* This part of the spec is wierd, gotta hack it out */
  if (name_only_lookup)
     priv_byte |= 0x10;
  priv_byte |= (requested_privilege_level & 0xF);

  memcpy(hash_data + hash_data_len, 
	 (void *)&priv_byte, 
	 sizeof(uint8_t));
  hash_data_len += sizeof(uint8_t);

  memcpy(hash_data + hash_data_len, 
	 (void *)&user_name_len, 
	 sizeof(uint8_t));
  hash_data_len += sizeof(uint8_t);

  if (user_name && user_name_len > 0)
    {
      memcpy(hash_data + hash_data_len, (void *)user_name, user_name_len);
      hash_data_len += user_name_len;
    }

  ERR_CLEANUP (!((computed_digest_len =  ipmi_crypt_hash(hash_algorithm,
                                                         hash_flags,
                                                         k_g,
                                                         k_g_len,
                                                         hash_data,
                                                         hash_data_len,
                                                         sik,
                                                         sik_len)) < 0));

  ERR_CLEANUP (!(computed_digest_len != crypt_digest_len));

  rv = computed_digest_len;
 cleanup:
  secure_memset(hash_data, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  return (rv);
}

static int32_t
_calculate_k_rakp_hmac(int hash_algorithm,
                       unsigned int expected_digest_len,
                       uint8_t *sik_key,
                       uint32_t sik_key_len,
                       uint8_t *k,
                       uint32_t k_len,
                       uint8_t *constant,
                       uint32_t constant_len)
{
  int computed_digest_len;
  int32_t crypt_digest_len;
  
  ERR_EINVAL (IPMI_CRYPT_HASH_ALGORITHM_VALID(hash_algorithm)
	      && sik_key
	      && sik_key_len
	      && k
	      && k_len
	      && constant
	      && constant_len
	      && !(sik_key_len < expected_digest_len)
	      && !(k_len < expected_digest_len)
	      && !(constant_len < IPMI_KEY_CONSTANT_LENGTH));

  ERR (!((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0));
  ERR (crypt_digest_len == expected_digest_len);

  /* SPEC: achu: I believe the length of the constant you pass in
   * is the digest_len, atleast according to IPMI 2.0 Spec Section
   * 13.32, "constants are constructed using a hexadecimal octet
   * value repeated up to the HMAC block size in length starting
   * with the constant 01h".
   */
  ERR (!((computed_digest_len =  ipmi_crypt_hash(hash_algorithm,
						 IPMI_CRYPT_HASH_FLAGS_HMAC,
						 sik_key,
						 crypt_digest_len,
						 constant,
						 IPMI_KEY_CONSTANT_LENGTH,
						 k,
						 k_len)) < 0));

  ERR (!(computed_digest_len != crypt_digest_len));

  return (computed_digest_len);
}

static int32_t
_calculate_k_rakp_hmac_sha1(uint8_t *sik_key,
                            uint32_t sik_key_len,
                            uint8_t *k,
                            uint32_t k_len,
                            uint8_t *constant,
                            uint32_t constant_len)
{
  ERR_EINVAL (sik_key
	      && sik_key_len
	      && k
	      && k_len
	      && constant
	      && constant_len
	      && !(constant_len < IPMI_KEY_CONSTANT_LENGTH));

  return _calculate_k_rakp_hmac(IPMI_CRYPT_HASH_SHA1,
                                IPMI_HMAC_SHA1_DIGEST_LENGTH,
                                sik_key,
                                sik_key_len,
                                k,
                                k_len,
                                constant,
                                constant_len);
}

static int32_t
_calculate_k_rakp_hmac_md5(uint8_t *sik_key,
                           uint32_t sik_key_len,
                           uint8_t *k,
                           uint32_t k_len,
                           uint8_t *constant,
                           uint32_t constant_len)
{
  ERR_EINVAL (sik_key
	      && sik_key_len
	      && k
	      && k_len
	      && constant
	      && constant_len
	      && !(constant_len < IPMI_KEY_CONSTANT_LENGTH));
  
  return _calculate_k_rakp_hmac(IPMI_CRYPT_HASH_MD5,
                                IPMI_HMAC_MD5_DIGEST_LENGTH,
                                sik_key,
                                sik_key_len,
                                k,
                                k_len,
                                constant,
                                constant_len);
}

static int32_t
_ipmi_calculate_k(uint8_t authentication_algorithm,
                  uint8_t *sik_key,
                  uint32_t sik_key_len,
                  uint8_t *k,
                  uint32_t k_len,
                  uint8_t *constant,
                  uint32_t constant_len)
{
  ERR_EINVAL ((authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
	      && sik_key 
              && sik_key_len
	      && k
	      && k_len
	      && constant
	      && constant_len
	      && !(constant_len < IPMI_KEY_CONSTANT_LENGTH));

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    return _calculate_k_rakp_hmac_sha1(sik_key, 
                                       sik_key_len, 
                                       k, 
                                       k_len, 
                                       constant, 
                                       constant_len);
  else /* IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 */
    return _calculate_k_rakp_hmac_md5(sik_key, 
                                      sik_key_len, 
                                      k, 
                                      k_len, 
                                      constant, 
                                      constant_len);
}

int32_t
ipmi_calculate_k1(uint8_t authentication_algorithm,
                  uint8_t *sik_key,
                  uint32_t sik_key_len,
                  uint8_t *k1,
                  uint32_t k1_len)
{
  uint8_t constant[IPMI_KEY_CONSTANT_LENGTH] = { 0x01, 0x01, 0x01, 0x01, 0x01, 
						 0x01, 0x01, 0x01, 0x01, 0x01, 
						 0x01, 0x01, 0x01, 0x01, 0x01, 
						 0x01, 0x01, 0x01, 0x01, 0x01}; 
  
  return _ipmi_calculate_k(authentication_algorithm,
                           sik_key,
                           sik_key_len,
                           k1,
                           k1_len,
                           constant,
                           IPMI_KEY_CONSTANT_LENGTH);                          
}

int32_t
ipmi_calculate_k2(uint8_t authentication_algorithm,
                  uint8_t *sik_key,
                  uint32_t sik_key_len,
                  uint8_t *k1,
                  uint32_t k1_len)
{
  uint8_t constant[IPMI_KEY_CONSTANT_LENGTH] = { 0x02, 0x02, 0x02, 0x02, 0x02, 
						 0x02, 0x02, 0x02, 0x02, 0x02, 
						 0x02, 0x02, 0x02, 0x02, 0x02, 
						 0x02, 0x02, 0x02, 0x02, 0x02}; 
  return _ipmi_calculate_k(authentication_algorithm,
                           sik_key,
                           sik_key_len,
                           k1,
                           k1_len,
                           constant,
                           IPMI_KEY_CONSTANT_LENGTH);  
}

int32_t
ipmi_calculate_rmcpplus_session_keys(uint8_t authentication_algorithm,
                                     uint8_t integrity_algorithm,
                                     uint8_t confidentiality_algorithm,
                                     uint8_t *authentication_code_data,
                                     uint32_t authentication_code_data_len,
                                     uint8_t *k_g,
                                     uint32_t k_g_len,
                                     uint8_t *remote_console_random_number, 
                                     uint32_t remote_console_random_number_len, 
                                     uint8_t *managed_system_random_number,
                                     uint32_t managed_system_random_number_len,
                                     uint8_t name_only_lookup,
                                     uint8_t requested_privilege_level,
                                     char *user_name, 
                                     uint8_t user_name_len,
                                     uint8_t **sik_key,
                                     uint32_t *sik_key_len,
                                     uint8_t **integrity_key,
                                     uint32_t *integrity_key_len,
                                     uint8_t **confidentiality_key,
                                     uint32_t *confidentiality_key_len)
{
  uint8_t *sik_key_buf;
  int32_t sik_key_buf_len;
  uint8_t *integrity_key_buf;
  uint32_t integrity_key_buf_len;
  uint8_t *confidentiality_key_buf;
  uint32_t confidentiality_key_buf_len;
  uint8_t k_g_buf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  uint8_t *k_g_ptr;
  uint32_t k_g_ptr_len;
  uint8_t sik[IPMI_MAX_SIK_KEY_LENGTH];
  int32_t sik_len;
  uint8_t k1[IPMI_MAX_K1_LENGTH];
  int32_t k1_len;
  uint8_t k2[IPMI_MAX_K2_LENGTH];
  int32_t k2_len;
  int32_t rv = -1;

  ERR_EINVAL (IPMI_CIPHER_SUITE_COMBINATION_VALID(authentication_algorithm,
                                                  integrity_algorithm,
                                                  confidentiality_algorithm)
              && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
	      && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
	      && (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
		  || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
	      && sik_key
	      && (*sik_key)
	      && sik_key_len
	      && (*sik_key_len)
	      && integrity_key
	      && (*integrity_key)
	      && integrity_key_len
	      && (*integrity_key_len)
	      && confidentiality_key
	      && (*confidentiality_key)
	      && confidentiality_key_len
	      && (*confidentiality_key_len));

  sik_key_buf = *sik_key;
  sik_key_buf_len = *sik_key_len;
  integrity_key_buf = *integrity_key;
  integrity_key_buf_len = *integrity_key_len;
  confidentiality_key_buf = *confidentiality_key;
  confidentiality_key_buf_len = *confidentiality_key_len;
 
  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      sik_key_buf = NULL;
      sik_key_buf_len = 0;
    }
  else /* 
          authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
          || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
       */
    {
      ERR_EINVAL_CLEANUP (!(authentication_code_data_len && !authentication_code_data)
                          && !(authentication_code_data && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
                          && !(k_g_len && !k_g)
                          && !(k_g && k_g_len > IPMI_MAX_K_G_LENGTH)
                          && remote_console_random_number
                          && !(remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
                          && managed_system_random_number
                          && !(managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
                          && IPMI_USER_NAME_LOOKUP_VALID(name_only_lookup)
                          && IPMI_PRIVILEGE_LEVEL_VALID(requested_privilege_level)
                          && !(user_name && !user_name_len));

      if (k_g && k_g_len)
	{
	  k_g_ptr = k_g;
	  k_g_ptr_len = k_g_len;
	}
      else
	{
          if (authentication_code_data && authentication_code_data_len)
            {
              k_g_ptr = authentication_code_data;
              k_g_ptr_len = authentication_code_data_len;
            }
          else
            {
              memset(k_g_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
              k_g_ptr = k_g_buf;
              k_g_ptr_len = IPMI_2_0_MAX_PASSWORD_LENGTH;
            }
	}
      
      ERR_CLEANUP (!((sik_len = ipmi_calculate_sik(authentication_algorithm,
                                                   k_g_ptr,
                                                   k_g_ptr_len,
                                                   remote_console_random_number,
                                                   remote_console_random_number_len,
                                                   managed_system_random_number,
                                                   managed_system_random_number_len,
                                                   name_only_lookup,
                                                   requested_privilege_level,
                                                   user_name,
                                                   user_name_len,
                                                   sik,
                                                   IPMI_MAX_SIK_KEY_LENGTH)) < 0));
      ERR_EMSGSIZE_CLEANUP (!(sik_key_buf_len < sik_len));
      
      memcpy(sik_key_buf, sik, sik_len);
      sik_key_buf_len = sik_len;
    }

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    {
      integrity_key_buf = NULL;
      integrity_key_buf_len = 0;
    }
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
	   || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    {    
      ERR_CLEANUP (!((k1_len = ipmi_calculate_k1(authentication_algorithm,
                                                 sik_key_buf,
                                                 sik_key_buf_len,
                                                 k1,
                                                 IPMI_MAX_K1_LENGTH)) < 0));
      
      ERR_EMSGSIZE_CLEANUP (!(integrity_key_buf_len < k1_len));
      
      memcpy(integrity_key_buf, k1, k1_len);
      integrity_key_buf_len = k1_len;
    }
  else /* IPMI_INTEGRITY_ALGORITHM_MD5_128 */
    {
      ERR_EMSGSIZE_CLEANUP (!(authentication_code_data
                              && (!authentication_code_data_len
                                  || integrity_key_buf_len < authentication_code_data_len)));
      
      memset(integrity_key_buf, '\0', integrity_key_buf_len);
      if (authentication_code_data)
	{
	  memcpy(integrity_key_buf,
		 authentication_code_data,
		 authentication_code_data_len);
	  integrity_key_buf_len = authentication_code_data_len;
	}
      else
        integrity_key_buf_len = 0;
    }
  
  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    {
      confidentiality_key_buf = NULL;
      confidentiality_key_buf_len = 0;
    }
  else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
    {
      ERR_CLEANUP (!((k2_len = ipmi_calculate_k2(authentication_algorithm,
                                                 sik_key_buf,
                                                 sik_key_buf_len,
                                                 k2,
                                                 IPMI_MAX_K2_LENGTH)) < 0));
      
      ERR_EMSGSIZE_CLEANUP (!(confidentiality_key_buf_len < k2_len));
      
      memcpy(confidentiality_key_buf, k2, k2_len);
      confidentiality_key_buf_len = k2_len;
    }
  
  *sik_key = sik_key_buf;
  *sik_key_len = sik_key_buf_len;
  *integrity_key = integrity_key_buf;
  *integrity_key_len = integrity_key_buf_len;
  *confidentiality_key = confidentiality_key_buf;
  *confidentiality_key_len = confidentiality_key_buf_len;
  rv = 0; 
 cleanup:
  secure_memset(k_g_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
  secure_memset(sik, '\0', IPMI_MAX_SIK_KEY_LENGTH);
  secure_memset(k1, '\0', IPMI_MAX_K1_LENGTH);
  secure_memset(k2, '\0', IPMI_MAX_K2_LENGTH);
  return (rv);
}

int32_t 
ipmi_calculate_rakp_3_key_exchange_authentication_code(int8_t authentication_algorithm, 
                                                       uint8_t *k_uid, 
                                                       uint32_t k_uid_len, 
                                                       uint8_t *managed_system_random_number, 
                                                       uint32_t managed_system_random_number_len, 
                                                       uint32_t remote_console_session_id, 
                                                       uint8_t name_only_lookup, 
                                                       uint8_t requested_privilege_level, 
                                                       char *user_name, 
                                                       uint8_t user_name_length, 
                                                       uint8_t *key_exchange_authentication_code, 
                                                       uint32_t key_exchange_authentication_code_len)
{
  uint8_t priv_byte = 0;
  uint8_t k_uid_buf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  uint8_t *k_uid_ptr;
  uint32_t k_uid_ptr_len;
  uint8_t buf[IPMI_MAX_KEY_DATA_LENGTH];
  uint32_t buf_index = 0;
  uint8_t digest[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  uint8_t hash_algorithm, hash_flags;
  int32_t digest_len, expected_digest_len;
  int32_t rv = -1;

  ERR_EINVAL ((authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
	      && !(k_uid && !k_uid_len)
	      && !(k_uid && k_uid_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
	      && managed_system_random_number
	      && !(managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
              && IPMI_USER_NAME_LOOKUP_VALID(name_only_lookup)
	      && IPMI_PRIVILEGE_LEVEL_VALID(requested_privilege_level)
	      && !(user_name && user_name_length > IPMI_MAX_USER_NAME_LENGTH)
	      && !(!user_name && user_name_length)
	      && key_exchange_authentication_code);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      memset(key_exchange_authentication_code, '\0', key_exchange_authentication_code_len);
      return (0);
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      ERR_EINVAL_CLEANUP (!(key_exchange_authentication_code_len < IPMI_HMAC_SHA1_DIGEST_LENGTH));

      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else /* IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 */
    {
      ERR_EINVAL_CLEANUP (!(key_exchange_authentication_code_len < IPMI_HMAC_MD5_DIGEST_LENGTH));

      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }

  memset(buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  memcpy(buf + buf_index, managed_system_random_number, IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH);
  buf_index += IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH;
  buf[buf_index] = (remote_console_session_id & 0x000000ff);
  buf_index++;
  buf[buf_index] = (remote_console_session_id & 0x0000ff00) >> 8;
  buf_index++;
  buf[buf_index] = (remote_console_session_id & 0x00ff0000) >> 16;
  buf_index++;
  buf[buf_index] = (remote_console_session_id & 0xff000000) >> 24;
  buf_index++;
  /* This part of the spec is wierd, gotta hack it out */
  if (name_only_lookup)
     priv_byte |= 0x10;
  priv_byte |= (requested_privilege_level & 0xF);
  buf[buf_index] = priv_byte;
  buf_index++;
  buf[buf_index] = user_name_length;
  buf_index++;
  if (user_name && user_name_length)
    {
      memcpy(buf + buf_index, user_name, user_name_length);
      buf_index += user_name_length;
    }

  if (k_uid && k_uid_len)
    {
      k_uid_ptr = k_uid;
      k_uid_ptr_len = k_uid_len;
    }
  else
    {
      memset(k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      k_uid_ptr = k_uid_buf;
      k_uid_ptr_len = IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  ERR_CLEANUP (!((digest_len = ipmi_crypt_hash(hash_algorithm,
                                               hash_flags,
                                               k_uid_ptr,
                                               k_uid_ptr_len,
                                               buf,
                                               buf_index,
                                               digest,
                                               IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0));
  ERR_CLEANUP (digest_len == expected_digest_len);
  
  memcpy(key_exchange_authentication_code, digest, digest_len);
  rv = digest_len;
 cleanup:
  secure_memset(k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
  secure_memset(buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  secure_memset(digest, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  return (rv);
}

int8_t
ipmi_rmcpplus_check_payload_pad(uint8_t confidentiality_algorithm,
				fiid_obj_t obj_rmcpplus_payload)
{
  ERR_EINVAL ((confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
	       || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
	      && fiid_obj_valid(obj_rmcpplus_payload));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_payload, tmpl_rmcpplus_payload);

  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    /* No padding */
    return (1);
  else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
    {
      uint8_t confidentiality_trailer[IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH];
      int32_t confidentiality_trailer_len;
      int8_t pad_len;
      int i;

      FIID_OBJ_GET_DATA_LEN(confidentiality_trailer_len,
                            obj_rmcpplus_payload,
                            "confidentiality_trailer",
                            confidentiality_trailer,
                            IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH);

      if (!confidentiality_trailer_len)
	return (0);

      pad_len = confidentiality_trailer[confidentiality_trailer_len - 1];

      if ((confidentiality_trailer_len - 1) != pad_len)
	return (0);

      for (i = 0; i < pad_len; i++)
        {
          if (confidentiality_trailer[i] != i + 1)
            return (0);
        }

      return (1);
    }
}

int8_t
ipmi_rmcpplus_check_integrity_pad(fiid_obj_t obj_rmcpplus_session_trlr)
{
  uint8_t integrity_pad[IPMI_MAX_INTEGRITY_PAD_LENGTH];
  uint64_t pad_length;
  int i;
    
  ERR_EINVAL (fiid_obj_valid(obj_rmcpplus_session_trlr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  FIID_OBJ_GET(obj_rmcpplus_session_trlr, "pad_length", &pad_length);

  if (!pad_length)
    return (1);

  if (pad_length > IPMI_INTEGRITY_PAD_MULTIPLE)
    return (0);

  FIID_OBJ_GET_DATA(obj_rmcpplus_session_trlr,
                    "integrity_pad",
                    integrity_pad,
                    IPMI_MAX_INTEGRITY_PAD_LENGTH);
  
  for (i = 0; i < pad_length; i++)
    {
      if (integrity_pad[i] != IPMI_INTEGRITY_PAD_DATA)
        return (0);
    }
  
  return (1);
}

int8_t
ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code(int8_t authentication_algorithm,
                                                            uint8_t *k_uid,
                                                            uint32_t k_uid_len,
                                                            uint32_t remote_console_session_id,
                                                            uint32_t managed_system_session_id,
                                                            uint8_t *remote_console_random_number,
                                                            uint32_t remote_console_random_number_len,
                                                            uint8_t *managed_system_random_number,
                                                            uint32_t managed_system_random_number_len,
                                                            uint8_t *managed_system_guid,
                                                            uint32_t managed_system_guid_len,
                                                            uint8_t name_only_lookup,
                                                            uint8_t requested_privilege_level,
                                                            char *user_name,
                                                            uint8_t user_name_length,
                                                            fiid_obj_t obj_cmd)
{
  uint8_t priv_byte = 0;
  uint8_t k_uid_buf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  uint8_t *k_uid_ptr;
  uint32_t k_uid_ptr_len;
  uint8_t buf[IPMI_MAX_KEY_DATA_LENGTH];
  uint32_t buf_index = 0;
  uint8_t digest[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  uint8_t hash_algorithm = 0;
  uint8_t hash_flags = 0;
  int32_t digest_len = 0;
  uint8_t key_exchange_authentication_code[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  int32_t key_exchange_authentication_code_len;
  int32_t compare_len = 0;
  int8_t rv = -1;

  ERR_EINVAL ((authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
	      && !(k_uid && !k_uid_len)
	      && !(k_uid && k_uid_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
	      && remote_console_random_number
	      && !(remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
	      && managed_system_random_number
	      && !(managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
	      && managed_system_guid
	      && !(managed_system_guid_len < IPMI_MANAGED_SYSTEM_GUID_LENGTH)
              && IPMI_USER_NAME_LOOKUP_VALID(name_only_lookup)
	      && IPMI_PRIVILEGE_LEVEL_VALID(requested_privilege_level)
	      && !(user_name && user_name_length > IPMI_MAX_USER_NAME_LENGTH)
	      && !(!user_name && user_name_length)
	      && fiid_obj_valid(obj_cmd));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_rakp_message_2);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      compare_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      compare_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }

  FIID_OBJ_GET_DATA_LEN_CLEANUP(key_exchange_authentication_code_len,
                                obj_cmd,
                                "key_exchange_authentication_code",
                                key_exchange_authentication_code,
                                IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      if (key_exchange_authentication_code_len)
        rv = 0;
      else
        rv = 1;
      goto cleanup;
    }

  if (key_exchange_authentication_code_len < compare_len)
    {
      rv = 0;
      goto cleanup;
    }
 
  memset(buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  buf[buf_index] = (remote_console_session_id & 0x000000ff);
  buf_index++;
  buf[buf_index] = (remote_console_session_id & 0x0000ff00) >> 8;
  buf_index++;
  buf[buf_index] = (remote_console_session_id & 0x00ff0000) >> 16;
  buf_index++;
  buf[buf_index] = (remote_console_session_id & 0xff000000) >> 24;
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0x000000ff);
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0x0000ff00) >> 8;
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0x00ff0000) >> 16;
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0xff000000) >> 24;
  buf_index++;
  memcpy(buf + buf_index, remote_console_random_number, IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH);
  buf_index += IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH;
  memcpy(buf + buf_index, managed_system_random_number, IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH);
  buf_index += IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH;
  memcpy(buf + buf_index, managed_system_guid, IPMI_MANAGED_SYSTEM_GUID_LENGTH);
  buf_index += IPMI_MANAGED_SYSTEM_GUID_LENGTH;
  /* This part of the spec is wierd, gotta hack it out */
  if (name_only_lookup)
    priv_byte |= 0x10;
  priv_byte |= (requested_privilege_level & 0xF);
  buf[buf_index] = priv_byte;
  buf_index++;
  buf[buf_index] = user_name_length;
  buf_index++;
  if (user_name && user_name_length)
    {
      memcpy(buf + buf_index, user_name, user_name_length);
      buf_index += user_name_length;
    }

  if (k_uid && k_uid_len)
    {
      k_uid_ptr = k_uid;
      k_uid_ptr_len = k_uid_len;
    }
  else
    {
      memset(k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      k_uid_ptr = k_uid_buf;
      k_uid_ptr_len = IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  ERR_CLEANUP (!((digest_len = ipmi_crypt_hash(hash_algorithm,
                                               hash_flags,
                                               k_uid_ptr,
                                               k_uid_ptr_len,
                                               buf,
                                               buf_index,
                                               digest,
                                               IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0));

  if (key_exchange_authentication_code_len != digest_len)
    {
      rv = 0;
      goto cleanup;
    }
  
  rv = memcmp(digest, key_exchange_authentication_code, digest_len) ? 0 : 1;
 cleanup:
  secure_memset(k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
  secure_memset(buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  secure_memset(digest, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  secure_memset(key_exchange_authentication_code, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  return (rv);
}

int8_t 
ipmi_rmcpplus_check_rakp_4_integrity_check_value(int8_t authentication_algorithm,
                                                 uint8_t *sik_key,
                                                 uint32_t sik_key_len,
                                                 uint8_t *remote_console_random_number,
                                                 uint32_t remote_console_random_number_len,
                                                 uint32_t managed_system_session_id,
                                                 uint8_t *managed_system_guid,
                                                 uint32_t managed_system_guid_len,
                                                 fiid_obj_t obj_cmd)
{
  uint8_t buf[IPMI_MAX_KEY_DATA_LENGTH];
  uint32_t buf_index = 0;
  uint8_t digest[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  uint8_t hash_algorithm = 0;
  uint8_t hash_flags = 0;
  int32_t digest_len = 0;
  int32_t compare_len = 0;
  uint8_t integrity_check_value[IPMI_MAX_INTEGRITY_CHECK_VALUE_LENGTH];
  int32_t integrity_check_value_len;
  int8_t rv = -1;

  ERR_EINVAL ((authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
	       || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
	      && remote_console_random_number
	      && !(remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
	      && managed_system_guid
	      && !(managed_system_guid_len < IPMI_MANAGED_SYSTEM_GUID_LENGTH)
	      && fiid_obj_valid(obj_cmd));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_rakp_message_4);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      ERR_EINVAL (!(!sik_key || sik_key_len < IPMI_HMAC_SHA1_DIGEST_LENGTH));

      /* achu: For some reason they want SHA1_96 for this check, sigh */
      compare_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      ERR_EINVAL (!(!sik_key || sik_key_len < IPMI_HMAC_MD5_DIGEST_LENGTH));

      compare_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }

  FIID_OBJ_GET_DATA_LEN_CLEANUP(integrity_check_value_len,
                                obj_cmd,
                                "integrity_check_value",
                                integrity_check_value,
                                IPMI_MAX_INTEGRITY_CHECK_VALUE_LENGTH);
  
  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      if (integrity_check_value_len)
        rv = 0;
      else
        rv = 1;
      goto cleanup;
    }

  if (integrity_check_value_len < compare_len)
    {
      rv = 0;
      goto cleanup;
    }

  memset(buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  memcpy(buf + buf_index, remote_console_random_number, IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH);
  buf_index += IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH;
  buf[buf_index] = (managed_system_session_id & 0x000000ff);
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0x0000ff00) >> 8;
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0x00ff0000) >> 16;
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0xff000000) >> 24;
  buf_index++;
  memcpy(buf + buf_index, managed_system_guid, IPMI_MANAGED_SYSTEM_GUID_LENGTH);
  buf_index += IPMI_MANAGED_SYSTEM_GUID_LENGTH;

  ERR_CLEANUP (!((digest_len = ipmi_crypt_hash(hash_algorithm,
                                               hash_flags,
                                               sik_key,
                                               sik_key_len,
                                               buf,
                                               buf_index,
                                               digest,
                                               IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0));
  ERR_CLEANUP (!(digest_len < compare_len));

  rv = memcmp(digest, integrity_check_value, compare_len) ? 0 : 1;
 cleanup:
  secure_memset(buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  secure_memset(buf, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  return (rv);
}

int8_t 
ipmi_rmcpplus_check_packet_session_authentication_code(int8_t integrity_algorithm,
						       uint8_t *pkt,
						       uint32_t pkt_len,
						       uint8_t *integrity_key,
						       uint32_t integrity_key_len,
						       uint8_t *authentication_code_data,
						       uint32_t authentication_code_data_len,
						       fiid_obj_t obj_rmcpplus_session_trlr)
{
  int32_t rmcp_header_len;
  int hash_algorithm, hash_flags, crypt_digest_len;
  unsigned int expected_digest_len, compare_digest_len, hash_data_len;
  uint8_t hash_data[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t integrity_digest[IPMI_MAX_INTEGRITY_DATA_LENGTH];
  uint8_t authentication_code[IPMI_MAX_INTEGRITY_DATA_LENGTH];
  int32_t authentication_code_len, integrity_digest_len;
  uint8_t pwbuf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  int8_t rv = -1;

  ERR_EINVAL ((integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE
	       || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
	       || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
	       || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
	      && pkt
	      && pkt_len
              && !(authentication_code_data && !authentication_code_data_len)
              && !(authentication_code_data && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
	      && fiid_obj_valid(obj_rmcpplus_session_trlr));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
      compare_digest_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
    }
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      compare_digest_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  else	/* IPMI_INTEGRITY_ALGORITHM_MD5_128 */
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = 0;
      expected_digest_len = IPMI_MD5_DIGEST_LENGTH;
      compare_digest_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  
  ERR_CLEANUP (!((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0));
     
  ERR_EXIT (crypt_digest_len == expected_digest_len);
  
  ERR_EXIT (!((rmcp_header_len = fiid_template_len_bytes(tmpl_rmcp_hdr)) < 0));

  FIID_OBJ_GET_DATA_LEN_CLEANUP(authentication_code_len,
                                obj_rmcpplus_session_trlr,
                                "authentication_code",
                                authentication_code,
                                IPMI_MAX_INTEGRITY_DATA_LENGTH);

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    {
      if (authentication_code_len)
        rv = 0;
      else
        rv = 1;
      goto cleanup;
    }
  
  if  (authentication_code_len != compare_digest_len)
    {
      rv = 0;
      goto cleanup;
    }

  /* Packet is too short, packet must be bogus :-) */
  if (pkt_len <= (rmcp_header_len + compare_digest_len))
    {
      rv = 0;
      goto cleanup;
    }

  memset(hash_data, '\0', IPMI_MAX_PAYLOAD_LENGTH);

  hash_data_len = 0;
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      /* achu: Password must be zero padded */
      memset(pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      
      if (authentication_code_data && authentication_code_data_len)
        memcpy(pwbuf, authentication_code_data, authentication_code_data_len);

      memcpy(hash_data + hash_data_len,
             pwbuf,
             IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  memcpy(hash_data + hash_data_len, pkt + rmcp_header_len, pkt_len - rmcp_header_len - compare_digest_len);
  hash_data_len += pkt_len - rmcp_header_len - compare_digest_len;
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      memcpy(hash_data + hash_data_len,
             pwbuf,
             IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  ERR_CLEANUP (!((integrity_digest_len = ipmi_crypt_hash(hash_algorithm,
                                                         hash_flags,
                                                         integrity_key,
                                                         integrity_key_len,
                                                         hash_data,
                                                         hash_data_len,
                                                         integrity_digest,
                                                         IPMI_MAX_INTEGRITY_DATA_LENGTH)) < 0));

  ERR_CLEANUP (!(integrity_digest_len != crypt_digest_len));

  rv = memcmp(integrity_digest, authentication_code, compare_digest_len) ? 0 : 1;
 cleanup:
  secure_memset(integrity_digest, '\0', IPMI_MAX_INTEGRITY_DATA_LENGTH);
  secure_memset(authentication_code, '\0', IPMI_MAX_INTEGRITY_DATA_LENGTH);
  return rv;
}

int8_t
ipmi_rmcpplus_check_payload_type(fiid_obj_t obj_rmcpplus_session_hdr, uint8_t payload_type)
{
  uint64_t val;
  int32_t len;

  ERR_EINVAL (IPMI_PAYLOAD_TYPE_VALID(payload_type)
	      && fiid_obj_valid(obj_rmcpplus_session_hdr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "payload_type");
  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_rmcpplus_session_hdr, "payload_type", &val);

  return ((payload_type == val) ? 1 : 0);
}

int8_t
ipmi_rmcpplus_check_status_code(fiid_obj_t obj_cmd,
				uint8_t status_code)
{
  uint64_t val;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_cmd)
	      && RMCPPLUS_STATUS_VALID(status_code)
	      && (fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_response) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_3) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) == 1));

  FIID_OBJ_FIELD_LEN (len, obj_cmd, "rmcpplus_status_code");
  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_cmd, "rmcpplus_status_code", &val);

  return ((status_code == val) ? 1 : 0);
}
                            
int8_t 
ipmi_rmcpplus_check_message_tag(fiid_obj_t obj_cmd, uint8_t message_tag)
{
  uint64_t val;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_cmd)
	      && (fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_request) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_response) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_1) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_3) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) == 1));

  FIID_OBJ_FIELD_LEN (len, obj_cmd, "message_tag");
  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_cmd, "message_tag", &val);

  return ((message_tag == val) ? 1 : 0);
}

int8_t 
ipmi_rmcpplus_check_remote_console_session_id(fiid_obj_t obj_cmd, uint32_t remote_console_session_id)
{
  uint64_t val;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_cmd)
	      && (fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_request) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_response) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) == 1
		  || fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) == 1));
  
  FIID_OBJ_FIELD_LEN (len, obj_cmd, "remote_console_session_id");
  ERR_EINVAL (len);
  
  FIID_OBJ_GET(obj_cmd, "remote_console_session_id", &val);

  return ((remote_console_session_id == val) ? 1 : 0);
}

int8_t 
ipmi_rmcpplus_check_session_id(fiid_obj_t obj_rmcpplus_session_hdr, 
                               uint32_t session_id)
{
  uint64_t val;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_rmcpplus_session_hdr));

  FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);
  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "session_id");
  ERR_EINVAL (len);
  
  FIID_OBJ_GET(obj_rmcpplus_session_hdr, "session_id", &val);

  return ((session_id == val) ? 1 : 0);
}

int8_t
ipmi_rmcpplus_calculate_payload_type(uint8_t *pkt, uint32_t pkt_len)
{
  int32_t rmcp_hdr_len;
  uint8_t auth_type, payload_type;

  ERR_EINVAL(pkt && pkt_len);

  FIID_TEMPLATE_LEN_BYTES(rmcp_hdr_len, tmpl_rmcp_hdr);

  ERR_EINVAL (!(pkt_len <= rmcp_hdr_len));

  auth_type = *(pkt + rmcp_hdr_len);
  ERR_EINVAL (auth_type == IPMI_AUTHENTICATION_TYPE_RMCPPLUS);

  payload_type = *(pkt + rmcp_hdr_len + 1);
  payload_type &= 0x3F;
  ERR_EINVAL(IPMI_PAYLOAD_TYPE_VALID(payload_type));

  return payload_type;
}
