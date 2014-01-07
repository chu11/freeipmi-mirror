/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/util/ipmi-rmcpplus-util.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"

#include "libcommon/ipmi-crypt.h"
#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

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

int
ipmi_calculate_sik (uint8_t authentication_algorithm,
                    const void *k_g,
                    unsigned int k_g_len,
                    const void *remote_console_random_number,
                    unsigned int remote_console_random_number_len,
                    const void *managed_system_random_number,
                    unsigned int managed_system_random_number_len,
                    uint8_t name_only_lookup,
                    uint8_t requested_privilege_level,
                    const char *user_name,
                    unsigned int user_name_len,
                    void *sik,
                    unsigned int sik_len)
{
  int expected_digest_len, crypt_digest_len, computed_digest_len, rv = -1;
  unsigned int hash_algorithm, hash_flags, hash_data_len;
  uint8_t hash_data[IPMI_MAX_KEY_DATA_LENGTH];
  uint8_t priv_byte = 0;

  /* k_g can be NULL, indicating a empty k_g */
  if ((authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256)
      || (k_g && !k_g_len)
      || (k_g && k_g_len > IPMI_MAX_K_G_LENGTH)
      || !remote_console_random_number
      || (remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
      || !managed_system_random_number
      || (managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
      || !IPMI_USER_NAME_LOOKUP_VALID (name_only_lookup)
      || !IPMI_PRIVILEGE_LEVEL_VALID (requested_privilege_level)
      || (user_name && !user_name_len)
      || (user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
      || (!user_name && user_name_len)
      || !sik
      || !sik_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
    }
  else /* IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256 */
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA256;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA256_DIGEST_LENGTH;
    }

  if ((crypt_digest_len = crypt_hash_digest_len (hash_algorithm)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  assert (crypt_digest_len == expected_digest_len);

  if (sik_len < expected_digest_len)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  k_g_len = (k_g_len > IPMI_MAX_SIK_KEY_LENGTH) ? IPMI_MAX_SIK_KEY_LENGTH : k_g_len;

  memset (hash_data, '\0', IPMI_MAX_KEY_DATA_LENGTH);

  /*
   * Build up data for hashing.
   */

  /* checks above and memcpy limits below ensure can't overflow unsigned int */
  hash_data_len = 0;
  memcpy (hash_data + hash_data_len,
          (void *)remote_console_random_number,
          IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH);
  hash_data_len += IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH;

  memcpy (hash_data + hash_data_len,
          (void *)managed_system_random_number,
          IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH);
  hash_data_len += IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH;

  /* This part of the spec is wierd, gotta hack it out */
  if (name_only_lookup)
    priv_byte |= 0x10;
  priv_byte |= (requested_privilege_level & 0xF);

  hash_data[hash_data_len++] = priv_byte;
  hash_data[hash_data_len++] = user_name_len;

  if (user_name && user_name_len > 0)
    {
      memcpy (hash_data + hash_data_len, (void *)user_name, user_name_len);
      hash_data_len += user_name_len;
    }

  if ((computed_digest_len = crypt_hash (hash_algorithm,
					 hash_flags,
					 k_g,
					 k_g_len,
					 hash_data,
					 hash_data_len,
					 sik,
					 sik_len)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (computed_digest_len != crypt_digest_len)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  rv = computed_digest_len;
 cleanup:
  secure_memset (hash_data, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  return (rv);
}

static int
_calculate_k_rakp_hmac (unsigned int hash_algorithm,
                        unsigned int expected_digest_len,
                        const void *sik_key,
                        unsigned int sik_key_len,
                        void *k,
                        unsigned int k_len,
                        const void *constant,
                        unsigned int constant_len)
{
  int computed_digest_len, crypt_digest_len;

  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID (hash_algorithm)
      || !sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len
      || (sik_key_len < expected_digest_len)
      || (k_len < expected_digest_len)
      || (constant_len < IPMI_KEY_CONSTANT_LENGTH))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((crypt_digest_len = crypt_hash_digest_len (hash_algorithm)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  assert (crypt_digest_len == expected_digest_len);

  /* SPEC: achu: I believe the length of the constant you pass in
   * is the digest_len, atleast according to IPMI 2.0 Spec Section
   * 13.32, "constants are constructed using a hexadecimal octet
   * value repeated up to the HMAC block size in length starting
   * with the constant 01h".
   */
  if ((computed_digest_len =  crypt_hash (hash_algorithm,
					  IPMI_CRYPT_HASH_FLAGS_HMAC,
					  sik_key,
					  crypt_digest_len,
					  constant,
					  IPMI_KEY_CONSTANT_LENGTH,
					  k,
					  k_len)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (computed_digest_len != crypt_digest_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (computed_digest_len);
}

static int
_calculate_k_rakp_hmac_sha1 (const void *sik_key,
                             unsigned int sik_key_len,
                             void *k,
                             unsigned int k_len,
                             const void *constant,
                             unsigned int constant_len)
{
  if (!sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len
      || (constant_len < IPMI_KEY_CONSTANT_LENGTH))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (_calculate_k_rakp_hmac (IPMI_CRYPT_HASH_SHA1,
                                  IPMI_HMAC_SHA1_DIGEST_LENGTH,
                                  sik_key,
                                  sik_key_len,
                                  k,
                                  k_len,
                                  constant,
                                  constant_len));
}

static int
_calculate_k_rakp_hmac_md5 (const void *sik_key,
                            unsigned int sik_key_len,
                            void *k,
                            unsigned int k_len,
                            const void *constant,
                            unsigned int constant_len)
{
  if (!sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len
      || (constant_len < IPMI_KEY_CONSTANT_LENGTH))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (_calculate_k_rakp_hmac (IPMI_CRYPT_HASH_MD5,
                                  IPMI_HMAC_MD5_DIGEST_LENGTH,
                                  sik_key,
                                  sik_key_len,
                                  k,
                                  k_len,
                                  constant,
                                  constant_len));
}

static int
_calculate_k_rakp_hmac_sha256 (const void *sik_key,
                               unsigned int sik_key_len,
                               void *k,
                               unsigned int k_len,
                               const void *constant,
                               unsigned int constant_len)
{
  if (!sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len
      || (constant_len < IPMI_KEY_CONSTANT_LENGTH))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (_calculate_k_rakp_hmac (IPMI_CRYPT_HASH_SHA256,
                                  IPMI_HMAC_SHA256_DIGEST_LENGTH,
                                  sik_key,
                                  sik_key_len,
                                  k,
                                  k_len,
                                  constant,
                                  constant_len));
}

static int
_ipmi_calculate_k (uint8_t authentication_algorithm,
                   const void *sik_key,
                   unsigned int sik_key_len,
                   void *k,
                   unsigned int k_len,
                   const void *constant,
                   unsigned int constant_len)
{
  if ((authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256)
      || !sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len
      || (constant_len < IPMI_KEY_CONSTANT_LENGTH))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    return (_calculate_k_rakp_hmac_sha1 (sik_key,
                                         sik_key_len,
                                         k,
                                         k_len,
                                         constant,
                                         constant_len));
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    return (_calculate_k_rakp_hmac_md5 (sik_key,
                                        sik_key_len,
                                        k,
                                        k_len,
                                        constant,
                                        constant_len));
  else /* IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256 */
    return (_calculate_k_rakp_hmac_sha256 (sik_key,
					   sik_key_len,
					   k,
					   k_len,
					   constant,
					   constant_len));
}

int
ipmi_calculate_k1 (uint8_t authentication_algorithm,
                   const void *sik_key,
                   unsigned int sik_key_len,
                   void *k1,
                   unsigned int k1_len)
{
  uint8_t constant[IPMI_KEY_CONSTANT_LENGTH] = { 0x01, 0x01, 0x01, 0x01, 0x01,
                                                 0x01, 0x01, 0x01, 0x01, 0x01,
                                                 0x01, 0x01, 0x01, 0x01, 0x01,
                                                 0x01, 0x01, 0x01, 0x01, 0x01};

  return (_ipmi_calculate_k (authentication_algorithm,
                             sik_key,
                             sik_key_len,
                             k1,
                             k1_len,
                             constant,
                             IPMI_KEY_CONSTANT_LENGTH));
}

int
ipmi_calculate_k2 (uint8_t authentication_algorithm,
                   const void *sik_key,
                   unsigned int sik_key_len,
                   void *k1,
                   unsigned int k1_len)
{
  uint8_t constant[IPMI_KEY_CONSTANT_LENGTH] = { 0x02, 0x02, 0x02, 0x02, 0x02,
                                                 0x02, 0x02, 0x02, 0x02, 0x02,
                                                 0x02, 0x02, 0x02, 0x02, 0x02,
                                                 0x02, 0x02, 0x02, 0x02, 0x02};
  return (_ipmi_calculate_k (authentication_algorithm,
                             sik_key,
                             sik_key_len,
                             k1,
                             k1_len,
                             constant,
                             IPMI_KEY_CONSTANT_LENGTH));
}

int
ipmi_calculate_rmcpplus_session_keys (uint8_t authentication_algorithm,
                                      uint8_t integrity_algorithm,
                                      uint8_t confidentiality_algorithm,
                                      const void *authentication_code_data,
                                      unsigned int authentication_code_data_len,
                                      const void *k_g,
                                      unsigned int k_g_len,
                                      const void *remote_console_random_number,
                                      unsigned int remote_console_random_number_len,
                                      const void *managed_system_random_number,
                                      unsigned int managed_system_random_number_len,
                                      uint8_t name_only_lookup,
                                      uint8_t requested_privilege_level,
                                      const char *user_name,
                                      unsigned int user_name_len,
                                      void **sik_key,
                                      unsigned int *sik_key_len,
                                      void **integrity_key,
                                      unsigned int *integrity_key_len,
                                      void **confidentiality_key,
                                      unsigned int *confidentiality_key_len)
{
  void *sik_key_buf;
  unsigned int sik_key_buf_len;
  void *integrity_key_buf;
  unsigned int integrity_key_buf_len;
  void *confidentiality_key_buf;
  unsigned int confidentiality_key_buf_len;
  uint8_t k_g_buf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  void *k_g_ptr;
  unsigned int k_g_ptr_len;
  uint8_t sik[IPMI_MAX_SIK_KEY_LENGTH];
  int sik_len;
  uint8_t k1[IPMI_MAX_K1_LENGTH];
  int k1_len;
  uint8_t k2[IPMI_MAX_K2_LENGTH];
  int k2_len;
  int rv = -1;

  if (!IPMI_CIPHER_SUITE_COMBINATION_VALID (authentication_algorithm,
                                            integrity_algorithm,
                                            confidentiality_algorithm)
      || !IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
      || !sik_key
      || !(*sik_key)
      || !sik_key_len
      || !(*sik_key_len)
      || !integrity_key
      || !(*integrity_key)
      || !integrity_key_len
      || !(*integrity_key_len)
      || !confidentiality_key
      || !(*confidentiality_key)
      || !confidentiality_key_len
      || !(*confidentiality_key_len))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

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
	 || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256
       */
    {
      if ((authentication_code_data_len && !authentication_code_data)
          || (authentication_code_data && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
          || (k_g_len && !k_g)
          || (k_g && k_g_len > IPMI_MAX_K_G_LENGTH)
          || !remote_console_random_number
          || (remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
          || !managed_system_random_number
          || (managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
          || !IPMI_USER_NAME_LOOKUP_VALID (name_only_lookup)
          || !IPMI_PRIVILEGE_LEVEL_VALID (requested_privilege_level)
          || (user_name && !user_name_len)
          || (user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
          || (!user_name && user_name_len))
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }

      if (k_g && k_g_len)
        {
          k_g_ptr = (void *)k_g;
          k_g_ptr_len = k_g_len;
        }
      else
        {
          if (authentication_code_data && authentication_code_data_len)
            {
              k_g_ptr = (void *)authentication_code_data;
              k_g_ptr_len = authentication_code_data_len;
            }
          else
            {
              memset (k_g_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
              k_g_ptr = k_g_buf;
              k_g_ptr_len = IPMI_2_0_MAX_PASSWORD_LENGTH;
            }
        }

      if ((sik_len = ipmi_calculate_sik (authentication_algorithm,
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
                                         IPMI_MAX_SIK_KEY_LENGTH)) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (sik_key_buf_len < sik_len)
        {
          SET_ERRNO (EMSGSIZE);
          goto cleanup;
        }

      memcpy (sik_key_buf, sik, sik_len);
      sik_key_buf_len = sik_len;
    }

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    {
      integrity_key_buf = NULL;
      integrity_key_buf_len = 0;
    }
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128)
    {
      if ((k1_len = ipmi_calculate_k1 (authentication_algorithm,
                                       sik_key_buf,
                                       sik_key_buf_len,
                                       k1,
                                       IPMI_MAX_K1_LENGTH)) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (integrity_key_buf_len < k1_len)
        {
          SET_ERRNO (EMSGSIZE);
          goto cleanup;
        }

      memcpy (integrity_key_buf, k1, k1_len);
      integrity_key_buf_len = k1_len;
    }
  else /* IPMI_INTEGRITY_ALGORITHM_MD5_128 */
    {
      if (authentication_code_data
          && (!authentication_code_data_len
              || integrity_key_buf_len < authentication_code_data_len))
        {
          SET_ERRNO (EMSGSIZE);
          goto cleanup;
        }

      memset (integrity_key_buf, '\0', integrity_key_buf_len);
      if (authentication_code_data)
        {
          memcpy (integrity_key_buf,
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
      if ((k2_len = ipmi_calculate_k2 (authentication_algorithm,
                                       sik_key_buf,
                                       sik_key_buf_len,
                                       k2,
                                       IPMI_MAX_K2_LENGTH)) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (confidentiality_key_buf_len < k2_len)
        {
          SET_ERRNO (EMSGSIZE);
          goto cleanup;
        }

      memcpy (confidentiality_key_buf, k2, k2_len);
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
  secure_memset (k_g_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
  secure_memset (sik, '\0', IPMI_MAX_SIK_KEY_LENGTH);
  secure_memset (k1, '\0', IPMI_MAX_K1_LENGTH);
  secure_memset (k2, '\0', IPMI_MAX_K2_LENGTH);
  return (rv);
}

int
ipmi_calculate_rakp_3_key_exchange_authentication_code (uint8_t authentication_algorithm,
                                                        const void *k_uid,
                                                        unsigned int k_uid_len,
                                                        const void *managed_system_random_number,
                                                        unsigned int managed_system_random_number_len,
                                                        uint32_t remote_console_session_id,
                                                        uint8_t name_only_lookup,
                                                        uint8_t requested_privilege_level,
                                                        const char *user_name,
                                                        unsigned int user_name_len,
                                                        void *key_exchange_authentication_code,
                                                        unsigned int key_exchange_authentication_code_len)
{
  uint8_t priv_byte = 0;
  uint8_t k_uid_buf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  void *k_uid_ptr;
  unsigned int k_uid_ptr_len;
  uint8_t buf[IPMI_MAX_KEY_DATA_LENGTH];
  unsigned int buf_index = 0;
  uint8_t digest[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  unsigned int hash_algorithm, hash_flags;
  int digest_len, expected_digest_len;
  int rv = -1;

  if ((authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256)
      || (k_uid && !k_uid_len)
      || (k_uid && k_uid_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || !managed_system_random_number
      || (managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
      || !IPMI_USER_NAME_LOOKUP_VALID (name_only_lookup)
      || !IPMI_PRIVILEGE_LEVEL_VALID (requested_privilege_level)
      || (user_name && !user_name_len)
      || (user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
      || (!user_name && user_name_len)
      || !key_exchange_authentication_code)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      memset (key_exchange_authentication_code, '\0', key_exchange_authentication_code_len);
      return (0);
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      if (key_exchange_authentication_code_len < IPMI_HMAC_SHA1_DIGEST_LENGTH)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }

      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      if (key_exchange_authentication_code_len < IPMI_HMAC_MD5_DIGEST_LENGTH)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }

      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else /* IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256 */
    {
      if (key_exchange_authentication_code_len < IPMI_HMAC_SHA256_DIGEST_LENGTH)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }

      expected_digest_len = IPMI_HMAC_SHA256_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA256;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }

  /* checks above and memcpy limits below ensure can't overflow unsigned int */
  memset (buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  memcpy (buf + buf_index, managed_system_random_number, IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH);
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
  buf[buf_index] = user_name_len;
  buf_index++;
  if (user_name && user_name_len)
    {
      memcpy (buf + buf_index, user_name, user_name_len);
      buf_index += user_name_len;
    }

  if (k_uid && k_uid_len)
    {
      k_uid_ptr = (void *)k_uid;
      k_uid_ptr_len = k_uid_len;
    }
  else
    {
      memset (k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      k_uid_ptr = k_uid_buf;
      k_uid_ptr_len = IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  if ((digest_len = crypt_hash (hash_algorithm,
				hash_flags,
				k_uid_ptr,
				k_uid_ptr_len,
				buf,
				buf_index,
				digest,
				IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (digest_len != expected_digest_len)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  memcpy (key_exchange_authentication_code, digest, digest_len);
  rv = digest_len;
 cleanup:
  secure_memset (k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
  secure_memset (buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  secure_memset (digest, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  return (rv);
}

int
ipmi_rmcpplus_check_payload_pad (uint8_t confidentiality_algorithm,
                                 fiid_obj_t obj_rmcpplus_payload)
{
  if ((confidentiality_algorithm != IPMI_CONFIDENTIALITY_ALGORITHM_NONE
       && confidentiality_algorithm != IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
      || !fiid_obj_valid (obj_rmcpplus_payload))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_payload, tmpl_rmcpplus_payload) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    /* No padding */
    return (1);
  else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
    {
      uint8_t confidentiality_trailer[IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH];
      int confidentiality_trailer_len;
      uint8_t pad_len;
      unsigned int i;

      if ((confidentiality_trailer_len = fiid_obj_get_data (obj_rmcpplus_payload,
                                                            "confidentiality_trailer",
                                                            confidentiality_trailer,
                                                            IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
          return (-1);
        }

      if (!confidentiality_trailer_len)
        return (0);

      pad_len = confidentiality_trailer[confidentiality_trailer_len - 1];

      if ((uint8_t)(confidentiality_trailer_len - 1) != pad_len)
        return (0);

      for (i = 0; i < pad_len; i++)
        {
          if (confidentiality_trailer[i] != i + 1)
            return (0);
        }

      return (1);
    }
}

int
ipmi_rmcpplus_check_integrity_pad (fiid_obj_t obj_rmcpplus_session_trlr)
{
  uint8_t integrity_pad[IPMI_MAX_INTEGRITY_PAD_LENGTH];
  uint8_t pad_length;
  uint64_t val;
  unsigned int i;

  if (!fiid_obj_valid (obj_rmcpplus_session_trlr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_rmcpplus_session_trlr, "pad_length", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
      return (-1);
    }
  pad_length = val;

  if (!pad_length)
    return (1);

  if (pad_length > IPMI_INTEGRITY_PAD_MULTIPLE)
    return (0);

  memset (integrity_pad, '\0', IPMI_MAX_INTEGRITY_PAD_LENGTH);
  if (fiid_obj_get_data (obj_rmcpplus_session_trlr,
                         "integrity_pad",
                         integrity_pad,
                         IPMI_MAX_INTEGRITY_PAD_LENGTH) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
      return (-1);
    }

  for (i = 0; i < pad_length; i++)
    {
      if (integrity_pad[i] != IPMI_INTEGRITY_PAD_DATA)
        return (0);
    }

  return (1);
}

int
ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code (uint8_t authentication_algorithm,
                                                             const void *k_uid,
                                                             unsigned int k_uid_len,
                                                             uint32_t remote_console_session_id,
                                                             uint32_t managed_system_session_id,
                                                             const void *remote_console_random_number,
                                                             unsigned int remote_console_random_number_len,
                                                             const void *managed_system_random_number,
                                                             unsigned int managed_system_random_number_len,
                                                             const void *managed_system_guid,
                                                             unsigned int managed_system_guid_len,
                                                             uint8_t name_only_lookup,
                                                             uint8_t requested_privilege_level,
                                                             const char *user_name,
                                                             unsigned int user_name_len,
                                                             fiid_obj_t obj_cmd)
{
  uint8_t priv_byte = 0;
  uint8_t k_uid_buf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  void *k_uid_ptr;
  unsigned int k_uid_ptr_len;
  uint8_t buf[IPMI_MAX_KEY_DATA_LENGTH];
  unsigned int buf_index = 0;
  uint8_t digest[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  unsigned int hash_algorithm = 0, hash_flags = 0;
  int digest_len = 0;
  uint8_t key_exchange_authentication_code[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  int key_exchange_authentication_code_len;
  unsigned int compare_len = 0;
  int rv = -1;

  if ((authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256)
      || (k_uid && !k_uid_len)
      || (k_uid && k_uid_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || !remote_console_random_number
      || (remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
      || !managed_system_random_number
      || (managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
      || !managed_system_guid
      || (managed_system_guid_len < IPMI_MANAGED_SYSTEM_GUID_LENGTH)
      || !IPMI_USER_NAME_LOOKUP_VALID (name_only_lookup)
      || !IPMI_PRIVILEGE_LEVEL_VALID (requested_privilege_level)
      || (user_name && !user_name_len)
      || (user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
      || (!user_name && user_name_len)
      || !fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_2) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

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
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256)
    {
      compare_len = IPMI_HMAC_SHA256_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA256;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }

  if ((key_exchange_authentication_code_len = fiid_obj_get_data (obj_cmd,
                                                                 "key_exchange_authentication_code",
                                                                 key_exchange_authentication_code,
                                                                 IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }

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

  /* checks above and memcpy limits below ensure we can't overflow an unsigned int */
  memset (buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
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
  memcpy (buf + buf_index, remote_console_random_number, IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH);
  buf_index += IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH;
  memcpy (buf + buf_index, managed_system_random_number, IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH);
  buf_index += IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH;
  memcpy (buf + buf_index, managed_system_guid, IPMI_MANAGED_SYSTEM_GUID_LENGTH);
  buf_index += IPMI_MANAGED_SYSTEM_GUID_LENGTH;
  /* This part of the spec is wierd, gotta hack it out */
  if (name_only_lookup)
    priv_byte |= 0x10;
  priv_byte |= (requested_privilege_level & 0xF);
  buf[buf_index] = priv_byte;
  buf_index++;
  buf[buf_index] = user_name_len;
  buf_index++;
  if (user_name && user_name_len)
    {
      memcpy (buf + buf_index, user_name, user_name_len);
      buf_index += user_name_len;
    }

  if (k_uid && k_uid_len)
    {
      k_uid_ptr = (void *)k_uid;
      k_uid_ptr_len = k_uid_len;
    }
  else
    {
      memset (k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      k_uid_ptr = k_uid_buf;
      k_uid_ptr_len = IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  if ((digest_len = crypt_hash (hash_algorithm,
				hash_flags,
				k_uid_ptr,
				k_uid_ptr_len,
				buf,
				buf_index,
				digest,
				IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (key_exchange_authentication_code_len != digest_len)
    {
      rv = 0;
      goto cleanup;
    }

  rv = memcmp (digest, key_exchange_authentication_code, digest_len) ? 0 : 1;
 cleanup:
  secure_memset (k_uid_buf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
  secure_memset (buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  secure_memset (digest, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  secure_memset (key_exchange_authentication_code, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  return (rv);
}

int
ipmi_rmcpplus_check_rakp_4_integrity_check_value (uint8_t authentication_algorithm,
                                                  const void *sik_key,
                                                  unsigned int sik_key_len,
                                                  const void *remote_console_random_number,
                                                  unsigned int remote_console_random_number_len,
                                                  uint32_t managed_system_session_id,
                                                  const void *managed_system_guid,
                                                  unsigned int managed_system_guid_len,
                                                  fiid_obj_t obj_cmd)
{
  uint8_t buf[IPMI_MAX_KEY_DATA_LENGTH];
  unsigned int buf_index = 0;
  uint8_t digest[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  unsigned int hash_algorithm = 0, hash_flags = 0;
  int digest_len = 0;
  unsigned int compare_len = 0;
  uint8_t integrity_check_value[IPMI_MAX_INTEGRITY_CHECK_VALUE_LENGTH];
  int integrity_check_value_len, rv = -1;

  if ((authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
       && authentication_algorithm != IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256)
      || !remote_console_random_number
      || (remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
      || !managed_system_guid
      || (managed_system_guid_len < IPMI_MANAGED_SYSTEM_GUID_LENGTH)
      || !fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_4) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      if (!sik_key || sik_key_len < IPMI_HMAC_SHA1_DIGEST_LENGTH)
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }

      /* achu: For some reason they want SHA1_96 for this check, sigh */
      compare_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      if (!sik_key || sik_key_len < IPMI_HMAC_MD5_DIGEST_LENGTH)
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }

      compare_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256)
    {
      if (!sik_key || sik_key_len < IPMI_HMAC_SHA256_DIGEST_LENGTH)
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }

      compare_len = IPMI_HMAC_SHA256_128_AUTHENTICATION_CODE_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA256;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }

  if ((integrity_check_value_len = fiid_obj_get_data (obj_cmd,
                                                      "integrity_check_value",
                                                      integrity_check_value,
                                                      IPMI_MAX_INTEGRITY_CHECK_VALUE_LENGTH)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }

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

  /* checks above and memcpy limits below ensure can't overflow unsigned int */
  memset (buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  memcpy (buf + buf_index, remote_console_random_number, IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH);
  buf_index += IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH;
  buf[buf_index] = (managed_system_session_id & 0x000000ff);
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0x0000ff00) >> 8;
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0x00ff0000) >> 16;
  buf_index++;
  buf[buf_index] = (managed_system_session_id & 0xff000000) >> 24;
  buf_index++;
  memcpy (buf + buf_index, managed_system_guid, IPMI_MANAGED_SYSTEM_GUID_LENGTH);
  buf_index += IPMI_MANAGED_SYSTEM_GUID_LENGTH;

  if ((digest_len = crypt_hash (hash_algorithm,
				hash_flags,
				sik_key,
				sik_key_len,
				buf,
				buf_index,
				digest,
				IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (digest_len < compare_len)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  rv = memcmp (digest, integrity_check_value, compare_len) ? 0 : 1;
 cleanup:
  secure_memset (buf, '\0', IPMI_MAX_KEY_DATA_LENGTH);
  secure_memset (digest, '\0', IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);
  return (rv);
}

int
ipmi_rmcpplus_check_packet_session_authentication_code (uint8_t integrity_algorithm,
                                                        const void *pkt,
                                                        unsigned int pkt_len,
                                                        const void *integrity_key,
                                                        unsigned int integrity_key_len,
                                                        const void *authentication_code_data,
                                                        unsigned int authentication_code_data_len,
                                                        fiid_obj_t obj_rmcpplus_session_trlr)
{
  unsigned int hash_algorithm, hash_flags;
  unsigned int expected_digest_len, compare_digest_len, hash_data_len;
  uint8_t hash_data[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t integrity_digest[IPMI_MAX_INTEGRITY_DATA_LENGTH];
  uint8_t authentication_code[IPMI_MAX_INTEGRITY_DATA_LENGTH];
  int rmcp_header_len, authentication_code_len, integrity_digest_len, crypt_digest_len, rv = -1;
  uint8_t pwbuf[IPMI_2_0_MAX_PASSWORD_LENGTH];

  if ((integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_NONE
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_MD5_128
       && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128)
      || !pkt
      || !pkt_len
      || (authentication_code_data && !authentication_code_data_len)
      || (authentication_code_data && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || !fiid_obj_valid (obj_rmcpplus_session_trlr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

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
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = 0;
      expected_digest_len = IPMI_MD5_DIGEST_LENGTH;
      compare_digest_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  else /* IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128 */
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA256;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA256_DIGEST_LENGTH;
      compare_digest_len = IPMI_HMAC_SHA256_128_AUTHENTICATION_CODE_LENGTH;
    }

  if ((crypt_digest_len = crypt_hash_digest_len (hash_algorithm)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  assert (crypt_digest_len == expected_digest_len);

  if ((rmcp_header_len = fiid_template_len_bytes (tmpl_rmcp_hdr)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((authentication_code_len = fiid_obj_get_data (obj_rmcpplus_session_trlr,
                                                    "authentication_code",
                                                    authentication_code,
                                                    IPMI_MAX_INTEGRITY_DATA_LENGTH)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
      goto cleanup;
    }

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    {
      if (authentication_code_len)
        rv = 0;
      else
        rv = 1;
      goto cleanup;
    }

  if (authentication_code_len != compare_digest_len)
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

  memset (hash_data, '\0', IPMI_MAX_PAYLOAD_LENGTH);

  hash_data_len = 0;

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      /* achu: Password must be zero padded */
      memset (pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);

      if (authentication_code_data && authentication_code_data_len)
        memcpy (pwbuf, authentication_code_data, authentication_code_data_len);

      memcpy (hash_data + hash_data_len,
              pwbuf,
              IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  memcpy (hash_data + hash_data_len, pkt + rmcp_header_len, pkt_len - rmcp_header_len - compare_digest_len);
  hash_data_len += pkt_len - rmcp_header_len - compare_digest_len;

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      memcpy (hash_data + hash_data_len,
              pwbuf,
              IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  if ((integrity_digest_len = crypt_hash (hash_algorithm,
					  hash_flags,
					  integrity_key,
					  integrity_key_len,
					  hash_data,
					  hash_data_len,
					  integrity_digest,
					  IPMI_MAX_INTEGRITY_DATA_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (integrity_digest_len != crypt_digest_len)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  rv = memcmp (integrity_digest, authentication_code, compare_digest_len) ? 0 : 1;
 cleanup:
  secure_memset (integrity_digest, '\0', IPMI_MAX_INTEGRITY_DATA_LENGTH);
  secure_memset (authentication_code, '\0', IPMI_MAX_INTEGRITY_DATA_LENGTH);
  return (rv);
}

int
ipmi_rmcpplus_check_payload_type (fiid_obj_t obj_rmcpplus_session_hdr, uint8_t payload_type)
{
  uint8_t l_payload_type;
  uint64_t val;

  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !fiid_obj_valid (obj_rmcpplus_session_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr, "payload_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  l_payload_type = val;

  return ((payload_type == l_payload_type) ? 1 : 0);
}

int
ipmi_rmcpplus_check_status_code (fiid_obj_t obj_cmd,
                                 uint8_t status_code)
{
  uint8_t l_rmcpplus_status_code;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd)
      || !RMCPPLUS_STATUS_VALID (status_code)
      || (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_response) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_2) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_3) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_4) < 0))
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd, "rmcpplus_status_code", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  l_rmcpplus_status_code = val;

  return ((status_code == l_rmcpplus_status_code) ? 1 : 0);
}

int
ipmi_rmcpplus_check_message_tag (fiid_obj_t obj_cmd, uint8_t message_tag)
{
  uint8_t l_message_tag;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd)
      || (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_request) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_response) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_1) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_2) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_3) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_4) < 0))
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd, "message_tag", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  l_message_tag = val;

  return ((message_tag == l_message_tag) ? 1 : 0);
}

int
ipmi_rmcpplus_check_remote_console_session_id (fiid_obj_t obj_cmd, uint32_t remote_console_session_id)
{
  uint32_t l_remote_console_session_id;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd)
      || (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_request) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_response) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_2) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_4) < 0))
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd, "remote_console_session_id", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  l_remote_console_session_id = val;

  return ((remote_console_session_id == l_remote_console_session_id) ? 1 : 0);
}

int
ipmi_rmcpplus_check_session_id (fiid_obj_t obj_rmcpplus_session_hdr,
                                uint32_t session_id)
{
  uint32_t l_session_id;
  uint64_t val;

  if (!fiid_obj_valid (obj_rmcpplus_session_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr, "session_id", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  l_session_id = val;

  return ((session_id == l_session_id) ? 1 : 0);
}

int
ipmi_rmcpplus_calculate_payload_type (const void *pkt,
                                      unsigned int pkt_len,
                                      uint8_t *payload_type)
{
  int rmcp_hdr_len;
  uint8_t auth_type;

  if (!pkt
      || !pkt_len
      || !payload_type)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((rmcp_hdr_len = fiid_template_len_bytes (tmpl_rmcp_hdr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (pkt_len <= rmcp_hdr_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  auth_type = *((uint8_t *)pkt + rmcp_hdr_len);

  if (auth_type != IPMI_AUTHENTICATION_TYPE_RMCPPLUS)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  (*payload_type) = *((uint8_t *)pkt + rmcp_hdr_len + 1);
  (*payload_type) &= 0x3F;

  if (!IPMI_PAYLOAD_TYPE_VALID ((*payload_type)))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (0);
}
