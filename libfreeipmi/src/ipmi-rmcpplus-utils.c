/* 
   ipmi-rmcpplus-util.c - IPMI RMCPPLUS Utils

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
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <pthread.h>
#include <gcrypt.h>
GCRY_THREAD_OPTION_PTHREAD_IMPL;

#include "freeipmi/ipmi-rmcpplus-utils.h"
#include "freeipmi/ipmi-rmcpplus.h"
#include "freeipmi/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/ipmi-debug.h"
#include "freeipmi/ipmi-messaging-support-cmds.h" /* XXX  - only for IPMI_MAX_USER_NAME_LENGTH */
#include "freeipmi/ipmi-privilege-level-spec.h"
#include "freeipmi/rmcp.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "md5.h"

static int8_t
_ipmi_init_crypt(void)
{
  gcry_error_t e;

  /* XXX: achu: This is my lazy approach to thread safety.  All crypt
   * functions must continually re-init.  I need to come up with a
   * more creative/better method later.
   */

  if ((e = gcry_control(GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread)) != GPG_ERR_NO_ERROR)
    {
      ipmi_debug("gcry_control: %s", gcry_strerror(e));
      return (-1);
    }

  if (!gcry_check_version(GCRYPT_VERSION))
    {
      ipmi_debug("gcry_check_version");
      return (-1);
    }

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

  return (0);
}

int32_t
ipmi_crypt_hash(int hash_algorithm,
                int hash_flags,
                uint8_t *key,
                uint32_t key_len,
                uint8_t *hash_data,
                uint32_t hash_data_len,
                uint8_t *digest,
                uint32_t digest_len)
{
  gcry_md_hd_t h;
  gcry_error_t e;
  int gcry_md_algorithm, gcry_md_flags = 0;
  unsigned int gcry_md_digest_len;
  uint8_t *digestPtr;

  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID(hash_algorithm)
      || (hash_data && !hash_data_len)
      || !digest
      || !digest_len)
    {
      errno = EINVAL;
      return (-1);
    }
    
  if (_ipmi_init_crypt() < 0)
    return (-1);

  if (hash_algorithm == IPMI_CRYPT_HASH_SHA1)
    gcry_md_algorithm = GCRY_MD_SHA1;
  else
    gcry_md_algorithm = GCRY_MD_MD5;

  if (hash_flags & IPMI_CRYPT_HASH_FLAGS_HMAC)
    gcry_md_flags |= GCRY_MD_FLAG_HMAC;

  if ((gcry_md_digest_len = gcry_md_get_algo_dlen(gcry_md_algorithm)) > digest_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((e = gcry_md_open(&h, gcry_md_algorithm, gcry_md_flags)) != GPG_ERR_NO_ERROR)
    {
      ipmi_debug("gcry_md_open: %s", gcry_strerror(e));
      return (-1);
    }
      
  if (!h)
    {
      ipmi_debug("gcry_md_open: NULL handle return");
      return (-1);
    }

  /* achu: Technically any key length can be supplied.  We'll assume
   * callers have checked if the key is of a length they care about.
   */
  if ((hash_flags & IPMI_CRYPT_HASH_FLAGS_HMAC) && key && key_len)
    {
      if ((e = gcry_md_setkey(h, key, key_len)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_md_setkey: %s", gcry_strerror(e));
          return (-1);
        }
    }

  if (hash_data && hash_data_len)
    gcry_md_write(h, (void *)hash_data, hash_data_len);

  gcry_md_final(h);

  if (!(digestPtr = gcry_md_read(h, gcry_md_algorithm)))
    {
      ipmi_debug("gcry_md_read: NULL digest return");
      return (-1);
    }

  memcpy(digest, digestPtr, gcry_md_digest_len);
  gcry_md_close(h);
  return (gcry_md_digest_len);
}

int32_t
ipmi_crypt_hash_digest_len(int hash_algorithm)
{
  int gcry_md_algorithm;

  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID(hash_algorithm))
    {
      errno = EINVAL;
      return (-1);
    }

  if (_ipmi_init_crypt() < 0)
    return (-1);

  if (hash_algorithm == IPMI_CRYPT_HASH_SHA1)
    gcry_md_algorithm = GCRY_MD_SHA1;
  else
    gcry_md_algorithm = GCRY_MD_MD5;

  return gcry_md_get_algo_dlen(gcry_md_algorithm);
}

static int32_t
_cipher_crypt(int cipher_algorithm,
              int cipher_mode,
              uint8_t *key,
              uint32_t key_len,
              uint8_t *iv,
              uint32_t iv_len,
              uint8_t *data,
              uint32_t data_len,
              int encrypt_flag)
{
  int gcry_cipher_algorithm, gcry_cipher_mode = 0;
  int cipher_keylen, cipher_blocklen;
  int expected_cipher_key_len, expected_cipher_block_len;
  gcry_cipher_hd_t h;
  gcry_error_t e;

  if (!IPMI_CRYPT_CIPHER_ALGORITHM_VALID(cipher_algorithm)
      || !IPMI_CRYPT_CIPHER_MODE_VALID(cipher_mode)
      || !data
      || !data_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if (cipher_algorithm == IPMI_CRYPT_CIPHER_AES)
    {
      if (!iv || !iv_len)
        {
          errno = EINVAL;
          return (-1);
        }

      gcry_cipher_algorithm = GCRY_CIPHER_AES;
      expected_cipher_key_len = IPMI_AES_CBC_128_KEY_LENGTH;
      expected_cipher_block_len = IPMI_AES_CBC_128_BLOCK_LENGTH;
    }
  else
    {
      errno = EINVAL;
      ipmi_debug("_ipmi_crypt_cipher_info: Invalid parameters");
      return (-1);
    }

  if (cipher_mode == IPMI_CRYPT_CIPHER_MODE_NONE)
    gcry_cipher_mode = GCRY_CIPHER_MODE_NONE;
  else
    gcry_cipher_mode = GCRY_CIPHER_MODE_CBC;

  if ((cipher_keylen = ipmi_crypt_cipher_key_len(cipher_algorithm)) < 0)
    {
      ipmi_debug("ipmi_crypt_cipher_encrypt: ipmi_crypt_cipher_key_len");
      return (-1);
    }
  
  if ((cipher_blocklen = ipmi_crypt_cipher_block_len(cipher_algorithm)) < 0)
    {
      ipmi_debug("ipmi_crypt_cipher_encrypt: ipmi_crypt_cipher_block_len");
      return (-1);
    }

  if (cipher_keylen < expected_cipher_key_len
      || cipher_blocklen != expected_cipher_block_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if (cipher_algorithm == IPMI_CRYPT_CIPHER_AES)
    {
      if (iv_len < cipher_blocklen)
        {
          errno = EINVAL;
          return (-1);
        }

      if (data_len % cipher_blocklen != 0)
        {
          errno = EINVAL;
          return (-1);
        }

      if (iv_len > cipher_blocklen)
        iv_len = cipher_blocklen;

      if (key && key_len > expected_cipher_key_len)
        key_len = expected_cipher_key_len;
    }

  if (_ipmi_init_crypt() < 0)
    return (-1);

  if ((e = gcry_cipher_open(&h,
                            gcry_cipher_algorithm,
                            gcry_cipher_mode,
                            0) != GPG_ERR_NO_ERROR))
    {
      ipmi_debug("gcry_cipher_open: %s", gcry_strerror(e));
      return (-1);
    }
  
  if (key && key_len)
    {
      if ((e = gcry_cipher_setkey(h,
                                  (void *)key,
                                  key_len)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_setkey: %s", gcry_strerror(e));
          return (-1);
        }
    }

  if (iv && iv_len)
    {
      if ((e = gcry_cipher_setiv(h, (void *)iv, iv_len)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_setiv: %s", gcry_strerror(e));
          return (-1);
        }
    }

  if (encrypt_flag)
    {
      if ((e = gcry_cipher_encrypt(h,
                                   (void *)data,
                                   data_len,
                                   NULL,
                                   0)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_encrypt: %s", gcry_strerror(e));
          return (-1);
        }
    }
  else
    {
      if ((e = gcry_cipher_decrypt(h,
                                   (void *)data,
                                   data_len,
                                   NULL,
                                   0)) != GPG_ERR_NO_ERROR)
        {
          ipmi_debug("gcry_cipher_decrypt: %s", gcry_strerror(e));
          return (-1);
        }
    }

  gcry_cipher_close(h);

  return (data_len);
}

int32_t
ipmi_crypt_cipher_encrypt(int cipher_algorithm,
                          int cipher_mode,
                          uint8_t *key,
                          uint32_t key_len,
                          uint8_t *iv,
                          uint32_t iv_len,
                          uint8_t *data,
                          uint32_t data_len)
{
  return _cipher_crypt(cipher_algorithm,
                       cipher_mode,
                       key,
                       key_len,
                       iv,
                       iv_len,
                       data,
                       data_len,
                       1);
}

int32_t
ipmi_crypt_cipher_decrypt(int cipher_algorithm,
                          int cipher_mode,
                          uint8_t *key,
                          uint32_t key_len,
                          uint8_t *iv,
                          uint32_t iv_len,
                          uint8_t *data,
                          uint32_t data_len)
{
  return _cipher_crypt(cipher_algorithm,
                       cipher_mode,
                       key,
                       key_len,
                       iv,
                       iv_len,
                       data,
                       data_len,
                       0);
}

static int32_t
_ipmi_crypt_cipher_info(int cipher_algorithm, int cipher_info)
{
  int gcry_cipher_algorithm, gcry_crypt_cipher_info_what;
  gcry_error_t e;
  size_t len;

  if (!IPMI_CRYPT_CIPHER_ALGORITHM_VALID(cipher_algorithm))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!IPMI_CRYPT_CIPHER_INFO_VALID(cipher_info))
    {
      errno = EINVAL;
      ipmi_debug("_ipmi_crypt_cipher_info: Invalid parameters");
      return (-1);
    }
  
  if (cipher_algorithm == IPMI_CRYPT_CIPHER_AES)
    gcry_cipher_algorithm = GCRY_CIPHER_AES;
  else
    {
      errno = EINVAL;
      ipmi_debug("_ipmi_crypt_cipher_info: Invalid parameters");
      return (-1);
    }

  if (cipher_info == IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH)
    gcry_crypt_cipher_info_what = GCRYCTL_GET_KEYLEN;
  else
    gcry_crypt_cipher_info_what = GCRYCTL_GET_BLKLEN;

  if (_ipmi_init_crypt() < 0)
    return (-1);

  if ((e = gcry_cipher_algo_info(gcry_cipher_algorithm,
                                 gcry_crypt_cipher_info_what,
                                 NULL,
                                 &len)) != GPG_ERR_NO_ERROR)
    {
      ipmi_debug("gcry_cipher_algo_info: %s", gcry_strerror(e));
      return (-1);
    }

  return (len);
}
                        

int32_t
ipmi_crypt_cipher_key_len(int cipher_algorithm)
{
  return _ipmi_crypt_cipher_info(cipher_algorithm, IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH);
}

int32_t 
ipmi_crypt_cipher_block_len(int cipher_algorithm)
{
  return _ipmi_crypt_cipher_info(cipher_algorithm, IPMI_CRYPT_CIPHER_INFO_BLOCK_LENGTH);
}

int32_t 
ipmi_calculate_sik(uint8_t authentication_algorithm,
                   uint8_t *key,
                   uint32_t key_len,
                   uint8_t *remote_console_random_number,
                   uint32_t remote_console_random_number_len,
                   uint8_t *managed_system_random_number,
                   uint32_t managed_system_random_number_len,
                   uint8_t requested_privilege_level,
                   uint8_t *user_name,
                   uint8_t user_name_len,
                   uint8_t *sik,
                   uint32_t sik_len)
{
  /* key can be NULL, indicating a empty key */
  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || (key && !key_len)
      || !remote_console_random_number
      || remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH
      || !managed_system_random_number
      || managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH
      || !IPMI_PRIVILEGE_LEVEL_VALID(requested_privilege_level)
      || (!user_name && user_name_len != 0)
      || !sik
      || !sik_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      /* SPEC: Not sure what to do */
      memset(sik, '\0', sik_len);
      return (0);
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
           || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      int hash_algorithm, hash_flags, expected_digest_len, crypt_digest_len, 
        computed_digest_len;
      unsigned int hash_data_len;
      uint8_t hash_data[IPMI_MAX_KEY_DATA_LENGTH];
      
      if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
        {
          hash_algorithm = IPMI_CRYPT_HASH_SHA1;
          hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
          expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
        }
      else
        {
          hash_algorithm = IPMI_CRYPT_HASH_MD5;
          hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
          expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
        }

      if ((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0)
        return (-1);

      ERR_EXIT (crypt_digest_len == expected_digest_len);

      if (sik_len < expected_digest_len)
        {
          errno = EINVAL;
          return (-1);
        }

      key_len = (key_len > IPMI_MAX_SIK_KEY_LENGTH) ? IPMI_MAX_SIK_KEY_LENGTH : key_len;

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

      memcpy(hash_data + hash_data_len, 
             (void *)&requested_privilege_level, 
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

      if ((computed_digest_len =  ipmi_crypt_hash(hash_algorithm,
                                                  hash_flags,
                                                  key,
                                                  key_len,
                                                  hash_data,
                                                  hash_data_len,
                                                  sik,
                                                  sik_len)) < 0)
        return (-1);

      if (computed_digest_len != crypt_digest_len)
        {
          ipmi_debug("ipmi_crypt_hash: invalid digest length returned");
          return (-1);
        }

      return (computed_digest_len);
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

static int32_t
_calculate_k_rakp_none(uint8_t *k,
                       uint32_t k_len,
                       uint8_t *constant,
                       uint32_t constant_len)
{
  /* SPEC: achu: The spec doesn't give information on what to do if
   * rakp is none.  So we're just going to say a NULL key.
   */

  if (!k
      || !k_len
      || !constant
      || !constant_len)
    {
      errno = EINVAL;
      ipmi_debug("_calculate_k_rakp_none: Invalid parameters");
      return (-1);
    }
 
  memset(k, '\0', k_len);
  return (k_len);
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
  unsigned int crypt_digest_len;
  
  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID(hash_algorithm)
      || !sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len
      || k_len < expected_digest_len 
      || constant_len < expected_digest_len 
      || sik_key_len < expected_digest_len)
    {
      errno = EINVAL;
      ipmi_debug("_calculate_k_rakp_hmac: Invalid parameters");
      return (-1);
    } 

  if ((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0)
    {
      errno = EINVAL;
      ipmi_debug("_calculate_k_rakp_hmac: ipmi_crypt_hash_digest_len");
      return (-1);
    }

  if (crypt_digest_len != expected_digest_len)
    {
      errno = EINVAL;
      ipmi_debug("_calculate_k_rakp_hmac: Invalid parameters");
      return (-1);
    }

  /* SPEC: achu: I believe the length of the constant you pass in
   * is the digest_len, atleast according to IPMI 2.0 Spec Section
   * 13.32, "constants are constructed using a hexadecimal octet
   * value repeated up to the HMAC block size in length starting
   * with the constant 01h".
   */
  if ((computed_digest_len =  ipmi_crypt_hash(hash_algorithm,
                                              GCRY_MD_FLAG_HMAC,
                                              sik_key,
                                              crypt_digest_len,
                                              constant,
                                              crypt_digest_len,
                                              k,
                                              k_len)) < 0)
    return (-1);

  if (computed_digest_len != crypt_digest_len)
    {
      ipmi_debug("ipmi_crypt_hash: invalid digest length returned");
      return (-1);
    }

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
  if (!sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len)
    {
      errno = EINVAL;
      ipmi_debug("_calculate_k_rakp_hmac_sha1: Invalid parameters");
      return (-1);
    }

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
  if (!sik_key
      || !sik_key_len
      || !k
      || !k_len
      || !constant
      || !constant_len)
    {
      errno = EINVAL;
      ipmi_debug("_calculate_k_rakp_hmac_md5: Invalid parameters");
      return (-1);
    }

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
  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || ((authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
	   || authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
	  && (!sik_key || !sik_key_len))
      || !k
      || !k_len
      || !constant
      || !constant_len)
    {
      errno = EINVAL;
      ipmi_debug("_ipmi_calculate_k: Invalid parameters");
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    return _calculate_k_rakp_none(k, k_len, constant, constant_len);
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    return _calculate_k_rakp_hmac_sha1(sik_key, 
                                       sik_key_len, 
                                       k, 
                                       k_len, 
                                       constant, 
                                       constant_len);
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    return _calculate_k_rakp_hmac_md5(sik_key, 
                                      sik_key_len, 
                                      k, 
                                      k_len, 
                                      constant, 
                                      constant_len);
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
ipmi_calculate_rakp_3_key_exchange_authentication_code(int8_t authentication_algorithm, 
                                                       uint8_t *authentication_key, 
                                                       uint32_t authentication_key_len, 
                                                       uint8_t *managed_system_random_number, 
                                                       uint32_t managed_system_random_number_len, 
                                                       uint32_t remote_console_session_id, 
                                                       uint8_t name_only_lookup, 
                                                       uint8_t requested_maximum_privilege_level, 
                                                       uint8_t *user_name, 
                                                       uint8_t user_name_length, 
                                                       uint8_t *key_exchange_authentication_code, 
                                                       uint32_t key_exchange_authentication_code_len)
{
  uint8_t priv_byte = 0;
  uint8_t buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint32_t buf_index = 0;
  uint8_t digest[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t hash_algorithm, hash_flags;
  int32_t digest_len, expected_digest_len;
  
  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !managed_system_random_number
      || managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH
      || !IPMI_PRIVILEGE_LEVEL_VALID(requested_maximum_privilege_level)
      || (user_name && user_name_length > IPMI_MAX_USER_NAME_LENGTH)
      || (!user_name && user_name_length)
      || !key_exchange_authentication_code)
    {
      errno = EINVAL;
      return (-1);
    }

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      memset(key_exchange_authentication_code, '\0', key_exchange_authentication_code_len);
      return (0);
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      if (key_exchange_authentication_code_len < IPMI_HMAC_SHA1_DIGEST_LENGTH)
        {
          errno = EINVAL;
          return (-1);
        }

      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5)
    {
      if (key_exchange_authentication_code_len < IPMI_HMAC_MD5_DIGEST_LENGTH)
        {
          errno = EINVAL;
          return (-1);
        }

      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else
    {
      errno = EINVAL;
      return (-1);
    }

  memset(buf, '\0', IPMI_MAX_PAYLOAD_LENGTH);
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
  priv_byte |= (requested_maximum_privilege_level & 0xF);
  buf[buf_index] = priv_byte;
  buf_index++;
  buf[buf_index] = user_name_length;
  buf_index++;
  if (user_name && user_name_length)
    {
      memcpy(buf + buf_index, user_name, user_name_length);
      buf_index++;
    }

  if ((digest_len = ipmi_crypt_hash(hash_algorithm,
                                    hash_flags,
                                    authentication_key,
                                    authentication_key_len,
                                    buf,
                                    buf_index,
                                    digest,
                                    IPMI_MAX_PAYLOAD_LENGTH)) < 0)
    return (-1);
      
  if (digest_len != expected_digest_len)
    {
      errno = EINVAL;
      return (-1);
    }
  
  memcpy(key_exchange_authentication_code, digest, digest_len);
  return (digest_len);
}

int8_t
ipmi_rmcpplus_check_payload_pad(uint8_t confidentiality_algorithm,
				fiid_obj_t obj_rmcpplus_payload)
{
  if (!IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !fiid_obj_valid(obj_rmcpplus_payload))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_payload, tmpl_rmcpplus_payload);

  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    /* No padding */
    return (1);
  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
    {
      uint8_t confidentiality_trailer[IPMI_MAX_PAYLOAD_LENGTH];
      int32_t confidentiality_trailer_len;
      int8_t pad_len;
      int i;

      FIID_OBJ_GET_DATA_LEN(confidentiality_trailer_len,
                            obj_rmcpplus_payload,
                            "confidentiality_trailer",
                            confidentiality_trailer,
                            IPMI_MAX_PAYLOAD_LENGTH);

      if (!confidentiality_trailer_len)
        {
          errno = EINVAL;
          return (-1);
        }

      pad_len = confidentiality_trailer[confidentiality_trailer_len - 1];

      if ((confidentiality_trailer_len - 1) != pad_len)
        {
          errno = EINVAL;
          return (-1);
        }

      for (i = 0; i < pad_len; i++)
        {
          if (confidentiality_trailer[i] != i + 1)
            return (0);
        }

      return (1);
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

int8_t
ipmi_rmcpplus_check_integrity_pad(fiid_obj_t obj_rmcpplus_session_trlr)
{
  uint8_t integrity_pad[IPMI_MAX_PAYLOAD_LENGTH];
  uint64_t pad_length;
  int i;
    
  if (!fiid_obj_valid(obj_rmcpplus_session_trlr))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  FIID_OBJ_GET(obj_rmcpplus_session_trlr, "pad_length", &pad_length);

  if (!pad_length)
    return (1);

  if (pad_length > IPMI_INTEGRITY_PAD_MULTIPLE)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET_DATA(obj_rmcpplus_session_trlr,
                    "integrity_pad",
                    integrity_pad,
                    IPMI_MAX_PAYLOAD_LENGTH);
  
  for (i = 0; i < pad_length; i++)
    {
      if (integrity_pad[i] != IPMI_INTEGRITY_PAD_DATA)
        return (0);
    }
  
  return (1);
}

int8_t
ipmi_rmcpplus_check_rakp_message_2_key_exchange_authentication_code(int8_t authentication_algorithm,
								    uint8_t *authentication_key,
								    uint32_t authentication_key_len,
								    uint32_t remote_console_session_id,
								    uint32_t managed_system_session_id,
								    uint8_t *remote_console_random_number,
								    uint32_t remote_console_random_number_len,
								    uint8_t *managed_system_random_number,
								    uint32_t managed_system_random_number_len,
								    uint8_t *managed_system_guid,
								    uint32_t managed_system_guid_len,
								    uint8_t name_only_lookup,
								    uint8_t requested_maximum_privilege_level,
								    uint8_t *user_name,
								    uint8_t user_name_length,
								    fiid_obj_t obj_cmd)
{
  uint8_t priv_byte = 0;
  uint8_t buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint32_t buf_index = 0;
  uint8_t digest[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t hash_algorithm, hash_flags;
  int32_t digest_len;
  uint8_t key_exchange_authentication_code[IPMI_MAX_PAYLOAD_LENGTH];
  uint32_t key_exchange_authentication_code_len;
  int32_t compare_len;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !remote_console_random_number
      || remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH
      || !managed_system_random_number
      || managed_system_random_number_len < IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH
      || !managed_system_guid
      || managed_system_guid_len < IPMI_MANAGED_SYSTEM_GUID_LENGTH
      || !IPMI_PRIVILEGE_LEVEL_VALID(requested_maximum_privilege_level)
      || (user_name && user_name_length > IPMI_MAX_USER_NAME_LENGTH)
      || (!user_name && user_name_length)
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }

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
  else
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET_DATA_LEN(key_exchange_authentication_code_len,
                        obj_cmd,
                        "key_exchange_authentication_code",
                        key_exchange_authentication_code,
                        IPMI_MAX_PAYLOAD_LENGTH);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      if (key_exchange_authentication_code_len)
        return (0);
      return (1);
    }

  if (key_exchange_authentication_code_len < compare_len)
    return (0);
 
  memset(buf, '\0', IPMI_MAX_PAYLOAD_LENGTH);
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
  priv_byte |= (requested_maximum_privilege_level & 0xF);
  buf[buf_index] = priv_byte;
  buf_index++;
  buf[buf_index] = user_name_length;
  buf_index++;
  if (user_name && user_name_length)
    {
      memcpy(buf + buf_index, user_name, user_name_length);
      buf_index += user_name_length;
    }

  if ((digest_len = ipmi_crypt_hash(hash_algorithm,
                                    hash_flags,
                                    authentication_key,
                                    authentication_key_len,
                                    buf,
                                    buf_index,
                                    digest,
                                    IPMI_MAX_PAYLOAD_LENGTH)) < 0)
    return (-1);

  if (key_exchange_authentication_code_len != digest_len)
    return (0);

  return (memcmp(digest, key_exchange_authentication_code, digest_len) ? 0 : 1);
}

int8_t 
ipmi_rmcpplus_check_rakp_message_4_integrity_check_value(int8_t authentication_algorithm,
							 uint8_t *sik_key,
							 uint32_t sik_key_len,
							 uint8_t *remote_console_random_number,
							 uint32_t remote_console_random_number_len,
							 uint32_t managed_system_session_id,
							 uint8_t *managed_system_guid,
							 uint32_t managed_system_guid_len,
							 fiid_obj_t obj_cmd)
{
  uint8_t buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint32_t buf_index = 0;
  uint8_t digest[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t hash_algorithm, hash_flags;
  int32_t digest_len;
  int32_t compare_len;
  uint8_t integrity_check_value[IPMI_MAX_PAYLOAD_LENGTH];
  uint32_t integrity_check_value_len;

  if (!IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !remote_console_random_number
      || remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH
      || !managed_system_guid
      || managed_system_guid_len < IPMI_MANAGED_SYSTEM_GUID_LENGTH
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_rakp_message_4);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      if (!sik_key || sik_key_len < IPMI_HMAC_SHA1_DIGEST_LENGTH)
        {
          errno = EINVAL;
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
          errno = EINVAL;
          return (-1);
        }

      compare_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
    }
  else
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET_DATA_LEN(integrity_check_value_len,
                        obj_cmd,
                        "integrity_check_value_len",
                        integrity_check_value,
                        IPMI_MAX_PAYLOAD_LENGTH);
  
  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE)
    {
      if (integrity_check_value_len)
        return (0);
      return (1);
    }

  if (integrity_check_value_len < compare_len)
    return (0);

  memset(buf, '\0', IPMI_MAX_PAYLOAD_LENGTH);
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

  if ((digest_len = ipmi_crypt_hash(hash_algorithm,
                                    hash_flags,
                                    sik_key,
                                    sik_key_len,
                                    buf,
                                    buf_index,
                                    digest,
                                    IPMI_MAX_PAYLOAD_LENGTH)) < 0)
    return (-1);

  if (digest_len < compare_len)
    {
      errno = EINVAL;
      return (-1);
    }

  return (memcmp(digest, integrity_check_value, compare_len) ? 0 : 1);
}

int8_t 
ipmi_rmcpplus_check_session_trlr(int8_t integrity_algorithm,
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
  unsigned int expected_digest_len, compare_digest_len, hash_data_len, integrity_digest_len;
  uint8_t hash_data[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t integrity_digest[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t authentication_code[IPMI_MAX_PAYLOAD_LENGTH];
  uint32_t authentication_code_len;
  
  if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !pkt
      || !pkt_len
      || !fiid_obj_valid(obj_rmcpplus_session_trlr))
    {
      errno = EINVAL;
      return (-1);
    }
  
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
  else
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = 0;
      expected_digest_len = MD5_DIGEST_LENGTH;
      compare_digest_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  
  if ((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0)
    return (-1);
     
  ERR_EXIT (crypt_digest_len == expected_digest_len);
  
  ERR_EXIT (!((rmcp_header_len = fiid_template_len_bytes(tmpl_rmcp_hdr)) < 0));

  FIID_OBJ_GET_DATA_LEN(authentication_code_len,
                        obj_rmcpplus_session_trlr,
                        "authentication_code",
                        authentication_code,
                        IPMI_MAX_PAYLOAD_LENGTH);

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    {
      if (authentication_code_len)
        return (0);
      return (1);
    }
  
  if  (authentication_code_len != compare_digest_len)
    return (0);

  /* Packet is too short, packet must be bogus :-) */
  if (pkt_len <= (rmcp_header_len + compare_digest_len))
    return (0);

  memset(hash_data, '\0', IPMI_MAX_PAYLOAD_LENGTH);

  hash_data_len = 0;
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128 
      && authentication_code_data 
      && authentication_code_data_len)
    {
      memcpy(hash_data + hash_data_len,
             authentication_code_data,
             authentication_code_data_len);
      hash_data_len += authentication_code_data_len;
    }

  memcpy(hash_data + hash_data_len, pkt + rmcp_header_len, pkt_len - rmcp_header_len - compare_digest_len);
  hash_data_len += pkt_len - rmcp_header_len - compare_digest_len;
  
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

  return (memcmp(integrity_digest, authentication_code, compare_digest_len) ? 0 : 1);
}

int8_t
ipmi_rmcpplus_check_payload_type(fiid_obj_t obj_rmcpplus_session_hdr, uint8_t payload_type)
{
  uint64_t val;
  int32_t len;

  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !obj_rmcpplus_session_hdr)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, (uint8_t *)"payload_type");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_rmcpplus_session_hdr, "payload_type", &val);

  return ((payload_type == val) ? 1 : 0);
}

int8_t
ipmi_rmcpplus_check_status_code(fiid_obj_t obj_cmd,
				uint8_t status_code)
{
  uint64_t val;
  int32_t len;

  if (!fiid_obj_valid(obj_cmd)
      || !RMCPPLUS_STATUS_VALID(status_code))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_rs) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_3) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) != 1)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_cmd, (uint8_t *)"rmcpplus_status_code");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, "rmcpplus_status_code", &val);

  return ((status_code == val) ? 1 : 0);
}
                            
int8_t 
ipmi_rmcpplus_check_message_tag(fiid_obj_t obj_cmd, uint8_t message_tag)
{
  uint64_t val;
  int32_t len;

  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_rq) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_rs) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_1) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_3) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) != 1)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_FIELD_LEN (len, obj_cmd, (uint8_t *)"message_tag");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, "message_tag", &val);

  return ((message_tag == val) ? 1 : 0);
}

int8_t 
ipmi_rmcpplus_check_remote_console_session_id(fiid_obj_t obj_cmd, uint32_t remote_console_session_id)
{
  uint64_t val;
  int32_t len;

  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_rq) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_rs) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) != 1
      && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) != 1)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_FIELD_LEN (len, obj_cmd, (uint8_t *)"remote_console_session_id");
  
  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, "remote_console_session_id", &val);

  return ((remote_console_session_id == val) ? 1 : 0);
}
