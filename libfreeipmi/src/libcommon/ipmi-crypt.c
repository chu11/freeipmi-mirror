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
#if HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
#if HAVE_GCRYPT_H
#include <gcrypt.h>
GCRY_THREAD_OPTION_PTHREAD_IMPL;
#endif /* !HAVE_GCRYPT_H */

#include "ipmi-crypt.h"

#include "ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

static int ipmi_crypt_initialized = 0;

int8_t
ipmi_crypt_init(void)
{
#ifdef WITH_ENCRYPTION
  gcry_error_t e;

  ERR (!((e = gcry_control(GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread)) != GPG_ERR_NO_ERROR));

  ERR (gcry_check_version(GCRYPT_VERSION));

  ERR (!((e = gcry_control(GCRYCTL_DISABLE_SECMEM, 0)) != GPG_ERR_NO_ERROR));
  
  ERR (!((e = gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0)) != GPG_ERR_NO_ERROR));

  ipmi_crypt_initialized++;
  return (0);
#else /* !WITH_ENCRYPTION */
  /* Can run this init, but the actual encryption functions will fail */
  return (0);
#endif /* !WITH_ENCRYPTION */
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
#ifdef WITH_ENCRYPTION
  gcry_md_hd_t h;
  gcry_error_t e;
  int gcry_md_algorithm, gcry_md_flags = 0;
  unsigned int gcry_md_digest_len;
  uint8_t *digestPtr;

  ERR_EINVAL (IPMI_CRYPT_HASH_ALGORITHM_VALID(hash_algorithm)
	      && !(hash_data && !hash_data_len)
	      && digest
	      && digest_len);
    
  ERR(ipmi_crypt_initialized);

  if (hash_algorithm == IPMI_CRYPT_HASH_SHA1)
    gcry_md_algorithm = GCRY_MD_SHA1;
  else
    gcry_md_algorithm = GCRY_MD_MD5;

  if (hash_flags & IPMI_CRYPT_HASH_FLAGS_HMAC)
    gcry_md_flags |= GCRY_MD_FLAG_HMAC;

  ERR (!((gcry_md_digest_len = gcry_md_get_algo_dlen(gcry_md_algorithm)) > digest_len));

  ERR (!((e = gcry_md_open(&h, gcry_md_algorithm, gcry_md_flags)) != GPG_ERR_NO_ERROR));
      
  ERR (h);

  /* achu: Technically any key length can be supplied.  We'll assume
   * callers have checked if the key is of a length they care about.
   */
  /* SPEC: There is no indication that if a NULL password/key is used,
   * that a zero padded password of some length should be the key.
   */
  if ((hash_flags & IPMI_CRYPT_HASH_FLAGS_HMAC) && key && key_len)
    ERR (!((e = gcry_md_setkey(h, key, key_len)) != GPG_ERR_NO_ERROR));

  if (hash_data && hash_data_len)
    gcry_md_write(h, (void *)hash_data, hash_data_len);

  gcry_md_final(h);

  ERR ((digestPtr = gcry_md_read(h, gcry_md_algorithm)));

  memcpy(digest, digestPtr, gcry_md_digest_len);
  gcry_md_close(h);
  return (gcry_md_digest_len);
#else /* !WITH_ENCRYPTION */
  ERR_EPERM (0);
#endif /* !WITH_ENCRYPTION */
}

int32_t
ipmi_crypt_hash_digest_len(int hash_algorithm)
{
#ifdef WITH_ENCRYPTION
  int gcry_md_algorithm;

  ERR_EINVAL (IPMI_CRYPT_HASH_ALGORITHM_VALID(hash_algorithm));

  ERR(ipmi_crypt_initialized);

  if (hash_algorithm == IPMI_CRYPT_HASH_SHA1)
    gcry_md_algorithm = GCRY_MD_SHA1;
  else
    gcry_md_algorithm = GCRY_MD_MD5;

  return gcry_md_get_algo_dlen(gcry_md_algorithm);
#else /* !WITH_ENCRYPTION */
  ERR_EPERM (0);
#endif /* !WITH_ENCRYPTION */
}

#ifdef WITH_ENCRYPTION
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

  ERR_EINVAL (cipher_algorithm == IPMI_CRYPT_CIPHER_AES
	      && IPMI_CRYPT_CIPHER_MODE_VALID(cipher_mode)
	      && iv
	      && iv_len
	      && data
	      && data_len);

  gcry_cipher_algorithm = GCRY_CIPHER_AES;
  expected_cipher_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;
  expected_cipher_block_len = IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;

  if (cipher_mode == IPMI_CRYPT_CIPHER_MODE_NONE)
    gcry_cipher_mode = GCRY_CIPHER_MODE_NONE;
  else
    gcry_cipher_mode = GCRY_CIPHER_MODE_CBC;

  ERR (!((cipher_keylen = ipmi_crypt_cipher_key_len(cipher_algorithm)) < 0));
  
  ERR (!((cipher_blocklen = ipmi_crypt_cipher_block_len(cipher_algorithm)) < 0));

  ERR (!(cipher_keylen < expected_cipher_key_len
	 || cipher_blocklen != expected_cipher_block_len));

  ERR_EINVAL (!(iv_len < cipher_blocklen));

  ERR_EINVAL ((data_len % cipher_blocklen == 0));

  if (iv_len > cipher_blocklen)
    iv_len = cipher_blocklen;
  
  if (key && key_len > expected_cipher_key_len)
    key_len = expected_cipher_key_len;

  ERR(ipmi_crypt_initialized);

  ERR (!((e = gcry_cipher_open(&h,
			       gcry_cipher_algorithm,
			       gcry_cipher_mode,
			       0) != GPG_ERR_NO_ERROR)));
  
  if (key && key_len)
    ERR (!((e = gcry_cipher_setkey(h,
				   (void *)key,
				   key_len)) != GPG_ERR_NO_ERROR));

  if (iv && iv_len)
    ERR (!((e = gcry_cipher_setiv(h, (void *)iv, iv_len)) != GPG_ERR_NO_ERROR));

  if (encrypt_flag)
    ERR (!((e = gcry_cipher_encrypt(h,
				    (void *)data,
				    data_len,
				    NULL,
				    0)) != GPG_ERR_NO_ERROR));
  else
    ERR (!((e = gcry_cipher_decrypt(h,
				    (void *)data,
				    data_len,
				    NULL,
				    0)) != GPG_ERR_NO_ERROR));

  gcry_cipher_close(h);

  return (data_len);
}
#endif /* !WITH_ENCRYPTION */

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
#ifdef WITH_ENCRYPTION
  return _cipher_crypt(cipher_algorithm,
                       cipher_mode,
                       key,
                       key_len,
                       iv,
                       iv_len,
                       data,
                       data_len,
                       1);
#else /* !WITH_ENCRYPTION */
  ERR_EPERM (0);
#endif /* !WITH_ENCRYPTION */
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
#ifdef WITH_ENCRYPTION
  return _cipher_crypt(cipher_algorithm,
                       cipher_mode,
                       key,
                       key_len,
                       iv,
                       iv_len,
                       data,
                       data_len,
                       0);
#else /* !WITH_ENCRYPTION */
  ERR_EPERM (0);
#endif /* !WITH_ENCRYPTION */
}

#ifdef WITH_ENCRYPTION
static int32_t
_ipmi_crypt_cipher_info(int cipher_algorithm, int cipher_info)
{
  int gcry_cipher_algorithm, gcry_crypt_cipher_info_what;
  gcry_error_t e;
  size_t len;

  ERR_EINVAL (cipher_algorithm == IPMI_CRYPT_CIPHER_AES
	      && IPMI_CRYPT_CIPHER_INFO_VALID(cipher_info));

  gcry_cipher_algorithm = GCRY_CIPHER_AES;

  if (cipher_info == IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH)
    gcry_crypt_cipher_info_what = GCRYCTL_GET_KEYLEN;
  else
    gcry_crypt_cipher_info_what = GCRYCTL_GET_BLKLEN;

  ERR(ipmi_crypt_initialized);

  ERR (!((e = gcry_cipher_algo_info(gcry_cipher_algorithm,
				    gcry_crypt_cipher_info_what,
				    NULL,
				    &len)) != GPG_ERR_NO_ERROR));

  return (len);
}
#endif /* !WITH_ENCRYPTION */
                        
int32_t
ipmi_crypt_cipher_key_len(int cipher_algorithm)
{
#ifdef WITH_ENCRYPTION
  return _ipmi_crypt_cipher_info(cipher_algorithm, IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH);
#else /* !WITH_ENCRYPTION */
  ERR_EPERM (0);
#endif /* !WITH_ENCRYPTION */
}

int32_t 
ipmi_crypt_cipher_block_len(int cipher_algorithm)
{
#ifdef WITH_ENCRYPTION
  return _ipmi_crypt_cipher_info(cipher_algorithm, IPMI_CRYPT_CIPHER_INFO_BLOCK_LENGTH);
#else /* !WITH_ENCRYPTION */
  ERR_EPERM (0);
#endif /* !WITH_ENCRYPTION */
}
