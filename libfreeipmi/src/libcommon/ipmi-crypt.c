/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

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
#include <gcrypt.h>
GCRY_THREAD_OPTION_PTHREAD_IMPL;

#include "ipmi-crypt.h"
#include "ipmi-trace.h"

#include "freeipmi-portability.h"

static int ipmi_crypt_initialized = 0;

static int
_gpg_error_to_errno (gcry_error_t e)
{
  /* be lazy right now */
  if (e == GPG_ERR_NO_ERROR)
    return (0);
  else
    return (EINVAL);
}

int
ipmi_crypt_init (void)
{
  gcry_error_t e;

  if ((e = gcry_control (GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread)) != GPG_ERR_NO_ERROR)
    {
      ERR_TRACE (gcry_strerror (e), e);
      SET_ERRNO (_gpg_error_to_errno (e));
      return (-1);
    }

  if (!gcry_check_version (GCRYPT_VERSION))
    {
      ERR_TRACE (gcry_strerror (e), e);
      SET_ERRNO (_gpg_error_to_errno (e));
      return (-1);
    }

  if ((e = gcry_control (GCRYCTL_DISABLE_SECMEM, 0)) != GPG_ERR_NO_ERROR)
    {
      ERR_TRACE (gcry_strerror (e), e);
      SET_ERRNO (_gpg_error_to_errno (e));
      return (-1);
    }

  if ((e = gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0)) != GPG_ERR_NO_ERROR)
    {
      ERR_TRACE (gcry_strerror (e), e);
      SET_ERRNO (_gpg_error_to_errno (e));
      return (-1);
    }

  ipmi_crypt_initialized++;
  return (0);
}

int
ipmi_crypt_hash (unsigned int hash_algorithm,
                 unsigned int hash_flags,
                 const void *key,
                 unsigned int key_len,
                 const void *hash_data,
                 unsigned int hash_data_len,
                 void *digest,
                 unsigned int digest_len)
{
  gcry_md_hd_t h = NULL;
  gcry_error_t e;
  int gcry_md_algorithm, gcry_md_flags = 0;
  unsigned int gcry_md_digest_len;
  void *digestPtr;
  int rv = -1;

  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID (hash_algorithm)
      || (hash_data && !hash_data_len)
      || !digest
      || !digest_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!ipmi_crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (hash_algorithm == IPMI_CRYPT_HASH_SHA1)
    gcry_md_algorithm = GCRY_MD_SHA1;
  else
    gcry_md_algorithm = GCRY_MD_MD5;

  if (hash_flags & IPMI_CRYPT_HASH_FLAGS_HMAC)
    gcry_md_flags |= GCRY_MD_FLAG_HMAC;

  if ((gcry_md_digest_len = gcry_md_get_algo_dlen (gcry_md_algorithm)) > digest_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((e = gcry_md_open (&h, gcry_md_algorithm, gcry_md_flags)) != GPG_ERR_NO_ERROR)
    {
      ERR_TRACE (gcry_strerror (e), e);
      SET_ERRNO (_gpg_error_to_errno (e));
      return (-1);
    }

  if (!h)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* achu: Technically any key length can be supplied.  We'll assume
   * callers have checked if the key is of a length they care about.
   */
  /* SPEC: There is no indication that if a NULL password/key is used,
   * that a zero padded password of some length should be the key.
   */
  if ((hash_flags & IPMI_CRYPT_HASH_FLAGS_HMAC) && key && key_len)
    {
      if ((e = gcry_md_setkey (h, key, key_len)) != GPG_ERR_NO_ERROR)
        {
          ERR_TRACE (gcry_strerror (e), e);
          SET_ERRNO (_gpg_error_to_errno (e));
          goto cleanup;
        }
    }

  if (hash_data && hash_data_len)
    gcry_md_write (h, (void *)hash_data, hash_data_len);

  gcry_md_final (h);

  if (!(digestPtr = gcry_md_read (h, gcry_md_algorithm)))
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  memcpy (digest, digestPtr, gcry_md_digest_len);
  rv = gcry_md_digest_len;
 cleanup:
  if (h)
    gcry_md_close (h);
  return (rv);
}

int
ipmi_crypt_hash_digest_len (unsigned int hash_algorithm)
{
  int gcry_md_algorithm;

  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID (hash_algorithm))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!ipmi_crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (hash_algorithm == IPMI_CRYPT_HASH_SHA1)
    gcry_md_algorithm = GCRY_MD_SHA1;
  else
    gcry_md_algorithm = GCRY_MD_MD5;

  return (gcry_md_get_algo_dlen (gcry_md_algorithm));
}

static int
_cipher_crypt (unsigned int cipher_algorithm,
               unsigned int cipher_mode,
               const void *key,
               unsigned int key_len,
               const void *iv,
               unsigned int iv_len,
               void *data,
               unsigned int data_len,
               int encrypt_flag)
{
  int gcry_cipher_algorithm, gcry_cipher_mode = 0;
  int cipher_keylen, cipher_blocklen;
  int expected_cipher_key_len, expected_cipher_block_len;
  gcry_cipher_hd_t h = NULL;
  gcry_error_t e;
  int rv = -1;

  if (cipher_algorithm != IPMI_CRYPT_CIPHER_AES
      || !IPMI_CRYPT_CIPHER_MODE_VALID (cipher_mode)
      || !iv
      || !iv_len
      || !data
      || !data_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  gcry_cipher_algorithm = GCRY_CIPHER_AES;
  expected_cipher_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;
  expected_cipher_block_len = IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;

  if (cipher_mode == IPMI_CRYPT_CIPHER_MODE_NONE)
    gcry_cipher_mode = GCRY_CIPHER_MODE_NONE;
  else
    gcry_cipher_mode = GCRY_CIPHER_MODE_CBC;

  if ((cipher_keylen = ipmi_crypt_cipher_key_len (cipher_algorithm)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if ((cipher_blocklen = ipmi_crypt_cipher_block_len (cipher_algorithm)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (cipher_keylen < expected_cipher_key_len
      || cipher_blocklen != expected_cipher_block_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (iv_len < cipher_blocklen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (data_len % cipher_blocklen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (iv_len > cipher_blocklen)
    iv_len = cipher_blocklen;

  if (key && key_len > expected_cipher_key_len)
    key_len = expected_cipher_key_len;

  if (!ipmi_crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((e = gcry_cipher_open (&h,
                             gcry_cipher_algorithm,
                             gcry_cipher_mode,
                             0) != GPG_ERR_NO_ERROR))
    {
      ERR_TRACE (gcry_strerror (e), e);
      SET_ERRNO (_gpg_error_to_errno (e));
      return (-1);
    }

  if (key && key_len)
    {
      if ((e = gcry_cipher_setkey (h,
                                   (void *)key,
                                   key_len)) != GPG_ERR_NO_ERROR)
        {
          ERR_TRACE (gcry_strerror (e), e);
          SET_ERRNO (_gpg_error_to_errno (e));
          goto cleanup;
        }
    }

  if (iv && iv_len)
    {
      if ((e = gcry_cipher_setiv (h, (void *)iv, iv_len)) != GPG_ERR_NO_ERROR)
        {
          ERR_TRACE (gcry_strerror (e), e);
          SET_ERRNO (_gpg_error_to_errno (e));
          goto cleanup;
        }
    }

  if (encrypt_flag)
    {
      if ((e = gcry_cipher_encrypt (h,
                                    (void *)data,
                                    data_len,
                                    NULL,
                                    0)) != GPG_ERR_NO_ERROR)
        {
          ERR_TRACE (gcry_strerror (e), e);
          SET_ERRNO (_gpg_error_to_errno (e));
          goto cleanup;
        }
    }
  else
    {
      if ((e = gcry_cipher_decrypt (h,
                                    (void *)data,
                                    data_len,
                                    NULL,
                                    0)) != GPG_ERR_NO_ERROR)
        {
          ERR_TRACE (gcry_strerror (e), e);
          SET_ERRNO (_gpg_error_to_errno (e));
          goto cleanup;
        }
    }

  rv = data_len;
 cleanup:
  if (h)
    gcry_cipher_close (h);
  return (rv);
}

int
ipmi_crypt_cipher_encrypt (unsigned int cipher_algorithm,
                           unsigned int cipher_mode,
                           const void *key,
                           unsigned int key_len,
                           const void *iv,
                           unsigned int iv_len,
                           void *data,
                           unsigned int data_len)
{
  return (_cipher_crypt (cipher_algorithm,
                         cipher_mode,
                         key,
                         key_len,
                         iv,
                         iv_len,
                         data,
                         data_len,
                         1));
}

int
ipmi_crypt_cipher_decrypt (unsigned int cipher_algorithm,
                           unsigned int cipher_mode,
                           const void *key,
                           unsigned int key_len,
                           const void *iv,
                           unsigned int iv_len,
                           void *data,
                           unsigned int data_len)
{
  return (_cipher_crypt (cipher_algorithm,
                         cipher_mode,
                         key,
                         key_len,
                         iv,
                         iv_len,
                         data,
                         data_len,
                         0));
}

static int
_ipmi_crypt_cipher_info (unsigned int cipher_algorithm, unsigned int cipher_info)
{
  int gcry_cipher_algorithm, gcry_crypt_cipher_info_what;
  gcry_error_t e;
  size_t len;

  if (cipher_algorithm != IPMI_CRYPT_CIPHER_AES
      || !IPMI_CRYPT_CIPHER_INFO_VALID (cipher_info))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  gcry_cipher_algorithm = GCRY_CIPHER_AES;

  if (cipher_info == IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH)
    gcry_crypt_cipher_info_what = GCRYCTL_GET_KEYLEN;
  else
    gcry_crypt_cipher_info_what = GCRYCTL_GET_BLKLEN;

  if (!ipmi_crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((e = gcry_cipher_algo_info (gcry_cipher_algorithm,
                                  gcry_crypt_cipher_info_what,
                                  NULL,
                                  &len)) != GPG_ERR_NO_ERROR)
    {
      ERR_TRACE (gcry_strerror (e), e);
      SET_ERRNO (_gpg_error_to_errno (e));
      return (-1);
    }

  return (len);
}


int
ipmi_crypt_cipher_key_len (unsigned int cipher_algorithm)
{
  return (_ipmi_crypt_cipher_info (cipher_algorithm, IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH));
}

int
ipmi_crypt_cipher_block_len (unsigned int cipher_algorithm)
{
  return (_ipmi_crypt_cipher_info (cipher_algorithm, IPMI_CRYPT_CIPHER_INFO_BLOCK_LENGTH));
}
