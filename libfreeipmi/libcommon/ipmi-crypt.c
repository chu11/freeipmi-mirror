/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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
#include <errno.h>

#include "ipmi-crypt.h"
#include "ipmi-crypt-gcrypt.h"
#include "ipmi-trace.h"

#include "freeipmi-portability.h"

static int crypt_initialized = 0;

int
crypt_init (void)
{
#ifdef WITH_ENCRYPTION
  if (gcrypt_init () < 0)
    return (-1);
  crypt_initialized++;
  return (0);
#else /* !WITH_ENCRYPTION */
  /* Can run this init, but the actual encryption functions will fail */
  return (0);
#endif /* !WITH_ENCRYPTION */
}

int
crypt_hash (unsigned int hash_algorithm,
            unsigned int hash_flags,
            const void *key,
            unsigned int key_len,
            const void *hash_data,
            unsigned int hash_data_len,
            void *digest,
            unsigned int digest_len)
{
#ifdef WITH_ENCRYPTION
  int rv;

  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID (hash_algorithm)
      || (hash_data && !hash_data_len)
      || !digest
      || !digest_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  rv = gcrypt_hash (hash_algorithm,
                    hash_flags,
                    key,
                    key_len,
                    hash_data,
                    hash_data_len,
                    digest,
                    digest_len);
  return (rv);
#else /* !WITH_ENCRYPTION */
  SET_ERRNO (EPERM);
  return (-1);
#endif /* !WITH_ENCRYPTION */
}

int
crypt_hash_digest_len (unsigned int hash_algorithm)
{
#ifdef WITH_ENCRYPTION
  int rv;

  if (!IPMI_CRYPT_HASH_ALGORITHM_VALID (hash_algorithm))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  rv = gcrypt_hash_digest_len (hash_algorithm);
  return (rv);
#else /* !WITH_ENCRYPTION */
  SET_ERRNO (EPERM);
  return (-1);
#endif /* !WITH_ENCRYPTION */
}

int
crypt_cipher_encrypt (unsigned int cipher_algorithm,
                      unsigned int cipher_mode,
                      const void *key,
                      unsigned int key_len,
                      const void *iv,
                      unsigned int iv_len,
                      void *data,
                      unsigned int data_len)
{
#ifdef WITH_ENCRYPTION
  int rv;

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

  if (!crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  rv = gcrypt_cipher_encrypt (cipher_algorithm,
                              cipher_mode,
                              key,
                              key_len,
                              iv,
                              iv_len,
                              data,
                              data_len);
  return (rv);
#else /* !WITH_ENCRYPTION */
  SET_ERRNO (EPERM);
  return (-1);
#endif /* !WITH_ENCRYPTION */
}

int
crypt_cipher_decrypt (unsigned int cipher_algorithm,
                      unsigned int cipher_mode,
                      const void *key,
                      unsigned int key_len,
                      const void *iv,
                      unsigned int iv_len,
                      void *data,
                      unsigned int data_len)
{
#ifdef WITH_ENCRYPTION
  int rv;

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

  if (!crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  rv = gcrypt_cipher_decrypt (cipher_algorithm,
                              cipher_mode,
                              key,
                              key_len,
                              iv,
                              iv_len,
                              data,
                              data_len);
  return (rv);
#else /* !WITH_ENCRYPTION */
  SET_ERRNO (EPERM);
  return (-1);
#endif /* !WITH_ENCRYPTION */
}

int
crypt_cipher_key_len (unsigned int cipher_algorithm)
{
#ifdef WITH_ENCRYPTION
  int rv;

  if (cipher_algorithm != IPMI_CRYPT_CIPHER_AES)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  rv = gcrypt_cipher_key_len (cipher_algorithm);
  return (rv);
#else /* !WITH_ENCRYPTION */
  SET_ERRNO (EPERM);
  return (-1);
#endif /* !WITH_ENCRYPTION */
}

int
crypt_cipher_block_len (unsigned int cipher_algorithm)
{
#ifdef WITH_ENCRYPTION
  int rv;

  if (cipher_algorithm != IPMI_CRYPT_CIPHER_AES)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!crypt_initialized)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  rv = gcrypt_cipher_block_len (cipher_algorithm);
  return (rv);
#else /* !WITH_ENCRYPTION */
  SET_ERRNO (EPERM);
  return (-1);
#endif /* !WITH_ENCRYPTION */
}
