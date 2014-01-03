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

#ifndef IPMI_CRYPT_H
#define IPMI_CRYPT_H

#include <stdint.h>

#define IPMI_CRYPT_HASH_SHA1             0x00
#define IPMI_CRYPT_HASH_MD5              0x01
#define IPMI_CRYPT_HASH_SHA256           0x02

#define IPMI_CRYPT_HASH_ALGORITHM_VALID(__hash_algorithm)       \
  (((__hash_algorithm) == IPMI_CRYPT_HASH_SHA1                  \
    || (__hash_algorithm) == IPMI_CRYPT_HASH_MD5                \
    || (__hash_algorithm) == IPMI_CRYPT_HASH_SHA256) ? 1 : 0)

#define IPMI_CRYPT_HASH_FLAGS_HMAC       0x01

#define IPMI_CRYPT_CIPHER_AES            0x00

#define IPMI_CRYPT_CIPHER_ALGORITHM_VALID(__cipher_algorithm)   \
  (((__cipher_algorithm) == IPMI_CRYPT_CIPHER_AES) ? 1 : 0)

#define IPMI_CRYPT_CIPHER_MODE_NONE      0x00
#define IPMI_CRYPT_CIPHER_MODE_CBC       0x01

#define IPMI_CRYPT_CIPHER_MODE_VALID(__cipher_mode)             \
  (((__cipher_mode) == IPMI_CRYPT_CIPHER_MODE_NONE              \
    || (__cipher_mode) == IPMI_CRYPT_CIPHER_MODE_CBC) ? 1 : 0)

#define IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH   0x00
#define IPMI_CRYPT_CIPHER_INFO_BLOCK_LENGTH 0x01

#define IPMI_CRYPT_CIPHER_INFO_VALID(__cipher_info)                     \
  (((__cipher_info) == IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH                \
    || (__cipher_info) == IPMI_CRYPT_CIPHER_INFO_BLOCK_LENGTH) ? 1 : 0)

#define IPMI_CRYPT_AES_CBC_128_IV_LENGTH         16
#define IPMI_CRYPT_AES_CBC_128_KEY_LENGTH        16
#define IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH      16

/* crypt_init
 *
 * Must be called first before anything else that may use crypt
 * functions.  In threaded programs, must be called before threads are
 * created.
 *
 * Returns 0 on success, -1 on error.
 */
int crypt_init (void);

/* return length of data written into buffer on success, -1 on error */
int crypt_hash (unsigned int hash_algorithm,
                     unsigned int hash_flags,
                     const void *key,
                     unsigned int key_len,
                     const void *hash_data,
                     unsigned int hash_data_len,
                     void *digest,
                     unsigned int digest_len);

int crypt_hash_digest_len (unsigned int hash_algorithm);

/* return length of data written into buffer on success, -1 on error */
int crypt_cipher_encrypt (unsigned int cipher_algorithm,
                               unsigned int cipher_mode,
                               const void *key,
                               unsigned int key_len,
                               const void *iv,
                               unsigned int iv_len,
                               void *data,
                               unsigned int data_len);

/* return length of data written into buffer on success, -1 on error */
int crypt_cipher_decrypt (unsigned int cipher_algorithm,
                               unsigned int cipher_mode,
                               const void *key,
                               unsigned int key_len,
                               const void *iv,
                               unsigned int iv_len,
                               void *data,
                               unsigned int data_len);

int crypt_cipher_key_len (unsigned int cipher_algorithm);

int crypt_cipher_block_len (unsigned int cipher_algorithm);

#endif /* IPMI_CRYPT_H */
