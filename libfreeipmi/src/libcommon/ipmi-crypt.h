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

#ifndef _IPMI_CRYPT_H
#define _IPMI_CRYPT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define IPMI_CRYPT_HASH_SHA1             0x00
#define IPMI_CRYPT_HASH_MD5              0x01

#define IPMI_CRYPT_HASH_ALGORITHM_VALID(__hash_algorithm) \
        (((__hash_algorithm) == IPMI_CRYPT_HASH_SHA1 \
          || (__hash_algorithm) == IPMI_CRYPT_HASH_MD5) ? 1 : 0)

#define IPMI_CRYPT_HASH_FLAGS_HMAC       0x01

#define IPMI_CRYPT_CIPHER_AES            0x00

#define IPMI_CRYPT_CIPHER_ALGORITHM_VALID(__cipher_algorithm) \
        (((__cipher_algorithm) == IPMI_CRYPT_CIPHER_AES) ? 1 : 0)

#define IPMI_CRYPT_CIPHER_MODE_NONE      0x00
#define IPMI_CRYPT_CIPHER_MODE_CBC       0x01

#define IPMI_CRYPT_CIPHER_MODE_VALID(__cipher_mode) \
        (((__cipher_mode) == IPMI_CRYPT_CIPHER_MODE_NONE \
          || (__cipher_mode) == IPMI_CRYPT_CIPHER_MODE_CBC) ? 1 : 0)

#define IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH   0x00
#define IPMI_CRYPT_CIPHER_INFO_BLOCK_LENGTH 0x01

#define IPMI_CRYPT_CIPHER_INFO_VALID(__cipher_info) \
        (((__cipher_info) == IPMI_CRYPT_CIPHER_INFO_KEY_LENGTH \
          || (__cipher_info) == IPMI_CRYPT_CIPHER_INFO_BLOCK_LENGTH) ? 1 : 0)

#define IPMI_CRYPT_AES_CBC_128_IV_LENGTH         16
#define IPMI_CRYPT_AES_CBC_128_KEY_LENGTH        16
#define IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH      16

/* ipmi_crypt_init
 *
 * Must be called first before anything else that may use crypt
 * functions.  In threaded programs, must be called before threads are
 * created.
 *
 * Returns 0 on success, -1 on error.
 */
int8_t ipmi_crypt_init(void);

int32_t ipmi_crypt_hash(int hash_algorithm, int hash_flags, uint8_t *key, uint32_t key_len, uint8_t *hash_data, uint32_t hash_data_len, uint8_t *digest, uint32_t digest_len);

int32_t ipmi_crypt_hash_digest_len(int hash_algorithm);

int32_t ipmi_crypt_cipher_encrypt(int cipher_algorithm, int cipher_mode, uint8_t *key, uint32_t key_len, uint8_t *iv, uint32_t iv_len, uint8_t *data, uint32_t data_len);

int32_t ipmi_crypt_cipher_decrypt(int cipher_algorithm, int cipher_mode, uint8_t *key, uint32_t key_len, uint8_t *iv, uint32_t iv_len, uint8_t *data, uint32_t data_len);

int32_t ipmi_crypt_cipher_key_len(int cipher_algorithm);

int32_t ipmi_crypt_cipher_block_len(int cipher_algorithm);

#ifdef __cplusplus
}
#endif

#endif
