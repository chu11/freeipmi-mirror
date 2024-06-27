/*
 * Copyright (C) 2003-2024 FreeIPMI Core Team
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

#ifndef IPMI_CRYPT_GCRYPT_H
#define IPMI_CRYPT_GCRYPT_H
#ifdef WITH_ENCRYPTION

#include <stdint.h>

/* gcrypt specific implementations for ipmi crypt functions */

int gcrypt_init (void);

int gcrypt_hash (unsigned int hash_algorithm,
                 unsigned int hash_flags,
                 const void *key,
                 unsigned int key_len,
                 const void *hash_data,
                 unsigned int hash_data_len,
                 void *digest,
                 unsigned int digest_len);

int gcrypt_hash_digest_len (unsigned int hash_algorithm);

int gcrypt_cipher_encrypt (unsigned int cipher_algorithm,
                           unsigned int cipher_mode,
                           const void *key,
                           unsigned int key_len,
                           const void *iv,
                           unsigned int iv_len,
                           void *data,
                           unsigned int data_len);

int gcrypt_cipher_decrypt (unsigned int cipher_algorithm,
                           unsigned int cipher_mode,
                           const void *key,
                           unsigned int key_len,
                           const void *iv,
                           unsigned int iv_len,
                           void *data,
                           unsigned int data_len);

int gcrypt_cipher_key_len (unsigned int cipher_algorithm);

int gcrypt_cipher_block_len (unsigned int cipher_algorithm);

#endif /* !WITH_ENCRYPTION */
#endif /* IPMI_CRYPT_GCRYPT_H */
