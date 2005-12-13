/*
   ipmi-rmcpplus-util.h - IPMI LAN Commands

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _IPMI_RMCPPLUS_UTIL_H
#define _IPMI_RMCPPLUS_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

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

#define IPMI_CRYPT_CIPHER_INFO_KEY_LEN   0x00
#define IPMI_CRYPT_CIPHER_INFO_BLOCK_LEN 0x01

#define IPMI_CRYPT_CIPHER_INFO_VALID(__cipher_info) \
        (((__cipher_info) == IPMI_CRYPT_CIPHER_INFO_KEY_LEN \
          || (__cipher_info) == IPMI_CRYPT_CIPHER_INFO_BLOCK_LEN) ? 1 : 0)

int8_t ipmi_init_crypt(void);

int32_t ipmi_crypt_hash(int hash_algorithm, int hash_flags, u_int8_t *key, u_int32_t key_len, u_int8_t *hash_data, u_int32_t hash_data_len, u_int8_t *digest, u_int32_t digest_len);

int32_t ipmi_crypt_hash_digest_len(int hash_algorithm);

int32_t ipmi_crypt_cipher_encrypt(int cipher_algorithm, int cipher_mode, u_int8_t *key, u_int32_t key_len, u_int8_t *iv, u_int32_t iv_len, u_int8_t *data, u_int32_t data_len);

int32_t ipmi_crypt_cipher_decrypt(int cipher_algorithm, int cipher_mode, u_int8_t *key, u_int32_t key_len, u_int8_t *iv, u_int32_t iv_len, u_int8_t *data, u_int32_t data_len);

int32_t ipmi_crypt_cipher_key_len(int cipher_algorithm);

int32_t ipmi_crypt_cipher_block_len(int cipher_algorithm);

int32_t ipmi_calculate_sik(u_int8_t authentication_algorithm, u_int8_t *key, u_int32_t key_len, u_int8_t *remote_console_random_number, u_int32_t remote_console_random_number_len, u_int8_t *managed_system_random_number, u_int32_t managed_system_random_number_len, u_int8_t requested_privilege_level, u_int8_t *username, u_int8_t username_len, u_int8_t *sik, u_int32_t sik_len);

int32_t ipmi_calculate_k1(u_int8_t authentication_algorithm, u_int8_t *sik_key, u_int32_t sik_key_len, u_int8_t *k1, u_int32_t k1_len);

int32_t ipmi_calculate_k2(u_int8_t authentication_algorithm, u_int8_t *sik_key, u_int32_t sik_key_len, u_int8_t *k2, u_int32_t k2_len);

int32_t check_rmcpplus_payload_pad(u_int8_t confidentiality_algorithm, fiid_obj_t obj_payload);

int32_t check_rmcpplus_integriy_pad(fiid_template_t tmpl_rmcpplus_trlr_session, fiid_obj_t obj_rmcpplus_trlr_session);

#ifdef __cplusplus
}
#endif

#endif
