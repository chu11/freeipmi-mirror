/*
   ipmi-rmcpplus-utils.h - IPMI RMCPPLUS Utils

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

#ifndef _IPMI_RMCPPLUS_UTILS_H
#define _IPMI_RMCPPLUS_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid.h>

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

int32_t ipmi_crypt_hash(int hash_algorithm, int hash_flags, uint8_t *key, uint32_t key_len, uint8_t *hash_data, uint32_t hash_data_len, uint8_t *digest, uint32_t digest_len);

int32_t ipmi_crypt_hash_digest_len(int hash_algorithm);

int32_t ipmi_crypt_cipher_encrypt(int cipher_algorithm, int cipher_mode, uint8_t *key, uint32_t key_len, uint8_t *iv, uint32_t iv_len, uint8_t *data, uint32_t data_len);

int32_t ipmi_crypt_cipher_decrypt(int cipher_algorithm, int cipher_mode, uint8_t *key, uint32_t key_len, uint8_t *iv, uint32_t iv_len, uint8_t *data, uint32_t data_len);

int32_t ipmi_crypt_cipher_key_len(int cipher_algorithm);

int32_t ipmi_crypt_cipher_block_len(int cipher_algorithm);

int32_t ipmi_calculate_sik(uint8_t authentication_algorithm, uint8_t *key, uint32_t key_len, uint8_t *remote_console_random_number, uint32_t remote_console_random_number_len, uint8_t *managed_system_random_number, uint32_t managed_system_random_number_len, uint8_t requested_privilege_level, uint8_t *user_name, uint8_t user_name_len, uint8_t *sik, uint32_t sik_len);

int32_t ipmi_calculate_k1(uint8_t authentication_algorithm, uint8_t *sik_key, uint32_t sik_key_len, uint8_t *k1, uint32_t k1_len);

int32_t ipmi_calculate_k2(uint8_t authentication_algorithm, uint8_t *sik_key, uint32_t sik_key_len, uint8_t *k2, uint32_t k2_len);

int32_t ipmi_calculate_rakp_3_key_exchange_authentication_code(int8_t authentication_algorithm, uint8_t *authentication_key, uint32_t authentication_key_len, uint8_t *managed_system_random_number, uint32_t managed_system_random_number_len, uint32_t remote_console_session_id, uint8_t name_only_lookup, uint8_t requested_maximum_privilege_level, uint8_t *user_name, uint8_t user_name_length, uint8_t *key_exchange_authentication_code, uint32_t key_exchange_authentication_code_len);

int8_t ipmi_rmcpplus_check_payload_pad(uint8_t confidentiality_algorithm, fiid_obj_t obj_payload);

int8_t ipmi_rmcpplus_check_integrity_pad(fiid_obj_t obj_rmcpplus_session_trlr);
   
int8_t ipmi_rmcpplus_check_rakp_message_2_key_exchange_authentication_code(int8_t authentication_algorithm, uint8_t *authentication_key, uint32_t authentication_key_len, uint32_t remote_console_session_id, uint32_t managed_system_session_id, uint8_t *remote_console_random_number, uint32_t remote_console_random_number_len, uint8_t *managed_system_random_number, uint32_t managed_system_random_number_len, uint8_t *managed_system_guid, uint32_t managed_system_guid_len, uint8_t name_only_lookup, uint8_t requested_maximum_privilege_level, uint8_t *user_name, uint8_t user_name_length, fiid_obj_t obj_msg);

int8_t ipmi_rmcpplus_check_rakp_message_4_integrity_check_value(int8_t authentication_algorithm, uint8_t *sik_key, uint32_t sik_key_len, uint8_t *remote_console_random_number, uint32_t remote_console_random_number_len, uint32_t managed_system_session_id, uint8_t *managed_system_guid, uint32_t managed_system_guid_len, fiid_obj_t obj_msg);

int8_t ipmi_rmcpplus_check_session_trlr(int8_t integrity_algorithm, uint8_t *pkt, uint32_t pkt_len, uint8_t *integrity_key, uint32_t integrity_key_len, uint8_t *authentication_code_data, uint32_t authentication_code_data_len, fiid_obj_t obj_rmcpplus_session_trlr);

int8_t ipmi_rmcpplus_check_payload_type(fiid_obj_t obj_rmcpplus_session_hdr, uint8_t payload_type);

int8_t ipmi_rmcpplus_check_status_code(fiid_obj_t obj_cmd, uint8_t status_code);

int8_t ipmi_rmcpplus_check_message_tag(fiid_obj_t obj_msg, uint8_t message_tag);

int8_t ipmi_rmcpplus_check_remote_console_session_id(fiid_obj_t obj_msg, uint32_t remote_console_session_id);

#ifdef __cplusplus
}
#endif

#endif
