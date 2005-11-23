/*
   ipmi-lanplus-sessions.h - IPMI LAN Commands

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

int8_t ipmi_init_gcrypt(void);

int32_t ipmi_gcrypt_hash(int gcry_md_algorithm, unsigned int gcry_md_flags, unsigned int expected_digest_len, u_int8_t *key, u_int32_t key_len, u_int8_t *hash_data, u_int32_t hash_data_len, u_int8_t *digest, u_int32_t digest_len);

int32_t ipmi_calculate_sik(u_int8_t authentication_algorithm, u_int8_t *key, u_int32_t key_len, u_int8_t *remote_console_random_number, u_int32_t remote_console_random_number_len, u_int8_t *managed_system_random_number, u_int32_t managed_system_random_number_len, u_int8_t requested_privilege_level, u_int8_t *username, u_int8_t username_len, u_int8_t *sik, u_int32_t sik_len);

int32_t ipmi_calculate_k1(u_int8_t authentication_algorithm, u_int8_t *sik_key, u_int32_t sik_key_len, u_int8_t *k1, u_int32_t k1_len);

int32_t ipmi_calculate_k2(u_int8_t authentication_algorithm, u_int8_t *sik_key, u_int32_t sik_key_len, u_int8_t *k2, u_int32_t k2_len);

#ifdef __cplusplus
}
#endif

#endif
