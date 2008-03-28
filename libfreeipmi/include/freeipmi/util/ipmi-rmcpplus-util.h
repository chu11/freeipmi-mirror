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

#ifndef _IPMI_RMCPPLUS_UTIL_H
#define _IPMI_RMCPPLUS_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

int32_t ipmi_calculate_sik(uint8_t authentication_algorithm, 
			   uint8_t *k_g, 
			   uint32_t k_g_len, 
			   uint8_t *remote_console_random_number, 
			   uint32_t remote_console_random_number_len, 
			   uint8_t *managed_system_random_number, 
			   uint32_t managed_system_random_number_len, 
                           uint8_t name_only_lookup,
			   uint8_t requested_privilege_level, 
			   char *user_name,
			   uint8_t user_name_len, 
			   uint8_t *sik, 
			   uint32_t sik_len);

int32_t ipmi_calculate_k1(uint8_t authentication_algorithm, 
			  uint8_t *sik_key, 
			  uint32_t sik_key_len, 
			  uint8_t *k1, 
			  uint32_t k1_len);

int32_t ipmi_calculate_k2(uint8_t authentication_algorithm, 
			  uint8_t *sik_key, 
			  uint32_t sik_key_len, 
			  uint8_t *k2, 
			  uint32_t k2_len);

int32_t ipmi_calculate_rmcpplus_session_keys(uint8_t authentication_algorithm,
                                             uint8_t integrity_algorithm,
                                             uint8_t confidentiality_algorithm,
                                             uint8_t *authentication_code_data,
                                             uint32_t authentication_code_data_len,
					     uint8_t *k_g,
					     uint32_t k_g_len,
                                             uint8_t *remote_console_random_number, 
                                             uint32_t remote_console_random_number_len, 
                                             uint8_t *managed_system_random_number,
                                             uint32_t managed_system_random_number_len,
                                             uint8_t name_only_lookup,
                                             uint8_t requested_privilege_level,
                                             char *user_name, 
                                             uint8_t user_name_len,
                                             uint8_t **sik_key,
                                             uint32_t *sik_key_len,
                                             uint8_t **integrity_key,
                                             uint32_t *integrity_key_len,
                                             uint8_t **confidentiality_key,
                                             uint32_t *confidentiality_key_len);

int32_t ipmi_calculate_rakp_3_key_exchange_authentication_code(int8_t authentication_algorithm, 
							       uint8_t *k_uid, 
							       uint32_t k_uid_len, 
							       uint8_t *managed_system_random_number, 
							       uint32_t managed_system_random_number_len, 
							       uint32_t remote_console_session_id, 
							       uint8_t name_only_lookup, 
							       uint8_t requested_privilege_level,
							       char *user_name, 
							       uint8_t user_name_length,
							       uint8_t *key_exchange_authentication_code, 
							       uint32_t key_exchange_authentication_code_len);

int8_t ipmi_rmcpplus_check_payload_pad(uint8_t confidentiality_algorithm, 
				       fiid_obj_t obj_rmcpplus_payload);

int8_t ipmi_rmcpplus_check_integrity_pad(fiid_obj_t obj_rmcpplus_session_trlr);
   
int8_t ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code(int8_t authentication_algorithm, 
                                                                   uint8_t *k_uid, 
                                                                   uint32_t k_uid_len, 
                                                                   uint32_t remote_console_session_id, 
                                                                   uint32_t managed_system_session_id, 
                                                                   uint8_t *remote_console_random_number, 
                                                                   uint32_t remote_console_random_number_len, 
                                                                   uint8_t *managed_system_random_number, 
                                                                   uint32_t managed_system_random_number_len, 
                                                                   uint8_t *managed_system_guid, 
                                                                   uint32_t managed_system_guid_len, 
                                                                   uint8_t name_only_lookup, 
                                                                   uint8_t requested_privilege_level,
                                                                   char *user_name, 
                                                                   uint8_t user_name_length, 
                                                                   fiid_obj_t obj_cmd);

int8_t ipmi_rmcpplus_check_rakp_4_integrity_check_value(int8_t authentication_algorithm, 
                                                        uint8_t *sik_key, 
                                                        uint32_t sik_key_len, 
                                                        uint8_t *remote_console_random_number, 
                                                        uint32_t remote_console_random_number_len, 
                                                        uint32_t managed_system_session_id, 
                                                        uint8_t *managed_system_guid, 
                                                        uint32_t managed_system_guid_len, 
                                                        fiid_obj_t obj_cmd);
  
int8_t ipmi_rmcpplus_check_packet_session_authentication_code(int8_t integrity_algorithm, 
							      uint8_t *pkt, 
							      uint32_t pkt_len, 
							      uint8_t *integrity_key, 
							      uint32_t integrity_key_len, 
							      uint8_t *authentication_code_data, 
							      uint32_t authentication_code_data_len, 
							      fiid_obj_t obj_rmcpplus_session_trlr);

int8_t ipmi_rmcpplus_check_payload_type(fiid_obj_t obj_rmcpplus_session_hdr, 
					uint8_t payload_type);

int8_t ipmi_rmcpplus_check_status_code(fiid_obj_t obj_cmd, 
				       uint8_t status_code);

int8_t ipmi_rmcpplus_check_message_tag(fiid_obj_t obj_cmd, 
				       uint8_t message_tag);

int8_t ipmi_rmcpplus_check_remote_console_session_id(fiid_obj_t obj_cmd, 
						     uint32_t remote_console_session_id);

int8_t ipmi_rmcpplus_check_session_id(fiid_obj_t obj_rmcpplus_session_hdr,
                                      uint32_t session_id);

int8_t ipmi_rmcpplus_calculate_payload_type(uint8_t *pkt, uint32_t pkt_len);

#ifdef __cplusplus
}
#endif

#endif
