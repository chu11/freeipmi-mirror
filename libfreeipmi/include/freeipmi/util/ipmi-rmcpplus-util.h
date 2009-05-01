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

#ifndef _IPMI_RMCPPLUS_UTIL_H
#define _IPMI_RMCPPLUS_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

int ipmi_calculate_sik (uint8_t authentication_algorithm,
                        const uint8_t *k_g,
                        unsigned int k_g_len,
                        const uint8_t *remote_console_random_number,
                        unsigned int remote_console_random_number_len,
                        const uint8_t *managed_system_random_number,
                        unsigned int managed_system_random_number_len,
                        uint8_t name_only_lookup,
                        uint8_t requested_privilege_level,
                        const char *user_name,
                        unsigned int user_name_len,
                        uint8_t *sik,
                        unsigned int sik_len);

int ipmi_calculate_k1 (uint8_t authentication_algorithm,
                       const uint8_t *sik_key,
                       unsigned int sik_key_len,
                       uint8_t *k1,
                       unsigned int k1_len);

int ipmi_calculate_k2 (uint8_t authentication_algorithm,
                       const uint8_t *sik_key,
                       unsigned int sik_key_len,
                       uint8_t *k2,
                       unsigned int k2_len);

int ipmi_calculate_rmcpplus_session_keys (uint8_t authentication_algorithm,
                                          uint8_t integrity_algorithm,
                                          uint8_t confidentiality_algorithm,
                                          const uint8_t *authentication_code_data,
                                          unsigned int authentication_code_data_len,
                                          const uint8_t *k_g,
                                          unsigned int k_g_len,
                                          const uint8_t *remote_console_random_number,
                                          unsigned int remote_console_random_number_len,
                                          const uint8_t *managed_system_random_number,
                                          unsigned int managed_system_random_number_len,
                                          uint8_t name_only_lookup,
                                          uint8_t requested_privilege_level,
                                          const char *user_name,
                                          uint8_t user_name_len,
                                          uint8_t **sik_key,
                                          unsigned int *sik_key_len,
                                          uint8_t **integrity_key,
                                          unsigned int *integrity_key_len,
                                          uint8_t **confidentiality_key,
                                          unsigned int *confidentiality_key_len);

int ipmi_calculate_rakp_3_key_exchange_authentication_code (uint8_t authentication_algorithm,
                                                            const uint8_t *k_uid,
                                                            unsigned int k_uid_len,
                                                            const uint8_t *managed_system_random_number,
                                                            unsigned int managed_system_random_number_len,
                                                            uint32_t remote_console_session_id,
                                                            uint8_t name_only_lookup,
                                                            uint8_t requested_privilege_level,
                                                            const char *user_name,
                                                            uint8_t user_name_length,
                                                            uint8_t *key_exchange_authentication_code,
                                                            unsigned int key_exchange_authentication_code_len);

int ipmi_rmcpplus_check_payload_pad (uint8_t confidentiality_algorithm,
                                        fiid_obj_t obj_rmcpplus_payload);

int ipmi_rmcpplus_check_integrity_pad (fiid_obj_t obj_rmcpplus_session_trlr);

int ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code (uint8_t authentication_algorithm,
                                                                 const uint8_t *k_uid,
                                                                 unsigned int k_uid_len,
                                                                 uint32_t remote_console_session_id,
                                                                 uint32_t managed_system_session_id,
                                                                 const uint8_t *remote_console_random_number,
                                                                 unsigned int remote_console_random_number_len,
                                                                 const uint8_t *managed_system_random_number,
                                                                 unsigned int managed_system_random_number_len,
                                                                 const uint8_t *managed_system_guid,
                                                                 unsigned int managed_system_guid_len,
                                                                 uint8_t name_only_lookup,
                                                                 uint8_t requested_privilege_level,
                                                                 const char *user_name,
                                                                 uint8_t user_name_length,
                                                                 fiid_obj_t obj_cmd);

int ipmi_rmcpplus_check_rakp_4_integrity_check_value (uint8_t authentication_algorithm,
                                                      const uint8_t *sik_key,
                                                      unsigned int sik_key_len,
                                                      const uint8_t *remote_console_random_number,
                                                      unsigned int remote_console_random_number_len,
                                                      uint32_t managed_system_session_id,
                                                      const uint8_t *managed_system_guid,
                                                      unsigned int managed_system_guid_len,
                                                      fiid_obj_t obj_cmd);

int ipmi_rmcpplus_check_packet_session_authentication_code (uint8_t integrity_algorithm,
                                                            const uint8_t *pkt,
                                                            unsigned int pkt_len,
                                                            const uint8_t *integrity_key,
                                                            unsigned int integrity_key_len,
                                                            const uint8_t *authentication_code_data,
                                                            unsigned int authentication_code_data_len,
                                                            fiid_obj_t obj_rmcpplus_session_trlr);

int ipmi_rmcpplus_check_payload_type (fiid_obj_t obj_rmcpplus_session_hdr,
                                      uint8_t payload_type);

int ipmi_rmcpplus_check_status_code (fiid_obj_t obj_cmd,
                                     uint8_t status_code);

int ipmi_rmcpplus_check_message_tag (fiid_obj_t obj_cmd,
                                     uint8_t message_tag);

int ipmi_rmcpplus_check_remote_console_session_id (fiid_obj_t obj_cmd,
                                                   uint32_t remote_console_session_id);

int ipmi_rmcpplus_check_session_id (fiid_obj_t obj_rmcpplus_session_hdr,
                                    uint32_t session_id);

int ipmi_rmcpplus_calculate_payload_type (const uint8_t *pkt,
                                          unsigned int pkt_len,
                                          uint8_t *payload_type);

#ifdef __cplusplus
}
#endif

#endif
