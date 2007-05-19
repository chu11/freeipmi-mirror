/* 
   bmc-config-wrapper.h: BMC Config functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
*/

#ifndef _BMC_CONFIG_WRAPPER_H
#define _BMC_CONFIG_WRAPPER_H

#include "bmc-config.h"
#include "bmc-config-common.h"

bmc_err_t set_bmc_lan_channel_volatile_access (bmc_config_state_data_t *state_data, 
                                               uint8_t access_mode, 
                                               uint8_t user_level_authentication, 
                                               uint8_t per_message_authentication, 
                                               uint8_t pef_alerting, 
                                               uint8_t channel_privilege_limit);
bmc_err_t set_bmc_lan_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
                                                   uint8_t access_mode, 
                                                   uint8_t user_level_authentication, 
                                                   uint8_t per_message_authentication, 
                                                   uint8_t pef_alerting, 
                                                   uint8_t channel_privilege_limit);
bmc_err_t set_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
                                              uint8_t ip_address_source);
bmc_err_t set_bmc_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
                                       char *ip_address);
bmc_err_t set_bmc_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
                                        char *mac_address);
bmc_err_t set_bmc_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
                                        char *subnet_mask);
bmc_err_t set_bmc_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
                                                    char *default_gateway_address);
bmc_err_t set_bmc_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                        char *default_gateway_mac_address);
bmc_err_t set_bmc_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
                                                   char *backup_gateway_address);
bmc_err_t set_bmc_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                       char *backup_gateway_mac_address);
bmc_err_t set_bmc_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
                                    uint32_t vlan_id,
                                    uint8_t vlan_id_enable);
bmc_err_t set_bmc_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
                                          uint8_t vlan_priority);

bmc_err_t set_bmc_lan_conf_authentication_type_enables (bmc_config_state_data_t *state_data, 
                                                        uint8_t callback_level_none,
                                                        uint8_t callback_level_md2,
                                                        uint8_t callback_level_md5,
                                                        uint8_t callback_level_straight_password,
                                                        uint8_t callback_level_oem_proprietary,
                                                        uint8_t user_level_none,
                                                        uint8_t user_level_md2,
                                                        uint8_t user_level_md5,
                                                        uint8_t user_level_straight_password,
                                                        uint8_t user_level_oem_proprietary,
                                                        uint8_t operator_level_none,
                                                        uint8_t operator_level_md2,
                                                        uint8_t operator_level_md5,
                                                        uint8_t operator_level_straight_password,
                                                        uint8_t operator_level_oem_proprietary,
                                                        uint8_t admin_level_none,
                                                        uint8_t admin_level_md2,
                                                        uint8_t admin_level_md5,
                                                        uint8_t admin_level_straight_password,
                                                        uint8_t admin_level_oem_proprietary,
                                                        uint8_t oem_level_none,
                                                        uint8_t oem_level_md2,
                                                        uint8_t oem_level_md5,
                                                        uint8_t oem_level_straight_password,
                                                        uint8_t oem_level_oem_proprietary);

bmc_err_t set_bmc_lan_conf_bmc_generated_arp_control (bmc_config_state_data_t *state_data, 
                                                      uint8_t bmc_generated_gratuitous_arps,
                                                      uint8_t bmc_generated_arp_responses);
bmc_err_t set_bmc_lan_conf_gratuitous_arp_interval (bmc_config_state_data_t *state_data, 
                                                    uint8_t gratuitous_arp_interval);
bmc_err_t set_bmc_serial_channel_volatile_access (bmc_config_state_data_t *state_data, 
                                                  uint8_t access_mode, 
                                                  uint8_t user_level_authentication, 
                                                  uint8_t per_message_authentication, 
                                                  uint8_t pef_alerting, 
                                                  uint8_t channel_privilege_limit);
bmc_err_t set_bmc_serial_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
                                                      uint8_t access_mode, 
                                                      uint8_t user_level_authentication, 
                                                      uint8_t per_message_authentication, 
                                                      uint8_t pef_alerting, 
                                                      uint8_t channel_privilege_limit);
bmc_err_t set_bmc_serial_conf_connection_mode (bmc_config_state_data_t *state_data, 
                                               uint8_t basic_mode, 
                                               uint8_t ppp_mode, 
                                               uint8_t terminal_mode, 
                                               uint8_t connect_mode);
bmc_err_t set_bmc_serial_conf_page_blackout_interval (bmc_config_state_data_t *state_data, 
                                                      uint8_t page_blackout_interval);
bmc_err_t set_bmc_serial_conf_call_retry_interval (bmc_config_state_data_t *state_data, 
                                                   uint8_t call_retry_interval);
bmc_err_t set_bmc_serial_conf_ipmi_messaging_comm_settings (bmc_config_state_data_t *state_data, 
                                                            uint8_t dtr_hangup, 
                                                            uint8_t flow_control, 
                                                            uint8_t bit_rate);
bmc_err_t set_bmc_power_restore_policy (bmc_config_state_data_t *state_data, 
                                        uint8_t power_restore_policy);

bmc_err_t set_sol_sol_enable(bmc_config_state_data_t *state_data,
                             uint8_t sol_enable);
bmc_err_t set_sol_sol_authentication(bmc_config_state_data_t *state_data,
                                     uint8_t sol_privilege_level,
                                     uint8_t force_sol_payload_authentication,
                                     uint8_t force_sol_payload_encryption);
bmc_err_t set_sol_character_accumulate_interval_and_send_threshold(bmc_config_state_data_t *state_data,
                                                                   uint8_t character_accumulate_interval,
                                                                   uint8_t character_send_threshold);
bmc_err_t set_sol_sol_retry(bmc_config_state_data_t *state_data,
                            uint8_t retry_count,
                            uint8_t retry_interval);
bmc_err_t set_sol_sol_non_volatile_bit_rate(bmc_config_state_data_t *state_data,
                                            uint8_t bit_rate);
bmc_err_t set_sol_sol_volatile_bit_rate(bmc_config_state_data_t *state_data,
                                        uint8_t bit_rate);
bmc_err_t set_sol_sol_payload_port_number(bmc_config_state_data_t *state_data,
                                          uint16_t port_number);

bmc_err_t set_rmcpplus_cipher_suite_id_privilege (bmc_config_state_data_t *state_data,
                                                  uint8_t cipher_suite_id,
                                                  uint8_t privilege);

bmc_err_t set_k_r(bmc_config_state_data_t *state_data,
                  uint8_t *k_r,
                  uint32_t k_r_len);

bmc_err_t set_k_g(bmc_config_state_data_t *state_data,
                  uint8_t *k_g,
                  uint32_t k_g_len);

/***********************************************************/
bmc_err_t get_bmc_lan_channel_volatile_access (bmc_config_state_data_t *state_data, 
                                               uint8_t *access_mode, 
                                               uint8_t *user_level_authentication, 
                                               uint8_t *per_message_authentication, 
                                               uint8_t *pef_alerting, 
                                               uint8_t *privilege_limit);
bmc_err_t get_bmc_lan_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
                                                   uint8_t *access_mode, 
                                                   uint8_t *user_level_authentication, 
                                                   uint8_t *per_message_authentication, 
                                                   uint8_t *pef_alerting, 
                                                   uint8_t *privilege_limit);
bmc_err_t get_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
                                              uint8_t *ip_address_source);
bmc_err_t get_bmc_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
                                       char *ip_address,
                                       unsigned int ip_address_len);
bmc_err_t get_bmc_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
                                        char *mac_address,
                                        unsigned int mac_address_len);
bmc_err_t get_bmc_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
                                        char *subnet_mask,
                                        unsigned int subnet_mask_len);
bmc_err_t get_bmc_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
                                                    char *default_gateway_address,
                                                    unsigned int default_gateway_address_len);
bmc_err_t get_bmc_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                        char *default_gateway_mac_address,
                                                        unsigned int default_gateway_mac_address_len);
bmc_err_t get_bmc_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
                                                   char *backup_gateway_address,
                                                   unsigned int backup_gateway_address_len);
bmc_err_t get_bmc_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                       char *backup_gateway_mac_address,
                                                       unsigned int backup_gateway_mac_address_len);
bmc_err_t get_bmc_lan_conf_authentication_type_enables (bmc_config_state_data_t *state_data, 
                                                        uint8_t *callback_level_none,
                                                        uint8_t *callback_level_md2,
                                                        uint8_t *callback_level_md5,
                                                        uint8_t *callback_level_straight_password,
                                                        uint8_t *callback_level_oem_proprietary,
                                                        uint8_t *user_level_none,
                                                        uint8_t *user_level_md2,
                                                        uint8_t *user_level_md5,
                                                        uint8_t *user_level_straight_password,
                                                        uint8_t *user_level_oem_proprietary,
                                                        uint8_t *operator_level_none,
                                                        uint8_t *operator_level_md2,
                                                        uint8_t *operator_level_md5,
                                                        uint8_t *operator_level_straight_password,
                                                        uint8_t *operator_level_oem_proprietary,
                                                        uint8_t *admin_level_none,
                                                        uint8_t *admin_level_md2,
                                                        uint8_t *admin_level_md5,
                                                        uint8_t *admin_level_straight_password,
                                                        uint8_t *admin_level_oem_proprietary,
                                                        uint8_t *oem_level_none,
                                                        uint8_t *oem_level_md2,
                                                        uint8_t *oem_level_md5,
                                                        uint8_t *oem_level_straight_password,
                                                        uint8_t *oem_level_oem_proprietary);
bmc_err_t get_bmc_lan_conf_bmc_generated_arp_control (bmc_config_state_data_t *state_data, 
                                                      uint8_t *gratuitous_arps, 
                                                      uint8_t *arp_response);
bmc_err_t get_bmc_lan_conf_gratuitous_arp_interval (bmc_config_state_data_t *state_data, 
                                                    uint8_t *gratuitous_arp_interval);

bmc_err_t get_bmc_serial_channel_volatile_access (bmc_config_state_data_t *state_data, 
                                                  uint8_t *access_mode, 
                                                  uint8_t *user_level_authentication, 
                                                  uint8_t *per_message_authentication, 
                                                  uint8_t *pef_alerting, 
                                                  uint8_t *privilege_limit);
bmc_err_t get_bmc_serial_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
                                                      uint8_t *access_mode, 
                                                      uint8_t *user_level_authentication, 
                                                      uint8_t *per_message_authentication, 
                                                      uint8_t *pef_alerting, 
                                                      uint8_t *privilege_limit);
bmc_err_t get_bmc_serial_conf_connection_mode (bmc_config_state_data_t *state_data, 
                                               uint8_t *basic_mode, 
                                               uint8_t *ppp_mode, 
                                               uint8_t *terminal_mode, 
                                               uint8_t *connect_mode);
bmc_err_t get_bmc_serial_conf_page_blackout_interval (bmc_config_state_data_t *state_data, 
                                                      uint8_t *page_blackout_interval);
bmc_err_t get_bmc_serial_conf_call_retry_interval (bmc_config_state_data_t *state_data, 
                                                   uint8_t *call_retry_interval);
bmc_err_t get_bmc_serial_conf_ipmi_messaging_comm_settings (bmc_config_state_data_t *state_data, 
                                                            uint8_t *dtr_hangup, 
                                                            uint8_t *flow_control, 
                                                            uint8_t *bit_rate);
bmc_err_t get_bmc_power_restore_policy (bmc_config_state_data_t *state_data, 
                                        uint8_t *power_restore_policy);
bmc_err_t get_bmc_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
                                    uint32_t *vlan_id,
                                    uint8_t *vlan_id_enable);
bmc_err_t get_bmc_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
                                          uint8_t *vlan_priority);
bmc_err_t get_sol_sol_enable (bmc_config_state_data_t *state_data,
                              uint8_t *sol_enable);
bmc_err_t get_sol_sol_authentication (bmc_config_state_data_t *state_data,
                                      uint8_t *sol_privilege_level,
                                      uint8_t *force_sol_payload_authentication,
                                      uint8_t *force_sol_payload_encryption);
bmc_err_t get_sol_character_accumulate_interval_and_send_threshold (bmc_config_state_data_t *state_data,
                                                                    uint8_t *character_accumulate_interval,
                                                                    uint8_t *character_send_threshold);
bmc_err_t get_sol_sol_retry (bmc_config_state_data_t *state_data,
                             uint8_t *retry_count,
                             uint8_t *retry_interval);
bmc_err_t get_sol_sol_non_volatile_bit_rate (bmc_config_state_data_t *state_data,
                                             uint8_t *bit_rate);
bmc_err_t get_sol_sol_volatile_bit_rate (bmc_config_state_data_t *state_data,
                                         uint8_t *bit_rate);
bmc_err_t get_sol_sol_payload_port_number (bmc_config_state_data_t *state_data,
                                           uint16_t *port_number);

bmc_err_t get_rmcpplus_cipher_suite_id_privilege (bmc_config_state_data_t *state_data,
                                                  uint8_t cipher_suite_id,
                                                  uint8_t *privilege);

bmc_err_t get_k_r(bmc_config_state_data_t *state_data,
                  uint8_t *k_r,
                  uint32_t k_r_len);

bmc_err_t get_k_g(bmc_config_state_data_t *state_data,
                  uint8_t *k_g,
                  uint32_t k_g_len);
#endif
