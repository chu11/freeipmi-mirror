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

config_err_t set_bmc_username (bmc_config_state_data_t *state_data, 
                               uint8_t userid, 
                               uint8_t *username);
config_err_t set_bmc_enable_user (bmc_config_state_data_t *state_data, 
                                  uint8_t userid, 
                                  int user_status);
config_err_t set_bmc_user_password (bmc_config_state_data_t *state_data, 
                                    uint8_t userid, 
                                    uint8_t *password);
config_err_t set_bmc_user_password20 (bmc_config_state_data_t *state_data, 
                                      uint8_t userid, 
                                      uint8_t *password);
config_err_t set_bmc_user_lan_channel_access (bmc_config_state_data_t *state_data, 
                                              uint8_t userid, 
                                              uint8_t lan_user_ipmi_messaging, 
                                              uint8_t lan_user_link_authentication, 
                                              uint8_t lan_user_restricted_to_callback, 
                                              uint8_t lan_privilege_limit, 
                                              uint8_t lan_session_limit);
config_err_t set_bmc_user_payload_access (bmc_config_state_data_t *state_data,
                                          uint8_t userid,
                                          uint8_t operation,
                                          uint8_t standard_payload_1,
                                          uint8_t standard_payload_2,
                                          uint8_t standard_payload_3,
                                          uint8_t standard_payload_4,
                                          uint8_t standard_payload_5,
                                          uint8_t standard_payload_6,
                                          uint8_t standard_payload_7,
                                          uint8_t oem_payload_0,
                                          uint8_t oem_payload_1,
                                          uint8_t oem_payload_2,
                                          uint8_t oem_payload_3,
                                          uint8_t oem_payload_4,
                                          uint8_t oem_payload_5,
                                          uint8_t oem_payload_6,
                                          uint8_t oem_payload_7);
config_err_t set_bmc_user_serial_channel_access (bmc_config_state_data_t *state_data, 
                                                 uint8_t userid, 
                                                 uint8_t serial_user_ipmi_messaging, 
                                                 uint8_t serial_user_link_authentication, 
                                                 uint8_t serial_user_restricted_to_callback, 
                                                 uint8_t serial_privilege_limit, 
                                                 uint8_t serial_session_limit);
config_err_t set_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
                                                 uint8_t ip_address_source);
config_err_t set_bmc_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
                                          char *ip_address);
config_err_t set_bmc_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
                                           char *mac_address);
config_err_t set_bmc_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
                                           char *subnet_mask);
config_err_t set_bmc_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
                                                       char *default_gateway_address);
config_err_t set_bmc_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                           char *default_gateway_mac_address);
config_err_t set_bmc_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
                                                      char *backup_gateway_address);
config_err_t set_bmc_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                          char *backup_gateway_mac_address);
config_err_t set_bmc_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
                                       uint32_t vlan_id,
                                       uint8_t vlan_id_enable);
config_err_t set_bmc_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
                                             uint8_t vlan_priority);
config_err_t set_pef_control (bmc_config_state_data_t *state_data,
                              uint8_t pef,
                              uint8_t pef_event_messages,
                              uint8_t pef_startup_delay,
                              uint8_t pef_alert_startup_delay);
config_err_t set_pef_action_global_control (bmc_config_state_data_t *state_data,
                                            uint8_t alert_action,
                                            uint8_t power_down_action,
                                            uint8_t reset_action,
                                            uint8_t power_cycle_action,
                                            uint8_t oem_action,
                                            uint8_t diagnostic_interrupt);
config_err_t set_pef_startup_delay (bmc_config_state_data_t *state_data,
                                    uint8_t pef_startup_delay);
config_err_t set_pef_alert_startup_delay (bmc_config_state_data_t *state_data,
                                          uint8_t pef_alert_startup_delay);

/***********************************************************/
config_err_t get_bmc_username (bmc_config_state_data_t *state_data, 
                               uint8_t userid, 
                               uint8_t *username,
                               uint32_t username_len);
config_err_t get_bmc_user_lan_channel_access (bmc_config_state_data_t *state_data, 
                                              uint8_t userid, 
                                              uint8_t *user_ipmi_messaging, 
                                              uint8_t *user_link_authentication, 
                                              uint8_t *user_restricted_to_callback, 
                                              uint8_t *privilege_limit, 
                                              uint8_t *session_limit,
                                              uint8_t *user_id_enable_status);
config_err_t get_bmc_user_payload_access (bmc_config_state_data_t *state_data,
                                          uint8_t userid,
                                          uint8_t *standard_payload_1,
                                          uint8_t *standard_payload_2,
                                          uint8_t *standard_payload_3,
                                          uint8_t *standard_payload_4,
                                          uint8_t *standard_payload_5,
                                          uint8_t *standard_payload_6,
                                          uint8_t *standard_payload_7,
                                          uint8_t *oem_payload_0,
                                          uint8_t *oem_payload_1,
                                          uint8_t *oem_payload_2,
                                          uint8_t *oem_payload_3,
                                          uint8_t *oem_payload_4,
                                          uint8_t *oem_payload_5,
                                          uint8_t *oem_payload_6,
                                          uint8_t *oem_payload_7);
config_err_t get_bmc_user_serial_channel_access (bmc_config_state_data_t *state_data, 
                                                 uint8_t userid, 
                                                 uint8_t *user_ipmi_messaging, 
                                                 uint8_t *user_link_authentication, 
                                                 uint8_t *user_restricted_to_callback, 
                                                 uint8_t *privilege_limit, 
                                                 uint8_t *session_limit,
                                                 uint8_t *user_id_enable_status);
config_err_t get_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
                                                 uint8_t *ip_address_source);
config_err_t get_bmc_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
                                          char *ip_address,
                                          unsigned int ip_address_len);
config_err_t get_bmc_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
                                           char *mac_address,
                                           unsigned int mac_address_len);
config_err_t get_bmc_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
                                           char *subnet_mask,
                                           unsigned int subnet_mask_len);
config_err_t get_bmc_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
                                                       char *default_gateway_address,
                                                       unsigned int default_gateway_address_len);
config_err_t get_bmc_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                           char *default_gateway_mac_address,
                                                           unsigned int default_gateway_mac_address_len);
config_err_t get_bmc_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
                                                      char *backup_gateway_address,
                                                      unsigned int backup_gateway_address_len);
config_err_t get_bmc_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                                          char *backup_gateway_mac_address,
                                                          unsigned int backup_gateway_mac_address_len);
config_err_t get_bmc_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
                                       uint32_t *vlan_id,
                                       uint8_t *vlan_id_enable);
config_err_t get_bmc_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
                                             uint8_t *vlan_priority);
config_err_t get_pef_control (bmc_config_state_data_t *state_data,
                              uint8_t *pef,
                              uint8_t *pef_event_messages,
                              uint8_t *pef_startup_delay,
                              uint8_t *pef_alert_startup_delay);
config_err_t get_pef_action_global_control (bmc_config_state_data_t *state_data,
                                            uint8_t *alert_action,
                                            uint8_t *power_down_action,
                                            uint8_t *reset_action,
                                            uint8_t *power_cycle_action,
                                            uint8_t *oem_action,
                                            uint8_t *diagnostic_interrupt);
config_err_t get_pef_startup_delay (bmc_config_state_data_t *state_data,
                                    uint8_t *pef_startup_delay);
config_err_t get_pef_alert_startup_delay (bmc_config_state_data_t *state_data,
                                          uint8_t *pef_alert_startup_delay);

/***********************************************************/
config_err_t check_bmc_user_password (bmc_config_state_data_t *state_data, 
                                      uint8_t userid, 
                                      uint8_t *password,
                                      int *is_same);
config_err_t check_bmc_user_password20 (bmc_config_state_data_t *state_data, 
                                        uint8_t userid, 
                                        uint8_t *password,
                                        int *is_same);
#endif
