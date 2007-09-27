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

config_err_t check_bmc_user_password (bmc_config_state_data_t *state_data, 
                                      uint8_t userid, 
                                      uint8_t *password,
                                      int *is_same);
config_err_t check_bmc_user_password20 (bmc_config_state_data_t *state_data, 
                                        uint8_t userid, 
                                        uint8_t *password,
                                        int *is_same);
#endif
