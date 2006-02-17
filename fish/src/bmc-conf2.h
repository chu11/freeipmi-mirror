/* 
   bmc-conf2.h: BMC Config functions
   
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

#ifndef _BMC_CONF2_H
#define _BMC_CONF2_H

struct authentication_type
{
  uint8_t type_none;
  uint8_t type_md2;
  uint8_t type_md5;
  uint8_t type_straight_password;
  uint8_t type_oem_proprietary;
};

struct bmc_authentication_level
{
  struct authentication_type callback;
  struct authentication_type user;
  struct authentication_type operator;
  struct authentication_type admin;
  struct authentication_type oem;
};

int8_t set_bmc_username (ipmi_device_t *dev, 
			 uint8_t userid, 
			 uint8_t *username);
int8_t set_bmc_enable_user (ipmi_device_t *dev, 
			    uint8_t userid, 
			    int user_status);
int8_t set_bmc_user_password (ipmi_device_t *dev, 
			      uint8_t userid, 
			      uint8_t *password);
int8_t set_bmc_user_lan_channel_access (ipmi_device_t *dev, 
					uint8_t userid, 
					uint8_t lan_user_ipmi_messaging, 
					uint8_t lan_user_link_authentication, 
					uint8_t lan_user_restricted_to_callback, 
					uint8_t lan_privilege_limit, 
					uint8_t lan_session_limit);
int8_t set_bmc_user_serial_channel_access (ipmi_device_t *dev, 
					   uint8_t userid, 
					   uint8_t serial_user_ipmi_messaging, 
					   uint8_t serial_user_link_authentication, 
					   uint8_t serial_user_restricted_to_callback, 
					   uint8_t serial_privilege_limit, 
					   uint8_t serial_session_limit);
int8_t set_bmc_lan_channel_volatile_access (ipmi_device_t *dev, 
					    uint8_t access_mode, 
					    uint8_t user_level_authentication, 
					    uint8_t per_message_authentication, 
					    uint8_t pef_alerting, 
					    uint8_t channel_privilege_limit);
int8_t set_bmc_lan_channel_non_volatile_access (ipmi_device_t *dev, 
						uint8_t access_mode, 
						uint8_t user_level_authentication, 
						uint8_t per_message_authentication, 
						uint8_t pef_alerting, 
						uint8_t channel_privilege_limit);
int8_t set_bmc_lan_conf_ip_address_source (ipmi_device_t *dev, 
                                           uint8_t ip_address_source);
int8_t set_bmc_lan_conf_ip_address (ipmi_device_t *dev, 
                                    char *ip_address);
int8_t set_bmc_lan_conf_mac_address (ipmi_device_t *dev, 
                                     char *mac_address);
int8_t set_bmc_lan_conf_subnet_mask (ipmi_device_t *dev, 
				     char *subnet_mask);
int8_t set_bmc_lan_conf_default_gateway_address (ipmi_device_t *dev, 
                                                 char *default_gateway_address);
int8_t set_bmc_lan_conf_default_gateway_mac_address (ipmi_device_t *dev, 
                                                     char *default_gateway_mac_address);
int8_t set_bmc_lan_conf_backup_gateway_address (ipmi_device_t *dev, 
                                                char *backup_gateway_address);
int8_t set_bmc_lan_conf_backup_gateway_mac_address (ipmi_device_t *dev, 
                                                    char *backup_gateway_mac_address);
int8_t set_bmc_lan_conf_vlan_id (ipmi_device_t *dev, 
				 uint32_t vlan_id,
                                 uint8_t vlan_id_enable);
int8_t set_bmc_lan_conf_vlan_priority (ipmi_device_t *dev, 
				       uint8_t vlan_priority);

int8_t set_bmc_lan_conf_authentication_type_enables (ipmi_device_t *dev, 
                                                     struct bmc_authentication_level *bmc_authentication_level);
int8_t set_bmc_lan_conf_bmc_generated_arp_control (ipmi_device_t *dev, 
                                                   uint8_t bmc_generated_gratuitous_arps,
                                                   uint8_t bmc_generated_arp_responses);
int8_t set_bmc_lan_conf_gratuitous_arp_interval (ipmi_device_t *dev, 
                                                 uint8_t gratuitous_arp_interval);
int8_t set_bmc_serial_channel_volatile_access (ipmi_device_t *dev, 
					       uint8_t access_mode, 
					       uint8_t user_level_authentication, 
					       uint8_t per_message_authentication, 
					       uint8_t pef_alerting, 
					       uint8_t channel_privilege_limit);
int8_t set_bmc_serial_channel_non_volatile_access (ipmi_device_t *dev, 
						   uint8_t access_mode, 
						   uint8_t user_level_authentication, 
						   uint8_t per_message_authentication, 
						   uint8_t pef_alerting, 
						   uint8_t channel_privilege_limit);
int8_t set_bmc_serial_conf_connection_mode (ipmi_device_t *dev, 
                                            uint8_t basic_mode, 
                                            uint8_t ppp_mode, 
                                            uint8_t terminal_mode, 
                                            uint8_t connect_mode);
int8_t set_bmc_serial_conf_page_blackout_interval (ipmi_device_t *dev, 
						   uint8_t page_blackout_interval);
int8_t set_bmc_serial_conf_call_retry_interval (ipmi_device_t *dev, 
                                                uint8_t call_retry_interval);
int8_t set_bmc_serial_conf_ipmi_messaging_comm_settings (ipmi_device_t *dev, 
                                                         uint8_t dtr_hangup, 
                                                         uint8_t flow_control, 
                                                         uint8_t bit_rate);
int8_t set_pef_control (ipmi_device_t *dev, 
			uint8_t pef, 
			uint8_t pef_event_messages, 
			uint8_t pef_startup_delay, 
			uint8_t pef_alert_startup_delay);
int8_t set_pef_action_global_control (ipmi_device_t *dev, 
				      uint8_t alert_action, 
				      uint8_t power_down_action, 
				      uint8_t reset_action, 
				      uint8_t power_cycle_action, 
				      uint8_t oem_action, 
				      uint8_t diagnostic_interrupt);
int8_t set_pef_startup_delay (ipmi_device_t *dev, 
			      uint8_t pef_startup_delay);
int8_t set_pef_alert_startup_delay (ipmi_device_t *dev, 
				    uint8_t pef_alert_startup_delay);
int8_t set_bmc_power_restore_policy (ipmi_device_t *dev, 
				     uint8_t power_restore_policy);
/***********************************************************/
int8_t get_bmc_username (ipmi_device_t *dev, 
			 uint8_t userid, 
			 uint8_t *username,
			 uint32_t username_len);
int8_t get_bmc_user_lan_channel_access (ipmi_device_t *dev, 
					uint8_t userid, 
					uint8_t *user_ipmi_messaging, 
					uint8_t *user_link_authentication, 
					uint8_t *user_restricted_to_callback, 
					uint8_t *privilege_limit, 
					uint8_t *session_limit);
int8_t get_bmc_user_serial_channel_access (ipmi_device_t *dev, 
					   uint8_t userid, 
					   uint8_t *user_ipmi_messaging, 
					   uint8_t *user_link_authentication, 
					   uint8_t *user_restricted_to_callback, 
					   uint8_t *privilege_limit, 
					   uint8_t *session_limit);
int8_t get_bmc_lan_channel_volatile_access (ipmi_device_t *dev, 
					    uint8_t *access_mode, 
					    uint8_t *user_level_authentication, 
					    uint8_t *per_message_authentication, 
					    uint8_t *pef_alerting, 
					    uint8_t *privilege_limit);
int8_t get_bmc_lan_channel_non_volatile_access (ipmi_device_t *dev, 
						uint8_t *access_mode, 
						uint8_t *user_level_authentication, 
						uint8_t *per_message_authentication, 
						uint8_t *pef_alerting, 
						uint8_t *privilege_limit);
int8_t get_bmc_lan_conf_ip_address_source (ipmi_device_t *dev, 
                                           uint8_t *ip_address_source);
int8_t get_bmc_lan_conf_ip_address (ipmi_device_t *dev, 
                                    char *ip_address);
int8_t get_bmc_lan_conf_mac_address (ipmi_device_t *dev, 
                                     char *mac_address);
int8_t get_bmc_lan_conf_subnet_mask (ipmi_device_t *dev, 
				     char *subnet_mask);
int8_t get_bmc_lan_conf_default_gateway_address (ipmi_device_t *dev, 
                                                 char *default_gateway_address);
int8_t get_bmc_lan_conf_default_gateway_mac_address (ipmi_device_t *dev, 
                                                     char *default_gateway_mac_address);
int8_t get_bmc_lan_conf_backup_gateway_address (ipmi_device_t *dev, 
                                                char *backup_gateway_address);
int8_t get_bmc_lan_conf_backup_gateway_mac_address (ipmi_device_t *dev, 
                                                    char *backup_gateway_mac_address);
int8_t get_bmc_lan_conf_authentication_type_enables (ipmi_device_t *dev, 
                                                     struct bmc_authentication_level *bmc_authentication_level);
int8_t get_bmc_lan_conf_bmc_generated_arp_control (ipmi_device_t *dev, 
                                                   uint8_t *gratuitous_arps, 
                                                   uint8_t *arp_response);
int8_t get_bmc_lan_conf_gratuitous_arp_interval (ipmi_device_t *dev, 
                                                 uint8_t *gratuitous_arp_interval);

int8_t get_bmc_serial_channel_volatile_access (ipmi_device_t *dev, 
					       uint8_t *access_mode, 
					       uint8_t *user_level_authentication, 
					       uint8_t *per_message_authentication, 
					       uint8_t *pef_alerting, 
					       uint8_t *privilege_limit);
int8_t get_bmc_serial_channel_non_volatile_access (ipmi_device_t *dev, 
						   uint8_t *access_mode, 
						   uint8_t *user_level_authentication, 
						   uint8_t *per_message_authentication, 
						   uint8_t *pef_alerting, 
						   uint8_t *privilege_limit);
int8_t get_bmc_serial_conf_connection_mode (ipmi_device_t *dev, 
                                            uint8_t *basic_mode, 
                                            uint8_t *ppp_mode, 
                                            uint8_t *terminal_mode, 
                                            uint8_t *connect_mode);
int8_t get_bmc_serial_conf_page_blackout_interval (ipmi_device_t *dev, 
						   uint8_t *page_blackout_interval);
int8_t get_bmc_serial_conf_call_retry_interval (ipmi_device_t *dev, 
                                                uint8_t *call_retry_interval);
int8_t get_bmc_serial_conf_ipmi_messaging_comm_settings (ipmi_device_t *dev, 
                                                         uint8_t *dtr_hangup, 
                                                         uint8_t *flow_control, 
                                                         uint8_t *bit_rate);
int8_t get_bmc_power_restore_policy (ipmi_device_t *dev, 
				     uint8_t *power_restore_policy);
int8_t get_bmc_lan_conf_vlan_id (ipmi_device_t *dev, 
                                 uint32_t *vlan_id,
				 uint8_t *vlan_id_enable);
int8_t get_bmc_lan_conf_vlan_priority (ipmi_device_t *dev, 
				       uint8_t *vlan_priority);
int8_t get_pef_control (ipmi_device_t *dev, 
			uint8_t *pef, 
			uint8_t *pef_event_messages, 
			uint8_t *pef_startup_delay, 
			uint8_t *pef_alert_startup_delay);
int8_t get_pef_action_global_control (ipmi_device_t *dev, 
				      uint8_t *alert_action, 
				      uint8_t *power_down_action, 
				      uint8_t *reset_action, 
				      uint8_t *power_cycle_action, 
				      uint8_t *oem_action, 
				      uint8_t *diagnostic_interrupt);
int8_t get_pef_startup_delay (ipmi_device_t *dev, 
			      uint8_t *pef_startup_delay);
int8_t get_pef_alert_startup_delay (ipmi_device_t *dev, 
				    uint8_t *pef_alert_startup_delay);
/***********************************************************/
int8_t check_bmc_user_password (ipmi_device_t *dev, 
				uint8_t userid, 
				uint8_t *password);
#endif
