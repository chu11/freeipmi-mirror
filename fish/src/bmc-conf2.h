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
					    uint8_t enable_user_level_authentication, 
					    uint8_t enable_per_message_authentication, 
					    uint8_t enable_pef_alerting, 
					    uint8_t channel_privilege_limit);
int8_t set_bmc_lan_channel_non_volatile_access (ipmi_device_t *dev, 
						uint8_t access_mode, 
						uint8_t enable_user_level_authentication, 
						uint8_t enable_per_message_authentication, 
						uint8_t enable_pef_alerting, 
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
				 uint8_t vlan_id_flag,
				 uint32_t vlan_id);
int8_t set_bmc_lan_conf_vlan_priority (ipmi_device_t *dev, 
				       uint8_t vlan_priority);

int8_t set_bmc_lan_conf_authentication_type_enables (ipmi_device_t *dev, 
                                                     struct bmc_authentication_level *bmc_authentication_level);
int8_t set_bmc_lan_conf_bmc_generated_arp_control (ipmi_device_t *dev, 
                                                   uint8_t enable_gratuitous_arps, 
                                                   uint8_t enable_arp_response);
int8_t set_bmc_lan_conf_gratuitous_arp_interval (ipmi_device_t *dev, 
                                                 uint8_t gratuitous_arp_interval);
int8_t set_bmc_serial_channel_volatile_access (ipmi_device_t *dev, 
					       uint8_t access_mode, 
					       uint8_t enable_user_level_authentication, 
					       uint8_t enable_per_message_authentication, 
					       uint8_t enable_pef_alerting, 
					       uint8_t channel_privilege_limit);
int8_t set_bmc_serial_channel_non_volatile_access (ipmi_device_t *dev, 
						   uint8_t access_mode, 
						   uint8_t enable_user_level_authentication, 
						   uint8_t enable_per_message_authentication, 
						   uint8_t enable_pef_alerting, 
						   uint8_t channel_privilege_limit);
int8_t set_bmc_serial_conf_conn_mode (ipmi_device_t *dev, 
				      uint8_t enable_basic_mode, 
				      uint8_t enable_ppp_mode, 
				      uint8_t enable_terminal_mode, 
				      uint8_t connect_mode);
int8_t set_bmc_serial_conf_page_blackout_interval (ipmi_device_t *dev, 
						   uint8_t page_blackout_interval);
int8_t set_bmc_serial_conf_call_retry_time (ipmi_device_t *dev, 
					    uint8_t call_retry_time);
int8_t set_bmc_serial_conf_ipmi_msg_comm_settings (ipmi_device_t *dev, 
						   uint8_t dtr_hangup, 
						   uint8_t flow_control, 
						   uint8_t bit_rate);
int8_t set_pef_control (ipmi_device_t *dev, 
			uint8_t pef_enable, 
			uint8_t pef_event_msgs_enable, 
			uint8_t pef_startup_delay_enable, 
			uint8_t pef_alert_startup_delay_enable);
int8_t set_pef_global_action_control (ipmi_device_t *dev, 
				      uint8_t alert_action_enable, 
				      uint8_t powerdown_action_enable, 
				      uint8_t reset_action_enable, 
				      uint8_t powercycle_action_enable, 
				      uint8_t oem_action_enable, 
				      uint8_t diag_interrupt_enable);
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
                                                   uint8_t *enable_gratuitous_arps, 
                                                   uint8_t *enable_arp_response);
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
int8_t get_bmc_serial_conf_conn_mode (ipmi_device_t *dev, 
				      uint8_t *enable_basic_mode, 
				      uint8_t *enable_ppp_mode, 
				      uint8_t *enable_terminal_mode, 
				      uint8_t *connect_mode);
int8_t get_bmc_serial_conf_page_blackout_interval (ipmi_device_t *dev, 
						   uint8_t *page_blackout_interval);
int8_t get_bmc_serial_conf_call_retry_time (ipmi_device_t *dev, 
					    uint8_t *call_retry_time);
int8_t get_bmc_serial_conf_ipmi_msg_comm_settings (ipmi_device_t *dev, 
						   uint8_t *dtr_hangup, 
						   uint8_t *flow_control, 
						   uint8_t *bit_rate);
int8_t get_bmc_power_restore_policy (ipmi_device_t *dev, 
				     uint8_t *power_restore_policy);
int8_t get_bmc_lan_conf_vlan_id (ipmi_device_t *dev, 
				 uint8_t *vlan_id_flag, uint32_t *vlan_id);
int8_t get_bmc_lan_conf_vlan_priority (ipmi_device_t *dev, 
				       uint8_t *vlan_priority);
int8_t get_pef_control (ipmi_device_t *dev, 
			uint8_t *pef_enable, 
			uint8_t *pef_event_msgs_enable, 
			uint8_t *pef_startup_delay_enable, 
			uint8_t *pef_alert_startup_delay_enable);
int8_t get_pef_global_action_control (ipmi_device_t *dev, 
				      uint8_t *alert_action_enable, 
				      uint8_t *powerdown_action_enable, 
				      uint8_t *reset_action_enable, 
				      uint8_t *powercycle_action_enable, 
				      uint8_t *oem_action_enable, 
				      uint8_t *diag_interrupt_enable);
int8_t get_pef_startup_delay (ipmi_device_t *dev, 
			      uint8_t *pef_startup_delay);
int8_t get_pef_alert_startup_delay (ipmi_device_t *dev, 
				    uint8_t *pef_alert_startup_delay);
/***********************************************************/
int8_t check_bmc_user_password (ipmi_device_t *dev, 
				uint8_t userid, 
				uint8_t *password);
#endif
