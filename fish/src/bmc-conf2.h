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

/* struct bmc_channel */
/* { */
/*   u_int8_t access_mode; */
/*   u_int8_t enable_user_level_auth; */
/*   u_int8_t enable_per_message_auth; */
/*   u_int8_t enable_pef_alerting; */
/*   u_int8_t channel_privilege_limit; */
/* }; */

struct auth_type
{
  u_int8_t type_none;
  u_int8_t type_md2;
  u_int8_t type_md5;
  u_int8_t type_straight_password;
  u_int8_t type_oem_proprietary;
};

struct bmc_auth_level
{
  struct auth_type callback;
  struct auth_type user;
  struct auth_type operator;
  struct auth_type admin;
  struct auth_type oem;
};


u_int8_t set_bmc_user_access (u_int8_t userid, 
			      u_int8_t channel_number, 
			      u_int8_t enable_ipmi_msgs, 
			      u_int8_t enable_link_auth, 
			      u_int8_t enable_restrict_to_callback, 
			      u_int8_t privilege_limit, 
			      u_int8_t session_limit);
u_int8_t set_bmc_channel_access (u_int8_t channel_number, 
				 u_int8_t set_option, 
				 u_int8_t access_mode, 
				 u_int8_t enable_user_level_auth, 
				 u_int8_t enable_per_message_auth, 
				 u_int8_t enable_pef_alerting, 
				 u_int8_t channel_privilege_limit);



u_int8_t set_bmc_username (u_int8_t userid, u_int8_t *username);
u_int8_t set_bmc_enable_user (u_int8_t userid, int user_status);
u_int8_t set_bmc_user_password (u_int8_t userid, u_int8_t *password);
u_int8_t set_bmc_user_lan_channel_access (u_int8_t userid, 
					  u_int8_t lan_enable_ipmi_msgs, 
					  u_int8_t lan_enable_link_auth, 
					  u_int8_t lan_enable_restrict_to_callback, 
					  u_int8_t lan_privilege_limit, 
					  u_int8_t lan_session_limit);
u_int8_t set_bmc_user_serial_channel_access (u_int8_t userid, 
					     u_int8_t serial_enable_ipmi_msgs, 
					     u_int8_t serial_enable_link_auth, 
					     u_int8_t serial_enable_restrict_to_callback, 
					     u_int8_t serial_privilege_limit, 
					     u_int8_t serial_session_limit);



u_int8_t set_bmc_lan_channel_volatile_access (u_int8_t access_mode, 
					      u_int8_t enable_user_level_auth, 
					      u_int8_t enable_per_message_auth, 
					      u_int8_t enable_pef_alerting, 
					      u_int8_t channel_privilege_limit);
u_int8_t set_bmc_lan_channel_non_volatile_access (u_int8_t access_mode, 
						  u_int8_t enable_user_level_auth, 
						  u_int8_t enable_per_message_auth, 
						  u_int8_t enable_pef_alerting, 
						  u_int8_t channel_privilege_limit);
u_int8_t set_bmc_lan_conf_ip_addr_source (u_int8_t ip_addr_source);
u_int8_t set_bmc_lan_conf_ip_addr (char *ip_addr);
u_int8_t set_bmc_lan_conf_mac_addr (char *mac_addr);
u_int8_t set_bmc_lan_conf_subnet_mask (char *subnet_mask);
u_int8_t set_bmc_lan_conf_default_gw_ip_addr (char *default_gw_ip_addr);
u_int8_t set_bmc_lan_conf_default_gw_mac_addr (char *default_gw_mac_addr);
u_int8_t set_bmc_lan_conf_backup_gw_ip_addr (char *backup_gw_ip_addr);
u_int8_t set_bmc_lan_conf_backup_gw_mac_addr (char *backup_gw_mac_addr);
u_int8_t set_bmc_lan_conf_vlan_id (u_int8_t vlan_id_flag,
                                   u_int32_t vlan_id);
u_int8_t set_bmc_lan_conf_vlan_priority (u_int8_t vlan_priority);

u_int8_t set_bmc_lan_conf_auth_type_enables (struct bmc_auth_level *bmc_auth_level);
u_int8_t set_bmc_lan_conf_arp_control (u_int8_t enable_gratuitous_arps, 
				       u_int8_t enable_arp_response);
u_int8_t set_bmc_lan_conf_gratuitous_arp (u_int8_t gratuitous_arp_interval);



u_int8_t set_bmc_serial_channel_volatile_access (u_int8_t access_mode, 
						 u_int8_t enable_user_level_auth, 
						 u_int8_t enable_per_message_auth, 
						 u_int8_t enable_pef_alerting, 
						 u_int8_t channel_privilege_limit);
u_int8_t set_bmc_serial_channel_non_volatile_access (u_int8_t access_mode, 
						     u_int8_t enable_user_level_auth, 
						     u_int8_t enable_per_message_auth, 
						     u_int8_t enable_pef_alerting, 
						     u_int8_t channel_privilege_limit);
u_int8_t set_bmc_serial_conf_conn_mode (u_int8_t enable_basic_mode, 
					u_int8_t enable_ppp_mode, 
					u_int8_t enable_terminal_mode, 
					u_int8_t connect_mode);
u_int8_t set_bmc_serial_conf_page_blackout_interval (u_int8_t page_blackout_interval);
u_int8_t set_bmc_serial_conf_call_retry_time (u_int8_t call_retry_time);
u_int8_t set_bmc_serial_conf_ipmi_msg_comm_settings (u_int8_t dtr_hangup, 
						     u_int8_t flow_control, 
						     u_int8_t bit_rate);



u_int8_t set_bmc_power_restore_policy (u_int8_t power_restore_policy);



u_int8_t get_bmc_user_access (u_int8_t userid, 
			      u_int8_t channel_number, 
			      u_int8_t *enable_ipmi_msgs, 
			      u_int8_t *enable_link_auth, 
			      u_int8_t *enable_restrict_to_callback, 
			      u_int8_t *privilege_limit, 
			      u_int8_t *session_limit);
u_int8_t get_bmc_channel_access (u_int8_t channel_number, 
				 u_int8_t access_type, 
				 u_int8_t *access_mode, 
				 u_int8_t *user_level_auth, 
				 u_int8_t *per_message_auth, 
				 u_int8_t *pef_alerting, 
				 u_int8_t *privilege_limit);
u_int8_t get_bmc_chassis_status (u_int8_t *power_restore_policy);


u_int8_t get_bmc_username (u_int8_t userid, u_int8_t *username);
u_int8_t get_bmc_user_lan_channel_access (u_int8_t userid, 
					  u_int8_t *enable_ipmi_msgs, 
					  u_int8_t *enable_link_auth, 
					  u_int8_t *enable_restrict_to_callback, 
					  u_int8_t *privilege_limit, 
					  u_int8_t *session_limit);
u_int8_t get_bmc_user_serial_channel_access (u_int8_t userid, 
					     u_int8_t *enable_ipmi_msgs, 
					     u_int8_t *enable_link_auth, 
					     u_int8_t *enable_restrict_to_callback, 
					     u_int8_t *privilege_limit, 
					     u_int8_t *session_limit);


u_int8_t get_bmc_lan_channel_volatile_access (u_int8_t *access_mode, 
					      u_int8_t *user_level_auth, 
					      u_int8_t *per_message_auth, 
					      u_int8_t *pef_alerting, 
					      u_int8_t *privilege_limit);
u_int8_t get_bmc_lan_channel_non_volatile_access (u_int8_t *access_mode, 
						  u_int8_t *user_level_auth, 
						  u_int8_t *per_message_auth, 
						  u_int8_t *pef_alerting, 
						  u_int8_t *privilege_limit);
u_int8_t get_bmc_lan_conf_ip_addr_source (u_int8_t *ip_addr_source);
u_int8_t get_bmc_lan_conf_ip_addr (char *ip_addr);
u_int8_t get_bmc_lan_conf_mac_addr (char *mac_addr);
u_int8_t get_bmc_lan_conf_subnet_mask (char *subnet_mask);
u_int8_t get_bmc_lan_conf_default_gw_ip_addr (char *default_gw_ip_addr);
u_int8_t get_bmc_lan_conf_default_gw_mac_addr (char *default_gw_mac_addr);
u_int8_t get_bmc_lan_conf_backup_gw_ip_addr (char *backup_gw_ip_addr);
u_int8_t get_bmc_lan_conf_backup_gw_mac_addr (char *backup_gw_mac_addr);
u_int8_t get_bmc_lan_conf_auth_type_enables (struct bmc_auth_level *bmc_auth_level);
u_int8_t get_bmc_lan_conf_arp_control (u_int8_t *enable_gratuitous_arps, 
				       u_int8_t *enable_arp_response);
u_int8_t get_bmc_lan_conf_gratuitous_arp (u_int8_t *gratuitous_arp_interval);

u_int8_t get_bmc_serial_channel_volatile_access (u_int8_t *access_mode, 
						 u_int8_t *user_level_auth, 
						 u_int8_t *per_message_auth, 
						 u_int8_t *pef_alerting, 
						 u_int8_t *privilege_limit);
u_int8_t get_bmc_serial_channel_non_volatile_access (u_int8_t *access_mode, 
						     u_int8_t *user_level_auth, 
						     u_int8_t *per_message_auth, 
						     u_int8_t *pef_alerting, 
						     u_int8_t *privilege_limit);
u_int8_t get_bmc_serial_conf_conn_mode (u_int8_t *enable_basic_mode, 
					u_int8_t *enable_ppp_mode, 
					u_int8_t *enable_terminal_mode, 
					u_int8_t *connect_mode);
u_int8_t get_bmc_serial_conf_page_blackout_interval (u_int8_t *page_blackout_interval);
u_int8_t get_bmc_serial_conf_call_retry_time (u_int8_t *call_retry_time);
u_int8_t get_bmc_serial_conf_ipmi_msg_comm_settings (u_int8_t *dtr_hangup, 
						     u_int8_t *flow_control, 
						     u_int8_t *bit_rate);


u_int8_t get_bmc_power_restore_policy (u_int8_t *power_restore_policy);
u_int8_t get_bmc_lan_conf_vlan_id (u_int8_t *vlan_id_flag, u_int32_t *vlan_id);
u_int8_t get_bmc_lan_conf_vlan_priority (u_int8_t *vlan_priority);

/***********************************************************/
u_int8_t check_bmc_user_password (u_int8_t userid, u_int8_t *password);
#endif
