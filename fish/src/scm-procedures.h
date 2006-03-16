/* 
   scm_procedures.h: scm procedures that are exported to
   guile environment
   
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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef _SCM_PROCEDURES_H
#define _SCM_PROCEDURES_H

SCM ex_version (void);
SCM ex_display (SCM scm_message);
SCM ex_bell (void);
SCM ex_toggle_x (SCM scm_what2toggle);
SCM ex_hook_return (void);
SCM ex_register_command_x (SCM command);
SCM ex_unregister_command_x (SCM command);
SCM ex_set_prompt_x (SCM scm_prompt);
SCM ex_exit (SCM scm_status);
SCM ex_quit ();

SCM ex_load (SCM scm_filename);
SCM ex_get_sysconfig_dir (void);

SCM ex_ipmi_ping (SCM scm_host, SCM scm_timeout);

SCM ex_get_script_command_line ();

SCM ex_get_default_sdr_repository_cache_filename (void);

SCM ex_sensors_cache_get_current_group ();
SCM ex_sensors_get_group_list ();

SCM ex_sel_get_first_entry ();
SCM ex_sel_get_next_entry ();
SCM ex_sel_get_first_entry_raw ();
SCM ex_sel_get_next_entry_raw ();
SCM ex_sel_get_first_entry_hex ();
SCM ex_sel_get_next_entry_hex ();
SCM ex_sel_get_info ();
SCM ex_sel_get_info_binary ();
SCM ex_sel_delete_entry (SCM scm_record_id);
SCM ex_sel_clear ();
SCM ex_sel_get_clear_status ();

SCM ex_get_sensors_errno ();

/***
 *** bmc-conf2 extension functions
 ***/
SCM ex_set_bmc_username (SCM scm_userid, SCM scm_username);
SCM ex_set_bmc_enable_user (SCM scm_userid, SCM scm_user_status);
SCM ex_set_bmc_user_password (SCM scm_userid, SCM scm_password);
SCM ex_set_bmc_user_lan_channel_access (SCM scm_userid, 
					SCM scm_lan_user_ipmi_messaging, 
					SCM scm_lan_user_link_authentication, 
					SCM scm_lan_user_restricted_to_callback, 
					SCM scm_lan_privilege_limit, 
					SCM scm_lan_session_limit);
SCM ex_set_bmc_user_sol_payload_access (SCM scm_userid, SCM scm_sol_access);
SCM ex_set_bmc_user_serial_channel_access (SCM scm_userid, 
					   SCM scm_serial_user_ipmi_messaging, 
					   SCM scm_serial_user_link_authentication, 
					   SCM scm_serial_user_restricted_to_callback, 
					   SCM scm_serial_privilege_limit, 
					   SCM scm_serial_session_limit);
SCM ex_set_bmc_lan_channel_volatile_access (SCM scm_access_mode, 
					    SCM scm_user_level_auth, 
					    SCM scm_per_message_auth, 
					    SCM scm_pef_alerting, 
					    SCM scm_channel_privilege_limit);
SCM ex_set_bmc_lan_channel_non_volatile_access (SCM scm_access_mode, 
						SCM scm_user_level_auth, 
						SCM scm_per_message_auth, 
						SCM scm_pef_alerting, 
						SCM scm_channel_privilege_limit);
SCM ex_set_bmc_lan_conf_ip_address_source (SCM scm_ip_address_source);
SCM ex_set_bmc_lan_conf_ip_address (SCM scm_ip_address);
SCM ex_set_bmc_lan_conf_mac_address (SCM scm_mac_address);
SCM ex_set_bmc_lan_conf_subnet_mask (SCM scm_subnet_mask);
SCM ex_set_bmc_lan_conf_default_gateway_address (SCM scm_gateway_address);
SCM ex_set_bmc_lan_conf_default_gateway_mac_address (SCM scm_gateway_mac_address);
SCM ex_set_bmc_lan_conf_backup_gateway_address (SCM scm_gateway_address);
SCM ex_set_bmc_lan_conf_backup_gateway_mac_address (SCM scm_gateway_mac_address);
SCM ex_set_bmc_lan_conf_vlan_id (SCM scm_vlan_id,
                                 SCM scm_vlan_id_flag);
SCM ex_set_bmc_lan_conf_vlan_priority (SCM scm_vlan_priority);

SCM ex_set_bmc_lan_conf_authentication_type_callback_enables (SCM scm_authentication_type_none, 
							      SCM scm_authentication_type_md2, 
							      SCM scm_authentication_type_md5, 
							      SCM scm_authentication_type_straight_password, 
							      SCM scm_authentication_type_oem_proprietary);
SCM ex_set_bmc_lan_conf_authentication_type_user_enables (SCM scm_authentication_type_none, 
							  SCM scm_authentication_type_md2, 
							  SCM scm_authentication_type_md5, 
							  SCM scm_authentication_type_straight_password, 
							  SCM scm_authentication_type_oem_proprietary);
SCM ex_set_bmc_lan_conf_authentication_type_operator_enables (SCM scm_authentication_type_none, 
							      SCM scm_authentication_type_md2, 
							      SCM scm_authentication_type_md5, 
							      SCM scm_authentication_type_straight_password, 
							      SCM scm_authentication_type_oem_proprietary);
SCM ex_set_bmc_lan_conf_authentication_type_admin_enables (SCM scm_authentication_type_none, 
							   SCM scm_authentication_type_md2, 
							   SCM scm_authentication_type_md5, 
							   SCM scm_authentication_type_straight_password, 
							   SCM scm_authentication_type_oem_proprietary);
SCM ex_set_bmc_lan_conf_authentication_type_oem_enables (SCM scm_authentication_type_none, 
							 SCM scm_authentication_type_md2, 
							 SCM scm_authentication_type_md5, 
							 SCM scm_authentication_type_straight_password, 
							 SCM scm_authentication_type_oem_proprietary);
SCM ex_set_bmc_lan_conf_bmc_generated_arp_control (SCM scm_bmc_generated_gratuitous_arps, 
						   SCM scm_bmc_generated_arp_responses);
SCM ex_set_bmc_lan_conf_gratuitous_arp_interval (SCM scm_gratuitous_arp_interval);

SCM ex_set_bmc_serial_channel_volatile_access (SCM scm_access_mode, 
					       SCM scm_user_level_auth, 
					       SCM scm_per_message_auth, 
					       SCM scm_pef_alerting, 
					       SCM scm_channel_privilege_limit);
SCM ex_set_bmc_serial_channel_non_volatile_access (SCM scm_access_mode, 
						   SCM scm_user_level_auth, 
						   SCM scm_per_message_auth, 
						   SCM scm_pef_alerting, 
						   SCM scm_channel_privilege_limit);
SCM ex_set_bmc_serial_conf_connection_mode (SCM scm_basic_mode, 
				      SCM scm_ppp_mode, 
				      SCM scm_terminal_mode, 
				      SCM scm_connect_mode);
SCM ex_set_bmc_serial_conf_page_blackout_interval (SCM scm_page_blackout_interval);
SCM ex_set_bmc_serial_conf_call_retry_interval (SCM scm_call_retry_interval);
SCM ex_set_bmc_serial_conf_ipmi_messaging_comm_settings (SCM scm_dtr_hangup, 
                                                         SCM scm_flow_control, 
                                                         SCM scm_bit_rate);
SCM ex_set_bmc_power_restore_policy (SCM scm_power_restore_policy);
SCM ex_set_bmc_pef_conf_pef_control (SCM scm_pef, 
				     SCM scm_pef_event_messages, 
				     SCM scm_pef_startup_delay, 
				     SCM scm_pef_alert_startup_delay);
SCM ex_set_bmc_pef_conf_pef_action_global_control (SCM scm_alert_action, 
						   SCM scm_power_down_action, 
						   SCM scm_reset_action, 
						   SCM scm_power_cycle_action, 
						   SCM scm_oem_action, 
						   SCM scm_diagnostic_interrupt);
SCM ex_set_bmc_pef_conf_pef_startup_delay (SCM scm_pef_startup_delay);
SCM ex_set_bmc_pef_conf_pef_alert_startup_delay (SCM scm_pef_alert_startup_delay);

SCM ex_set_sol_sol_enable (SCM scm_sol_enable);

SCM ex_set_sol_sol_authentication (SCM scm_sol_privilege_level,
                                   SCM scm_force_sol_payload_authentication,
                                   SCM scm_force_sol_payload_encryption);
SCM ex_set_sol_character_accumulate_interval_and_send_threshold (SCM scm_character_accumulate_interval,
                                                                 SCM scm_character_send_threshold);
SCM ex_set_sol_sol_retry (SCM scm_retry_count,
                          SCM scm_retry_interval);

SCM ex_set_sol_sol_non_volatile_bit_rate (SCM scm_bit_rate);
SCM ex_set_sol_sol_volatile_bit_rate (SCM scm_bit_rate);
SCM ex_set_sol_sol_payload_port_number (SCM scm_port_number);

SCM ex_set_rmcpplus_cipher_suite_id_0 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_1 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_2 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_3 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_4 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_5 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_6 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_7 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_8 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_9 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_10 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_11 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_12 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_13 (SCM scm_privilege);
SCM ex_set_rmcpplus_cipher_suite_id_14 (SCM scm_privilege);

SCM ex_set_k_r (SCM scm_k_r);
SCM ex_set_k_g (SCM scm_k_g);

/*****************/
SCM ex_get_bmc_username (SCM scm_userid);
SCM ex_get_bmc_user_lan_channel_access (SCM scm_userid);
SCM ex_get_bmc_user_sol_payload_access (SCM scm_userid);
SCM ex_get_bmc_user_serial_channel_access (SCM scm_userid);
SCM ex_get_bmc_lan_channel_volatile_access ();
SCM ex_get_bmc_lan_channel_non_volatile_access ();
SCM ex_get_bmc_lan_conf_ip_address_source ();
SCM ex_get_bmc_lan_conf_ip_address ();
SCM ex_get_bmc_lan_conf_mac_address ();
SCM ex_get_bmc_lan_conf_subnet_mask ();
SCM ex_get_bmc_lan_conf_default_gateway_address ();
SCM ex_get_bmc_lan_conf_default_gateway_mac_address ();
SCM ex_get_bmc_lan_conf_backup_gateway_address ();
SCM ex_get_bmc_lan_conf_backup_gateway_mac_address ();
SCM ex_get_bmc_lan_conf_vlan_id ();
SCM ex_get_bmc_lan_conf_vlan_priority ();
SCM ex_get_bmc_lan_conf_authentication_type_callback_enables ();
SCM ex_get_bmc_lan_conf_authentication_type_user_enables ();
SCM ex_get_bmc_lan_conf_authentication_type_operator_enables ();
SCM ex_get_bmc_lan_conf_authentication_type_admin_enables ();
SCM ex_get_bmc_lan_conf_authentication_type_oem_enables ();
SCM ex_get_bmc_lan_conf_bmc_generated_arp_control ();
SCM ex_get_bmc_lan_conf_gratuitous_arp_interval ();
SCM ex_get_bmc_serial_channel_volatile_access ();
SCM ex_get_bmc_serial_channel_non_volatile_access ();
SCM ex_get_bmc_serial_conf_connection_mode ();
SCM ex_get_bmc_serial_conf_page_blackout_interval ();
SCM ex_get_bmc_serial_conf_call_retry_interval ();
SCM ex_get_bmc_serial_conf_ipmi_messaging_comm_settings ();
SCM ex_get_bmc_power_restore_policy ();
SCM ex_get_bmc_pef_conf_pef_control ();
SCM ex_get_bmc_pef_conf_pef_action_global_control ();
SCM ex_get_bmc_pef_conf_pef_startup_delay ();
SCM ex_get_bmc_pef_conf_pef_alert_startup_delay ();
SCM ex_get_sol_sol_enable ();
SCM ex_get_sol_sol_authentication ();
SCM ex_get_sol_character_accumulate_interval_and_send_threshold ();
SCM ex_get_sol_sol_retry ();
SCM ex_get_sol_sol_non_volatile_bit_rate ();
SCM ex_get_sol_sol_volatile_bit_rate ();
SCM ex_get_sol_sol_payload_port_number ();
SCM ex_get_rmcpplus_cipher_suite_id_0 ();
SCM ex_get_rmcpplus_cipher_suite_id_1 ();
SCM ex_get_rmcpplus_cipher_suite_id_2 ();
SCM ex_get_rmcpplus_cipher_suite_id_3 ();
SCM ex_get_rmcpplus_cipher_suite_id_4 ();
SCM ex_get_rmcpplus_cipher_suite_id_5 ();
SCM ex_get_rmcpplus_cipher_suite_id_6 ();
SCM ex_get_rmcpplus_cipher_suite_id_7 ();
SCM ex_get_rmcpplus_cipher_suite_id_8 ();
SCM ex_get_rmcpplus_cipher_suite_id_9 ();
SCM ex_get_rmcpplus_cipher_suite_id_10 ();
SCM ex_get_rmcpplus_cipher_suite_id_11 ();
SCM ex_get_rmcpplus_cipher_suite_id_12 ();
SCM ex_get_rmcpplus_cipher_suite_id_13 ();
SCM ex_get_rmcpplus_cipher_suite_id_14 ();
SCM ex_check_bmc_user_password (SCM scm_userid, SCM scm_password);

SCM ex_get_k_r ();
SCM ex_get_k_g ();

/***********************************************************/

SCM ex_get_sdr_record (SCM scm_record_id);
SCM ex_get_sensor_reading (SCM scm_sdr_record);
SCM ex_get_sdr_cache_filename ();
SCM ex_get_sdr_repository_info ();

/* udm driver exports */
SCM ex_ipmi_open (SCM scm_arg_list);
SCM ex_ipmi_close ();

/* bmc info exports */
SCM ex_cmd_get_device_id_display (void);

/* pef exports */
SCM ex_get_pef_info ();

#endif
