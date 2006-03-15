/* 
   extension.c: fish extensions to guile
   
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

#include "common.h"

SCM ex_fish_ready_hook;
int hook_return = 0;

int
get_hook_return (void)
{
  return hook_return;
}

void
set_hook_return (int hook_return_value)
{
  hook_return = hook_return_value;
}

SCM
get_fish_ready_hook ()
{
  return (ex_fish_ready_hook);
}

void
install_new_variables (void)
{
  // This is this is the place to install new variables
}

void
install_new_hooks (void)
{
  /* 
     ;; sample eqivalent code in scheme

     (define hook-test (make-hook 2))
     (define hook-proc (lambda (frm message)
     (display frm)
     (newline)
     (display message)
     (newline)))
     (add-hook! hook-test hook-proc)
     (run-hook hook-test "hi" "test")
   */

  /* scm_create_hook creates a hook with name, which is
     interned. This is only really useful from c, since in scheme, you
     can (define name (make-hook n_args))
   */

  ex_fish_ready_hook = scm_permanent_object (scm_make_hook (SCM_INUM0));
  scm_c_define ("fi-fish-ready-hook", ex_fish_ready_hook); 

  /* hook: fi-fish-ready-hook
     is called from
     file: fish.c
     function: inner_main (...)
     with no arguments

     sets the hook like this
     (add-hook! fi-fish-ready-hook (lambda () (do-something)))
   */
}

void
install_new_procedures (void)
{
  /* installing general procedures */
  gh_new_procedure ("fi-load", ex_load, 1, 0, 0);
  /* example scheme expression
     (fi-load "discover.scm")
  */

  gh_new_procedure ("fi-register-command!",
		    ex_register_command_x, 1, 0, 0);
  /* example scheme expression
     (fi-register-command  "ping")
  */

  gh_new_procedure ("fi-unregister-command!",
		    ex_unregister_command_x, 1, 0, 0);
  /* example scheme expression
     (fi-unregister-command  "ping")
  */

  gh_new_procedure ("fi-version", ex_version, 0, 0, 0);
  /* example scheme expression
     (display (fi-version))
  */

  gh_new_procedure ("fi-toggle!", ex_toggle_x, 1, 0, 0);
  /* example scheme expression
     (fi-bell-toggle! 'bell)
  */

  gh_new_procedure ("fi-get-sysconfig-dir",
		    ex_get_sysconfig_dir, 0, 0, 0);
  /* example scheme expression
     (display (fi-get-sysconfig-dir))
  */

  gh_new_procedure ("fi-set-prompt!", ex_set_prompt_x, 1, 0, 0);
  /* example scheme expression
     ;; set prompt string
     (fi-set-prompt! "~qp~> ")
  */

  /* installing hook related procedures */
  gh_new_procedure ("fi-hook-return", ex_hook_return, 0, 0, 0);
  /* example scheme expression
     (fi-hook-return)
  */

  gh_new_procedure ("fi-exit", ex_exit, 1, 0, 0);
  /* example scheme expression
     (fi-exit 0)
  */

  gh_new_procedure ("fi-quit", ex_quit, 0, 0, 0);
  /* example scheme expression
     (fi-quit)
  */

  gh_new_procedure ("fi-ping", ex_ipmi_ping, 2, 0, 0);
  /* example scheme expression
     (fi-ping "ipmi.gnu.org" timeout)
  */

  gh_new_procedure ("fi-command-line", ex_get_script_command_line, 0, 0, 0);
  /* example scheme expression
     (display (fi-command-line))
  */
  
  gh_new_procedure ("fi-sensors-get-default-cache-filename", 
		    ex_get_default_sdr_repository_cache_filename, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-get-default-cache-filename))
  */
  
  gh_new_procedure ("fi-sensors-get-group-list", ex_sensors_get_group_list, 0, 0, 0);
  /* example scheme expression
     (display (fi-sensors-get-group-list))
  */
  
  gh_new_procedure ("fi-sel-get-first-entry", 
		    ex_sel_get_first_entry, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-get-first-entry))
  */
  
  gh_new_procedure ("fi-sel-get-next-entry", 
		    ex_sel_get_next_entry, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-get-next-entry))
  */
  
  gh_new_procedure ("fi-sel-get-first-entry-raw", 
		    ex_sel_get_first_entry_raw, 0, 0, 0);
  
  gh_new_procedure ("fi-sel-get-next-entry-raw", 
		    ex_sel_get_next_entry_raw, 0, 0, 0);
  
  gh_new_procedure ("fi-sel-get-first-entry-hex", 
		    ex_sel_get_first_entry_hex, 0, 0, 0);
  
  gh_new_procedure ("fi-sel-get-next-entry-hex", 
		    ex_sel_get_next_entry_hex, 0, 0, 0);
  
  gh_new_procedure ("fi-sel-get-info-binary",
                    ex_sel_get_info_binary, 0, 0, 0);

  gh_new_procedure ("fi-sel-delete-entry", 
		    ex_sel_delete_entry, 1, 0, 0);
  /* example scheme expression
     (display (fi-sel-delete-entry 44))
  */
  
  gh_new_procedure ("fi-sel-clear", 
		    ex_sel_clear, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-clear))
  */
  
  gh_new_procedure ("fi-sel-get-clear-status", 
		    ex_sel_get_clear_status, 0, 0, 0);
  /* example scheme expression
     (display (fi-sel-get-clear-status))
  */
  
  /***
   *** bmc-conf2 extension functions
   ***/
  gh_new_procedure ("fi-set-bmc-username", ex_set_bmc_username, 2, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-username USERID USERNAME)
  */
  
  gh_new_procedure ("fi-set-bmc-enable-user", ex_set_bmc_enable_user, 2, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-enable-user USERID BOOLEAN)
  */
  
  gh_new_procedure ("fi-set-bmc-user-password", ex_set_bmc_user_password, 2, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-user-password USERID PASSWORD)
  */
  
  gh_new_procedure ("fi-set-bmc-user-lan-channel-access", ex_set_bmc_user_lan_channel_access, 6, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-user-lan-channel-access USERID ENABLE-IPMI-MSGS ENABLE-LINK-AUTH ENABLE-RESTRICT-TO-CALLBACK PRIVILEGE-LIMIT SESSION-LIMIT)
  */
  
  gh_new_procedure ("fi-set-bmc-user-serial-channel-access", ex_set_bmc_user_serial_channel_access, 6, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-user-serial-channel-access USERID ENABLE-IPMI-MSGS ENABLE-LINK-AUTH ENABLE-RESTRICT-TO-CALLBACK PRIVILEGE-LIMIT SESSION-LIMIT)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-channel-volatile-access", ex_set_bmc_lan_channel_volatile_access, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-channel-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-channel-non-volatile-access", ex_set_bmc_lan_channel_non_volatile_access, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-channel-non-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-ip-address-source", ex_set_bmc_lan_conf_ip_address_source, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-ip-address-source IP-ADDRESS-SOURCE)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-ip-address", ex_set_bmc_lan_conf_ip_address, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-ip-address IP-ADDRESS)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-mac-address", ex_set_bmc_lan_conf_mac_address, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-mac-address MAC-ADDRESS)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-subnet-mask", ex_set_bmc_lan_conf_subnet_mask, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-subnet-mask SUBNET-MASK)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-default-gateway-address", ex_set_bmc_lan_conf_default_gateway_address, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-default-gateway-address IP-ADDRESS)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-default-gateway-mac-address", ex_set_bmc_lan_conf_default_gateway_mac_address, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-default-gateway-mac-address MAC-ADDRESS)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-backup-gateway-address", ex_set_bmc_lan_conf_backup_gateway_address, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-backup-gateway-address IP-ADDRESS)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-backup-gateway-mac-address", ex_set_bmc_lan_conf_backup_gateway_mac_address, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-backup-gateway-mac-address MAC-ADDRESS)
  */

  gh_new_procedure ("fi-set-bmc-lan-conf-vlan-id", ex_set_bmc_lan_conf_vlan_id, 2, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-vlan-id VLAN_ID VLAN_ID_ENABLE)
  */

  gh_new_procedure ("fi-set-bmc-lan-conf-vlan-priority", ex_set_bmc_lan_conf_vlan_priority, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-vlan-priority VLAN_PRIORITY)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-auth-type-callback-enables", ex_set_bmc_lan_conf_authentication_type_callback_enables, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-callback-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-auth-type-user-enables", ex_set_bmc_lan_conf_authentication_type_user_enables, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-user-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-auth-type-operator-enables", ex_set_bmc_lan_conf_authentication_type_operator_enables, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-operator-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-auth-type-admin-enables", ex_set_bmc_lan_conf_authentication_type_admin_enables, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-admin-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-auth-type-oem-enables", ex_set_bmc_lan_conf_authentication_type_oem_enables, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-oem-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-bmc-generated-arp-control", ex_set_bmc_lan_conf_bmc_generated_arp_control, 2, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-bmc-generated-arp-control ENABLE-GRATUITOUS-ARPS ENABLE-ARP-RESPONSE)
  */
  
  gh_new_procedure ("fi-set-bmc-lan-conf-gratuitous-arp-interval", ex_set_bmc_lan_conf_gratuitous_arp_interval, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-gratuitous-arp-interval ARP-INTERVAL)
  */
  
  gh_new_procedure ("fi-set-bmc-serial-channel-volatile-access", ex_set_bmc_serial_channel_volatile_access, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-serial-channel-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  gh_new_procedure ("fi-set-bmc-serial-channel-non-volatile-access", ex_set_bmc_serial_channel_non_volatile_access, 5, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-serial-channel-non-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  gh_new_procedure ("fi-set-bmc-serial-conf-connection-mode", ex_set_bmc_serial_conf_connection_mode, 4, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-connection-mode BASIC-MODE PPP-MODE TERMINAL-MODE CONNECT-MODE)
  */
  
  gh_new_procedure ("fi-set-bmc-serial-conf-page-blackout-interval", ex_set_bmc_serial_conf_page_blackout_interval, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-page-blackout-interval PAGE-BLACKOUT-INTERVAL)
  */
  
  gh_new_procedure ("fi-set-bmc-serial-conf-call-retry-interval", ex_set_bmc_serial_conf_call_retry_interval, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-call-retry-interval CALL-RETRY-INTERVAL)
  */
  
  gh_new_procedure ("fi-set-bmc-serial-conf-ipmi-messaging-comm-settings", ex_set_bmc_serial_conf_ipmi_messaging_comm_settings, 3, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-ipmi-messaging-comm-settings DTR-HANGUP FLOW-CONTROL BIT-RATE)
  */
  
  gh_new_procedure ("fi-set-bmc-power-restore-policy", ex_set_bmc_power_restore_policy, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-power-restore-policy POWER-RESTORE-POLICY)
  */
  
  gh_new_procedure ("fi-set-bmc-pef-conf-pef-control", 
		    ex_set_bmc_pef_conf_pef_control, 1, 0, 0);
  gh_new_procedure ("fi-set-bmc-pef-conf-pef-action-global-control", 
		    ex_set_bmc_pef_conf_pef_action_global_control, 1, 0, 0);
  gh_new_procedure ("fi-set-bmc-pef-conf-pef-startup-delay", 
		    ex_set_bmc_pef_conf_pef_startup_delay, 1, 0, 0);
  gh_new_procedure ("fi-set-bmc-pef-conf-pef-alert-startup-delay", 
		    ex_set_bmc_pef_conf_pef_alert_startup_delay, 1, 0, 0);
  
  gh_new_procedure ("fi-set-sol-sol-enable", ex_set_sol_sol_enable, 1, 0, 0);
  /*
     syntax in scheme: (fi-set-sol-sol-enable BOOLEAN)
  */

  gh_new_procedure ("fi-set-sol-sol-authentication", ex_set_sol_sol_authentication, 3, 0, 0);
  /*
     syntax in scheme: (fi-set-sol-sol-authentication PRIVILEGE BOOLEN BOOLEAN)
  */
  
  gh_new_procedure ("fi-set-sol-character-accumulate-interval-and-send-threshold", 
                    ex_set_sol_character_accumulate_interval_and_send_threshold, 2, 0, 0);
  /* 
     syntax in scheme: (fi-set-sol-character-accumulate-interval-and-send-threshold INTERVAL THRESHOLD)
   */

  gh_new_procedure ("fi-set-sol-sol-retry", ex_set_sol_sol_retry, 2, 0, 0);
  /* 
     syntax in scheme: (fi-set-sol-sol-retry COUNT INTERVAL)
   */

  gh_new_procedure ("fi-set-sol-sol-non-volatile-bit-rate", ex_set_sol_sol_non_volatile_bit_rate, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-sol-sol-non-volatile-bit-rate BITRATE)
   */

  gh_new_procedure ("fi-set-sol-sol-volatile-bit-rate", ex_set_sol_sol_volatile_bit_rate, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-sol-sol-volatile-bit-rate BITRATE)
   */

  gh_new_procedure ("fi-set-sol-sol-payload-port-number", ex_set_sol_sol_payload_port_number, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-sol-sol-payload-port-number PORTNUMBER)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-0", ex_get_rmcpplus_cipher_suite_id_1, 1, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-0 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-1", ex_set_rmcpplus_cipher_suite_id_1, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-1 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-2", ex_set_rmcpplus_cipher_suite_id_2, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-2 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-3", ex_set_rmcpplus_cipher_suite_id_3, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-3 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-4", ex_set_rmcpplus_cipher_suite_id_4, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-4 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-5", ex_set_rmcpplus_cipher_suite_id_5, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-5 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-6", ex_set_rmcpplus_cipher_suite_id_6, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-6 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-7", ex_set_rmcpplus_cipher_suite_id_7, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-7 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-8", ex_set_rmcpplus_cipher_suite_id_8, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-8 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-9", ex_set_rmcpplus_cipher_suite_id_9, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-9 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-10", ex_set_rmcpplus_cipher_suite_id_11, 0, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-10 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-11", ex_set_rmcpplus_cipher_suite_id_11, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-11 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-12", ex_set_rmcpplus_cipher_suite_id_12, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-12 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-13", ex_set_rmcpplus_cipher_suite_id_13, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-13 privilege)
   */

  gh_new_procedure ("fi-set-rmcpplus-cipher-suite-id-14", ex_set_rmcpplus_cipher_suite_id_14, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-rmcpplus-cipher-suite-id-14 privilege)
   */

  gh_new_procedure ("fi-set-k-r", ex_set_k_r, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-k-r str)
   */
  
  gh_new_procedure ("fi-set-k-g", ex_set_k_g, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-k-g str)
   */


  gh_new_procedure ("fi-get-bmc-username", ex_get_bmc_username, 1, 0, 0);
  /* 
     syntax in scheme: (fi-set-bmc-username USERID)
  */
  
  gh_new_procedure ("fi-get-bmc-user-lan-channel-access", ex_get_bmc_user_lan_channel_access, 1, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-user-lan-channel-access USERID)
  */
  
  gh_new_procedure ("fi-get-bmc-user-serial-channel-access", ex_get_bmc_user_serial_channel_access, 1, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-user-serial-channel-access USERID)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-channel-volatile-access", ex_get_bmc_lan_channel_volatile_access, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-channel-volatile-access)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-channel-non-volatile-access", ex_get_bmc_lan_channel_non_volatile_access, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-channel-non-volatile-access)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-ip-address-source", ex_get_bmc_lan_conf_ip_address_source, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-ip-address-source)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-ip-address", ex_get_bmc_lan_conf_ip_address, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-ip-address)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-mac-address", ex_get_bmc_lan_conf_mac_address, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-mac-address)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-subnet-mask", ex_get_bmc_lan_conf_subnet_mask, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-subnet-mask)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-default-gateway-address", ex_get_bmc_lan_conf_default_gateway_address, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-default-gateway-address)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-default-gateway-mac-address", ex_get_bmc_lan_conf_default_gateway_mac_address, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-default-gateway-mac-address)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-backup-gateway-address", ex_get_bmc_lan_conf_backup_gateway_address, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-backup-gateway-address)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-backup-gateway-mac-address", ex_get_bmc_lan_conf_backup_gateway_mac_address, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-backup-gateway-mac-address)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-vlan-id", ex_get_bmc_lan_conf_vlan_id, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-vlan-id)
  */

  gh_new_procedure ("fi-get-bmc-lan-conf-vlan-priority", ex_get_bmc_lan_conf_vlan_priority, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-vlan-priority)
  */

  gh_new_procedure ("fi-get-bmc-lan-conf-auth-type-callback-enables", ex_get_bmc_lan_conf_authentication_type_callback_enables, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-callback-enables)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-auth-type-user-enables", ex_get_bmc_lan_conf_authentication_type_user_enables, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-user-enables)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-auth-type-operator-enables", ex_get_bmc_lan_conf_authentication_type_operator_enables, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-operator-enables)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-auth-type-admin-enables", ex_get_bmc_lan_conf_authentication_type_admin_enables, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-admin-enables)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-auth-type-oem-enables", ex_get_bmc_lan_conf_authentication_type_oem_enables, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-oem-enables)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-bmc-generated-arp-control", ex_get_bmc_lan_conf_bmc_generated_arp_control, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-bmc-generated-arp-control)
  */
  
  gh_new_procedure ("fi-get-bmc-lan-conf-gratuitous-arp-interval", ex_get_bmc_lan_conf_gratuitous_arp_interval, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-gratuitous-arp-interval)
  */
  
  gh_new_procedure ("fi-get-bmc-serial-channel-volatile-access", ex_get_bmc_serial_channel_volatile_access, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-serial-channel-volatile-access)
  */
  
  gh_new_procedure ("fi-get-bmc-serial-channel-non-volatile-access", ex_get_bmc_serial_channel_non_volatile_access, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-serial-channel-non-volatile-access)
  */
  
  gh_new_procedure ("fi-get-bmc-serial-conf-connection-mode", ex_get_bmc_serial_conf_connection_mode, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-connection-mode)
  */
  
  gh_new_procedure ("fi-get-bmc-serial-conf-page-blackout-interval", ex_get_bmc_serial_conf_page_blackout_interval, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-page-blackout-interval)
  */
  
  gh_new_procedure ("fi-get-bmc-serial-conf-call-retry-interval", ex_get_bmc_serial_conf_call_retry_interval, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-call-retry-interval)
  */
  
  gh_new_procedure ("fi-get-bmc-serial-conf-ipmi-messaging-comm-settings", ex_get_bmc_serial_conf_ipmi_messaging_comm_settings, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-ipmi-messaging-comm-settings)
  */
  
  gh_new_procedure ("fi-get-bmc-power-restore-policy", ex_get_bmc_power_restore_policy, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-bmc-power-restore-policy)
  */
  
  gh_new_procedure ("fi-get-bmc-pef-conf-pef-control", 
		    ex_get_bmc_pef_conf_pef_control, 0, 0, 0);
  gh_new_procedure ("fi-get-bmc-pef-conf-pef-action-global-control", 
		    ex_get_bmc_pef_conf_pef_action_global_control, 0, 0, 0);
  gh_new_procedure ("fi-get-bmc-pef-conf-pef-startup-delay", 
		    ex_get_bmc_pef_conf_pef_startup_delay, 0, 0, 0);
  gh_new_procedure ("fi-get-bmc-pef-conf-pef-alert-startup-delay", 
		    ex_get_bmc_pef_conf_pef_alert_startup_delay, 0, 0, 0);
  
  gh_new_procedure ("fi-get-sol-sol-enable", ex_get_sol_sol_enable, 0, 0, 0);
  /*
     syntax in scheme: (fi-get-sol-sol-enable)
  */

  gh_new_procedure ("fi-get-sol-sol-authentication", ex_get_sol_sol_authentication, 0, 0, 0);
  /*
     syntax in scheme: (fi-get-sol-sol-authentication)
  */
  
  gh_new_procedure ("fi-get-sol-character-accumulate-interval-and-send-threshold", 
                    ex_get_sol_character_accumulate_interval_and_send_threshold, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sol-character-accumulate-interval-and-send-threshold)
   */

  gh_new_procedure ("fi-get-sol-sol-retry", ex_get_sol_sol_retry, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sol-sol-retry)
   */

  gh_new_procedure ("fi-get-sol-sol-non-volatile-bit-rate", ex_get_sol_sol_non_volatile_bit_rate, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sol-sol-non-volatile-bit-rate)
   */

  gh_new_procedure ("fi-get-sol-sol-volatile-bit-rate", ex_get_sol_sol_volatile_bit_rate, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sol-sol-volatile-bit-rate)
   */

  gh_new_procedure ("fi-get-sol-sol-payload-port-number", ex_get_sol_sol_payload_port_number, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sol-sol-payload-port-number)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-0", ex_get_rmcpplus_cipher_suite_id_1, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-0)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-1", ex_get_rmcpplus_cipher_suite_id_1, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-1)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-2", ex_get_rmcpplus_cipher_suite_id_2, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-2)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-3", ex_get_rmcpplus_cipher_suite_id_3, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-3)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-4", ex_get_rmcpplus_cipher_suite_id_4, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-4)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-5", ex_get_rmcpplus_cipher_suite_id_5, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-5)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-6", ex_get_rmcpplus_cipher_suite_id_6, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-6)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-7", ex_get_rmcpplus_cipher_suite_id_7, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-7)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-8", ex_get_rmcpplus_cipher_suite_id_8, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-8)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-9", ex_get_rmcpplus_cipher_suite_id_9, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-9)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-10", ex_get_rmcpplus_cipher_suite_id_10, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-10)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-11", ex_get_rmcpplus_cipher_suite_id_11, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-11)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-12", ex_get_rmcpplus_cipher_suite_id_12, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-12)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-13", ex_get_rmcpplus_cipher_suite_id_13, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-13)
   */

  gh_new_procedure ("fi-get-rmcpplus-cipher-suite-id-14", ex_get_rmcpplus_cipher_suite_id_14, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-rmcpplus-cipher-suite-id-14)
   */

  gh_new_procedure ("fi-get-k-r", ex_get_k_r, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-k-r)
   */
  
  gh_new_procedure ("fi-get-k-g", ex_get_k_g, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-k-g)
   */

  gh_new_procedure ("fi-check-bmc-user-password", ex_check_bmc_user_password, 2, 0, 0);
  /* 
     syntax in scheme: (fi-check-bmc-user-password USERID PASSWORD)
  */
  
  gh_new_procedure ("fi-get-sdr-record", ex_get_sdr_record, 1, 0, 0);
  /* 
     syntax in scheme: (fi-get-sdr-record)
  */
  
  gh_new_procedure ("fi-get-sensor-reading", ex_get_sensor_reading, 1, 0, 0);
  /* 
     syntax in scheme: (fi-get-sensor-reading)
  */
  
  gh_new_procedure ("fi-get-sdr-cache-filename", ex_get_sdr_cache_filename, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sdr-cache-filename)
  */
  
  gh_new_procedure ("fi-get-sdr-repository-info", ex_get_sdr_repository_info, 0, 0, 0);
  /* 
     syntax in scheme: (fi-get-sdr-repository-info)
  */
  
  gh_new_procedure ("fi-ipmi-open", ex_ipmi_open, 1, 0, 0);
  gh_new_procedure ("fi-ipmi-close", ex_ipmi_close, 0, 0, 0);
  gh_new_procedure ("fi-cmd-get-device-id-display", ex_cmd_get_device_id_display, 0, 0, 0);
  gh_new_procedure ("fi-get-pef-info", ex_get_pef_info, 0, 0, 0);
  
}

// guile initialization area
void
guile_env_init (void)
{
  install_new_variables ();
  install_new_procedures ();
  install_new_hooks ();
}
