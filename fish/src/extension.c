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

  ex_fish_ready_hook = scm_make_hook (0);
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
  scm_c_define_gsubr ("fi-load", 1, 0, 0, ex_load);
  /* example scheme expression
     (fi-load "discover.scm")
  */

  scm_c_define_gsubr ("fi-register-command!", 1, 0, 0, ex_register_command_x);
  /* example scheme expression
     (fi-register-command  "ping")
  */

  scm_c_define_gsubr ("fi-unregister-command!", 1, 0, 0, ex_unregister_command_x);
  /* example scheme expression
     (fi-unregister-command  "ping")
  */

  scm_c_define_gsubr ("fi-version", 0, 0, 0, ex_version);
  /* example scheme expression
     (display (fi-version))
  */

  scm_c_define_gsubr ("fi-toggle!", 1, 0, 0, ex_toggle_x);
  /* example scheme expression
     (fi-bell-toggle! 'bell)
  */

  scm_c_define_gsubr ("fi-get-sysconfig-dir", 0, 0, 0, ex_get_sysconfig_dir);
  /* example scheme expression
     (display (fi-get-sysconfig-dir))
  */

  scm_c_define_gsubr ("fi-set-prompt!", 1, 0, 0, ex_set_prompt_x);
  /* example scheme expression
     ;; set prompt string
     (fi-set-prompt! "~qp~> ")
  */

  /* installing hook related procedures */
  scm_c_define_gsubr ("fi-hook-return", 0, 0, 0, ex_hook_return);
  /* example scheme expression
     (fi-hook-return)
  */

  scm_c_define_gsubr ("fi-exit", 1, 0, 0, ex_exit);
  /* example scheme expression
     (fi-exit 0)
  */

  scm_c_define_gsubr ("fi-quit", 0, 0, 0, ex_quit);
  /* example scheme expression
     (fi-quit)
  */

  scm_c_define_gsubr ("fi-set-sms-io-base!", 1, 0, 0, ex_set_sms_io_base);
  /* example scheme expression
     (fi-set-sms-io-base! #x0CA2)
  */

  scm_c_define_gsubr ("fi-set-default-driver-poll-interval", 1, 0, 0, ex_set_driver_poll_interval);
  /* example scheme expression
     (fi-set-default-driver-poll-interval 100)
  */

  scm_c_define_gsubr ("fi-ping", 2, 0, 0, ex_ipmi_ping);
  /* example scheme expression
     (fi-ping "ipmi.gnu.org" timeout)
  */

  scm_c_define_gsubr ("fi-command-line", 0, 0, 0, ex_get_script_command_line);
  /* example scheme expression
     (display (fi-command-line))
  */
  
  scm_c_define_gsubr ("fi-sensors-get-default-cache-filename", 0, 0, 0, ex_get_default_sdr_repo_cache_filename);
  /* example scheme expression
     (display (fi-sensors-get-default-cache-filename))
  */
  
  scm_c_define_gsubr ("fi-sensors-get-group-list", 0, 0, 0, ex_sensors_get_group_list);
  /* example scheme expression
     (display (fi-sensors-get-group-list))
  */
  
  scm_c_define_gsubr ("fi-sel-get-first-entry", 0, 0, 0, ex_sel_get_first_entry);
  /* example scheme expression
     (display (fi-sel-get-first-entry))
  */
  
  scm_c_define_gsubr ("fi-sel-get-next-entry", 0, 0, 0, ex_sel_get_next_entry);
  /* example scheme expression
     (display (fi-sel-get-next-entry))
  */
  
  scm_c_define_gsubr ("fi-sel-get-first-entry-raw", 0, 0, 0, ex_sel_get_first_entry_raw);
  
  scm_c_define_gsubr ("fi-sel-get-next-entry-raw", 0, 0, 0, ex_sel_get_next_entry_raw);
  
  scm_c_define_gsubr ("fi-sel-get-first-entry-hex", 0, 0, 0, ex_sel_get_first_entry_hex);
  
  scm_c_define_gsubr ("fi-sel-get-next-entry-hex", 0, 0, 0, ex_sel_get_next_entry_hex);
  
  scm_c_define_gsubr ("fi-sel-get-info-binary", 0, 0, 0, ex_sel_get_info_binary);

  scm_c_define_gsubr ("fi-sel-delete-entry", 1, 0, 0, ex_sel_delete_entry);
  /* example scheme expression
     (display (fi-sel-delete-entry 44))
  */
  
  scm_c_define_gsubr ("fi-sel-clear", 0, 0, 0, ex_sel_clear);
  /* example scheme expression
     (display (fi-sel-clear))
  */
  
  scm_c_define_gsubr ("fi-sel-get-clear-status", 0, 0, 0, ex_sel_get_clear_status);
  /* example scheme expression
     (display (fi-sel-get-clear-status))
  */
  
  /***
   *** bmc-conf2 extension functions
   ***/
  scm_c_define_gsubr ("fi-set-bmc-username", 2, 0, 0, ex_set_bmc_username);
  /* 
     syntax in scheme: (fi-set-bmc-username USERID USERNAME)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-enable-user", 2, 0, 0, ex_set_bmc_enable_user);
  /* 
     syntax in scheme: (fi-set-bmc-enable-user USERID BOOLEAN)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-user-password", 2, 0, 0, ex_set_bmc_user_password);
  /* 
     syntax in scheme: (fi-set-bmc-user-password USERID PASSWORD)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-user-lan-channel-access", 6, 0, 0, ex_set_bmc_user_lan_channel_access);
  /* 
     syntax in scheme: (fi-set-bmc-user-lan-channel-access USERID ENABLE-IPMI-MSGS ENABLE-LINK-AUTH ENABLE-RESTRICT-TO-CALLBACK PRIVILEGE-LIMIT SESSION-LIMIT)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-user-serial-channel-access", 6, 0, 0, ex_set_bmc_user_serial_channel_access);
  /* 
     syntax in scheme: (fi-set-bmc-user-serial-channel-access USERID ENABLE-IPMI-MSGS ENABLE-LINK-AUTH ENABLE-RESTRICT-TO-CALLBACK PRIVILEGE-LIMIT SESSION-LIMIT)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-channel-volatile-access", 5, 0, 0, ex_set_bmc_lan_channel_volatile_access);
  /* 
     syntax in scheme: (fi-set-bmc-lan-channel-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-channel-non-volatile-access", 5, 0, 0, ex_set_bmc_lan_channel_non_volatile_access);
  /* 
     syntax in scheme: (fi-set-bmc-lan-channel-non-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-ip-address-source", 1, 0, 0, ex_set_bmc_lan_conf_ip_addr_source);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-ip-address-source IP-ADDRESS-SOURCE)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-ip-address", 1, 0, 0, ex_set_bmc_lan_conf_ip_addr);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-ip-address IP-ADDRESS)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-mac-address", 1, 0, 0, ex_set_bmc_lan_conf_mac_addr);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-mac-address MAC-ADDRESS)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-subnet-mask", 1, 0, 0, ex_set_bmc_lan_conf_subnet_mask);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-subnet-mask SUBNET-MASK)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-default-gateway-ip-address", 1, 0, 0, ex_set_bmc_lan_conf_default_gw_ip_addr);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-default-gateway-ip-address IP-ADDRESS)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-default-gateway-mac-address", 1, 0, 0, ex_set_bmc_lan_conf_default_gw_mac_addr);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-default-gateway-mac-address MAC-ADDRESS)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-backup-gateway-ip-address", 1, 0, 0, ex_set_bmc_lan_conf_backup_gw_ip_addr);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-backup-gateway-ip-address IP-ADDRESS)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-backup-gateway-mac-address", 1, 0, 0, ex_set_bmc_lan_conf_backup_gw_mac_addr);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-backup-gateway-mac-address MAC-ADDRESS)
  */

  scm_c_define_gsubr ("fi-set-bmc-lan-conf-vlan-id", 2, 0, 0, ex_set_bmc_lan_conf_vlan_id);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-vlan-id VLAN_ID_FLAG VLAN_ID)
  */

  scm_c_define_gsubr ("fi-set-bmc-lan-conf-vlan-priority", 1, 0, 0, ex_set_bmc_lan_conf_vlan_priority);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-vlan-priority VLAN_PRIORITY)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-auth-type-callback-enables", 5, 0, 0, ex_set_bmc_lan_conf_auth_type_callback_enables);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-callback-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-auth-type-user-enables", 5, 0, 0, ex_set_bmc_lan_conf_auth_type_user_enables);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-user-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-auth-type-operator-enables", 5, 0, 0, ex_set_bmc_lan_conf_auth_type_operator_enables);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-operator-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-auth-type-admin-enables", 5, 0, 0, ex_set_bmc_lan_conf_auth_type_admin_enables);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-admin-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-auth-type-oem-enables", 5, 0, 0, ex_set_bmc_lan_conf_auth_type_oem_enables);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-auth-type-oem-enables AUTH_NONE AUTH_MD2 AUTH_MD5 AUTH_STRAIGHT_PASSWORD AUTH_OEM_PROPRIETARY)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-arp-control", 2, 0, 0, ex_set_bmc_lan_conf_arp_control);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-arp-control ENABLE-GRATUITOUS-ARPS ENABLE-ARP-RESPONSE)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-lan-conf-gratuitous-arp", 1, 0, 0, ex_set_bmc_lan_conf_gratuitous_arp);
  /* 
     syntax in scheme: (fi-set-bmc-lan-conf-gratuitous-arp ARP-INTERVAL)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-serial-channel-volatile-access", 5, 0, 0, ex_set_bmc_serial_channel_volatile_access);
  /* 
     syntax in scheme: (fi-set-bmc-serial-channel-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-serial-channel-non-volatile-access", 5, 0, 0, ex_set_bmc_serial_channel_non_volatile_access);
  /* 
     syntax in scheme: (fi-set-bmc-serial-channel-non-volatile-access ACCESS-MODE ENABLE-USER-LEVEL-AUTH ENABLE-PER-MESSAGE-AUTH ENABLE-PEF-ALERTING CHANNEL-PRIVILEGE-LIMIT)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-serial-conf-conn-mode", 4, 0, 0, ex_set_bmc_serial_conf_conn_mode);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-conn-mode BASIC-MODE PPP-MODE TERMINAL-MODE CONNECT-MODE)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-serial-conf-page-blackout-interval", 1, 0, 0, ex_set_bmc_serial_conf_page_blackout_interval);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-page-blackout-interval PAGE-BLACKOUT-INTERVAL)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-serial-conf-call-retry-time", 1, 0, 0, ex_set_bmc_serial_conf_call_retry_time);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-call-retry-time CALL-RETRY-TIME)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-serial-conf-ipmi-msg-comm-settings", 3, 0, 0, ex_set_bmc_serial_conf_ipmi_msg_comm_settings);
  /* 
     syntax in scheme: (fi-set-bmc-serial-conf-ipmi-msg-comm-settings DTR-HANGUP FLOW-CONTROL BIT-RATE)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-power-restore-policy", 1, 0, 0, ex_set_bmc_power_restore_policy);
  /* 
     syntax in scheme: (fi-set-bmc-power-restore-policy POWER-RESTORE-POLICY)
  */
  
  scm_c_define_gsubr ("fi-set-bmc-pef-conf-pef-control", 1, 0, 0, ex_set_bmc_pef_conf_pef_control);
  scm_c_define_gsubr ("fi-set-bmc-pef-conf-pef-global-action-control", 1, 0, 0, ex_set_bmc_pef_conf_pef_global_action_control);
  scm_c_define_gsubr ("fi-set-bmc-pef-conf-pef-startup-delay", 1, 0, 0, ex_set_bmc_pef_conf_pef_startup_delay);
  scm_c_define_gsubr ("fi-set-bmc-pef-conf-pef-alert-startup-delay", 1, 0, 0, ex_set_bmc_pef_conf_pef_alert_startup_delay);
  
  scm_c_define_gsubr ("fi-get-bmc-username", 1, 0, 0, ex_get_bmc_username);
  /* 
     syntax in scheme: (fi-set-bmc-username USERID)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-user-lan-channel-access", 1, 0, 0, ex_get_bmc_user_lan_channel_access);
  /* 
     syntax in scheme: (fi-get-bmc-user-lan-channel-access USERID)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-user-serial-channel-access", 1, 0, 0, ex_get_bmc_user_serial_channel_access);
  /* 
     syntax in scheme: (fi-get-bmc-user-serial-channel-access USERID)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-channel-volatile-access", 0, 0, 0, ex_get_bmc_lan_channel_volatile_access);
  /* 
     syntax in scheme: (fi-get-bmc-lan-channel-volatile-access)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-channel-non-volatile-access", 0, 0, 0, ex_get_bmc_lan_channel_non_volatile_access);
  /* 
     syntax in scheme: (fi-get-bmc-lan-channel-non-volatile-access)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-ip-address-source", 0, 0, 0, ex_get_bmc_lan_conf_ip_addr_source);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-ip-address-source)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-ip-address", 0, 0, 0, ex_get_bmc_lan_conf_ip_addr);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-ip-address)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-mac-address", 0, 0, 0, ex_get_bmc_lan_conf_mac_addr);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-mac-address)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-subnet-mask", 0, 0, 0, ex_get_bmc_lan_conf_subnet_mask);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-subnet-mask)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-default-gateway-ip-address", 0, 0, 0, ex_get_bmc_lan_conf_default_gw_ip_addr);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-default-gateway-ip-address)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-default-gateway-mac-address", 0, 0, 0, ex_get_bmc_lan_conf_default_gw_mac_addr);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-default-gateway-mac-address)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-backup-gateway-ip-address", 0, 0, 0, ex_get_bmc_lan_conf_backup_gw_ip_addr);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-backup-gateway-ip-address)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-backup-gateway-mac-address", 0, 0, 0, ex_get_bmc_lan_conf_backup_gw_mac_addr);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-backup-gateway-mac-address)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-vlan-id", 0, 0, 0, ex_get_bmc_lan_conf_vlan_id);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-vlan-id)
  */

  scm_c_define_gsubr ("fi-get-bmc-lan-conf-vlan-priority", 0, 0, 0, ex_get_bmc_lan_conf_vlan_priority);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-vlan-priority)
  */

  scm_c_define_gsubr ("fi-get-bmc-lan-conf-auth-type-callback-enables", 0, 0, 0, ex_get_bmc_lan_conf_auth_type_callback_enables);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-callback-enables)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-auth-type-user-enables", 0, 0, 0, ex_get_bmc_lan_conf_auth_type_user_enables);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-user-enables)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-auth-type-operator-enables", 0, 0, 0, ex_get_bmc_lan_conf_auth_type_operator_enables);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-operator-enables)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-auth-type-admin-enables", 0, 0, 0, ex_get_bmc_lan_conf_auth_type_admin_enables);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-admin-enables)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-auth-type-oem-enables", 0, 0, 0, ex_get_bmc_lan_conf_auth_type_oem_enables);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-auth-type-oem-enables)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-arp-control", 0, 0, 0, ex_get_bmc_lan_conf_arp_control);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-arp-control)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-lan-conf-gratuitous-arp", 0, 0, 0, ex_get_bmc_lan_conf_gratuitous_arp);
  /* 
     syntax in scheme: (fi-get-bmc-lan-conf-gratuitous-arp)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-serial-channel-volatile-access", 0, 0, 0, ex_get_bmc_serial_channel_volatile_access);
  /* 
     syntax in scheme: (fi-get-bmc-serial-channel-volatile-access)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-serial-channel-non-volatile-access", 0, 0, 0, ex_get_bmc_serial_channel_non_volatile_access);
  /* 
     syntax in scheme: (fi-get-bmc-serial-channel-non-volatile-access)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-serial-conf-conn-mode", 0, 0, 0, ex_get_bmc_serial_conf_conn_mode);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-conn-mode)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-serial-conf-page-blackout-interval", 0, 0, 0, ex_get_bmc_serial_conf_page_blackout_interval);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-page-blackout-interval)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-serial-conf-call-retry-time", 0, 0, 0, ex_get_bmc_serial_conf_call_retry_time);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-call-retry-time)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-serial-conf-ipmi-msg-comm-settings", 0, 0, 0, ex_get_bmc_serial_conf_ipmi_msg_comm_settings);
  /* 
     syntax in scheme: (fi-get-bmc-serial-conf-ipmi-msg-comm-settings)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-power-restore-policy", 0, 0, 0, ex_get_bmc_power_restore_policy);
  /* 
     syntax in scheme: (fi-get-bmc-power-restore-policy)
  */
  
  scm_c_define_gsubr ("fi-get-bmc-pef-conf-pef-control", 0, 0, 0, ex_get_bmc_pef_conf_pef_control);
  scm_c_define_gsubr ("fi-get-bmc-pef-conf-pef-global-action-control", 0, 0, 0, ex_get_bmc_pef_conf_pef_global_action_control);
  scm_c_define_gsubr ("fi-get-bmc-pef-conf-pef-startup-delay", 0, 0, 0, ex_get_bmc_pef_conf_pef_startup_delay);
  scm_c_define_gsubr ("fi-get-bmc-pef-conf-pef-alert-startup-delay", 0, 0, 0, ex_get_bmc_pef_conf_pef_alert_startup_delay);
  
  scm_c_define_gsubr ("fi-check-bmc-user-password", 2, 0, 0, ex_check_bmc_user_password);
  /* 
     syntax in scheme: (fi-check-bmc-user-password USERID PASSWORD)
  */
  
  scm_c_define_gsubr ("fi-get-sdr-record", 1, 0, 0, ex_get_sdr_record);
  /* 
     syntax in scheme: (fi-get-sdr-record)
  */
  
  scm_c_define_gsubr ("fi-get-sensor-reading", 1, 0, 0, ex_get_sensor_reading);
  /* 
     syntax in scheme: (fi-get-sensor-reading)
  */
  
  scm_c_define_gsubr ("fi-get-sdr-cache-filename", 0, 0, 0, ex_get_sdr_cache_filename);
  /* 
     syntax in scheme: (fi-get-sdr-cache-filename)
  */
  
  scm_c_define_gsubr ("fi-get-sdr-repo-info", 0, 0, 0, ex_get_sdr_repo_info);
  /* 
     syntax in scheme: (fi-get-sdr-repo-info)
  */
  
  scm_c_define_gsubr ("fi-ipmi-open", 1, 0, 0, ex_ipmi_open);
  scm_c_define_gsubr ("fi-ipmi-close", 0, 0, 0, ex_ipmi_close);
  scm_c_define_gsubr ("fi-cmd-get-dev-id-display", 0, 0, 0, ex_cmd_get_dev_id_display);
  scm_c_define_gsubr ("fi-get-pef-info", 0, 0, 0, ex_get_pef_info); 
  
}

// guile initialization area
void
guile_env_init (void)
{
  install_new_variables ();
  install_new_procedures ();
  install_new_hooks ();
}
