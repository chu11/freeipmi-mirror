/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef IPMI_OEM_WISTRON_H
#define IPMI_OEM_WISTRON_H

#include "ipmi-oem.h"

int ipmi_oem_wistron_get_system_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_nic_mode (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_nic_mode (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_shared_nic_selection (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_shared_nic_selection (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_bmc_services (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_bmc_services (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_account_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_dns_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_dns_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_web_server_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_web_server_config (ipmi_oem_state_data_t *state_data);

#if 0
/* can't verify - doesn't appear to work */
int ipmi_oem_wistron_get_server_services_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_server_services_config (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_wistron_get_power_management_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_power_management_config (ipmi_oem_state_data_t *state_data);

#if 0
/* can't verify - doesn't appear to work */
int ipmi_oem_wistron_get_firmware_information (ipmi_oem_state_data_t *state_data);
#endif

#if 0
/* can't verify - doesn't appear to work */
int ipmi_oem_wistron_user_default_setting (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_wistron_get_ipv6_settings (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_ipv6_settings (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_ipv6_trap_settings (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_ipv6_trap_settings (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_telnet_redirect_function (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_telnet_redirect_function (ipmi_oem_state_data_t *state_data);

#if 0
/* can't verify - doesn't appear to work */
int ipmi_oem_wistron_get_ssh_redirect_function (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_ssh_redirect_function (ipmi_oem_state_data_t *state_data);
#endif

#if 0
/* can't verify - doesn't appear to work */
int ipmi_oem_wistron_get_chassis_power_readings (ipmi_oem_state_data_t *state_data);
#endif

#if 0
/* can't verify - doesn't appear to work */
int ipmi_oem_wistron_get_chassis_led_status (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_chassis_led_status (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_wistron_get_dhcp_retry (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_dhcp_retry (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_link_status_change_control (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_link_status_change_control (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_set_password_policy (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_read_proprietary_string (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_write_proprietary_string (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_clear_proprietary_string (ipmi_oem_state_data_t *state_data);

#if 0
/* can't verify - doesn't appear to work */
int ipmi_oem_wistron_reset_to_defaults (ipmi_oem_state_data_t *state_data);
#endif

#endif /* IPMI_OEM_WISTRON_H */
