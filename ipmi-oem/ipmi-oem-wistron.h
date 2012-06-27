/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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

int ipmi_oem_wistron_get_server_services_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_server_services_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_telnet_redirect_function (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_telnet_redirect_function (ipmi_oem_state_data_t *state_data);

int ipmi_oem_wistron_get_ssh_redirect_function (ipmi_oem_state_data_t *state_data);
int ipmi_oem_wistron_set_ssh_redirect_function (ipmi_oem_state_data_t *state_data);

#endif /* IPMI_OEM_WISTRON_H */
