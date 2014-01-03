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

#ifndef IPMI_OEM_QUANTA_H
#define IPMI_OEM_QUANTA_H

#include "ipmi-oem.h"

int ipmi_oem_quanta_get_nic_mode (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_set_nic_mode (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_bmc_services (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_set_bmc_services (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_account_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_dns_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_set_dns_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_web_server_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_set_web_server_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_power_management_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_set_power_management_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_set_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_reset_to_defaults (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_get_processor_information (ipmi_oem_state_data_t *state_data);

int ipmi_oem_quanta_read_mac_address (ipmi_oem_state_data_t *state_data);
int ipmi_oem_quanta_write_mac_address (ipmi_oem_state_data_t *state_data);

#endif /* IPMI_OEM_QUANTA_H */
