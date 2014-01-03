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

#ifndef IPMI_OEM_DELL_H
#define IPMI_OEM_DELL_H

#include "ipmi-oem.h"

int ipmi_oem_dell_get_system_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_nic_selection (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_nic_selection (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_nic_selection_failover (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_nic_selection_failover (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_active_lom_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_ssh_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_ssh_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_telnet_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_telnet_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_web_server_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_web_server_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_active_directory_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_active_directory_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_reset_to_defaults (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_power_consumption_data (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_reset_power_consumption_data (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_power_supply_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_instantaneous_power_consumption_data (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_power_head_room (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_power_consumption_statistics (ipmi_oem_state_data_t *state_data);

/* legacy */
int ipmi_oem_dell_get_average_power_history (ipmi_oem_state_data_t *state_data);

/* legacy */
int ipmi_oem_dell_get_peak_power_history (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_power_capacity (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_power_capacity (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_power_capacity_status (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_set_power_capacity_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_chassis_identify_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_power_monitoring_over_interval (ipmi_oem_state_data_t *state_data);
int ipmi_oem_dell_power_monitoring_interval_range (ipmi_oem_state_data_t *state_data);

#if 0
/* cannot verify */
int ipmi_oem_dell_get_blade_slot_id (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_dell_get_last_post_code (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_slot_power_toggle (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_slot_power_control (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_get_port_map (ipmi_oem_state_data_t *state_data);

int ipmi_oem_dell_set_port_map (ipmi_oem_state_data_t *state_data);

#endif /* IPMI_OEM_DELL_H */
