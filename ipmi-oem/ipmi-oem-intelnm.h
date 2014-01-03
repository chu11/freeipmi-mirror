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

#ifndef IPMI_OEM_INTELNM_H
#define IPMI_OEM_INTELNM_H

#include "ipmi-oem.h"

int ipmi_oem_intelnm_get_node_manager_statistics (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_reset_node_manager_statistics (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_get_node_manager_capabilities (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_node_manager_policy_control (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_get_node_manager_policy (ipmi_oem_state_data_t *state_data);
int ipmi_oem_intelnm_set_node_manager_policy (ipmi_oem_state_data_t *state_data);
int ipmi_oem_intelnm_remove_node_manager_policy (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_get_node_manager_alert_thresholds (ipmi_oem_state_data_t *state_data);
int ipmi_oem_intelnm_set_node_manager_alert_thresholds (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_get_node_manager_policy_suspend_periods (ipmi_oem_state_data_t *state_data);
int ipmi_oem_intelnm_set_node_manager_policy_suspend_periods (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_set_node_manager_power_draw_range (ipmi_oem_state_data_t *state_data);

#if 0
/* can't verify */
int ipmi_oem_intelnm_get_limiting_policy_id (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_intelnm_get_node_manager_alert_destination (ipmi_oem_state_data_t *state_data);
int ipmi_oem_intelnm_set_node_manager_alert_destination (ipmi_oem_state_data_t *state_data);

int ipmi_oem_intelnm_get_node_manager_version (ipmi_oem_state_data_t *state_data);

#endif /* IPMI_OEM_INTELNM_H */
