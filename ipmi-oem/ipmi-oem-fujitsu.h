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

#ifndef IPMI_OEM_FUJITSU_H
#define IPMI_OEM_FUJITSU_H

#include "ipmi-oem.h"

int ipmi_oem_fujitsu_get_power_on_source (ipmi_oem_state_data_t *state_data);

int ipmi_oem_fujitsu_get_power_off_source (ipmi_oem_state_data_t *state_data);

int ipmi_oem_fujitsu_get_remote_storage_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_fujitsu_get_system_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_fujitsu_get_eeprom_version_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_fujitsu_get_identify_led (ipmi_oem_state_data_t *state_data);
int ipmi_oem_fujitsu_set_identify_led (ipmi_oem_state_data_t *state_data);

int ipmi_oem_fujitsu_get_error_led (ipmi_oem_state_data_t *state_data);

int ipmi_oem_fujitsu_get_sel_entry_long_text (ipmi_oem_state_data_t *state_data);

#endif /* IPMI_OEM_FUJITSU_H */
