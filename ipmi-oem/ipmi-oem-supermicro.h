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

#ifndef IPMI_OEM_SUPERMICRO_H
#define IPMI_OEM_SUPERMICRO_H

#include "ipmi-oem.h"

int ipmi_oem_supermicro_extra_firmware_info (ipmi_oem_state_data_t *state_data);

int ipmi_oem_supermicro_reset_intrusion (ipmi_oem_state_data_t *state_data);

int ipmi_oem_supermicro_get_bmc_services_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_supermicro_set_bmc_services_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_supermicro_get_power_supply_status (ipmi_oem_state_data_t *state_data);

int ipmi_oem_supermicro_get_power_supply_status2 (ipmi_oem_state_data_t *state_data);

int ipmi_oem_supermicro_get_pmbus_power_supply_status (ipmi_oem_state_data_t *state_data);

#endif /* IPMI_OEM_SUPERMICRO_H */
