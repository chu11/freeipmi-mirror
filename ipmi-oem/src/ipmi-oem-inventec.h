/*
 * Copyright (C) 2008-2010 FreeIPMI Core Team
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

#ifndef _IPMI_OEM_INVENTEC_H
#define _IPMI_OEM_INVENTEC_H

#include "ipmi-oem.h"

int ipmi_oem_inventec_get_nic_mode (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_nic_mode (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_get_mac_address (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_mac_address (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_bmc_services (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_get_authentication_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_authentication_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_get_account_status (ipmi_oem_state_data_t *state_data);

#if 0
/* basics appear to work, but untested due to other infracture/information needed */
int ipmi_oem_inventec_get_dns_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_dns_config (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_inventec_get_web_server_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_web_server_config (ipmi_oem_state_data_t *state_data);

int ipmi_oem_inventec_get_power_management_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_power_management_config (ipmi_oem_state_data_t *state_data);

#if 0
/* waiting for verification from Dell */
int ipmi_oem_inventec_get_firmware_update_config (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_set_firmware_update_config (ipmi_oem_state_data_t *state_data);
#endif

#if 0
/* cannot verify */
int ipmi_oem_inventec_get_firmware_information (ipmi_oem_state_data_t *state_data);
#endif

#if 0
/* waiting for verification from Dell */
int ipmi_oem_inventec_update_firmware (ipmi_oem_state_data_t *state_data);
#endif

#if 0
/* cannot verify */
int ipmi_oem_inventec_restore_to_defaults (ipmi_oem_state_data_t *state_data);
#endif

#if 0
/* cannot verify */
int ipmi_oem_inventec_set_system_guid (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_inventec_read_eeprom (ipmi_oem_state_data_t *state_data);
int ipmi_oem_inventec_clear_eeprom (ipmi_oem_state_data_t *state_data);

#endif
