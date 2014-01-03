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

#ifndef IPMI_OEM_THIRDPARTY_H
#define IPMI_OEM_THIRDPARTY_H

#include "ipmi-oem.h"

/* Common functions for OEM extensions shared between multiple vendors
 * b/c they share a common third party firmware
 */

/* Shared between:
 * Wistron/Dell Poweredge C6220
 *
 * String format is:
 *
 * Set Selector 0:
 *
 * 1st byte = set selector
 * 2nd byte
 * - 7:4 - reserved
 * - 3:0 - string encoding, 0 = printable ascii  
 * 3rd byte = string length
 * ? bytes = string
 *
 * Set Selector > 0
 *
 * 1st byte = set selector
 * ? bytes = string
 */
int ipmi_oem_thirdparty_get_system_info_block_pstring (ipmi_oem_state_data_t *state_data,
						       uint8_t parameter_selector,
						       char *string,
						       unsigned int string_len);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_get_extended_config_value (ipmi_oem_state_data_t *state_data,
						   uint8_t configuration_id,
						   uint8_t attribute_id,
						   uint8_t index,
						   unsigned int value_return_length,
						   uint32_t *value);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_get_extended_config_string (ipmi_oem_state_data_t *state_data,
						    uint8_t configuration_id,
						    uint8_t attribute_id,
						    uint8_t index,
						    char *buf,
						    unsigned int buflen);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_set_extended_config_value (ipmi_oem_state_data_t *state_data,
						   uint8_t configuration_id,
						   uint8_t attribute_id,
						   uint8_t index,
						   unsigned int value_length,
						   uint32_t value);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_set_extended_config_string (ipmi_oem_state_data_t *state_data,
						    uint8_t configuration_id,
						    uint8_t attribute_id,
						    uint8_t index,
						    char *buf,
						    unsigned int buflen);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_get_nic_mode (ipmi_oem_state_data_t *state_data);
int ipmi_oem_thirdparty_set_nic_mode (ipmi_oem_state_data_t *state_data);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_get_bmc_services_bitmask (ipmi_oem_state_data_t *state_data,
						  uint8_t *services);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 */
int ipmi_oem_thirdparty_get_bmc_services_v1 (ipmi_oem_state_data_t *state_data);
int ipmi_oem_thirdparty_set_bmc_services_v1 (ipmi_oem_state_data_t *state_data);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_get_account_status (ipmi_oem_state_data_t *state_data);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 */
int ipmi_oem_thirdparty_get_dns_config_v1 (ipmi_oem_state_data_t *state_data);
int ipmi_oem_thirdparty_set_dns_config_v1 (ipmi_oem_state_data_t *state_data);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 * Wistron/Dell Poweredge C6220
 */
int ipmi_oem_thirdparty_get_web_server_config_v1 (ipmi_oem_state_data_t *state_data);
int ipmi_oem_thirdparty_set_web_server_config_v1 (ipmi_oem_state_data_t *state_data);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 */
int ipmi_oem_thirdparty_get_power_management_config_v1 (ipmi_oem_state_data_t *state_data);
int ipmi_oem_thirdparty_set_power_management_config_v1 (ipmi_oem_state_data_t *state_data);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 */
int ipmi_oem_thirdparty_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data);
int ipmi_oem_thirdparty_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data);

/* Shared between:
 * Inventec 5441/Dell Xanadu II OEM
 * Inventec 5442/Dell Xanadu III OEM
 * Quanta S99Q/Dell FS12-TY OEM
 */
int ipmi_oem_thirdparty_get_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data);
int ipmi_oem_thirdparty_set_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data);

#endif /* IPMI_OEM_THIRDPARTY_H */
