/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifndef IPMI_CONFIG_MAP_H
#define IPMI_CONFIG_MAP_H

#include "ipmi-config.h"

int channel_access_mode (const char *string);

char *channel_access_mode_string (uint8_t value);

uint8_t get_privilege_limit_number (const char *string);

char *get_privilege_limit_string (uint8_t value);

int privilege_level_number (const char *string);

char *privilege_level_string (uint8_t value);

int rmcpplus_priv_number (const char *string);

char *rmcpplus_priv_string (int value);

int ip_address_source_number (const char *source);

char *ip_address_source_string (uint8_t source);

int power_restore_policy_number (const char *string);

char *power_restore_policy_string (uint8_t value);

int connect_mode_number (const char *string);

char *connect_mode_string (uint8_t value);

int flow_control_number (const char *string);

char *flow_control_string (uint8_t value);

int bit_rate_number (const char *string);

char * bit_rate_string (uint8_t value);

int sol_bit_rate_number (const char *string);

char *sol_bit_rate_string (uint8_t value);

int alert_destination_type_number (const char *string);

char *alert_destination_type_string (uint8_t value);

int alert_gateway_number (const char *string);

char *alert_gateway_string (uint8_t value);

int bios_boot_type_number (const char *string);

char *bios_boot_type_string (uint8_t value);

int boot_device_number (const char *string);

char *boot_device_string (uint8_t value);

int device_instance_selector_number (const char *string);

char *device_instance_selector_string (uint8_t value);

int firmware_bios_verbosity_number (const char *string);

char *firmware_bios_verbosity_string (uint8_t value);

int console_redirection_number (const char *string);

char *console_redirection_string (uint8_t value);

int policy_type_number (const char *string);

char *policy_type_string (uint8_t value);

int filter_type_number (const char *string);

char *filter_type_string (uint8_t value);

int event_severity_number (const char *string);

char *event_severity_string (uint8_t value);

int sensor_type_number (const char *string);

char *sensor_type_string (uint8_t value);

int exception_actions_number (const char *source);

char *exception_actions_string (uint8_t source);

#endif /* IPMI_CONFIG_MAP_H */
