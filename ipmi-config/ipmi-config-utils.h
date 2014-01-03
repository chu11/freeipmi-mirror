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

#ifndef IPMI_CONFIG_UTILS_H
#define IPMI_CONFIG_UTILS_H

#include "ipmi-config.h"

struct ipmi_config_section *ipmi_config_find_section (ipmi_config_state_data_t *state_data,
                                                      const char *section_name);

struct ipmi_config_key *ipmi_config_find_key (struct ipmi_config_section *section,
                                              const char *key_name);

struct ipmi_config_keyvalue *ipmi_config_find_keyvalue (struct ipmi_config_section *section,
                                                        const char *key_name);


int ipv4_address_string2int (ipmi_config_state_data_t *state_data,
                             const char *src,
                             uint32_t *dest);

int mac_address_string2int (ipmi_config_state_data_t *state_data,
                            const char *src,
                            uint64_t *dest);

int ipmi_errnum_is_non_fatal (ipmi_config_state_data_t *state_data,
                              fiid_obj_t obj_cmd_rs,
                              ipmi_config_err_t *non_fatal_err);

int ipmi_config_param_errnum_is_non_fatal (ipmi_config_state_data_t *state_data,
                                           fiid_obj_t obj_cmd_rs,
                                           ipmi_config_err_t *non_fatal_err);

int ipmi_config_pstdout_fprintf (ipmi_config_state_data_t *state_data,
                                 FILE *stream,
                                 const char *format, ...);

ipmi_config_err_t load_lan_channel_numbers (ipmi_config_state_data_t *state_data);

ipmi_config_err_t load_serial_channel_numbers (ipmi_config_state_data_t *state_data);

ipmi_config_err_t load_sol_channel_numbers (ipmi_config_state_data_t *state_data);

ipmi_config_err_t get_lan_channel_number (ipmi_config_state_data_t *state_data,
                                          const char *section_name,
                                          uint8_t *channel_number);

ipmi_config_err_t get_serial_channel_number (ipmi_config_state_data_t *state_data,
                                             const char *section_name,
                                             uint8_t *channel_number);

ipmi_config_err_t get_sol_channel_number (ipmi_config_state_data_t *state_data,
                                          const char *section_name,
                                          uint8_t *channel_number);

#endif /* IPMI_CONFIG_UTILS_H */
