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

#ifndef IPMI_CONFIG_VALIDATE_H
#define IPMI_CONFIG_VALIDATE_H

#include "ipmi-config.h"

ipmi_config_validate_t yes_no_validate (ipmi_config_state_data_t *state_data,
					const char *section_name,
                                        const char *key_name,
                                        const char *value);

ipmi_config_validate_t check_number_range (const char *value,
                                           int min,
                                           int max);

ipmi_config_validate_t check_number_range_unsigned (const char *value,
						    unsigned int min,
						    unsigned int max);

ipmi_config_validate_t number_range_three_bits_validate (ipmi_config_state_data_t *state_data,
							 const char *section_name,
                                                         const char *key_name,
                                                         const char *value);

ipmi_config_validate_t number_range_four_bits_validate (ipmi_config_state_data_t *state_data,
							const char *section_name,
                                                        const char *key_name,
                                                        const char *value);

ipmi_config_validate_t number_range_seven_bits_validate (ipmi_config_state_data_t *state_data,
							 const char *section_name,
                                                         const char *key_name,
                                                         const char *value);

ipmi_config_validate_t number_range_twelve_bits_validate (ipmi_config_state_data_t *state_data,
							  const char *section_name,
                                                          const char *key_name,
                                                          const char *value);

ipmi_config_validate_t number_range_one_byte_validate (ipmi_config_state_data_t *state_data,
						       const char *section_name,
                                                       const char *key_name,
                                                       const char *value);

ipmi_config_validate_t number_range_one_byte_non_zero_validate (ipmi_config_state_data_t *state_data,
								const char *section_name,
                                                                const char *key_name,
                                                                const char *value);

ipmi_config_validate_t number_range_two_bytes_validate (ipmi_config_state_data_t *state_data,
							const char *section_name,
                                                        const char *key_name,
                                                        const char *value);

ipmi_config_validate_t number_range_four_bytes_validate (ipmi_config_state_data_t *state_data,
							 const char *section_name,
							 const char *key_name,
							 const char *value);

ipmi_config_validate_t ip_address_validate (ipmi_config_state_data_t *state_data,
					    const char *section_name,
                                            const char *key_name,
                                            const char *value);

ipmi_config_validate_t mac_address_validate (ipmi_config_state_data_t *state_data,
					     const char *section_name,
                                             const char *key_name,
                                             const char *value);

ipmi_config_validate_t channel_access_mode_validate (ipmi_config_state_data_t *state_data,
						     const char *section_name,
                                                     const char *key_name,
                                                     const char *value);

ipmi_config_validate_t get_privilege_limit_number_validate (ipmi_config_state_data_t *state_data,
							    const char *section_name,
                                                            const char *key_name,
                                                            const char *value);

ipmi_config_validate_t privilege_level_number_validate (ipmi_config_state_data_t *state_data,
							const char *section_name,
                                                        const char *key_name,
                                                        const char *value);

ipmi_config_validate_t rmcpplus_priv_number_validate (ipmi_config_state_data_t *state_data,
						      const char *section_name,
                                                      const char *key_name,
                                                      const char *value);

ipmi_config_validate_t ip_address_source_number_validate (ipmi_config_state_data_t *state_data,
							  const char *section_name,
                                                          const char *key_name,
                                                          const char *value);

ipmi_config_validate_t power_restore_policy_number_validate (ipmi_config_state_data_t *state_data,
							     const char *section_name,
                                                             const char *key_name,
                                                             const char *value);

ipmi_config_validate_t connect_mode_number_validate (ipmi_config_state_data_t *state_data,
						     const char *section_name,
                                                     const char *key_name,
                                                     const char *value);

ipmi_config_validate_t flow_control_number_validate (ipmi_config_state_data_t *state_data,
						     const char *section_name,
                                                     const char *key_name,
                                                     const char *value);

ipmi_config_validate_t bit_rate_number_validate (ipmi_config_state_data_t *state_data,
						 const char *section_name,
                                                 const char *key_name,
                                                 const char *value);

ipmi_config_validate_t sol_bit_rate_number_validate (ipmi_config_state_data_t *state_data,
						     const char *section_name,
                                                     const char *key_name,
                                                     const char *value);

ipmi_config_validate_t alert_destination_type_number_validate (ipmi_config_state_data_t *state_data,
							       const char *section_name,
                                                               const char *key_name,
                                                               const char *value);

ipmi_config_validate_t alert_gateway_number_validate (ipmi_config_state_data_t *state_data,
						      const char *section_name,
                                                      const char *key_name,
                                                      const char *value);

ipmi_config_validate_t bios_boot_type_number_validate (ipmi_config_state_data_t *state_data,
						       const char *section_name,
						       const char *key_name,
						       const char *value);

ipmi_config_validate_t boot_device_number_validate (ipmi_config_state_data_t *state_data,
						    const char *section_name,
						    const char *key_name,
						    const char *value);

ipmi_config_validate_t device_instance_selector_number_validate (ipmi_config_state_data_t *state_data,
								 const char *section_name,
								 const char *key_name,
								 const char *value);

ipmi_config_validate_t firmware_bios_verbosity_number_validate (ipmi_config_state_data_t *state_data,
								const char *section_name,
								const char *key_name,
								const char *value);

ipmi_config_validate_t console_redirection_number_validate (ipmi_config_state_data_t *state_data,
							    const char *section_name,
							    const char *key_name,
							    const char *value);

ipmi_config_validate_t alert_destination_type_validate (ipmi_config_state_data_t *state_data,
							const char *section_name,
                                                   const char *key_name,
                                                   const char *value);

ipmi_config_validate_t alert_gateway_validate (ipmi_config_state_data_t *state_data,
					       const char *section_name,
					       const char *key_name,
					       const char *value);

ipmi_config_validate_t policy_type_validate (ipmi_config_state_data_t *state_data,
					     const char *section_name,
					     const char *key_name,
					     const char *value);

ipmi_config_validate_t filter_type_validate (ipmi_config_state_data_t *state_data,
					     const char *section_name,
					     const char *key_name,
					     const char *value);

ipmi_config_validate_t event_severity_validate (ipmi_config_state_data_t *state_data,
						const char *section_name,
						const char *key_name,
						const char *value);

ipmi_config_validate_t sensor_type_validate (ipmi_config_state_data_t *state_data,
					     const char *section_name,
					     const char *key_name,
					     const char *value);

ipmi_config_validate_t exception_actions_validate (ipmi_config_state_data_t *state_data,
						   const char *section_name,
						   const char *key_name,
						   const char *value);

#endif /* IPMI_CONFIG_VALIDATE_H */
