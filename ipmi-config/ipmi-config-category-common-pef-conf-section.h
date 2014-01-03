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

#ifndef IPMI_CONFIG_CATEGORY_COMMON_PEF_CONF_SECTION_H
#define IPMI_CONFIG_CATEGORY_COMMON_PEF_CONF_SECTION_H

#include <freeipmi/freeipmi.h>

#include "ipmi-config.h"

ipmi_config_err_t enable_pef_checkout (ipmi_config_state_data_t *state_data,
				       const char *section_name,
                                       struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_pef_commit (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_pef_event_messages_checkout (ipmi_config_state_data_t *state_data,
						      const char *section_name,
                                                      struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_pef_event_messages_commit (ipmi_config_state_data_t *state_data,
						    const char *section_name,
                                                    const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_pef_startup_delay_checkout (ipmi_config_state_data_t *state_data,
						     const char *section_name,
                                                     struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_pef_startup_delay_commit (ipmi_config_state_data_t *state_data,
						   const char *section_name,
                                                   const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_pef_alert_startup_delay_checkout (ipmi_config_state_data_t *state_data,
							   const char *section_name,
                                                           struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_pef_alert_startup_delay_commit (ipmi_config_state_data_t *state_data,
							 const char *section_name,
                                                         const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_alert_action_checkout (ipmi_config_state_data_t *state_data,
						const char *section_name,
                                                struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_alert_action_commit (ipmi_config_state_data_t *state_data,
					      const char *section_name,
                                              const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_power_down_action_checkout (ipmi_config_state_data_t *state_data,
						     const char *section_name,
                                                     struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_power_down_action_commit (ipmi_config_state_data_t *state_data,
						   const char *section_name,
                                                   const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_reset_action_checkout (ipmi_config_state_data_t *state_data,
						const char *section_name,
                                                struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_reset_action_commit (ipmi_config_state_data_t *state_data,
					      const char *section_name,
                                              const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_power_cycle_action_checkout (ipmi_config_state_data_t *state_data,
						      const char *section_name,
                                                      struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_power_cycle_action_commit (ipmi_config_state_data_t *state_data,
						    const char *section_name,
                                                    const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_oem_action_checkout (ipmi_config_state_data_t *state_data,
					      const char *section_name,
                                              struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_oem_action_commit (ipmi_config_state_data_t *state_data,
					    const char *section_name,
                                            const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_diagnostic_interrupt_checkout (ipmi_config_state_data_t *state_data,
							const char *section_name,
                                                        struct ipmi_config_keyvalue *kv);

ipmi_config_err_t enable_diagnostic_interrupt_commit (ipmi_config_state_data_t *state_data,
						      const char *section_name,
                                                      const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t pef_startup_delay_checkout (ipmi_config_state_data_t *state_data,
					      const char *section_name,
                                              struct ipmi_config_keyvalue *kv);

ipmi_config_err_t pef_startup_delay_commit (ipmi_config_state_data_t *state_data,
					    const char *section_name,
                                            const struct ipmi_config_keyvalue *kv);

ipmi_config_err_t pef_alert_startup_delay_checkout (ipmi_config_state_data_t *state_data,
						    const char *section_name,
                                                    struct ipmi_config_keyvalue *kv);

ipmi_config_err_t pef_alert_startup_delay_commit (ipmi_config_state_data_t *state_data,
						  const char *section_name,
                                                  const struct ipmi_config_keyvalue *kv);

#endif /* IPMI_CONFIG_CATEGORY_COMMON_PEF_CONF_SECTION_H */
