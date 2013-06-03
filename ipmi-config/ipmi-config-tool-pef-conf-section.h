/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

#ifndef IPMI_CONFIG_TOOL_PEF_CONF_SECTION_H
#define IPMI_CONFIG_TOOL_PEF_CONF_SECTION_H

#include <freeipmi/freeipmi.h>

#include "ipmi-config.h"
#include "pstdout.h"

ipmi_config_err_t enable_pef_checkout (pstdout_state_t pstate,
                                       struct ipmi_config_keyvalue *kv,
                                       ipmi_ctx_t ipmi_ctx,
                                       struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_pef_commit (pstdout_state_t pstate,
                                     const struct ipmi_config_keyvalue *kv,
                                     ipmi_ctx_t ipmi_ctx,
                                     struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_pef_event_messages_checkout (pstdout_state_t pstate,
                                                      struct ipmi_config_keyvalue *kv,
                                                      ipmi_ctx_t ipmi_ctx,
                                                      struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_pef_event_messages_commit (pstdout_state_t pstate,
                                                    const struct ipmi_config_keyvalue *kv,
                                                    ipmi_ctx_t ipmi_ctx,
                                                    struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_pef_startup_delay_checkout (pstdout_state_t pstate,
                                                     struct ipmi_config_keyvalue *kv,
                                                     ipmi_ctx_t ipmi_ctx,
                                                     struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_pef_startup_delay_commit (pstdout_state_t pstate,
                                                   const struct ipmi_config_keyvalue *kv,
                                                   ipmi_ctx_t ipmi_ctx,
                                                   struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_pef_alert_startup_delay_checkout (pstdout_state_t pstate,
                                                           struct ipmi_config_keyvalue *kv,
                                                           ipmi_ctx_t ipmi_ctx,
                                                           struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_pef_alert_startup_delay_commit (pstdout_state_t pstate,
                                                         const struct ipmi_config_keyvalue *kv,
                                                         ipmi_ctx_t ipmi_ctx,
                                                         struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_alert_action_checkout (pstdout_state_t pstate,
                                                struct ipmi_config_keyvalue *kv,
                                                ipmi_ctx_t ipmi_ctx,
                                                struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_alert_action_commit (pstdout_state_t pstate,
                                              const struct ipmi_config_keyvalue *kv,
                                              ipmi_ctx_t ipmi_ctx,
                                              struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_power_down_action_checkout (pstdout_state_t pstate,
                                                     struct ipmi_config_keyvalue *kv,
                                                     ipmi_ctx_t ipmi_ctx,
                                                     struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_power_down_action_commit (pstdout_state_t pstate,
                                                   const struct ipmi_config_keyvalue *kv,
                                                   ipmi_ctx_t ipmi_ctx,
                                                   struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_reset_action_checkout (pstdout_state_t pstate,
                                                struct ipmi_config_keyvalue *kv,
                                                ipmi_ctx_t ipmi_ctx,
                                                struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_reset_action_commit (pstdout_state_t pstate,
                                              const struct ipmi_config_keyvalue *kv,
                                              ipmi_ctx_t ipmi_ctx,
                                              struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_power_cycle_action_checkout (pstdout_state_t pstate,
                                                      struct ipmi_config_keyvalue *kv,
                                                      ipmi_ctx_t ipmi_ctx,
                                                      struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_power_cycle_action_commit (pstdout_state_t pstate,
                                                    const struct ipmi_config_keyvalue *kv,
                                                    ipmi_ctx_t ipmi_ctx,
                                                    struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_oem_action_checkout (pstdout_state_t pstate,
                                              struct ipmi_config_keyvalue *kv,
                                              ipmi_ctx_t ipmi_ctx,
                                              struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_oem_action_commit (pstdout_state_t pstate,
                                            const struct ipmi_config_keyvalue *kv,
                                            ipmi_ctx_t ipmi_ctx,
                                            struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_diagnostic_interrupt_checkout (pstdout_state_t pstate,
                                                        struct ipmi_config_keyvalue *kv,
                                                        ipmi_ctx_t ipmi_ctx,
                                                        struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t enable_diagnostic_interrupt_commit (pstdout_state_t pstate,
                                                      const struct ipmi_config_keyvalue *kv,
                                                      ipmi_ctx_t ipmi_ctx,
                                                      struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t pef_startup_delay_checkout (pstdout_state_t pstate,
                                              struct ipmi_config_keyvalue *kv,
                                              ipmi_ctx_t ipmi_ctx,
                                              struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t pef_startup_delay_commit (pstdout_state_t pstate,
                                            const struct ipmi_config_keyvalue *kv,
                                            ipmi_ctx_t ipmi_ctx,
                                            struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t pef_alert_startup_delay_checkout (pstdout_state_t pstate,
                                                    struct ipmi_config_keyvalue *kv,
                                                    ipmi_ctx_t ipmi_ctx,
                                                    struct ipmi_config_arguments *cmd_args);

ipmi_config_err_t pef_alert_startup_delay_commit (pstdout_state_t pstate,
                                                  const struct ipmi_config_keyvalue *kv,
                                                  ipmi_ctx_t ipmi_ctx,
                                                  struct ipmi_config_arguments *cmd_args);

#endif /* IPMI_CONFIG_TOOL_PEF_CONF_SECTION_H */
