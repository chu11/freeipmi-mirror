#ifndef _CONFIG_PEF_CONF_SECTION_H_
#define _CONFIG_PEF_CONF_SECTION_H_

#include <freeipmi/freeipmi.h>
#include <freeipmi/api/api.h>

#include "config-common.h"

config_err_t enable_pef_checkout (struct config_keyvalue *kv,
                                  ipmi_ctx_t ipmi_ctx,
                                  struct config_arguments *cmd_args);

config_err_t enable_pef_commit (const struct config_keyvalue *kv,
                                ipmi_ctx_t ipmi_ctx,
                                struct config_arguments *cmd_args);

config_err_t enable_pef_event_messages_checkout (struct config_keyvalue *kv,
                                                 ipmi_ctx_t ipmi_ctx,
                                                 struct config_arguments *cmd_args);

config_err_t enable_pef_event_messages_commit (const struct config_keyvalue *kv,
                                               ipmi_ctx_t ipmi_ctx,
                                               struct config_arguments *cmd_args);

config_err_t enable_pef_startup_delay_checkout (struct config_keyvalue *kv,
                                                ipmi_ctx_t ipmi_ctx,
                                                struct config_arguments *cmd_args);

config_err_t enable_pef_startup_delay_commit (const struct config_keyvalue *kv,
                                              ipmi_ctx_t ipmi_ctx,
                                              struct config_arguments *cmd_args);

config_err_t enable_pef_alert_startup_delay_checkout (struct config_keyvalue *kv,
                                                      ipmi_ctx_t ipmi_ctx,
                                                      struct config_arguments *cmd_args);

config_err_t enable_pef_alert_startup_delay_commit (const struct config_keyvalue *kv,
                                                    ipmi_ctx_t ipmi_ctx,
                                                    struct config_arguments *cmd_args);

config_err_t enable_alert_action_checkout (struct config_keyvalue *kv,
                                           ipmi_ctx_t ipmi_ctx,
                                           struct config_arguments *cmd_args);

config_err_t enable_alert_action_commit (const struct config_keyvalue *kv,
                                         ipmi_ctx_t ipmi_ctx,
                                         struct config_arguments *cmd_args);

config_err_t enable_power_down_action_checkout (struct config_keyvalue *kv,
                                                ipmi_ctx_t ipmi_ctx,
                                                struct config_arguments *cmd_args);

config_err_t enable_power_down_action_commit (const struct config_keyvalue *kv,
                                              ipmi_ctx_t ipmi_ctx,
                                              struct config_arguments *cmd_args);

config_err_t enable_reset_action_checkout (struct config_keyvalue *kv,
                                           ipmi_ctx_t ipmi_ctx,
                                           struct config_arguments *cmd_args);

config_err_t enable_reset_action_commit (const struct config_keyvalue *kv,
                                         ipmi_ctx_t ipmi_ctx,
                                         struct config_arguments *cmd_args);

config_err_t enable_power_cycle_action_checkout (struct config_keyvalue *kv,
                                                 ipmi_ctx_t ipmi_ctx,
                                                 struct config_arguments *cmd_args);

config_err_t enable_power_cycle_action_commit (const struct config_keyvalue *kv,
                                               ipmi_ctx_t ipmi_ctx,
                                               struct config_arguments *cmd_args);

config_err_t enable_oem_action_checkout (struct config_keyvalue *kv,
                                         ipmi_ctx_t ipmi_ctx,
                                         struct config_arguments *cmd_args);

config_err_t enable_oem_action_commit (const struct config_keyvalue *kv,
                                       ipmi_ctx_t ipmi_ctx,
                                       struct config_arguments *cmd_args);

config_err_t enable_diagnostic_interrupt_checkout (struct config_keyvalue *kv,
                                                   ipmi_ctx_t ipmi_ctx,
                                                   struct config_arguments *cmd_args);

config_err_t enable_diagnostic_interrupt_commit (const struct config_keyvalue *kv,
                                                 ipmi_ctx_t ipmi_ctx,
                                                 struct config_arguments *cmd_args);

config_err_t pef_startup_delay_checkout (struct config_keyvalue *kv,
                                         ipmi_ctx_t ipmi_ctx,
                                         struct config_arguments *cmd_args);

config_err_t pef_startup_delay_commit (const struct config_keyvalue *kv,
                                       ipmi_ctx_t ipmi_ctx,
                                       struct config_arguments *cmd_args);

config_err_t pef_alert_startup_delay_checkout (struct config_keyvalue *kv,
                                               ipmi_ctx_t ipmi_ctx,
                                               struct config_arguments *cmd_args);

config_err_t pef_alert_startup_delay_commit (const struct config_keyvalue *kv,
                                             ipmi_ctx_t ipmi_ctx,
                                             struct config_arguments *cmd_args);

#endif /* _CONFIG_PEF_CONF_SECTION_H_ */
