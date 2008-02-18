#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "pef-config.h"
#include "pef-config-validate.h"

#include "config-pef-conf-section.h"

static config_err_t
_enable_pef_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_event_messages_checkout (const char *section_name,
                                     struct config_keyvalue *kv,
                                     void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_event_messages_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_event_messages_commit (const char *section_name,
                                   const struct config_keyvalue *kv,
                                   void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_event_messages_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_startup_delay_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_startup_delay_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_startup_delay_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_startup_delay_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_alert_startup_delay_checkout (const char *section_name,
                                          struct config_keyvalue *kv,
                                          void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_alert_startup_delay_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_alert_startup_delay_commit (const char *section_name,
                                        const struct config_keyvalue *kv,
                                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_pef_alert_startup_delay_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_alert_action_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_alert_action_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_alert_action_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_alert_action_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_down_action_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_power_down_action_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_down_action_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_power_down_action_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_reset_action_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_reset_action_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_reset_action_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_reset_action_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_cycle_action_checkout (const char *section_name,
                                     struct config_keyvalue *kv,
                                     void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_power_cycle_action_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_cycle_action_commit (const char *section_name,
                                   const struct config_keyvalue *kv,
                                   void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_power_cycle_action_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_oem_action_checkout (const char *section_name,
                             struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_oem_action_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_oem_action_commit (const char *section_name,
                           const struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_oem_action_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_diagnostic_interrupt_checkout (const char *section_name,
                                       struct config_keyvalue *kv,
                                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_diagnostic_interrupt_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_diagnostic_interrupt_commit (const char *section_name,
                                     const struct config_keyvalue *kv,
                                     void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return enable_diagnostic_interrupt_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_startup_delay_checkout (const char *section_name,
                             struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return pef_startup_delay_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_startup_delay_commit (const char *section_name,
                           const struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return pef_startup_delay_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_alert_startup_delay_checkout (const char *section_name,
                                   struct config_keyvalue *kv,
                                   void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return pef_alert_startup_delay_checkout(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_alert_startup_delay_commit (const char *section_name,
                                 const struct config_keyvalue *kv,
                                 void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return pef_alert_startup_delay_commit(kv, state_data->ipmi_ctx, &(state_data->prog_data->args->config_args));
}

struct config_section *
pef_config_pef_conf_section_get (pef_config_state_data_t *state_data)
{
  struct config_section *pef_section;

  if (!(pef_section = config_section_create ("PEF_Conf",
                                             NULL,
                                             NULL,
                                             0)))
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF",
                              "Possible values: Yes/No",
                              0,
                              _enable_pef_checkout,
                              _enable_pef_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Event_Messages",
                              "Possible values: Yes/No",
                              0,
                              _enable_pef_event_messages_checkout,
                              _enable_pef_event_messages_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Startup_Delay",
                              "Possible values: Yes/No",
                              0,
                              _enable_pef_startup_delay_checkout,
                              _enable_pef_startup_delay_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Alert_Startup_Delay",
                              "Possible values: Yes/No",
                              0,
                              _enable_pef_alert_startup_delay_checkout,
                              _enable_pef_alert_startup_delay_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;
  
  if (config_section_add_key (pef_section,
                              "Enable_Alert_Action",
                              "Possible values: Yes/No",
                              0,
                              _enable_alert_action_checkout,
                              _enable_alert_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Power_Down_Action",
                              "Possible values: Yes/No",
                              0,
                              _enable_power_down_action_checkout,
                              _enable_power_down_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Reset_Action",
                              "Possible values: Yes/No",
                              0,
                              _enable_reset_action_checkout,
                              _enable_reset_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Power_Cycle_Action",
                              "Possible values: Yes/No",
                              0,
                              _enable_power_cycle_action_checkout,
                              _enable_power_cycle_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_OEM_Action",
                              "Possible values: Yes/No",
                              0,
                              _enable_oem_action_checkout,
                              _enable_oem_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Diagnostic_Interrupt",
                              "Possible values: Yes/No",
                              0,
                              _enable_diagnostic_interrupt_checkout,
                              _enable_diagnostic_interrupt_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Startup_Delay",
                              "Give value in seconds",
                              0,
                              _pef_startup_delay_checkout,
                              _pef_startup_delay_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Alert_Startup_Delay",
                              "Give value in seconds",
                              0,
                              _pef_alert_startup_delay_checkout,
                              _pef_alert_startup_delay_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return pef_section;

 cleanup:
  if (pef_section)
    config_section_destroy(pef_section);
  return NULL;
}

