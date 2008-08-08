/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-validate.h"

#include "freeipmi-portability.h"
#include "config-tool-pef-conf-section.h"

static config_err_t
_enable_pef_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_checkout(state_data->pstate,
                             kv,
                             state_data->ipmi_ctx, 
                             &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_commit(state_data->pstate,
                           kv, 
                           state_data->ipmi_ctx, 
                           &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_event_messages_checkout (const char *section_name,
                                     struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_event_messages_checkout(state_data->pstate,
                                            kv, 
                                            state_data->ipmi_ctx, 
                                            &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_event_messages_commit (const char *section_name,
                                   const struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_event_messages_commit(state_data->pstate,
                                          kv,
                                          state_data->ipmi_ctx, 
                                          &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_startup_delay_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_startup_delay_checkout(state_data->pstate,
                                           kv, 
                                           state_data->ipmi_ctx, 
                                           &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_startup_delay_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_startup_delay_commit(state_data->pstate,
                                         kv, 
                                         state_data->ipmi_ctx, 
                                         &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_alert_startup_delay_checkout (const char *section_name,
                                          struct config_keyvalue *kv,
                                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_alert_startup_delay_checkout(state_data->pstate,
                                                 kv,
                                                 state_data->ipmi_ctx,
                                                 &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_pef_alert_startup_delay_commit (const char *section_name,
                                        const struct config_keyvalue *kv,
                                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_pef_alert_startup_delay_commit(state_data->pstate,
                                               kv, 
                                               state_data->ipmi_ctx, 
                                               &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_alert_action_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_alert_action_checkout(state_data->pstate,
                                      kv,
                                      state_data->ipmi_ctx,
                                      &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_alert_action_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_alert_action_commit(state_data->pstate,
                                    kv,
                                    state_data->ipmi_ctx,
                                    &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_down_action_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_power_down_action_checkout(state_data->pstate,
                                           kv, 
                                           state_data->ipmi_ctx, 
                                           &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_down_action_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_power_down_action_commit(state_data->pstate,
                                         kv, 
                                         state_data->ipmi_ctx, 
                                         &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_reset_action_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_reset_action_checkout(state_data->pstate,
                                      kv, 
                                      state_data->ipmi_ctx, 
                                      &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_reset_action_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_reset_action_commit(state_data->pstate,
                                    kv,
                                    state_data->ipmi_ctx,
                                    &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_cycle_action_checkout (const char *section_name,
                                     struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_power_cycle_action_checkout(state_data->pstate,
                                            kv,
                                            state_data->ipmi_ctx, 
                                            &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_power_cycle_action_commit (const char *section_name,
                                   const struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_power_cycle_action_commit(state_data->pstate,
                                          kv,
                                          state_data->ipmi_ctx, 
                                          &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_oem_action_checkout (const char *section_name,
                             struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_oem_action_checkout(state_data->pstate,
                                    kv,
                                    state_data->ipmi_ctx, 
                                    &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_oem_action_commit (const char *section_name,
                           const struct config_keyvalue *kv,
                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_oem_action_commit(state_data->pstate,
                                  kv,
                                  state_data->ipmi_ctx, 
                                  &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_diagnostic_interrupt_checkout (const char *section_name,
                                       struct config_keyvalue *kv,
                                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_diagnostic_interrupt_checkout(state_data->pstate,
                                              kv, 
                                              state_data->ipmi_ctx, 
                                              &(state_data->prog_data->args->config_args));
}

static config_err_t
_enable_diagnostic_interrupt_commit (const char *section_name,
                                     const struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return enable_diagnostic_interrupt_commit(state_data->pstate,
                                            kv, 
                                            state_data->ipmi_ctx, 
                                            &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_startup_delay_checkout (const char *section_name,
                             struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return pef_startup_delay_checkout(state_data->pstate,
                                    kv,
                                    state_data->ipmi_ctx, 
                                    &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_startup_delay_commit (const char *section_name,
                           const struct config_keyvalue *kv,
                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return pef_startup_delay_commit(state_data->pstate,
                                  kv,
                                  state_data->ipmi_ctx, 
                                  &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_alert_startup_delay_checkout (const char *section_name,
                                   struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return pef_alert_startup_delay_checkout(state_data->pstate,
                                          kv, 
                                          state_data->ipmi_ctx, 
                                          &(state_data->prog_data->args->config_args));
}

static config_err_t
_pef_alert_startup_delay_commit (const char *section_name,
                                 const struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return pef_alert_startup_delay_commit(state_data->pstate,
                                        kv, 
                                        state_data->ipmi_ctx,
                                        &(state_data->prog_data->args->config_args));
}

struct config_section *
bmc_config_pef_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *pef_section;

  if (!(pef_section = config_section_create (state_data->pstate,
                                             "PEF_Conf",
                                             NULL,
                                             NULL,
                                             CONFIG_DO_NOT_CHECKOUT,
                                             NULL,
                                             NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_PEF",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_pef_checkout,
                              _enable_pef_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_PEF_Event_Messages",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_pef_event_messages_checkout,
                              _enable_pef_event_messages_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_PEF_Startup_Delay",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_pef_startup_delay_checkout,
                              _enable_pef_startup_delay_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_PEF_Alert_Startup_Delay",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_pef_alert_startup_delay_checkout,
                              _enable_pef_alert_startup_delay_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;
  
  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_Alert_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_alert_action_checkout,
                              _enable_alert_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_Power_Down_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_power_down_action_checkout,
                              _enable_power_down_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_Reset_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_reset_action_checkout,
                              _enable_reset_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_Power_Cycle_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_power_cycle_action_checkout,
                              _enable_power_cycle_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_OEM_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_oem_action_checkout,
                              _enable_oem_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "Enable_Diagnostic_Interrupt",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              _enable_diagnostic_interrupt_checkout,
                              _enable_diagnostic_interrupt_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "PEF_Startup_Delay",
                              "Give value in seconds",
                              CONFIG_DO_NOT_CHECKOUT,
                              _pef_startup_delay_checkout,
                              _pef_startup_delay_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              pef_section,
                              "PEF_Alert_Startup_Delay",
                              "Give value in seconds",
                              CONFIG_DO_NOT_CHECKOUT,
                              _pef_alert_startup_delay_checkout,
                              _pef_alert_startup_delay_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return pef_section;

 cleanup:
  if (pef_section)
    config_section_destroy(state_data->pstate, pef_section);
  return NULL;
}

