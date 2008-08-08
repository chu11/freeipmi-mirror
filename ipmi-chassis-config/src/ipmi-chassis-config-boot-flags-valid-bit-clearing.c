/* 
   Copyright (C) 2008 FreeIPMI Core Team

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
#include <assert.h>

#include "ipmi-chassis-config.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

/* convenience struct */
struct boot_flags_valid_bit_clearing
{
  uint8_t dont_clear_on_power_cycle_by_PEF;
  uint8_t dont_clear_on_chassis_control_timeout;
  uint8_t dont_clear_on_power_cycle_by_watchdog_timeout;
  uint8_t dont_clear_on_pushbutton_or_soft_reset;
  uint8_t dont_clear_on_powerup_via_pushbutton_or_wake_event;
};

static config_err_t
_get_boot_flags_valid_bit_clearing (ipmi_chassis_config_state_data_t *state_data,
                                   struct boot_flags_valid_bit_clearing *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint64_t val;

  assert(state_data);
  assert(data);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_system_boot_options_BMC_boot_flag_valid_bit_clearing_rs);
  
  if (ipmi_cmd_get_system_boot_options_BMC_boot_flag_valid_bit_clearing (state_data->ipmi_ctx, 
                                                                         IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR,
                                                                         IPMI_CHASSIS_BOOT_OPTIONS_NO_BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_system_boot_options_BMC_boot_flag_valid_bit_clearing: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, 
                 "dont_clear_valid_bit_on_reset_power_cycle_caused_by_PEF", 
                 &val);
  data->dont_clear_on_power_cycle_by_PEF = val;

  _FIID_OBJ_GET (obj_cmd_rs, 
                 "dont_automatically_clear_boot_flag_valid_bit_if_chassis_control_command_not_received_within_60_second_timeout", 
                 &val);
  data->dont_clear_on_chassis_control_timeout = val;

  _FIID_OBJ_GET (obj_cmd_rs, 
                 "dont_clear_valid_bit_on_reset_power_cycle_caused_by_watchdog_timeout", 
                 &val);
  data->dont_clear_on_power_cycle_by_watchdog_timeout = val;

  _FIID_OBJ_GET (obj_cmd_rs,
                 "dont_clear_valid_bit_on_pushbutton_reset_soft_reset", 
                 &val);
  data->dont_clear_on_pushbutton_or_soft_reset = val;

  _FIID_OBJ_GET (obj_cmd_rs, 
                 "dont_clear_valid_bit_on_power_up_via_power_pushbutton_or_wake_event", 
                 &val);
  data->dont_clear_on_powerup_via_pushbutton_or_wake_event = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_boot_flags_valid_bit_clearing (ipmi_chassis_config_state_data_t *state_data,
                                   struct boot_flags_valid_bit_clearing *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(data);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_system_boot_options_rs);
  
  if (ipmi_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing (state_data->ipmi_ctx,
                                                                         data->dont_clear_on_powerup_via_pushbutton_or_wake_event,
                                                                         data->dont_clear_on_pushbutton_or_soft_reset,
                                                                         data->dont_clear_on_power_cycle_by_watchdog_timeout,
                                                                         data->dont_clear_on_chassis_control_timeout,
                                                                         data->dont_clear_on_power_cycle_by_PEF,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
boot_flags_valid_bit_clearing_checkout (const char *section_name,
                                       struct config_keyvalue *kv,
                                       void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_valid_bit_clearing data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t val = 0;

  memset(&data, '\0', sizeof(struct boot_flags_valid_bit_clearing));
  if ((ret = _get_boot_flags_valid_bit_clearing (state_data, &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp(kv->key->key_name, "Clear_on_Power_Cycle_By_PEF"))
    val = data.dont_clear_on_power_cycle_by_PEF;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Chassis_Control_Timeout"))
    val = data.dont_clear_on_chassis_control_timeout;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Power_Cycle_By_Watchdog_Timeout"))
    val = data.dont_clear_on_power_cycle_by_watchdog_timeout;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Pushbutton_or_Soft_Reset"))
    val = data.dont_clear_on_pushbutton_or_soft_reset;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Powerup_via_Pushbutton_or_Wake_Event"))
    val = data.dont_clear_on_powerup_via_pushbutton_or_wake_event;
  else
    {
      if (state_data->prog_data->args->config_args.verbose)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  /* achu: notice, "Yes" vs. "No" is opposite logic */
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            val ? "No" : "Yes") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static config_err_t
boot_flags_valid_bit_clearing_commit (const char *section_name,
                                     const struct config_keyvalue *kv,
                                     void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_valid_bit_clearing data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  memset(&data, '\0', sizeof(struct boot_flags_valid_bit_clearing));
  if ((ret = _get_boot_flags_valid_bit_clearing (state_data, &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp(kv->key->key_name, "Clear_on_Power_Cycle_By_PEF"))
    data.dont_clear_on_power_cycle_by_PEF = same (kv->value_input, "yes") ? IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT : IPMI_CHASSIS_BOOT_OPTIONS_DONT_CLEAR_VALID_BIT;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Chassis_Control_Timeout"))
    data.dont_clear_on_chassis_control_timeout = same (kv->value_input, "yes") ? IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT : IPMI_CHASSIS_BOOT_OPTIONS_DONT_CLEAR_VALID_BIT;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Power_Cycle_By_Watchdog_Timeout"))
    data.dont_clear_on_power_cycle_by_watchdog_timeout = same (kv->value_input, "yes") ? IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT : IPMI_CHASSIS_BOOT_OPTIONS_DONT_CLEAR_VALID_BIT;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Pushbutton_or_Soft_Reset"))
    data.dont_clear_on_pushbutton_or_soft_reset = same (kv->value_input, "yes") ? IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT : IPMI_CHASSIS_BOOT_OPTIONS_DONT_CLEAR_VALID_BIT;
  else if (!strcasecmp(kv->key->key_name, "Clear_on_Powerup_via_Pushbutton_or_Wake_Event"))
    data.dont_clear_on_powerup_via_pushbutton_or_wake_event = same (kv->value_input, "yes") ? IPMI_CHASSIS_BOOT_OPTIONS_CLEAR_VALID_BIT : IPMI_CHASSIS_BOOT_OPTIONS_DONT_CLEAR_VALID_BIT;
  else
    {
      if (state_data->prog_data->args->config_args.verbose)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if ((ret = _set_boot_flags_valid_bit_clearing (state_data, &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

struct config_section *
ipmi_chassis_config_boot_flags_valid_bit_clearing_get (ipmi_chassis_config_state_data_t *state_data)
{
  struct config_section *section = NULL;
  char *section_comment = 
    "The following configuration options are for enabling or disabling "
    "conditions under which the \"Boot_Flags_Valid\" field in the "
    "\"Chassis_Boot_Flags\" section.";

  if (!(section = config_section_create (state_data->pstate,
                                         "Chassis_Boot_Flags_Valid_Bit_Clearing",
                                         "Chassis_Boot_Flags_Valid_Bit_Clearing",
                                         section_comment,
                                         0,
                                         NULL,
                                         NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Clear_on_Power_Cycle_By_PEF",
                              "Possible values: Yes/No",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              boot_flags_valid_bit_clearing_checkout,
                              boot_flags_valid_bit_clearing_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Clear_on_Chassis_Control_Timeout",
                              "Possible values: Yes/No",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              boot_flags_valid_bit_clearing_checkout,
                              boot_flags_valid_bit_clearing_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Clear_on_Power_Cycle_By_Watchdog_Timeout",
                              "Possible values: Yes/No",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              boot_flags_valid_bit_clearing_checkout,
                              boot_flags_valid_bit_clearing_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Clear_on_Pushbutton_or_Soft_Reset",
                              "Possible values: Yes/No",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              boot_flags_valid_bit_clearing_checkout,
                              boot_flags_valid_bit_clearing_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Clear_on_Powerup_via_Pushbutton_or_Wake_Event",
                              "Possible values: Yes/No",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              boot_flags_valid_bit_clearing_checkout,
                              boot_flags_valid_bit_clearing_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  return section;

 cleanup:
  if (section)
    config_section_destroy(state_data->pstate, section);
  return NULL;
}
