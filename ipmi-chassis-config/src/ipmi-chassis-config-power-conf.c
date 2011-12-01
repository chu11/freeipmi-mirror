/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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
#include "ipmi-chassis-config-map.h"
#include "ipmi-chassis-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

static config_err_t
power_restore_policy_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_chassis_config_state_data_t *state_data;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint8_t power_restore_policy;
  uint64_t val;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_chassis_config_state_data_t *)arg;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_chassis_status_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_chassis_status (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      config_err_t ret;

      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_chassis_status: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_non_fatal_error (state_data->ipmi_ctx,
                                     obj_cmd_rs,
                                     &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "current_power_state.power_restore_policy", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'current_power_state.power_restore_policy': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  power_restore_policy = val;

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             power_restore_policy_string (power_restore_policy)) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
power_restore_policy_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_chassis_config_state_data_t *state_data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  fiid_obj_t obj_cmd_rs = NULL;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_chassis_config_state_data_t *)arg;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_power_restore_policy_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_power_restore_policy (state_data->ipmi_ctx,
                                         power_restore_policy_number (kv->value_input),
                                         obj_cmd_rs) < 0)
    {
      config_err_t ret;

      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_power_restore_policy: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_non_fatal_error (state_data->ipmi_ctx,
                                     obj_cmd_rs,
                                     &ret))
        rv = ret;

      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
power_cycle_interval_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_chassis_config_state_data_t *state_data;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_chassis_config_state_data_t *)arg;

  /* achu: value cannot be checked out */
  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             "") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
power_cycle_interval_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_chassis_config_state_data_t *state_data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  fiid_obj_t obj_cmd_rs = NULL;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_chassis_config_state_data_t *)arg;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_power_cycle_interval_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_power_cycle_interval (state_data->ipmi_ctx,
                                         atoi (kv->value_input),
                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_power_cycle_interval: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      if (!IPMI_ERRNUM_IS_FATAL_ERROR (state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

struct config_section *
ipmi_chassis_config_power_conf_get (ipmi_chassis_config_state_data_t *state_data)
{
  struct config_section *section = NULL;
  char *section_comment =
    "The following configuration options are for configuring "
    "chassis power behavior."
    "\n"
    "The \"Power_Restore_Policy\" determines the behavior of the machine "
    "when AC power returns after a power loss.  The behavior can be set to "
    "always power on the machine (\"On_State_AC_Apply\"), power off the "
    "machine (\"Off_State_AC_Apply\"), or return the power to the state that "
    "existed before the power loss (\"Restore_State_AC_Apply\")."
    "\n"
    "The \"Power_Cycle_Interval\" determines the time the system will be "
    "powered down following a power cycle command.";

  if (!(section = config_section_create (state_data->pstate,
                                         "Chassis_Power_Conf",
                                         "Chassis_Power_Conf",
                                         section_comment,
                                         0,
                                         NULL,
                                         NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Power_Restore_Policy",
                              "Possible values: Off_State_AC_Apply/Restore_State_AC_Apply/On_State_AC_Apply",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              power_restore_policy_checkout,
                              power_restore_policy_commit,
                              power_restore_policy_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Power_Cycle_Interval",
                              "Give value in seconds",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              power_cycle_interval_checkout,
                              power_cycle_interval_commit,
                              config_number_range_one_byte_non_zero) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}
