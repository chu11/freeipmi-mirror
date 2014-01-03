/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

#include "ipmi-config.h"
#include "ipmi-config-section.h"
#include "ipmi-config-validate.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience struct */
struct front_panel_buttons
{
  uint8_t standby;
  uint8_t diagnostic_interrupt;
  uint8_t reset;
  uint8_t power_off;
  uint8_t standby_disable_allowed;
  uint8_t diagnostic_interrupt_disable_allowed;
  uint8_t reset_disable_allowed;
  uint8_t power_off_disable_allowed;
};

#define BUTTON_ENABLED  IPMI_CHASSIS_BUTTON_ENABLE
#define BUTTON_DISABLED IPMI_CHASSIS_BUTTON_DISABLE
#define BUTTON_UNKNOWN  0x2

#define BUTTON_DISABLE_NOT_ALLOWED IPMI_CHASSIS_BUTTON_DISABLE_NOT_ALLOWED
#define BUTTON_DISABLE_ALLOWED     IPMI_CHASSIS_BUTTON_DISABLE_ALLOWED
#define BUTTON_DISABLE_UNKNOWN     0x2

static ipmi_config_err_t
_get_front_panel_buttons (ipmi_config_state_data_t *state_data,
                          struct front_panel_buttons *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  uint64_t val;
  int flag;

  assert (state_data);
  assert (data);

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
      ipmi_config_err_t ret;

      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_chassis_status: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.standby_button_disabled",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.standby_button_disabled': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->standby = val;
  else
    data->standby = BUTTON_UNKNOWN;

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.diagnostic_interrupt_button_disabled",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.diagnostic_interrupt_button_disabled': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->diagnostic_interrupt = val;
  else
    data->diagnostic_interrupt = BUTTON_UNKNOWN;

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.reset_button_disabled",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.reset_button_disabled': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->reset = val;
  else
    data->reset = BUTTON_UNKNOWN;

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.power_off_button_disabled",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.power_off_button_disabled': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->power_off = val;
  else
    data->power_off = BUTTON_UNKNOWN;

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.standby_button_disable_allowed",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.standby_button_disable_allowed': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->standby_disable_allowed = val;
  else
    data->standby_disable_allowed = BUTTON_DISABLE_UNKNOWN;

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.diagnostic_interrupt_button_disable_allowed",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.diagnostic_interrupt_button_disable_allowed': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->diagnostic_interrupt_disable_allowed = val;
  else
    data->diagnostic_interrupt_disable_allowed = BUTTON_DISABLE_UNKNOWN;

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.reset_button_disable_allowed",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.reset_button_disable_allowed': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->reset_disable_allowed = val;
  else
    data->reset_disable_allowed = BUTTON_DISABLE_UNKNOWN;

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.power_off_button_disable_allowed",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.power_off_button_disable_allowed': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    data->power_off_disable_allowed = val;
  else
    data->power_off_disable_allowed = BUTTON_DISABLE_UNKNOWN;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_front_panel_buttons (ipmi_config_state_data_t *state_data,
                          struct front_panel_buttons *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_front_panel_enables_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_front_panel_enables (state_data->ipmi_ctx,
                                        data->power_off,
                                        data->reset,
                                        data->diagnostic_interrupt,
                                        data->standby,
                                        obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_front_panel_enables: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
front_panel_buttons_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  struct front_panel_buttons data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t enabled = 0;
  char *enabled_str = NULL;

  assert (state_data);
  assert (section_name);
  assert (kv);

  memset (&data, '\0', sizeof (struct front_panel_buttons));
  if ((ret = _get_front_panel_buttons (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp (kv->key->key_name, "Enable_Standby_Button_For_Entering_Standby"))
    enabled = data.standby;
  else if (!strcasecmp (kv->key->key_name, "Enable_Diagnostic_Interrupt_Button"))
    enabled = data.diagnostic_interrupt;
  else if (!strcasecmp (kv->key->key_name, "Enable_Reset_Button"))
    enabled = data.reset;
  else if (!strcasecmp (kv->key->key_name, "Enable_Power_Off_Button_For_Power_Off_Only"))
    enabled = data.power_off;
  else
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (enabled == BUTTON_ENABLED)
    enabled_str = "Yes";
  else if (enabled == BUTTON_DISABLED)
    enabled_str = "No";
  else
    enabled_str = "";

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  enabled_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
front_panel_buttons_commit (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  struct front_panel_buttons data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t enable_or_disable;
  uint8_t disable_allowed;

  assert (state_data);
  assert (section_name);
  assert (kv);

  memset (&data, '\0', sizeof (struct front_panel_buttons));
  if ((ret = _get_front_panel_buttons (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp (kv->key->key_name, "Enable_Standby_Button_For_Entering_Standby"))
    {
      data.standby = same (kv->value_input, "yes") ? BUTTON_ENABLED : BUTTON_DISABLED;
      enable_or_disable = data.standby;
      disable_allowed = data.standby_disable_allowed;
    }
  else if (!strcasecmp (kv->key->key_name, "Enable_Diagnostic_Interrupt_Button"))
    {
      data.diagnostic_interrupt = same (kv->value_input, "yes") ? BUTTON_ENABLED : BUTTON_DISABLED;
      enable_or_disable = data.diagnostic_interrupt;
      disable_allowed = data.diagnostic_interrupt_disable_allowed;
    }
  else if (!strcasecmp (kv->key->key_name, "Enable_Reset_Button"))
    {
      data.reset = same (kv->value_input, "yes") ? BUTTON_ENABLED : BUTTON_DISABLED;
      enable_or_disable = data.reset;
      disable_allowed = data.reset_disable_allowed;
    }
  else if (!strcasecmp (kv->key->key_name, "Enable_Power_Off_Button_For_Power_Off_Only"))
    {
      data.power_off = same (kv->value_input, "yes") ? BUTTON_ENABLED : BUTTON_DISABLED;
      enable_or_disable = data.power_off;
      disable_allowed = data.power_off_disable_allowed;
    }
  else
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (enable_or_disable == BUTTON_DISABLED
      && disable_allowed == BUTTON_DISABLE_NOT_ALLOWED)
    {
      if (state_data->prog_data->args->verbose_count > 1)
        pstdout_printf (state_data->pstate,
                        "## Button disable on section:key_name '%s:%s' not allowed\n",
                        section_name,
                        kv->key->key_name);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (data.standby == BUTTON_UNKNOWN)
    {
      if (state_data->front_panel_enable_standby_button_for_entering_standby_initialized)
        data.standby = state_data->front_panel_enable_standby_button_for_entering_standby;
      else
        data.standby = BUTTON_ENABLED;
    }
  if (data.diagnostic_interrupt == BUTTON_UNKNOWN)
    {
      if (state_data->front_panel_enable_diagnostic_interrupt_button_initialized)
        data.diagnostic_interrupt = state_data->front_panel_enable_diagnostic_interrupt_button;
      else
        data.diagnostic_interrupt = BUTTON_ENABLED;
    }
  if (data.reset == BUTTON_UNKNOWN)
    {
      if (state_data->front_panel_enable_reset_button_initialized)
        data.reset = state_data->front_panel_enable_reset_button;
      else
        data.reset = BUTTON_ENABLED;
    }
  if (data.power_off == BUTTON_UNKNOWN)
    {
      if (state_data->front_panel_enable_power_off_button_for_power_off_only_initialized)
        data.power_off = state_data->front_panel_enable_power_off_button_for_power_off_only;
      else
        data.power_off = BUTTON_ENABLED;
    }

  if ((ret = _set_front_panel_buttons (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

struct ipmi_config_section *
ipmi_config_chassis_front_panel_buttons_get (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *section = NULL;
  char *section_comment =
    "The following configuration options are for enabling or disabling "
    "button functionality on the chassis.  Button may refer to a "
    "pushbutton, switch, or other front panel control built into the "
    "system chassis."
    "\n"
    "The value of the below may not be able to be checked out.  Therefore "
    "we recommend the user configure all four fields rather than a subset "
    "of them, otherwise some assumptions on configure may be made.";

  assert (state_data);

  if (!(section = ipmi_config_section_create (state_data,
					      "Chassis_Front_Panel_Buttons",
					      "Chassis_Front_Panel_Buttons",
					      section_comment,
					      0,
					      NULL,
					      NULL)))
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Enable_Standby_Button_For_Entering_Standby",
				   "Possible values: Yes/No",
				   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
				   front_panel_buttons_checkout,
				   front_panel_buttons_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Enable_Diagnostic_Interrupt_Button",
				   "Possible values: Yes/No",
				   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
				   front_panel_buttons_checkout,
				   front_panel_buttons_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Enable_Reset_Button",
				   "Possible values: Yes/No",
				   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
				   front_panel_buttons_checkout,
				   front_panel_buttons_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Enable_Power_Off_Button_For_Power_Off_Only",
				   "Possible values: Yes/No",
				   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
				   front_panel_buttons_checkout,
				   front_panel_buttons_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}
