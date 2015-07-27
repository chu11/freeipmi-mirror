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
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience struct */
struct boot_flags_data
{
  uint8_t boot_flags_persistent;
  uint8_t bios_boot_type;
  uint8_t cmos_clear;
  uint8_t lock_keyboard;
  uint8_t screen_blank;
  uint8_t boot_device;
  uint8_t lock_out_reset_button;
  uint8_t lock_out_power_button;
  uint8_t lock_out_sleep_button;
  uint8_t firmware_bios_verbosity;
  uint8_t force_progress_event_traps;
  uint8_t user_password_bypass;
  uint8_t console_redirection;
  uint8_t bios_shared_mode_override;
  uint8_t bios_mux_control_override;
  uint8_t device_instance_selector;
};

static ipmi_config_err_t
_get_boot_flags (ipmi_config_state_data_t *state_data,
                 struct boot_flags_data *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  uint64_t val;

  assert (state_data);
  assert (data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_boot_options_boot_flags_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_system_boot_options_boot_flags (state_data->ipmi_ctx,
                                                   IPMI_SYSTEM_BOOT_OPTIONS_NO_SET_SELECTOR,
                                                   IPMI_SYSTEM_BOOT_OPTIONS_NO_BLOCK_SELECTOR,
                                                   obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
						 obj_cmd_rs,
						 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_system_boot_options_boot_flags: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "boot_flags_persistent", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'boot_flags_persistent': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->boot_flags_persistent = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "bios_boot_type", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bios_boot_type': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->bios_boot_type = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "cmos_clear", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'cmos_clear': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->cmos_clear = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "lock_keyboard", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'lock_keyboard': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->lock_keyboard = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "screen_blank", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'screen_blank': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->screen_blank = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "boot_device", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'boot_device': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->boot_device = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "lock_out_reset_button", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'lock_out_reset_button': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->lock_out_reset_button = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "lock_out_via_power_button", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'lock_out_via_power_button': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->lock_out_power_button = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "lock_out_sleep_button", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'lock_out_sleep_button': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->lock_out_sleep_button = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "firmware_bios_verbosity", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'firmware_bios_verbosity': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->firmware_bios_verbosity = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "force_progress_event_traps", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'force_progress_event_traps': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->force_progress_event_traps = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_password_bypass", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_password_bypass': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->user_password_bypass = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "console_redirection", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'console_redirection': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->console_redirection = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "bios_mux_control_override", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bios_mux_control_override': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->bios_mux_control_override = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "bios_shared_mode_override", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bios_shared_mode_override': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->bios_shared_mode_override = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "device_instance_selector", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'device_instance_selector': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->device_instance_selector = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_boot_flags (ipmi_config_state_data_t *state_data,
                 struct boot_flags_data *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_system_boot_options_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* achu (workaround)
   *
   * Some motherboards seem to require that boot_flags_valid always be
   * "Yes".  So we'll enforce it.  It also doesn't make much sense
   * that we ever set it to "no".
   */

  if (ipmi_cmd_set_system_boot_options_boot_flags (state_data->ipmi_ctx,
                                                   IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED,
                                                   data->bios_boot_type,
                                                   data->boot_flags_persistent,
                                                   IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_VALID,
                                                   data->lock_out_reset_button,
                                                   data->screen_blank,
                                                   data->boot_device,
                                                   data->lock_keyboard,
                                                   data->cmos_clear,
                                                   data->console_redirection,
                                                   data->lock_out_sleep_button,
                                                   data->user_password_bypass,
                                                   data->force_progress_event_traps,
                                                   data->firmware_bios_verbosity,
                                                   data->lock_out_power_button,
                                                   data->bios_mux_control_override,
                                                   data->bios_shared_mode_override,
                                                   data->device_instance_selector,
                                                   obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
						 obj_cmd_rs,
						 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_system_boot_options_boot_flags: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
boot_flags_persistent_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.boot_flags_persistent ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
boot_flags_persistent_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.boot_flags_persistent = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
bios_boot_type_checkout (ipmi_config_state_data_t *state_data,
			 const char *section_name,
                         struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  bios_boot_type_string (data.bios_boot_type)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
bios_boot_type_commit (ipmi_config_state_data_t *state_data,
		       const char *section_name,
                       const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.bios_boot_type = bios_boot_type_number (kv->value_input);

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
cmos_clear_checkout (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.cmos_clear ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
cmos_clear_commit (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.cmos_clear = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_keyboard_checkout (ipmi_config_state_data_t *state_data,
			const char *section_name,
                        struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.lock_keyboard ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_keyboard_commit (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.lock_keyboard = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
screen_blank_checkout (ipmi_config_state_data_t *state_data,
		       const char *section_name,
                       struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.screen_blank ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
screen_blank_commit (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.screen_blank = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
boot_device_checkout (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  boot_device_string (data.boot_device)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
boot_device_commit (ipmi_config_state_data_t *state_data,
		    const char *section_name,
                    const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.boot_device = boot_device_number (kv->value_input);

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
device_instance_selector_checkout (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  device_instance_selector_string (data.device_instance_selector)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
device_instance_selector_commit (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.device_instance_selector = device_instance_selector_number (kv->value_input);

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_out_reset_button_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.lock_out_reset_button ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_out_reset_button_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.lock_out_reset_button = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_out_power_button_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.lock_out_power_button ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_out_power_button_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.lock_out_power_button = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_out_sleep_button_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.lock_out_sleep_button ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lock_out_sleep_button_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.lock_out_sleep_button = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
_set_system_boot_options_BMC_boot_flag_valid_bit_clearing (ipmi_config_state_data_t *state_data)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  fiid_obj_t obj_cmd_rs = NULL;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_system_boot_options_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing (state_data->ipmi_ctx,
                                                                         IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED,
                                                                         IPMI_SYSTEM_BOOT_OPTION_DONT_CLEAR_VALID_BIT,
                                                                         IPMI_SYSTEM_BOOT_OPTION_DONT_CLEAR_VALID_BIT,
                                                                         IPMI_SYSTEM_BOOT_OPTION_DONT_CLEAR_VALID_BIT,
                                                                         IPMI_SYSTEM_BOOT_OPTION_DONT_CLEAR_VALID_BIT,
                                                                         IPMI_SYSTEM_BOOT_OPTION_DONT_CLEAR_VALID_BIT,
                                                                         obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;
      
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
						 obj_cmd_rs,
						 &ret))
        rv = ret;
      
      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_system_boot_options_BMC_boot_flag_valid_bit_clearing: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }
  
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv); 
}

static ipmi_config_err_t
_set_system_boot_options_boot_info_acknowledge (ipmi_config_state_data_t *state_data)
{
  uint8_t boot_info_acknowledge = IPMI_SYSTEM_BOOT_OPTION_BOOT_INFO_UNACKNOWLEDGE;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  fiid_obj_t obj_cmd_rs = NULL;
  
  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_system_boot_options_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_system_boot_options_boot_info_acknowledge (state_data->ipmi_ctx,
                                                              IPMI_SYSTEM_BOOT_OPTIONS_PARAMETER_VALID_UNLOCKED,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
						 obj_cmd_rs,
						 &ret))
        rv = ret;
      
      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_system_boot_options_boot_info_acknowledge: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
chassis_boot_flags_post (ipmi_config_state_data_t *state_data,
			 const char *section_name)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);

  /* Following should be called to ensure system keeps the
   * configuration permanent
   */

  if ((ret = _set_system_boot_options_BMC_boot_flag_valid_bit_clearing (state_data)) == IPMI_CONFIG_ERR_FATAL_ERROR)
    {
      rv = ret;
      goto cleanup;
    }

  if (ret != IPMI_CONFIG_ERR_SUCCESS)
    rv = ret;

  /* Following should be called to inform remaining chassis subsystems
   * that a boot configuration change has taken place.
   */

  if ((ret = _set_system_boot_options_boot_info_acknowledge (state_data)) == IPMI_CONFIG_ERR_FATAL_ERROR)
    {
      rv = ret;
      goto cleanup;
    }

  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR)
    rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
firmware_bios_verbosity_checkout (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  firmware_bios_verbosity_string (data.firmware_bios_verbosity)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
firmware_bios_verbosity_commit (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.firmware_bios_verbosity = firmware_bios_verbosity_number (kv->value_input);

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
force_progress_event_traps_checkout (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.force_progress_event_traps ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
force_progress_event_traps_commit (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.force_progress_event_traps = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
user_password_bypass_checkout (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.user_password_bypass ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
user_password_bypass_commit (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.user_password_bypass = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
console_redirection_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  console_redirection_string (data.console_redirection)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
console_redirection_commit (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  struct boot_flags_data data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  data.console_redirection = console_redirection_number (kv->value_input);

  if ((ret = _set_boot_flags (state_data, &data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

struct ipmi_config_section *
ipmi_config_chassis_boot_flags_get (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *section = NULL;
  char *section_comment =
    "The following configuration options are for configuring "
    "chassis boot behavior.  Please note that some fields may apply to "
    "all future boots while some may only apply to the next system boot."
    "\n"
    "\"Boot_Flags_Persistent\" determines if flags apply to the next boot only "
    "or all future boots."
    "\n"
    "\"Boot_Device\" allows the user to configure which device the BIOS should "
    "boot off of.  Most users may wish to select NO-OVERRIDE to select the "
    "configuration currently determined by the BIOS.  Note that the configuration "
    "value BIOS-SETUP refers to booting *into* the BIOS Setup, not from it.  FLOPPY "
    "may refer to any type of removable media.  \"Device_Instance_Selector\" may "
    "be be used to select a specific device instance for booting.";

  assert (state_data);

  if (!(section = ipmi_config_section_create (state_data,
					      "Chassis_Boot_Flags",
					      "Chassis_Boot_Flags",
					      section_comment,
					      0,
					      NULL,
					      chassis_boot_flags_post)))
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Boot_Flags_Persistent",
				   "Possible values: Yes/No (Yes = All Future Boots; No = Next Boot Only)",
				   0,
				   boot_flags_persistent_checkout,
				   boot_flags_persistent_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "BIOS_Boot_Type",
				   "Possible values: PC-COMPATIBLE/EFI",
				   0,
				   bios_boot_type_checkout,
				   bios_boot_type_commit,
				   bios_boot_type_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "CMOS_Clear",
				   "Possible values: Yes/No (Only applies to Next Boot)",
				   0,
				   cmos_clear_checkout,
				   cmos_clear_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Lock_Keyboard",
				   "Possible values: Yes/No (Only applies to Next Boot)",
				   0,
				   lock_keyboard_checkout,
				   lock_keyboard_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Screen_Blank",
				   "Possible values: Yes/No (Only applies to Next Boot)",
				   0,
				   screen_blank_checkout,
				   screen_blank_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Boot_Device",
				   "Possible values: NO-OVERRIDE/PXE/HARD-DRIVE/HARD-DRIVE-SAFE-MODE/\n"
				   "                 DIAGNOSTIC_PARTITION/CD-DVD/BIOS-SETUP/REMOTE-FLOPPY\n"
				   "                 PRIMARY-REMOTE-MEDIA/REMOTE-CD-DVD/REMOTE-HARD-DRIVE/FLOPPY",
				   0,
				   boot_device_checkout,
				   boot_device_commit,
				   boot_device_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Device_Instance_Selector",
				   "Possible values: None/Internal-{1-15}/External-{1-15} (e.g. Internal-5)",
				   0,
				   device_instance_selector_checkout,
				   device_instance_selector_commit,
				   device_instance_selector_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Lock_Out_Reset_Button",
				   "Possible values: Yes/No",
				   0,
				   lock_out_reset_button_checkout,
				   lock_out_reset_button_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Lock_Out_Power_Button",
				   "Possible values: Yes/No",
				   0,
				   lock_out_power_button_checkout,
				   lock_out_power_button_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Lock_Out_Sleep_Button",
				   "Possible values: Yes/No",
				   0,
				   lock_out_sleep_button_checkout,
				   lock_out_sleep_button_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Firmware_Bios_Verbosity",
				   "Possible values: DEFAULT/QUIET/VERBOSE",
				   0,
				   firmware_bios_verbosity_checkout,
				   firmware_bios_verbosity_commit,
				   firmware_bios_verbosity_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Force_Progress_Event_Traps",
				   "Possible values: Yes/No",
				   0,
				   force_progress_event_traps_checkout,
				   force_progress_event_traps_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "User_Password_Bypass",
				   "Possible values: Yes/No",
				   0,
				   user_password_bypass_checkout,
				   user_password_bypass_commit,
				   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Console_Redirection",
				   "Possible values: BIOS-SETTING/SUPPRESS/ENABLE",
				   0,
				   console_redirection_checkout,
				   console_redirection_commit,
				   console_redirection_number_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}
