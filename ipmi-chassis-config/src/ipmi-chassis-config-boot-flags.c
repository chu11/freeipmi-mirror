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
#include "ipmi-chassis-config-map.h"
#include "ipmi-chassis-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

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
};

static config_err_t
_get_boot_flags (ipmi_chassis_config_state_data_t *state_data,
                 struct boot_flags_data *data)
{ 
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint64_t val;

  assert(state_data);
  assert(data);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_system_boot_options_boot_flags_rs);

  if (ipmi_cmd_get_system_boot_options_boot_flags (state_data->ipmi_ctx, 
                                                   IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR,
                                                   IPMI_CHASSIS_BOOT_OPTIONS_NO_BLOCK_SELECTOR,
                                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_system_boot_options_boot_flags: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "boot_flags_persistent", &val);
  data->boot_flags_persistent = val;

  _FIID_OBJ_GET (obj_cmd_rs, "bios_boot_type", &val);
  data->bios_boot_type = val;

  _FIID_OBJ_GET (obj_cmd_rs, "cmos_clear", &val);
  data->cmos_clear = val;

  _FIID_OBJ_GET (obj_cmd_rs, "lock_keyboard", &val);
  data->lock_keyboard = val;

  _FIID_OBJ_GET (obj_cmd_rs, "screen_blank", &val);
  data->screen_blank = val;

  _FIID_OBJ_GET (obj_cmd_rs, "boot_device", &val);
  data->boot_device = val;

  _FIID_OBJ_GET (obj_cmd_rs, "lock_out_reset_button", &val);
  data->lock_out_reset_button = val;

  _FIID_OBJ_GET (obj_cmd_rs, "lock_out_via_power_button", &val);
  data->lock_out_power_button = val;

  _FIID_OBJ_GET (obj_cmd_rs, "lock_out_sleep_button", &val);
  data->lock_out_sleep_button = val;

  _FIID_OBJ_GET (obj_cmd_rs, "firmware_bios_verbosity", &val);
  data->firmware_bios_verbosity = val;

  _FIID_OBJ_GET (obj_cmd_rs, "force_progress_event_traps", &val);
  data->force_progress_event_traps = val;

  _FIID_OBJ_GET (obj_cmd_rs, "user_password_bypass", &val);
  data->user_password_bypass = val;

  _FIID_OBJ_GET (obj_cmd_rs, "console_redirection", &val);
  data->console_redirection = val;

  _FIID_OBJ_GET (obj_cmd_rs, "bios_mux_control_override", &val);
  data->bios_mux_control_override = val; 

  _FIID_OBJ_GET (obj_cmd_rs, "bios_shared_mode_override", &val);
  data->bios_shared_mode_override = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_boot_flags (ipmi_chassis_config_state_data_t *state_data,
                 struct boot_flags_data *data)
{ 
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(data);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_system_boot_options_rs);

  /* achu (workaround)
   *
   * Some motherboards seem to require that boot_flags_valid always be
   * "Yes".  So we'll enforce it.  It also doesn't make much sense
   * that we ever set it to "no".
   */

  if (ipmi_cmd_set_system_boot_options_boot_flags (state_data->ipmi_ctx, 
                                                   data->bios_boot_type,
                                                   data->boot_flags_persistent,
                                                   IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_VALID,
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
                                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_system_boot_options_boot_flags: %s\n",
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
boot_flags_persistent_checkout (const char *section_name,
                                struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.boot_flags_persistent ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
boot_flags_persistent_commit (const char *section_name,
                              const struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.boot_flags_persistent = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
bios_boot_type_checkout (const char *section_name,
                         struct config_keyvalue *kv,
                         void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;
  
  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            bios_boot_type_string(data.bios_boot_type)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
bios_boot_type_commit (const char *section_name,
                       const struct config_keyvalue *kv,
                       void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;
  
  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  data.bios_boot_type = bios_boot_type_number (kv->value_input);
  
  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
cmos_clear_checkout (const char *section_name,
                     struct config_keyvalue *kv,
                     void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.cmos_clear ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
cmos_clear_commit (const char *section_name,
                   const struct config_keyvalue *kv,
                   void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.cmos_clear = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_keyboard_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.lock_keyboard ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_keyboard_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.lock_keyboard = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
screen_blank_checkout (const char *section_name,
                       struct config_keyvalue *kv,
                       void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.screen_blank ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
screen_blank_commit (const char *section_name,
                     const struct config_keyvalue *kv,
                     void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.screen_blank = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
boot_device_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            boot_device_string(data.boot_device)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
boot_device_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.boot_device = boot_device_number (kv->value_input);

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_out_reset_button_checkout (const char *section_name,
                                struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.lock_out_reset_button ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_out_reset_button_commit (const char *section_name,
                              const struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.lock_out_reset_button = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_out_power_button_checkout (const char *section_name,
                                struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.lock_out_power_button ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_out_power_button_commit (const char *section_name,
                              const struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.lock_out_power_button = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_out_sleep_button_checkout (const char *section_name,
                                struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.lock_out_sleep_button ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lock_out_sleep_button_commit (const char *section_name,
                              const struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.lock_out_sleep_button = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
chassis_boot_flags_post (const char *section_name,
                         void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  uint8_t boot_info_acknowledge = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_UNACKNOWLEDGE;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  fiid_obj_t obj_cmd_rs = NULL;

  /* Following should be called to inform remaining chassis subsystems
   * that a boot configuration change has taken place.
   */

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_system_boot_options_rs);

  if (ipmi_cmd_set_system_boot_options_boot_info_acknowledge (state_data->ipmi_ctx,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              &boot_info_acknowledge,
                                                              obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_system_boot_options_boot_info_acknowledge: %s\n",
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
firmware_bios_verbosity_checkout (const char *section_name,
                                  struct config_keyvalue *kv,
                                  void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;
  
  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            firmware_bios_verbosity_string(data.firmware_bios_verbosity)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
firmware_bios_verbosity_commit (const char *section_name,
                                const struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;
  
  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  data.firmware_bios_verbosity = firmware_bios_verbosity_number (kv->value_input);
  
  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_progress_event_traps_checkout (const char *section_name,
                                     struct config_keyvalue *kv,
                                     void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.force_progress_event_traps ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_progress_event_traps_commit (const char *section_name,
                                   const struct config_keyvalue *kv,
                                   void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.force_progress_event_traps = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
user_password_bypass_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            data.user_password_bypass ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
user_password_bypass_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;

  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  data.user_password_bypass = same (kv->value_input, "yes");

  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
console_redirection_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;
  
  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            console_redirection_string(data.console_redirection)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
console_redirection_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_chassis_config_state_data_t *state_data = (ipmi_chassis_config_state_data_t *)arg;
  struct boot_flags_data data;
  config_err_t ret;
  
  if ((ret = _get_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  data.console_redirection = console_redirection_number (kv->value_input);
  
  if ((ret = _set_boot_flags(state_data, &data)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  return CONFIG_ERR_SUCCESS;
}

struct config_section *
ipmi_chassis_config_boot_flags_get (ipmi_chassis_config_state_data_t *state_data)
{
  struct config_section *section = NULL;
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
    "may refer to any type of removeable media.";

  if (!(section = config_section_create (state_data->pstate,
                                         "Chassis_Boot_Flags",
                                         "Chassis_Boot_Flags",
                                         section_comment,
                                         0,
                                         NULL,
                                         chassis_boot_flags_post)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Boot_Flags_Persistent",
                              "Possible values: Yes/No (Yes = All Future Boots; No = Next Boot Only)",
                              0,
                              boot_flags_persistent_checkout,
                              boot_flags_persistent_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "BIOS_Boot_Type",
                              "Possible values: PC-COMPATIBLE/EFI",
                              0,
                              bios_boot_type_checkout,
                              bios_boot_type_commit,
                              bios_boot_type_number_validate) < 0)
    goto cleanup;
  
  if (config_section_add_key (state_data->pstate,
                              section,
                              "CMOS_Clear",
                              "Possible values: Yes/No (Only applies to Next Boot)",
                              0,
                              cmos_clear_checkout,
                              cmos_clear_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Lock_Keyboard",
                              "Possible values: Yes/No (Only applies to Next Boot)",
                              0,
                              lock_keyboard_checkout,
                              lock_keyboard_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;
  
  if (config_section_add_key (state_data->pstate,
                              section,
                              "Screen_Blank",
                              "Possible values: Yes/No (Only applies to Next Boot)",
                              0,
                              screen_blank_checkout,
                              screen_blank_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Boot_Device",
                              "Possible values: NO-OVERRIDE/PXE/HARD-DRIVE/HARD-DRIVE-SAFE-MODE/\n"
                              "                 DIAGNOSTIC_PARTITION/CD-DVD/BIOS-SETUP/FLOPPY",
                              0,
                              boot_device_checkout,
                              boot_device_commit,
                              boot_device_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Lock_Out_Reset_Button",
                              "Possible values: Yes/No",
                              0,
                              lock_out_reset_button_checkout,
                              lock_out_reset_button_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Lock_Out_Power_Button",
                              "Possible values: Yes/No",
                              0,
                              lock_out_power_button_checkout,
                              lock_out_power_button_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Lock_Out_Sleep_Button",
                              "Possible values: Yes/No",
                              0,
                              lock_out_sleep_button_checkout,
                              lock_out_sleep_button_commit,
                              config_yes_no_validate) < 0)
    goto cleanup; 

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Firmware_Bios_Verbosity",
                              "Possible values: DEFAULT/QUIET/VERBOSE",
                              0,
                              firmware_bios_verbosity_checkout,
                              firmware_bios_verbosity_commit,
                              firmware_bios_verbosity_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Force_Progress_Event_Traps",
                              "Possible values: Yes/No",
                              0,
                              force_progress_event_traps_checkout,
                              force_progress_event_traps_commit,
                              config_yes_no_validate) < 0)
    goto cleanup; 

  if (config_section_add_key (state_data->pstate,
                              section,
                              "User_Password_Bypass",
                              "Possible values: Yes/No",
                              0,
                              user_password_bypass_checkout,
                              user_password_bypass_commit,
                              config_yes_no_validate) < 0)
    goto cleanup; 

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Console_Redirection",
                              "Possible values: BIOS-SETTING/SUPPRESS/ENABLE",
                              0,
                              console_redirection_checkout,
                              console_redirection_commit,
                              console_redirection_number_validate) < 0)
    goto cleanup;

  return section;

 cleanup:
  if (section)
    config_section_destroy(state_data->pstate, section);
  return NULL;
}
