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
  uint8_t boot_flags_valid;
  uint8_t boot_flags_persistent;
  uint8_t bios_boot_type;
  uint8_t cmos_clear;
  uint8_t lock_keyboard;
  uint8_t screen_blank;
  uint8_t boot_device_selector;
  uint8_t lock_out_reset_button;
  uint8_t lock_out_via_power_button;
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

  _FIID_OBJ_GET (obj_cmd_rs, "boot_flags_valid", &val);
  data->boot_flags_valid = val;

  _FIID_OBJ_GET (obj_cmd_rs, "boot_flags_persistent", &val);
  data->boot_flags_persistent = val;

  _FIID_OBJ_GET (obj_cmd_rs, "bios_boot_type", &val);
  data->bios_boot_type = val;

  _FIID_OBJ_GET (obj_cmd_rs, "clear_cmos", &val);
  data->cmos_clear = val;

  _FIID_OBJ_GET (obj_cmd_rs, "lock_keyboard", &val);
  data->lock_keyboard = val;

  _FIID_OBJ_GET (obj_cmd_rs, "screen_blank", &val);
  data->screen_blank = val;

  _FIID_OBJ_GET (obj_cmd_rs, "boot_device_selector", &val);
  data->boot_device_selector = val;

  _FIID_OBJ_GET (obj_cmd_rs, "lock_out_reset_button", &val);
  data->lock_out_reset_button = val;

  _FIID_OBJ_GET (obj_cmd_rs, "lock_out_via_power_button", &val);
  data->lock_out_via_power_button = val;

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

  _FIID_OBJ_GET (obj_cmd_rs, "", &val);
  data->bios_shared_mode_override = val;

  _FIID_OBJ_GET (obj_cmd_rs, "", &val);
  data->bios_mux_control_override = val; 

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
  uint64_t val;

  assert(state_data);
  assert(data);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_system_boot_options_rs);

  if (ipmi_cmd_set_system_boot_options_boot_flags (state_data->ipmi_ctx, 
                                                   data->bios_boot_type,
                                                   data->boot_flags_persistent,
                                                   data->boot_flags_valid,
                                                   data->lock_out_reset_button,
                                                   data->screen_blank,
                                                   data->boot_device_selector,
                                                   data->lock_keyboard,
                                                   data->cmos_clear,
                                                   data->console_redirection,
                                                   data->lock_out_sleep_button,
                                                   data->user_password_bypass,
                                                   data->force_progress_event_traps,
                                                   data->firmware_bios_verbosity,
                                                   data->lock_out_via_power_button,
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

struct config_section *
ipmi_chassis_config_boot_flags_get (ipmi_chassis_config_state_data_t *state_data)
{
  struct config_section *section = NULL;
  char *section_comment = 
    "The following configuration options are for configuring "
    "chassis boot behavior.";

  if (!(section = config_section_create (state_data->pstate,
                                         "Chassis_Boot_Flags",
                                         "Chassis_Boot_Flags",
                                         section_comment,
                                         0,
                                         NULL,
                                         chassis_boot_flags_post)))
    goto cleanup;

#if 0
  if (config_section_add_key (state_data->pstate,
                              section,
                              "Power_Restore_Policy",
                              "Possible values: Off_State_AC_Apply/Restore_State_AC_Apply/On_State_AC_Apply",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              power_restore_policy_checkout,
                              power_restore_policy_commit,
                              power_restore_policy_number_validate) < 0)
    goto cleanup;
#endif
  
  return section;

 cleanup:
  if (section)
    config_section_destroy(state_data->pstate, section);
  return NULL;
}
