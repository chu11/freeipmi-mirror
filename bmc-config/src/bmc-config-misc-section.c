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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

static config_err_t
power_restore_policy_checkout (const char *section_name,
			       struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint64_t val;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_chassis_status_rs);

  if (ipmi_cmd_get_chassis_status (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_chassis_status: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "current_power_state.power_restore_policy", &val);

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            power_restore_policy_string ((uint8_t)val)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
power_restore_policy_commit (const char *section_name,
			     const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  fiid_obj_t obj_cmd_rs = NULL;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_power_restore_policy_rs);
  
  if (ipmi_cmd_set_power_restore_policy (state_data->ipmi_ctx,
                                         power_restore_policy_number (kv->value_input),
                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_power_restore_policy: %s\n",
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
bmc_config_misc_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *misc_section = NULL;
  char *section_comment = 
    "The following miscellaneous configuration options are optionally "
    "implemented by the vendor.  They may not be available your system and "
    "may not be visible below."
    "\n"
    "The \"Power_Restore_Policy\" determines the behavior of the machine "
    "when AC power returns after a power loss.  The behavior can be set to "
    "always power on the machine (\"On_State_AC_Apply\"), power off the "
    "machine (\"Off_State_AC_Apply\"), or return the power to the state that "
    "existed before the power loss (\"Restore_State_AC_Apply\").";

  if (!(misc_section = config_section_create (state_data->pstate,
                                              "Misc",
                                              "Misc",
                                              section_comment,
                                              CONFIG_DO_NOT_CHECKOUT,
                                              NULL,
                                              NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              misc_section,
                              "Power_Restore_Policy",
                              "Possible values: Off_State_AC_Apply/Restore_State_AC_Apply/On_State_AC_Apply",
                              CONFIG_DO_NOT_CHECKOUT | CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              power_restore_policy_checkout,
                              power_restore_policy_commit,
                              power_restore_policy_number_validate) < 0)
    goto cleanup;

  return misc_section;

 cleanup:
  if (misc_section)
    config_section_destroy(state_data->pstate, misc_section);
  return NULL;
}
