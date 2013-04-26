/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

#include "bmc-config.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience structs */
struct bmc_authentication_level {
  uint8_t callback_level_none;
  uint8_t callback_level_md2;
  uint8_t callback_level_md5;
  uint8_t callback_level_straight_password;
  uint8_t callback_level_oem_proprietary;
  uint8_t user_level_none;
  uint8_t user_level_md2;
  uint8_t user_level_md5;
  uint8_t user_level_straight_password;
  uint8_t user_level_oem_proprietary;
  uint8_t operator_level_none;
  uint8_t operator_level_md2;
  uint8_t operator_level_md5;
  uint8_t operator_level_straight_password;
  uint8_t operator_level_oem_proprietary;
  uint8_t admin_level_none;
  uint8_t admin_level_md2;
  uint8_t admin_level_md5;
  uint8_t admin_level_straight_password;
  uint8_t admin_level_oem_proprietary;
  uint8_t oem_level_none;
  uint8_t oem_level_md2;
  uint8_t oem_level_md5;
  uint8_t oem_level_straight_password;
  uint8_t oem_level_oem_proprietary;
};

static config_err_t
_get_authentication_type_support (bmc_config_state_data_t *state_data,
				  const char *section_name)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (state_data->authentication_type_initialized
      && state_data->authentication_type_channel_number == channel_number)
    goto out;
  
  state_data->authentication_type_initialized = 0;
  state_data->authentication_type_channel_number = 0;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_authentication_type_support_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_authentication_type_support (state_data->ipmi_ctx,
                                                                             channel_number,
                                                                             IPMI_GET_LAN_PARAMETER,
                                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_authentication_type_support: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "none", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'none': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->authentication_type_none = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "md2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'md2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->authentication_type_md2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "md5", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'md5': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->authentication_type_md5 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "straight_password", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'straight_password': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->authentication_type_straight_password = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_proprietary", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_proprietary': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->authentication_type_oem_proprietary = val;

  state_data->authentication_type_initialized++;
 out:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_get_authentication_type_enables (bmc_config_state_data_t *state_data,
				  const char *section_name,
                                  struct bmc_authentication_level *al)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (al);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables (state_data->ipmi_ctx,
                                                                             channel_number,
                                                                             IPMI_GET_LAN_PARAMETER,
                                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "callback_level.none", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'callback_level.none': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->callback_level_none = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "callback_level.md2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'callback_level.md2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->callback_level_md2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "callback_level.md5", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'callback_level.md5': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->callback_level_md5 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "callback_level.straight_password", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'callback_level.straight_password': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->callback_level_straight_password = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "callback_level.oem_proprietary", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'callback_level.oem_proprietary': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->callback_level_oem_proprietary = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_level.none", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_level.none': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->user_level_none = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_level.md2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_level.md2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->user_level_md2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_level.md5", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_level.md5': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->user_level_md5 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_level.straight_password", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_level.straight_password': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->user_level_straight_password = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_level.oem_proprietary", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_level.oem_proprietary': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->user_level_oem_proprietary = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "operator_level.none", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'operator_level.none': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->operator_level_none = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "operator_level.md2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'operator_level.md2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->operator_level_md2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "operator_level.md5", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'operator_level.md5': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->operator_level_md5 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "operator_level.straight_password", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'operator_level.straight_password': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->operator_level_straight_password = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "operator_level.oem_proprietary", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'operator_level.oem_proprietary': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->operator_level_oem_proprietary = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "admin_level.none", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'admin_level.none': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->admin_level_none = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "admin_level.md2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'admin_level.md2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->admin_level_md2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "admin_level.md5", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'admin_level.md5': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->admin_level_md5 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "admin_level.straight_password", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'admin_level.straight_password': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->admin_level_straight_password = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "admin_level.oem_proprietary", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'admin_level.oem_proprietary': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->admin_level_oem_proprietary = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_level.none", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_level.none': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->oem_level_none = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_level.md2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_level.md2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->oem_level_md2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_level.md5", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_level.md5': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->oem_level_md5 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_level.straight_password", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_level.straight_password': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->oem_level_straight_password = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_level.oem_proprietary", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_level.oem_proprietary': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  al->oem_level_oem_proprietary = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_authentication_type_enables (bmc_config_state_data_t *state_data,
				  const char *section_name,
                                  struct bmc_authentication_level *al)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (al);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables (state_data->ipmi_ctx,
                                                                             channel_number,
                                                                             al->callback_level_none,
                                                                             al->callback_level_md2,
                                                                             al->callback_level_md5,
                                                                             al->callback_level_straight_password,
                                                                             al->callback_level_oem_proprietary,
                                                                             al->user_level_none,
                                                                             al->user_level_md2,
                                                                             al->user_level_md5,
                                                                             al->user_level_straight_password,
                                                                             al->user_level_oem_proprietary,
                                                                             al->operator_level_none,
                                                                             al->operator_level_md2,
                                                                             al->operator_level_md5,
                                                                             al->operator_level_straight_password,
                                                                             al->operator_level_oem_proprietary,
                                                                             al->admin_level_none,
                                                                             al->admin_level_md2,
                                                                             al->admin_level_md5,
                                                                             al->admin_level_straight_password,
                                                                             al->admin_level_oem_proprietary,
                                                                             al->oem_level_none,
                                                                             al->oem_level_md2,
                                                                             al->oem_level_md5,
                                                                             al->oem_level_straight_password,
                                                                             al->oem_level_oem_proprietary,
                                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      /*
       * IPMI Workaround
       *
       * Dell Poweredge R610
       *
       * Nodes come default w/ OEM authentication enables turned
       * on, but you cannot configure them on.  So this always
       * leads to invalid data errors (0xCC) b/c we are
       * configuring one field at a time.  So we will "absorb" the
       * OEM configuration of later fields and try again, hoping
       * that the user has tried to "right" the badness already
       * sitting on the motherboard.
       */
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
        {
          struct config_section *section;
          struct config_keyvalue *kv;
          
          section = state_data->sections;
          while (section)
            {
              if (!strcasecmp (section->section_name, "Lan_Conf_Auth"))
                break;
              section = section->next;
            }
          
          /* shouldn't be possible */
          if (!section)
            goto cleanup;
          
          if ((kv = config_find_keyvalue (section,
                                          "Callback_Enable_Auth_Type_OEM_Proprietary")))
            al->callback_level_oem_proprietary = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "User_Enable_Auth_Type_OEM_Proprietary")))
            al->user_level_oem_proprietary = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "Operator_Enable_Auth_Type_OEM_Proprietary")))
            al->operator_level_oem_proprietary = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "Admin_Enable_Auth_Type_OEM_Proprietary")))
            al->admin_level_oem_proprietary = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "OEM_Enable_Auth_Type_None")))
            al->oem_level_none = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "OEM_Enable_Auth_Type_MD2")))
            al->oem_level_md2 = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "OEM_Enable_Auth_Type_MD5")))
            al->oem_level_md5 = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "OEM_Enable_Auth_Type_Straight_Password")))
            al->oem_level_straight_password = same (kv->value_input, "yes");
          
          if ((kv = config_find_keyvalue (section,
                                          "OEM_Enable_Auth_Type_OEM_Proprietary")))
            al->oem_level_oem_proprietary = same (kv->value_input, "yes");
          
          if (ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     al->callback_level_none,
                                                                                     al->callback_level_md2,
                                                                                     al->callback_level_md5,
                                                                                     al->callback_level_straight_password,
                                                                                     al->callback_level_oem_proprietary,
                                                                                     al->user_level_none,
                                                                                     al->user_level_md2,
                                                                                     al->user_level_md5,
                                                                                     al->user_level_straight_password,
                                                                                     al->user_level_oem_proprietary,
                                                                                     al->operator_level_none,
                                                                                     al->operator_level_md2,
                                                                                     al->operator_level_md5,
                                                                                     al->operator_level_straight_password,
                                                                                     al->operator_level_oem_proprietary,
                                                                                     al->admin_level_none,
                                                                                     al->admin_level_md2,
                                                                                     al->admin_level_md5,
                                                                                     al->admin_level_straight_password,
                                                                                     al->admin_level_oem_proprietary,
                                                                                     al->oem_level_none,
                                                                                     al->oem_level_md2,
                                                                                     al->oem_level_md5,
                                                                                     al->oem_level_straight_password,
                                                                                     al->oem_level_oem_proprietary,
                                                                                     obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->config_args.common_args.debug)
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables: %s\n",
                                 ipmi_ctx_errormsg (state_data->ipmi_ctx));
              
              if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                          obj_cmd_rs,
                                                          &ret))
                rv = ret;
              
              goto cleanup;
            }
          
          /* success!! */
          goto out;
        }
      else if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                       obj_cmd_rs,
                                                       &ret))
        rv = ret;

      goto cleanup;
    }

 out:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static uint8_t *
_authentication_level_ptr (bmc_config_state_data_t *state_data,
                           const char *section_name,
                           const char *key_name,
                           struct bmc_authentication_level *al)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (al);

  if (!strcasecmp (key_name, "Callback_Enable_Auth_Type_None"))
    return (&al->callback_level_none);
  else if (!strcasecmp (key_name, "Callback_Enable_Auth_Type_MD2"))
    return (&al->callback_level_md2);
  else if (!strcasecmp (key_name, "Callback_Enable_Auth_Type_MD5"))
    return (&al->callback_level_md5);
  else if (!strcasecmp (key_name, "Callback_Enable_Auth_Type_Straight_Password"))
    return (&al->callback_level_straight_password);
  else if (!strcasecmp (key_name, "Callback_Enable_Auth_Type_OEM_Proprietary"))
    return (&al->callback_level_oem_proprietary);
  else if (!strcasecmp (key_name, "User_Enable_Auth_Type_None"))
    return (&al->user_level_none);
  else if (!strcasecmp (key_name, "User_Enable_Auth_Type_MD2"))
    return (&al->user_level_md2);
  else if (!strcasecmp (key_name, "User_Enable_Auth_Type_MD5"))
    return (&al->user_level_md5);
  else if (!strcasecmp (key_name, "User_Enable_Auth_Type_Straight_Password"))
    return (&al->user_level_straight_password);
  else if (!strcasecmp (key_name, "User_Enable_Auth_Type_OEM_Proprietary"))
    return (&al->user_level_oem_proprietary);
  else if (!strcasecmp (key_name, "Operator_Enable_Auth_Type_None"))
    return (&al->operator_level_none);
  else if (!strcasecmp (key_name, "Operator_Enable_Auth_Type_MD2"))
    return (&al->operator_level_md2);
  else if (!strcasecmp (key_name, "Operator_Enable_Auth_Type_MD5"))
    return (&al->operator_level_md5);
  else if (!strcasecmp (key_name, "Operator_Enable_Auth_Type_Straight_Password"))
    return (&al->operator_level_straight_password);
  else if (!strcasecmp (key_name, "Operator_Enable_Auth_Type_OEM_Proprietary"))
    return (&al->operator_level_oem_proprietary);
  else if (!strcasecmp (key_name, "Admin_Enable_Auth_Type_None"))
    return (&al->admin_level_none);
  else if (!strcasecmp (key_name, "Admin_Enable_Auth_Type_MD2"))
    return (&al->admin_level_md2);
  else if (!strcasecmp (key_name, "Admin_Enable_Auth_Type_MD5"))
    return (&al->admin_level_md5);
  else if (!strcasecmp (key_name, "Admin_Enable_Auth_Type_Straight_Password"))
    return (&al->admin_level_straight_password);
  else if (!strcasecmp (key_name, "Admin_Enable_Auth_Type_OEM_Proprietary"))
    return (&al->admin_level_oem_proprietary);
  else if (!strcasecmp (key_name, "OEM_Enable_Auth_Type_None"))
    return (&al->oem_level_none);
  else if (!strcasecmp (key_name, "OEM_Enable_Auth_Type_MD2"))
    return (&al->oem_level_md2);
  else if (!strcasecmp (key_name, "OEM_Enable_Auth_Type_MD5"))
    return (&al->oem_level_md5);
  else if (!strcasecmp (key_name, "OEM_Enable_Auth_Type_Straight_Password"))
    return (&al->oem_level_straight_password);
  else if (!strcasecmp (key_name, "OEM_Enable_Auth_Type_OEM_Proprietary"))
    return (&al->oem_level_oem_proprietary);

  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "Unknown key '%s' in section '%s'\n",
                   key_name,
                   section_name);
  return (NULL);
}

/* based on support flags, determine if checkout is available
 * - if we cannot determine support, we always checkout
 */
static config_err_t
_authentication_type_enable_available (bmc_config_state_data_t *state_data,
                                       const char *section_name,
                                       const char *key_name,
                                       unsigned int *available)
{
  config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (available);

  /* default to always allow checkout */
  *available = 1;

  /* always output under very verbose mode */
  if (state_data->prog_data->args->config_args.verbose_count > 1)
    return (CONFIG_ERR_SUCCESS);

  if ((ret = _get_authentication_type_support (state_data, section_name)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (state_data->authentication_type_initialized)
    {
      if (stristr (key_name, "None")
          && !state_data->authentication_type_none)
        *available = 0;
      else if (stristr (key_name, "MD2")
               && !state_data->authentication_type_md2)
        *available = 0;
      else if (stristr (key_name, "MD5")
               && !state_data->authentication_type_md5)
        *available = 0;
      else if (stristr (key_name, "Straight_Password")
               && !state_data->authentication_type_straight_password)
        *available = 0;
      else if (stristr (key_name, "OEM_Proprietary")
               && !state_data->authentication_type_oem_proprietary)
        *available = 0;
    }

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
_authentication_level_checkout (const char *section_name,
                                struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data;
  struct bmc_authentication_level al;
  config_err_t ret;
  unsigned int available_flag = 1; /* default is to always allow checkout */
  uint8_t *al_ptr;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_authentication_type_enables (state_data,
					       section_name,
                                               &al)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* non-fatal error is ok here */
  ret = _authentication_type_enable_available (state_data,
                                               section_name,
                                               kv->key->key_name,
                                               &available_flag);
  if (ret == CONFIG_ERR_FATAL_ERROR)
    return (ret);

  if (available_flag)
    {
      if (!(al_ptr = _authentication_level_ptr (state_data,
                                                section_name,
                                                kv->key->key_name,
                                                &al)))
        return (CONFIG_ERR_FATAL_ERROR);

      if (config_section_update_keyvalue_output (state_data->pstate,
                                                 kv,
                                                 *al_ptr ? "Yes" : "No") < 0)
        return (CONFIG_ERR_FATAL_ERROR);

      return (CONFIG_ERR_SUCCESS);
    }

  return (CONFIG_ERR_NON_FATAL_ERROR);
}

static config_err_t
_authentication_level_commit (const char *section_name,
                              const struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data;
  struct bmc_authentication_level al;
  config_err_t ret;
  uint8_t *flag;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_authentication_type_enables (state_data,
					       section_name,
                                               &al)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (!(flag = _authentication_level_ptr (state_data,
                                          section_name,
                                          kv->key->key_name,
                                          &al)))
    return (CONFIG_ERR_FATAL_ERROR);

  *flag = same (kv->value_input, "yes");

  if ((ret = _set_authentication_type_enables (state_data,
					       section_name,
                                               &al)) != CONFIG_ERR_SUCCESS)
    return (ret);

  return (CONFIG_ERR_SUCCESS);
}

struct config_section *
bmc_config_lan_conf_auth_section_get (bmc_config_state_data_t *state_data,
				      unsigned int config_flags,
				      int channel_index)
{
  struct config_section *section = NULL;
  char *section_comment =
    "In the Lan_Conf_Auth section, allowable authentication mechanisms for "
    "IPMI 1.5 is configured.  Most users will want to set all \"MD5\" "
    "authentication to \"Yes\" and the rest to \"No\".  If you have "
    "configured a NULL username and a NULL password, you "
    "will also want to configure some of the \"None\" fields to \"Yes\" "
    "to allow \"None\" authentication to work.  Some motherboards do not "
    "allow you to enable OEM authentication, so you may wish to set all "
    "OEM related fields to \"No\".";
  char *section_name_base_str = "Lan_Conf_Auth";

  assert (state_data);

  if (!(section = config_section_multi_channel_create (state_data->pstate,
						       section_name_base_str,
						       section_comment,
						       NULL,
						       NULL,
						       config_flags,
						       channel_index,
						       state_data->lan_channel_numbers,
						       state_data->lan_channel_numbers_count)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Callback_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Callback_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Callback_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Callback_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Callback_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "User_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "User_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "User_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "User_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "User_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Operator_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Operator_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Operator_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Operator_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Operator_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Admin_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Admin_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Admin_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Admin_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Admin_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "OEM_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "OEM_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "OEM_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "OEM_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "OEM_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}
