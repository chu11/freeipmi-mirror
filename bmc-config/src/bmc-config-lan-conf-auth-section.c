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
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

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
_get_authentication_type_support (bmc_config_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_authentication_type_support_rs);
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_authentication_type_support (state_data->ipmi_ctx,
                                                                             channel_number,
                                                                             IPMI_GET_LAN_PARAMETER,
                                                                             SET_SELECTOR,
                                                                             BLOCK_SELECTOR,
                                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_authentication_type_support: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "none", &val);
  state_data->authentication_type_none = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "md2", &val);
  state_data->authentication_type_md2 = val;

  _FIID_OBJ_GET (obj_cmd_rs, "md5", &val);
  state_data->authentication_type_md5 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "straight_password", &val);
  state_data->authentication_type_straight_password = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "oem_proprietary", &val);
  state_data->authentication_type_oem_proprietary = val;
  
  state_data->authentication_type_initialized++;
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv); 
}

static config_err_t 
_get_authentication_type_enables (bmc_config_state_data_t *state_data,
                                  struct bmc_authentication_level *al)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  assert(al);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs);
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables (state_data->ipmi_ctx, 
									     channel_number, 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET (obj_cmd_rs, "callback_level.none", &val);
  al->callback_level_none = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "callback_level.md2", &val);
  al->callback_level_md2 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "callback_level.md5", &val);
  al->callback_level_md5 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "callback_level.straight_password", &val);
  al->callback_level_straight_password = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "callback_level.oem_proprietary", &val);
  al->callback_level_oem_proprietary = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "user_level.none", &val);
  al->user_level_none = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "user_level.md2", &val);
  al->user_level_md2 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "user_level.md5", &val);
  al->user_level_md5 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "user_level.straight_password", &val);
  al->user_level_straight_password = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "user_level.oem_proprietary", &val);
  al->user_level_oem_proprietary = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "operator_level.none", &val);
  al->operator_level_none = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "operator_level.md2", &val);
  al->operator_level_md2 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "operator_level.md5", &val);
  al->operator_level_md5 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "operator_level.straight_password", &val);
  al->operator_level_straight_password = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "operator_level.oem_proprietary", &val);
  al->operator_level_oem_proprietary = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "admin_level.none", &val);
  al->admin_level_none = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "admin_level.md2", &val);
  al->admin_level_md2 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "admin_level.md5", &val);
  al->admin_level_md5 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "admin_level.straight_password", &val);
  al->admin_level_straight_password = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "admin_level.oem_proprietary", &val);
  al->admin_level_oem_proprietary = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "oem_level.none", &val);
  al->oem_level_none = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "oem_level.md2", &val);
  al->oem_level_md2 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "oem_level.md5", &val);
  al->oem_level_md5 = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "oem_level.straight_password", &val);
  al->oem_level_straight_password = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "oem_level.oem_proprietary", &val);
  al->oem_level_oem_proprietary = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_set_authentication_type_enables (bmc_config_state_data_t *state_data,
                                  struct bmc_authentication_level *al)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(al);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
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
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        {
          /*
           * IPMI Workaround
           *
           * Dell Poweredge R610
           *
           * Nodes come default w/ OEM authentication enables turned
           * on, but you cannot configure them on.  So this always
           * leads to invalid data errors (0xCC) b/c we are
           * configuring one field at a time (and continuing
           * non-this-field to whatever is on the motherboard).  So we
           * will "absorb" the OEM configuration of later fields and
           * try again, hoping that the user has tried to "right" the
           * badness already sitting on the motherboard.
           */
          if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
              && (ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
            {
              if (state_data->lan_conf_auth_callback_level_oem_proprietary_set)
                al->callback_level_oem_proprietary = state_data->lan_conf_auth_callback_level_oem_proprietary;
              if (state_data->lan_conf_auth_user_level_oem_proprietary_set)
                al->user_level_oem_proprietary = state_data->lan_conf_auth_user_level_oem_proprietary;
              if (state_data->lan_conf_auth_operator_level_oem_proprietary_set)
                al->operator_level_oem_proprietary = state_data->lan_conf_auth_operator_level_oem_proprietary;
              if (state_data->lan_conf_auth_admin_level_oem_proprietary_set)
                al->admin_level_oem_proprietary = state_data->lan_conf_auth_admin_level_oem_proprietary;
              if (state_data->lan_conf_auth_oem_level_none_set)
                al->oem_level_none = state_data->lan_conf_auth_oem_level_none;
              if (state_data->lan_conf_auth_oem_level_md2_set)
                al->oem_level_md2 = state_data->lan_conf_auth_oem_level_md2;
              if (state_data->lan_conf_auth_oem_level_md5_set)
                al->oem_level_md5 = state_data->lan_conf_auth_oem_level_md5;
              if (state_data->lan_conf_auth_oem_level_straight_password_set)
                al->oem_level_straight_password = state_data->lan_conf_auth_oem_level_straight_password;
              if (state_data->lan_conf_auth_oem_level_oem_proprietary_set)
                al->oem_level_oem_proprietary = state_data->lan_conf_auth_oem_level_oem_proprietary;
              
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
                  if (state_data->prog_data->args->config_args.common.debug)
                    pstdout_fprintf (state_data->pstate,
                                     stderr,
                                     "ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables: %s\n",
                                     ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
                  rv = CONFIG_ERR_NON_FATAL_ERROR;
                  goto cleanup;
                }
              
              /* success!! */
              goto out;
            }
          else
            rv = CONFIG_ERR_NON_FATAL_ERROR;
        }
      goto cleanup;
    }

 out:  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static uint8_t *
_authentication_level_ptr (bmc_config_state_data_t *state_data,
                           const char *section_name,
                           const char *key_name,
                           struct bmc_authentication_level *al)
{
  assert(state_data);
  assert(key_name);
  assert(al);

  if (!strcasecmp(key_name, "Callback_Enable_Auth_Type_None"))
    return &al->callback_level_none;
  else if (!strcasecmp(key_name, "Callback_Enable_Auth_Type_MD2"))
    return &al->callback_level_md2;
  else if (!strcasecmp(key_name, "Callback_Enable_Auth_Type_MD5"))
    return &al->callback_level_md5;
  else if (!strcasecmp(key_name, "Callback_Enable_Auth_Type_Straight_Password"))
    return &al->callback_level_straight_password;
  else if (!strcasecmp(key_name, "Callback_Enable_Auth_Type_OEM_Proprietary"))
    return &al->callback_level_oem_proprietary;
  else if (!strcasecmp(key_name, "User_Enable_Auth_Type_None"))
    return &al->user_level_none;
  else if (!strcasecmp(key_name, "User_Enable_Auth_Type_MD2"))
    return &al->user_level_md2;
  else if (!strcasecmp(key_name, "User_Enable_Auth_Type_MD5"))
    return &al->user_level_md5;
  else if (!strcasecmp(key_name, "User_Enable_Auth_Type_Straight_Password"))
    return &al->user_level_straight_password;
  else if (!strcasecmp(key_name, "User_Enable_Auth_Type_OEM_Proprietary"))
    return &al->user_level_oem_proprietary;
  else if (!strcasecmp(key_name, "Operator_Enable_Auth_Type_None"))
    return &al->operator_level_none;
  else if (!strcasecmp(key_name, "Operator_Enable_Auth_Type_MD2"))
    return &al->operator_level_md2;
  else if (!strcasecmp(key_name, "Operator_Enable_Auth_Type_MD5"))
    return &al->operator_level_md5;
  else if (!strcasecmp(key_name, "Operator_Enable_Auth_Type_Straight_Password"))
    return &al->operator_level_straight_password;
  else if (!strcasecmp(key_name, "Operator_Enable_Auth_Type_OEM_Proprietary"))
    return &al->operator_level_oem_proprietary;
  else if (!strcasecmp(key_name, "Admin_Enable_Auth_Type_None"))
    return &al->admin_level_none;
  else if (!strcasecmp(key_name, "Admin_Enable_Auth_Type_MD2"))
    return &al->admin_level_md2;
  else if (!strcasecmp(key_name, "Admin_Enable_Auth_Type_MD5"))
    return &al->admin_level_md5;
  else if (!strcasecmp(key_name, "Admin_Enable_Auth_Type_Straight_Password"))
    return &al->admin_level_straight_password;
  else if (!strcasecmp(key_name, "Admin_Enable_Auth_Type_OEM_Proprietary"))
    return &al->admin_level_oem_proprietary;
  else if (!strcasecmp(key_name, "OEM_Enable_Auth_Type_None"))
    return &al->oem_level_none;
  else if (!strcasecmp(key_name, "OEM_Enable_Auth_Type_MD2"))
    return &al->oem_level_md2;
  else if (!strcasecmp(key_name, "OEM_Enable_Auth_Type_MD5"))
    return &al->oem_level_md5;
  else if (!strcasecmp(key_name, "OEM_Enable_Auth_Type_Straight_Password"))
    return &al->oem_level_straight_password;
  else if (!strcasecmp(key_name, "OEM_Enable_Auth_Type_OEM_Proprietary"))
    return &al->oem_level_oem_proprietary;

  pstdout_fprintf(state_data->pstate,
                  stderr, 
                  "Unknown key '%s' in section '%s'\n", 
                  key_name,
                  section_name);
  return NULL;
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

  assert(state_data);
  assert(key_name);
  assert(available);

  /* default to always allow checkout */
  *available = 1;
          
  /* always output under verbose mode */
  if (state_data->prog_data->args->config_args.verbose)
    return CONFIG_ERR_SUCCESS;

  if (!state_data->authentication_type_initialized)
    {
      if ((ret = _get_authentication_type_support (state_data)) != CONFIG_ERR_SUCCESS)
        return ret;
    }

  if (state_data->authentication_type_initialized)
    {
      if (stristr(key_name, "None") && !state_data->authentication_type_none)
        *available = 0;
      else if (stristr(key_name, "MD2") && !state_data->authentication_type_md2)
        *available = 0;
      else if (stristr(key_name, "MD5") && !state_data->authentication_type_md5)
        *available = 0;
      else if (stristr(key_name, "Straight_Password") && !state_data->authentication_type_straight_password)
        *available = 0;
      else if (stristr(key_name, "OEM_Proprietary") && !state_data->authentication_type_oem_proprietary)
        *available = 0;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_authentication_level_checkout (const char *section_name,
                                struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct bmc_authentication_level al;
  config_err_t ret;
  unsigned int available_flag = 1; /* default is to always allow checkout */
  uint8_t *al_ptr;

  if ((ret = _get_authentication_type_enables (state_data, 
                                               &al)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* non-fatal error is ok here */
  ret = _authentication_type_enable_available (state_data,
                                               section_name,
                                               kv->key->key_name,
                                               &available_flag);
  if (ret == CONFIG_ERR_FATAL_ERROR)
    return ret;
      
  if (available_flag) 
    {
      if (!(al_ptr = _authentication_level_ptr(state_data,
                                               section_name, 
                                               kv->key->key_name, 
                                               &al)))
        return CONFIG_ERR_FATAL_ERROR;
      
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv,
                                                *al_ptr ? "Yes" : "No") < 0)
        return CONFIG_ERR_FATAL_ERROR;

      return CONFIG_ERR_SUCCESS;
    }
  
  return CONFIG_ERR_NON_FATAL_ERROR;
}

static config_err_t
_authentication_level_commit (const char *section_name,
                              const struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct bmc_authentication_level al;
  config_err_t ret;
  uint8_t *flag;

  if ((ret = _get_authentication_type_enables (state_data, 
                                               &al)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(flag = _authentication_level_ptr(state_data,
                                         section_name, 
                                         kv->key->key_name, 
                                         &al)))
    return CONFIG_ERR_FATAL_ERROR;
  
  *flag = same (kv->value_input, "yes");

  if ((ret = _set_authentication_type_enables (state_data, 
                                               &al)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

struct config_section *
bmc_config_lan_conf_auth_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_conf_auth_section = NULL;
  char *section_comment =
    "In the Lan_Conf_Auth section, allowable authentication mechanisms for "
    "IPMI 1.5 is configured.  Most users will want to set all \"MD5\" "
    "authentication to \"Yes\" and the rest to \"No\".  If you have "
    "configured a NULL username and a NULL password, you "
    "will also want to configure some of the \"None\" fields to \"Yes\" "
    "to allow \"None\" authentication to work.  Some motherboards do not "
    "allow you to enable OEM authentication, so you may wish to set all "
    "OEM related fields to \"No\".";

  if (!(lan_conf_auth_section = config_section_create(state_data->pstate,
                                                      "Lan_Conf_Auth",
                                                      "Lan_Conf_Auth",
                                                      section_comment,
                                                      0,
                                                      NULL,
                                                      NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Callback_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Callback_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Callback_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Callback_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Callback_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "User_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "User_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "User_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "User_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "User_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Operator_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Operator_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Operator_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Operator_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Operator_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Admin_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Admin_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Admin_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Admin_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "Admin_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "OEM_Enable_Auth_Type_None",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "OEM_Enable_Auth_Type_MD2",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "OEM_Enable_Auth_Type_MD5",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "OEM_Enable_Auth_Type_Straight_Password",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_auth_section,
                              "OEM_Enable_Auth_Type_OEM_Proprietary",
                              "Possible values: Yes/No",
                              0,
                              _authentication_level_checkout,
                              _authentication_level_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  return lan_conf_auth_section;

 cleanup:
  if (lan_conf_auth_section)
    config_section_destroy(state_data->pstate, lan_conf_auth_section);
  return NULL;
}
