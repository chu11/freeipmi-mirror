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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

/* convenience struct */
struct channel_access
{
  uint8_t access_mode;
  uint8_t user_level_authentication;
  uint8_t per_message_authentication;
  uint8_t pef_alerting;
  uint8_t channel_privilege_limit;
};

static config_err_t
_channel_info(bmc_config_state_data_t *state_data,
              const char *section_name,
              uint8_t *channel_number)
{
  config_err_t ret;

  assert(state_data);
  assert(section_name);
  assert(channel_number);

  if (stristr(section_name, "Lan"))
    {
      if ((ret = get_lan_channel_number (state_data,
                                         channel_number)) != CONFIG_ERR_SUCCESS)
        return ret;
    }
  else
    {
      if ((ret = get_serial_channel_number (state_data,
                                            channel_number)) != CONFIG_ERR_SUCCESS)
        return ret;
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_get_key_info(bmc_config_state_data_t *state_data,
              const char *section_name,
              const char *key_name,
              uint8_t *channel_number,
              uint8_t *access_type)
{
  config_err_t ret;

  assert(state_data);
  assert(section_name);
  assert(key_name);
  assert(channel_number);
  assert(access_type);

  if ((ret = _channel_info(state_data, 
                           section_name, 
                           channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* Must check for Non_Volatile b/c Volatile is a substring of the former */
  if (stristr(key_name, "Non_Volatile"))
    *access_type = IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE;
  else
    *access_type = IPMI_CHANNEL_ACCESS_GET_VOLATILE;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_set_key_info(bmc_config_state_data_t *state_data,
              const char *section_name,
              const char *key_name,
              uint8_t *channel_number,
              uint8_t *access_type)
{
  config_err_t ret;

  assert(state_data);
  assert(section_name);
  assert(key_name);
  assert(channel_number);
  assert(access_type);

  if ((ret = _channel_info(state_data, 
                           section_name, 
                           channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* Must check for Non_Volatile b/c Volatile is a substring of the former */
  if (stristr(key_name, "Non_Volatile"))
    *access_type = IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE;
  else
    *access_type = IPMI_CHANNEL_ACCESS_SET_VOLATILE;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_get_channel_access (bmc_config_state_data_t *state_data,
                     const char *section_name,
                     const char *key_name,
                     struct channel_access *ch)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t access_type;
  uint64_t val;

  assert(state_data);
  assert(section_name);
  assert(key_name);
  assert(ch);

  if ((ret = _get_key_info(state_data,
                           section_name,
                           key_name,
                           &channel_number,
                           &access_type)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_channel_access_rs);

  if (ipmi_cmd_get_channel_access (state_data->ipmi_ctx,
                                   channel_number,
                                   access_type,
                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_channel_access: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "ipmi_messaging_access_mode", &val);
  ch->access_mode = val;

  /* yes/no is backwards here, see ipmi spec */
  _FIID_OBJ_GET (obj_cmd_rs, "user_level_authentication", &val);
  ch->user_level_authentication = (val ? 0 : 1);

  /* yes/no is backwards here, see ipmi spec */
  _FIID_OBJ_GET (obj_cmd_rs, "per_message_authentication", &val);
  ch->per_message_authentication = (val ? 0 : 1);

  /* yes/no is backwards here, see ipmi spec */
  _FIID_OBJ_GET (obj_cmd_rs, "pef_alerting", &val);
  ch->pef_alerting = (val ? 0 : 1);

  _FIID_OBJ_GET (obj_cmd_rs, "channel_privilege_level_limit", &val);
  ch->channel_privilege_limit = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_channel_access (bmc_config_state_data_t *state_data,
                     const char *section_name,
                     const char *key_name,
                     struct channel_access *ch)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t set_type;
  
  assert(state_data);
  assert(section_name);
  assert(key_name);
  assert(ch);

  if ((ret = _set_key_info(state_data,
                           section_name,
                           key_name,
                           &channel_number,
                           &set_type)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_channel_access_rs);

  /* yes/no is backwards several places, see ipmi spec */

  if (ipmi_cmd_set_channel_access (state_data->ipmi_ctx,
                                   channel_number,
                                   ch->access_mode,
                                   (ch->user_level_authentication ? 0 : 1),
                                   (ch->per_message_authentication ? 0 : 1),
                                   (ch->pef_alerting ? 0 : 1),
                                   set_type,
                                   ch->channel_privilege_limit,
                                   (set_type == IPMI_CHANNEL_ACCESS_SET_VOLATILE 
                                    ? IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VOLATILE 
                                    : IPMI_PRIVILEGE_LEVEL_LIMIT_SET_NON_VOLATILE),
                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_channel_access: %s\n",
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
_access_mode_checkout (const char *section_name,
                       struct config_keyvalue *kv,
                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            channel_access_mode_string (ch.access_mode)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_access_mode_commit (const char *section_name,
                     const struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  ch.access_mode = channel_access_mode (kv->value_input);

  if ((ret = _set_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_enable_user_level_authentication_checkout (const char *section_name,
                                            struct config_keyvalue *kv,
                                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* achu: Backwards values in this command are handled above */
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ch.user_level_authentication ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_enable_user_level_authentication_commit (const char *section_name,
                                          const struct config_keyvalue *kv,
                                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  ch.user_level_authentication = same (kv->value_input, "yes");

  if ((ret = _set_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_enable_per_message_authentication_checkout (const char *section_name,
                                             struct config_keyvalue *kv,
                                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* achu: Backwards values in this command are handled above */
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            ch.per_message_authentication ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_enable_per_message_authentication_commit (const char *section_name,
                                           const struct config_keyvalue *kv,
                                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  ch.per_message_authentication = same (kv->value_input, "yes");

  if ((ret = _set_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_enable_pef_alerting_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* achu: Backwards values in this command are handled above */
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            ch.pef_alerting ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_enable_pef_alerting_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  ch.pef_alerting = same (kv->value_input, "yes");

  if ((ret = _set_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_channel_privilege_limit_checkout (const char *section_name,
                                   struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            privilege_level_string (ch.channel_privilege_limit)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_channel_privilege_limit_commit (const char *section_name,
                                 const struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct channel_access ch;
  config_err_t ret;

  if ((ret = _get_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;

  ch.channel_privilege_limit = privilege_level_number (kv->value_input);

  if ((ret = _set_channel_access(state_data, 
                                 section_name, 
                                 kv->key->key_name, 
                                 &ch)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  return CONFIG_ERR_SUCCESS;
}

int
bmc_config_channel_common_section_get(bmc_config_state_data_t *state_data,
                                      struct config_section *channel_section,
                                      unsigned int verbose_flags)
{
  assert(state_data);
  assert(channel_section);

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Volatile_Access_Mode",
                              "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                              verbose_flags,
                              _access_mode_checkout,
                              _access_mode_commit,
                              channel_access_mode_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Volatile_Enable_User_Level_Auth",
                              "Possible values: Yes/No",
                              verbose_flags,
                              _enable_user_level_authentication_checkout,
                              _enable_user_level_authentication_commit,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Volatile_Enable_Per_Message_Auth",
                              "Possible values: Yes/No",
                              verbose_flags,
                              _enable_per_message_authentication_checkout,
                              _enable_per_message_authentication_commit,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Volatile_Enable_Pef_Alerting",
                              "Possible values: Yes/No",
                              verbose_flags,
                              _enable_pef_alerting_checkout,
                              _enable_pef_alerting_commit,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Volatile_Channel_Privilege_Limit",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                              verbose_flags,
                              _channel_privilege_limit_checkout,
                              _channel_privilege_limit_commit,
                              privilege_level_number_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Non_Volatile_Access_Mode",
                              "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                              verbose_flags,
                              _access_mode_checkout,
                              _access_mode_commit,
                              channel_access_mode_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Non_Volatile_Enable_User_Level_Auth",
                              "Possible values: Yes/No",
                              verbose_flags,
                              _enable_user_level_authentication_checkout,
                              _enable_user_level_authentication_commit,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Non_Volatile_Enable_Per_Message_Auth",
                              "Possible values: Yes/No",
                              verbose_flags,
                              _enable_per_message_authentication_checkout,
                              _enable_per_message_authentication_commit,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Non_Volatile_Enable_Pef_Alerting",
                              "Possible values: Yes/No",
                              verbose_flags,
                              _enable_pef_alerting_checkout,
                              _enable_pef_alerting_commit,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (state_data->pstate,
                              channel_section,
                              "Non_Volatile_Channel_Privilege_Limit",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                              verbose_flags,
                              _channel_privilege_limit_checkout,
                              _channel_privilege_limit_commit,
                              privilege_level_number_validate) < 0)
    return -1;

  return 0;
}
