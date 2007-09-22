#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-channel-common.h"
#include "bmc-config-map.h"
#include "bmc-config-utils.h"
#include "bmc-config-validate.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define KEY_NAME_VOLATILE_ACCESS_MODE             "Volatile_Access_Mode"
#define KEY_NAME_VOLATILE_ENABLE_USER_LEVEL_AUTH  "Volatile_Enable_User_Level_Auth"
#define KEY_NAME_VOLATILE_ENABLE_PER_MESSAGE_AUTH "Volatile_Enable_Per_Message_Auth"
#define KEY_NAME_VOLATILE_ENABLE_PEF_ALERTING     "Volatile_Enable_Pef_Alerting"
#define KEY_NAME_VOLATILE_PRIVILEGE_LIMIT         "Volatile_Channel_Privilege_Limit"

#define KEY_NAME_NON_VOLATILE_ACCESS_MODE             "Non_Volatile_Access_Mode"
#define KEY_NAME_NON_VOLATILE_ENABLE_USER_LEVEL_AUTH  "Non_Volatile_Enable_User_Level_Auth"
#define KEY_NAME_NON_VOLATILE_ENABLE_PER_MESSAGE_AUTH "Non_Volatile_Enable_Per_Message_Auth"
#define KEY_NAME_NON_VOLATILE_ENABLE_PEF_ALERTING     "Non_Volatile_Enable_Pef_Alerting"
#define KEY_NAME_NON_VOLATILE_PRIVILEGE_LIMIT         "Non_Volatile_Channel_Privilege_Limit"

static config_err_t
_get_channel_access(bmc_config_state_data_t *state_data,
                    int debug,
                    uint8_t channel_number,
                    uint8_t channel_access,
                    uint8_t *access_mode,
                    uint8_t *user_level_authentication,
                    uint8_t *per_message_authentication,
                    uint8_t *pef_alerting,
                    uint8_t *privilege_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(channel_access == IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE
         || channel_access == IPMI_CHANNEL_ACCESS_GET_VOLATILE);
  assert(access_mode);
  assert(user_level_authentication);
  assert(per_message_authentication);
  assert(pef_alerting);
  assert(privilege_limit);

  if (ipmi_cmd_get_channel_access (state_data->dev,
                                   channel_number,
                                   channel_access,
                                   obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_channel_access: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "ipmi_messaging_access_mode", &val) < 0)
    goto cleanup;
  *access_mode = val;

  if (fiid_obj_get (obj_cmd_rs, "user_level_authentication", &val) < 0)
    goto cleanup;
  /* user_level_authentication is "backwards" in logic, so flip it */
  *user_level_authentication = (val ? 0 : 1);

  if (fiid_obj_get (obj_cmd_rs, "per_message_authentication", &val) < 0)
    goto cleanup;
  /* per_message_authentication is "backwards" in logic, so flip it */
  *per_message_authentication = (val ? 0 : 1);

  if (fiid_obj_get (obj_cmd_rs, "pef_alerting", &val) < 0)
    goto cleanup;
  /* pef_alerting is "backwards" in logic, so flip it */
  *pef_alerting = (val ? 0 : 1);

  if (fiid_obj_get (obj_cmd_rs, "channel_privilege_level_limit", &val) < 0)
    goto cleanup;
  *privilege_limit = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_channel_access(bmc_config_state_data_t *state_data,
                    int debug,
                    uint8_t channel_number,
                    uint8_t channel_access,
                    uint8_t access_mode,
                    uint8_t user_level_authentication,
                    uint8_t per_message_authentication,
                    uint8_t pef_alerting,
                    uint8_t privilege_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(channel_access == IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE
         || channel_access == IPMI_CHANNEL_ACCESS_SET_VOLATILE);
  assert(access_mode);
  assert(user_level_authentication);
  assert(per_message_authentication);
  assert(pef_alerting);
  assert(privilege_limit);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_access_rs)))
    goto cleanup;

  /* channel access flags are backwards, so we need to flip them */
  if (ipmi_cmd_set_channel_access (state_data->dev,
                                   channel_number,
                                   access_mode,
                                   (user_level_authentication ? 0 : 1),
                                   (per_message_authentication ? 0 : 1),
                                   (pef_alerting ? 0 : 1),
                                   channel_access,
                                   privilege_limit,
                                   ((channel_access == IPMI_CHANNEL_ACCESS_SET_VOLATILE)
                                    ? IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VOLATILE :
                                    IPMI_PRIVILEGE_LEVEL_LIMIT_SET_NON_VOLATILE),
                                   obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_channel_access: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
channel_checkout(const char *section_name,
                 struct config_keyvalue *keyvalues,
                 int debug,
                 void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  uint8_t volatile_checkout = 0;
  uint8_t volatile_checkout_success = 0;
  uint8_t non_volatile_checkout = 0;
  uint8_t non_volatile_checkout_success = 0;
  uint8_t channel_number;
  uint8_t channel_number_retrieve_success = 0;
  uint8_t volatile_access_mode;
  uint8_t volatile_user_level_authentication;
  uint8_t volatile_per_message_authentication;
  uint8_t volatile_pef_alerting;
  uint8_t volatile_privilege_limit;
  uint8_t non_volatile_access_mode;
  uint8_t non_volatile_user_level_authentication;
  uint8_t non_volatile_per_message_authentication;
  uint8_t non_volatile_pef_alerting;
  uint8_t non_volatile_privilege_limit;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  /* two passes through the list is minimally slower but makes the
   * code far simpler
   */
  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ACCESS_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_USER_LEVEL_AUTH)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PEF_ALERTING)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_PRIVILEGE_LIMIT))
        volatile_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ACCESS_MODE)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_USER_LEVEL_AUTH)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PEF_ALERTING)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_PRIVILEGE_LIMIT))
        non_volatile_checkout++;
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
               section_name);
        }
      
      kv = kv->next;
    }
  
  if (volatile_checkout || non_volatile_checkout)
    {
      if (!strcasecmp(section_name, "Serial_Channel"))
        {
          if ((ret = get_serial_channel_number (state_data,
                                                &channel_number)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            channel_number_retrieve_success++;
          else
            rv = ret;
        }
      else if (!strcasecmp(section_name, "Lan_Channel"))
        {
          if ((ret = get_lan_channel_number (state_data,
                                             &channel_number)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            channel_number_retrieve_success++;
          else
            rv = ret;
        }
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown section '%s'\n",
                    section_name);
        }
    }

  if (channel_number_retrieve_success)
    {
      if (volatile_checkout)
        {
          if ((ret = _get_channel_access(state_data,
                                         debug,
                                         channel_number,
                                         IPMI_CHANNEL_ACCESS_GET_VOLATILE,
                                         &volatile_access_mode,
                                         &volatile_user_level_authentication,
                                         &volatile_per_message_authentication,
                                         &volatile_pef_alerting,
                                         &volatile_privilege_limit)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            volatile_checkout_success++;
          else
            rv = ret;
        }
      
      if (non_volatile_checkout)
        {
          if ((ret = _get_channel_access(state_data,
                                         debug,
                                         channel_number,
                                         IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE,
                                         &non_volatile_access_mode,
                                         &non_volatile_user_level_authentication,
                                         &non_volatile_per_message_authentication,
                                         &non_volatile_pef_alerting,
                                         &non_volatile_privilege_limit)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            non_volatile_checkout_success++;
          else
            rv = ret;
        }
    }

  if (volatile_checkout_success || non_volatile_checkout_success)
    {
      kv = keyvalues;
      while (kv)
        {
          int temp = 0;
          
          assert(!kv->value_output);
          
          if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ACCESS_MODE)
              && volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  channel_access_mode_string(volatile_access_mode));
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_USER_LEVEL_AUTH)
                   && volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  volatile_user_level_authentication ? "Yes" : "No");
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
                   && volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  volatile_per_message_authentication ? "Yes" : "No");
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PEF_ALERTING)
                   && volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  volatile_pef_alerting ? "Yes" : "No");
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_PRIVILEGE_LIMIT)
                   && volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  privilege_level_string(volatile_privilege_limit));
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ACCESS_MODE)
                   && non_volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  channel_access_mode_string(non_volatile_access_mode));
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_USER_LEVEL_AUTH)
                   && non_volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  non_volatile_user_level_authentication ? "Yes" : "No");
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
                   && non_volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  non_volatile_per_message_authentication ? "Yes" : "No");
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PEF_ALERTING)
                   && non_volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  non_volatile_pef_alerting ? "Yes" : "No");
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_PRIVILEGE_LIMIT)
                   && non_volatile_checkout_success)
            temp = config_section_update_keyvalue(kv,
                                                  NULL,
                                                  privilege_level_string(non_volatile_privilege_limit));
          else
            {
              if (debug)
                fprintf(stderr,
                        "ERROR: Unknown key '%s' in '%s'\n",
                        kv->key->key_name,
                        section_name);
            }
          
          if (temp < 0)
            {
              if (debug)
                fprintf(stderr, "config_section_update_keyvalue error\n");
              return CONFIG_ERR_FATAL_ERROR;
            }
          
          kv = kv->next;
        }
    }

  return rv;
}

config_err_t
channel_commit(const char *section_name,
               struct config_keyvalue *keyvalues,
               int debug,
               void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  uint8_t volatile_checkout = 0;
  uint8_t volatile_checkout_success = 0;
  uint8_t non_volatile_checkout = 0;
  uint8_t non_volatile_checkout_success = 0;
  uint8_t channel_number;
  uint8_t channel_number_retrieve_success = 0;
  uint8_t volatile_commit = 0;
  uint8_t non_volatile_commit = 0;
  uint8_t volatile_access_mode;
  uint8_t volatile_user_level_authentication;
  uint8_t volatile_per_message_authentication;
  uint8_t volatile_pef_alerting;
  uint8_t volatile_privilege_limit;
  uint8_t non_volatile_access_mode;
  uint8_t non_volatile_user_level_authentication;
  uint8_t non_volatile_per_message_authentication;
  uint8_t non_volatile_pef_alerting;
  uint8_t non_volatile_privilege_limit;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  /* two passes through the list is minimally slower but makes the
   * code far simpler
   */

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ACCESS_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_USER_LEVEL_AUTH)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PEF_ALERTING)
          || !strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_PRIVILEGE_LIMIT))
        volatile_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ACCESS_MODE)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_USER_LEVEL_AUTH)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PEF_ALERTING)
               || !strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_PRIVILEGE_LIMIT))
        non_volatile_checkout++;
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
               section_name);
        }
      
      kv = kv->next;
    }
  
  if (volatile_checkout || non_volatile_checkout)
    {
      if (!strcasecmp(section_name, "Serial_Channel"))
        {
          if ((ret = get_serial_channel_number (state_data,
                                                &channel_number)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            channel_number_retrieve_success++;
          else
            rv = ret;
        }
      else if (!strcasecmp(section_name, "Lan_Channel"))
        {
          if ((ret = get_lan_channel_number (state_data,
                                             &channel_number)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            channel_number_retrieve_success++;
          else
            rv = ret;
        }
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown section '%s'\n",
                    section_name);
        }
    }

  if (channel_number_retrieve_success)
    {
      if (volatile_checkout)
        {
          if ((ret = _get_channel_access(state_data,
                                         debug,
                                         channel_number,
                                         IPMI_CHANNEL_ACCESS_GET_VOLATILE,
                                         &volatile_access_mode,
                                         &volatile_user_level_authentication,
                                         &volatile_per_message_authentication,
                                         &volatile_pef_alerting,
                                         &volatile_privilege_limit)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            volatile_checkout_success++;
          else
            rv = ret;
        }
      
      if (non_volatile_checkout)
        {
          if ((ret = _get_channel_access(state_data,
                                         debug,
                                         channel_number,
                                         IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE,
                                         &non_volatile_access_mode,
                                         &non_volatile_user_level_authentication,
                                         &non_volatile_per_message_authentication,
                                         &non_volatile_pef_alerting,
                                         &non_volatile_privilege_limit)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret == CONFIG_ERR_SUCCESS)
            non_volatile_checkout_success++;
          else
            rv = ret;
        }
    }

  if (volatile_checkout_success || non_volatile_checkout_success)
    {
      kv = keyvalues;
      while (kv)
        {
          assert(kv->value_input);
          
          if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ACCESS_MODE)
              && volatile_checkout_success)
            {
              volatile_access_mode = channel_access_mode(kv->value_input);
              volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_USER_LEVEL_AUTH)
                   && volatile_checkout_success)
            {
              volatile_user_level_authentication = same (kv->value_input, "yes");
              volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
                   && volatile_checkout_success)
            {
              volatile_per_message_authentication = same (kv->value_input, "yes");
              volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_ENABLE_PEF_ALERTING)
                   && volatile_checkout_success)
            {
              volatile_pef_alerting = same (kv->value_input, "yes");
              volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_VOLATILE_PRIVILEGE_LIMIT)
                   && volatile_checkout_success)
            {
              volatile_privilege_limit = privilege_level_number(kv->value_input);
              volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ACCESS_MODE)
              && non_volatile_checkout_success)
            {
              non_volatile_access_mode = channel_access_mode(kv->value_input);
              non_volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_USER_LEVEL_AUTH)
                   && non_volatile_checkout_success)
            {
              non_volatile_user_level_authentication = same (kv->value_input, "yes");
              non_volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PER_MESSAGE_AUTH)
                   && non_volatile_checkout_success)
            {
              non_volatile_per_message_authentication = same (kv->value_input, "yes");
              non_volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_ENABLE_PEF_ALERTING)
                   && non_volatile_checkout_success)
            {
              non_volatile_pef_alerting = same (kv->value_input, "yes");
              non_volatile_commit++;
            }
          else if (!strcasecmp(kv->key->key_name, KEY_NAME_NON_VOLATILE_PRIVILEGE_LIMIT)
                   && non_volatile_checkout_success)
            {
              non_volatile_privilege_limit = privilege_level_number(kv->value_input);
              non_volatile_commit++;
            }
          else
            {
              if (debug)
                fprintf(stderr,
                        "ERROR: Unknown key '%s' in '%s'\n",
                        kv->key->key_name,
                        section_name);
            }
          
          kv = kv->next;
        }
    }

  if (volatile_commit)
    {
      if ((ret = _set_channel_access(state_data,
                                     debug,
                                     channel_number,
                                     IPMI_CHANNEL_ACCESS_SET_VOLATILE,
                                     volatile_access_mode,
                                     volatile_user_level_authentication,
                                     volatile_per_message_authentication,
                                     volatile_pef_alerting,
                                     volatile_privilege_limit)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  if (non_volatile_commit)
    {
      if ((ret = _set_channel_access(state_data,
                                     debug,
                                     channel_number,
                                     IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE,
                                     non_volatile_access_mode,
                                     non_volatile_user_level_authentication,
                                     non_volatile_per_message_authentication,
                                     non_volatile_pef_alerting,
                                     non_volatile_privilege_limit)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  return rv;
}

int
channel_section_get(bmc_config_state_data_t *state_data,
                    struct config_section *channel_section)
{
  assert(state_data);
  assert(channel_section);

  if (config_section_add_key (channel_section,
                              KEY_NAME_VOLATILE_ACCESS_MODE,
                              "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                              0,
                              channel_access_mode_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_VOLATILE_ENABLE_USER_LEVEL_AUTH,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_VOLATILE_ENABLE_PER_MESSAGE_AUTH,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_VOLATILE_ENABLE_PEF_ALERTING,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_VOLATILE_PRIVILEGE_LIMIT,
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              privilege_level_number_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_NON_VOLATILE_ACCESS_MODE,
                              "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                              0,
                              channel_access_mode_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_NON_VOLATILE_ENABLE_USER_LEVEL_AUTH,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_NON_VOLATILE_ENABLE_PER_MESSAGE_AUTH,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_NON_VOLATILE_ENABLE_PEF_ALERTING,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    return -1;

  if (config_section_add_key (channel_section,
                              KEY_NAME_NON_VOLATILE_PRIVILEGE_LIMIT,
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              privilege_level_number_validate) < 0)
    return -1;

  return 0;
}
