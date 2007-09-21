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
#include "bmc-config-wrapper.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define KEY_NAME_CALLBACK_NONE               "Callback_Enable_Auth_Type_None"
#define KEY_NAME_CALLBACK_MD2                "Callback_Enable_Auth_Type_MD2"
#define KEY_NAME_CALLBACK_MD5                "Callback_Enable_Auth_Type_MD5"
#define KEY_NAME_CALLBACK_STRAIGHT_PASSWORD  "Callback_Enable_Auth_Type_Straight_Password"
#define KEY_NAME_CALLBACK_OEM_PROPRIETARY    "Callback_Enable_Auth_Type_OEM_Proprietary"

#define KEY_NAME_USER_NONE               "User_Enable_Auth_Type_None"
#define KEY_NAME_USER_MD2                "User_Enable_Auth_Type_MD2"
#define KEY_NAME_USER_MD5                "User_Enable_Auth_Type_MD5"
#define KEY_NAME_USER_STRAIGHT_PASSWORD  "User_Enable_Auth_Type_Straight_Password"
#define KEY_NAME_USER_OEM_PROPRIETARY    "User_Enable_Auth_Type_OEM_Proprietary"

#define KEY_NAME_OPERATOR_NONE               "Operator_Enable_Auth_Type_None"
#define KEY_NAME_OPERATOR_MD2                "Operator_Enable_Auth_Type_MD2"
#define KEY_NAME_OPERATOR_MD5                "Operator_Enable_Auth_Type_MD5"
#define KEY_NAME_OPERATOR_STRAIGHT_PASSWORD  "Operator_Enable_Auth_Type_Straight_Password"
#define KEY_NAME_OPERATOR_OEM_PROPRIETARY    "Operator_Enable_Auth_Type_OEM_Proprietary"

#define KEY_NAME_ADMIN_NONE               "Admin_Enable_Auth_Type_None"
#define KEY_NAME_ADMIN_MD2                "Admin_Enable_Auth_Type_MD2"
#define KEY_NAME_ADMIN_MD5                "Admin_Enable_Auth_Type_MD5"
#define KEY_NAME_ADMIN_STRAIGHT_PASSWORD  "Admin_Enable_Auth_Type_Straight_Password"
#define KEY_NAME_ADMIN_OEM_PROPRIETARY    "Admin_Enable_Auth_Type_OEM_Proprietary"

#define KEY_NAME_OEM_NONE               "OEM_Enable_Auth_Type_None"
#define KEY_NAME_OEM_MD2                "OEM_Enable_Auth_Type_MD2"
#define KEY_NAME_OEM_MD5                "OEM_Enable_Auth_Type_MD5"
#define KEY_NAME_OEM_STRAIGHT_PASSWORD  "OEM_Enable_Auth_Type_Straight_Password"
#define KEY_NAME_OEM_OEM_PROPRIETARY    "OEM_Enable_Auth_Type_OEM_Proprietary"

/* convenience struct */
struct authentication_type_enables {
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

config_err_t 
_get_lan_conf_authentication_type_enables (bmc_config_state_data_t *state_data, 
                                           int debug,
                                           struct authentication_type_enables *ate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  assert(ate);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs)))
    goto cleanup;
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables (state_data->dev, 
									     channel_number, 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr, 
                "ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.none", &val) < 0)
    goto cleanup;
  ate->callback_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.md2", &val) < 0)
    goto cleanup;
  ate->callback_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.md5", &val) < 0)
    goto cleanup;
  ate->callback_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.straight_password", &val) < 0)
    goto cleanup;
  ate->callback_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.oem_proprietary", &val) < 0)
    goto cleanup;
  ate->callback_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.none", &val) < 0)
    goto cleanup;
  ate->user_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.md2", &val) < 0)
    goto cleanup;
  ate->user_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.md5", &val) < 0)
    goto cleanup;
  ate->user_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.straight_password", &val) < 0)
    goto cleanup;
  ate->user_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.oem_proprietary", &val) < 0)
    goto cleanup;
  ate->user_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.none", &val) < 0)
    goto cleanup;
  ate->operator_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.md2", &val) < 0)
    goto cleanup;
  ate->operator_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.md5", &val) < 0)
    goto cleanup;
  ate->operator_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.straight_password", &val) < 0)
    goto cleanup;
  ate->operator_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.oem_proprietary", &val) < 0)
    goto cleanup;
  ate->operator_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.none", &val) < 0)
    goto cleanup;
  ate->admin_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.md2", &val) < 0)
    goto cleanup;
  ate->admin_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.md5", &val) < 0)
    goto cleanup;
  ate->admin_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.straight_password", &val) < 0)
    goto cleanup;
  ate->admin_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.oem_proprietary", &val) < 0)
    goto cleanup;
  ate->admin_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.none", &val) < 0)
    goto cleanup;
  ate->oem_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.md2", &val) < 0)
    goto cleanup;
  ate->oem_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.md5", &val) < 0)
    goto cleanup;
  ate->oem_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.straight_password", &val) < 0)
    goto cleanup;
  ate->oem_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.oem_proprietary", &val) < 0)
    goto cleanup;
  ate->oem_level_oem_proprietary = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_set_lan_conf_authentication_type_enables (bmc_config_state_data_t *state_data, 
                                           int debug,
                                           struct authentication_type_enables *ate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(ate);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables (state_data->dev,
                                                                             channel_number, 
                                                                             ate->callback_level_none,
                                                                             ate->callback_level_md2,
                                                                             ate->callback_level_md5,
                                                                             ate->callback_level_straight_password,
                                                                             ate->callback_level_oem_proprietary,
                                                                             ate->user_level_none,
                                                                             ate->user_level_md2,
                                                                             ate->user_level_md5,
                                                                             ate->user_level_straight_password,
                                                                             ate->user_level_oem_proprietary,
                                                                             ate->operator_level_none,
                                                                             ate->operator_level_md2,
                                                                             ate->operator_level_md5,
                                                                             ate->operator_level_straight_password,
                                                                             ate->operator_level_oem_proprietary,
                                                                             ate->admin_level_none,
                                                                             ate->admin_level_md2,
                                                                             ate->admin_level_md5,
                                                                             ate->admin_level_straight_password,
                                                                             ate->admin_level_oem_proprietary,
                                                                             ate->oem_level_none,
                                                                             ate->oem_level_md2,
                                                                             ate->oem_level_md5,
                                                                             ate->oem_level_straight_password,
                                                                             ate->oem_level_oem_proprietary,
                                                                             obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr, 
                "ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables: %s\n",
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

static config_err_t
_lan_conf_auth_checkout(const char *section_name,
                        struct config_keyvalue *keyvalues,
                        int debug,
                        void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  struct authentication_type_enables ate;
  config_err_t ret;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  memset(&ate, '\0', sizeof(struct authentication_type_enables));

  if ((ret = _get_lan_conf_authentication_type_enables(state_data, 
                                                       debug,
                                                       &ate)) != CONFIG_ERR_SUCCESS)
    return ret;

  kv = keyvalues;
  while (kv)
    {
      uint8_t flag;

      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_NONE))
        flag = ate.callback_level_none;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_MD2))
        flag = ate.callback_level_md2;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_MD5))
        flag = ate.callback_level_md5;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_STRAIGHT_PASSWORD))
        flag = ate.callback_level_straight_password;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_OEM_PROPRIETARY))
        flag = ate.callback_level_oem_proprietary;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_NONE))
        flag = ate.user_level_none;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_MD2))
        flag = ate.user_level_md2;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_MD5))
        flag = ate.user_level_md5;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_STRAIGHT_PASSWORD))
        flag = ate.user_level_straight_password;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_OEM_PROPRIETARY))
        flag = ate.user_level_oem_proprietary;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_NONE))
        flag = ate.operator_level_none;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_MD2))
        flag = ate.operator_level_md2;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_MD5))
        flag = ate.operator_level_md5;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_STRAIGHT_PASSWORD))
        flag = ate.operator_level_straight_password;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_OEM_PROPRIETARY))
        flag = ate.operator_level_oem_proprietary;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_NONE))
        flag = ate.admin_level_none;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_MD2))
        flag = ate.admin_level_md2;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_MD5))
        flag = ate.admin_level_md5;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_STRAIGHT_PASSWORD))
        flag = ate.admin_level_straight_password;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_OEM_PROPRIETARY))
        flag = ate.admin_level_oem_proprietary;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_NONE))
        flag = ate.oem_level_none;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_MD2))
        flag = ate.oem_level_md2;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_MD5))
        flag = ate.oem_level_md5;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_STRAIGHT_PASSWORD))
        flag = ate.oem_level_straight_password;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_OEM_PROPRIETARY))
        flag = ate.oem_level_oem_proprietary;

      if (config_section_update_keyvalue(kv,
                                         NULL,
                                         flag ? "Yes" : "No") < 0)
        {
          if (debug)
            fprintf(stderr, "config_section_update_keyvalue error\n");
          return CONFIG_ERR_FATAL_ERROR;
        }
          
      kv = kv->next;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_lan_conf_auth_commit(const char *section_name,
                      struct config_keyvalue *keyvalues,
                      int debug,
                      void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  struct authentication_type_enables ate;
  config_err_t ret;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  memset(&ate, '\0', sizeof(struct authentication_type_enables));

  if ((ret = _get_lan_conf_authentication_type_enables(state_data, 
                                                       debug,
                                                       &ate)) != CONFIG_ERR_SUCCESS)
    return ret;

  kv = keyvalues;
  while (kv)
    {
      uint8_t flag;

      assert(kv->value_input);

      flag = same (kv->value_input, "yes");

      if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_NONE))
        ate.callback_level_none = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_MD2))
        ate.callback_level_md2 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_MD5))
        ate.callback_level_md5 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_STRAIGHT_PASSWORD))
        ate.callback_level_straight_password = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALLBACK_OEM_PROPRIETARY))
        ate.callback_level_oem_proprietary = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_NONE))
        ate.user_level_none = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_MD2))
        ate.user_level_md2 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_MD5))
        ate.user_level_md5 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_STRAIGHT_PASSWORD))
        ate.user_level_straight_password = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_USER_OEM_PROPRIETARY))
        ate.user_level_oem_proprietary = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_NONE))
        ate.operator_level_none = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_MD2))
        ate.operator_level_md2 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_MD5))
        ate.operator_level_md5 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_STRAIGHT_PASSWORD))
        ate.operator_level_straight_password = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OPERATOR_OEM_PROPRIETARY))
        ate.operator_level_oem_proprietary = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_NONE))
        ate.admin_level_none = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_MD2))
        ate.admin_level_md2 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_MD5))
        ate.admin_level_md5 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_STRAIGHT_PASSWORD))
        ate.admin_level_straight_password = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ADMIN_OEM_PROPRIETARY))
        ate.admin_level_oem_proprietary = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_NONE))
        ate.oem_level_none = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_MD2))
        ate.oem_level_md2 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_MD5))
        ate.oem_level_md5 = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_STRAIGHT_PASSWORD))
        ate.oem_level_straight_password = flag;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_OEM_OEM_PROPRIETARY))
        ate.oem_level_oem_proprietary = flag;
          
      kv = kv->next;
    }

  if ((ret = _set_lan_conf_authentication_type_enables(state_data, 
                                                       debug,
                                                       &ate)) != CONFIG_ERR_SUCCESS)
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
    "to allow \"None\" authentication to work.";

  if (!(lan_conf_auth_section = config_section_create("Lan_Conf_Auth",
                                                      "Lan_Conf_Auth",
                                                      section_comment,
                                                      0,
                                                      _lan_conf_auth_checkout,
                                                      _lan_conf_auth_commit)))
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_CALLBACK_NONE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_CALLBACK_MD2,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_CALLBACK_MD5,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_CALLBACK_STRAIGHT_PASSWORD,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_CALLBACK_OEM_PROPRIETARY,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_USER_NONE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_USER_MD2,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_USER_MD5,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_USER_STRAIGHT_PASSWORD,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_USER_OEM_PROPRIETARY,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OPERATOR_NONE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OPERATOR_MD2,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OPERATOR_MD5,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OPERATOR_STRAIGHT_PASSWORD,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OPERATOR_OEM_PROPRIETARY,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_ADMIN_NONE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_ADMIN_MD2,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_ADMIN_MD5,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_ADMIN_STRAIGHT_PASSWORD,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_ADMIN_OEM_PROPRIETARY,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OEM_NONE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OEM_MD2,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OEM_MD5,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OEM_STRAIGHT_PASSWORD,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_auth_section,
                              KEY_NAME_OEM_OEM_PROPRIETARY,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  return lan_conf_auth_section;

 cleanup:
  if (lan_conf_auth_section)
    config_section_destroy(lan_conf_auth_section);
  return NULL;
}
