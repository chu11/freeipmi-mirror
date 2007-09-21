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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define KEY_NAME_CIPHER_SUITE_ID_0  "Maximum_Privilege_Cipher_Suite_Id_0"
#define KEY_NAME_CIPHER_SUITE_ID_1  "Maximum_Privilege_Cipher_Suite_Id_1"
#define KEY_NAME_CIPHER_SUITE_ID_2  "Maximum_Privilege_Cipher_Suite_Id_2"
#define KEY_NAME_CIPHER_SUITE_ID_3  "Maximum_Privilege_Cipher_Suite_Id_3"
#define KEY_NAME_CIPHER_SUITE_ID_4  "Maximum_Privilege_Cipher_Suite_Id_4"
#define KEY_NAME_CIPHER_SUITE_ID_5  "Maximum_Privilege_Cipher_Suite_Id_5"
#define KEY_NAME_CIPHER_SUITE_ID_6  "Maximum_Privilege_Cipher_Suite_Id_6"
#define KEY_NAME_CIPHER_SUITE_ID_7  "Maximum_Privilege_Cipher_Suite_Id_7"
#define KEY_NAME_CIPHER_SUITE_ID_8  "Maximum_Privilege_Cipher_Suite_Id_8"
#define KEY_NAME_CIPHER_SUITE_ID_9  "Maximum_Privilege_Cipher_Suite_Id_9"
#define KEY_NAME_CIPHER_SUITE_ID_10 "Maximum_Privilege_Cipher_Suite_Id_10"
#define KEY_NAME_CIPHER_SUITE_ID_11 "Maximum_Privilege_Cipher_Suite_Id_11"
#define KEY_NAME_CIPHER_SUITE_ID_12 "Maximum_Privilege_Cipher_Suite_Id_12"
#define KEY_NAME_CIPHER_SUITE_ID_13 "Maximum_Privilege_Cipher_Suite_Id_13"
#define KEY_NAME_CIPHER_SUITE_ID_14 "Maximum_Privilege_Cipher_Suite_Id_14"
#define KEY_NAME_CIPHER_SUITE_ID_PREFIX "Maximum_Privilege_Cipher_Suite_Id_"

#define CIPHER_SUITE_LEN 16

static config_err_t
_get_rmcpplus_cipher_suite_id_privileges(bmc_config_state_data_t *state_data,
                                         int debug,
                                         uint8_t *cipher_suite_id_supported,
                                         unsigned int cipher_suite_id_supported_len,
                                         uint8_t *cipher_suite_privilege,
                                         unsigned int cipher_suite_privilege_len,
                                         unsigned int *cipher_suite_entry_count)
{
  fiid_obj_t obj_cmd_count_rs = NULL;
  fiid_obj_t obj_cmd_id_rs = NULL;
  fiid_obj_t obj_cmd_priv_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  int i;

  assert(cipher_suite_id_supported);
  assert(cipher_suite_id_supported_len == CIPHER_SUITE_LEN);
  assert(cipher_suite_privilege);
  assert(cipher_suite_privilege_len == CIPHER_SUITE_LEN);
  assert(cipher_suite_entry_count);

  memset(cipher_suite_id_supported, '\0', sizeof(uint8_t)*cipher_suite_id_supported_len);
  memset(cipher_suite_privilege, '\0', sizeof(uint8_t)*cipher_suite_privilege_len);
  *cipher_suite_entry_count = 0;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!(obj_cmd_count_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support (state_data->dev,
                                                                                               channel_number,
                                                                                               IPMI_GET_LAN_PARAMETER,
                                                                                               SET_SELECTOR,
                                                                                               BLOCK_SELECTOR,
                                                                                               obj_cmd_count_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_count_rs, "cipher_suite_entry_count", &val) < 0)
    goto cleanup;

  *cipher_suite_entry_count = val;
  if (*cipher_suite_entry_count > CIPHER_SUITE_LEN)
    *cipher_suite_entry_count = CIPHER_SUITE_LEN;

  if (!(obj_cmd_id_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs)))
    goto cleanup;
  
  if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries (state_data->dev,
                                                                                         channel_number,
                                                                                         IPMI_GET_LAN_PARAMETER,
                                                                                         SET_SELECTOR,
                                                                                         BLOCK_SELECTOR,
                                                                                         obj_cmd_id_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entriest: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  for (i = 0; i < *cipher_suite_entry_count; i++)
    {
      char *field = NULL;
      
      if (i == 0)
        field = "cipher_suite_id_entry_A";
      else if (i == 1)
        field = "cipher_suite_id_entry_B";
      else if (i == 2)
        field = "cipher_suite_id_entry_C";
      else if (i == 3)
        field = "cipher_suite_id_entry_D";
      else if (i == 4)
        field = "cipher_suite_id_entry_E";
      else if (i == 5)
        field = "cipher_suite_id_entry_F";
      else if (i == 6)
        field = "cipher_suite_id_entry_G";
      else if (i == 7)
        field = "cipher_suite_id_entry_H";
      else if (i == 8)
        field = "cipher_suite_id_entry_I";
      else if (i == 9)
        field = "cipher_suite_id_entry_J";
      else if (i == 10)
        field = "cipher_suite_id_entry_K";
      else if (i == 11)
        field = "cipher_suite_id_entry_L";
      else if (i == 12)
        field = "cipher_suite_id_entry_M";
      else if (i == 13)
        field = "cipher_suite_id_entry_N";
      else if (i == 14)
        field = "cipher_suite_id_entry_O";
      else if (i == 15)
        field = "cipher_suite_id_entry_P";

      if (fiid_obj_get (obj_cmd_id_rs, field, &val) < 0)
        goto cleanup;
      
      cipher_suite_id_supported[i] = val;
    }

  if (!(obj_cmd_priv_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (state_data->dev,
                                                                                                  channel_number,
                                                                                                  IPMI_GET_LAN_PARAMETER,
                                                                                                  SET_SELECTOR,
                                                                                                  BLOCK_SELECTOR,
                                                                                                  obj_cmd_priv_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  /* note: returns fixed number of entries, unlike id supported list above */
  for (i = 0; i < CIPHER_SUITE_LEN; i++)
    {
      char *field = NULL;
      
      if (i == 0)
        field = "maximum_privilege_for_cipher_suite_1";
      else if (i == 1)
        field = "maximum_privilege_for_cipher_suite_2";
      else if (i == 2)
        field = "maximum_privilege_for_cipher_suite_3";
      else if (i == 3)
        field = "maximum_privilege_for_cipher_suite_4";
      else if (i == 4)
        field = "maximum_privilege_for_cipher_suite_5";
      else if (i == 5)
        field = "maximum_privilege_for_cipher_suite_6";
      else if (i == 6)
        field = "maximum_privilege_for_cipher_suite_7";
      else if (i == 7)
        field = "maximum_privilege_for_cipher_suite_8";
      else if (i == 8)
        field = "maximum_privilege_for_cipher_suite_9";
      else if (i == 9)
        field = "maximum_privilege_for_cipher_suite_10";
      else if (i == 10)
        field = "maximum_privilege_for_cipher_suite_11";
      else if (i == 11)
        field = "maximum_privilege_for_cipher_suite_12";
      else if (i == 12)
        field = "maximum_privilege_for_cipher_suite_13";
      else if (i == 13)
        field = "maximum_privilege_for_cipher_suite_14";
      else if (i == 14)
        field = "maximum_privilege_for_cipher_suite_15";
      else if (i == 15)
        field = "maximum_privilege_for_cipher_suite_16";

      if (fiid_obj_get (obj_cmd_priv_rs, field, &val) < 0)
        goto cleanup;

      cipher_suite_privilege[i] = val;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_count_rs)
    fiid_obj_destroy(obj_cmd_count_rs);
  if (obj_cmd_id_rs)
    fiid_obj_destroy(obj_cmd_id_rs);
  if (obj_cmd_priv_rs)
    fiid_obj_destroy(obj_cmd_priv_rs);
  return (rv);
}

static config_err_t
_set_rmcpplus_cipher_suite_id_privileges (bmc_config_state_data_t *state_data,
                                          int debug,
                                          uint8_t *cipher_suite_privilege,
                                          unsigned int cipher_suite_privilege_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(cipher_suite_privilege);
  assert(cipher_suite_privilege_len == CIPHER_SUITE_LEN);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels(state_data->dev,
                                                                                                 channel_number,
                                                                                                 cipher_suite_privilege[0],
                                                                                                 cipher_suite_privilege[1],
                                                                                                 cipher_suite_privilege[2],
                                                                                                 cipher_suite_privilege[3],
                                                                                                 cipher_suite_privilege[4],
                                                                                                 cipher_suite_privilege[5],
                                                                                                 cipher_suite_privilege[6],
                                                                                                 cipher_suite_privilege[7],
                                                                                                 cipher_suite_privilege[8],
                                                                                                 cipher_suite_privilege[9],
                                                                                                 cipher_suite_privilege[10],
                                                                                                 cipher_suite_privilege[11],
                                                                                                 cipher_suite_privilege[12],
                                                                                                 cipher_suite_privilege[13],
                                                                                                 cipher_suite_privilege[14],
                                                                                                 cipher_suite_privilege[15],
                                                                                                 obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_rmcpplus_conf_privilege_checkout(const char *section_name,
                                  struct config_keyvalue *keyvalues,
                                  int debug,
                                  void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  uint8_t cipher_suite_id_supported[CIPHER_SUITE_LEN];
  uint8_t cipher_suite_privilege[CIPHER_SUITE_LEN];
  unsigned int cipher_suite_entry_count;
  config_err_t ret;
  config_err_t rv = CONFIG_ERR_SUCCESS;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_rmcpplus_cipher_suite_id_privileges(state_data,
                                                      debug,
                                                      cipher_suite_id_supported,
                                                      CIPHER_SUITE_LEN,
                                                      cipher_suite_privilege,
                                                      CIPHER_SUITE_LEN,
                                                      &cipher_suite_entry_count)) != CONFIG_ERR_SUCCESS)
    return ret;

  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      if (strstr(kv->key->key_name, KEY_NAME_CIPHER_SUITE_ID_PREFIX))
        {
          uint8_t cipher_suite_id;
          uint8_t privilege;
          int id_found = 0;
          int i;

          cipher_suite_id = atoi(kv->key->key_name + strlen(KEY_NAME_CIPHER_SUITE_ID_PREFIX));

          for (i = 0; i < cipher_suite_entry_count; i++)
            {
              if (cipher_suite_id_supported[i] == cipher_suite_id)
                {
                  privilege = cipher_suite_privilege[cipher_suite_id];
                  id_found++;
                  break;
                }
            }

          if (id_found)
            {
              if (config_section_update_keyvalue(kv,
                                                 NULL,
                                                 rmcpplus_privilege_string(privilege)) < 0)
                {
                  if (debug)
                    fprintf(stderr, "config_section_update_keyvalue error\n");
                  return CONFIG_ERR_FATAL_ERROR;
                }
            }
          else
            rv = CONFIG_ERR_NON_FATAL_ERROR;
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

  return rv;
}

static config_err_t
_rmcpplus_conf_privilege_commit(const char *section_name,
                                struct config_keyvalue *keyvalues,
                                int debug,
                                void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  uint8_t cipher_suite_id_supported[CIPHER_SUITE_LEN];
  uint8_t cipher_suite_privilege[CIPHER_SUITE_LEN];
  unsigned int cipher_suite_entry_count;
  config_err_t ret;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_rmcpplus_cipher_suite_id_privileges(state_data,
                                                      debug,
                                                      cipher_suite_id_supported,
                                                      CIPHER_SUITE_LEN,
                                                      cipher_suite_privilege,
                                                      CIPHER_SUITE_LEN,
                                                      &cipher_suite_entry_count)) != CONFIG_ERR_SUCCESS)
    return ret;

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      if (strstr(kv->key->key_name, KEY_NAME_CIPHER_SUITE_ID_PREFIX))
        {
          uint8_t cipher_suite_id;
          uint8_t privilege;

          cipher_suite_id = atoi(kv->key->key_name + strlen(KEY_NAME_CIPHER_SUITE_ID_PREFIX));
          privilege = rmcpplus_privilege_number(kv->value_input);
          cipher_suite_privilege[cipher_suite_id] = privilege;
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

  if ((ret = _set_rmcpplus_cipher_suite_id_privileges(state_data,
                                                      debug,
                                                      cipher_suite_privilege,
                                                      CIPHER_SUITE_LEN)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

struct config_section *
bmc_config_rmcpplus_conf_privilege_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *rmcpplus_conf_privilege_section = NULL;
  char *section_comment = 
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL),"
    "cipher suite IDs may be configurable below.  In the "
    "Rmcpplus_Conf_Privilege section, maximum user privilege levels "
    "allowed for authentication under IPMI 2.0 (including Serial-over-LAN) "
    "are set for each supported cipher suite ID.  Each cipher suite ID "
    "supports different sets of authentication, integrity, and encryption "
    "algorithms for IPMI 2.0.  Typically, the highest privilege level any "
    "username configured should set for support under a cipher suite ID. "
    "This is typically \"Administrator\".";

  if (!(rmcpplus_conf_privilege_section = config_section_create ("Rmcpplus_Conf_Privilege",
                                                                 "Rmcpplus_Conf_Privilege",
                                                                 section_comment,
                                                                 0,
                                                                 _rmcpplus_conf_privilege_checkout,
                                                                 _rmcpplus_conf_privilege_commit)))

    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_0,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_1,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_2,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_3,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_4,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_5,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_6,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_7,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_8,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_9,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_10,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_11,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_12,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_13,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (rmcpplus_conf_privilege_section,
                              KEY_NAME_CIPHER_SUITE_ID_14,
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              rmcpplus_privilege_number_validate) < 0)
    goto cleanup;

  return rmcpplus_conf_privilege_section;

 cleanup:
  if (rmcpplus_conf_privilege_section)
    config_section_destroy (rmcpplus_conf_privilege_section);
  return NULL;
}

