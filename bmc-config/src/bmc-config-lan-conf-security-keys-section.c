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

#include "tool-common.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define KEY_NAME_K_R "K_R"
#define KEY_NAME_K_G "K_G"

static config_validate_t
k_r_validate (const char *section_name,
              const char *key_name,
	      const char *value)
{
  if (strlen (value) <= IPMI_MAX_K_R_LENGTH)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

static config_validate_t
k_g_validate (const char *section_name,
              const char *key_name,
	      const char *value)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];

  if (parse_kg(k_g, IPMI_MAX_K_G_LENGTH + 1, value) < 0)
    return CONFIG_VALIDATE_INVALID_VALUE;
  return CONFIG_VALIDATE_VALID_VALUE;
}

static config_err_t
_get_key(bmc_config_state_data_t *state_data,
         int debug,
         uint8_t *keybuf,
         uint32_t keybuflen,
         uint8_t key_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t buf[BMC_CONFIG_BUFLEN];
  uint32_t buf_len;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(keybuf);
  assert(keybuflen);
  assert(key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R
         || key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_security_keys_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_channel_security_keys (state_data->dev,
                                          channel_number,
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_READ_KEY,
                                          key_id,
                                          NULL,
                                          0,
                                          obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_channel_security_keys: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((buf_len = fiid_obj_get_data (obj_cmd_rs, "key_value", buf, BMC_CONFIG_BUFLEN)) < 0)
    goto cleanup;

  if (keybuflen < buf_len)
    {
      if (debug)
        fprintf(stderr,
                "keybuflen short\n");
      goto cleanup;
    }
  memcpy(keybuf, buf, buf_len);

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
_set_key(bmc_config_state_data_t *state_data,
         int debug,
         uint8_t *keybuf,
         uint32_t keybuflen,
         uint8_t key_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(keybuf);
  /* no keybuflen check, can be 0 to clear a keybuf */
  assert(key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R
         || key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_security_keys_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_channel_security_keys (state_data->dev,
                                          channel_number,
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY,
                                          key_id,
                                          keybuf,
                                          keybuflen,
                                          obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_channel_security_keys: %s\n",
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
_lan_conf_security_keys_checkout(const char *section_name,
                                 struct config_keyvalue *keyvalues,
                                 int debug,
                                 void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_K_R))
        {
          uint8_t k_r[IPMI_MAX_K_R_LENGTH+1];

          memset (k_r, 0, IPMI_MAX_K_R_LENGTH+1);

          if ((ret = _get_key(state_data,
                              debug,
                              k_r,
                              IPMI_MAX_K_R_LENGTH,
                              IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          
          if (ret == CONFIG_ERR_SUCCESS)
            {
              k_r[IPMI_MAX_K_R_LENGTH] = '\0';
              if (config_section_update_keyvalue(kv,
                                                 NULL,
                                                 (char *)k_r) < 0)
                {
                  if (debug)
                    fprintf(stderr, "config_section_update_keyvalue error\n");
                  return CONFIG_ERR_FATAL_ERROR;
                }
            }
          else
            rv = ret;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_K_G))
        {
          uint8_t k_g[IPMI_MAX_K_G_LENGTH];

          memset (k_g, '\0', IPMI_MAX_K_G_LENGTH);

          if ((ret = _get_key(state_data,
                              debug,
                              k_g,
                              IPMI_MAX_K_G_LENGTH,
                              IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;

          if (ret == CONFIG_ERR_SUCCESS)
            {
              if (state_data->prog_data->args->action == CONFIG_ACTION_DIFF)
                {
                  char kv_k_g[IPMI_MAX_K_G_LENGTH+1];

                  /* There is an exception on a diff here, since the input from
                   * the user may come in two different forms.
                   *
                   * If the user passed in something that we can confirm is the
                   * same as what's on the BMC, copy back to the user whatever
                   * they passed in.
                   */
                  assert(kv->value_input);

                  memset (kv_k_g, '\0', IPMI_MAX_K_G_LENGTH+1);
                  
                  if (parse_kg(kv_k_g, IPMI_MAX_K_G_LENGTH + 1, kv->value_input) < 0)
                    return CONFIG_ERR_FATAL_ERROR;

                  /* compare binary, not string representation */
                  if (!memcmp(kv_k_g, k_g, IPMI_MAX_K_G_LENGTH))
                    {
                      if (config_section_update_keyvalue(kv,
                                                         NULL,
                                                         kv->value_input) < 0)
                        {
                          if (debug)
                            fprintf(stderr, "config_section_update_keyvalue error\n");
                          return CONFIG_ERR_FATAL_ERROR;
                        }
                    }
                }
              else
                {
                  char k_g_buf[IPMI_MAX_K_G_LENGTH*2 + 3];
                  
                  memset (k_g_buf, '\0', IPMI_MAX_K_G_LENGTH*2 + 3);
                  
                  if (!format_kg(k_g_buf, IPMI_MAX_K_G_LENGTH*2+3, (unsigned char *)k_g))
                    return CONFIG_ERR_FATAL_ERROR;
                  
                  if (config_section_update_keyvalue(kv,
                                                     NULL,
                                                     (char *)k_g_buf) < 0)
                    {
                      if (debug)
                        fprintf(stderr, "config_section_update_keyvalue error\n");
                      return CONFIG_ERR_FATAL_ERROR;
                    }
                }
            }
          else
            rv = ret;
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
_lan_conf_security_keys_commit(const char *section_name,
                               struct config_keyvalue *keyvalues,
                               int debug,
                               void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_K_R))
        {
          if ((ret = _set_key(state_data,
                              debug,
                              (uint8_t *)kv->value_input,
                              strlen (kv->value_input),
                              IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret != CONFIG_ERR_SUCCESS)
            rv = ret;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_K_G))
        {
          uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
          int k_g_len;
          
          memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);
          
          if ((k_g_len = parse_kg(k_g, IPMI_MAX_K_G_LENGTH+1, kv->value_input)) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          
          if ((ret = _set_key(state_data,
                              debug,
                              k_g,
                              k_g_len,
                              IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret != CONFIG_ERR_SUCCESS)
            rv = ret;
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

struct config_section *
bmc_config_lan_conf_security_keys_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_conf_security_keys_section = NULL;
  char *section_comment = 
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), a "
    "K_g BMC key may be configurable.  The K_g key is an optional key that "
    "can be set for two key authentication in IPMI 2.0.  It is optionally "
    "configured.  Most users will may to set this to zero (or blank).";

  if (!(lan_conf_security_keys_section = config_section_create ("Lan_Conf_Security_Keys",
                                                                "Lan_Conf_Security_Keys",
                                                                section_comment,
                                                                0,
                                                                _lan_conf_security_keys_checkout,
                                                                _lan_conf_security_keys_commit)))
    goto cleanup;

  if (config_section_add_key (lan_conf_security_keys_section,
                              KEY_NAME_K_R,
                              "Give string or blank to clear. Max 20 chars",
                              0,
                              k_r_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_security_keys_section,
                              KEY_NAME_K_G,
                              "Give string or blank to clear. Max 20 bytes, prefix with 0x to enter hex",
                              0,
                              k_g_validate) < 0)
    goto cleanup;

  return lan_conf_security_keys_section;

 cleanup:
  if (lan_conf_security_keys_section)
    config_section_destroy(lan_conf_security_keys_section);
  return NULL;
}
