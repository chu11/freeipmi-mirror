/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-util-common.h"

static config_err_t
_get_key (bmc_config_state_data_t *state_data,
	  const char *section_name,
          uint8_t key_type,
          void *key,
          unsigned int key_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t buf[CONFIG_PARSE_BUFLEN];
  int buf_len;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (key_type == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R
          || key_type == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G);
  assert (key);
  assert (key_len);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_channel_security_keys_rs)))
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

  if (ipmi_cmd_set_channel_security_keys (state_data->ipmi_ctx,
                                          channel_number,
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_READ_KEY,
                                          key_type,
                                          NULL,
                                          0,
                                          obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_channel_security_keys: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      if (!IPMI_ERRNUM_IS_FATAL_ERROR (state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((buf_len = fiid_obj_get_data (obj_cmd_rs,
                                    "key_value",
                                    buf,
                                    CONFIG_PARSE_BUFLEN)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'key_value': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (key_len < buf_len)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_set_channel_security_keys: short buffer\n");
      goto cleanup;
    }
  memcpy (key, buf, buf_len);

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_key (bmc_config_state_data_t *state_data,
	  const char *section_name,
          uint8_t key_type,
          const void *key,
          unsigned int key_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (key_type == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R
          || key_type == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G);
  assert (key);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_channel_security_keys_rs)))
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

  if (ipmi_cmd_set_channel_security_keys (state_data->ipmi_ctx,
                                          channel_number,
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY,
                                          key_type,
                                          key,
                                          key_len,
                                          obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_channel_security_keys: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      if (!IPMI_ERRNUM_IS_FATAL_ERROR (state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
k_r_checkout (const char *section_name,
              struct config_keyvalue *kv,
              void *arg)
{
  bmc_config_state_data_t *state_data;
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (bmc_config_state_data_t *)arg;

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);
  
  if ((ret = _get_key (state_data,
		       section_name,
                       IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R,
                       k_r,
                       IPMI_MAX_K_R_LENGTH)) != CONFIG_ERR_SUCCESS)
    return (ret);
  
  k_r[IPMI_MAX_K_R_LENGTH] = '\0';
  
  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             (char *)k_r) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
k_r_commit (const char *section_name,
            const struct config_keyvalue *kv,
            void *arg)
{
  bmc_config_state_data_t *state_data;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (bmc_config_state_data_t *)arg;

  return (_set_key (state_data,
		    section_name,
                    IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R,
                    kv->value_input,
                    strlen (kv->value_input)));
}

static config_validate_t
k_r_validate (const char *section_name,
              const char *key_name,
              const char *value,
              void *arg)
{
  assert (section_name);
  assert (key_name);
  assert (value);

  if (strlen (value) <= IPMI_MAX_K_R_LENGTH)
    return (CONFIG_VALIDATE_VALID_VALUE);
  return (CONFIG_VALIDATE_INVALID_VALUE);
}

static config_err_t
k_g_checkout (const char *section_name,
              struct config_keyvalue *kv,
              void *arg)
{
  bmc_config_state_data_t *state_data;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  char k_g_str[IPMI_MAX_K_G_LENGTH*2+3];
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (bmc_config_state_data_t *)arg;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);

  if ((ret = _get_key (state_data,
		       section_name,
                       IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G,
                       k_g,
                       IPMI_MAX_K_G_LENGTH)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* a printable k_g key can have two representations, so compare the
   * binary keys and return what the user passed in if they are the
   * same.
   */

  if (state_data->prog_data->args->config_args.action == CONFIG_ACTION_DIFF)
    {
      uint8_t kv_k_g[IPMI_MAX_K_G_LENGTH+1];

      memset (kv_k_g, '\0', IPMI_MAX_K_G_LENGTH);
      if (parse_kg (kv_k_g, IPMI_MAX_K_G_LENGTH + 1, kv->value_input) < 0)
        return (CONFIG_ERR_FATAL_ERROR);

      if (!memcmp (kv_k_g, k_g, IPMI_MAX_K_G_LENGTH))
        {
          if (config_section_update_keyvalue_output (state_data->pstate,
                                                     kv,
                                                     kv->value_input) < 0)
            return (CONFIG_ERR_FATAL_ERROR);

          return (CONFIG_ERR_SUCCESS);
        }
      /* else, fall through and return the default checked out value */
    }

  memset (k_g_str, '\0', IPMI_MAX_K_G_LENGTH*2+3);
  if (!format_kg (k_g_str, IPMI_MAX_K_G_LENGTH*2+3, k_g))
    return (CONFIG_ERR_FATAL_ERROR);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             k_g_str) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
k_g_commit (const char *section_name,
            const struct config_keyvalue *kv,
            void *arg)
{
  bmc_config_state_data_t *state_data;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
  int k_g_len;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (bmc_config_state_data_t *)arg;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);

  if ((k_g_len = parse_kg (k_g, IPMI_MAX_K_G_LENGTH + 1, kv->value_input)) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (_set_key (state_data,
		    section_name,
                    IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G,
                    k_g,
                    k_g_len));
}

static config_validate_t
k_g_validate (const char *section_name,
              const char *key_name,
              const char *value,
              void *arg)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];

  assert (section_name);
  assert (key_name);
  assert (value);

  if (parse_kg (k_g, IPMI_MAX_K_G_LENGTH + 1, value) < 0)
    return (CONFIG_VALIDATE_INVALID_VALUE);
  return (CONFIG_VALIDATE_VALID_VALUE);
}

struct config_section *
bmc_config_lan_conf_security_keys_section_get (bmc_config_state_data_t *state_data,
					       unsigned int config_flags,
					       int channel_index)
{
  struct config_section *section = NULL;
  char *section_comment =
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), a "
    "K_g BMC key may be configurable.  The K_g key is an optional key that "
    "can be set for two key authentication in IPMI 2.0.  It is optionally "
    "configured.  Most users will want to set this to zero (or blank).";
  char *section_name_base_str = "Lan_Conf_Security_Keys";

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
                              "K_R",
                              "Give string or blank to clear. Max 20 chars",
                              0,
                              k_r_checkout,
                              k_r_commit,
                              k_r_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "K_G",
                              "Give string or blank to clear. Max 20 bytes, prefix with 0x to enter hex",
                              0,
                              k_g_checkout,
                              k_g_commit,
                              k_g_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}
