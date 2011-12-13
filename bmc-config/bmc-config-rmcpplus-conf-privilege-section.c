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

#define BMC_CONFIG_FIELD_LENGTH_MAX 128

#define BMC_CONFIG_PRIVILEGE_LEVEL_SUPPORTED_BUT_NOT_READABLE 0xFF

static config_err_t
_rmcpplus_cipher_suite_id_privilege_setup (bmc_config_state_data_t *state_data,
					   const char *section_name)
{
  fiid_obj_t obj_cmd_count_rs = NULL;
  fiid_obj_t obj_cmd_id_rs = NULL;
  fiid_obj_t obj_cmd_priv_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  unsigned int i;

  assert (state_data);
  assert (section_name);

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (state_data->cipher_suite_entry_count_set
      && state_data->cipher_suite_id_supported_set
      && state_data->cipher_suite_priv_set
      && state_data->cipher_suite_channel_number == channel_number)
    return (CONFIG_ERR_SUCCESS);

  state_data->cipher_suite_entry_count = 0;
  state_data->cipher_suite_entry_count_set = 0;
  state_data->cipher_suite_id_supported_set = 0;
  state_data->cipher_suite_priv_set = 0;
  state_data->cipher_suite_channel_number = channel_number;

  memset (state_data->cipher_suite_id_supported, '\0', sizeof (state_data->cipher_suite_id_supported));
  memset (state_data->cipher_suite_priv, '\0', sizeof (state_data->cipher_suite_priv));

  if (!state_data->cipher_suite_entry_count_set)
    {
      if (!(obj_cmd_count_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_create: %s\n",
                           strerror (errno));
          goto cleanup;
        }

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support (state_data->ipmi_ctx,
                                                                                                   channel_number,
                                                                                                   IPMI_GET_LAN_PARAMETER,
                                                                                                   IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                                   IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                                   obj_cmd_count_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));

          if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                      obj_cmd_count_rs,
                                                      &ret))
            rv = ret;

          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_count_rs, "cipher_suite_entry_count", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'cipher_suite_entry_count': %s\n",
                           fiid_obj_errormsg (obj_cmd_count_rs));
          goto cleanup;
        }

      state_data->cipher_suite_entry_count = val;

      if (state_data->cipher_suite_entry_count > CIPHER_SUITE_LEN)
        state_data->cipher_suite_entry_count = CIPHER_SUITE_LEN;

      state_data->cipher_suite_entry_count_set++;
    }

  if (state_data->cipher_suite_entry_count && !state_data->cipher_suite_id_supported_set)
    {
      if (!(obj_cmd_id_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_create: %s\n",
                           strerror (errno));
          goto cleanup;
        }

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries (state_data->ipmi_ctx,
                                                                                             channel_number,
                                                                                             IPMI_GET_LAN_PARAMETER,
                                                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                             obj_cmd_id_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));

          if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                      obj_cmd_id_rs,
                                                      &ret))
            rv = ret;

          goto cleanup;
        }

      for (i = 0; i < state_data->cipher_suite_entry_count; i++)
        {
          char field[BMC_CONFIG_FIELD_LENGTH_MAX + 1];

          memset (field, '\0', BMC_CONFIG_FIELD_LENGTH_MAX + 1);
          
          snprintf (field,
                    BMC_CONFIG_FIELD_LENGTH_MAX,
                    "cipher_suite_id_entry_%c",
                    'A' + i);
          
          if (FIID_OBJ_GET (obj_cmd_id_rs, field, &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: '%s': %s\n",
                               field,
                               fiid_obj_errormsg (obj_cmd_id_rs));
              goto cleanup;
            }

          state_data->cipher_suite_id_supported[i] = val;
        }

      state_data->cipher_suite_id_supported_set++;
    }

  if (state_data->cipher_suite_entry_count && !state_data->cipher_suite_priv_set)
    {
      if (!(obj_cmd_priv_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_create: %s\n",
                           strerror (errno));
          goto cleanup;
        }

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (state_data->ipmi_ctx,
                                                                                                      channel_number,
                                                                                                      IPMI_GET_LAN_PARAMETER,
                                                                                                      IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                                      IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                                      obj_cmd_priv_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_level: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));

          if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                      obj_cmd_priv_rs,
                                                      &ret))
            rv = ret;

          goto cleanup;
        }

      for (i = 0; i < CIPHER_SUITE_LEN; i++)
        {
          char field[BMC_CONFIG_FIELD_LENGTH_MAX + 1];
          
          memset (field, '\0', BMC_CONFIG_FIELD_LENGTH_MAX + 1);
          
          snprintf (field,
                    BMC_CONFIG_FIELD_LENGTH_MAX,
                    "maximum_privilege_for_cipher_suite_%u",
                    i + 1);

          if (FIID_OBJ_GET (obj_cmd_priv_rs, field, &val) < 0)
            {
	      int id_found = 0;

              /* IPMI Workaround (achu)
               *
               * HP DL145
               *
               * The number of entries returned from a RMCP+ Messaging
               * Cipher Suite Privilege Levels request is not valid.  Not
               * only is it not valid, the number of entries does not even
               * match the number of entries specified by a RMCP+
               * Messaging Cipher Suite Entry Support Count request.
               *
               * Instead, indicate the privilege is illegal and have
               * the output indicated appropriately for this
               * situation.
               */
	      if (fiid_obj_errnum (obj_cmd_priv_rs) == FIID_ERR_DATA_NOT_AVAILABLE)
		{
		  unsigned int j;
		  
		  for (j = 0; j < state_data->cipher_suite_entry_count; j++)
		    {
		      if (state_data->cipher_suite_id_supported[j] == i)
			{
			  id_found++;
			  break;
			}
		    }
		}

              if (fiid_obj_errnum (obj_cmd_priv_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "fiid_obj_get: '%s': %s\n",
                                   field,
                                   fiid_obj_errormsg (obj_cmd_priv_rs));
                  goto cleanup;
                }
              else
		{
		  if (id_found)
		    val = BMC_CONFIG_PRIVILEGE_LEVEL_SUPPORTED_BUT_NOT_READABLE;
		  else
		    val = IPMI_PRIVILEGE_LEVEL_UNSPECIFIED;
		}
            }
          
          state_data->cipher_suite_priv[i] = val;
        }

      state_data->cipher_suite_priv_set++;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_count_rs);
  fiid_obj_destroy (obj_cmd_id_rs);
  fiid_obj_destroy (obj_cmd_priv_rs);
  return (rv);
}

static config_err_t
id_checkout (const char *section_name,
             struct config_keyvalue *kv,
             void *arg,
             int id)
{
  bmc_config_state_data_t *state_data;
  config_err_t ret;
  uint8_t privilege;
  unsigned int i;
  int id_found = 0;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _rmcpplus_cipher_suite_id_privilege_setup (state_data, section_name)) != CONFIG_ERR_SUCCESS)
    return (ret);

  for (i = 0; i < state_data->cipher_suite_entry_count; i++)
    {
      if (state_data->cipher_suite_id_supported[i] == id)
        {
          privilege = state_data->cipher_suite_priv[id];
          id_found++;
          break;
        }
    }

  if (id_found)
    {
      /* achu: see HP DL145 workaround description above in
       * _rmcpplus_cipher_suite_id_privilege_setup()
       */
      if (privilege != BMC_CONFIG_PRIVILEGE_LEVEL_SUPPORTED_BUT_NOT_READABLE)
        {
          if (config_section_update_keyvalue_output (state_data->pstate,
                                                     kv,
                                                     rmcpplus_priv_string (privilege)) < 0)
            return (CONFIG_ERR_FATAL_ERROR);
        }
      else
        {
          /* output empty string, will match with
           * CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY flag to
           * output commented out section.
           */
          if (config_section_update_keyvalue_output (state_data->pstate,
                                                     kv,
                                                     "") < 0)
            return (CONFIG_ERR_FATAL_ERROR);
        }

      return (CONFIG_ERR_SUCCESS);
    }

  /* if ID not found, return non-fatal error, will not output at all */
  return (CONFIG_ERR_NON_FATAL_ERROR);
}

static config_err_t
id_commit (const char *section_name,
           const struct config_keyvalue *kv,
           void *arg,
           int id)
{
  bmc_config_state_data_t *state_data;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t privs[CIPHER_SUITE_LEN];
  uint8_t privilege;
  unsigned int i;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _rmcpplus_cipher_suite_id_privilege_setup (state_data, section_name)) != CONFIG_ERR_SUCCESS)
    return (ret);

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

  privilege = rmcpplus_priv_number (kv->value_input);

  memset (privs, '\0', CIPHER_SUITE_LEN);
  memcpy (privs, state_data->cipher_suite_priv, CIPHER_SUITE_LEN);
  privs[id] = privilege;
  
  /* IPMI Workaround (achu)
   *
   * HP DL145
   *
   * See comments above in _rmcpplus_cipher_suite_id_privilege_setup
   * surrounding HP DL145 workaround.
   *
   * B/c of the issue above, there may be illegal privilege levels
   * sitting in the cipher_suite_priv[] array, we need to fill them in
   * with the values configured by users.
   *
   * If the users didn't configure all the entries, they're out of
   * luck, we need to return an error.
   */

  for (i = 0; i < CIPHER_SUITE_LEN; i++)
    {
      if (privs[i] == BMC_CONFIG_PRIVILEGE_LEVEL_SUPPORTED_BUT_NOT_READABLE)
        {
          struct config_section *section;
	  	  
          if ((section = config_find_section (state_data->sections,
                                              section_name)))
            {
              char keynametmp[CONFIG_MAX_KEY_NAME_LEN + 1];
              struct config_keyvalue *kvtmp;
              
              memset (keynametmp, '\0', CONFIG_MAX_KEY_NAME_LEN + 1);
              
              snprintf (keynametmp,
                        CONFIG_MAX_KEY_NAME_LEN,
                        "Maximum_Privilege_Cipher_Suite_Id_%u",
                        i);

              if ((kvtmp = config_find_keyvalue (section, keynametmp)))
                {
		  uint8_t privilege_tmp;
                  privilege_tmp = rmcpplus_priv_number (kvtmp->value_input);
                  privs[i] = privilege;
                }
              else
                {
		  pstdout_fprintf (state_data->pstate,
				   stderr,
				   "ERROR: '%s:%s' Field Required\n",
				   section_name,
				   keynametmp);
                  rv = CONFIG_ERR_NON_FATAL_ERROR;
                  goto cleanup;
                }
            }
          else
            {
              /* This is a fatal error, we're already in this section,
               * it should be findable
               */
              if (state_data->prog_data->args->config_args.common.debug)
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "Cannot find section '%s'\n",
                                 section_name);
              
              goto cleanup;
            }
        }
    }

  if (ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (state_data->ipmi_ctx,
                                                                                                  channel_number,
                                                                                                  privs[0],
                                                                                                  privs[1],
                                                                                                  privs[2],
                                                                                                  privs[3],
                                                                                                  privs[4],
                                                                                                  privs[5],
                                                                                                  privs[6],
                                                                                                  privs[7],
                                                                                                  privs[8],
                                                                                                  privs[9],
                                                                                                  privs[10],
                                                                                                  privs[11],
                                                                                                  privs[12],
                                                                                                  privs[13],
                                                                                                  privs[14],
                                                                                                  privs[15],
                                                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  state_data->cipher_suite_priv[id] = privilege;
  rv = CONFIG_ERR_SUCCESS;

 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
id_checkout_cb (const char *section_name,
                struct config_keyvalue *kv,
                void *arg)
{
  uint8_t id = atoi (kv->key->key_name + strlen ("Maximum_Privilege_Cipher_Suite_Id_"));
  return (id_checkout (section_name, kv, arg, id));
}

static config_err_t
id_commit_cb (const char *section_name,
              const struct config_keyvalue *kv,
              void *arg)
{
  uint8_t id = atoi (kv->key->key_name + strlen ("Maximum_Privilege_Cipher_Suite_Id_"));
  return (id_commit (section_name, kv, arg, id));
}

struct config_section *
bmc_config_rmcpplus_conf_privilege_section_get (bmc_config_state_data_t *state_data,
						unsigned int config_flags,
						int channel_index)
{
  struct config_section *section = NULL;
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
  char *section_name_base_str = "Rmcpplus_Conf_Privilege";

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
                              "Maximum_Privilege_Cipher_Suite_Id_0",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_1",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_2",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_3",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_4",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_5",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_6",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_7",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_8",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_9",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_10",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_11",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_12",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_13",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Maximum_Privilege_Cipher_Suite_Id_14",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}

