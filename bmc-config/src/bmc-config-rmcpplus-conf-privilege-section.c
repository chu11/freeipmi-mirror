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

static config_err_t
_rmcpplus_cipher_suite_id_privilege_setup(bmc_config_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_count_rs = NULL;
  fiid_obj_t obj_cmd_id_rs = NULL;
  fiid_obj_t obj_cmd_priv_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  int i;

  assert(state_data);

  if (state_data->cipher_suite_entry_count
      && state_data->cipher_suite_id_supported_set
      && state_data->cipher_suite_priv_set)
    return CONFIG_ERR_SUCCESS;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!state_data->cipher_suite_entry_count)
    {
      _FIID_OBJ_CREATE(obj_cmd_count_rs, tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs);

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support (state_data->ipmi_ctx, 
												   channel_number, 
												   IPMI_GET_LAN_PARAMETER, 
												   SET_SELECTOR, 
												   BLOCK_SELECTOR, 
												   obj_cmd_count_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
            rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      _FIID_OBJ_GET (obj_cmd_count_rs, "cipher_suite_entry_count", &val);

      state_data->cipher_suite_entry_count = val;

      if (state_data->cipher_suite_entry_count > CIPHER_SUITE_LEN)
	state_data->cipher_suite_entry_count = CIPHER_SUITE_LEN;
    }

  if (!state_data->cipher_suite_id_supported_set)
    {
      _FIID_OBJ_CREATE(obj_cmd_id_rs, tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs);

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries (state_data->ipmi_ctx, 
											     channel_number, 
											     IPMI_GET_LAN_PARAMETER, 
											     SET_SELECTOR, 
											     BLOCK_SELECTOR, 
											     obj_cmd_id_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
            rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      for (i = 0; i < state_data->cipher_suite_entry_count; i++)
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

	  _FIID_OBJ_GET (obj_cmd_id_rs, field, &val);
	  
	  state_data->cipher_suite_id_supported[i] = val;
	}
      
      state_data->cipher_suite_id_supported_set++;
    }
  
  if (!state_data->cipher_suite_priv_set)
    {
      _FIID_OBJ_CREATE(obj_cmd_priv_rs, tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs);

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (state_data->ipmi_ctx, 
												      channel_number, 
												      IPMI_GET_LAN_PARAMETER, 
												      SET_SELECTOR, 
												      BLOCK_SELECTOR, 
												      obj_cmd_priv_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_level: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
            rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

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

	  _FIID_OBJ_GET (obj_cmd_priv_rs, field, &val);
	  
	  state_data->cipher_suite_priv[i] = val;
	}
      
      state_data->cipher_suite_priv_set++;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_count_rs);
  _FIID_OBJ_DESTROY(obj_cmd_id_rs);
  _FIID_OBJ_DESTROY(obj_cmd_priv_rs);
  return (rv);
}

static config_err_t
id_checkout (const char *section_name,
	     struct config_keyvalue *kv,
             void *arg,
	     int id)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t privilege;
  int i, id_found = 0;

  if ((ret = _rmcpplus_cipher_suite_id_privilege_setup(state_data)) != CONFIG_ERR_SUCCESS)
    return ret;

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
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv, 
                                                rmcpplus_priv_string (privilege)) < 0)
        return CONFIG_ERR_FATAL_ERROR;
      return CONFIG_ERR_SUCCESS;
    }
  return CONFIG_ERR_NON_FATAL_ERROR;
}

static config_err_t
id_commit (const char *section_name,
	   const struct config_keyvalue *kv,
           void *arg,
	   int id)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t privs[CIPHER_SUITE_LEN];
  uint8_t privilege;

  if ((ret = _rmcpplus_cipher_suite_id_privilege_setup(state_data)) != CONFIG_ERR_SUCCESS)
    return ret;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  privilege = rmcpplus_priv_number (kv->value_input);
  
  memset(privs, '\0', CIPHER_SUITE_LEN);
  memcpy(privs, state_data->cipher_suite_priv, CIPHER_SUITE_LEN);
  privs[id] = privilege;
  
  if (ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels(state_data->ipmi_ctx,
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
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
      
  state_data->cipher_suite_priv[id] = privilege;
  rv = CONFIG_ERR_SUCCESS;
  
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
id_checkout_cb (const char *section_name,
                struct config_keyvalue *kv,
                void *arg)
{
  uint8_t id = atoi (kv->key->key_name + strlen("Maximum_Privilege_Cipher_Suite_Id_"));
  return id_checkout (section_name, kv, arg, id);
}

static config_err_t
id_commit_cb (const char *section_name,
              const struct config_keyvalue *kv,
              void *arg)
{
  uint8_t id = atoi (kv->key->key_name + strlen("Maximum_Privilege_Cipher_Suite_Id_"));
  return id_commit (section_name, kv, arg, id);
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

  if (!(rmcpplus_conf_privilege_section = config_section_create (state_data->pstate,
                                                                 "Rmcpplus_Conf_Privilege",
                                                                 "Rmcpplus_Conf_Privilege",
                                                                 section_comment,
                                                                 0,
                                                                 NULL,
                                                                 NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_0",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_1",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_2",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_3",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_4",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_5",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_6",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_7",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_8",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_9",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_10",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_11",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_12",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_13",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              rmcpplus_conf_privilege_section,
                              "Maximum_Privilege_Cipher_Suite_Id_14",
                              "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              id_checkout_cb,
                              id_commit_cb,
                              rmcpplus_priv_number_validate) < 0)
    goto cleanup;

  return rmcpplus_conf_privilege_section;

 cleanup:
  if (rmcpplus_conf_privilege_section)
    config_section_destroy(state_data->pstate, rmcpplus_conf_privilege_section);
  return NULL;
}

