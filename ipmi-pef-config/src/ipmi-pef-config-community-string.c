/*
 * Copyright (C) 2007-2011 FreeIPMI Core Team
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

#include "ipmi-pef-config.h"
#include "ipmi-pef-config-map.h"
#include "ipmi-pef-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

static config_err_t
community_string_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_community_string_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_community_string (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                  IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_community_string: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  memset (community_string,'\0', IPMI_MAX_COMMUNITY_STRING_LENGTH+1);
  if (fiid_obj_get_data (obj_cmd_rs,
                         "community_string",
                         community_string,
                         IPMI_MAX_COMMUNITY_STRING_LENGTH+1) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'obj_cmd_rs': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             community_string) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
community_string_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

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

  if (ipmi_cmd_set_lan_configuration_parameters_community_string (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  kv->value_input,
                                                                  strlen (kv->value_input),
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_community_string: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_validate_t
community_string_validate (const char *section_name,
                           const char *key_name,
                           const char *value,
                           void *arg)
{
  assert (section_name);
  assert (key_name);
  assert (value);

  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return (CONFIG_VALIDATE_INVALID_VALUE);
  return (CONFIG_VALIDATE_VALID_VALUE);
}

struct config_section *
ipmi_pef_config_community_string_section_get (ipmi_pef_config_state_data_t *state_data,
					      unsigned int config_flags,
					      int channel_index)
{
  struct config_section *section = NULL;
  char *section_name_base_str = "Community_String";

  assert (state_data);

  if (!(section = config_section_multi_channel_create (state_data->pstate,
                                                       section_name_base_str,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       config_flags,
                                                       channel_index,
                                                       state_data->lan_channel_numbers,
                                                       state_data->lan_channel_numbers_count)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Community_String",
                              "Give valid string",
                              0,
                              community_string_checkout,
                              community_string_commit,
                              community_string_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}

