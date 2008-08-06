/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

static config_err_t
community_string_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_community_string_rs);
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_get_lan_configuration_parameters_community_string (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  SET_SELECTOR,
                                                                  BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_community_string: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  memset(community_string,'\0', IPMI_MAX_COMMUNITY_STRING_LENGTH+1);
  _FIID_OBJ_GET_DATA (obj_cmd_rs,
                      "community_string",
                      (uint8_t *)community_string,
                      IPMI_MAX_COMMUNITY_STRING_LENGTH+1);

  if (config_section_update_keyvalue_output(state_data->pstate, 
                                            kv,
                                            community_string) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
community_string_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_set_lan_configuration_parameters_community_string (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  kv->value_input,
                                                                  strlen(kv->value_input),
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_community_string: %s\n",
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

static config_validate_t
community_string_validate (const char *section_name,
                           const char *key_name,
                           const char *value,
                           void *arg)
{
  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return CONFIG_VALIDATE_INVALID_VALUE;
  return CONFIG_VALIDATE_VALID_VALUE;
}

struct config_section *
pef_config_community_string_section_get (pef_config_state_data_t *state_data)
{
  struct config_section *section = NULL;

  if (!(section = config_section_create (state_data->pstate, 
                                         "Community_String",
                                         NULL, 
                                         NULL, 
                                         0,
                                         NULL,
                                         NULL)))
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

  return section;

 cleanup:
  if (section)
    config_section_destroy(state_data->pstate, section);
  return NULL;
}

