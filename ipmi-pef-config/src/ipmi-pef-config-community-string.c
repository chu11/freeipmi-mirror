/*
  Copyright (C) 2007-2009 FreeIPMI Core Team

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
  ipmi_pef_config_state_data_t *state_data = (ipmi_pef_config_state_data_t *)arg;
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_community_string_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_community_string (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  CONFIG_SET_SELECTOR,
                                                                  CONFIG_BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_community_string: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      if (!IPMI_ERRNUM_IS_FATAL_ERROR (state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
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
  ipmi_pef_config_state_data_t *state_data = (ipmi_pef_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
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
      if (!IPMI_ERRNUM_IS_FATAL_ERROR (state_data->ipmi_ctx))
        {
          if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && (ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_SET_LAN_WRITE_READ_ONLY_PARAMETER) == 1))
            rv = CONFIG_ERR_NON_FATAL_ERROR_READ_ONLY;
          else
            rv = CONFIG_ERR_NON_FATAL_ERROR;
        }
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
  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return (CONFIG_VALIDATE_INVALID_VALUE);
  return (CONFIG_VALIDATE_VALID_VALUE);
}

struct config_section *
ipmi_pef_config_community_string_section_get (ipmi_pef_config_state_data_t *state_data)
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

  return (section);

 cleanup:
  if (section)
    config_section_destroy (state_data->pstate, section);
  return (NULL);
}

