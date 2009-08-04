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

#include "bmc-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

config_err_t 
get_lan_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num)
{
  if (state_data->lan_channel_number_initialized)
    {
      *channel_num = state_data->lan_channel_number;
      return CONFIG_ERR_SUCCESS;
    }
  
  if ((state_data->lan_channel_number = ipmi_get_channel_number (state_data->ipmi_ctx, 
                                                                 IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr, 
                        "ipmi_get_channel_number: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      return CONFIG_ERR_NON_FATAL_ERROR;
    }

  state_data->lan_channel_number_initialized = 1;
  *channel_num = state_data->lan_channel_number;
  return CONFIG_ERR_SUCCESS;
}

config_err_t 
get_serial_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num)
{
  if (state_data->serial_channel_number_initialized)
    {
      *channel_num = state_data->serial_channel_number;
      return CONFIG_ERR_SUCCESS;
    }
  
  if ((state_data->serial_channel_number = ipmi_get_channel_number (state_data->ipmi_ctx, 
                                                                    IPMI_CHANNEL_MEDIUM_TYPE_RS232)) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr, 
                        "ipmi_get_channel_number: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      return CONFIG_ERR_NON_FATAL_ERROR;
    }

  state_data->serial_channel_number_initialized = 1;
  *channel_num = state_data->serial_channel_number;
  return CONFIG_ERR_SUCCESS;
}

config_err_t 
get_sol_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t rc;
  uint64_t val;
  uint8_t channel_number;

  if (state_data->sol_channel_number_initialized)
    {
      *channel_num = state_data->sol_channel_number;
      return CONFIG_ERR_SUCCESS;
    }
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs);

  if ((rc = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = rc;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel (state_data->ipmi_ctx,
                                                                     channel_number,
								     IPMI_GET_SOL_PARAMETER,
								     SET_SELECTOR,
								     BLOCK_SELECTOR,
								     obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_SET_SOL_PARAMETER_NOT_SUPPORTED) == 1))
        {
          /* Assume LAN channel */
          state_data->sol_channel_number_initialized = 1;
          state_data->sol_channel_number = channel_number;
          *channel_num = state_data->sol_channel_number;
          goto out;
        }


      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr, 
                        "ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  /* don't use wrapper - non-fatal error */
  if (fiid_obj_get(obj_cmd_rs,
		   "payload_channel",
		   &val) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  state_data->sol_channel_number_initialized = 1;
  state_data->sol_channel_number = val;
  *channel_num = state_data->sol_channel_number;

 out:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

config_err_t 
get_number_of_users (bmc_config_state_data_t *state_data, uint8_t *number_of_users)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t rc;
  uint64_t val;
  uint8_t channel_number;

  if (state_data->number_of_users_initialized)
    {
      *number_of_users = state_data->number_of_users;
      return CONFIG_ERR_SUCCESS;
    }
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_user_access_rs);

  if ((rc = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = rc;
      goto cleanup;
    }

  if (ipmi_cmd_get_user_access (state_data->ipmi_ctx,
                                channel_number,
                                1, /* user_id number */
                                obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr, 
                        "ipmi_cmd_get_user_access: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  /* don't use wrapper - non-fatal error */
  if (fiid_obj_get(obj_cmd_rs,
                   "max_channel_user_ids",
                   &val) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  state_data->number_of_users_initialized = 1;
  state_data->number_of_users = val;

  *number_of_users = state_data->number_of_users;
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}
