#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config-utils.h"

config_err_t 
get_lan_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num)
{
  if (state_data->lan_channel_number_initialized)
    {
      *channel_num = state_data->lan_channel_number;
      return CONFIG_ERR_SUCCESS;
    }
  
  if ((state_data->lan_channel_number = ipmi_get_channel_number (state_data->dev, 
                                                                 IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    return CONFIG_ERR_NON_FATAL_ERROR;

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
  
  if ((state_data->serial_channel_number = ipmi_get_channel_number (state_data->dev, 
                                                                    IPMI_CHANNEL_MEDIUM_TYPE_RS232)) < 0)
    return CONFIG_ERR_NON_FATAL_ERROR;

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
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs)))
    goto cleanup;

  if ((rc = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = rc;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel (state_data->dev,
                                                                     channel_number,
								     IPMI_GET_SOL_PARAMETER,
								     SET_SELECTOR,
								     BLOCK_SELECTOR,
								     obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
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
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_access_rs)))
    goto cleanup;

  if ((rc = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = rc;
      goto cleanup;
    }

  if (ipmi_cmd_get_user_access (state_data->dev,
                                channel_number,
                                1, /* user_id number */
                                obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
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
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return rv;
}
