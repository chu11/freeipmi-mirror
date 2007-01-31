/* 
   ipmi_wrapper.c: higher level wrapper to libfreeipmi functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#include "bmc-ipmi-wrapper.h"

bmc_err_t 
get_lan_channel_number (bmc_config_state_data_t *state_data, int8_t *channel_num)
{
  if (state_data->lan_channel_number_initialized)
    {
      *channel_num = state_data->lan_channel_number;
      return BMC_ERR_SUCCESS;
    }
  
  if ((state_data->lan_channel_number = ipmi_get_channel_number (state_data->dev, 
                                                                 IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    return BMC_ERR_NON_FATAL_ERROR;

  state_data->lan_channel_number_initialized = true;
  *channel_num = state_data->lan_channel_number;
  return BMC_ERR_SUCCESS;
}

bmc_err_t 
get_serial_channel_number (bmc_config_state_data_t *state_data, int8_t *channel_num)
{
  if (state_data->serial_channel_number_initialized)
    {
      *channel_num = state_data->serial_channel_number;
      return BMC_ERR_SUCCESS;
    }
  
  if ((state_data->serial_channel_number = ipmi_get_channel_number (state_data->dev, 
                                                                    IPMI_CHANNEL_MEDIUM_TYPE_RS232)) < 0)
    return BMC_ERR_NON_FATAL_ERROR;

  state_data->serial_channel_number_initialized = true;
  *channel_num = state_data->serial_channel_number;
  return BMC_ERR_SUCCESS;
}

bmc_err_t 
get_sol_channel_number (bmc_config_state_data_t *state_data, int8_t *channel_num)
{
  fiid_obj_t obj_cmd_rs = NULL;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t rc;
  uint64_t val;
  int8_t num;

  if (state_data->sol_channel_number_initialized)
    {
      *channel_num = state_data->sol_channel_number;
      return BMC_ERR_SUCCESS;
    }
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs)))
    goto cleanup;

  if ((rc = get_lan_channel_number (state_data, &num)) != BMC_ERR_SUCCESS)
    {
      rv = rc;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel (state_data->dev,
                                                                     num,
								     IPMI_GET_SOL_PARAMETER,
								     SET_SELECTOR,
								     BLOCK_SELECTOR,
								     obj_cmd_rs) < 0)
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get(obj_cmd_rs,
		   "payload_channel",
		   &val) < 0)
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  state_data->sol_channel_number_initialized = true;
  state_data->sol_channel_number = val;

  *channel_num = state_data->sol_channel_number;
  rv = BMC_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return rv;
}
