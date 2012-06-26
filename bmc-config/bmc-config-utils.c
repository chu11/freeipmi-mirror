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
#include <errno.h>
#include <assert.h>

#include "bmc-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

config_err_t
load_lan_channel_numbers (bmc_config_state_data_t *state_data)
{
  int ret;

  assert (state_data);

  if ((ret = ipmi_get_channel_numbers (state_data->ipmi_ctx,
                                       IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                                       state_data->lan_channel_numbers,
                                       IPMI_CHANNEL_NUMBERS_MAX)) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_get_channel_numbers: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      return (CONFIG_ERR_NON_FATAL_ERROR);
    }
  
  state_data->lan_channel_numbers_count = (unsigned int)ret;
  state_data->lan_channel_numbers_loaded++;

  return (CONFIG_ERR_SUCCESS);
}

config_err_t
load_serial_channel_numbers (bmc_config_state_data_t *state_data)
{
  int ret;

  assert (state_data);

  if ((ret = ipmi_get_channel_numbers (state_data->ipmi_ctx,
                                       IPMI_CHANNEL_MEDIUM_TYPE_RS232,
                                       state_data->serial_channel_numbers,
                                       IPMI_CHANNEL_NUMBERS_MAX)) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_get_channel_numbers: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      return (CONFIG_ERR_NON_FATAL_ERROR);
    }
  
  state_data->serial_channel_numbers_count = (unsigned int)ret;
  state_data->serial_channel_numbers_loaded++;

  return (CONFIG_ERR_SUCCESS);
}

static void
_sol_channel_number_save (bmc_config_state_data_t *state_data,
			  uint8_t lan_channel_number,
			  uint8_t sol_channel_number)
{
  unsigned int i;
  int found = 0;

  assert (state_data);
  assert (state_data->sol_channel_numbers_count <= IPMI_CHANNEL_NUMBERS_MAX);

  for (i = 0; i < state_data->sol_channel_numbers_count; i++)
    {
      if (state_data->sol_channel_numbers_sol_channel[i] == sol_channel_number)
	{
	  found++;
	  break;
	}
    }

  if (!found)
    {
      state_data->sol_channel_numbers_unique[state_data->sol_channel_numbers_unique_count] = sol_channel_number;
      state_data->sol_channel_numbers_unique_count++;
    }

  state_data->sol_channel_numbers_lan_channel[state_data->sol_channel_numbers_count] = lan_channel_number;
  state_data->sol_channel_numbers_sol_channel[state_data->sol_channel_numbers_count] = sol_channel_number;
  state_data->sol_channel_numbers_count++;

}

config_err_t
_get_sol_channel_number_for_channel (bmc_config_state_data_t *state_data,
				     uint8_t lan_channel_number)
{
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;

  assert (state_data);

  if (state_data->prog_data->args->config_args.common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SOL_CHANNEL_ASSUME_LAN_CHANNEL)
    {
      _sol_channel_number_save (state_data, lan_channel_number, lan_channel_number);
      goto out;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel (state_data->ipmi_ctx,
                                                                     lan_channel_number,
                                                                     IPMI_GET_SOL_PARAMETER,
                                                                     IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                     IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                     obj_cmd_rs) < 0)
    {
      /* This parameter is optional, if its not supported, assume LAN channel */
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_SET_SOL_CONFIGURATION_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1))
        {
	  _sol_channel_number_save (state_data, lan_channel_number, lan_channel_number);
          goto out;
        }

      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      
      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;
      
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "payload_channel",
                    &val) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _sol_channel_number_save (state_data, lan_channel_number, val);

 out:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

config_err_t
load_sol_channel_numbers (bmc_config_state_data_t *state_data)
{
  unsigned int channelindex;

  assert (state_data);
  
  /* There's a lot of trickery here.  Even if motherboard specifies
   * multiple LAN channels, they could map to a fewer number of SOL
   * channels.  So we need to calculate the number of unique SOL
   * channels
   */

  if (state_data->lan_channel_numbers_count > 0)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
  	  if (_get_sol_channel_number_for_channel (state_data,
						   state_data->lan_channel_numbers[channelindex]) == CONFIG_ERR_FATAL_ERROR)
	    return (CONFIG_ERR_FATAL_ERROR);
        }
      state_data->sol_channel_numbers_loaded++;
    }

  return (CONFIG_ERR_SUCCESS);
}

config_err_t
get_lan_channel_number (bmc_config_state_data_t *state_data,
			const char *section_name,
			uint8_t *channel_number)
{
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert (state_data);
  /* section_name can be NULL if want to force IPMI search */
  assert (channel_number);

  /* For multi-channel cases, channel will be in the section name */

  if (section_name)
    {
      char *ptr;

      /* Special case for Lan_Channel_Channel_X */
      if ((ptr = stristr (section_name, "Channel_Channel_")))
	{
	  (*channel_number) = atoi (ptr + strlen ("Channel_Channel_"));
	  return (CONFIG_ERR_SUCCESS);
	}

      /* For all other sections with a channel number at the end of the section name */
      if ((ptr = stristr (section_name, "Channel_")))
	{
	  (*channel_number) = atoi (ptr + strlen ("Channel_"));
	  return (CONFIG_ERR_SUCCESS);
	}
    }

  /* for single-channel, channel is first one found */

  if (!state_data->prog_data->args->config_args.lan_channel_number_set)
    {
      if (!state_data->lan_channel_numbers_loaded)
	{
	  if ((ret = load_lan_channel_numbers (state_data)) != CONFIG_ERR_SUCCESS)
	    {
	      rv = ret;
	      goto cleanup;
	    }
	}

      if (!state_data->lan_channel_numbers_count)
	{
	  rv = CONFIG_ERR_NON_FATAL_ERROR;
	  goto cleanup;
	}

      (*channel_number) = state_data->lan_channel_numbers[0];
    }
  else
    (*channel_number) = state_data->prog_data->args->config_args.lan_channel_number;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
get_serial_channel_number (bmc_config_state_data_t *state_data,
			   const char *section_name,
			   uint8_t *channel_number)
{
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
 
  assert (state_data);
  /* section_name can be NULL if want to force IPMI search */
  assert (channel_number);

  /* For multi-channel cases, channel will be in the section name */

  if (section_name)
    {
      char *ptr;

      /* Special case for Serial_Channel_Channel_X */
      if ((ptr = stristr (section_name, "Channel_Channel_")))
	{
	  (*channel_number) = atoi (ptr + strlen ("Channel_Channel_"));
	  return (CONFIG_ERR_SUCCESS);
	}

      /* For all other sections with a channel number at the end of the section name */
      if ((ptr = stristr (section_name, "Channel_")))
	{
	  (*channel_number) = atoi (ptr + strlen ("Channel_"));
	  return (CONFIG_ERR_SUCCESS);
	}
    }

  /* for single-channel, channel is first one found */

  if (!state_data->prog_data->args->config_args.serial_channel_number_set)
    {
      if (!state_data->serial_channel_numbers_loaded)
	{
	  if ((ret = load_serial_channel_numbers (state_data)) != CONFIG_ERR_SUCCESS)
	    {
	      rv = ret;
	      goto cleanup;
	    }
	}

      if (!state_data->serial_channel_numbers_count)
	{
	  rv = CONFIG_ERR_NON_FATAL_ERROR;
	  goto cleanup;
	}

      (*channel_number) = state_data->serial_channel_numbers[0];
    }
  else
    (*channel_number) = state_data->prog_data->args->config_args.serial_channel_number;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
get_sol_channel_number (bmc_config_state_data_t *state_data,
			const char *section_name,
			uint8_t *channel_number)
{
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert (state_data);
  /* section_name can be NULL if want to force IPMI search */
  assert (channel_number);

  /* For multi-channel cases, channel will be in the section name */

  if (section_name)
    {
      char *ptr;

      /* Sections with a channel number at the end of the section name */
      if ((ptr = stristr (section_name, "Channel_")))
	{
	  (*channel_number) = atoi (ptr + strlen ("Channel_"));
	  return (CONFIG_ERR_SUCCESS);
	}
    }

  /* for single-channel, channel is first one found */

  if (!state_data->prog_data->args->config_args.sol_channel_number_set)
    {
      if (!state_data->sol_channel_numbers_loaded)
	{
	  if ((ret = load_sol_channel_numbers (state_data)) != CONFIG_ERR_SUCCESS)
	    {
	      rv = ret;
	      goto cleanup;
	    }
	}
      
      if (!state_data->sol_channel_numbers_count)
	{
	  rv = CONFIG_ERR_NON_FATAL_ERROR;
	  goto cleanup;
	}
      
      (*channel_number) = state_data->sol_channel_numbers_unique[0];
    }
  else
    (*channel_number) = state_data->prog_data->args->config_args.sol_channel_number;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

