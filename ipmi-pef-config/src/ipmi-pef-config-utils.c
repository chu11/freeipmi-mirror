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
#include <errno.h>
#include <assert.h>

#include "ipmi-pef-config.h"
#include "ipmi-pef-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

config_err_t
load_lan_channel_numbers (ipmi_pef_config_state_data_t *state_data)
{
  int ret;

  assert (state_data);

  if ((ret = ipmi_get_channel_numbers (state_data->ipmi_ctx,
                                       IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                                       state_data->lan_channel_numbers,
                                       CHANNEL_NUMBERS_MAX)) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
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
get_lan_channel_number (struct ipmi_pef_config_state_data *state_data,
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

      /* For all sections with a channel number at the end of the section name */
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

