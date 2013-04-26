/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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
#include "bmc-config-channel-common.h"
#include "bmc-config-utils.h"

#include "freeipmi-portability.h"

struct config_section *
bmc_config_lan_channel_section_get (bmc_config_state_data_t *state_data,
                                    unsigned int config_flags,
				    int channel_index)
{
  struct config_section * section = NULL;
  char *section_comment =
    "In the Lan_Channel section, general IPMI over LAN can be enabled for "
    "disabled.  In the below, \"Volatile\" configurations are immediately "
    "configured onto the BMC and will have immediate effect on the system.  "
    "\"Non_Volatile\" configurations are only available after the next "
    "system reset.  Generally, both the \"Volatile\" and \"Non_Volatile\" "
    "equivalent fields should be configured identically."
    "\n"
    "To enable IPMI over LAN, typically \"Access_Mode\" "
    "should be set to \"Always_Available\".  "
    "\"Channel_Privilege_Limit\" should be set to the highest privilege "
    "level any username was configured with.  Typically, this "
    "is set to \"Administrator\"."
    "\n"
    "\"User_Level_Auth\" and \"Per_Message_Auth\" are typically set to "
    "\"Yes\" for additional security.";
  char *section_name_base_str = "Lan_Channel";

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

  if (bmc_config_channel_common_section_get (state_data,
                                             section) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}

