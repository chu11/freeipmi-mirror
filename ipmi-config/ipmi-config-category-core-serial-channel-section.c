/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#include "ipmi-config.h"
#include "ipmi-config-category-core-channel-common.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"

struct ipmi_config_section *
ipmi_config_core_serial_channel_section_get (ipmi_config_state_data_t *state_data,
					     unsigned int config_flags,
					     int channel_index)
{
  struct ipmi_config_section * section = NULL;
  char *section_comment =
    "In the Serial_Channel section, IPMI over Serial communication can be "
    "enabled or disabled.  "
    "In the below, \"Volatile\" configurations are immediately "
    "configured onto the BMC and will have immediate effect on the system.  "
    "\"Non_Volatile\" configurations are only available after the next "
    "system reset.  Generally, both the \"Volatile\" and \"Non_Volatile\" "
    "equivalent fields should be configured identically."
    "\n"
    "Most users will only be interested in IPMI over LAN, therefore serial "
    "communication can be disabled.  This can be done by setting "
    "\"Access_Mode\" to \"Disabled\".";
  char *section_name_base_str = "Serial_Channel";

  assert (state_data);

  /*
   * achu: section not checked out by default.
   */

  if (!state_data->prog_data->args->verbose_count)
    config_flags |= IPMI_CONFIG_DO_NOT_CHECKOUT;

  if (!(section = ipmi_config_section_multi_channel_create (state_data,
                                                            section_name_base_str,
                                                            section_comment,
                                                            NULL,
                                                            NULL,
                                                            config_flags,
                                                            channel_index,
                                                            state_data->serial_channel_numbers,
                                                            state_data->serial_channel_numbers_count)))
    goto cleanup;

  if (ipmi_config_core_channel_common_section_get (state_data,
						   section) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}

