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

#include "bmc-config.h"
#include "bmc-config-channel-common.h"

#include "freeipmi-portability.h"

struct config_section *
bmc_config_lan_channel_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section * lan_channel_section = NULL;
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

  if (!(lan_channel_section = config_section_create (state_data->pstate,
                                                     "Lan_Channel",
                                                     "Lan_Channel",
                                                     section_comment,
                                                     0,
                                                     NULL,
                                                     NULL)))
    goto cleanup;

  if (bmc_config_channel_common_section_get(state_data, 
                                            lan_channel_section,
                                            0) < 0)
    goto cleanup;

  return lan_channel_section;

 cleanup:
  if (lan_channel_section)
    config_section_destroy(state_data->pstate, lan_channel_section);
  return NULL;
}

