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
#include <errno.h>
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-sections.h"
#include "bmc-config-utils.h"

#include "bmc-config-user-sections.h"
#include "bmc-config-lan-channel-section.h"
#include "bmc-config-lan-conf-section.h"
#include "bmc-config-lan-conf-auth-section.h"
#include "bmc-config-lan-conf-security-keys-section.h"
#include "bmc-config-lan-conf-misc-section.h"
#include "bmc-config-pef-conf-section.h"
#include "bmc-config-rmcpplus-conf-privilege-section.h"
#include "bmc-config-serial-channel-section.h"
#include "bmc-config-serial-conf-section.h"
#include "bmc-config-sol-conf-section.h"
#include "bmc-config-misc-section.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

struct config_section *
bmc_config_sections_create (bmc_config_state_data_t *state_data)
{
  struct config_section *sections = NULL;
  struct config_section *section = NULL;
  uint8_t number_of_users;
  int i;

  if (get_number_of_users(state_data, &number_of_users) != CONFIG_ERR_SUCCESS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr, 
                       "Unable to get Number of Users\n");
      return NULL;
    }

  for (i = 0; i < number_of_users; i++)
    {
      if (!(section = bmc_config_user_section_get(state_data, i+1)))
	goto cleanup;
      if (config_section_append (state_data->pstate, &sections, section) < 0)
	goto cleanup;
    }
  
  if (!(section = bmc_config_lan_channel_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_auth_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_security_keys_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_lan_conf_misc_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_rmcpplus_conf_privilege_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_serial_channel_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_serial_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_pef_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_sol_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = bmc_config_misc_section_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  return sections;

 cleanup:
  config_sections_destroy(state_data->pstate, sections);
  return NULL;
}

