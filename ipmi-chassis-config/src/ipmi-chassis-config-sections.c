/* 
   Copyright (C) 2008 FreeIPMI Core Team
   
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

#include "ipmi-chassis-config.h"
#include "ipmi-chassis-config-sections.h"
#include "ipmi-chassis-config-boot-flags.h"
#include "ipmi-chassis-config-front-panel-buttons.h"
#include "ipmi-chassis-config-power-conf.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

struct config_section *
ipmi_chassis_config_sections_create (ipmi_chassis_config_state_data_t *state_data)
{
  struct config_section *sections = NULL;
  struct config_section *section = NULL;

  if (!(section = ipmi_chassis_config_front_panel_buttons_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = ipmi_chassis_config_power_conf_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  if (!(section = ipmi_chassis_config_boot_flags_get (state_data)))
    goto cleanup;
  if (config_section_append (state_data->pstate, &sections, section) < 0)
    goto cleanup;

  return sections;

 cleanup:
  config_sections_destroy(state_data->pstate, sections);
  return NULL;
}
