/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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
#include "ipmi-config-section.h"
#include "ipmi-config-category-sensors-sensor-event-enable-common.h"
#include "ipmi-config-category-sensors-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

ipmi_config_err_t
ipmi_config_sensors_discrete_section (ipmi_config_state_data_t *state_data,
                                      struct ipmi_config_section **section_ptr)
{
  struct ipmi_config_section *section = NULL;
  char section_name[IPMI_CONFIG_MAX_SECTION_NAME_LEN];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_ptr);

  if ((ret = ipmi_config_sensors_create_section_name (state_data,
						      section_name,
						      IPMI_CONFIG_MAX_SECTION_NAME_LEN)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_config_sensors_create_section_name: %s\n",
                         strerror (errno));

      goto cleanup;
    }

  if (!(section = ipmi_config_section_create (state_data,
					      section_name,
					      NULL,
					      NULL,
					      0,
					      NULL,
					      NULL)))
    goto cleanup;

  if (setup_sensor_event_enable_fields (state_data, section) < 0)
    goto cleanup;

  *section_ptr = section;
  return (IPMI_CONFIG_ERR_SUCCESS);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (rv);
}
