/*
 * Copyright (C) 2008-2013 FreeIPMI Core Team
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

#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-sensor-event-enable-common.h"
#include "ipmi-sensors-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

config_err_t
ipmi_sensors_config_discrete_section (ipmi_sensors_config_state_data_t *state_data,
                                      struct config_section **section_ptr)
{
  struct config_section *section = NULL;
  char section_name[CONFIG_MAX_SECTION_NAME_LEN];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert (state_data);
  assert (section_ptr);

  if ((ret = create_section_name (state_data,
                                  section_name,
                                  CONFIG_MAX_SECTION_NAME_LEN)) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "create_section_name: %s\n",
                         strerror (errno));
      rv = ret;
      goto cleanup;
    }

  if (!(section = config_section_create (state_data->pstate,
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
  return (CONFIG_ERR_SUCCESS);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (rv);
}
