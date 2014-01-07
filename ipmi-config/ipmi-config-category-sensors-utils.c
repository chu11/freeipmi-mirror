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
#include <errno.h>
#include <assert.h>

#include "ipmi-config-category-sensors-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

static ipmi_config_err_t
_convert_id_string (ipmi_config_state_data_t *state_data,
		    char *id_string)
{
  char *ptr;

  assert (state_data);
  assert (id_string);

  /* Convert stuff to underscore.  I will not convert +/-/. for now.
   * I think they are fine.
   */
  while ((ptr = strchr (id_string, ' ')))
    *ptr = '_';

  while ((ptr = strchr (id_string, '/')))
    *ptr = '_';

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
ipmi_config_sensors_create_section_name (ipmi_config_state_data_t *state_data,
					 char *section_name,
					 unsigned int section_name_len)
{
  char id_string[IPMI_SDR_MAX_ID_STRING_LENGTH + 1];
  uint16_t record_id;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (section_name_len);

  memset (section_name, '\0', section_name_len);
  memset (id_string, '\0', IPMI_SDR_MAX_ID_STRING_LENGTH + 1);

  if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
					 NULL,
					 0,
                                         &record_id,
                                         NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_id_string (state_data->sdr_ctx,
				NULL,
				0,
                                id_string,
                                IPMI_SDR_MAX_ID_STRING_LENGTH) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_id_string: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if ((ret = _convert_id_string (state_data,
				 id_string)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "_convert_id_string: %s\n",
                         strerror (errno));

      goto cleanup;
    }

  /* We will name sections by record_id then name, since id_strings
   * could be identical.
   */
  if (strlen (id_string) > 0)
    snprintf (section_name,
              IPMI_CONFIG_MAX_SECTION_NAME_LEN,
              "%u_%s",
              record_id,
              id_string);
  else
    /* I guess its conceivable the sensor won't have a name, so we
     * make one up.
     */
    snprintf (section_name,
              IPMI_CONFIG_MAX_SECTION_NAME_LEN,
              "%u_%s",
              record_id,
              "Unknown_Sensor_Name");

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
ipmi_config_sensors_seek_to_sdr_record (ipmi_config_state_data_t *state_data,
					const char *section_name)
{
  uint16_t record_id;
  char *str = NULL;
  char *ptr;
  char *endptr;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);

  if (!(str = strdup (section_name)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }

  if (!(ptr = strchr (str, '_')))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Invalid section_name: %s\n",
		       section_name);
      goto cleanup;
    }

  *ptr = '\0';

  errno = 0;
  record_id = strtoul (str, &endptr,0);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Invalid section_name: %s\n",
		       section_name);
      goto cleanup;
    }

  if (ipmi_sdr_cache_search_record_id (state_data->sdr_ctx,
                                       record_id) < 0)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Record_id not found: %u\n",
                         record_id);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  free (str);
  return (rv);
}
