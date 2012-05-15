/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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

#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-sections.h"
#include "ipmi-sensors-config-discrete-section.h"
#include "ipmi-sensors-config-threshold-section.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

struct ipmi_sensors_config_sdr_callback {
  ipmi_sensors_config_state_data_t *state_data;
  struct config_section *sections;
}; 

static int
_sections_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
			uint8_t record_type,
			const void *sdr_record,
			unsigned int sdr_record_len,
			void *arg)
{
  struct ipmi_sensors_config_sdr_callback *sdr_callback_arg;
  ipmi_sensors_config_state_data_t *state_data;
  struct config_section *section = NULL;
  uint8_t event_reading_type_code;
  int event_reading_type_code_class;
  config_err_t ret;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  sdr_callback_arg = (struct ipmi_sensors_config_sdr_callback *)arg;
  state_data = sdr_callback_arg->state_data;

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats
   * also support settable thresholds.  However, since compact
   * records don't contain any information for interpreting
   * threshold sensors (e.g. R exponent) I don't know how they
   * could be of any use.  No vendor that I know of supports
   * threshold sensors via a compact record (excluding possible
   * OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */
  
  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    {
      if (state_data->prog_data->args->config_args.verbose_count)
	pstdout_fprintf (state_data->pstate,
			 stderr,
			 "## Cannot handle SDR record format '0x%X'\n",
			 record_type);
      return (0);
    }
  
  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      sdr_record,
					      sdr_record_len,
					      &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_event_reading_type_code: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
	{
	  if (state_data->prog_data->args->config_args.verbose_count)
	    pstdout_printf (state_data->pstate,
			    "## Unable to handle threshold sensor with compact SDR record\n");
	  return (0);
	}

      if ((ret = ipmi_sensors_config_threshold_section (state_data,
							&section)) != CONFIG_ERR_SUCCESS)
	{
	  if (ret == CONFIG_ERR_FATAL_ERROR)
	    return (-1);
	  return (0);
	}
    }
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
	   || event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
    {
      if ((ret = ipmi_sensors_config_discrete_section (state_data,
						       &section)) != CONFIG_ERR_SUCCESS)
	{
	  if (ret == CONFIG_ERR_FATAL_ERROR)
	    return (-1);
	  return (0);
	}
    }
  else
    {
      if (state_data->prog_data->args->config_args.common.debug)
	pstdout_fprintf (state_data->pstate,
			 stderr,
			 "## Cannot handle SDR with event reading type code '0x%X'\n",
			 event_reading_type_code);
      return (0);
    }

  if (config_section_append (&sdr_callback_arg->sections, section) < 0)
    return (-1);

  return (0);
}

struct config_section *
ipmi_sensors_config_sections_create (ipmi_sensors_config_state_data_t *state_data)
{
  struct ipmi_sensors_config_sdr_callback sdr_callback_arg;

  assert (state_data);

  sdr_callback_arg.state_data = state_data;
  sdr_callback_arg.sections = NULL;

  if (ipmi_sdr_cache_iterate (state_data->sdr_ctx,
			      _sections_sdr_callback,
			      &sdr_callback_arg) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_iterate: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  return (sdr_callback_arg.sections);

 cleanup:
  config_sections_destroy (sdr_callback_arg.sections);
  return (NULL);
}
