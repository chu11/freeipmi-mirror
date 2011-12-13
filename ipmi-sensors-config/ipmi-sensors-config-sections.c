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

struct config_section *
ipmi_sensors_config_sections_create (ipmi_sensors_config_state_data_t *state_data)
{
  struct config_section *sections = NULL;
  uint16_t record_count;
  unsigned int i;

  assert (state_data);

  if (ipmi_sdr_cache_record_count (state_data->sdr_cache_ctx, &record_count) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_cache_record_count: %s\n",
                       ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
      goto cleanup;
    }

  for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (state_data->sdr_cache_ctx))
    {
      struct config_section *section = NULL;
      uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
      uint8_t record_type;
      uint8_t event_reading_type_code;
      int sdr_record_len;
      int event_reading_type_code_class;
      config_err_t ret;

      memset (sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);
      if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_cache_ctx,
                                                        sdr_record,
                                                        IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_cache_record_read: %s\n",
                           ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
          goto cleanup;
        }

      if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_parse_ctx,
                                             sdr_record,
                                             sdr_record_len,
                                             NULL,
                                             &record_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_record_id_and_type: %s\n",
                           ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
          goto cleanup;
        }

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
          continue;
        }

      if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_parse_ctx,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  &event_reading_type_code) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_event_reading_type_code: %s\n",
                           ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
          goto cleanup;
        }

      event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

      if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
        {
          if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
            {
              if (state_data->prog_data->args->config_args.verbose_count)
                pstdout_printf (state_data->pstate,
                                "## Unable to handle threshold sensor with compact SDR record\n");
              continue;
            }

          if ((ret = ipmi_sensors_config_threshold_section (state_data,
                                                            sdr_record,
                                                            sdr_record_len,
                                                            &section)) != CONFIG_ERR_SUCCESS)
            {
              if (ret == CONFIG_ERR_FATAL_ERROR)
                goto cleanup;
              continue;
            }
        }
      else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
               || event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
        {
          if ((ret = ipmi_sensors_config_discrete_section (state_data,
                                                           sdr_record,
                                                           sdr_record_len,
                                                           &section)) != CONFIG_ERR_SUCCESS)
            {
              if (ret == CONFIG_ERR_FATAL_ERROR)
                goto cleanup;
              continue;
            }
        }
      else
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "## Cannot handle SDR with event reading type code '0x%X'\n",
                             event_reading_type_code);
          continue;
        }

      if (config_section_append (&sections, section) < 0)
        goto cleanup;
    }


  return (sections);

 cleanup:
  config_sections_destroy (sections);
  return (NULL);
}
