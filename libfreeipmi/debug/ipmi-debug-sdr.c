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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

int
ipmi_dump_sdr_record (int fd,
                      const char *prefix,
                      const char *hdr,
                      const char *trlr,
                      const void *sdr_record,
                      unsigned int sdr_record_len)
{
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  fiid_obj_t obj_sdr_record_header = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint64_t val;
  uint8_t record_type;
  int sdr_record_header_len;
  int rv = -1;

  if (!sdr_record)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if ((sdr_record_header_len = fiid_template_len_bytes (tmpl_sdr_record_header)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (!(obj_sdr_record_header = fiid_obj_create (tmpl_sdr_record_header)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sdr_record_header,
                        sdr_record,
                        sdr_record_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record_header);
      goto cleanup;
    }

  /* Not enough for a real SDR record, just dump whatever we have */
  if (sdr_record_len <= sdr_record_header_len)
    {
      if (ipmi_obj_dump (fd,
                         prefix,
                         hdr,
                         trlr,
                         obj_sdr_record_header) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
      rv = 0;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_sdr_record_header, "record_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record_header);
      goto cleanup;
    }
  record_type = val;

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_full_sensor_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_compact_sensor_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_event_only_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_entity_association_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_device_relative_entity_association_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_generic_device_locator_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_fru_device_locator_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_management_controller_device_locator_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_management_controller_confirmation_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_bmc_message_channel_info_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_OEM_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_oem_record)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else
    {
      /* I don't know this record_type, maybe its in a newer spec?? */
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_record_header)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }

  if (fiid_obj_set_all (obj_sdr_record,
                        sdr_record,
                        sdr_record_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record);
      goto cleanup;
    }

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    {
      uint8_t event_reading_type_code;
      fiid_obj_t obj_temp = NULL;

      if (FIID_OBJ_GET (obj_sdr_record,
                        "event_reading_type_code",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record);
          goto cleanup;
        }
      event_reading_type_code = val;

      if ((IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code)))
        {
          if (!(obj_temp = fiid_obj_copy (obj_sdr_record,
                                          tmpl_sdr_full_sensor_record_threshold_based_sensors)))
            {
              FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record);
              goto cleanup;
            }
        }
      else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code)
               || IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
        {
          if (!(obj_temp = fiid_obj_copy (obj_sdr_record,
                                          tmpl_sdr_full_sensor_record_non_threshold_based_sensors)))
            {
              FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record);
              goto cleanup;
            }
        }

      if (obj_temp)
        {
          fiid_obj_destroy (obj_sdr_record);
          obj_sdr_record = obj_temp;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    {
      uint8_t event_reading_type_code;
      fiid_obj_t obj_temp = NULL;

      if (FIID_OBJ_GET (obj_sdr_record,
                        "event_reading_type_code",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record);
          goto cleanup;
        }
      event_reading_type_code = val;

      /* achu: shouldn't be possible for this to be a threshold based
       * sensor, but we'll allow it just in case.
       */

      if ((IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code)))
        {
          if (!(obj_temp = fiid_obj_copy (obj_sdr_record,
                                          tmpl_sdr_compact_sensor_record_threshold_based_sensors)))
            {
              FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record);
              goto cleanup;
            }
        }
      else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code)
               || IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
        {
          if (!(obj_temp = fiid_obj_copy (obj_sdr_record,
                                          tmpl_sdr_compact_sensor_record_non_threshold_based_sensors)))
            {
              FIID_OBJECT_ERROR_TO_ERRNO (obj_sdr_record);
              goto cleanup;
            }
        }

      if (obj_temp)
        {
          fiid_obj_destroy (obj_sdr_record);
          obj_sdr_record = obj_temp;
        }
    }

  if (ipmi_obj_dump (fd,
                     prefix,
                     NULL,
                     NULL,
                     obj_sdr_record) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (debug_output_str (fd, prefix_buf, trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

#if WITH_RAWDUMPS
  /* For those vendors that get confused when they see the nice output
   * and want the hex output
   */
  if (ipmi_dump_hex (fd,
		     prefix,
		     hdr,
		     trlr,
		     sdr_record,
		     sdr_record_len) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
#endif

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_sdr_record_header);
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

