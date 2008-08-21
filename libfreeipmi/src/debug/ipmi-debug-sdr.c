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
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

int32_t
ipmi_dump_sdr_record (int fd, 
                      const char *prefix, 
                      const char *hdr, 
                      const char *trlr, 
                      uint8_t *sdr_record, 
                      uint32_t sdr_record_len)
{
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  fiid_obj_t obj_sdr_record_header = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint64_t val;
  uint8_t record_type;
  int32_t sdr_record_header_len;
  int8_t rv = -1;

  ERR_EINVAL (sdr_record);

  ERR(!(ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  ERR(!(ipmi_debug_output_str (fd, prefix_buf, hdr) < 0));

  FIID_TEMPLATE_LEN_BYTES(sdr_record_header_len, tmpl_sdr_record_header);

  FIID_OBJ_CREATE(obj_sdr_record_header, tmpl_sdr_record_header);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record_header,
                           sdr_record,
                           sdr_record_len);
  
  /* Not enough for a real SDR record, just dump whatever we have */
  if (sdr_record_len <= sdr_record_header_len)
    {
      ERR_CLEANUP (!(ipmi_obj_dump (fd, 
                                    prefix, 
                                    hdr, 
                                    trlr, 
                                    obj_sdr_record_header) < 0));
      rv = 0;
      goto cleanup;
    }
  
  FIID_OBJ_GET_CLEANUP(obj_sdr_record_header, "record_type", &val);
  record_type = val;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_full_sensor_record);
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_compact_sensor_record);
  else if (record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_event_only_record);
  else if (record_type == IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD)
    FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_entity_association_record);
  else if (record_type == IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)
    FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_device_relative_entity_association_record);
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
    FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_generic_device_locator_record);
  else if (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
      FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_fru_device_locator_record);
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
      FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_management_controller_device_locator_record);
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
      FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_management_controller_confirmation_record);
  else if (record_type == IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD)
      FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_bmc_message_channel_info_record);
  else if (record_type == IPMI_SDR_FORMAT_OEM_RECORD)
      FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_oem_record);
  else
    /* I don't know this record_type, maybe its in a newer spec?? */
    FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_record_header);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record,
                           sdr_record,
                           sdr_record_len);

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      uint8_t event_reading_type_code;
      fiid_obj_t obj_temp = NULL;

      FIID_OBJ_GET_CLEANUP(obj_sdr_record, 
                           "event_reading_type_code",
                           &val);
      event_reading_type_code = val;

      if ((IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code)))
        FIID_OBJ_COPY_CLEANUP(obj_temp,
                              obj_sdr_record,
                              tmpl_sdr_full_sensor_record_threshold_based_sensors);
      else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code)
               || IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC(event_reading_type_code))
        FIID_OBJ_COPY_CLEANUP(obj_temp,
                              obj_sdr_record,
                              tmpl_sdr_full_sensor_record_non_threshold_based_sensors);

      if (obj_temp)
        {
          FIID_OBJ_DESTROY(obj_sdr_record);
          obj_sdr_record = obj_temp;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      uint8_t event_reading_type_code;
      fiid_obj_t obj_temp = NULL;

      FIID_OBJ_GET_CLEANUP(obj_sdr_record, 
                           "event_reading_type_code",
                           &val);
      event_reading_type_code = val;

      /* achu: shouldn't be possible for this to be a threshold based
       * sensor, but we'll allow it just in case.
       */

      if ((IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code)))
        FIID_OBJ_COPY_CLEANUP(obj_temp,
                              obj_sdr_record,
                              tmpl_sdr_compact_sensor_record_threshold_based_sensors);
      else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code)
               || IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC(event_reading_type_code))
        FIID_OBJ_COPY_CLEANUP(obj_temp,
                              obj_sdr_record,
                              tmpl_sdr_compact_sensor_record_non_threshold_based_sensors);

      if (obj_temp)
        {
          FIID_OBJ_DESTROY(obj_sdr_record);
          obj_sdr_record = obj_temp;
        }
    }

  ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                               prefix, 
                               NULL, 
                               NULL, 
                               obj_sdr_record) < 0));

  ERR_CLEANUP (!(ipmi_debug_output_str(fd, prefix_buf, trlr) < 0));
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY(obj_sdr_record_header);
  FIID_OBJ_DESTROY(obj_sdr_record);
  return (rv);
}

