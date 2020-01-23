/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/oem/ipmi-event-reading-type-code-oem-intel-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-intel-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-numbers-oem-intel-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-intel-node-manager.h"
#include "ipmi-sel-string-intel-xeon-common.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define INTEL_EVENT_BUFFER_LENGTH 4096

int
sel_string_output_intel_s2600bpb_sensor_name (ipmi_sel_ctx_t ctx,
                                              struct ipmi_sel_entry *sel_entry,
                                              uint8_t sel_record_type,
                                              char *buf,
                                              unsigned int buflen,
                                              unsigned int flags,
                                              unsigned int *wlen,
                                              struct ipmi_sel_system_event_record_data *system_event_record_data,
                                              int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((ret = sel_string_output_intel_xeon_sensor_name (ctx,
                                                       sel_entry,
                                                       sel_record_type,
                                                       buf,
                                                       buflen,
                                                       flags,
                                                       wlen,
                                                       system_event_record_data,
                                                       oem_rv)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600bpb_event_data1_class_oem (ipmi_sel_ctx_t ctx,
                                                       struct ipmi_sel_entry *sel_entry,
                                                       uint8_t sel_record_type,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((ret = sel_string_output_intel_xeon_event_data1_class_oem (ctx,
                                                                 sel_entry,
                                                                 sel_record_type,
                                                                 tmpbuf,
                                                                 tmpbuflen,
                                                                 flags,
                                                                 wlen,
                                                                 system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_NVME1_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_NVME2_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_NVME3_CRIT_WARN)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NVME_CRITICAL_WARNING_SENSOR)
    {
      uint8_t drive;

      drive = (system_event_record_data->event_data1 & IPMI_OEM_INTEL_NVME_CRITICAL_WARNING_EVENT_DATA1_DISK_DRIVE_BITMASK);
      drive >>= IPMI_OEM_INTEL_NVME_CRITICAL_WARNING_EVENT_DATA1_DISK_DRIVE_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Drive %u",
                drive);
      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600bpb_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
                                                          struct ipmi_sel_entry *sel_entry,
                                                          uint8_t sel_record_type,
                                                          char *tmpbuf,
                                                          unsigned int tmpbuflen,
                                                          unsigned int flags,
                                                          unsigned int *wlen,
                                                          struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((ret = sel_string_output_intel_xeon_event_data2_discrete_oem (ctx,
                                                                    sel_entry,
                                                                    sel_record_type,
                                                                    tmpbuf,
                                                                    tmpbuflen,
                                                                    flags,
                                                                    wlen,
                                                                    system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_XEON_SYSTEM_EVENT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_IMAGE_IS_UPLOADED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_IMAGE_IS_LOST))
    {
      char *str;

      switch (system_event_record_data->event_data2)
        {
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_EVENT_DATA2_BIOS_CONFIGURATION_TABLE:
          str = "BIOS Configuration Table";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_EVENT_DATA2_BIOS_CONFIGURATION_CHANGE:
          str = "BIOS Configuration change";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_EVENT_DATA2_BIOS_IMAGE:
          str = "BIOS Image";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_EVENT_DATA2_ME_IMAGE:
          str = "ME Image";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_EVENT_DATA2_FD_IMAGE:
          str = "FD Image";
          break;
        default:
          str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "%s",
                str);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600bpb_event_data2_class_oem (ipmi_sel_ctx_t ctx,
                                                       struct ipmi_sel_entry *sel_entry,
                                                       uint8_t sel_record_type,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((ret = sel_string_output_intel_xeon_event_data2_class_oem (ctx,
                                                                 sel_entry,
                                                                 sel_record_type,
                                                                 tmpbuf,
                                                                 tmpbuflen,
                                                                 flags,
                                                                 wlen,
                                                                 system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_NVME1_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_NVME2_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_NVME3_CRIT_WARN)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NVME_CRITICAL_WARNING_SENSOR)
    {
      char smart_warning_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      unsigned int lentmp = 0;

      memset (smart_warning_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_SPARE_SPACE_BELOW_THRESHOLD)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "Spare space below threshold"))
            return (1);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_TEMPERATURE_ABOVE_OR_BELOW_THRESHOLD)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "Temperature above or below threshold"))
            return (1);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_NVM_RELIABILITY_DEGRADED)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "NVM reliability degraded"))
            return (1);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_IN_READ_ONLY_MODE)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "In read-only mode"))
            return (1);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_VOLATILE_BACKUP_SERVICE_FAILED)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "Volatile backup service failed"))
            return (1);
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "SMART Warning = %s",
                smart_warning_str);
      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600bpb_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
                                                          struct ipmi_sel_entry *sel_entry,
                                                          uint8_t sel_record_type,
                                                          char *tmpbuf,
                                                          unsigned int tmpbuflen,
                                                          unsigned int flags,
                                                          unsigned int *wlen,
                                                          struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((ret = sel_string_output_intel_xeon_event_data3_discrete_oem (ctx,
                                                                    sel_entry,
                                                                    sel_record_type,
                                                                    tmpbuf,
                                                                    tmpbuflen,
                                                                    flags,
                                                                    wlen,
                                                                    system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR1_THERMAL_TRIP
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR2_THERMAL_TRIP
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR3_THERMAL_TRIP
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR4_THERMAL_TRIP)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CRITICAL_OVERTEMPERATURE)
    {
      sel_string_output_intel_xeon_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);
      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SESSION_AUDIT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BAD_USE_PWD
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SESSION_AUDIT_INVALID_USERNAME_OR_PASSWORD
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SESSION_AUDIT_INVALID_PASSWORD_DISABLE))
    {
      uint8_t channel;

      channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_SESSION_AUDIT_EVENT_DATA3_OEM_INTEL_CHANNEL_BITMASK);
      channel >>= IPMI_SENSOR_TYPE_SESSION_AUDIT_EVENT_DATA3_OEM_INTEL_CHANNEL_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Channel = %u",
                channel);
      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600bpb_event_data3_class_oem (ipmi_sel_ctx_t ctx,
                                                       struct ipmi_sel_entry *sel_entry,
                                                       uint8_t sel_record_type,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((ret = sel_string_output_intel_xeon_event_data3_class_oem (ctx,
                                                                 sel_entry,
                                                                 sel_record_type,
                                                                 tmpbuf,
                                                                 tmpbuflen,
                                                                 flags,
                                                                 wlen,
                                                                 system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_intel_s2600bpb_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
                                                         struct ipmi_sel_entry *sel_entry,
                                                         uint8_t sel_record_type,
                                                         char *buf,
                                                         unsigned int buflen,
                                                         unsigned int flags,
                                                         unsigned int *wlen,
                                                         struct ipmi_sel_system_event_record_data *system_event_record_data,
                                                         int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((ret = sel_string_output_intel_xeon_event_data2_event_data3 (ctx,
                                                                   sel_entry,
                                                                   sel_record_type,
                                                                   buf,
                                                                   buflen,
                                                                   flags,
                                                                   wlen,
                                                                   system_event_record_data,
                                                                   oem_rv)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

struct sel_string_oem sel_string_oem_intel_s2600bpb =
  {
    sel_string_output_intel_s2600bpb_sensor_name,
    NULL,
    sel_string_output_intel_s2600bpb_event_data1_class_oem,
    NULL,
    sel_string_output_intel_s2600bpb_event_data2_discrete_oem,
    sel_string_output_intel_s2600bpb_event_data2_class_oem,
    NULL,
    sel_string_output_intel_s2600bpb_event_data3_discrete_oem,
    sel_string_output_intel_s2600bpb_event_data3_class_oem,
    sel_string_output_intel_s2600bpb_event_data2_event_data3,
    NULL,
    NULL,
  };
