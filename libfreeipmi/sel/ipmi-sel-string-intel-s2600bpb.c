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
#include "freeipmi/spec/oem/ipmi-sensor-types-oem-intel-spec.h"
#include "freeipmi/spec/oem/ipmi-slave-address-oem-intel-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

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
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME1_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME2_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME3_CRIT_WARN)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING)
    {
      uint8_t drive;

      drive = (system_event_record_data->event_data1 & IPMI_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING_EVENT_DATA1_DISK_DRIVE_BITMASK);
      drive >>= IPMI_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING_EVENT_DATA1_DISK_DRIVE_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Drive %u",
                drive);
      return (1);
    }


  if ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_S2600BPB_REMOTE_DEBUG
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_REMOTE_DEBUG
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_REMOTE_DEBUG)
      || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_S2600BPB_SYSTEM_FIRMWARE_SECURITY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_FIRMWARE_SECURITY
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_SYSTEM_FIRMWARE_SECURITY)
      || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_S2600BPB_KCS_POLICY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_KCS_POLICY
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_KCS_POLICY))
    {
      int ret;

      ret = ipmi_get_oem_specific_message (ctx->manufacturer_id,
                                           ctx->product_id,
                                           system_event_record_data->event_type_code,
                                           system_event_record_data->sensor_type,
                                           system_event_record_data->offset_from_event_reading_type_code,
                                           tmpbuf,
                                           tmpbuflen);

      if (ret > 0)
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
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_IMAGE_IS_UPLOADED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_IMAGE_IS_LOST)
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      char *str;

      switch (system_event_record_data->event_data2)
        {
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_EVENT_DATA2_BIOS_CONFIGURATION_TABLE:
          str = "BIOS Configuration Table";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_EVENT_DATA2_BIOS_CONFIGURATION_CHANGE:
          str = "BIOS Configuration change";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_EVENT_DATA2_BIOS_IMAGE:
          str = "BIOS Image";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_EVENT_DATA2_ME_IMAGE:
          str = "ME Image";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_EVENT_DATA2_FD_IMAGE:
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

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MIRRORING_REDUNDANCY_STATE
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
       && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
           || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED)
       && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR)
          && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE))
    {
      uint8_t dimm_index, rank_index;

      dimm_index = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_EVENT_DATA2_DIMM_INDEX_BITMASK);
      dimm_index >>= IPMI_OEM_INTEL_S2600BPB_EVENT_DATA2_DIMM_INDEX_SHIFT;

      rank_index = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_EVENT_DATA2_RANK_INDEX_BITMASK);
      rank_index >>= IPMI_OEM_INTEL_S2600BPB_EVENT_DATA2_RANK_INDEX_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "DIMM Index = %u, Rank Index = %u",
                dimm_index, rank_index);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PARITY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t dimm_index;

      dimm_index = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_EVENT_DATA2_DIMM_INDEX_BITMASK);
      dimm_index >>= IPMI_OEM_INTEL_S2600BPB_EVENT_DATA2_DIMM_INDEX_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "DIMM Index = %u",
                dimm_index);

      return (1);
    }

  return (0);
}


/* documentation is ambiguous if this is in event data2 or 3.  It's a
 * clear error in the document.  So we will try both, with callers
 * passing in event data 2 or 3 for parsing.
 */
static int
_sel_string_output_intel_s2600bpb_remote_debug_sensor (ipmi_sel_ctx_t ctx,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       uint8_t event_data)
{
  char info_str[INTEL_EVENT_BUFFER_LENGTH + 1];
  unsigned int lentmp = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  memset (info_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

  if (event_data & IPMI_OEM_INTEL_S2600BPB_SPECIFIC_REMOTE_DEBUG_EVENT_DATA_JTAG_SESSION_STATE_BITMASK)
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "JTAG Session State In Progress"))
        return (0);
    }
  else
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "JTAG Session Idle"))
        return (0);
    }

  if (event_data & IPMI_OEM_INTEL_S2600BPB_SPECIFIC_REMOTE_DEBUG_EVENT_DATA_JTAG_ENABLED_BITMASK)
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "JTAG Enabled"))
        return (0);
    }
  else
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "JTAG Disabled"))
        return (0);
    }

  if (event_data & IPMI_OEM_INTEL_S2600BPB_SPECIFIC_REMOTE_DEBUG_EVENT_DATA_JTAG_DEBUG_CONSENT_BITMASK)
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "JTAG Debug Consent given"))
        return (0);
    }
  else
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "JTAG Debug Consent not given"))
        return (0);
    }

  if (event_data & IPMI_OEM_INTEL_S2600BPB_SPECIFIC_REMOTE_DEBUG_EVENT_DATA_PECI_SESSION_STATE_BITMASK)
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "PECI Session State In Progress"))
        return (0);
    }
  else
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "PECI Session Idle"))
        return (0);
    }

  if (event_data & IPMI_OEM_INTEL_S2600BPB_SPECIFIC_REMOTE_DEBUG_EVENT_DATA_PECI_ENABLED_BITMASK)
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "PECI Enabled"))
        return (0);
    }
  else
    {
      if (sel_string_strcat_comma_separate (info_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "PECI Disabled"))
        return (0);
    }

  snprintf (tmpbuf,
            tmpbuflen,
            "%s",
            info_str);

  return (1);
}

/* documentation is ambiguous if this is in event data2 or 3.  It's a
 * clear error in the document.  So we will try both, with callers
 * passing in event data 2 or 3 for parsing.
 */
static int
_sel_string_output_intel_s2600bpb_system_firmware_security_sensor (ipmi_sel_ctx_t ctx,
                                                                   char *tmpbuf,
                                                                   unsigned int tmpbuflen,
                                                                   unsigned int flags,
                                                                   unsigned int *wlen,
                                                                   uint8_t event_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  snprintf (tmpbuf,
            tmpbuflen,
            "Image Type = %u",
            event_data);

  return (1);
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
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME1_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME2_CRIT_WARN
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME3_CRIT_WARN)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING)
    {
      char smart_warning_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      unsigned int lentmp = 0;

      memset (smart_warning_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_SPARE_SPACE_BELOW_THRESHOLD)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "Spare space below threshold"))
            return (0);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_TEMPERATURE_ABOVE_OR_BELOW_THRESHOLD)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "Temperature above or below threshold"))
            return (0);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_NVM_RELIABILITY_DEGRADED)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "NVM reliability degraded"))
            return (0);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_IN_READ_ONLY_MODE)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "In read-only mode"))
            return (0);
        }
      if (system_event_record_data->event_data2 & IPMI_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING_EVENT_DATA2_DISK_DRIVE_VOLATILE_BACKUP_SERVICE_FAILED)
        {
          if (sel_string_strcat_comma_separate (smart_warning_str,
                                                INTEL_EVENT_BUFFER_LENGTH,
                                                &lentmp,
                                                "Volatile backup service failed"))
            return (0);
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "SMART Warning = %s",
                smart_warning_str);
      return (1);
    }

  /* Documentation is ambiguous on event data2 or 3, so we check under both circumstances */
  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_S2600BPB_REMOTE_DEBUG
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_REMOTE_DEBUG
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_REMOTE_DEBUG
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      if ((ret = _sel_string_output_intel_s2600bpb_remote_debug_sensor (ctx,
                                                                        tmpbuf,
                                                                        tmpbuflen,
                                                                        flags,
                                                                        wlen,
                                                                        system_event_record_data->event_data2) < 0))
        return (-1);

      if (ret)
        return (1);
    }

  /* Documentation is ambiguous on event data2 or 3, so we check under both circumstances */
  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_S2600BPB_SYSTEM_FIRMWARE_SECURITY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_FIRMWARE_SECURITY
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_SYSTEM_FIRMWARE_SECURITY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      if ((ret = _sel_string_output_intel_s2600bpb_system_firmware_security_sensor (ctx,
                                                                                    tmpbuf,
                                                                                    tmpbuflen,
                                                                                    flags,
                                                                                    wlen,
                                                                                    system_event_record_data->event_data2) < 0))
        return (-1);

      if (ret)
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
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CRITICAL_OVERTEMPERATURE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      sel_string_output_intel_xeon_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);
      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_XEON_SYSTEM_EVENT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_IMAGE_IS_UPLOADED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_IMAGE_IS_LOST)
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      char *str;

      switch (system_event_record_data->event_data3)
        {
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_EVENT_DATA3_FIRMWARE_UPDATE:
          str = "Firmware Update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_INTEL_S2600BPB_EVENT_DATA3_BIOS_CONFIGURATION:
          str = "BIOS Configuration";
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

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SESSION_AUDIT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_BAD_USE_PWD
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SESSION_AUDIT_INVALID_USERNAME_OR_PASSWORD
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SESSION_AUDIT_INVALID_PASSWORD_DISABLE)
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t userid;

      userid = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_SESSION_AUDIT_EVENT_DATA2_OEM_INTEL_S2600BPB_USERID_BITMASK);
      userid >>= IPMI_SENSOR_TYPE_SESSION_AUDIT_EVENT_DATA2_OEM_INTEL_S2600BPB_USERID_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Userid = %u",
                userid);
      return (1);
    }

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MIRRORING_REDUNDANCY_STATE
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
       && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
           || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED)
       && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR)
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PARITY
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE))
    {
      uint8_t socket_index, channel_index;
      char *str;

      socket_index = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_SOCKET_INDEX_BITMASK);
      socket_index >>= IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_SOCKET_INDEX_SHIFT;

      channel_index = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_CHANNEL_INDEX_BITMASK);
      channel_index >>= IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_CHANNEL_INDEX_SHIFT;

      switch (socket_index)
        {
        case IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_SOCKET_INDEX_CPU1:
          str = "CPU 1";
          break;
        case IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_SOCKET_INDEX_CPU2:
          str = "CPU 2";
          break;
        case IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_SOCKET_INDEX_CPU3:
          str = "CPU 3";
          break;
        case IPMI_OEM_INTEL_S2600BPB_EVENT_DATA3_SOCKET_INDEX_CPU4:
          str = "CPU 4";
          break;
        default:
          str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "%s, Channel Index = %u",
                str, channel_index);

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

  /* Documentation is ambiguous on event data2 or 3, so we check under both circumstances */
  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_S2600BPB_REMOTE_DEBUG
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_REMOTE_DEBUG
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_REMOTE_DEBUG
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      if ((ret = _sel_string_output_intel_s2600bpb_remote_debug_sensor (ctx,
                                                                        tmpbuf,
                                                                        tmpbuflen,
                                                                        flags,
                                                                        wlen,
                                                                        system_event_record_data->event_data3) < 0))
        return (-1);

      if (ret)
        return (1);
    }

  /* Documentation is ambiguous on event data2 or 3, so we check under both circumstances */
  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_S2600BPB_SYSTEM_FIRMWARE_SECURITY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_FIRMWARE_SECURITY
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_SYSTEM_FIRMWARE_SECURITY
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      if ((ret = _sel_string_output_intel_s2600bpb_system_firmware_security_sensor (ctx,
                                                                                    tmpbuf,
                                                                                    tmpbuflen,
                                                                                    flags,
                                                                                    wlen,
                                                                                    system_event_record_data->event_data3) < 0))
        return (-1);

      if (ret)
        return (1);
    }

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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_POST_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_SYSTEM_FIRMWARE_ERROR
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint16_t error_code;
      char *error_code_str = NULL;

      error_code = system_event_record_data->event_data2;
      error_code |= (system_event_record_data->event_data3 << 8);

      switch (error_code)
        {
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_SYSTEM_RTC_DATE_TIME_NOT_SET:
          error_code_str = "System RTC date/time not set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
          error_code_str = "Password check failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT:
          error_code_str = "PCI resource conflict";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
          error_code_str = "PCI out of resources error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_CORE_THREAD_COUNT_MISMATCH_DETECTED:
          error_code_str = "Processor core/thread count mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_CACHE_SIZE_MISMATCH_DETECTED:
          error_code_str = "Processor cache size mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_FAMILY_MISMATCH_DETECTED:
          error_code_str = "Processor family mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_INTEL_QPI_LINK_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor Intel(R) QPI link frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_MODEL_MISMATCH_DETECTED:
          error_code_str = "Processor model mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_BIOS_SETTINGS_RESET_TO_DEFAULT_SETTINGS:
          error_code_str = "BIOS Settings reset to default settings";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
          error_code_str = "Passwords cleared by jumper";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
          error_code_str = "Password clear jumper is set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_01_DISABLED:
          error_code_str = "Processor 01 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_02_DISABLED:
          error_code_str = "Processor 02 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 01 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 02 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_01_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 01 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_02_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 02 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_01_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 01 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PROCESSOR_02_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 02 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
          error_code_str = "Watchdog timer failed on last boot";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
          error_code_str = "OS boot watchdog timer failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
          error_code_str = "Baseboard management controller failed self-test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_HOT_SWAP_CONTROLLER_FAILURE:
          error_code_str = "Hot-Swap Controller failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MANAGEMENT_ENGINE_ME_FAILED_SELF_TEST:
          error_code_str = "Management Engine (ME) failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MANAGEMENT_ME_FAILED_TO_RESPOND:
          error_code_str = "Management Engine (ME) Failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
          error_code_str = "Baseboard management controller failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
          error_code_str = "Baseboard management controller in update mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_SENSOR_DATA_RECORD_EMPTY:
          error_code_str = "Baseboard management controller Sensor data record empty";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL:
          error_code_str = "System event log full";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE:
          error_code_str = "Memory component could not be configured in the selected RAS mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_POPULATION_ERROR:
          error_code_str = "Memory Population Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_A1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_A1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_A2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_A2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_A3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_A3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_B1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_B1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_B2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_B2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_B3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_B3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_C1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_C1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_C2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_C2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_C3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_C3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_D1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_D1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_D2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_D2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_D3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_D3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_E1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_E1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_E2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_E2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_E3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_E3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_F1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_F1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_F2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_F2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_F3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_F3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_G1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_G1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_G2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_G2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_G3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_G3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_H1:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_H1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_H2:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_H2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU1_DIMM_H3:
          error_code_str = "Memory failed test/initialization CPU1_DIMM_H3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_A1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_A1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_A2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_A2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_A3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_A3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_B1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_B1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_B2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_B2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_B3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_B3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_C1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_C1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_C2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_C2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_C3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_C3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_D1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_D1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_D2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_D2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_D3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_D3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_E1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_E1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_E2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_E2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_E3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_E3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_F1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_F1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_F2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_F2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_F3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_F3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_G1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_G1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_G2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_G2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_G3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_G3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_H1:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_H1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_H2:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_H2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_FAILED_TEST_INITIALIZATION_CPU2_DIMM_H3:
          error_code_str = "Memory failed test/initialization CPU2_DIMM_H3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_A1:
          error_code_str = "Memory disabled CPU1_DIMM_A1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_A2:
          error_code_str = "Memory disabled CPU1_DIMM_A2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_A3:
          error_code_str = "Memory disabled CPU1_DIMM_A3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_B1:
          error_code_str = "Memory disabled CPU1_DIMM_B1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_B2:
          error_code_str = "Memory disabled CPU1_DIMM_B2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_B3:
          error_code_str = "Memory disabled CPU1_DIMM_B3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_C1:
          error_code_str = "Memory disabled CPU1_DIMM_C1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_C2:
          error_code_str = "Memory disabled CPU1_DIMM_C2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_C3:
          error_code_str = "Memory disabled CPU1_DIMM_C3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_D1:
          error_code_str = "Memory disabled CPU1_DIMM_D1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_D2:
          error_code_str = "Memory disabled CPU1_DIMM_D2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_D3:
          error_code_str = "Memory disabled CPU1_DIMM_D3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_E1:
          error_code_str = "Memory disabled CPU1_DIMM_E1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_E2:
          error_code_str = "Memory disabled CPU1_DIMM_E2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_E3:
          error_code_str = "Memory disabled CPU1_DIMM_E3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_F1:
          error_code_str = "Memory disabled CPU1_DIMM_F1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_F2:
          error_code_str = "Memory disabled CPU1_DIMM_F2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_F3:
          error_code_str = "Memory disabled CPU1_DIMM_F3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_G1:
          error_code_str = "Memory disabled CPU1_DIMM_G1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_G2:
          error_code_str = "Memory disabled CPU1_DIMM_G2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_G3:
          error_code_str = "Memory disabled CPU1_DIMM_G3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_H1:
          error_code_str = "Memory disabled CPU1_DIMM_H1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_H2:
          error_code_str = "Memory disabled CPU1_DIMM_H2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU1_DIMM_H3:
          error_code_str = "Memory disabled CPU1_DIMM_H3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_A1:
          error_code_str = "Memory disabled CPU2_DIMM_A1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_A2:
          error_code_str = "Memory disabled CPU2_DIMM_A2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_A3:
          error_code_str = "Memory disabled CPU2_DIMM_A3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_B1:
          error_code_str = "Memory disabled CPU2_DIMM_B1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_B2:
          error_code_str = "Memory disabled CPU2_DIMM_B2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_B3:
          error_code_str = "Memory disabled CPU2_DIMM_B3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_C1:
          error_code_str = "Memory disabled CPU2_DIMM_C1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_C2:
          error_code_str = "Memory disabled CPU2_DIMM_C2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_C3:
          error_code_str = "Memory disabled CPU2_DIMM_C3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_D1:
          error_code_str = "Memory disabled CPU2_DIMM_D1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_D2:
          error_code_str = "Memory disabled CPU2_DIMM_D2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_D3:
          error_code_str = "Memory disabled CPU2_DIMM_D3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_E1:
          error_code_str = "Memory disabled CPU2_DIMM_E1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_E2:
          error_code_str = "Memory disabled CPU2_DIMM_E2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_E3:
          error_code_str = "Memory disabled CPU2_DIMM_E3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_F1:
          error_code_str = "Memory disabled CPU2_DIMM_F1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_F2:
          error_code_str = "Memory disabled CPU2_DIMM_F2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_F3:
          error_code_str = "Memory disabled CPU2_DIMM_F3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_G1:
          error_code_str = "Memory disabled CPU2_DIMM_G1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_G2:
          error_code_str = "Memory disabled CPU2_DIMM_G2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_G3:
          error_code_str = "Memory disabled CPU2_DIMM_G3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_H1:
          error_code_str = "Memory disabled CPU2_DIMM_H1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_H2:
          error_code_str = "Memory disabled CPU2_DIMM_H2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_DISABLED_CPU2_DIMM_H3:
          error_code_str = "Memory disabled CPU2_DIMM_H3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_A1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_A1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_A2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_A2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_A3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_A3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_B1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_B1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_B2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_B2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_B3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_B3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_C1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_C1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_C2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_C2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_C3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_C3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_D1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_D1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_D2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_D2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_D3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_D3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_E1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_E1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_E2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_E2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_E3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_E3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_F1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_F1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_F2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_F2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_F3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_F3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_G1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_G1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_G2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_G2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_G3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_G3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_H1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_H1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_H2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_H2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU1_DIMM_H3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU1_DIMM_H3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_A1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_A1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_A2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_A2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_A3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_A3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_B1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_B1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_B2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_B2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_B3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_B3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_C1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_C1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_C2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_C2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_C3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_C3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_D1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_D1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_D2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_D2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_D3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_D3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_E1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_E1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_E2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_E2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_E3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_E3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_F1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_F1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_F2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_F2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_F3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_F3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_G1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_G1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_G2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_G2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_G3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_G3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_H1:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_H1";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_H2:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_H2";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_MEMORY_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE_CPU2_DIMM_H3:
          error_code_str = "Memory encountered a Serial Presence Detection (SPD) failure CPU2_DIMM_H3";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_POST_RECLAIM_OF_NON_CRITICAL_NVRAM_VARIABLES:
          error_code_str = "POST Reclaim of non-critical NVRAM variables";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_BIOS_SETTINGS_ARE_CORRUPTED:
          error_code_str = "BIOS Settings are corrupted";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_NVRAM_VARIABLE_SPACE_WAS_CORRUPTED_AND_HAS_BEEN_REINITIALIZED:
          error_code_str = "NVRAM variable space was corrupted and has been reinitialized";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_RECOVERY_BOOT_HAS_BEEN_INITIATED:
          error_code_str = "Recovery boot has been initiated";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_BIOS_ACM_ERROR:
          error_code_str = "BIOS ACM Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI Express component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI Express component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_SKYLAKE_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM:
          error_code_str = "DXE boot services driver Not enough memory available to shadow a legacy option ROM";
          break;
        default:
          error_code_str = "Undefined Post Error";
        }

      if (sel_string_snprintf (buf,
                               buflen,
                               wlen,
                               "%s",
                               error_code_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;

      return (1);
    }

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
