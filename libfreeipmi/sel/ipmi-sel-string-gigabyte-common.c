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
#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-gigabyte-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_gigabyte_common_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
                                                               struct ipmi_sel_entry *sel_entry,
                                                               uint8_t sel_record_type,
                                                               char *tmpbuf,
                                                               unsigned int tmpbuflen,
                                                               unsigned int flags,
                                                               unsigned int *wlen,
                                                               struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_GIGABYTE);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_GIGABYTE_PRODUCT_ID_MD90_FS0_ZB
          || ctx->product_id == IPMI_GIGABYTE_PRODUCT_ID_MG20_OP0_ZB);

  /*
   * Gigabyte MD90-FS0-ZB
   * Gigabyte MG20-OP0-ZB
   */

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Bus %u",
                system_event_record_data->event_data2);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_gigabyte_common_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
                                                            struct ipmi_sel_entry *sel_entry,
                                                            uint8_t sel_record_type,
                                                            char *tmpbuf,
                                                            unsigned int tmpbuflen,
                                                            unsigned int flags,
                                                            unsigned int *wlen,
                                                            struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_GIGABYTE);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_GIGABYTE_PRODUCT_ID_MD90_FS0_ZB
          || ctx->product_id == IPMI_GIGABYTE_PRODUCT_ID_MG20_OP0_ZB);

  /*
   * Gigabyte MD90-FS0-ZB
   * Gigabyte MG20-OP0-ZB
   */

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      uint8_t device, function;

      device = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_GIGABYTE_DEVICE_NUMBER_BITMASK);
      device >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_GIGABYTE_DEVICE_NUMBER_SHIFT;

      function = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_GIGABYTE_FUNCTION_NUMBER_BITMASK);
      function >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_GIGABYTE_FUNCTION_NUMBER_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Device %u, Function %u",
                device,
                function);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR_LOGGING_LIMIT_REACHED))
    {
      uint8_t channel, dimm;
      char *channel_str, *dimm_str;

      if (system_event_record_data->event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Cannot determine which DIMM error occurred on");
          return (1);
        }

      channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_BITMASK);
      channel >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_SHIFT;

      dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_DIMM_BITMASK);
      dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_DIMM_SHIFT;

      switch (channel)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_A:
          channel_str = "A";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_B:
          channel_str = "B";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_C:
          channel_str = "C";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_D:
          channel_str = "D";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_E:
          channel_str = "E";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_F:
          channel_str = "F";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_G:
          channel_str = "G";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_CHANNEL_H:
          channel_str = "H";
          break;
        default:
          channel_str = "Unknown";
          break;
        }

      switch (dimm)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_DIMM_0:
          dimm_str = "0";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_DIMM_1:
          dimm_str = "1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_DIMM_2:
          dimm_str = "2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_GIGABYTE_DIMM_3:
          dimm_str = "3";
          break;
        default:
          dimm_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Channel %s, DIMM %s",
                channel_str,
                dimm_str);

      return (1);
    }

  return (0);
}
