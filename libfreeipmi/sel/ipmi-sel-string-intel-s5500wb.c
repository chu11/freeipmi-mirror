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
#include "freeipmi/spec/oem/ipmi-slave-address-oem-intel-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-intel-node-manager.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define INTEL_EVENT_BUFFER_LENGTH 4096

int
sel_string_output_intel_s5500wb_sensor_name (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB);

  if ((ret = sel_string_output_intel_node_manager_sensor_name (ctx,
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
sel_string_output_intel_s5500wb_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB);

  if ((ret = sel_string_output_intel_node_manager_event_data1_class_oem (ctx,
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

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
    {
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


  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_CORRECTABLE_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_CORRECTABLE_SENSOR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Correctable Sensor Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_NON_FATAL_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_NON_FATAL_SENSOR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Non-Fatal Sensor Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_SENSOR_A
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_SENSOR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Fatal Sensor A Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_SENSOR_B
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_SENSOR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Fatal Sensor B Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  return (0);
}

static void
_sel_string_output_intel_s5500wb_bus (ipmi_sel_ctx_t ctx,
                                      char *tmpbuf,
                                      unsigned int tmpbuflen,
                                      unsigned int flags,
                                      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  snprintf (tmpbuf,
            tmpbuflen,
            "Bus %u",
            system_event_record_data->event_data2);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s5500wb_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_s5500wb_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      uint8_t logical_rank;

      logical_rank = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOGICAL_RANK_BITMASK);
      logical_rank >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOGICAL_RANK_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Logical Rank %u",
                logical_rank);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s5500wb_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB);

  if ((ret = sel_string_output_intel_node_manager_event_data2_class_oem (ctx,
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

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
    {
      _sel_string_output_intel_s5500wb_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_CORRECTABLE_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_CORRECTABLE_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_NON_FATAL_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_NON_FATAL_SENSOR)
          || ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_SENSOR_A
               || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_SENSOR_B)
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_SENSOR)))
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Socket %u",
                system_event_record_data->event_data2);

      return (1);
    }

  return (0);
}

static void
_sel_string_output_intel_s5500wb_device_function (ipmi_sel_ctx_t ctx,
                                                  char *tmpbuf,
                                                  unsigned int tmpbuflen,
                                                  unsigned int flags,
                                                  struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t device, function;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* From Bill Hannon @ Intel
   *
   * [7:3] = Device Number
   * [2:0] = Function Number
   */

  device = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_BITMASK);
  device >>= IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_SHIFT;

  function = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_BITMASK);
  function >>= IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_SHIFT;

  snprintf (tmpbuf,
            tmpbuflen,
            "Device %u, Function %u",
            device,
            function);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s5500wb_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_s5500wb_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      uint8_t processor_socket;
      uint8_t channel_number;
      uint8_t dimm_slot_id;
      char *processor_socket_str;
      char channel_number_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      char channel_number_char = 0;
      char *dimm_slot_id_str;
      int processor_socket_valid = 0;
      int channel_number_valid = 0;
      int dimm_slot_id_valid = 0;

      processor_socket = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_BITMASK);
      processor_socket >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_SHIFT;

      channel_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_NUMBER_BITMASK);
      channel_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_NUMBER_SHIFT;

      dimm_slot_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SLOT_ID_BITMASK);
      dimm_slot_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SLOT_ID_SHIFT;

      if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_1)
        {
          processor_socket_str = "1";
          processor_socket_valid++;
        }
      else if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_2)
        {
          processor_socket_str = "2";
          processor_socket_valid++;
        }
      else
        processor_socket_str = "Unknown";

      if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_A)
        {
          channel_number_char = 'A';
          channel_number_valid++;
        }
      else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_B)
        {
          channel_number_char = 'B';
          channel_number_valid++;
        }
      else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_C)
        {
          channel_number_char = 'C';
          channel_number_valid++;
        }

      if (processor_socket_valid && channel_number_valid)
        {
          /* If we're on socket #2, the DIMMs jump from A-C, to D-F */
          if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_2)
            channel_number_char += 3;
        }

      memset (channel_number_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
      if (channel_number_valid && channel_number_char)
        snprintf(channel_number_str,
                 INTEL_EVENT_BUFFER_LENGTH,
                 "%c",
                 channel_number_char);
      else
        snprintf(channel_number_str,
                 INTEL_EVENT_BUFFER_LENGTH,
                 "Unknown");

      if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SOCKET_1)
        {
          dimm_slot_id_str = "1";
          dimm_slot_id_valid++;
        }
      else if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SOCKET_2)
        {
          dimm_slot_id_str = "2";
          dimm_slot_id_valid++;
        }
      else
        dimm_slot_id_str = "Unknown";

      if (processor_socket_valid && channel_number_valid && dimm_slot_id_valid)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "DIMM = %s%s",
                    channel_number_str,
                    dimm_slot_id_str);
        }
      else
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Processor Socket = %s, Channel Number = %s, Dimm Slot = %s",
                  processor_socket_str,
                  channel_number_str,
                  dimm_slot_id_str);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s5500wb_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB);

  if ((ret = sel_string_output_intel_node_manager_event_data3_class_oem (ctx,
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

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
    {
      _sel_string_output_intel_s5500wb_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

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
sel_string_output_intel_s5500wb_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB);

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_ERROR
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
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_CMOS_DATE_TIME_NOT_SET:
          error_code_str = "CMOS date / time not set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
          error_code_str = "Password check failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_LOCKED_ERROR:
          error_code_str = "Keyboard component encountered a locked error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_STUCK_KEY_ERROR:
          error_code_str = "Keyboard component encountered a stuck key error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_FIXED_MEDIA_THE_SAS_RAID_FIRMWARE_CAN_NOT_RUN_PROPERLY:
          error_code_str = "Fixed Media The SAS RAID firmware can not run properly. The user should attempt to reflash the firmware";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT:
          error_code_str = "PCI resource conflict";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
          error_code_str = "PCI out of resources error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_CACHE_SIZE_MISMATCH_DETECTED:
          error_code_str = "Processor 0x cache size mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_STEPPING_MISMATCH:
          error_code_str = "Processor 0x stepping mismatch";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_MISMATCH_DETECTED:
          error_code_str = "Processor 0x family mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_INTEL_QPI_SPEED_MISMATCH:
          error_code_str = "Processor 0x Intel(R) QPI speed mismatch";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_MODEL_MISMATCH:
          error_code_str = "Processor 0x model mismatch";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_SPEEDS_MISMATCHED:
          error_code_str = "Processor 0x speeds mismatched";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_IS_NOT_SUPPORTED:
          error_code_str = "Processor 0x family is not supported";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_AND_CHIPSET_STEPPING_CONFIGURATION_IS_UNSUPPORTED:
          error_code_str = "Processor and chipset stepping configuration is unsupported";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_CMOS_NVRAM_CONFIGURATION_CLEARED:
          error_code_str = "CMOS/NVRAM Configuration Cleared";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
          error_code_str = "Passwords cleared by jumper";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
          error_code_str = "Password clear jumper is set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 01 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 02 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_0X_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 0x microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
          error_code_str = "Watchdog timer failed on last boot";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
          error_code_str = "OS boot watchdog timer failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
          error_code_str = "Baseboard management controller failed self-test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
          error_code_str = "Baseboard management controller failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
          error_code_str = "Baseboard management controller in update mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SENSOR_DATA_RECORD_EMPTY:
          error_code_str = "Sensor data record empty";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL:
          error_code_str = "System event log full";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE:
          error_code_str = "Memory component could not be configured in the selected RAS mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_POPULATION_ERROR:
          error_code_str = "DIMM Population Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_CLTT_CONFIGURATION_FAILURE_ERROR:
          error_code_str = "CLTT Configuration Failure Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A1_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_A1 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A2_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_A2 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B1_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_B1 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B2_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_B2 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C1_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_C1 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C2_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_C2 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D1_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_D1 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D2_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_D2 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E1_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_E1 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E2_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_E2 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F1_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_F1 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F2_FAILED_SELF_TEST_BIST:
          error_code_str = "DIMM_F2 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A1_DISABLED:
          error_code_str = "DIMM_A1 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A2_DISABLED:
          error_code_str = "DIMM_A2 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B1_DISABLED:
          error_code_str = "DIMM_B1 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B2_DISABLED:
          error_code_str = "DIMM_B2 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C1_DISABLED:
          error_code_str = "DIMM_C1 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C2_DISABLED:
          error_code_str = "DIMM_C2 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D1_DISABLED:
          error_code_str = "DIMM_D1 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D2_DISABLED:
          error_code_str = "DIMM_D2 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E1_DISABLED:
          error_code_str = "DIMM_E1 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E2_DISABLED:
          error_code_str = "DIMM_E2 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F1_DISABLED:
          error_code_str = "DIMM_F1 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F2_DISABLED:
          error_code_str = "DIMM_F2 Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_A1 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_A2 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_B1 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_B2 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_C1 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_C2 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_D1 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_D2 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_E1 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_E2 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_F1 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM_F2 Component encountered a Serial Presence Detection (SPD) fail error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_A1 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_A2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_A2 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_B1 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_B2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_B2 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_C1 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_C2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_C2 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_D1 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_D2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_D2 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_E1 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_E2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_E2 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_F1 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DIMM_F2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
          error_code_str = "DIMM_F2 Uncorrectable ECC error encountered";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_CHIPSET_RECLAIM_OF_NON_CRITICAL_VARIABLES_COMPLETE:
          error_code_str = "Chipset Reclaim of non critical variables complete";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_UNSPECIFIED_PROCESSOR_COMPONENT_HAS_ENCOUNTERED_A_NON_SPECIFIC_ERROR:
          error_code_str = "Unspecified processor component has encountered a non specific error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_KEYBOARD_COMPONENT_WAS_NOT_DETECTED:
          error_code_str = "Keyboard component was not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
          error_code_str = "Keyboard component encountered a controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_MOUSE_COMPONENT_WAS_NOT_DETECTED:
          error_code_str = "Mouse component was not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_MOUSE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
          error_code_str = "Mouse component encountered a controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
          error_code_str = "Local Console component encountered a controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
          error_code_str = "Local Console component encountered an output error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
          error_code_str = "Local Console component encountered a resource conflict error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
          error_code_str = "Remote Console component encountered a controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR:
          error_code_str = "Remote Console component encountered an input error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
          error_code_str = "Remote Console component encountered an output error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_WAS_NOT_DETECTED:
          error_code_str = "Serial port component was not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
          error_code_str = "Serial port component encountered a resource conflict error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SERIAL_PORT_CONTROLLER_ERROR:
          error_code_str = "Serial Port controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR:
          error_code_str = "Serial Port component encountered an input error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
          error_code_str = "Serial Port component encountered an output error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
          error_code_str = "LPC component encountered a controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
          error_code_str = "LPC component encountered a resource conflict error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_ATA_ATPI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
          error_code_str = "ATA/ATPI component encountered a controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
          error_code_str = "PCI component encountered a controller error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_READ_ERROR:
          error_code_str = "PCI component encountered a read error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_WRITE_ERROR:
          error_code_str = "PCI component encountered a write error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_UNSPECIFIED_SOFTWARE_COMPONENT_ENCOUNTERED_A_START_ERROR:
          error_code_str = "Unspecified software component encountered a start error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PEI_CORE_COMPONENT_ENCOUNTERED_A_LOAD_ERROR:
          error_code_str = "PEI Core component encountered a load error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PEI_MODULE_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
          error_code_str = "PEI module component encountered a illegal software state error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DXE_CORE_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
          error_code_str = "DXE core component encountered a illegal software state error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
          error_code_str = "DXE boot services driver component encountered a illegal software state error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_INVALID_CONFIGURATION:
          error_code_str = "DXE boot services driver component encountered invalid configuration";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_SMM_DRIVER_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
          error_code_str = "SMM driver component encountered a illegal software state error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED:
          error_code_str = "TPM device not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING:
          error_code_str = "TPM device missing or not responding";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_TPM_DEVICE_FAILURE:
          error_code_str = "TPM device failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST:
          error_code_str = "TPM device failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_MISMATCH_ERROR:
          error_code_str = "Processor component encountered a mismatch error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_LOW_VOLTAGE_ERROR:
          error_code_str = "Processor component encountered a low voltage error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_HIGH_VOLTAGE_ERROR:
          error_code_str = "Processor component encountered a high voltage error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_ATA_ATPI_ATA_BUS_SMART_NOT_SUPPORTED:
          error_code_str = "ATA/ATPI ATA bus SMART not supported";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_ATA_ATPI_ATA_SMART_IS_DISABLED:
          error_code_str = "ATA/ATPI ATA SMART is disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI Express component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI Express component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_PCI_EXPRESS_IBIST_ERROR:
          error_code_str = "PCI Express IBIST error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM:
          error_code_str = "DXE boot services driver Not enough memory available to shadow a legacy option ROM";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S5500WB_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_UNRECOGNIZED:
          error_code_str = "DXE boot services driver Unrecognized";
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

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S5500WB_MEMORY_PARITY_ERROR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PARITY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t channel_information_validity;
      uint8_t dimm_information_validity;
      uint8_t error_type;
      uint8_t processor_socket;
      uint8_t channel_number;
      uint8_t dimm_slot_id;
      char *error_type_str;
      char *processor_socket_str;
      char channel_number_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      char channel_number_char = 0;
      char *dimm_slot_id_str;
      int processor_socket_valid = 0;
      int channel_number_valid = 0;
      int dimm_slot_id_valid = 0;

      channel_information_validity = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_CHANNEL_INFORMATION_VALIDITY_BITMASK);
      channel_information_validity >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_CHANNEL_INFORMATION_VALIDITY_SHIFT;

      dimm_information_validity = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_DIMM_INFORMATION_VALIDITY_BITMASK);
      dimm_information_validity >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_DIMM_INFORMATION_VALIDITY_SHIFT;

      error_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_ERROR_TYPE_BITMASK);
      error_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_ERROR_TYPE_SHIFT;

      processor_socket = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_BITMASK);
      processor_socket >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_SHIFT;

      channel_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_NUMBER_BITMASK);
      channel_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_NUMBER_SHIFT;

      dimm_slot_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SLOT_ID_BITMASK);
      dimm_slot_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SLOT_ID_SHIFT;

      if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_ERROR_TYPE_DATA_PARITY_ERROR)
        error_type_str = "Data Parity Error";
      else if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_ERROR_TYPE_ADDRESS_PARITY_ERROR)
        error_type_str = "Address Parity Error";
      else
        error_type_str = "Unknown";

      if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_1)
        {
          processor_socket_str = "1";
          processor_socket_valid++;
        }
      else if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_2)
        {
          processor_socket_str = "2";
          processor_socket_valid++;
        }
      else
        processor_socket_str = "Unknown";

      if (channel_information_validity == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_CHANNEL_INFORMATION_VALID)
        {
          if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_A)
            {
              channel_number_char = 'A';
              channel_number_valid++;
            }
          else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_B)
            {
              channel_number_char = 'B';
              channel_number_valid++;
            }
          else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_CHANNEL_C)
            {
              channel_number_char = 'C';
              channel_number_valid++;
            }
        }

      if (processor_socket_valid && channel_number_valid)
        {
          /* If we're on socket #2, the DIMMs jump from A-C, to D-F */
          if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_PROCESSOR_SOCKET_2)
            channel_number_char += 3;
        }

      memset (channel_number_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
      if (channel_number_valid && channel_number_char)
        snprintf(channel_number_str,
                 INTEL_EVENT_BUFFER_LENGTH,
                 "%c",
                 channel_number_char);
      else
        snprintf(channel_number_str,
                 INTEL_EVENT_BUFFER_LENGTH,
                 "Unknown");

      if (dimm_information_validity == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_DIMM_INFORMATION_VALID)
        {
          if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SOCKET_1)
            {
              dimm_slot_id_str = "1";
              dimm_slot_id_valid++;
            }
          else if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DIMM_SOCKET_2)
            {
              dimm_slot_id_str = "2";
              dimm_slot_id_valid++;
            }
          else
            dimm_slot_id_str = "Unknown";
        }
      else
        dimm_slot_id_str = "Unknown";

      if (processor_socket_valid && channel_number_valid && dimm_slot_id_valid)
        {
          if (sel_string_snprintf (buf,
                                   buflen,
                                   wlen,
                                   "Error Type = %s, DIMM = %s%s",
                                   error_type_str,
                                   channel_number_str,
                                   dimm_slot_id_str))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
        }
      else
        {
          if (sel_string_snprintf (buf,
                                   buflen,
                                   wlen,
                                   "Error Type = %s, Processor Socket = %s, Channel Number = %s, Dimm Slot = %s",
                                   error_type_str,
                                   processor_socket_str,
                                   channel_number_str,
                                   dimm_slot_id_str))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
        }

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_MIRRORING_MODE
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_SPARING_MODE)
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_LOST)
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t domain_instance_type;
      uint8_t instance_id;

      domain_instance_type = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DOMAIN_INSTANCE_TYPE_BITMASK);
      domain_instance_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DOMAIN_INSTANCE_TYPE_SHIFT;

      instance_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_INSTANCE_ID_BITMASK);
      instance_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_INSTANCE_ID_SHIFT;

      if (domain_instance_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S5500WB_DOMAIN_INSTANCE_TYPE_LOCAL)
        {
          uint8_t mirroring_domain_local_subinstance;
          uint8_t socket_id;
          char mirroring_domain_local_subinstance_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
          char socket_id_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

          memset (mirroring_domain_local_subinstance_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
          memset (socket_id_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

          mirroring_domain_local_subinstance = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_BITMASK);
          mirroring_domain_local_subinstance >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_SHIFT;

          socket_id = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_SOCKET_ID_BITMASK);
          socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_SOCKET_ID_SHIFT;

          if (mirroring_domain_local_subinstance != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_UNUSED_FIELD)
            {
              char *mirroring_domain_local_subinstance_str = NULL;

              switch (mirroring_domain_local_subinstance)
                {
                case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_0_1:
                  mirroring_domain_local_subinstance_str = "{Ch0, Ch1}";
                  break;
                case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_0_2:
                  mirroring_domain_local_subinstance_str = "{Ch0, Ch2}";
                  break;
                case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_1_2:
                  mirroring_domain_local_subinstance_str = "{Ch1, Ch2}";
                  break;
                default:
                  mirroring_domain_local_subinstance_str = "Unknown";
                }

              snprintf (mirroring_domain_local_subinstance_buf,
                        INTEL_EVENT_BUFFER_LENGTH,
                        ", Subinstance = %s",
                        mirroring_domain_local_subinstance_str);
            }

          if (socket_id != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_UNUSED_FIELD)
            {
              if (socket_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_LOCAL_SOCKET_ID_APPLIES_TO_ALL_SOCKETS)
                snprintf (socket_id_buf,
                          INTEL_EVENT_BUFFER_LENGTH,
                          ", Applies to all sockets");
              else
                snprintf (socket_id_buf,
                          INTEL_EVENT_BUFFER_LENGTH,
                          ", Applies to Socket ID = %u",
                          socket_id);
            }

          if (sel_string_snprintf (buf,
                                   buflen,
                                   wlen,
                                   "Memory Mirroring Domain Instance Id = %u%s%s",
                                   instance_id,
                                   mirroring_domain_local_subinstance_buf,
                                   socket_id_buf))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
        }
      else
        {
          uint8_t first_socket_id;
          uint8_t second_socket_id;
          char first_socket_id_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
          char second_socket_id_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

          memset (first_socket_id_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
          memset (second_socket_id_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

          first_socket_id = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_GLOBAL_FIRST_SOCKET_ID_BITMASK);
          first_socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_GLOBAL_FIRST_SOCKET_ID_SHIFT;

          second_socket_id = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_GLOBAL_SECOND_SOCKET_ID_BITMASK);
          second_socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_GLOBAL_SECOND_SOCKET_ID_SHIFT;

          if (first_socket_id != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_UNUSED_FIELD)
            snprintf (first_socket_id_buf,
                      INTEL_EVENT_BUFFER_LENGTH,
                      ", First Socket ID = %u",
                      first_socket_id);

          if (second_socket_id != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S5500WB_UNUSED_FIELD)
            snprintf (second_socket_id_buf,
                      INTEL_EVENT_BUFFER_LENGTH,
                      ", Second Socket ID = %u",
                      second_socket_id);

          if (sel_string_snprintf (buf,
                                   buflen,
                                   wlen,
                                   "Memory Mirroring Domain Instance Id = %u%s%s",
                                   instance_id,
                                   first_socket_id_buf,
                                   second_socket_id_buf))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
        }

      return (1);
    }

  return (0);
}

struct sel_string_oem sel_string_oem_intel_s5500wb =
  {
    sel_string_output_intel_s5500wb_sensor_name,
    NULL,
    sel_string_output_intel_s5500wb_event_data1_class_oem,
    NULL,
    sel_string_output_intel_s5500wb_event_data2_discrete_oem,
    sel_string_output_intel_s5500wb_event_data2_class_oem,
    NULL,
    sel_string_output_intel_s5500wb_event_data3_discrete_oem,
    sel_string_output_intel_s5500wb_event_data3_class_oem,
    sel_string_output_intel_s5500wb_event_data2_event_data3,
    NULL,
    NULL,
  };
