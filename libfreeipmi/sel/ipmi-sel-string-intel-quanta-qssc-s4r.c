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
sel_string_output_intel_quanta_qssc_s4r_sensor_name (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R);

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
sel_string_output_intel_quanta_qssc_s4r_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R);

  /* Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
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
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR)))
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

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_CORRECTABLE_ERROR
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_CRC_ERROR_PERSISTENT)
    {
      char *event_msg_str = NULL;

      switch (system_event_record_data->offset_from_event_reading_type_code)
        {
        case IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_RECOVERABLE_ERROR:
          event_msg_str = "Persistent Recoverable Error";
          break;
        case IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_PARITY_ALERT:
          event_msg_str = "Persistent Parity Alert";
          break;
        case IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_PARITY_STATUS:
          event_msg_str = "Persistent Parity Status";
          break;
        case IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_SMI_LINK_LANE_FAIL_OVER_EVENT:
          event_msg_str = "SMI Link Lane Fail Over (LFO) Event";
          break;
        }

      if (event_msg_str)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    event_msg_str);

          return (1);
        }
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_UNCORRECTABLE_ERROR
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_CRC_ERROR_UNCORRECTABLE)
    {
      char *event_msg_str = NULL;

      if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_UNCORRECTABLE_MEMORY_ERROR_UNCORRECTABLE_CRC_ERROR)
        event_msg_str = "Uncorrectable CRC Error";
      else if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_UNCORRECTABLE_MEMORY_ERROR_UNCORRECTABLE_ALERT_FRAME)
        event_msg_str = "Uncorrectable Alert Frame";

      if (event_msg_str)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    event_msg_str);

          return (1);
        }
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_CORRECTABLE_ERROR
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PATROL_SCRUB_ERROR)
    {
      char *event_msg_str = NULL;

      if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_CORRECTABLE_RROR)
        event_msg_str = "Correctable Error";
      else if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_UNCORRECTABLE_ERROR)
        event_msg_str = "Uncorrectable Error";

      if (event_msg_str)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    event_msg_str);

          return (1);
        }
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_CORRECTABLE_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_CORRECTABLE_SENSOR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Correctable Sensor Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_NON_FATAL_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_NON_FATAL_SENSOR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Non-Fatal Sensor Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR_A
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Fatal Sensor A Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR_B
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR)
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
_sel_string_output_intel_quanta_qssc_s4r_bus (ipmi_sel_ctx_t ctx,
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
sel_string_output_intel_quanta_qssc_s4r_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R);

  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCI_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_quanta_qssc_s4r_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_ECC_ERROR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR)
    {
      uint8_t count_of_correctable_ecc_error;

      count_of_correctable_ecc_error = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_COUNT_OF_CORRECTABLE_ECC_ERROR_BITMASK);
      count_of_correctable_ecc_error >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_COUNT_OF_CORRECTABLE_ECC_ERROR_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Correctable ECC Error Count = %u",
                count_of_correctable_ecc_error);

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_STATE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SLOT_CONNECTOR_FAULT_STATUS_ASSERTED)
    {
      uint8_t event_special_code;
      char *event_special_code_str;
      uint8_t error_sub_code;
      char *error_sub_code_str = NULL;

      event_special_code = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_BITMASK);
      event_special_code >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_SHIFT;

      error_sub_code = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_BITMASK);
      error_sub_code >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_SHIFT;

      switch (event_special_code)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_INVALID_INFORMATION:
          event_special_code_str = "Invalid Information";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_BOARD_HOT_REPLACED_WITH_MISMATCHED_OR_FAULTY_MEMORY:
          event_special_code_str = "Memory Board hot-replaced with mismatched or faulty memory";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_HOT_PLUG_GENERIC_INITIALIZATION_ERROR:
          event_special_code_str = "Memory Hot-plug generic initialization error";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_HOT_PLUG_TIMEOUT:
          event_special_code_str = "Memory Hot-plug Timeout";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_USER_INITIATED_CANCELATION:
          event_special_code_str = "User-initiated cancellation";
          break;
        default:
          event_special_code_str = "Unknown";
        }

      if (event_special_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_HOT_PLUG_GENERIC_INITIALIZATION_ERROR)
        {
          switch (error_sub_code)
            {
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_MEMORY_BIST_ERROR:
              error_sub_code_str = "Memory BIST Error";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_SPD_ERROR:
              error_sub_code_str = "SPD Error";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_CLTT_CONFIGURATION_ERROR:
              error_sub_code_str = "CLTT Configuration Error";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_POPULATION_RULE_ERROR:
              error_sub_code_str = "Population Rule Error";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_MISMATCHED_DIMM_ERROR:
              error_sub_code_str = "Mismatched DIMM Error";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_OTHER_MEMORY_INITIALIZATION_ERRORS:
              error_sub_code_str = "Other Memory Initialization Errors";
              break;
            }
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Event Special Code = %s%s%s",
                event_special_code_str,
                error_sub_code_str ? ", Error Sub Code = " : "",
                error_sub_code_str);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_quanta_qssc_s4r_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R);

  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */

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
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR)))
    {
      _sel_string_output_intel_quanta_qssc_s4r_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_CORRECTABLE_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_CORRECTABLE_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_NON_FATAL_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_NON_FATAL_SENSOR)
          || ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR_A
               || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR_B)
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR)))
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
_sel_string_output_intel_quanta_qssc_s4r_device_function (ipmi_sel_ctx_t ctx,
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

static void
_sel_string_output_intel_quanta_qssc_s4r_memory_board (ipmi_sel_ctx_t ctx,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t memory_board;
  char *memory_board_str;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  memory_board = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_BITMASK);
  memory_board >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_SHIFT;

  switch (memory_board)
    {
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM1_SLOT:
      memory_board_str = "MEM1_SLOT";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM2_SLOT:
      memory_board_str = "MEM2_SLOT";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM3_SLOT:
      memory_board_str = "MEM3_SLOT";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM4_SLOT:
      memory_board_str = "MEM4_SLOT";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM5_SLOT:
      memory_board_str = "MEM5_SLOT";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM6_SLOT:
      memory_board_str = "MEM6_SLOT";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM7_SLOT:
      memory_board_str = "MEM7_SLOT";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM8_SLOT:
      memory_board_str = "MEM8_SLOT";
      break;
    default:
      memory_board_str = "Unknown";
    }

  snprintf (tmpbuf,
            tmpbuflen,
            "%s",
            memory_board_str);
}

static char *
_sel_string_output_intel_quanta_qssc_s4r_dimm_slot_str (uint8_t dimm_slot)
{
  switch (dimm_slot)
    {
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1B:
      return ("DIMM_1/B");
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1A:
      return ("DIMM_1/A");
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2B:
      return ("DIMM_2/B");
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2A:
      return ("DIMM_2/A");
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1D:
      return ("DIMM_1/D");
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1C:
      return ("DIMM_1/C");
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2D:
      return ("DIMM_2/D");
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2C:
      return ("DIMM_2/C");
    default:
      return ("Unknown");
    }

  return (NULL);                /* NOT REACHED */
}

static void
_sel_string_output_intel_quanta_qssc_s4r_dimm_slot (ipmi_sel_ctx_t ctx,
                                                    char *tmpbuf,
                                                    unsigned int tmpbuflen,
                                                    unsigned int flags,
                                                    struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t dimm_slot;
  char *dimm_slot_str;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  dimm_slot = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_BITMASK);
  dimm_slot >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_SHIFT;

  dimm_slot_str = _sel_string_output_intel_quanta_qssc_s4r_dimm_slot_str (dimm_slot);

  snprintf (tmpbuf,
            tmpbuflen,
            "%s",
            dimm_slot_str);
}

static void
_sel_string_output_intel_quanta_qssc_s4r_smi_link (ipmi_sel_ctx_t ctx,
                                                   char *tmpbuf,
                                                   unsigned int tmpbuflen,
                                                   unsigned int flags,
                                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t smi_link;
  char *smi_link_str;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  smi_link = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_BITMASK);
  smi_link >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_SHIFT;

  switch (smi_link)
    {
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_0:
      smi_link_str = "SMI_LINK0";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_1:
      smi_link_str = "SMI_LINK1";
      break;
    default:
      smi_link_str = "Unknown";
    }

  snprintf (tmpbuf,
            tmpbuflen,
            "%s",
            smi_link_str);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_quanta_qssc_s4r_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R);

  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCI_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_quanta_qssc_s4r_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_MIRRORING_MODE
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_LOST))
    {
      uint8_t domain_instance_type;
      uint8_t instance_id;
      char *domain_instance_str;
      char *instance_id_str;

      domain_instance_type = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_DOMAIN_INSTANCE_TYPE_BITMASK);
      domain_instance_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_DOMAIN_INSTANCE_TYPE_SHIFT;

      instance_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_BITMASK);
      instance_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_SHIFT;

      switch (domain_instance_type)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_DOMAIN_INSTANCE_TYPE_LOCAL_MEMORY_MIRRORING_INTRA_SOCKET:
          domain_instance_str = "Local memory mirroring";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_DOMAIN_INSTANCE_TYPE_GLOBAL_MEMORY_MIRRORING_INTER_SOCKET:
          domain_instance_str = "Global memory mirroring";
          break;
        default:
          domain_instance_str = "Unknown";
        }

      switch (instance_id)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_1_2:
          instance_id_str = "{MEM1_SLOT, MEM2_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_3_4:
          instance_id_str = "{MEM3_SLOT, MEM4_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_5_6:
          instance_id_str = "{MEM5_SLOT, MEM6_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_7_8:
          instance_id_str = "{MEM7_SLOT, MEM8_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_1_4:
          instance_id_str = "{MEM1_SLOT, MEM4_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_3_2:
          instance_id_str = "{MEM3_SLOT, MEM2_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_5_8:
          instance_id_str = "{MEM5_SLOT, MEM8_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_7_6:
          instance_id_str = "{MEM7_SLOT, MEM6_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_1_3:
          instance_id_str = "{MEM1_SLOT, MEM3_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_2_4:
          instance_id_str = "{MEM2_SLOT, MEM4_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_5_7:
          instance_id_str = "{MEM5_SLOT, MEM7_SLOT}";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_6_8:
          instance_id_str = "{MEM6_SLOT, MEM8_SLOT}";
          break;
        default:
          instance_id_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Domain Instance = %s, Instance = %s",
                domain_instance_str,
                instance_id_str);

      return (1);
    }

  if ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_ECC_ERROR
       && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
           || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
      || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_CORRECTABLE_ERROR
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PATROL_SCRUB_ERROR
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_CORRECTABLE_RROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_UNCORRECTABLE_ERROR)))
    {
      char memory_board_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
      char dimm_slot_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

      memset (memory_board_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
      memset (dimm_slot_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      _sel_string_output_intel_quanta_qssc_s4r_memory_board (ctx,
                                                             memory_board_buf,
                                                             INTEL_EVENT_BUFFER_LENGTH,
                                                             flags,
                                                             system_event_record_data);

      _sel_string_output_intel_quanta_qssc_s4r_dimm_slot (ctx,
                                                          dimm_slot_buf,
                                                          INTEL_EVENT_BUFFER_LENGTH,
                                                          flags,
                                                          system_event_record_data);

      snprintf (tmpbuf,
                tmpbuflen,
                "Memory Board = %s, DIMM Slot = %s",
                memory_board_buf,
                dimm_slot_buf);

      return (1);
    }

  if ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_CORRECTABLE_ERROR
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_CRC_ERROR_PERSISTENT
       && (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_RECOVERABLE_ERROR
           || system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_PARITY_ALERT
           || system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_PARITY_STATUS
           || system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_SMI_LINK_LANE_FAIL_OVER_EVENT))
      || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_UNCORRECTABLE_ERROR
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_CRC_ERROR_UNCORRECTABLE
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_UNCORRECTABLE_MEMORY_ERROR_UNCORRECTABLE_CRC_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_UNCORRECTABLE_MEMORY_ERROR_UNCORRECTABLE_ALERT_FRAME)))
    {
      char memory_board_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
      char smi_link_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

      memset (memory_board_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
      memset (smi_link_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      _sel_string_output_intel_quanta_qssc_s4r_memory_board (ctx,
                                                             memory_board_buf,
                                                             INTEL_EVENT_BUFFER_LENGTH,
                                                             flags,
                                                             system_event_record_data);

      _sel_string_output_intel_quanta_qssc_s4r_smi_link (ctx,
                                                         smi_link_buf,
                                                         INTEL_EVENT_BUFFER_LENGTH,
                                                         flags,
                                                         system_event_record_data);

      snprintf (tmpbuf,
                tmpbuflen,
                "Memory Board = %s, SMI Link = %s",
                memory_board_buf,
                smi_link_buf);

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_STATE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SLOT_CONNECTOR_FAULT_STATUS_ASSERTED)
    {
      char memory_board_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

      memset (memory_board_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      _sel_string_output_intel_quanta_qssc_s4r_memory_board (ctx,
                                                             memory_board_buf,
                                                             INTEL_EVENT_BUFFER_LENGTH,
                                                             flags,
                                                             system_event_record_data);

      snprintf (tmpbuf,
                tmpbuflen,
                "Memory Board = %s",
                memory_board_buf);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_quanta_qssc_s4r_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R);

  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */

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
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR)))
    {
      _sel_string_output_intel_quanta_qssc_s4r_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

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
sel_string_output_intel_quanta_qssc_s4r_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R);

  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_SPARING_MODE
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_LOST)
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t domain_instance_type;
      uint8_t sparing_type;
      char *domain_instance_str;
      char *sparing_type_str;
      char sparing_type_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
      uint8_t index_of_spared_memory_board;
      char index_of_spared_memory_board_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
      uint8_t spared_dimm_information;
      char *spared_dimm_information_str;
      char spared_dimm_information_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

      memset (sparing_type_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
      memset (index_of_spared_memory_board_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
      memset (spared_dimm_information_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      domain_instance_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_BITMASK);
      domain_instance_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_SHIFT;

      sparing_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_TYPE_BITMASK);
      sparing_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_TYPE_SHIFT;

      index_of_spared_memory_board = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_INDEX_OF_SPARED_MEMORY_BOARD_BITMASK);
      index_of_spared_memory_board >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_INDEX_OF_SPARED_MEMORY_BOARD_SHIFT;

      spared_dimm_information = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_BITMASK);
      spared_dimm_information >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_SHIFT;

      if (domain_instance_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_LOCAL_MEMORY_SPARING)
        {
          domain_instance_str = "Local memory sparing";

          switch (sparing_type)
            {
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_LOCAL_MEMORY_SPARING_SPARING_TYPE_DIMM_SPARING:
              sparing_type_str = "DIMM Sparing";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_LOCAL_MEMORY_SPARING_SPARING_TYPE_RANK_SPARING:
              sparing_type_str = "Rank Sparing";
              break;
            default:
              sparing_type_str = "Unknown";
            }

          snprintf (sparing_type_buf,
                    INTEL_EVENT_BUFFER_LENGTH,
                    ", Sparing Type = %s",
                    sparing_type_str);

          switch (spared_dimm_information)
            {
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_1B_LOCKSTEP_DIMM_1D:
              spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_1A_LOCKSTEP_DIMM_1C:
              spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_2B_LOCKSTEP_DIMM_2D:
              spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_2A_LOCKSTEP_DIMM_2C:
              spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
              break;
            default:
              spared_dimm_information_str = "Unknown";
            }

          snprintf (spared_dimm_information_buf,
                    INTEL_EVENT_BUFFER_LENGTH,
                    ", Spared DIMM Information = %s",
                    spared_dimm_information_str);
        }
      else if (domain_instance_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_GLOBAL_MEMORY_SPARING)
        domain_instance_str = "Global memory sparing";
      else
        domain_instance_str = "Unknown";

      snprintf (index_of_spared_memory_board_buf,
                INTEL_EVENT_BUFFER_LENGTH,
                ", Spared Memory Board = %u",
                index_of_spared_memory_board);

      if (sel_string_snprintf (buf,
                               buflen,
                               wlen,
                               "Domain Instance = %s%s%s%s",
                               domain_instance_str,
                               sparing_type_buf,
                               index_of_spared_memory_board_buf,
                               spared_dimm_information_buf))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_MISMATCH_CONFIGURATION_ERROR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CONFIGURATION_ERROR
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      /* achu: I have no idea what SMI Link# Valid is for, there is no SMI link data in this SEL event
       * Is it a typo.  Do they mean memory board?  It's mostly here for documentation.
       */
      uint8_t smi_link_valid;
      uint8_t dimm_slot_valid;
      uint8_t error_type;
      char *error_type_str;
      char memory_board_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
      char dimm_slot_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

      memset (memory_board_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
      memset (dimm_slot_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      smi_link_valid = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_VALID_BITMASK);
      smi_link_valid >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_VALID_SHIFT;

      dimm_slot_valid = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_VALID_BITMASK);
      dimm_slot_valid >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_VALID_SHIFT;

      error_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_BITMASK);
      error_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_SHIFT;

      switch (error_type)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_MIRROR:
          error_type_str = "Mirror";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_SPARE:
          error_type_str = "Spare";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_INTERLEAVE:
          error_type_str = "Interleave";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_HEMISPHERE:
          error_type_str = "Hemisphere";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_POPULATION:
          error_type_str = "Population";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_DEVICE_MISMATCH:
          error_type_str = "Device Mismatch";
          break;
        default:
          error_type_str = "Unknown";
        }

      _sel_string_output_intel_quanta_qssc_s4r_memory_board (ctx,
                                                             memory_board_buf,
                                                             INTEL_EVENT_BUFFER_LENGTH,
                                                             flags,
                                                             system_event_record_data);

      /* Technically Intel docs do not say 0 vs. 1 for true vs. false.  Gotta guess */
      if (dimm_slot_valid)
        _sel_string_output_intel_quanta_qssc_s4r_dimm_slot (ctx,
                                                            dimm_slot_buf,
                                                            INTEL_EVENT_BUFFER_LENGTH,
                                                            flags,
                                                            system_event_record_data);

      if (sel_string_snprintf (buf,
                               buflen,
                               wlen,
                               "Error Type = %s, Memory Board = %s%s%s",
                               error_type_str,
                               memory_board_buf,
                               dimm_slot_valid ? ", DIMM Slot = " : "",
                               dimm_slot_buf))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_QUANTA_QSSC_S4R_BIOS_POST
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_BIOS_POST_ERROR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_SYSTEM_FIRMWARE_ERROR
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint16_t error_code;
      char *error_code_str = NULL;
      char error_code_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
      uint16_t error_code_type;

      memset (error_code_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      error_code = system_event_record_data->event_data2;
      error_code |= (system_event_record_data->event_data3 << 8);

      error_code_type = (error_code & IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_BITMASK);
      error_code_type >>= IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_SHIFT;

      switch (error_code)
        {
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_CMOS_DATE_TIME_NOT_SET:
          error_code_str = "CMOS Date/Time not set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
          error_code_str = "Password check failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_KEYBOARD_LOCKED_ERROR:
          error_code_str = "Keyboard locked error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_KEYBOARD_STUCK_KEY_ERROR:
          error_code_str = "Keyboard stuck key error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_THE_SAS_RAID_FIRMWARE_CANNOT_RUN_PROPERLY:
          error_code_str = "The SAS RAID firmware can not run properly. The user should attempt to reflash the firmware";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PCI_PARITY_ERROR:
          error_code_str = "PCI Parity Error (PERR)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT_ERROR:
          error_code_str = "PCI resource conflict error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
          error_code_str = "PCI out of resources error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_CACHE_SIZE_MISMATCH_DETECTED:
          error_code_str = "Processor cache size mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_STEPPING_MISMATCH:
          error_code_str = "Processor stepping mismatch";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_FAMILY_MISMATCH_DETECTED:
          error_code_str = "Processor family mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_INTEL_QPI_SPEED_MISMATCH:
          error_code_str = "Processor Intel(R) QPI speed mismatch";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_AND_CHIPSET_STEPPING_CONFIGURATION_IS_UNSUPPORTED:
          error_code_str = "Processor and chipset stepping configuration is unsupported";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_CMOS_NVRAM_CONFIGURATION_CLEARED:
          error_code_str = "CMOS/NVRAM configuration cleared";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
          error_code_str = "Passwords cleared by jumper";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
          error_code_str = "Password clear jumper is set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_DISABLED:
          error_code_str = "Processor Disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_FRB_3_TIMEOUT:
          error_code_str = "Processor FRB-3 timeout";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 01 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 02 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_03_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 03 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_04_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 04 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_BUILD_IN_SELF_TEST_FAILURE:
          error_code_str = "Processor Build-In Self Test (BIST) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
          error_code_str = "Watchdog timer failed on last boot";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
          error_code_str = "OS boot watchdog timer failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
          error_code_str = "Baseboard Management Controller failed self-test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
          error_code_str = "Baseboard Management Controller failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
          error_code_str = "Baseboard Management Controller in update mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_SENSOR_DATA_RECORD_EMPTY:
          error_code_str = "Baseboard Management Controller Sensor Data Record empty";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_SYSTEM_EVENT_LOG_FULL:
          error_code_str = "Baseboard Management Controller System event log full";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_CHIPSET_RECLAIM_OF_NON_CRITICAL_VARIABLES_COMPLETE:
          error_code_str = "Chipset Reclaim of non critical variables complete";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED:
          error_code_str = "TPM device not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING:
          error_code_str = "TPM device missing or not responding";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_FAILURE:
          error_code_str = "TPM device failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST:
          error_code_str = "TPM device failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_MEMORY_WAS_NOT_CONFIGURED_FOR_THE_SELECTED_MEMORY_RAS_CONFIGURATION:
          error_code_str = "Memory was not configured for the selected Memory RAS configuration";
          break;
        default:
          if (error_code_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY)
            {
              uint16_t memory_error_code;
              uint16_t cpu_socket;
              uint16_t dimm_slot;
              char *memory_error_code_str;
              char *cpu_socket_str;
              char *dimm_slot_str;

              memory_error_code = (error_code & IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_BITMASK);
              memory_error_code >>= IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_SHIFT;

              cpu_socket = (error_code & IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_BITMASK);
              cpu_socket >>= IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_SHIFT;

              dimm_slot = (error_code & IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_DIMM_SLOT_BITMASK);
              dimm_slot >>= IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_DIMM_SLOT_SHIFT;

              switch (memory_error_code)
                {
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_INVALID_TYPE_ERROR:
                  memory_error_code_str = "Memory invalid type error";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_DISABLED:
                  memory_error_code_str = "Memory disabled";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_MISMATCH_ERROR:
                  memory_error_code_str = "Memory mismatch error";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_TRAINING_ERROR:
                  memory_error_code_str = "Memory Training Failed";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_TOO_MANY_DIMM_TYPES:
                  memory_error_code_str = "Too many DIMM Types";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_BIST_FAILED:
                  memory_error_code_str = "Memory BIST Failed";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_SPD_FAILED:
                  memory_error_code_str = "SPD Failed";
                  break;
                default:
                  memory_error_code_str = "Unknown Memory Failure";
                }

              switch (cpu_socket)
                {
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_1:
                  cpu_socket_str = "CPU_1";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_2:
                  cpu_socket_str = "CPU_2";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_3:
                  cpu_socket_str = "CPU_3";
                  break;
                case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_4:
                  cpu_socket_str = "CPU_4";
                  break;
                default:
                  cpu_socket_str = "Unknown CPU Socket";
                }

              dimm_slot_str = _sel_string_output_intel_quanta_qssc_s4r_dimm_slot_str (dimm_slot);

              snprintf (error_code_buf,
                        INTEL_EVENT_BUFFER_LENGTH,
                        "%s, CPU Socket = %s, DIMM Slot = %s",
                        memory_error_code_str,
                        cpu_socket_str,
                        dimm_slot_str);

              error_code_str = error_code_buf;
            }
          else
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

struct sel_string_oem sel_string_oem_intel_quanta_qssc_s4r =
  {
    sel_string_output_intel_quanta_qssc_s4r_sensor_name,
    NULL,
    sel_string_output_intel_quanta_qssc_s4r_event_data1_class_oem,
    NULL,
    sel_string_output_intel_quanta_qssc_s4r_event_data2_discrete_oem,
    sel_string_output_intel_quanta_qssc_s4r_event_data2_class_oem,
    NULL,
    sel_string_output_intel_quanta_qssc_s4r_event_data3_discrete_oem,
    sel_string_output_intel_quanta_qssc_s4r_event_data3_class_oem,
    sel_string_output_intel_quanta_qssc_s4r_event_data2_event_data3,
    NULL,
    NULL,
  };
