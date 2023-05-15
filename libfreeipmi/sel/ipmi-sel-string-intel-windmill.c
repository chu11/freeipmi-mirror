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
#include "freeipmi/record-format/oem/ipmi-sel-oem-intel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/oem/ipmi-event-reading-type-code-oem-intel-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-intel-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-numbers-oem-intel-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-types-oem-intel-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-intel-node-manager.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define INTEL_EVENT_BUFFER_LENGTH 4096

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_windmill_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
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
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL);

  /*
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_CPU_SEL_STATUS)
    {
      uint8_t sel_clear;
      uint8_t sel_rollover;

      sel_clear = (system_event_record_data->event_data1 & IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_SEL_CLEAR_BITMASK);
      sel_clear >>= IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_SEL_CLEAR_SHIFT;

      sel_rollover = (system_event_record_data->event_data1 & IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_SEL_ROLLOVER_BITMASK);
      sel_rollover >>= IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_SEL_ROLLOVER_SHIFT;

      if (sel_clear)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "SEL Clear");

          return (1);
        }

      if (sel_rollover)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "SEL Rollover");

          return (1);
        }
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS)
    {
      char *chassis_power_status_str;

      switch (system_event_record_data->event_data1)
        {
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_POWER_DOWN:
          chassis_power_status_str = "Power Down";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_POWER_CYCLE_RESET:
          chassis_power_status_str = "Power Cycle/Reset";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_POWER_ON:
          chassis_power_status_str = "Power On";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_AC_LOST:
          chassis_power_status_str = "AC Lost";
          break;
        default:
          chassis_power_status_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Power Status = %s",
                chassis_power_status_str);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW)
    {
      char *hsc_str;

      switch (system_event_record_data->event_data1)
        {
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_NONE_OF_THE_ABOVE:
          hsc_str = "Active status bits are waiting to be read by one or more status commands.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_CML_ERROR:
          hsc_str = "An error was detected on the I2C/PMBus interface.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_VIN_UV_FAULT:
          hsc_str = "An undervoltage input fault was detected on the UV pin.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_IOUT_OC_FAULT:
          hsc_str = "The hot swap controller detected an overcurrent condition.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_HOTSWAP_OFF:
          hsc_str = "The hot swap gate driver output is disabled.";
          break;
        default:
          hsc_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Status = %s",
                hsc_str);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH)
    {
      char *hsc_str;

      switch (system_event_record_data->event_data1)
        {
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_POWER_GOOD:
          hsc_str = "The voltage on the FLB pin is below the required threshold.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_MFR_STATUS:
          hsc_str = "There are one or more active status bits to be read by STATUS_MFR_SPECIFIC.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_INPUT_STATUS:
          hsc_str = "There are one or more active status bits to be read by STATUS_INPUT.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_IOUT_STATUS:
          hsc_str = "There are one or more active status bits to be read by STATUS_IOUT.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_VOUT_STATUS:
          hsc_str = "There are one or more active status bits to be read by STATUS_VOUT.";
          break;
        default:
          hsc_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Status = %s",
                hsc_str);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC)
    {
      char *hsc_str;

      switch (system_event_record_data->event_data1)
        {
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_IOUT_WARN2:
          hsc_str = "An undercurrent or overcurrent condition on the output supply detected.";
          break;
          /* achu: HS_SHUTDOWN_CAUSE1 & HS_SHUTDOWN_CAUSE2 list 4 error messages
           * with <00>, <01>, <10>, & <11> listed next to them.  I have no idea
           * where these other bits come from.
           *
           * So all user gets is a generic "hotswap shutdown"
           */
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_SHUTDOWN_CAUSE1:
          hsc_str = "Hotswap shutdown";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_SHUTDOWN_CAUSE2:
          hsc_str = "Hotswap shutdown";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_INLIM:
          hsc_str = "The ADM1276 has actively limited current into the load.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_OV_CMP_OUT:
          hsc_str = "Input Voltage to OV pin is above threshold.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_UV_CMP_OUT:
          hsc_str = "Input voltage to UV pin is below threshold.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_FET_HEALTH_BAD:
          hsc_str = "FET behavior suggests that the FET may be shorted.";
          break;
        default:
          hsc_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Status = %s",
                hsc_str);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT)
    {
      char *hsc_str;

      switch (system_event_record_data->event_data1)
        {
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_PIN_OP_WARN:
          hsc_str = "An overpower condition on the input supply was detected by power monitor.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_UV_FAULT:
          hsc_str = "An undervoltage was detected on the UV pin.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_UV_WARN:
          hsc_str = "An undervoltage condition on the input supply was detected by the power monitor.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_OV_WARN:
          hsc_str = "An overvoltage condition on the input supply was detected by the power monitor.";
          break;
        case IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_OV_FAULT:
          hsc_str = "An overvoltage was detected on the OV pin.";
          break;
        default:
          hsc_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Status = %s",
                hsc_str);

      return (1);
    }

  return (0);
}

static const char *
_sel_string_output_intel_windmill_native_vs_external_throttling (uint8_t event_data)
{
  uint8_t noe;
  char *noe_str;

  noe = (event_data & IPMI_OEM_INTEL_WINDMILL_EVENT_DATA2_THROTTLING_BITMASK);
  noe >>= IPMI_OEM_INTEL_WINDMILL_EVENT_DATA2_THROTTLING_SHIFT;

  switch (noe)
    {
    case IPMI_OEM_INTEL_WINDMILL_EVENT_DATA2_NATIVE_THROTTLING:
      noe_str = "Native";
      break;
    case IPMI_OEM_INTEL_WINDMILL_EVENT_DATA2_EXTERNAL_THROTTLING:
      noe_str = "External";
      break;
    default:
      noe_str = "Unknown";
      break;
    }

  return (noe_str);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_windmill_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL);

  /*
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_PROC_HOT_EXTENDED_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_PROCESSOR_AUTOMATICALLY_THROTTLED)
    {
      const char *noe_str;

      noe_str = _sel_string_output_intel_windmill_native_vs_external_throttling (system_event_record_data->event_data2);

      snprintf (tmpbuf,
                tmpbuflen,
                "Throttling = %s",
                noe_str);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MEM_HOT_EXTENDED_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_MEMORY_AUTOMATICALLY_THROTTLED)
    {
      const char *noe_str;

      noe_str = _sel_string_output_intel_windmill_native_vs_external_throttling (system_event_record_data->event_data2);

      snprintf (tmpbuf,
                tmpbuflen,
                "Throttling = %s",
                noe_str);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SENSOR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_MACHINE_CHECK_EXCEPTION
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_CORRECTABLE_MACHINE_CHECK_ERROR))
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Error Code Number = %u",
                system_event_record_data->event_data2);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_PCIE_ERROR_SENSOR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
    {
      uint8_t device;
      uint8_t function;

      device = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_WINDMILL_DEVICE_NUMBER_BITMASK);
      device >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_WINDMILL_DEVICE_NUMBER_SHIFT;

      function = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_WINDMILL_FUNCTION_NUMBER_BITMASK);
      function >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_WINDMILL_FUNCTION_NUMBER_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Device %u, Function %u",
                device,
                function);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MEMORY_ECC_ERROR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      uint8_t logical_rank;

      logical_rank = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_WINDMILL_LOGICAL_RANK_BITMASK);
      logical_rank >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_WINDMILL_LOGICAL_RANK_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Logical Rank = %u",
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
sel_string_output_intel_windmill_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL);

  /*
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_FIRMWARE_STATUS)
    {
      uint8_t health_event;
      char *health_event_str;

      health_event = system_event_record_data->event_data2;

      switch (health_event)
        {
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_RECOVERY_GPIO_FORCED:
          health_event_str = "Recovery GPIO forced";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_IMAGE_EXECUTION_FAILED:
          health_event_str = "Image execution failed";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FLASH_ERASE_ERROR:
          health_event_str = "Flash erase error";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FLASH_STATE_INFORMATION:
          health_event_str = "Flash state information";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_INTERNAL_ERROR:
          health_event_str = "Internal error";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_BMC_COLD_RESET_ERROR:
          health_event_str = "BMC did not respond to cold reset";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_DIRECT_FLASH_UPDATE:
          health_event_str = "Direct flash update requested by the BIOS";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_MANUFACTURING_ERROR:
          health_event_str = "Manufacturing error";
          break;
        case IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_PERSISTENT_STORAGE_INTEGRITY_ERROR:
          health_event_str = "Persistent storage integrity error";
          break;
        default:
          health_event_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Health Event = %s",
                health_event_str);

      return (1);
    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_OTHER_IIO_ERROR_SENSOR
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_OTHER_IIO_ERROR_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_FRONT_PANEL_NMI_DIAGNOSTIC_INTERRUPT)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Error ID = 0x%02X",
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
sel_string_output_intel_windmill_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL);

  /*
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_PROC_HOT_EXTENDED_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_PROCESSOR_AUTOMATICALLY_THROTTLED)
    {
      uint8_t cpu_vr;

      cpu_vr = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_CPU_VR_BITMASK);
      cpu_vr >>= IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_CPU_VR_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU/VR = %u",
                cpu_vr);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MEM_HOT_EXTENDED_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_MEMORY_AUTOMATICALLY_THROTTLED)
    {
      uint8_t cpu_vr;
      uint8_t channel_number;
      uint8_t dimm;

      cpu_vr = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_CPU_VR_BITMASK);
      cpu_vr >>= IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_CPU_VR_SHIFT;

      channel_number = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_CHANNEL_NUMBER_BITMASK);
      channel_number >>= IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_CHANNEL_NUMBER_SHIFT;

      dimm = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_DIMM_BITMASK);
      dimm >>= IPMI_OEM_INTEL_WINDMILL_EVENT_DATA3_THROTTLING_DIMM_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU/VR = %u, Channel Number = %u, Dimm = %u",
                cpu_vr,
                channel_number,
                dimm);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SENSOR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_MACHINE_CHECK_EXCEPTION
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_CORRECTABLE_MACHINE_CHECK_ERROR))
    {
      uint8_t cpu_number;
      uint8_t source;
      uint8_t source_extra;

      cpu_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_CPU_NUMBER_BITMASK);
      cpu_number >>= IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_CPU_NUMBER_SHIFT;

      source = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_BITMASK);
      source >>= IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_SHIFT;

      source_extra = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_EXTRA_BITMASK);
      source_extra >>= IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_EXTRA_SHIFT;

      if (source == IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_QPI)
        {
          char *qpi_str;

          switch (source_extra)
            {
            case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_EXTRA_QPI0:
              qpi_str = "QPI0";
              break;
            case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_EXTRA_QPI1:
              qpi_str = "QPI1";
              break;
            default:
              qpi_str = "Unknown QPI";
              break;
            }

          snprintf (tmpbuf,
                    tmpbuflen,
                    "CPU = %u, Source = %s",
                    cpu_number,
                    qpi_str);
        }
      else if (source == IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SOURCE_LLC)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "CPU = %u, Core = %u",
                    cpu_number,
                    source_extra);
        }
      else
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "CPU = %u, Source = %s",
                    cpu_number,
                    "Unknown");
        }

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_PCIE_ERROR_SENSOR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Bus Number = %u",
                system_event_record_data->event_data3);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MEMORY_ECC_ERROR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      uint8_t cpu_number;
      uint8_t channel_number;
      uint8_t dimm;

      cpu_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_WINDMILL_CPU_NUMBER_BITMASK);
      cpu_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_WINDMILL_CPU_NUMBER_SHIFT;

      channel_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_WINDMILL_CHANNEL_NUMBER_BITMASK);
      channel_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_WINDMILL_CHANNEL_NUMBER_SHIFT;

      dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_WINDMILL_DIMM_BITMASK);
      dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_WINDMILL_DIMM_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU Number = %u, Channel Number = %u, Dimm = %u",
                cpu_number,
                channel_number,
                dimm);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_windmill_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL);

  /*
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_WINDMILL_ME_FIRMWARE_HEALTH_EVENT_FIRMWARE_STATUS)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Extended Error Info = %02X",
                system_event_record_data->event_data3);

    }

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_OTHER_IIO_ERROR_SENSOR
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_OTHER_IIO_ERROR_SENSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_FRONT_PANEL_NMI_DIAGNOSTIC_INTERRUPT)
    {
      uint8_t cpu_number;
      uint8_t source;
      char *source_str;

      cpu_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_CPU_NUMBER_BITMASK);
      cpu_number >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_CPU_NUMBER_SHIFT;

      source = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_SOURCE_BITMASK);
      source >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_SOURCE_SHIFT;

      switch (source)
        {
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_SOURCE_IRP0:
          source_str = "IRP0";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_SOURCE_IRP1:
          source_str = "IRP1";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_SOURCE_IIO_CORE:
          source_str = "IIO-Core";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_WINDMILL_SOURCE_VT_D:
          source_str = "VT-d";
          break;
        default:
          source_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU Number = %u, Source = %s",
                cpu_number, source_str);

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
sel_string_output_intel_windmill_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL);

  /*
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_POST_ERROR_SENSOR
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
          /* These are from WiWynn doc */
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_CPU_MISMATCH:
          error_code_str = "PEI CPU Mismatch";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_CPU_SELF_TEST_FAILED:
          error_code_str = "PEI CPU Self Test Failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_CPU_CACHE_ERROR:
          error_code_str = "PEI CPU Cache Error ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_CPU_MICROCODE_UPDATE_FAILED:
          error_code_str = "PEI CPU Microcode Update Failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_CPU_NO_MICROCODE:
          error_code_str = "PEI CPU No Microcode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_CPU_INTERNAL_ERROR:
          error_code_str = "PEI CPU Internal Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_RESET_NOT_AVAILABLE1:
          error_code_str = "PEI Reset Not Available";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_RESET_NOT_AVAILABLE2:
          error_code_str = "PEI Reset Not Available";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_RECOVERY_NO_CAPSULE:
          error_code_str = "PEI Recovery No Capsule";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_SB_PWR_FLR:
          error_code_str = "PEI SB PWR FLR";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_PEI_SB_SYSPWR_FLR:
          error_code_str = "PEI SB SYSPWR FLR";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_CLEAR_CMOS:
          error_code_str = "DXE Clear CMOS ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_NB_ERROR:
          error_code_str = "DXE NB Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_ARCH_PROTOCOL_NOT_AVAILABLE:
          error_code_str = "DXE Arch Protocol Not Available";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_PCI_BUS_OUT_OF_RESOURCES:
          error_code_str = "DXE PCI Bus Out of Resources";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_LEGACY_OPROM_NO_SPACE:
          error_code_str = "DXE Legacy OPROM No Space";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_NO_CON_OUT:
          error_code_str = "DXE No Con Out";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_NO_CON_IN:
          error_code_str = "DXE No Con In";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_FLASH_UPDATE_FAILED:
          error_code_str = "DXE Flash Update Failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_DXE_RESET_NOT_AVAILABLE:
          error_code_str = "DXE Reset Not Available";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_ME_RECOVERED_VIA_GR:
          error_code_str = "ME Recovered via GR";
          break;
          /* These are from a Quanta doc */
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_CMOS_CLEAR:
          error_code_str = "CMOS Clear";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_THERMAL_TRIP:
          error_code_str = "Thermal Trip";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_SYS_PWROK_DROPS_UNEXPECTEDLY:
          error_code_str = "SYS_PWROK Drops Unexpectedly";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_AC_LOST:
          error_code_str = "AC Lost";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_RECOVER_ME_FROM_ABNORMAL_MODE:
          error_code_str = "Recover ME from Abnormal Mode Successfully";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_BACKUP_IMAGE_LOADED_DIRECT_FW_UPDATE_NEEDED:
          error_code_str = "Backup Image Loaded and a direct FW updated is needed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_WINDMILL_POST_ERROR_CODE_ROCVER_HECI_FROM_ABNORMAL_MODE:
          error_code_str = "Recover HECI from Abnormal Mode Successfully";
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

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_intel_windmill_oem_record_data (ipmi_sel_ctx_t ctx,
                                                  struct ipmi_sel_entry *sel_entry,
                                                  uint8_t sel_record_type,
                                                  char *buf,
                                                  unsigned int buflen,
                                                  unsigned int flags,
                                                  unsigned int *wlen,
                                                  int *oem_rv)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
          || ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (oem_rv);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL);

  /* Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */

  if (ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      /* Bytes 11-12 - Device ID
       * Bytes 13-14 - Device Identification Number
       * Bytes 15-16 - Error Code
       *
       * I'm assuming little endian, hopefully I'm right.
       */
      uint16_t device_id;
      uint16_t device_identification_number;
      char *device_identification_number_str;
      uint16_t error_code;
      char *error_code_str;

      device_id = sel_entry->sel_event_record[IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_INDEX_LSB_INDEX];
      device_id |= (sel_entry->sel_event_record[IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_INDEX_MSB_INDEX] << 8);

      device_identification_number = sel_entry->sel_event_record[IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_INDEX_LSB_INDEX];
      device_identification_number |= (sel_entry->sel_event_record[IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_INDEX_MSB_INDEX] << 8);

      error_code = sel_entry->sel_event_record[IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_INDEX_LSB_INDEX];
      error_code |= (sel_entry->sel_event_record[IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_INDEX_MSB_INDEX] << 8);

      switch (device_identification_number)
        {
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_DEVICE_0_IN_DMI_MODE:
          device_identification_number_str = "Device 0 in DMI Mode ";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_DMI_PORT_IN_PCIE_MODE:
          device_identification_number_str = "DMI Port in PCIe Mode ";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_1A:
          device_identification_number_str = "Port 1A";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_1B:
          device_identification_number_str = "Port 1B";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_2A:
          device_identification_number_str = "Port 2A";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_2B:
          device_identification_number_str = "Port 2B";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_2C:
          device_identification_number_str = "Port 2C";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_2D:
          device_identification_number_str = "Port 2D";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_3A_IN_PCIE_MODE:
          device_identification_number_str = "Port 3A in PCIe Mode";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_3B:
          device_identification_number_str = "Port 3B";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_3C:
          device_identification_number_str = "Port 3C";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_PORT_3D:
          device_identification_number_str = "Port 3D";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_DEVICE_IDENTIFICATION_NUMBER_IIO_NTB_SECONDARY_ENDPOINT:
          device_identification_number_str = "IIO NTB Secondary Endpoint";
          break;
        default:
          device_identification_number_str = "Unknown";
          break;
        }

      switch (error_code)
        {
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVER_ERROR:
          error_code_str = "Receiver Error";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_BAD_TLP:
          error_code_str = "Bad TLP";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_BAD_DLLP:
          error_code_str = "Bad DLPP";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_REPLAY_TIMEOUT:
          error_code_str = "Replay Time-out";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_REPLAY_NUMBER_ROLLOVER:
          error_code_str = "Replay Number Rollover";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_ADVISORY_NON_FATAL_ERROR:
          error_code_str = "Advisory Non-fatal Error";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_ERR_COR_MESSAGE_FROM_DOWNSTREAM_DEVICE:
          error_code_str = "Received ERR_COR message from downstream device";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_PCI_EXPRESS_LINK_BANDWIDTH_CHANGED:
          error_code_str = "PCI Express Link Bandwidth changed";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_UNSUPPORTED_REQUEST_COMPLETION_STATUS_FROM_DOWNSTREAM_DEVICE:
          error_code_str = "Received \"Unsupported Request\" completion status from downstream device.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_SENT_A_PCI_EXPRESS_UNSUPPORTED_REQUEST_RESPOND_ON_INBOUND_REQUEST:
          error_code_str = "Sent a PCI Express \"Unsupported Request\" respond on inbound request for address decode, request type, or other reason.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_COMPLETER_ABORT_COMPLETION_STATUS_FROM_DOWNSTREAM_DEVICE:
          error_code_str = "Received \"Completer Abort\" completion status from downstream device.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_SENT_A_PCI_EXPRESS_COMPLETER_ABORT_CONDITION_ON_INBOUND_REQUEST:
          error_code_str = "Sent a PCI Express \"Completer Abort\" condition on inbound request for address decode, request type, or other reason.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_COMPLETION_TIMEOUT_ON_NP_TRANSACTION_OUTSTANDING_ON_PCI_EXPRESS_DMI:
          error_code_str = "Completion timeout on NP transaction outstanding on PCI Express/DMI.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_PCI_EXPRESS_POISONED_TLP:
          error_code_str = "Received PCI Express Poisoned TLP.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_PCI_EXPRESS_UNEXPECTED_COMPLETION:
          error_code_str = "Received PCI Express unexpected Completion.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_PCI_EXPRESS_FLOW_CONTROL_PROTOCOL_ERROR:
          error_code_str = "PCI Express Flow Control Protocol Error.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_ERR_NONFATAL_MESSAGE_FROM_DOWNSTREAM_DEVICE:
          error_code_str = "Received ERR_NONFATAL Message from downstream device.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_A_REQUEST_FROM_A_DOWNSTREAM_COMPONENT_THAT_IS_UNSUPPORTED:
          error_code_str = "Received a Request from a downstream component that is unsupported.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_A_REQUEST_FROM_A_DOWNSTREAM_COMPONENT_THAT_IS_TO_BE_COMPLETER_ABORTED:
          error_code_str = "Received a Request from a downstream component that is to be completer aborted.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_ACS_VIOLATION:
          error_code_str = "ACS Violation";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_PCI_EXPRESS_MALFORMED_TLP:
          error_code_str = "PCI Express Malformed TLP";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_PCI_EXPRESS_DATA_LINK_PROTOCOL_ERROR:
          error_code_str = "PCI Express Data Link Protocol Error";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_PCI_EXPRESS_RECEIVER_OVERFLOW:
          error_code_str = "PCI Express Receiver Overflow";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_SURPRISE_DOWN:
          error_code_str = "Surprise Down";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_RECEIVED_ERR_FATAL_MESSAGE_FROM_DOWNSTREAM_DEVICE:
          error_code_str = "Received ERR_FATAL message from downstream device.";
          break;
        case IPMI_SEL_OEM_INTEL_WINDMILL_ERROR_CODE_OUTBOUND_SWITCH_HEADER_QUEUE_PARITY_ERROR:
          error_code_str = "Outbound switch header queue partiy error";
          break;
        default:
          error_code_str = "Unknown";
          break;
        }

      if (sel_string_snprintf (buf,
                               buflen,
                               wlen,
                               "Devie ID = %u, Device Identification Number = %s, Error = %s",
                               device_id,
                               device_identification_number_str,
                               error_code_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;

      return (1);
    }

  return (0);
}

struct sel_string_oem sel_string_oem_intel_windmill =
  {
    NULL,
    sel_string_output_intel_windmill_event_data1_class_sensor_specific_discrete,
    NULL,
    NULL,
    sel_string_output_intel_windmill_event_data2_discrete_oem,
    sel_string_output_intel_windmill_event_data2_class_oem,
    NULL,
    sel_string_output_intel_windmill_event_data3_discrete_oem,
    sel_string_output_intel_windmill_event_data3_class_oem,
    sel_string_output_intel_windmill_event_data2_event_data3,
    sel_string_output_intel_windmill_oem_record_data,
    NULL,
  };
