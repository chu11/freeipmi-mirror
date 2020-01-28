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
#include "freeipmi/spec/ipmi-slave-address-spec.h"
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
sel_string_output_intel_xeon_sensor_name (ipmi_sel_ctx_t ctx,
                                          struct ipmi_sel_entry *sel_entry,
                                          uint8_t sel_record_type,
                                          char *buf,
                                          unsigned int buflen,
                                          unsigned int flags,
                                          unsigned int *wlen,
                                          struct ipmi_sel_system_event_record_data *system_event_record_data,
                                          int *oem_rv)
{
  int nmret;

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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((nmret = sel_string_output_intel_node_manager_sensor_name (ctx,
                                                                 sel_entry,
                                                                 sel_record_type,
                                                                 buf,
                                                                 buflen,
                                                                 flags,
                                                                 wlen,
                                                                 system_event_record_data,
                                                                 oem_rv)) < 0)
    return (-1);

  if (nmret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_xeon_event_data1_class_oem (ipmi_sel_ctx_t ctx,
                                                    struct ipmi_sel_entry *sel_entry,
                                                    uint8_t sel_record_type,
                                                    char *tmpbuf,
                                                    unsigned int tmpbuflen,
                                                    unsigned int flags,
                                                    unsigned int *wlen,
                                                    struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int nmret;

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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((nmret = sel_string_output_intel_node_manager_event_data1_class_oem (ctx,
                                                                           sel_entry,
                                                                           sel_record_type,
                                                                           tmpbuf,
                                                                           tmpbuflen,
                                                                           flags,
                                                                           wlen,
                                                                           system_event_record_data)) < 0)
    return (-1);

  if (nmret)
    return (1);

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_INTEL_QUICK_PATH_INTERFACE_LINK_WIDTH_REDUCED
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_LINK_WIDTH_REDUCED)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_FATAL_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICKPATH_INTERFACE_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_BIOS_RECOVERY
          && (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_BIOS_RECOVERY_START
              || system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_BIOS_RECOVERY_COMPLETE))
      || (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_FIRMWARE_UPDATE_STATUS
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_FIRMWARE_UPDATE_STATUS_SENSOR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR)
      || (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_IERR_RECOVERY_DUMP_INFO
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO))
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_CORRECTABLE_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_CORRECTABLE_ERROR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Correctable Error Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  return (0);
}

static void
_sel_string_output_intel_xeon_pci_bus (ipmi_sel_ctx_t ctx,
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
            "PCI Bus Number %u",
            system_event_record_data->event_data2);
}


static const char *
_sel_string_output_intel_xeon_ras_mode (uint8_t event_data)
{
  uint8_t ras_mode;
  char *ras_mode_str;

  ras_mode = (event_data & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_RAS_MODE_BITMASK);
  ras_mode >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_RAS_MODE_SHIFT;

  switch (ras_mode)
    {
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_RAS_MODE_NONE:
      ras_mode_str = "None (Independent Channel Mode)";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_RAS_MODE_MIRRORING:
      ras_mode_str = "Mirroring Mode";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_RAS_MODE_LOCKSTEP:
      ras_mode_str = "Lockstep Mode";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_RAS_MODE_RANK_SPARING:
      ras_mode_str = "Rank Sparing Mode";
      break;
    default:
      ras_mode_str = "Unknown";
    }

  return (ras_mode_str);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_xeon_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_POWER_SUPPLY1_STATUS
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_POWER_SUPPLY2_STATUS)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC)
    {
      if (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED)
        {
          char *power_supply_status_str;

          switch (system_event_record_data->event_data2)
            {
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_OUTPUT_VOLTAGE_FAULT:
              power_supply_status_str = "Output voltage fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_OUTPUT_POWER_FAULT:
              power_supply_status_str = "Output power fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_OUTPUT_OVER_CURRENT_FAULT:
              power_supply_status_str = "Output over-current fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_OVER_TEMPERATURE_FAULT:
              power_supply_status_str = "Over-temperature fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_FAN_FAULT:
              power_supply_status_str = "Fan fault";
              break;
            default:
              power_supply_status_str = "Unknown";
            }

          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    power_supply_status_str);

          return (1);
        }

      if (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE)
        {
          char *power_supply_status_str;

          switch (system_event_record_data->event_data2)
            {
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_OUTPUT_VOLTAGE_WARNING:
              power_supply_status_str = "Output voltage warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_OUTPUT_POWER_WARNING:
              power_supply_status_str = "Output power warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_OUTPUT_OVER_CURRENT_WARNING:
              power_supply_status_str = "Output over-current warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_OVER_TEMPERATURE_WARNING:
              power_supply_status_str = "Over-temperature warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_FAN_WARNING:
              power_supply_status_str = "Fan warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_INPUT_UNDER_VOLTAGE_WARNING:
              power_supply_status_str = "Input under-voltage warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_INPUT_OVER_CURRENT_WARNING:
              power_supply_status_str = "Input over-current warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_INPUT_OVER_POWER_WARNING:
              power_supply_status_str = "Input over-power warning";
              break;
            default:
              power_supply_status_str = "Unknown";
            }

          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    power_supply_status_str);

          return (1);
        }

      if (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR)
        {
          char *power_supply_status_str;

          switch (system_event_record_data->event_data2)
            {
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_BMC_CANNOT_ACCESS_PMBUS:
              power_supply_status_str = "The BMC cannot access the PMBus device on the PSU but its FRU device is responding";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_PMBUS_REVISION_NOT_SUPPORTED:
              power_supply_status_str = "The PMBUS_REVISION command returns a version number that is not supported";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_PMBUS_REVISION_ERROR:
              power_supply_status_str = "The PMBus device does not successfully respond to the PMBUS_REVISION command";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_PSU_INCOMPATIBLE:
              power_supply_status_str = "ThE PSU is incompatible with one or more PSUs that are present in the system";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_PSU_FW_DEGRADED:
              power_supply_status_str = "The PSU FW is operating in a degraded mode (likely due to a failed firmware update)";
              break;
            default:
              power_supply_status_str = "Unknown";
            }

          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    power_supply_status_str);

          return (1);
        }
    }

  /* Document "System Event Log Troubleshooting Guide for PCSD Platforms Based on Intel Xeon Processor E5 2600 V3 Product Families"
   * Likewise Document "System Event Log (SEL) Troubleshooting Guide" for S2600BPB
   *
   * says 90h = SSB Thermal Trip
   *
   * But SSB Thermal Trip is 0x0D while 0x90 is VRD Over Temperature
   *
   * Given context, I believe it is VRD Over Temperature
   */
  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_TEMPERATURE
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_VRD_OVER_TEMPERATURE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_LIMIT
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_LIMIT_EXCEEDED)
    {
      char cpu_bitmask_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      uint8_t cpu_bitmask;
      unsigned int lentmp = 0;

      memset (cpu_bitmask_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      cpu_bitmask = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_PROCESSOR_VRD_HOT_BITMAP_BITMASK);
      cpu_bitmask >>= IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_PROCESSOR_VRD_HOT_BITMAP_SHIFT;

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_PROCESSOR_VRD_HOT_BITMAP_CPU1)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU1"))
            return (0);
        }

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_PROCESSOR_VRD_HOT_BITMAP_CPU2)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU2"))
            return (0);
        }

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_PROCESSOR_VRD_HOT_BITMAP_CPU3)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU3"))
            return (0);
        }

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_PROCESSOR_VRD_HOT_BITMAP_CPU4)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU4"))
            return (0);
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU = %s",
                cpu_bitmask_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR1_STATUS
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR2_STATUS
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR3_STATUS
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR4_STATUS)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_THERMAL_TRIP)
    {
      char *str;

      switch (system_event_record_data->event_data2)
        {
        case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_THERMAL_TRIP_OEM_INTEL_CPU_NON_RECOVERABLE_OVER_TEMP_CONDITION:
          str = "CPU non-recoverable over-temp condition";
          break;
        case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_THERMAL_TRIP_OEM_INTEL_CPU_BOOT_FIVR_FAULT:
          str = "CPU boot FIVR fault";
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

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_INTERNAL_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED)
    {
      char *str;

      switch (system_event_record_data->event_data2)
        {
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_UNKNOWN:
          str = "Unknown";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_CATERR:
          str = "CATERR";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_CPU_CORE_ERROR:
          str = "CPU Core Error";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_MSID_MISMATCH:
          str = "MSID Mismatch";
          break;
        default:
          str = "Unknown OEM code"; /* to differentiate from above */
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "%s",
                str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR_ERR2_TIMEOUT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED)
    {
      uint8_t cpu_bitmask;
      char *str;

      cpu_bitmask = (system_event_record_data->event_data2 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_BITMASK);
      cpu_bitmask >>= IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_SHIFT;

      switch (cpu_bitmask)
        {
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_CPU1:
          str = "CPU1";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_CPU2:
          str = "CPU2";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_CPU3:
          str = "CPU3";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_CPU4:
          str = "CPU4";
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_CONFIGURATION_STATUS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      uint8_t config_error;
      char *config_error_str;

      config_error = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_CONFIG_ERROR_BITMASK);
      config_error >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_CONFIG_ERROR_SHIFT;

      switch (config_error)
        {
        case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_CONFIG_ERROR_NONE:
          config_error_str = "None";
          break;
        case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_CONFIG_ERROR_INVALID_DIMM_CONFIGURATION_FOR_RAS_MODE:
          config_error_str = "Invalid DIMM Configuration for RAS Mode";
          break;
        default:
          config_error_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Config Error = %s",
                config_error_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_MODE_SELECT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_xeon_ras_mode (system_event_record_data->event_data2);

      snprintf (tmpbuf,
                tmpbuflen,
                "Prior RAS Mode = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_LEGACY_PCI_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_xeon_pci_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_SPARING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      uint8_t sparing_domain, rank_on_dimm;
      char *sparing_domain_str;

      sparing_domain = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_SPARING_DOMAIN_BITMASK);
      sparing_domain >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_SPARING_DOMAIN_SHIFT;

      rank_on_dimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_RANK_ON_DIMM_BITMASK);
      rank_on_dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_RANK_ON_DIMM_SHIFT;

      switch (sparing_domain)
        {
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_SPARING_DOMAIN_A:
          sparing_domain_str = "A";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_SPARING_DOMAIN_B:
          sparing_domain_str = "B";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_SPARING_DOMAIN_C:
          sparing_domain_str = "C";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_SPARING_DOMAIN_D:
          sparing_domain_str = "D";
          break;
        default:
          sparing_domain_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Sparing Domain = %s, Rank on DIMM = %u",
                sparing_domain_str,
                rank_on_dimm);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_xeon_event_data2_class_oem (ipmi_sel_ctx_t ctx,
                                                    struct ipmi_sel_entry *sel_entry,
                                                    uint8_t sel_record_type,
                                                    char *tmpbuf,
                                                    unsigned int tmpbuflen,
                                                    unsigned int flags,
                                                    unsigned int *wlen,
                                                    struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int nmret;

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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((nmret = sel_string_output_intel_node_manager_event_data2_class_oem (ctx,
                                                                           sel_entry,
                                                                           sel_record_type,
                                                                           tmpbuf,
                                                                           tmpbuflen,
                                                                           flags,
                                                                           wlen,
                                                                           system_event_record_data)) < 0)
    return (-1);

  if (nmret)
    return (1);

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_INTEL_QUICK_PATH_INTERFACE_LINK_WIDTH_REDUCED
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_LINK_WIDTH_REDUCED)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_CORRECTABLE_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_FATAL_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICKPATH_INTERFACE_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR_2))
    {
      uint8_t cpu;
      char *cpu_str;

      cpu = system_event_record_data->event_data2;

      switch (cpu)
        {
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_CPU_1:
          cpu_str = "1";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_CPU_2:
          cpu_str = "2";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_CPU_3:
          cpu_str = "3";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_CPU_4:
          cpu_str = "4";
          break;
        default:
          cpu_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU = %s",
                cpu_str);

      return (1);
    }

  /* achu: In document technically unclear if this */
  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_FIRMWARE_UPDATE_STATUS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_FIRMWARE_UPDATE_STATUS_SENSOR)
    {
      uint8_t target_of_update;
      uint8_t target_instance;
      char *target_of_update_str;

      target_of_update = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_BITMASK);
      target_of_update >>= IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_SHIFT;

      target_instance = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_INSTANCE_BITMASK);
      target_instance >>= IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_INSTANCE_SHIFT;

      switch (target_of_update)
        {
        case IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_BMC:
          target_of_update_str = "BMC";
          break;
        case IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_BIOS:
          target_of_update_str = "BIOS";
          break;
        case IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_ME:
          target_of_update_str = "ME";
          break;
        default:
          target_of_update_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Target of update = %s, Instance = %u",
                target_of_update_str, target_instance);

      return (1);
    }

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR))
    {
      _sel_string_output_intel_xeon_pci_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_IERR_RECOVERY_DUMP_INFO
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_SPECIFIC_IERR_RECOVERY_DUMP_INFO_DUMP_FAILED)
    {
      char failed_register_type_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      uint8_t failed_register_type;
      unsigned int lentmp = 0;

      memset (failed_register_type_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      failed_register_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_BITMASK);
      failed_register_type >>= IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_SHIFT;

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_UNCORE_MSR_REGISTER)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "Uncore MSR register"))
            return (0);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_CORE_MSR_REGISTERS)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "Core MSR registers"))
            return (0);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_IIO_REGISTER)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "IIO register"))
            return (0);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_PCI_CONFIG_SPACE)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "PCI config space"))
            return (0);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_MCA_ERROR_SOURCE_REGISTER)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "MCA error source register"))
            return (0);
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Failed register = %s",
                failed_register_type_str);

      return (1);
    }

  return (0);
}

static void
_sel_string_output_intel_xeon_pci_device_function (ipmi_sel_ctx_t ctx,
                                                   char *tmpbuf,
                                                   unsigned int tmpbuflen,
                                                   unsigned int flags,
                                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t pci_device, pci_function;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  pci_device = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_BITMASK);
  pci_device >>= IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_SHIFT;

  pci_function = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_BITMASK);
  pci_function >>= IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_SHIFT;

  snprintf (tmpbuf,
            tmpbuflen,
            "PCI_Device number %u, PCI Function number %u",
            pci_device,
            pci_function);
}

void
sel_string_output_intel_xeon_memory_dimm (ipmi_sel_ctx_t ctx,
                                          char *tmpbuf,
                                          unsigned int tmpbuflen,
                                          unsigned int flags,
                                          struct ipmi_sel_system_event_record_data *system_event_record_data,
                                          int channel_valid,
                                          int dimm_valid)
{
  uint8_t socket_id, channel, dimm;
  char *socket_id_str, *channel_str, *dimm_str;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_SOCKET_ID_BITMASK);
  socket_id >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_SOCKET_ID_SHIFT;

  channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_BITMASK);
  channel >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_SHIFT;

  dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_DIMM_BITMASK);
  dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_DIMM_SHIFT;

  switch (socket_id)
    {
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_SOCKET_ID_CPU1:
      socket_id_str = "1";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_A:
              channel_str = "A";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_B:
              channel_str = "B";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_C:
              channel_str = "C";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_D:
              channel_str = "D";
              break;
            default:
              channel_str = "Unknown";
            }
        }
      else
        channel_str = "Indeterminate";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_SOCKET_ID_CPU2:
      socket_id_str = "2";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_E:
              channel_str = "E";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_F:
              channel_str = "F";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_G:
              channel_str = "G";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_H:
              channel_str = "H";
              break;
            default:
              channel_str = "Unknown";
            }
        }
      else
        channel_str = "Indeterminate";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_SOCKET_ID_CPU3:
      socket_id_str = "3";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_J:
              channel_str = "J";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_K:
              channel_str = "K";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_L:
              channel_str = "L";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_M:
              channel_str = "M";
              break;
            default:
              channel_str = "Unknown";
            }
        }
      else
        channel_str = "Indeterminate";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_SOCKET_ID_CPU4:
      socket_id_str = "4";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_N:
              channel_str = "N";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_P:
              channel_str = "P";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_R:
              channel_str = "R";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_CHANNEL_T:
              channel_str = "T";
              break;
            default:
              channel_str = "Unknown";
            }
        }
      else
        channel_str = "Indeterminate";
      break;
    default:
      socket_id_str = "Unknown";
      channel_str = "Unknown";
    }

  if (dimm_valid)
    {
      switch (dimm)
        {
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_DIMM_1:
          dimm_str = "1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_DIMM_2:
          dimm_str = "2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA3_DIMM_3:
          dimm_str = "3";
          break;
        default:
          dimm_str = "Unknown";
        }
    }
  else
    dimm_str = "Indeterminate";

  snprintf (tmpbuf,
            tmpbuflen,
            "Socket CPU %s, Channel %s, DIMM %s",
            socket_id_str,
            channel_str,
            dimm_str);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_xeon_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
                                                       struct ipmi_sel_entry *sel_entry,
                                                       uint8_t sel_record_type,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int nmret;

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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((nmret = sel_string_output_intel_node_manager_event_data3_discrete_oem (ctx,
                                                                              sel_entry,
                                                                              sel_record_type,
                                                                              tmpbuf,
                                                                              tmpbuflen,
                                                                              flags,
                                                                              wlen,
                                                                              system_event_record_data)) < 0)
    return (-1);

  if (nmret)
    return (1);

  /* Document "System Event Log Troubleshooting Guide for PCSD Platforms Based on Intel Xeon Processor E5 2600 V3 Product Families"
   * Likewise Document "System Event Log (SEL) Troubleshooting Guide" for S2600BPB
   *
   * says 90h = SSB Thermal Trip
   *
   * But SSB Thermal Trip is 0x0D while 0x90 is VRD Over Temperature
   *
   * Given context, I believe it is VRD Over Temperature
   */
  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_TEMPERATURE
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_VRD_OVER_TEMPERATURE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_LIMIT
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_LIMIT_EXCEEDED)
    {
      char memory_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      uint8_t memory_bitmask;
      unsigned int lentmp = 0;

      memset (memory_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      memory_bitmask = system_event_record_data->event_data3;

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU1_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU1 - DIMM Channel 1/2"))
            return (0);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU1_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU1 - DIMM Channel 3/4"))
            return (0);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU2_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU2 - DIMM Channel 1/2"))
            return (0);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU2_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU2 - DIMM Channel 3/4"))
            return (0);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU3_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU3 - DIMM Channel 1/2"))
            return (0);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU3_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU3 - DIMM Channel 3/4"))
            return (0);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU4_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU4 - DIMM Channel 1/2"))
            return (0);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_MEMORY_VRD_HOT_BITMAP_CPU4_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU4 - DIMM Channel 3/4"))
            return (0);
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Memory = %s",
                memory_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR1_THERMAL_TRIP
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR2_THERMAL_TRIP
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR3_THERMAL_TRIP
          || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PROCESSOR4_THERMAL_TRIP)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CRITICAL_OVERTEMPERATURE)
    {
      uint8_t socket_id;
      uint8_t channel;
      uint8_t dimm;
      char *cpu_str;
      char channel_char;
      char *dimm_str;

      socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_BITMASK);
      socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_SHIFT;

      channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_BITMASK);
      channel >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_SHIFT;

      dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_BITMASK);
      dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SHIFT;

      switch (socket_id)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU1:
          cpu_str = "CPU1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU2:
          cpu_str = "CPU2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU3:
          cpu_str = "CPU3";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU4:
          cpu_str = "CPU4";
          break;
        default:
          cpu_str = "Unknown";
          break;
        }

      /* channel 0-3 maps to A-D for CPU1, E-H for CPU2, J-M for CPU3, N, P, R, T for CPU4 */
      switch (socket_id)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU1:
          channel_char = 'A' + channel;
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU2:
          channel_char = 'E' + channel;
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU3:
          channel_char = 'J' + channel;
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_SOCKET_ID_CPU4:
          /* For some reason it skips chars around here */
          channel_char = 'N' + channel * 2;
          break;
        default:
          channel_char = '?';
          break;
        }

      switch (dimm)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_1:
          dimm_str = "Dimm 1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_2:
          dimm_str = "Dimm 2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_3:
          dimm_str = "Dimm 3";
          break;
        default:
          dimm_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU = %s, Channel = %c, Dimm = %s",
                cpu_str,
                channel_char,
                dimm_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_INTERNAL_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED
      && system_event_record_data->event_data2 == IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_CATERR)
    {
      char cpu_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      unsigned int lentmp = 0;

      memset (cpu_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA3_OEM_INTEL_CPU1)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU1"))
            return (0);
        }

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA3_OEM_INTEL_CPU2)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU2"))
            return (0);
        }

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA3_OEM_INTEL_CPU3)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU3"))
            return (0);
        }

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA3_OEM_INTEL_CPU4)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "CPU4"))
            return (0);
        }

      if (!strlen (cpu_str))
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &lentmp, "Unknown CPU"))
            return (0);
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "%s caused CATERR",
                cpu_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_AUTO_CONFIG_STATUS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED)
    {
      uint8_t auto_config_error_bitmask;
      char *str;

      auto_config_error_bitmask = (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_BITMASK);
      auto_config_error_bitmask >>= IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_SHIFT;

      switch (auto_config_error_bitmask)
        {
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_CFG_SYNTAX_ERROR:
          str = "CFG syntax error";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_CHASSIS_AUTO_DETECT_ERROR:
          str = "Chassis auto-detect error";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_SDR_CFG_FILE_MISMATCH:
          str = "SDR/CFG file mismatch";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_SDR_OR_CFG_FILE_CORRUPTED:
          str = "SDR or CFG file corrupted";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_SDR_SYNTAX_ERROR:
          str = "SDR syntax error";
          break;
        default:
          str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Auto Config Error = %s",
                str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_CONFIGURATION_STATUS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_xeon_ras_mode (system_event_record_data->event_data3);

      snprintf (tmpbuf,
                tmpbuflen,
                "RAS Mode Configured = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_MODE_SELECT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_xeon_ras_mode (system_event_record_data->event_data3);

      snprintf (tmpbuf,
                tmpbuflen,
                "Selected RAS Mode = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_LEGACY_PCI_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_xeon_pci_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_SPARING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      sel_string_output_intel_xeon_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_xeon_event_data3_class_oem (ipmi_sel_ctx_t ctx,
                                                    struct ipmi_sel_entry *sel_entry,
                                                    uint8_t sel_record_type,
                                                    char *tmpbuf,
                                                    unsigned int tmpbuflen,
                                                    unsigned int flags,
                                                    unsigned int *wlen,
                                                    struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int nmret;

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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600BPB);

  if ((nmret = sel_string_output_intel_node_manager_event_data3_class_oem (ctx,
                                                                           sel_entry,
                                                                           sel_record_type,
                                                                           tmpbuf,
                                                                           tmpbuflen,
                                                                           flags,
                                                                           wlen,
                                                                           system_event_record_data)) < 0)
    return (-1);

  if (nmret)
    return (1);

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR))
    {
      _sel_string_output_intel_xeon_pci_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  return (0);
}
