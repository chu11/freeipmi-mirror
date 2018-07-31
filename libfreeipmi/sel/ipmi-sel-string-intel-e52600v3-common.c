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
#include "ipmi-sel-string-intel-e52600v3-common.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define INTEL_EVENT_BUFFER_LENGTH 4096

int
sel_string_output_intel_e52600v3_sensor_name (ipmi_sel_ctx_t ctx,
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
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

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
sel_string_output_intel_e52600v3_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

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

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_POST
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_INTEL_QUICK_PATH_INTERFACE_LINK_WIDTH_REDUCED
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_LINK_WIDTH_REDUCED)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_FATAL_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICKPATH_INTERFACE_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_BIOS_RECOVERY
          && (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_START
              || system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_FINISH))
      || (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_FIRMWARE_UPDATE_STATUS
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_FIRMWARE_UPDATE_STATUS_SENSOR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_CORRECTABLE_ERRORS)
      || (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_IERR_RECOVERY_DUMP_INFO
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO))
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_CORRECTABLE_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_CORRECTABLE_ERROR)
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
_sel_string_output_intel_e52600v3_pci_bus (ipmi_sel_ctx_t ctx,
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
_sel_string_output_intel_e52600v3_ras_mode (uint8_t event_data)
{
  uint8_t ras_mode;
  char *ras_mode_str;

  ras_mode = (event_data & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_E52600V3_RAS_MODE_BITMASK);
  ras_mode >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_E52600V3_RAS_MODE_SHIFT;

  switch (ras_mode)
    {
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_E52600V3_RAS_MODE_NONE:
      ras_mode_str = "None (Independent Channel Mode)";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_E52600V3_RAS_MODE_MIRRORING:
      ras_mode_str = "Mirroring Mode";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_E52600V3_RAS_MODE_LOCKSTEP:
      ras_mode_str = "Lockstep Mode";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_E52600V3_RAS_MODE_RANK_SPARING:
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
sel_string_output_intel_e52600v3_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

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
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_E52600V3_OUTPUT_VOLTAGE_FAULT:
              power_supply_status_str = "Output voltage fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_E52600V3_OUTPUT_POWER_FAULT:
              power_supply_status_str = "Output power fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_E52600V3_OUTPUT_OVER_CURRENT_FAULT:
              power_supply_status_str = "Output over-current fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_E52600V3_OVER_TEMPERATURE_FAULT:
              power_supply_status_str = "Over-temperature fault";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED_EVENT_DATA2_OEM_INTEL_E52600V3_FAN_FAULT:
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
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_OUTPUT_VOLTAGE_WARNING:
              power_supply_status_str = "Output voltage warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_OUTPUT_POWER_WARNING:
              power_supply_status_str = "Output power warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_OUTPUT_OVER_CURRENT_WARNING:
              power_supply_status_str = "Output over-current warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_OVER_TEMPERATURE_WARNING:
              power_supply_status_str = "Over-temperature warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_FAN_WARNING:
              power_supply_status_str = "Fan warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_INPUT_UNDER_VOLTAGE_WARNING:
              power_supply_status_str = "Input under-voltage warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_INPUT_OVER_CURRENT_WARNING:
              power_supply_status_str = "Input over-current warning";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_PREDICTIVE_FAILURE_EVENT_DATA2_OEM_INTEL_E52600V3_INPUT_OVER_POWER_WARNING:
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
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_E52600V3_BMC_CANNOT_ACCESS_PMBUS:
              power_supply_status_str = "The BMC cannot access the PMBus device on the PSU but its FRU device is responding";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_E52600V3_PMBUS_REVISION_NOT_SUPPORTED:
              power_supply_status_str = "The PMBUS_REVISION command returns a version number that is not supported";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_E52600V3_PMBUS_REVISION_ERROR:
              power_supply_status_str = "The PMBus device does not successfully respond to the PMBUS_REVISION command";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_E52600V3_PSU_INCOMPATIBLE:
              power_supply_status_str = "ThE PSU is incompatible with one or more PSUs that are present in the system";
              break;
            case IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR_EVENT_DATA2_OEM_INTEL_E52600V3_PSU_FW_DEGRADED:
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
      unsigned int wlen = 0;

      memset (cpu_bitmask_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      cpu_bitmask = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_E52600V3_PROCESSOR_VRD_HOT_BITMAP_BITMASK);
      cpu_bitmask >>= IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_E52600V3_PROCESSOR_VRD_HOT_BITMAP_SHIFT;

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_E52600V3_PROCESSOR_VRD_HOT_BITMAP_CPU1)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU1"))
            return (1);
        }

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_E52600V3_PROCESSOR_VRD_HOT_BITMAP_CPU2)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU2"))
            return (1);
        }

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_E52600V3_PROCESSOR_VRD_HOT_BITMAP_CPU3)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU3"))
            return (1);
        }

      if (cpu_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA2_OEM_INTEL_E52600V3_PROCESSOR_VRD_HOT_BITMAP_CPU4)
        {
          if (sel_string_strcat_comma_separate (cpu_bitmask_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU4"))
            return (1);
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
        case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_THERMAL_TRIP_OEM_INTEL_E52600V3_CPU_NON_RECOVERABLE_OVER_TEMP_CONDITION:
          str = "CPU non-recoverable over-temp condition";
          break;
        case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_THERMAL_TRIP_OEM_INTEL_E52600V3_CPU_BOOT_FIVR_FAULT:
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
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_UNKNOWN:
          str = "Unknown";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CATERR:
          str = "CATERR";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU_CORE_ERROR:
          str = "CPU Core Error";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_MSID_MISMATCH:
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

      cpu_bitmask = (system_event_record_data->event_data2 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_BITMASK);
      cpu_bitmask >>= IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_SHIFT;

      switch (cpu_bitmask)
        {
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU1:
          str = "CPU1";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU2:
          str = "CPU2";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU3:
          str = "CPU3";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU4:
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_CONFIGURATION_STATUS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      uint8_t config_error;
      char *config_error_str;

      config_error = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_E52600V3_CONFIG_ERROR_BITMASK);
      config_error >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_E52600V3_CONFIG_ERROR_SHIFT;

      switch (config_error)
        {
        case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_E52600V3_CONFIG_ERROR_NONE:
          config_error_str = "None";
          break;
        case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_E52600V3_CONFIG_ERROR_INVALID_DIMM_CONFIGURATION_FOR_RAS_MODE:
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_MODE_SELECT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_e52600v3_ras_mode (system_event_record_data->event_data2);

      snprintf (tmpbuf,
                tmpbuflen,
                "Prior RAS Mode = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_LEGACY_PCI_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_e52600v3_pci_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MIRRORING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      uint8_t mirroring_domain, rank_on_dimm;
      char *mirroring_domain_str;

      mirroring_domain = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_MIRRORING_DOMAIN_BITMASK);
      mirroring_domain >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_MIRRORING_DOMAIN_SHIFT;

      rank_on_dimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_RANK_ON_DIMM_BITMASK);
      rank_on_dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_RANK_ON_DIMM_SHIFT;

      switch (mirroring_domain)
        {
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_MIRRORING_DOMAIN_0:
          mirroring_domain_str = "0";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_MIRRORING_DOMAIN_1:
          mirroring_domain_str = "1";
          break;
        default:
          mirroring_domain_str = "Unknown";
          break;
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Mirroring Domain = %s, Rank on DIMM = %u",
                mirroring_domain_str,
                rank_on_dimm);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_SPARING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      uint8_t sparing_domain, rank_on_dimm;
      char *sparing_domain_str;

      sparing_domain = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_SPARING_DOMAIN_BITMASK);
      sparing_domain >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_SPARING_DOMAIN_SHIFT;

      rank_on_dimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_RANK_ON_DIMM_BITMASK);
      rank_on_dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_RANK_ON_DIMM_SHIFT;

      switch (sparing_domain)
        {
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_SPARING_DOMAIN_A:
          sparing_domain_str = "A";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_SPARING_DOMAIN_B:
          sparing_domain_str = "B";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_SPARING_DOMAIN_C:
          sparing_domain_str = "C";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_SPARING_DOMAIN_D:
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      uint8_t rank_on_dimm;

      rank_on_dimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_RANK_ON_DIMM_BITMASK);
      rank_on_dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_RANK_ON_DIMM_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Rank on DIMM = %u",
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
sel_string_output_intel_e52600v3_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

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

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_POST
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_INTEL_QUICK_PATH_INTERFACE_LINK_WIDTH_REDUCED
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_LINK_WIDTH_REDUCED)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_CORRECTABLE_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICK_PATH_INTERFACE_FATAL_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_INTEL_QUICKPATH_INTERFACE_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR_2))
    {
      uint8_t cpu;
      char *cpu_str;

      cpu = system_event_record_data->event_data2;

      switch (cpu)
        {
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_E52600V3_CPU_1:
          cpu_str = "1";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_E52600V3_CPU_2:
          cpu_str = "2";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_E52600V3_CPU_3:
          cpu_str = "3";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_E52600V3_CPU_4:
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
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_FIRMWARE_UPDATE_STATUS_SENSOR)
    {
      uint8_t target_of_update;
      uint8_t target_instance;
      char *target_of_update_str;

      target_of_update = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_E52600V3_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_BITMASK);
      target_of_update >>= IPMI_OEM_INTEL_E52600V3_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_SHIFT;

      target_instance = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_E52600V3_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_INSTANCE_BITMASK);
      target_instance >>= IPMI_OEM_INTEL_E52600V3_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_INSTANCE_SHIFT;

      switch (target_of_update)
        {
        case IPMI_OEM_INTEL_E52600V3_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_BMC:
          target_of_update_str = "BMC";
          break;
        case IPMI_OEM_INTEL_E52600V3_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_BIOS:
          target_of_update_str = "BIOS";
          break;
        case IPMI_OEM_INTEL_E52600V3_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_EVENT_DATA2_TARGET_OF_UPDATE_ME:
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

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_CORRECTABLE_ERRORS))
    {
      _sel_string_output_intel_e52600v3_pci_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_BMC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_IERR_RECOVERY_DUMP_INFO
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_E52600V3_SPECIFIC_IERR_RECOVERY_DUMP_INFO_DUMP_FAILED)
    {
      char failed_register_type_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      uint8_t failed_register_type;
      unsigned int wlen = 0;

      memset (failed_register_type_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      failed_register_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_BITMASK);
      failed_register_type >>= IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_SHIFT;

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_UNCORE_MSR_REGISTER)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "Uncore MSR register"))
            return (1);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_CORE_MSR_REGISTERS)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "Core MSR registers"))
            return (1);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_IIO_REGISTER)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "IIO register"))
            return (1);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_PCI_CONFIG_SPACE)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "PCI config space"))
            return (1);
        }

      if (failed_register_type & IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO_EVENT_DATA2_FAILED_REGISTER_TYPE_MCA_ERROR_SOURCE_REGISTER)
        {
          if (sel_string_strcat_comma_separate (failed_register_type_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "MCA error source register"))
            return (1);
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
_sel_string_output_intel_e52600v3_pci_device_function (ipmi_sel_ctx_t ctx,
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

  pci_device = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_E52600V3_PCI_DEVICE_NUMBER_BITMASK);
  pci_device >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_E52600V3_PCI_DEVICE_NUMBER_SHIFT;

  pci_function = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_E52600V3_PCI_FUNCTION_NUMBER_BITMASK);
  pci_function >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA3_OEM_INTEL_E52600V3_PCI_FUNCTION_NUMBER_SHIFT;

  snprintf (tmpbuf,
            tmpbuflen,
            "PCI_Device number %u, PCI Function number %u",
            pci_device,
            pci_function);
}

static void
_sel_string_output_intel_e52600v3_memory_dimm (ipmi_sel_ctx_t ctx,
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

  socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_SOCKET_ID_BITMASK);
  socket_id >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_SOCKET_ID_SHIFT;

  channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_BITMASK);
  channel >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_SHIFT;

  dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_DIMM_BITMASK);
  dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_DIMM_SHIFT;

  switch (socket_id)
    {
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_SOCKET_ID_CPU1:
      socket_id_str = "1";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_A:
              channel_str = "A";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_B:
              channel_str = "B";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_C:
              channel_str = "C";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_D:
              channel_str = "D";
              break;
            default:
              channel_str = "Unknown";
            }
        }
      else
        channel_str = "Indeterminate";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_SOCKET_ID_CPU2:
      socket_id_str = "2";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_E:
              channel_str = "E";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_F:
              channel_str = "F";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_G:
              channel_str = "G";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_H:
              channel_str = "H";
              break;
            default:
              channel_str = "Unknown";
            }
        }
      else
        channel_str = "Indeterminate";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_SOCKET_ID_CPU3:
      socket_id_str = "3";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_J:
              channel_str = "J";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_K:
              channel_str = "K";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_L:
              channel_str = "L";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_M:
              channel_str = "M";
              break;
            default:
              channel_str = "Unknown";
            }
        }
      else
        channel_str = "Indeterminate";
      break;
    case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_SOCKET_ID_CPU4:
      socket_id_str = "4";
      if (channel_valid)
        {
          switch (channel)
            {
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_N:
              channel_str = "N";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_P:
              channel_str = "P";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_R:
              channel_str = "R";
              break;
            case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_CHANNEL_T:
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
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_DIMM_1:
          dimm_str = "1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_DIMM_2:
          dimm_str = "2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA3_DIMM_3:
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
sel_string_output_intel_e52600v3_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

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
      unsigned int wlen = 0;

      memset (memory_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      memory_bitmask = system_event_record_data->event_data3;

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU1_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU1 - DIMM Channel 1/2"))
            return (1);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU1_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU1 - DIMM Channel 3/4"))
            return (1);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU2_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU2 - DIMM Channel 1/2"))
            return (1);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU2_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU2 - DIMM Channel 3/4"))
            return (1);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU3_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU3 - DIMM Channel 1/2"))
            return (1);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU3_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU3 - DIMM Channel 3/4"))
            return (1);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU4_DIMM_CHANNEL_1_2)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU4 - DIMM Channel 1/2"))
            return (1);
        }

      if (memory_bitmask & IPMI_SENSOR_TYPE_TEMPERATURE_EVENT_DATA3_OEM_INTEL_E52600V3_MEMORY_VRD_HOT_BITMAP_CPU4_DIMM_CHANNEL_3_4)
        {
          if (sel_string_strcat_comma_separate (memory_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU4 - DIMM Channel 3/4"))
            return (1);
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

      socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_BITMASK);
      socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_SHIFT;

      channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_CHANNEL_BITMASK);
      channel >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_CHANNEL_SHIFT;

      dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_DIMM_BITMASK);
      dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_DIMM_SHIFT;

      switch (socket_id)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU1:
          cpu_str = "CPU1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU2:
          cpu_str = "CPU2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU3:
          cpu_str = "CPU3";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU4:
          cpu_str = "CPU4";
          break;
        default:
          cpu_str = "Unknown";
          break;
        }

      /* channel 0-3 maps to A-D for CPU1, E-H for CPU2, J-M for CPU3, N, P, R, T for CPU4 */
      switch (socket_id)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU1:
          channel_char = 'A' + channel;
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU2:
          channel_char = 'E' + channel;
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU3:
          channel_char = 'J' + channel;
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_SOCKET_ID_CPU4:
          /* For some reason it skips chars around here */
          channel_char = 'N' + channel * 2;
          break;
        default:
          channel_char = '?';
          break;
        }

      switch (dimm)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_DIMM_1:
          dimm_str = "Dimm 1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_DIMM_2:
          dimm_str = "Dimm 2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_E52600V3_DIMM_3:
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
      && system_event_record_data->event_data2 == IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CATERR)
    {
      char cpu_str[INTEL_EVENT_BUFFER_LENGTH + 1];
      unsigned int wlen = 0;

      memset (cpu_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU1)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU1"))
            return (1);
        }

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU2)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU2"))
            return (1);
        }

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU3)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU3"))
            return (1);
        }

      if (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_PROCESSOR_EVENT_DATA2_OEM_INTEL_E52600V3_CPU4)
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "CPU4"))
            return (1);
        }

      if (!strlen (cpu_str))
        {
          if (sel_string_strcat_comma_separate (cpu_str, INTEL_EVENT_BUFFER_LENGTH, &wlen, "Unknown CPU"))
            return (1);
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

      auto_config_error_bitmask = (system_event_record_data->event_data3 & IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_E52600V3_BITMASK);
      auto_config_error_bitmask >>= IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_E52600V3_SHIFT;

      switch (auto_config_error_bitmask)
        {
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_E52600V3_CFG_SYNTAX_ERROR:
          str = "CFG syntax error";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_E52600V3_CHASSIS_AUTO_DETECT_ERROR:
          str = "Chassis auto-detect error";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_E52600V3_SDR_CFG_FILE_MISMATCH:
          str = "SDR/CFG file mismatch";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_E52600V3_SDR_OR_CFG_FILE_CORRUPTED:
          str = "SDR or CFG file corrupted";
          break;
        case IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED_MANAGEMENT_HEALTH_AUTO_CONFIG_ERROR_EVENT_DATA3_OEM_INTEL_E52600V3_SDR_SYNTAX_ERROR:
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_CONFIGURATION_STATUS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_e52600v3_ras_mode (system_event_record_data->event_data3);

      snprintf (tmpbuf,
                tmpbuflen,
                "RAS Mode Configured = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_POST
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_MEMORY_RAS_MODE_SELECT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_e52600v3_ras_mode (system_event_record_data->event_data3);

      snprintf (tmpbuf,
                tmpbuflen,
                "Selected RAS Mode = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_LEGACY_PCI_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_e52600v3_pci_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MIRRORING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      _sel_string_output_intel_e52600v3_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_SPARING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      _sel_string_output_intel_e52600v3_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      _sel_string_output_intel_e52600v3_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_e52600v3_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

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

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_FATAL_ERROR2
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS_2)
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_PCI_EXPRESS_CORRECTABLE_ERROR
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_CORRECTABLE_ERRORS))
    {
      _sel_string_output_intel_e52600v3_pci_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

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
sel_string_output_intel_e52600v3_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_POST
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
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_SYSTEM_RTC_DATE_TIME_NOT_SET:
          error_code_str = "System RTC date/time not set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
          error_code_str = "Password check failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT:
          error_code_str = "PCI resource conflict";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
          error_code_str = "PCI out of resources error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_CORE_THREAD_COUNT_MISMATCH_DETECTED:
          error_code_str = "Processor core/thread count mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_CACHE_SIZE_MISMATCH_DETECTED:
          error_code_str = "Processor cache size mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_FAMILY_MISMATCH_DETECTED:
          error_code_str = "Processor family mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_INTEL_QPI_LINK_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor Intel(R) QPI link frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_MODEL_MISMATCH_DETECTED:
          error_code_str = "Processor model mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_BIOS_SETTINGS_RESET_TO_DEFAULT_SETTINGS:
          error_code_str = "BIOS Settings reset to default settings";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
          error_code_str = "Passwords cleared by jumper";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
          error_code_str = "Password clear jumper is set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_01_DISABLED:
          error_code_str = "Processor 01 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_02_DISABLED:
          error_code_str = "Processor 02 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_03_DISABLED:
          error_code_str = "Processor 03 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_04_DISABLED:
          error_code_str = "Processor 04 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 01 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 02 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_03_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 03 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_04_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 04 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_01_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 01 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_02_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 02 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_03_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 03 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_04_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 04 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_01_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 01 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_02_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 02 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_03_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 03 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PROCESSOR_04_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 04 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
          error_code_str = "Watchdog timer failed on last boot";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
          error_code_str = "OS boot watchdog timer failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
          error_code_str = "Baseboard management controller failed self-test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_HOT_SWAP_CONTROLLER_FAILURE:
          error_code_str = "Hot-Swap Controller failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_MANAGEMENT_ENGINE_ME_FAILED_SELF_TEST:
          error_code_str = "Management Engine (ME) failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_MANAGEMENT_ME_FAILED_TO_RESPOND:
          error_code_str = "Management Engine (ME) Failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
          error_code_str = "Baseboard management controller failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
          error_code_str = "Baseboard management controller in update mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_SENSOR_DATA_RECORD_EMPTY:
          error_code_str = "Sensor data record empty";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL:
          error_code_str = "System event log full";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE:
          error_code_str = "Memory component could not be configured in the selected RAS mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_POPULATION_ERROR:
          error_code_str = "DIMM Population Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_A1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_A2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_A3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_B1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_B2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_B3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_C1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_C2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_C3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_D1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_D2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_D3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_E1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_E2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_E3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_F1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_F2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_F3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_G1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_G2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_G3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_H1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_H2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_H3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_J1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_J2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_J3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_K1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_K2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_K3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_L1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_L2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_L3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_M1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_M2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_M3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_N1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_N2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_N3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_P1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_P2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_P3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_R1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_R2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_R3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_T1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_T2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_T3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A1_DISABLED:
          error_code_str = "DIMM_A1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A2_DISABLED:
          error_code_str = "DIMM_A2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A3_DISABLED:
          error_code_str = "DIMM_A3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B1_DISABLED:
          error_code_str = "DIMM_B1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B2_DISABLED:
          error_code_str = "DIMM_B2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B3_DISABLED:
          error_code_str = "DIMM_B3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C1_DISABLED:
          error_code_str = "DIMM_C1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C2_DISABLED:
          error_code_str = "DIMM_C2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C3_DISABLED:
          error_code_str = "DIMM_C3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D1_DISABLED:
          error_code_str = "DIMM_D1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D2_DISABLED:
          error_code_str = "DIMM_D2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D3_DISABLED:
          error_code_str = "DIMM_D3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E1_DISABLED:
          error_code_str = "DIMM_E1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E2_DISABLED:
          error_code_str = "DIMM_E2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E3_DISABLED:
          error_code_str = "DIMM_E3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F1_DISABLED:
          error_code_str = "DIMM_F1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F2_DISABLED:
          error_code_str = "DIMM_F2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F3_DISABLED:
          error_code_str = "DIMM_F3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G1_DISABLED:
          error_code_str = "DIMM_G1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G2_DISABLED:
          error_code_str = "DIMM_G2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G3_DISABLED:
          error_code_str = "DIMM_G3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H1_DISABLED:
          error_code_str = "DIMM_H1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H2_DISABLED:
          error_code_str = "DIMM_H2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H3_DISABLED:
          error_code_str = "DIMM_H3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J1_DISABLED:
          error_code_str = "DIMM_J1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J2_DISABLED:
          error_code_str = "DIMM_J2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J3_DISABLED:
          error_code_str = "DIMM_J3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K1_DISABLED:
          error_code_str = "DIMM_K1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K2_DISABLED:
          error_code_str = "DIMM_K2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K3_DISABLED:
          error_code_str = "DIMM_K3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L1_DISABLED:
          error_code_str = "DIMM_L1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L2_DISABLED:
          error_code_str = "DIMM_L2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L3_DISABLED:
          error_code_str = "DIMM_L3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M1_DISABLED:
          error_code_str = "DIMM_M1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M2_DISABLED:
          error_code_str = "DIMM_M2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M3_DISABLED:
          error_code_str = "DIMM_M3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N1_DISABLED:
          error_code_str = "DIMM_N1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N2_DISABLED:
          error_code_str = "DIMM_N2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N3_DISABLED:
          error_code_str = "DIMM_N3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P1_DISABLED:
          error_code_str = "DIMM_P1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P2_DISABLED:
          error_code_str = "DIMM_P2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P3_DISABLED:
          error_code_str = "DIMM_P3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R1_DISABLED:
          error_code_str = "DIMM_R1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R2_DISABLED:
          error_code_str = "DIMM_R2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R3_DISABLED:
          error_code_str = "DIMM_R3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T1_DISABLED:
          error_code_str = "DIMM_T1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T2_DISABLED:
          error_code_str = "DIMM_T2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T3_DISABLED:
          error_code_str = "DIMM_T3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_A1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_A2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_A3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_A3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_B1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_B2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_B3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_B3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_C1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_C2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_C3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_C3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_D1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_D2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_D3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_D3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_E1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_E2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_E3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_E3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_F1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_F2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_F3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_F3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_G1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_G2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_G3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_G3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_H1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_H2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_H3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_H3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_J1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_J2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_J3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_J3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_K1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_K2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_K3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_K3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_L1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_L2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_L3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_L3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_M1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_M2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_M3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_M3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_N1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_N2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_N3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_N3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_P1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_P2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_P3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_P3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_R1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_R2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_R3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_R3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_T1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_T2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DIMM_T3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURED:
          error_code_str = "DIMM_T3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_POST_RECLAIM_OF_NON_CRITICAL_VARIABLES:
          error_code_str = "POST Reclaim of non-critical variables";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_BIOS_SETTINGS_ARE_CORRUPTED:
          error_code_str = "BIOS Settings are corrupted";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_NVRAM_VARIABLE_SPACE_WAS_CORRUPTED_AND_HAS_BEEN_REINITIALIZED:
          error_code_str = "NVRAM variable space was corrupted and has been reinitialized";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_WAS_NOT_DETECTED:
          error_code_str = "Serial port component was not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
          error_code_str = "Serial port component encountered a resource conflict error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED:
          error_code_str = "TPM device not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING:
          error_code_str = "TPM device missing or not responding";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_TPM_DEVICE_FAILURE:
          error_code_str = "TPM device failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST:
          error_code_str = "TPM device failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_BIOS_ACM_ERROR:
          error_code_str = "BIOS ACM Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI Express component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI Express component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_E52600V3_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM:
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

  /* achu: Documentation states only
   * IPMI_SENSOR_TYPE_MEMORY_MEMORY_SCRUB_FAILED, but I think
   * that's a typo. It should be IPMI_SENSOR_TYPE_MEMORY_PARITY.
   * Gonna check for both
   */
  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_E52600V3_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_PARITY_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_MEMORY_SCRUB_FAILED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PARITY)
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t channel_information_validity_check, dimm_information_validity_check, error_type;
      char *error_type_str;
      char dimm_str[INTEL_EVENT_BUFFER_LENGTH + 1];

      memset (dimm_str, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

      channel_information_validity_check = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_CHANNEL_INFORMATON_VALIDITY_CHECK_BITMASK);
      channel_information_validity_check >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_CHANNEL_INFORMATON_VALIDITY_CHECK_SHIFT;

      dimm_information_validity_check = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_DIMM_INFORMATON_VALIDITY_CHECK_BITMASK);
      dimm_information_validity_check >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_DIMM_INFORMATON_VALIDITY_CHECK_SHIFT;

      error_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_ERROR_TYPE_BITMASK);
      error_type >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_ERROR_TYPE_SHIFT;

      switch (error_type)
        {
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_ERROR_TYPE_PARITY_ERROR_TYPE_NOT_KNOWN:
          error_type_str = "Parity Error Type not known";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_ERROR_TYPE_DATA_PARITY_ERROR:
          error_type_str = "Data Parity Error";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_E52600V3_EVENT_DATA2_ERROR_TYPE_COMMAND_AND_ADDRESS_PARITY_ERROR:
          error_type_str = "Command and Address Parity Error";
          break;
        default:
          error_type_str = "Unknown";
        }

      _sel_string_output_intel_e52600v3_memory_dimm (ctx,
                                                     dimm_str,
                                                     INTEL_EVENT_BUFFER_LENGTH,
                                                     flags,
                                                     system_event_record_data,
                                                     channel_information_validity_check,
                                                     dimm_information_validity_check);

      if (sel_string_snprintf (buf,
                               buflen,
                               wlen,
                               "Error Type = %s, %s",
                               error_type_str,
                               dimm_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;

      return (1);
    }

  return (0);
}
