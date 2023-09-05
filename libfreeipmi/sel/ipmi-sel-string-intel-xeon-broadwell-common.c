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

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-intel-xeon-broadwell-common.h"
#include "ipmi-sel-string-intel-xeon-common.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define INTEL_EVENT_BUFFER_LENGTH 4096

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_xeon_broadwell_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MIRRORING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      uint8_t mirroring_domain, rank_on_dimm;
      char *mirroring_domain_str;

      mirroring_domain = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_MIRRORING_DOMAIN_BITMASK);
      mirroring_domain >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_MIRRORING_DOMAIN_SHIFT;

      rank_on_dimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_RANK_ON_DIMM_BITMASK);
      rank_on_dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_RANK_ON_DIMM_SHIFT;

      switch (mirroring_domain)
        {
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_MIRRORING_DOMAIN_0:
          mirroring_domain_str = "0";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_MIRRORING_DOMAIN_1:
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      uint8_t rank_on_dimm;

      rank_on_dimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_RANK_ON_DIMM_BITMASK);
      rank_on_dimm >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_RANK_ON_DIMM_SHIFT;

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
sel_string_output_intel_xeon_broadwell_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT
          || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MIRRORING_REDUNDANCY_STATE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      sel_string_output_intel_xeon_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_SMI_MEMORY_ECC_ERROR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      sel_string_output_intel_xeon_memory_dimm (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data, 1, 1);

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
sel_string_output_intel_xeon_broadwell_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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

  /* achu: Documentation states only
   * IPMI_SENSOR_TYPE_MEMORY_MEMORY_SCRUB_FAILED, but I think
   * that's a typo. It should be IPMI_SENSOR_TYPE_MEMORY_PARITY.
   * Gonna check for both
   */
  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
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

      channel_information_validity_check = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_CHANNEL_INFORMATION_VALIDITY_CHECK_BITMASK);
      channel_information_validity_check >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_CHANNEL_INFORMATION_VALIDITY_CHECK_SHIFT;

      dimm_information_validity_check = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_DIMM_INFORMATION_VALIDITY_CHECK_BITMASK);
      dimm_information_validity_check >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_DIMM_INFORMATION_VALIDITY_CHECK_SHIFT;

      error_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_ERROR_TYPE_BITMASK);
      error_type >>= IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_ERROR_TYPE_SHIFT;

      switch (error_type)
        {
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_ERROR_TYPE_PARITY_ERROR_TYPE_NOT_KNOWN:
          error_type_str = "Parity Error Type not known";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_ERROR_TYPE_DATA_PARITY_ERROR:
          error_type_str = "Data Parity Error";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_OEM_INTEL_EVENT_DATA2_ERROR_TYPE_COMMAND_AND_ADDRESS_PARITY_ERROR:
          error_type_str = "Command and Address Parity Error";
          break;
        default:
          error_type_str = "Unknown";
        }

      sel_string_output_intel_xeon_memory_dimm (ctx,
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
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_SYSTEM_RTC_DATE_TIME_NOT_SET:
          error_code_str = "System RTC date/time not set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
          error_code_str = "Password check failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT:
          error_code_str = "PCI resource conflict";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
          error_code_str = "PCI out of resources error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_CORE_THREAD_COUNT_MISMATCH_DETECTED:
          error_code_str = "Processor core/thread count mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_CACHE_SIZE_MISMATCH_DETECTED:
          error_code_str = "Processor cache size mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_FAMILY_MISMATCH_DETECTED:
          error_code_str = "Processor family mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_INTEL_QPI_LINK_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor Intel(R) QPI link frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_MODEL_MISMATCH_DETECTED:
          error_code_str = "Processor model mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_BIOS_SETTINGS_RESET_TO_DEFAULT_SETTINGS:
          error_code_str = "BIOS Settings reset to default settings";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
          error_code_str = "Passwords cleared by jumper";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
          error_code_str = "Password clear jumper is set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_01_DISABLED:
          error_code_str = "Processor 01 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_02_DISABLED:
          error_code_str = "Processor 02 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_03_DISABLED:
          error_code_str = "Processor 03 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_04_DISABLED:
          error_code_str = "Processor 04 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 01 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 02 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_03_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 03 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_04_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 04 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_01_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 01 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_02_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 02 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_03_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 03 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_04_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 04 failed Self Test (BIST) ";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_01_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 01 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_02_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 02 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_03_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 03 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PROCESSOR_04_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 04 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
          error_code_str = "Watchdog timer failed on last boot";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
          error_code_str = "OS boot watchdog timer failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
          error_code_str = "Baseboard management controller failed self-test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_HOT_SWAP_CONTROLLER_FAILURE:
          error_code_str = "Hot-Swap Controller failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_MANAGEMENT_ENGINE_ME_FAILED_SELF_TEST:
          error_code_str = "Management Engine (ME) failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_MANAGEMENT_ME_FAILED_TO_RESPOND:
          error_code_str = "Management Engine (ME) Failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
          error_code_str = "Baseboard management controller failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
          error_code_str = "Baseboard management controller in update mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_SENSOR_DATA_RECORD_EMPTY:
          error_code_str = "Sensor data record empty";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL:
          error_code_str = "System event log full";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE:
          error_code_str = "Memory component could not be configured in the selected RAS mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_POPULATION_ERROR:
          error_code_str = "DIMM Population Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_A1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_A2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_A3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_B1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_B2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_B3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_C1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_C2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_C3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_D1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_D2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_D3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_E1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_E2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_E3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_F1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_F2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_F3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_G1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_G2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_G3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_H1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_H2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_H3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_J1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_J2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_J3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_K1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_K2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_K3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_L1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_L2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_L3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_M1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_M2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_M3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_N1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_N2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_N3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_P1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_P2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_P3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_R1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_R2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_R3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T1_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_T1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T2_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_T2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T3_FAILED_SELF_TEST_INITIALIZATION:
          error_code_str = "DIMM_T3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A1_DISABLED:
          error_code_str = "DIMM_A1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A2_DISABLED:
          error_code_str = "DIMM_A2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A3_DISABLED:
          error_code_str = "DIMM_A3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B1_DISABLED:
          error_code_str = "DIMM_B1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B2_DISABLED:
          error_code_str = "DIMM_B2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B3_DISABLED:
          error_code_str = "DIMM_B3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C1_DISABLED:
          error_code_str = "DIMM_C1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C2_DISABLED:
          error_code_str = "DIMM_C2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C3_DISABLED:
          error_code_str = "DIMM_C3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D1_DISABLED:
          error_code_str = "DIMM_D1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D2_DISABLED:
          error_code_str = "DIMM_D2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D3_DISABLED:
          error_code_str = "DIMM_D3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E1_DISABLED:
          error_code_str = "DIMM_E1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E2_DISABLED:
          error_code_str = "DIMM_E2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E3_DISABLED:
          error_code_str = "DIMM_E3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F1_DISABLED:
          error_code_str = "DIMM_F1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F2_DISABLED:
          error_code_str = "DIMM_F2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F3_DISABLED:
          error_code_str = "DIMM_F3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G1_DISABLED:
          error_code_str = "DIMM_G1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G2_DISABLED:
          error_code_str = "DIMM_G2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G3_DISABLED:
          error_code_str = "DIMM_G3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H1_DISABLED:
          error_code_str = "DIMM_H1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H2_DISABLED:
          error_code_str = "DIMM_H2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H3_DISABLED:
          error_code_str = "DIMM_H3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J1_DISABLED:
          error_code_str = "DIMM_J1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J2_DISABLED:
          error_code_str = "DIMM_J2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J3_DISABLED:
          error_code_str = "DIMM_J3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K1_DISABLED:
          error_code_str = "DIMM_K1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K2_DISABLED:
          error_code_str = "DIMM_K2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K3_DISABLED:
          error_code_str = "DIMM_K3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L1_DISABLED:
          error_code_str = "DIMM_L1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L2_DISABLED:
          error_code_str = "DIMM_L2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L3_DISABLED:
          error_code_str = "DIMM_L3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M1_DISABLED:
          error_code_str = "DIMM_M1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M2_DISABLED:
          error_code_str = "DIMM_M2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M3_DISABLED:
          error_code_str = "DIMM_M3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N1_DISABLED:
          error_code_str = "DIMM_N1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N2_DISABLED:
          error_code_str = "DIMM_N2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N3_DISABLED:
          error_code_str = "DIMM_N3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P1_DISABLED:
          error_code_str = "DIMM_P1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P2_DISABLED:
          error_code_str = "DIMM_P2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P3_DISABLED:
          error_code_str = "DIMM_P3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R1_DISABLED:
          error_code_str = "DIMM_R1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R2_DISABLED:
          error_code_str = "DIMM_R2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R3_DISABLED:
          error_code_str = "DIMM_R3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T1_DISABLED:
          error_code_str = "DIMM_T1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T2_DISABLED:
          error_code_str = "DIMM_T2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T3_DISABLED:
          error_code_str = "DIMM_T3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_A1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_A2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_A3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_A3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_B1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_B2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_B3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_B3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_C1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_C2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_C3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_C3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_D1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_D2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_D3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_D3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_E1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_E2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_E3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_E3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_F1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_F2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_F3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_F3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_G1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_G2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_G3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_G3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_H1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_H2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_H3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_H3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_J1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_J2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_J3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_J3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_K1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_K2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_K3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_K3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_L1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_L2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_L3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_L3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_M1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_M2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_M3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_M3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_N1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_N2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_N3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_N3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_P1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_P2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_P3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_P3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_R1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_R2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_R3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_R3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_T1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_T2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DIMM_T3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAILURE:
          error_code_str = "DIMM_T3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_POST_RECLAIM_OF_NON_CRITICAL_VARIABLES:
          error_code_str = "POST Reclaim of non-critical variables";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_BIOS_SETTINGS_ARE_CORRUPTED:
          error_code_str = "BIOS Settings are corrupted";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_NVRAM_VARIABLE_SPACE_WAS_CORRUPTED_AND_HAS_BEEN_REINITIALIZED:
          error_code_str = "NVRAM variable space was corrupted and has been reinitialized";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_WAS_NOT_DETECTED:
          error_code_str = "Serial port component was not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
          error_code_str = "Serial port component encountered a resource conflict error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED:
          error_code_str = "TPM device not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING:
          error_code_str = "TPM device missing or not responding";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_TPM_DEVICE_FAILURE:
          error_code_str = "TPM device failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST:
          error_code_str = "TPM device failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_BIOS_ACM_ERROR:
          error_code_str = "BIOS ACM Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI Express component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI Express component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_BROADWELL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM:
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
