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
sel_string_output_intel_s2600wp_sensor_name (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WP);

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
sel_string_output_intel_s2600wp_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WP);

  /*
   * Intel S2600WP
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
      && ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
           && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR
                && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR)
               || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2
                   && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR_2)
               || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR
                   && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_ERROR)
               || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR
                   && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_ERROR)
               || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR_2
                   && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_ERROR_2)))
          || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_LINK_WIDTH_REDUCED
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_LINK_WIDTH_REDUCED)))
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_CORRECTABLE_ERRORS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_CORRECTABLE_ERROR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "QPI Correctable Errors Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_CHIPSET_PROPRIETARY
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_CHIPSET_PROPRIETARY)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Chipset Proprietary Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ERROR_EXTENSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_MEMORY_ERROR_EXTENSION)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Memory Error Extension Event = %02Xh",
                system_event_record_data->offset_from_event_reading_type_code);

      return (1);
    }

  return (0);
}

static void
_sel_string_output_intel_s2600wp_bus (ipmi_sel_ctx_t ctx,
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

static const char *
_sel_string_output_intel_s2600wp_ras_mode (uint8_t event_data)
{
  uint8_t ras_mode;
  char *ras_mode_str;

  ras_mode = (event_data & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600WP_RAS_MODE_BITMASK);
  ras_mode >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600WP_RAS_MODE_SHIFT;

  switch (ras_mode)
    {
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600WP_RAS_MODE_NONE:
      ras_mode_str = "None";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600WP_RAS_MODE_MIRRORING:
      ras_mode_str = "Mirroring";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600WP_RAS_MODE_LOCKSTEP:
      ras_mode_str = "Lockstep";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600WP_RAS_MODE_RANK_SPARING:
      ras_mode_str = "Rank Sparing";
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
sel_string_output_intel_s2600wp_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WP);

  /*
   * Intel S2600WP
   */

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MIRRORING_REDUNDANCY_STATE
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      uint8_t mirroring_domain_channel;
      uint8_t dimm_rank_number;

      mirroring_domain_channel = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_MIRRORING_DOMAIN_CHANNEL_BITMASK);
      mirroring_domain_channel >>= IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_MIRRORING_DOMAIN_CHANNEL_SHIFT;

      dimm_rank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_DIMM_RANK_NUMBER_BITMASK);
      dimm_rank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_DIMM_RANK_NUMBER_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Mirroring Domain Channel Pair for Socket = %u, DIMM Rank Number = %u",
                mirroring_domain_channel,
                dimm_rank_number);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_RAS_CONFIGURATION_STATUS
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      uint8_t config_error;
      char *config_error_str;

      config_error = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600WP_CONFIG_ERROR_BITMASK);
      config_error >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600WP_CONFIG_ERROR_SHIFT;

      switch (config_error)
        {
        case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600WP_CONFIG_ERROR_NONE:
          config_error_str = "None";
          break;
        case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600WP_CONFIG_ERROR_INVALID_DIMM_CONFIG_FOR_RAS_MODE:
          config_error_str = "Invalid";
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
    {
      uint8_t dimm_rank_number;

      dimm_rank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_DIMM_RANK_NUMBER_BITMASK);
      dimm_rank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_DIMM_RANK_NUMBER_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "DIMM Rank Number = %u",
                dimm_rank_number);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_LEGACY_PCI_ERROR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_s2600wp_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_SPARING_REDUNDANCY_STATE
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
    {
      uint8_t sparing_domain_channel;
      uint8_t dimm_rank_number;
      char *sparing_domain_channel_str;

      sparing_domain_channel = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_SPARING_DOMAIN_CHANNEL_BITMASK);
      sparing_domain_channel >>= IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_SPARING_DOMAIN_CHANNEL_SHIFT;

      dimm_rank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_DIMM_RANK_NUMBER_BITMASK);
      dimm_rank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_DIMM_RANK_NUMBER_SHIFT;

      switch (sparing_domain_channel)
        {
        case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_SPARING_DOMAIN_CHANNEL_A:
          sparing_domain_channel_str = "A";
          break;
        case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_SPARING_DOMAIN_CHANNEL_B:
          sparing_domain_channel_str = "B";
          break;
        case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_SPARING_DOMAIN_CHANNEL_C:
          sparing_domain_channel_str = "C";
          break;
        case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600WP_SPARING_DOMAIN_CHANNEL_D:
          sparing_domain_channel_str = "D";
          break;
        default:
          sparing_domain_channel_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "Sparing Domain Channel Pair for Socket = %s, DIMM Rank Number = %u",
                sparing_domain_channel_str,
                dimm_rank_number);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_RAS_MODE_SELECT
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_s2600wp_ras_mode (system_event_record_data->event_data2);

      snprintf (tmpbuf,
                tmpbuflen,
                "Previous RAS Mode = %s",
                ras_mode_str);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600wp_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WP);

  /*
   * Intel S2600WP
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR
           && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR_2)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_ERROR)))
    {
      _sel_string_output_intel_s2600wp_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
           && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR
                && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_ERROR)
               || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR_2
                   && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_ERROR_2)))
          || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_LINK_WIDTH_REDUCED
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_LINK_WIDTH_REDUCED)))
    {
      uint8_t node_id;
      char *node_id_str;

      node_id = system_event_record_data->event_data2;

      switch (node_id)
        {
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600WP_NODE_ID_CPU_1:
          node_id_str = "1";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600WP_NODE_ID_CPU_2:
          node_id_str = "2";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600WP_NODE_ID_CPU_3:
          node_id_str = "3";
          break;
        case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600WP_NODE_ID_CPU_4:
          node_id_str = "4";
          break;
        default:
          node_id_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU = %s",
                node_id_str);

      return (1);
    }

  return (0);
}

static void
_sel_string_output_intel_s2600wp_device_function (ipmi_sel_ctx_t ctx,
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
sel_string_output_intel_s2600wp_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WP);

  /*
   * Intel S2600WP
   */

  if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
       && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
       && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MIRRORING_REDUNDANCY_STATE
           || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_SPARING_REDUNDANCY_STATE)
       && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
           || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR)))
    {
      uint8_t socket_id;
      uint8_t channel;
      uint8_t dimm;
      char *socket_id_str;
      char *channel_str;
      char *dimm_str;

      socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_BITMASK);
      socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_SHIFT;

      channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_BITMASK);
      channel >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_SHIFT;

      dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_BITMASK);
      dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_SHIFT;

      switch (socket_id)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_1:
          socket_id_str = "1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_2:
          socket_id_str = "2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_3:
          socket_id_str = "3";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_4:
          socket_id_str = "4";
          break;
        default:
          socket_id_str = "Unknown";
        }

      switch (channel)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_A:
          channel_str = "A";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_B:
          channel_str = "B";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_C:
          channel_str = "C";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_D:
          channel_str = "D";
          break;
        default:
          channel_str = "Unknown";
        }

      switch (dimm)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_1:
          dimm_str = "1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_2:
          dimm_str = "2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_3:
          dimm_str = "3";
          break;
        default:
          dimm_str = "Unknown";
        }

      snprintf (tmpbuf,
                tmpbuflen,
                "CPU = %s, Channel = %s, DIMM = %s",
                socket_id_str,
                channel_str,
                dimm_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_RAS_CONFIGURATION_STATUS
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_s2600wp_ras_mode (system_event_record_data->event_data3);

      snprintf (tmpbuf,
                tmpbuflen,
                "RAS Mode = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_RAS_MODE_SELECT
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
    {
      const char *ras_mode_str;

      ras_mode_str = _sel_string_output_intel_s2600wp_ras_mode (system_event_record_data->event_data3);

      snprintf (tmpbuf,
                tmpbuflen,
                "Selected RAS Mode = %s",
                ras_mode_str);

      return (1);
    }

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_LEGACY_PCI_ERROR
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      _sel_string_output_intel_s2600wp_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600wp_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WP);

  /*
   * Intel S2600WP
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR
           && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR_2)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_ERROR)))
    {
      _sel_string_output_intel_s2600wp_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

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
sel_string_output_intel_s2600wp_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WP);

  /*
   * Intel S2600WP
   */
  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
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
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_SYSTEM_RTC_DATE_TIME_NOT_SET:
          error_code_str = "System RTC date/time not set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
          error_code_str = "Password check failed";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_FIXED_MEDIA_NOT_DETECTED:
          error_code_str = "Fixed media not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT:
          error_code_str = "PCI resource conflict";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
          error_code_str = "PCI out of resources error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_CORE_THREAD_COUNT_MISMATCH_DETECTED:
          error_code_str = "Processor core/thread count mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_CACHE_SIZE_MISMATCH_DETECTED:
          error_code_str = "Processor cache size mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_FAMILY_MISMATCH_DETECTED:
          error_code_str = "Processor family mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_INTEL_QPI_LINK_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor Intel(R) QPI link frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_MODEL_MISMATCH_DETECTED:
          error_code_str = "Processor model mismatch detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_FREQUENCIES_UNABLE_TO_SYNCHRONIZE:
          error_code_str = "Processor frequencies unable to synchronize";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_BIOS_SETTINGS_RESET_TO_DEFAULT_SETTINGS:
          error_code_str = "BIOS settings reset to default settings";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
          error_code_str = "Passwords cleared by jumper";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
          error_code_str = "Password clear jumper is set";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_01_DISABLED:
          error_code_str = "Processor 01 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_02_DISABLED:
          error_code_str = "Processor 02 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_03_DISABLED:
          error_code_str = "Processor 03 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_04_DISABLED:
          error_code_str = "Processor 04 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 01 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 02 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_03_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 03 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_04_UNABLE_TO_APPLY_MICROCODE_UPDATE:
          error_code_str = "Processor 04 unable to apply microcode update";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_01_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 01 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_02_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 02 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_03_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 03 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_04_FAILED_SELF_TEST_BIST:
          error_code_str = "Processor 04 failed Self Test (BIST)";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_01_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 01 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_02_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 02 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_03_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 03 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PROCESSOR_04_MICROCODE_UPDATE_NOT_FOUND:
          error_code_str = "Processor 04 microcode update not found";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
          error_code_str = "Watchdog timer failed on last boot";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
          error_code_str = "OS boot watchdog timer failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
          error_code_str = "Baseboard management controller failed self-test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_HOT_SWAP_CONTROLLER_FAILURE:
          error_code_str = "Hot Swap Controller failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_MANAGEMENT_ENGINE_FAILED_SELF_TEST:
          error_code_str = "Management Engine (ME) failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_MANAGEMENT_ENGINE_FAILED_TO_RESPOND:
          error_code_str = "Management Engine (ME) Failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
          error_code_str = "Baseboard management controller failed to respond";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
          error_code_str = "Baseboard management controller in update mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_SENSOR_DATA_RECORD_EMPTY:
          error_code_str = "Sensor data record empty";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL:
          error_code_str = "System event log full";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE:
          error_code_str = "Memory component could not be configured in the selected RAS mode";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_POPULATION_ERROR:
          error_code_str = "DIMM Population Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM A1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM A2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM A3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM B1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM B2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM B3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM C1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM C2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM C3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM D1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM D2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM D3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM E1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM E2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM E3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM F1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM F2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM F3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM G1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM G2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM G3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM H1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM H2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM H3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM J1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM J2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM J3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM K1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM K2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM K3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM L1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM L2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM L3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM M1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM M2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM M3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM N1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM N2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM N3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM P1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM P2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM P3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM R1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM R2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM R3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T1_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM T1 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T2_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM T2 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T3_FAILED_TEST_INITIALIZATION:
          error_code_str = "DIMM T3 failed test/initialization";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A1_DISABLED:
          error_code_str = "DIMM A1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A2_DISABLED:
          error_code_str = "DIMM A2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A3_DISABLED:
          error_code_str = "DIMM A3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B1_DISABLED:
          error_code_str = "DIMM B1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B2_DISABLED:
          error_code_str = "DIMM B2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B3_DISABLED:
          error_code_str = "DIMM B3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C1_DISABLED:
          error_code_str = "DIMM C1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C2_DISABLED:
          error_code_str = "DIMM C2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C3_DISABLED:
          error_code_str = "DIMM C3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D1_DISABLED:
          error_code_str = "DIMM D1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D2_DISABLED:
          error_code_str = "DIMM D2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D3_DISABLED:
          error_code_str = "DIMM D3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E1_DISABLED:
          error_code_str = "DIMM E1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E2_DISABLED:
          error_code_str = "DIMM E2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E3_DISABLED:
          error_code_str = "DIMM E3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F1_DISABLED:
          error_code_str = "DIMM F1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F2_DISABLED:
          error_code_str = "DIMM F2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F3_DISABLED:
          error_code_str = "DIMM F3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G1_DISABLED:
          error_code_str = "DIMM G1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G2_DISABLED:
          error_code_str = "DIMM G2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G3_DISABLED:
          error_code_str = "DIMM G3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H1_DISABLED:
          error_code_str = "DIMM H1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H2_DISABLED:
          error_code_str = "DIMM H2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H3_DISABLED:
          error_code_str = "DIMM H3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J1_DISABLED:
          error_code_str = "DIMM J1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J2_DISABLED:
          error_code_str = "DIMM J2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J3_DISABLED:
          error_code_str = "DIMM J3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K1_DISABLED:
          error_code_str = "DIMM K1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K2_DISABLED:
          error_code_str = "DIMM K2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K3_DISABLED:
          error_code_str = "DIMM K3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L1_DISABLED:
          error_code_str = "DIMM L1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L2_DISABLED:
          error_code_str = "DIMM L2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L3_DISABLED:
          error_code_str = "DIMM L3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M1_DISABLED:
          error_code_str = "DIMM M1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M2_DISABLED:
          error_code_str = "DIMM M2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M3_DISABLED:
          error_code_str = "DIMM M3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N1_DISABLED:
          error_code_str = "DIMM N1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N2_DISABLED:
          error_code_str = "DIMM N2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N3_DISABLED:
          error_code_str = "DIMM N3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P1_DISABLED:
          error_code_str = "DIMM P1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P2_DISABLED:
          error_code_str = "DIMM P2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P3_DISABLED:
          error_code_str = "DIMM P3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R1_DISABLED:
          error_code_str = "DIMM R1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R2_DISABLED:
          error_code_str = "DIMM R2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R3_DISABLED:
          error_code_str = "DIMM R3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T1_DISABLED:
          error_code_str = "DIMM T1 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T2_DISABLED:
          error_code_str = "DIMM T2 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T3_DISABLED:
          error_code_str = "DIMM T3 disabled";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM A1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM A2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_A3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM A3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM B1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM B2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_B3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM B3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM C1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM C2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_C3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM C3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM D1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM D2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_D3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM D3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM E1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM E2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_E3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM E3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM F1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM F2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_F3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM F3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM G1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM G2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_G3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM G3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM H1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM H2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_H3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM H3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM J1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM J2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_J3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM J3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM K1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM K2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_K3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM K3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM L1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM L2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_L3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM L3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM M1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM M2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_M3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM M3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM N1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM N2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_N3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM N3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM P1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM P2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_P3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM P3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM R1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM R2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_R3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM R3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T1_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM T1 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T2_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM T2 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DIMM_T3_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
          error_code_str = "DIMM T3 encountered a Serial Presence Detection (SPD) failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_POST_RECLAIM_OF_NON_CRITICAL_NVRAM_VARIABLES:
          error_code_str = "POST Reclaim of non-critical NVRAM variables";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_BIOS_SETTINGS_ARE_CORRUPTED:
          error_code_str = "BIOS Settings are corrupted";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_NVRAM_VARIABLE_SPACE_WAS_CORRUPTED_AND_HAS_BEEN_REINITIALIZED:
          error_code_str = "NVRAM variable space was corrupted and has been reinitialized";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_WAS_NOT_DETECTED:
          error_code_str = "Serial port component was not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
          error_code_str = "Serial port component encountered a resource conflict error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_ATA_ATPI_INTERFACE_ERROR:
          error_code_str = "ATA/ATPI interface error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED:
          error_code_str = "TPM device not detected";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING:
          error_code_str = "TPM device missing or not responding";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_TPM_DEVICE_FAILURE:
          error_code_str = "TPM device failure";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST:
          error_code_str = "TPM device failed self test";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_BIOS_ACM_ERROR:
          error_code_str = "BIOS ACM Error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
          error_code_str = "PCI Express component encountered a PERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
          error_code_str = "PCI Express component encountered a SERR error";
          break;
        case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600WP_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM:
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

  if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_SMI_HANDLER
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_PARITY_ERROR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PARITY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t channel_valid;
      uint8_t dimm_valid;
      uint8_t error_type;
      uint8_t socket_id;
      uint8_t channel;
      uint8_t dimm;
      char *error_type_str;
      char *socket_id_str;
      char *channel_str;
      char *dimm_str;

      channel_valid = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_EVENT_DATA3_CHANNEL_VALID_BITMASK);
      channel_valid >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_EVENT_DATA3_CHANNEL_VALID_SHIFT;

      dimm_valid = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_EVENT_DATA3_DIMM_VALID_BITMASK);
      dimm_valid >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_EVENT_DATA3_DIMM_VALID_SHIFT;

      error_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_ERROR_TYPE_BITMASK);
      error_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_ERROR_TYPE_SHIFT;

      socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_BITMASK);
      socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_SHIFT;

      channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_BITMASK);
      channel >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_SHIFT;

      dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_BITMASK);
      dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_SHIFT;

      switch (error_type)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_ERROR_TYPE_NOT_KNOWN:
          error_type_str = "Not Known";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600WP_ERROR_TYPE_ADDRESS_PARITY_ERROR:
          error_type_str = "Address Parity Error";
          break;
        default:
          error_type_str = "Unknown";
        }

      switch (socket_id)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_1:
          socket_id_str = "1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_2:
          socket_id_str = "2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_3:
          socket_id_str = "3";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_SOCKET_ID_CPU_4:
          socket_id_str = "4";
          break;
        default:
          socket_id_str = "Unknown";
        }

      switch (channel)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_A:
          channel_str = "A";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_B:
          channel_str = "B";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_C:
          channel_str = "C";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_CHANNEL_D:
          channel_str = "D";
          break;
        default:
          channel_str = "Unknown";
        }

      switch (dimm)
        {
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_1:
          dimm_str = "1";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_2:
          dimm_str = "2";
          break;
        case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600WP_DIMM_3:
          dimm_str = "3";
          break;
        default:
          dimm_str = "Unknown";
        }

      if (sel_string_snprintf (buf,
                               buflen,
                               wlen,
                               "Error Type = %s, CPU = %s%s%s%s%s",
                               error_type_str,
                               socket_id_str,
                               channel_valid ? ", Channel = " : "",
                               channel_valid ? channel_str : "",
                               dimm_valid ? ", DIMM = " : "",
                               dimm_valid ? dimm_str : ""))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;

      return (1);
    }

  return (0);
}

struct sel_string_oem sel_string_oem_intel_s2600wp =
  {
    sel_string_output_intel_s2600wp_sensor_name,
    NULL,
    sel_string_output_intel_s2600wp_event_data1_class_oem,
    NULL,
    sel_string_output_intel_s2600wp_event_data2_discrete_oem,
    sel_string_output_intel_s2600wp_event_data2_class_oem,
    NULL,
    sel_string_output_intel_s2600wp_event_data3_discrete_oem,
    sel_string_output_intel_s2600wp_event_data3_class_oem,
    sel_string_output_intel_s2600wp_event_data2_event_data3,
    NULL,
    NULL,
  };
