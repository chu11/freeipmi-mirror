/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-numbers-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/spec/ipmi-slave-address-oem-spec.h"
#include "freeipmi/util/ipmi-iana-enterprise-numbers-util.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-dell.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define DELL_EVENT_BUFFER_LENGTH 4096

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sel_parse_output_dell_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);

  /* OEM Interpretation
   *
   * From Dell Code
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE))
    {
      int ret;

      ret = ipmi_get_oem_sensor_type_message (ctx->manufacturer_id,
                                              ctx->product_id,
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
ipmi_sel_parse_output_dell_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * From Dell Code
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS)
    {
      int ret;

      ret = ipmi_get_oem_generic_event_message (ctx->manufacturer_id,
                                                ctx->product_id,
                                                system_event_record_data->event_type_code,
                                                system_event_record_data->offset_from_event_reading_type_code,
                                                tmpbuf,
                                                tmpbuflen);

      if (ret > 0)
        return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Spec and Dell Code
   *
   * Dell Poweredge 2900
   * Dell Poweredge 2950
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "OEM Diagnostic Data Event");
      
      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sel_parse_output_dell_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710))
    {
      /* From Dell Engineer and Dell code */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
        {
          if (system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY_INTRUSION_WHILE_SYSTEM_ON)
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Intrusion while system On");
              
              return (1);
            }
          else if (system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY_INTRUSION_WHILE_SYSTEM_OFF)
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Intrusion while system Off");
              
              return (1);
            }
        }

      /* From Dell Spec */
      if ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
           && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
           && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_RECOVERABLE
           && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CPU_MACHINE_CHECK_ERROR)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
              && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_IERR))
        {
          unsigned int num = 0;
          int found = 0;
          int i;
          
          for (i = 0; i < 8; i++)
            {
              if (system_event_record_data->event_data2 & (0x1 << i))
                {
                  num = i + 1;
                  found++;
                  break;
                }
            }
          
          if (found)
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "CPU %u",
                        num);
              
              return (1);
            }
        }

      /* From Dell Spec */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_RECOVERABLE
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CPU_PROTOCOL_ERROR)
        {
          unsigned int num = 0;
          int found = 0;
          int i;
          
          for (i = 0; i < 8; i++)
            {
              if (system_event_record_data->event_data2 & (0x1 << i))
                {
                  num = i + 1;
                  found++;
                  break;
                }
            }
          
          if (found)
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Front Side Bus %u",
                        num);
              
              return (1);
            }
        }

      /* From Dell Engineer and Dell Code */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED
          && (system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_PSU_COMMUNICATION_ERROR
              || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_TEMPERATURE_WARNING
              || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_TEMPERATURE_FAULT
              || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_UNDER_VOLTAGE_FAULT
              || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_VOLTAGE_FAULT
              || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_CURRENT_FAULT
              || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_FAN_FAULT))
        {
	  char *str;

	  switch (system_event_record_data->event_data2)
	    {
	    case IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_PSU_COMMUNICATION_ERROR:
	      str = "PSU Communication Error";
	      break;
	    case IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_TEMPERATURE_WARNING:
	      str = "Over Temperature Warning";
	      break;
	    case IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_TEMPERATURE_FAULT:
	      str = "Over Temperature Fault";
	      break;
	    case IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_UNDER_VOLTAGE_FAULT:
	      str = "Under Voltage Fault";
	      break;
	    case IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_VOLTAGE_FAULT:
	      str = "Over Voltage Fault";
	      break;
	    case IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OVER_CURRENT_FAULT:
	      str = "Over Current Fault";
	      break;
	    case IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_FAN_FAULT:
	      str = "Fan Fault";
	      break;
	    default:
	      str = "Internal Logic Error";
	    }
          
	  snprintf (tmpbuf, tmpbuflen, "%s", str);

          return (1);
        }

      /* From Dell Spec */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT)
        {
          char *str = NULL;
          
          if (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_SPARE_MODE_BITMASK)
            str = "Memory is in Spare mode";
          else if (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_RAID_MODE_BITMASK)
            str = "Memory is in RAID mode";
          else /* system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MIRROR_MODE_BITMASK */
            str = "Memory is in Mirror mode";
          
          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    str);
          
          return (1);
        }

      /* From Dell Spec */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_DELL_POST_FATAL_ERROR)
        {
          char *error_code_str = NULL;

          /* achu: I am assuming only fatal error codes are possible, not progress codes */
	  switch (system_event_record_data->event_data2)
	    {
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_NO_MEMORY_DETECTED:
	      error_code_str = "No memory detected";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_MEMORY_DETECTED_BUT_IS_NOT_CONFIGURABLE:
	      error_code_str = "Memory detected but is not configurable";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_MEMORY_CONFIGURED_BUT_NOT_USABLE:
	      error_code_str = "Memory configured but not usable";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_SYSTEM_BIOS_SHADOW_FAILURE:
	      error_code_str = "System BIOS shadow failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_CMOS_FAILURE:
	      error_code_str = "CMOS failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_DMA_CONTROLLER_FAILURE:
	      error_code_str = "DMA controller failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_INTERRUPT_CONTROLLER_FAILURE:
	      error_code_str = "Interrupt controller failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_TIMER_REFRESH_FAILURE:
	      error_code_str = "Timer refresh failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_PROGRAMMABLE_INTERVAL_TIMER_ERROR:
	      error_code_str = "Programmable interval timer error";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_PARITY_ERROR:
	      error_code_str = "Parity error";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_SIO_FAILURE:
	      error_code_str = "SIO failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_KEYBOARD_CONTROLLER_FAILURE:
	      error_code_str = "Keyboard controller failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_SMI_INITIALIZATION_FAILURE:
	      error_code_str = "SMI initialization failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_SHUTDOWN_TEST_FAILURE:
	      error_code_str = "Shutdown test failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_POST_MEMORY_TEST_FAILURE:
	      error_code_str = "POST Memory test failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_RAC_CONFIGURATION_FAILURE:
	      error_code_str = "RAC configuration failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_CPU_CONFIGURATION_FAILURE:
	      error_code_str = "CPU configuration failure";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_INCORRECT_MEMORY_CONFIGURATION:
	      error_code_str = "Incorrect memory configuration";
	      break;
	    case IPMI_OEM_DELL_BIOS_FATAL_ERROR_CODE_GENERAL_FAILURE_AFTER_VIDEO:
	      error_code_str = "General failure after video";
	      break;
	    default:
	      snprintf (tmpbuf,
			tmpbuflen,
			"BIOS Fatal Error code: %02Xh",
			system_event_record_data->event_data2);
	    }
          
          if (error_code_str)
            snprintf (tmpbuf,
                      tmpbuflen,
                      "%s",
                      error_code_str);

          return (1);
        }

      /* From Dell Spec and Dell Code */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
               && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_FAILED_TO_PROGRAM_VIRTUAL_MAC_ADDRESS)
              || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
                  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
                  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CHIPSET_ERROR)))
        {
          uint8_t device, function;
          
          device = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_BITMASK);
          device >>= IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_SHIFT;
          
          function = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_BITMASK);
          function >>= IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_SHIFT;
          
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Device %u, Function %u",
                    device,
                    function);
          
          return (1);
        }

    }

  /* achu: I don't know what motherboards this applies to, probably very old ones */
#if 0
  /* OEM Interpretation
   *
   * From Dell Spec and Dell Code
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PRESENCE_DETECTED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CONFIGURATION_ERROR)
      && ctx->ipmi_version_major == IPMI_1_5_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_1_5_MINOR_VERSION)
    {
      uint8_t memory_card;
      uint8_t bank_number;
          
      memory_card = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_BITMASK);
      memory_card >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_SHIFT;

      bank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_BITMASK);
      bank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_SHIFT;

      if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_VALID (memory_card)
          && IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_VALID (bank_number))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Memory Card %c, Bank %u",
                    'A' + memory_card,
                    bank_number);
          return (1);
        }
      else if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_VALID (memory_card))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Memory Card %c",
                    'A' + memory_card);
          return (1);
        }
      else if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_VALID (bank_number))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Bank %u",
                    bank_number);
          return (1);
        }
    }
#endif

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sel_parse_output_dell_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * From Dell Spec
   *
   * Dell Poweredge 2900
   * Dell Poweredge 2950
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   * offset_from_event_reading_type_code = register offset 11:8
   * data2 = register offset 0:7
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING)
    {
      uint16_t register_offset;
      
      register_offset = system_event_record_data->event_data2;
      register_offset |= (system_event_record_data->offset_from_event_reading_type_code) << 8;
      
      snprintf (tmpbuf,
                tmpbuflen,
                "Register Offset = %Xh",
                register_offset);
      
      return (1);
    }
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sel_parse_output_dell_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710))
    {
      /* From Dell Spec */
      if ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
           && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
           && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_IERR)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
              && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_RECOVERABLE
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CPU_MACHINE_CHECK_ERROR))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "APIC ID %u",
                    system_event_record_data->event_data3);
          
          return (1);
        }
      
      /* From Dell Spec and Dell Code
       *
       * [7] - 0 = device with option ROM is embedded, 1 = device with option ROM is in a slot
       * [6:0] - slot number where option ROM is located
       *
       * Note: deassertion means unsuccessful
       */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_VERSION_CHANGE_HARDWARE_CHANGE_DETECTED_WITH_ASSOCIATED_ENTITY_WAS_SUCCESSFUL)
        {
          uint8_t option_rom;
          
          option_rom = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_BITMASK);
          option_rom >>= IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_SHIFT;
          
          if (option_rom == IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_SLOT)
            {
              uint8_t slot;
              
              slot = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_SLOT_BITMASK);
              slot >>= IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_SLOT_SHIFT;
              
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Device Slot %u",
                        slot);
            }
          else
            snprintf (tmpbuf,
                      tmpbuflen,
                      "Device Embedded");
          
          return (1);
        }

      /* From Dell Spec and Dell Code */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
               && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_FAILED_TO_PROGRAM_VIRTUAL_MAC_ADDRESS)
              || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
                  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
                  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CHIPSET_ERROR)))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Bus %u",
                    system_event_record_data->event_data3);
          
          return (1);
        }
    }

  /* achu: I don't know what motherboards this applies to */
#if 0
  /* OEM Interpretation
   *
   * From Dell Spec and Dell Code
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PRESENCE_DETECTED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CONFIGURATION_ERROR)
      && ctx->ipmi_version_major == IPMI_1_5_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_1_5_MINOR_VERSION)
    {
      if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_DIMM_NUMBER_VALID (system_event_record_data->event_data3))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "DIMM %c",
                    'A' + system_event_record_data->event_data3);
          
          return (1);
        }
    }
#endif

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sel_parse_output_dell_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * From Dell Spec
   *
   * Dell Poweredge 2900
   * Dell Poweredge 2950
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Register Value = %02Xh",
                system_event_record_data->event_data3);
      
      return (1);
    }
  
  return (0);
}

#if 0
static char *
_dell_version_change_entity_string (uint8_t data_entity)
{
  if (data_entity == 0)
    return "BIOS";
  else if (data_entity == 1)
    return "BMC";
  else if (data_entity == 2)
    return "iDRAC";
  else if (data_entity == 3)
    return "CMC";
  else if (data_entity == 4)
    return "NIC";
  else
    return "Unrecognized Entity";
}
#endif

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_dell_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if ((ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
       || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710))
    {
      /* From Dell Engineer and Dell Code */
      /* Note that the normal event_data3 event still occurs here, so need to output that too */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR
          && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
        {
          uint8_t event_data3_error_type;

          event_data3_error_type = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA3_OEM_DELL_OFFSET_CONFIGURATION_ERROR_ERROR_TYPE_BITMASK);
          event_data3_error_type >>= IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA3_OEM_DELL_OFFSET_CONFIGURATION_ERROR_ERROR_TYPE_SHIFT;
          
          if (event_data3_error_type == IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA3_OFFSET_CONFIGURATION_ERROR_ERROR_TYPE_POWER_SUPPLY_RATING_MISMATCH)
            {
              unsigned int watts2;
              unsigned int watts3;
              unsigned int watts;
              
              /* achu: that's not a typo, it's '+=' not a '|=', I'm just
               * copying Dell source at this point in time, don't know why
               * this is 
               */
              watts2 = system_event_record_data->event_data2 << IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA2_OEM_DELL_OFFSET_CONFIGURATION_ERROR_WATTS_SHIFT;
              watts3 = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA3_OEM_DELL_OFFSET_CONFIGURATION_ERROR_WATTS_BITMASK);
              watts3 >>= IPMI_SENSOR_TYPE_POWER_SUPPLY_EVENT_DATA3_OEM_DELL_OFFSET_CONFIGURATION_ERROR_WATTS_SHIFT;
              
              watts = watts2 + watts3;

              if (ipmi_sel_parse_string_snprintf (buf,
						  buflen,
						  wlen,
						  "Power Supply rating mismatch ; Power Supply %u Watts",
						  watts))
                (*oem_rv) = 1;
              else
                (*oem_rv) = 0;
              
              return (1);
            }
        }

      /* From Dell Spec and Dell Code
       *
       * Data2
       * [7:4] - 00h - 07h - Memory Card Number
       *       - 08h = 4 Dimms per Node
       *       - 09h = 6 Dimms per Node
       *       - 0Ah = 8 Dimms per Node
       *       - 0Bh = 9 Dimms per Node
       *       - 0Ch - 0Eh = reserved
       *       - 0Fh = No Card
       * [3:0] - 0h - 0Fh = Bitmask Increment in Data3
       *
       * Data3
       * [7:0] - 00h - FFh = DIMM bitmap
       *
       * e.g. Increment = 0
       *      DIMM bitmap = 00000001b = DIMM 1
       * e.g. Increment = 1
       *      DIMM bitmap = 00000001b = DIMM 9
       */
      if (((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
            && ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
                 && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
                     || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR
                     || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CRITICAL_OVERTEMPERATURE))
                || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED
                    && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_CORRECTABLE_MEMORY_ERROR_LOGGING_DISABLED)))
           || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
               && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
               && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_LOST)
           || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
               && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
               && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_CRITICAL_FROM_OK
                   || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_CRITICAL_FROM_LESS_SEVERE)))
          && ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
          && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION
          && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
        {
          char dimmstr[DELL_EVENT_BUFFER_LENGTH + 1];
          uint8_t memory_card;
          uint8_t dimm_counter = 0;
          unsigned int offset = 0;
          int len;

          memset (dimmstr, '\0', DELL_EVENT_BUFFER_LENGTH + 1);
          
          memory_card = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_BITMASK);
          memory_card >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_SHIFT;
          
          if (memory_card != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_NO_CARD)
            {
              dimm_counter = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_DIMM_COUNTER_BITMASK);
              dimm_counter >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_DIMM_COUNTER_SHIFT;
              dimm_counter *= 8;
            }
          
          if (memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_4_DIMMS_PER_NODE
              || memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_6_DIMMS_PER_NODE
              || memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_8_DIMMS_PER_NODE
              || memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_9_DIMMS_PER_NODE)
            {
              uint8_t dimms_per_node;
              int found = 0;
              int i;
              
              if (memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_4_DIMMS_PER_NODE)
                dimms_per_node = 4;
              else if (memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_6_DIMMS_PER_NODE)
                dimms_per_node = 6;
              else if (memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_8_DIMMS_PER_NODE)
                dimms_per_node = 8;
              else /* memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_9_DIMMS_PER_NODE */
                dimms_per_node = 9;
              
              /* achu:
               * 
               * DIMM locations can be thought of in this mapping, lets
               * say dimms per node is 4.
               *
               * Dimm # = Location
               * 1 = A1
               * 2 = A2
               * 3 = A3
               * 4 = A4
               * 5 = B1
               * 6 = B2
               * ...
               * 
               * lets say dimms per node is 9
               *
               * Dimm # = Location
               * 1 = A1
               * ...
               * 8 = A8
               * 9 = A9
               * 10 = B1
               * ...
               */
              
              for (i = 0; i < 8; i++)
                {
                  if (system_event_record_data->event_data3 & (0x1 << i))
                    {
                      uint8_t node;
                      uint8_t dimmnum;
                      
                      node = (dimm_counter + i) / dimms_per_node;
                      
                      dimmnum  = ((dimm_counter + i) % dimms_per_node) + 1;
                      
                      if (!found)
                        len = snprintf (dimmstr + offset,
                                        DELL_EVENT_BUFFER_LENGTH - offset,
                                        "DIMM %c%u",
                                        'A' + node,
                                        dimmnum);
                      else
                        len = snprintf (dimmstr + offset,
                                        DELL_EVENT_BUFFER_LENGTH - offset,
                                        ", DIMM %c%u",
                                        'A' + node,
                                        dimmnum);
                      
                      offset += len;
                      found++;

                      if (offset >= DELL_EVENT_BUFFER_LENGTH)
                        break;

                      break;
                    }
                }
              
              if (found)
                {
                  if (ipmi_sel_parse_string_snprintf (buf,
						      buflen,
						      wlen,
						      "%s",
						      dimmstr))
                    (*oem_rv) = 1;
                  else
                    (*oem_rv) = 0;
                  
                  return (1);
                }
            }
          else
            {
              int found = 0;
              int i;
              
              for (i = 0; i < 8; i++)
                {
                  if (system_event_record_data->event_data3 & (0x1 << i))
                    {
                      if (!found)
                        len = snprintf (dimmstr + offset,
                                        DELL_EVENT_BUFFER_LENGTH - offset,
                                        "DIMM %u",
                                        (dimm_counter + i + 1));
                      else
                        len = snprintf (dimmstr + offset,
                                        DELL_EVENT_BUFFER_LENGTH - offset,
                                        ", DIMM %u",
                                        (dimm_counter + i + 1));
                      
                      offset += len;

                      found++;

                      if (offset >= DELL_EVENT_BUFFER_LENGTH)
                        break;

                      break;
                    }
                }
              
              if (found)
                {
                  if (memory_card != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_NO_CARD)
                    {
                      if (ipmi_sel_parse_string_snprintf (buf,
							  buflen,
							  wlen,
							  "Memory Card %u, %s",
							  memory_card,
							  dimmstr))
                        (*oem_rv) = 1;
                      else
                        (*oem_rv) = 0;
                    }
                  else
                    {
                      if (ipmi_sel_parse_string_snprintf (buf,
							  buflen,
							  wlen,
							  "DIMM %s",
							  dimmstr))
                        (*oem_rv) = 1;
                      else
                        (*oem_rv) = 0;
                    }
                  return (1);
                }
              
            }
        }

      /* From Dell Spec and Dell Code
       *
       * Data2
       * [7:3] = Device Number
       * [2:0] = Function Number
       *
       * Data3
       * [7] = 0 = [6:0] contain a bus number
       *       1 = [6:0] contain a slot number
       * [6:0] = bus or slot number
       */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
               && ((system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
                    && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_PCI_PARITY_ERROR)
                   || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR
                   || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
              || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
                  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR_PCIE_ERROR)
              || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR
                  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR_FATAL_IO_ERROR))
          && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
        {
          uint8_t slot_flag;
          uint8_t bus_slot_number;
          
          /* Dell documentation says to watch out for this specific case */
          if (system_event_record_data->event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT
              && system_event_record_data->event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            return (0);
          
          slot_flag = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_FLAG_BITMASK);
          slot_flag >>= IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_FLAG_SHIFT;
          
          bus_slot_number = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_BITMASK);
          bus_slot_number >>= IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_SHIFT;
          
          if (slot_flag)
            {
              if (ipmi_sel_parse_string_snprintf (buf,
						  buflen,
						  wlen,
						  "Slot %u",
						  bus_slot_number))
                (*oem_rv) = 1;
              else
                (*oem_rv) = 0;
              
              return (1);
            }
          else
            {
              uint8_t device, function;
              
              device = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_BITMASK);
              device >>= IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_SHIFT;
              
              function = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_BITMASK);
              function >>= IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_SHIFT;
              
              if (ipmi_sel_parse_string_snprintf (buf,
						  buflen,
						  wlen,
						  "Bus %u, Device %u, Function %u",
						  bus_slot_number,
						  device,
						  function))
                (*oem_rv) = 1;
              else
                (*oem_rv) = 0;

              return (1);
            }
        }

      /* From Dell Spec */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_VERSION_CHANGE_HARDWARE_INCOMPATABILITY_DETECTED_WITH_ASSOCIATED_ENTITY
          && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
        {
          if (system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_VERSION_CHANGE_EVENT_DATA2_OEM_DELL_MANAGEMENT_CONTROLLER_FIRMWARE_REVISION)
            {
              if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_VERSION_CHANGE_EVENT_DATA3_OEM_DELL_OTHER)
                {
                  if (ipmi_sel_parse_string_snprintf (buf,
						      buflen,
						      wlen,
						      "Hardware Type = Other"))
                    (*oem_rv) = 1;
                  else
                    (*oem_rv) = 0;
                }
              else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_VERSION_CHANGE_EVENT_DATA3_OEM_DELL_CPU)
                {
                  if (ipmi_sel_parse_string_snprintf (buf,
						      buflen,
						      wlen,
						      "Hardware Type = CPU"))
                    (*oem_rv) = 1;
                  else
                    (*oem_rv) = 0;
                }
              else
                {
                  if (ipmi_sel_parse_string_snprintf (buf,
						      buflen,
						      wlen,
						      "Hardware Type = %02Xh",
						      system_event_record_data->event_data3))
                    (*oem_rv) = 1;
                  else
                    (*oem_rv) = 0;
                }
              
              return (1);
            }
        }

      /* From Dell Spec and Dell Code
       * 
       * Data2
       * [7:6] - blade format
       *         0 - single height (e.g. Mezz B, Mezz C)
       *         1 - double height (e.g. Mezz B1, Mezz B2, Mezz C1, Mezz C2)
       *         2 - double height, double width (e.g. Mezz B1, Mezz B2, Mezz C1, Mezz C2)
       * [5:0] - reserved
       *
       * Data3
       * [7] - 0 = device with option ROM is embedded, 1 = device with option ROM is in a slot
       * [6:0] - slot number where option ROM is located
       */
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_DEVICE_OPTION_ROM_FAILED_TO_SUPPORT_LINK_TUNING_OR_FLEX_ADDRESS
          && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
        {
          uint8_t option_rom;
          
          option_rom = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_BITMASK);
          option_rom >>= IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_SHIFT;
          
          if (option_rom == IPMI_OEM_DELL_EVENT_DATA3_OPTION_ROM_SLOT)
            {
              uint8_t blade_format;
              uint8_t slots_per_node;
              uint8_t slot_number;
              char mezzanine;
              
              blade_format = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_EVENT_DATA2_BLADE_FORMAT_BITMASK);
              blade_format >>= IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_EVENT_DATA2_BLADE_FORMAT_SHIFT;
              
              if (blade_format == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_EVENT_DATA2_BLADE_FORMAT_SINGLE_HEIGHT)
                slots_per_node = 1;
              else if (blade_format == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_EVENT_DATA2_BLADE_FORMAT_DOUBLE_HEIGHT)
                slots_per_node = 2;
              else if (blade_format == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_EVENT_DATA2_BLADE_FORMAT_DOUBLE_HEIGHT_DOUBLE_WEIGHT)
                slots_per_node = 4;
              else
                return (0);
              
              slot_number = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_BITMASK);
              slot_number >>= IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_SHIFT;
              
              /* Comments in Dell code refer to 
               *
               * "Odd number is B"
               * "Even number is C"
               */
              if (slot_number % 2)
                mezzanine = 'B';
              else
                mezzanine = 'C';
              
              if (slots_per_node >= 2)
                {
                  uint8_t slot_position;
                  
                  /* need slot number zero based for determining position */
                  
                  slot_position = ((slot_number - 1)/ slots_per_node) + 1;
                  
                  if (ipmi_sel_parse_string_snprintf (buf,
						      buflen,
						      wlen,
						      "Mezzanine %c%c",
						      mezzanine,
						      '0' + slot_position))
                    (*oem_rv) = 1;
                  else
                    (*oem_rv) = 0;
                }
              else
                {
                  if (ipmi_sel_parse_string_snprintf (buf,
						      buflen,
						      wlen,
						      "Mezzanine %c",
						      mezzanine))
                    (*oem_rv) = 1;
                  else
                    (*oem_rv) = 0;
                }
            }
          else
            {
              if (ipmi_sel_parse_string_snprintf (buf,
						  buflen,
						  wlen,
						  "Device Embedded"))
                (*oem_rv) = 1;
              else
                (*oem_rv) = 0;
            }
          
          return (1);
        }

    }

  /* achu: I don't know what motherboards this applies to */
#if 0

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   *
   * Specifically for Version Change Sensors with an event offset
   * IPMI_SENSOR_TYPE_VERSION_CHANGE_FIRMWARE_OR_SOFTWARE_INCOMPATABILITY_DETECTED_WITH_ASSOCIATED_ENTITY
   *
   * achu: XXX: dataX & 0x1F != 1F ???  The bitmasks below have never
   * been verified by Dell.
   */

#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_BITMASK 0xE0
#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_SHIFT      5

#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_BITMASK 0x1F
#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_SHIFT      0

#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_INVALID 0x1F

  if (ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_VERSION_CHANGE_FIRMWARE_OR_SOFTWARE_INCOMPATABILITY_DETECTED_WITH_ASSOCIATED_ENTITY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t data2_entity, data3_entity;
      uint8_t data2_number, data3_number;
      char *data2_entity_str = NULL;
      char *data3_entity_str = NULL;
      char data2_number_str[DELL_EVENT_BUFFER_LENGTH];
      char data3_number_str[DELL_EVENT_BUFFER_LENGTH];

      data2_entity = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_BITMASK);
      data2_entity >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_SHIFT;

      data2_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_BITMASK);
      data2_number >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_SHIFT;

      data3_entity = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_BITMASK);
      data3_entity >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_SHIFT;

      data3_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_BITMASK);
      data3_number >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_SHIFT;

      data2_entity_str = _dell_version_change_entity_string (data2_entity);
      data3_entity_str = _dell_version_change_entity_string (data3_entity);

      memset (data2_number_str, '\0', DELL_EVENT_BUFFER_LENGTH);
      memset (data3_number_str, '\0', DELL_EVENT_BUFFER_LENGTH);

      if (data2_number != IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_INVALID)
        snprintf (data2_number_str,
                  DELL_EVENT_BUFFER_LENGTH,
                  "%u",
                  data2_number);

      if (data3_number != IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_INVALID)
        snprintf (data3_number_str,
                  DELL_EVENT_BUFFER_LENGTH,
                  "%u",
                  data3_number);

      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "%s%s%s with %s%s%s",
                                          data2_entity_str,
                                          strlen (data2_number_str) ? " " : "",
                                          data2_number_str,
                                          data3_entity_str,
                                          strlen (data3_number_str) ? " " : "",
                                          data3_number_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
      return (1);     
    }
#endif

  return (0);
}
