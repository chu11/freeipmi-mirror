/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

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
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/spec/ipmi-slave-address-oem-spec.h"
#include "freeipmi/util/ipmi-iana-enterprise-numbers-util.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-string.h"
#include "ipmi-sel-parse-string-intel.h"
#include "ipmi-sel-parse-trace.h"
#include "ipmi-sel-parse-util.h"

#include "freeipmi-portability.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_event_data1_class_oem (ipmi_sel_parse_ctx_t ctx,
                                                   struct ipmi_sel_parse_entry *sel_parse_entry,
                                                   uint8_t sel_record_type,
                                                   char *tmpbuf,
                                                   unsigned int tmpbuflen,
                                                   unsigned int flags,
                                                   unsigned int *wlen,
                                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
      && ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_event_data2_discrete_oem (ipmi_sel_parse_ctx_t ctx,
                                                      struct ipmi_sel_parse_entry *sel_parse_entry,
                                                      uint8_t sel_record_type,
                                                      char *tmpbuf,
                                                      unsigned int tmpbuflen,
                                                      unsigned int flags,
                                                      unsigned int *wlen,
                                                      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
      && ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_event_data2_class_oem (ipmi_sel_parse_ctx_t ctx,
                                                   struct ipmi_sel_parse_entry *sel_parse_entry,
                                                   uint8_t sel_record_type,
                                                   char *tmpbuf,
                                                   unsigned int tmpbuflen,
                                                   unsigned int flags,
                                                   unsigned int *wlen,
                                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
      && ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
	{
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Bus %u",
		    system_event_record_data->event_data2);
	  
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
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_event_data3_discrete_oem (ipmi_sel_parse_ctx_t ctx,
                                                      struct ipmi_sel_parse_entry *sel_parse_entry,
                                                      uint8_t sel_record_type,
                                                      char *tmpbuf,
                                                      unsigned int tmpbuflen,
                                                      unsigned int flags,
                                                      unsigned int *wlen,
                                                      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   *
   * From Bill Hannon @ Intel
   *
   * [7:3] = Device Number
   * [2:0] = Function Number
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
      && ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
    {
      uint8_t device, function;

      device = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_BITMASK);
      device >>= IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_SHIFT;

      function = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_BITMASK);
      function >>= IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Device %u, Function %u",
                device,
                function);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_event_data3_class_oem (ipmi_sel_parse_ctx_t ctx,
                                                   struct ipmi_sel_parse_entry *sel_parse_entry,
                                                   uint8_t sel_record_type,
                                                   char *tmpbuf,
                                                   unsigned int tmpbuflen,
                                                   unsigned int flags,
                                                   unsigned int *wlen,
                                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   *
   * From Bill Hannon @ Intel
   *
   * [7:3] = Device Number
   * [2:0] = Function Number
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
      && ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
      && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
          || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
    {
      uint8_t device, function;

      device = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_BITMASK);
      device >>= IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_SHIFT;

      function = (system_event_record_data->event_data3 & IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_BITMASK);
      function >>= IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Device %u, Function %u",
                device,
                function);
      
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
ipmi_sel_parse_output_intel_event_data2_event_data3 (ipmi_sel_parse_ctx_t ctx,
						     struct ipmi_sel_parse_entry *sel_parse_entry,
						     uint8_t sel_record_type,
						     char *buf,
						     unsigned int buflen,
						     unsigned int flags,
						     unsigned int *wlen,
						     struct ipmi_sel_system_event_record_data *system_event_record_data,
						     int *oem_rv)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
      && ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_BIOS_POST
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_ERROR)
    {
      uint16_t error_code;
      char *error_code_str = NULL;
          
      error_code = system_event_record_data->event_data2;
      error_code |= (system_event_record_data->event_data3 << 8);
          
      if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CMOS_DATE_TIME_NOT_SET)
	error_code_str = "CMOS date / time not set";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PASSWORD_CHECK_FAILED)
	error_code_str = "Password check failed";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_LOCKED_ERROR)
	error_code_str = "Keyboard component encountered a locked error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_STUCK_KEY_ERROR)
	error_code_str = "Keyboard component encountered a stuck key error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_FIXED_MEDIA_THE_SAS_RAID_FIRMWARE_CAN_NOT_RUN_PROPERLY)
	error_code_str = "Fixed Media The SAS RAID firmware can not run properly. The user should attempt to reflash the firmware";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR)
	error_code_str = "PCI component encountered a PERR error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT)
	error_code_str = "PCI resource conflict";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR)
	error_code_str = "PCI out of resources error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_CACHE_SIZE_MISMATCH_DETECTED)
	error_code_str = "Processor 0x cache size mismatch detected";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_STEPPING_MISMATCH)
	error_code_str = "Processor 0x stepping mismatch";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_MISMATCH_DETECTED)
	error_code_str = "Processor 0x family mismatch detected";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_INTEL_QPI_SPEED_MISMATCH)
	error_code_str = "Processor 0x Intel(R) QPI speed mismatch";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_MODEL_MISMATCH)
	error_code_str = "Processor 0x model mismatch";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_SPEEDS_MISMATCHED)
	error_code_str = "Processor 0x speeds mismatched";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_IS_NOT_SUPPORTED)
	error_code_str = "Processor 0x family is not supported";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_AND_CHIPSET_STEPPING_CONFIGURATION_IS_UNSUPPORTED)
	error_code_str = "Processor and chipset stepping configuration is unsupported";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CMOS_NVRAM_CONFIGURATION_CLEARED)
	error_code_str = "CMOS/NVRAM Configuration Cleared";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER)
	error_code_str = "Passwords cleared by jumper";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET)
	error_code_str = "Password clear Jumper is Set";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE)
	error_code_str = "Processor 01 unable to apply microcode update";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE)
	error_code_str = "Processor 02 unable to apply microcode update";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_MICROCODE_UPDATE_NOT_FOUND)
	error_code_str = "Processor 0x microcode update not found";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT)
	error_code_str = "Watchdog timer failed on last boot";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE)
	error_code_str = "OS boot watchdog timer failure";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST)
	error_code_str = "Baseboard management controller failed self-test";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND)
	error_code_str = "Baseboard management controller failed to respond";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE)
	error_code_str = "Baseboard management controller in update mode";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SENSOR_DATA_RECORD_EMPTY)
	error_code_str = "Sensor data record empty";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL)
	error_code_str = "System event log full";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE)
	error_code_str = "Memory component could not be configured in the selected RAS mode";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_POPULATION_ERROR)
	error_code_str = "DIMM Population Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CLTT_CONFIGURATION_FAILURE_ERROR)
	error_code_str = "CLTT Configuration Failure Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_A1 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_A2 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_B1 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_B2 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_C1 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_C2 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_D1 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_D2 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_E1 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_E2 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_F1 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_FAILED_SELF_TEST_BIST)
	error_code_str = "DIMM_F2 failed Self Test (BIST)";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_DISABLED)
	error_code_str = "DIMM_A1 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_DISABLED)
	error_code_str = "DIMM_A2 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_DISABLED)
	error_code_str = "DIMM_B1 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_DISABLED)
	error_code_str = "DIMM_B2 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_DISABLED)
	error_code_str = "DIMM_C1 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_DISABLED)
	error_code_str = "DIMM_C2 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_DISABLED)
	error_code_str = "DIMM_D1 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_DISABLED)
	error_code_str = "DIMM_D2 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_DISABLED)
	error_code_str = "DIMM_E1 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_DISABLED)
	error_code_str = "DIMM_E2 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_DISABLED)
	error_code_str = "DIMM_F1 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_DISABLED)
	error_code_str = "DIMM_F2 Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_A1 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_A2 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_B1 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_B2 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_C1 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_C2 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_D1 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_D2 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_E1 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_E2 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_F1 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR)
	error_code_str = "DIMM_F2 Component encountered a Serial Presence Detection (SPD) fail error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_A1 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_A2 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_B1 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_B2 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_C1 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_C2 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_D1 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_D2 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_E1 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_E2 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_F1 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED)
	error_code_str = "DIMM_F2 Uncorrectable ECC error encountered";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CHIPSET_RECLAIM_OF_NON_CRITICAL_VARIABLES_COMPLETE)
	error_code_str = "Chipset Reclaim of non critical variables complete";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_UNSPECIFIED_PROCESSOR_COMPONENT_HAS_ENCOUNTERED_A_NON_SPECIFIC_ERROR)
	error_code_str = "Unspecified processor component has encountered a non specific error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_WAS_NOT_DETECTED)
	error_code_str = "Keyboard component was not detected";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR)
	error_code_str = "Keyboard component encountered a controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_MOUSE_COMPONENT_WAS_NOT_DETECTED)
	error_code_str = "Mouse component was not detected";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_MOUSE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR)
	error_code_str = "Mouse component encountered a controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR)
	error_code_str = "Local Console component encountered a controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR)
	error_code_str = "Local Console component encountered an output error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR)
	error_code_str = "Local Console component encountered a resource conflict error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR)
	error_code_str = "Remote Console component encountered a controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR)
	error_code_str = "Remote Console component encountered an input error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR)
	error_code_str = "Remote Console component encountered an output error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_WAS_NOT_DETECTED)
	error_code_str = "Serial port component was not detected";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR)
	error_code_str = "Serial port component encountered a resource conflict error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_CONTROLLER_ERROR)
	error_code_str = "Serial Port controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR)
	error_code_str = "Serial Port component encountered an input error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR)
	error_code_str = "Serial Port component encountered an output error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR)
	error_code_str = "LPC component encountered a controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR)
	error_code_str = "LPC component encountered a resource conflict error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_ATA_ATPI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR)
	error_code_str = "ATA/ATPI component encountered a controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR)
	error_code_str = "PCI component encountered a controller error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_READ_ERROR)
	error_code_str = "PCI component encountered a read error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_WRITE_ERROR)
	error_code_str = "PCI component encountered a write error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_UNSPECIFIED_SOFTWARE_COMPONENT_ENCOUNTERED_A_START_ERROR)
	error_code_str = "Unspecified software component encountered a start error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PEI_CORE_COMPONENT_ENCOUNTERED_A_LOAD_ERROR)
	error_code_str = "PEI Core component encountered a load error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PEI_MODULE_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR)
	error_code_str = "PEI module component encountered a illegal software state error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_CORE_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR)
	error_code_str = "DXE core component encountered a illegal software state error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR)
	error_code_str = "DXE boot services driver component encountered a illegal software state error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_INVALID_CONFIGURATION)
	error_code_str = "DXE boot services driver component encountered invalid configuration";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SMM_DRIVER_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR)
	error_code_str = "SMM driver component encountered a illegal software state error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED)
	error_code_str = "TPM device not detected";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING)
	error_code_str = "TPM device missing or not responding";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_FAILURE)
	error_code_str = "TPM device failure";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST)
	error_code_str = "TPM device failed self test";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_MISMATCH_ERROR)
	error_code_str = "Processor component encountered a mismatch error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_LOW_VOLTAGE_ERROR)
	error_code_str = "Processor component encountered a low voltage error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_HIGH_VOLTAGE_ERROR)
	error_code_str = "Processor component encountered a high voltage error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR)
	error_code_str = "PCI component encountered a SERR error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_ATA_ATPI_ATA_BUS_SMART_NOT_SUPPORTED)
	error_code_str = "ATA/ATPI ATA bus SMART not supported";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_ATA_ATPI_ATA_SMART_IS_DISABLED)
	error_code_str = "ATA/ATPI ATA SMART is disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR)
	error_code_str = "PCI Express component encountered a PERR error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR)
	error_code_str = "PCI Express component encountered a SERR error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_EXPRESS_IBIST_ERROR)
	error_code_str = "PCI Express IBIST error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM)
	error_code_str = "DXE boot services driver Not enough memory available to shadow a legacy option ROM";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_UNRECOGNIZED)
	error_code_str = "DXE boot services driver Unrecognized";
      else
	error_code_str = "Undefined Post Error";

      if (ipmi_sel_parse_string_snprintf (buf,
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
