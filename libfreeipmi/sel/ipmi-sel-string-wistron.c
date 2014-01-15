/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#include "freeipmi/sel/ipmi-sel.h"

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

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-wistron.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_wistron_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON);
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
   * Wistron / Dell Poweredge C6220
   */
  if (ctx->product_id == IPMI_WISTRON_PRODUCT_ID_C6220
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_WISTRON_IOH_CORE_ERROR)
    {
      int ret;

      ret = ipmi_get_oem_sensor_type_message (ctx->manufacturer_id,
                                              ctx->product_id,
                                              system_event_record_data->sensor_type,
					      system_event_record_data->sensor_number,
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
sel_string_output_wistron_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON);
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
   * Wistron / Dell Poweredge C6220
   */
  if (ctx->product_id == IPMI_WISTRON_PRODUCT_ID_C6220)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_PROCESSOR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_IERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_THERMAL_TRIP
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_FRB1_BIST_FAILURE
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_FRB2_HANG_IN_POST_FAILURE
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_FRB3_PROCESSOR_STARTUP_INITIALIZATION_FAILURE
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_PROCESSOR_AUTOMATICALLY_THROTTLED))
	{
	  uint8_t processor;
	  char *processor_str = NULL;
	  
	  processor = system_event_record_data->event_data2;
	  
	  switch (processor)
	    {
	    case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_WISTRON_PROCESSOR_1:
	      processor_str = "Processor 1";
	      break;
	    case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_WISTRON_PROCESSOR_2:
	      processor_str = "Processor 2";
	      break;
	    case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_WISTRON_PROCESSOR_3:
	      processor_str = "Processor 3";
	      break;
	    case IPMI_SENSOR_TYPE_PROCESSOR_EVENT_DATA2_OEM_WISTRON_PROCESSOR_4:
	      processor_str = "Processor 4";
	      break;
	    default:
	      processor_str = "Unspecified Processor";
	    }
	  
	  snprintf (tmpbuf, tmpbuflen, "%s", processor_str);
	  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_PCI_SENSOR_ID
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
	{
	  uint8_t device, function;
	  
	  device = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_WISTRON_EVENT_DATA2_DEVICE_NUMBER_BITMASK);
	  device >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_WISTRON_EVENT_DATA2_DEVICE_NUMBER_SHIFT;
	  
	  function = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_WISTRON_EVENT_DATA2_FUNCTION_NUMBER_BITMASK);
	  function >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_WISTRON_EVENT_DATA2_FUNCTION_NUMBER_SHIFT;
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Device %u, Function %u",
		    device,
		    function);
	
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_WISTRON_IOH_CORE_ERROR
	  && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_QPI_SENSOR_ID
	      || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_INT_SENSOR_ID)
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_IOH_CORE_ERROR_OEM_WISTRON_CORE
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_IOH_CORE_ERROR_OEM_WISTRON_NON_FATAL
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_IOH_CORE_ERROR_OEM_WISTRON_FATAL))
	{
	  snprintf (tmpbuf, tmpbuflen, "Local Error Bit = %02Xh", system_event_record_data->event_data2);
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_SB_SENSOR_ID
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR))
	{
	  uint8_t local_error_bit;
	  char *local_error_bit_str = NULL;
	
	  local_error_bit = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_BITMASK);
	  local_error_bit >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_SHIFT;
	
	  switch (local_error_bit)
	    {
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_HT_PERIODIC_CRC_ERROR:
	      local_error_bit_str = "HT Periodic CRC Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_HT_PROTOCOL_ERROR:
	      local_error_bit_str = "HT Protocol Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_HT_FLOW_CONTROL_BUFFER_OVERFLOW:
	      local_error_bit_str = "HT Flow-Control Buffer Overflow";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_HT_RESPONSE_ERROR:
	      local_error_bit_str = "HT Response Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_HT_PER_PACKET_CRC_ERROR:
	      local_error_bit_str = "HT Per-Packet CRC";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_HT_RETRY_COUNTER_ERROR:
	      local_error_bit_str = "HT Retry Counter Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_WISTRON_LOCAL_ERROR_BIT_HT_MCU_PARITY_ERROR:
	      local_error_bit_str = "MCU Parity Error";
	      break;
	    default:
	      local_error_bit_str = "Unspecified Local Error";
	    }
	
	  snprintf (tmpbuf, tmpbuflen, "%s", local_error_bit_str);
	
	  return (1);
	}
      
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_POST_END
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_SYSTEM_BOOT_EVENT)
	{
	  uint8_t boot_type;
	  char *boot_type_str = NULL;
	  uint8_t boot_device;
	  char *boot_device_str = NULL;
  
	  boot_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_TYPE_BITMASK);
	  boot_type >>= IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_TYPE_SHIFT;

	  boot_device = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_BITMASK);
	  boot_device >>= IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_SHIFT;

	  if (boot_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_TYPE_PC_COMPATIBLE_BOOT)
	    boot_type_str = "PC Compatible Boot";
	  else
	    boot_type_str = "uEFI Boot";

	  switch (boot_device)
	    {
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_FORCE_PXE_BOOT:
	      boot_device_str = "Force PXE Boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_NIC_PXE_BOOT:
	      boot_device_str = "NIC PXE Boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_HARD_DISK_BOOT:
	      boot_device_str = "Hard Disk Boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_RAID_HDD_BOOT:
	      boot_device_str = "RAID HDD Boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_USB_STORAGE_BOOT:
	      boot_device_str = "USB Storage Boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_CD_DVD_ROM_BOOT:
	      boot_device_str = "CD/DVD ROM Boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_ISCSI_BOOT:
	      boot_device_str = "iSCSI Boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_UEFI_SHELL:
	      boot_device_str = "uEFI Shell";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BOOT_DEVICE_EPSA_DIAGNOSTIC_BOOT:
	      boot_device_str = "ePSA Diagnostic";
	      break;
	    default:
	      boot_device_str = "Unspecified";
	    }
	
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Boot Type = %s, Boot Device = %s",
		    boot_type_str,
		    boot_device_str);
  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_BIOS_RECOVERY_FAIL
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_SYSTEM_BOOT_EVENT)
	{
	  uint8_t bios_recover_event;
	  char *bios_recover_event_str = NULL;
	
	  bios_recover_event = system_event_record_data->event_data2;
	
	  switch (bios_recover_event)
	    {
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_RECOVERY_START_RECOVERY:
	      bios_recover_event_str = "Start Recovery";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_RECOVERY_RECOVERY_SUCCESS:
	      bios_recover_event_str = "Recovery Status";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_RECOVERY_LOAD_IMAGE_FAIL:
	      bios_recover_event_str = "Load Image Fail";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_RECOVERY_SIGNED_FAIL:
	      bios_recover_event_str = "Signed Fail";
	      break;
	    default:
	      bios_recover_event_str = "Unspecified";
	    }
	
	  snprintf (tmpbuf, tmpbuflen, "BIOS Recovery Event = %s", bios_recover_event_str);
	
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_ME_FAIL
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_SYSTEM_BOOT_EVENT
	  && system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_ME_FAIL)
	{
	  snprintf (tmpbuf, tmpbuflen, "ME Fail");
	  return (1);
	}
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_wistron_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON);
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
   * Wistron / Dell Poweredge C6220
   */
  if (ctx->product_id == IPMI_WISTRON_PRODUCT_ID_C6220)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_PCI_SENSOR_ID
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
      {
	snprintf (tmpbuf,
		  tmpbuflen,
		  "Bus %u",
		  system_event_record_data->event_data3);
          
	return (1);
      }

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_WISTRON_IOH_CORE_ERROR
	  && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_QPI_SENSOR_ID
	      || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_INT_SENSOR_ID)
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_IOH_CORE_ERROR_OEM_WISTRON_CORE
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_IOH_CORE_ERROR_OEM_WISTRON_NON_FATAL
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_IOH_CORE_ERROR_OEM_WISTRON_FATAL))
 	{
	  uint8_t error;
	  char *error_str =  NULL;

	  error = system_event_record_data->event_data3;

	  switch (error)
	    {
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI0_ERROR:
	      error_str = "QPI[0] Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI1_ERROR:
	      error_str = "QPI[1] Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI2_ERROR:
	      error_str = "QPI[2] Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI3_ERROR:
	      error_str = "QPI[3] Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI0_PROTOCOL_ERROR:
	      error_str = "QPI[0] Protocol Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI1_PROTOCOL_ERROR:
	      error_str = "QPI[1] Protocol Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI2_PROTOCOL_ERROR:
	      error_str = "QPI[2] Protocol Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_QPI3_PROTOCOL_ERROR:
	      error_str = "QPI[3] Protocol Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_MISCELLANEOUS_ERROR:
	      error_str = "Miscellaneous Error";
	      break;
	    case IPMI_SENSOR_TYPE_IOH_CORE_ERROR_EVENT_DATA3_OEM_WISTRON_IOH_CORE_ERROR:
	      error_str = "IOH Core Error";
	      break;
	    default:
	      error_str = "Unspecified Error";
	    }
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "%s",
		    error_str);
	  
	  return (1);
	}
      
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
sel_string_output_wistron_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON);
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
   * Wistron / Dell Poweredge C6220
   */
  if (ctx->product_id == IPMI_WISTRON_PRODUCT_ID_C6220)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_MEMORY
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_MEMORY_SCRUB_FAILED
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_MEMORY_DEVICE_DISABLED
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_SPARE))
	{
	  uint8_t threshold;
	  char *threshold_str = NULL;
	  uint8_t cpudimm;
	  char *cpudimm_str = NULL;
	  uint8_t dimmnumber;
	  char *dimmnumber_str = NULL;

	  threshold = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_THRESHOLD_BITMASK);
	  threshold >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_THRESHOLD_SHIFT;

	  cpudimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_CPU_DIMM_BITMASK);
	  cpudimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_CPU_DIMM_SHIFT;

	  dimmnumber = system_event_record_data->event_data3;

	  switch (threshold)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_SBE_WARNING_THRESHOLD:
	      threshold_str = "SBE warning threshold";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_SBE_CRITICAL_THRESHOLD:
	      threshold_str = "SBE critical threshold";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_UNSPECIFIED:
	      threshold_str = "Unspecified";
	      break;
	    default:
	      threshold_str = "Unknown";
	    }

	  switch (cpudimm)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_CPU1_DIMM_A:
	      cpudimm_str = "CPU1 DIMM A";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_CPU2_DIMM_B:
	      cpudimm_str = "CPU2 DIMM B";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_CPU3_DIMM_C:
	      cpudimm_str = "CPU3 DIMM C";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_WISTRON_CPU4_DIMM_D:
	      cpudimm_str = "CPU4 DIMM D";
	      break;
	    default:
	      cpudimm_str = "Uknown CPU DIMM ";
	    }

	  switch (dimmnumber)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_1_BITMASK:
	      dimmnumber_str = "1 error event";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_2_BITMASK:
	      dimmnumber_str = "2 error event";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_3_BITMASK:
	      dimmnumber_str = "3 error event";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_4_BITMASK:
	      dimmnumber_str = "4 error event";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_5_BITMASK:
	      dimmnumber_str = "5 error event";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_6_BITMASK:
	      dimmnumber_str = "6 error event";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_7_BITMASK:
	      dimmnumber_str = "7 error event";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_WISTRON_DIMM_8_BITMASK:
	      dimmnumber_str = "8 error event";
	      break;
	    }

	  if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "Threshold = %s, %s%s",
				   threshold_str,
				   cpudimm_str,
				   dimmnumber_str))
	    (*oem_rv) = 1;
	  else
	    (*oem_rv) = 0;
  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_POST_START
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_EVENT_OEM_SYSTEM_BOOT_EVENT)
	{
	  uint8_t first_field;
	  uint8_t second_field;
	  uint8_t third_field;
	  uint8_t tmp;
  
	  first_field = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_FIRST_FIELD_BITMASK);
	  first_field >>= IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_FIRST_FIELD_SHIFT;

	  second_field = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_SECOND_FIELD_HIGH_BITS_BITMASK);
	  second_field >>= IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_SECOND_FIELD_HIGH_BITS_SHIFT;
	  second_field <<= IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA2_OEM_WISTRON_BIOS_SECOND_FIELD_HIGH_BITS_LEFT_SHIFT;

	  tmp = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA3_OEM_WISTRON_BIOS_SECOND_FIELD_LOW_BITS_BITMASK);
	  tmp >>= IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA3_OEM_WISTRON_BIOS_SECOND_FIELD_LOW_BITS_SHIFT;
	  second_field |= tmp;

	  third_field = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA3_OEM_WISTRON_BIOS_THIRD_FIELD_BITMASK);
	  third_field >>= IPMI_SENSOR_TYPE_SYSTEM_EVENT_EVENT_DATA3_OEM_WISTRON_BIOS_THIRD_FIELD_SHIFT;
  
	  if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "BIOS Version = %u.%u.%u",
				   first_field,
				   second_field,
				   third_field))
	    (*oem_rv) = 1;
	  else
	    (*oem_rv) = 0;
  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_WISTRON_BIOS 
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_WISTRON_POST_ERROR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_SYSTEM_FIRMWARE_ERROR)
	{
	  uint16_t error_code;
	  char *error_code_str = NULL;
          
	  error_code = system_event_record_data->event_data2;
	  error_code |= (system_event_record_data->event_data3 << 8);

	  switch (error_code)
	    {
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_LOCAL_CONSOLE_RESOURCE_CONFLICT:
	      error_code_str = "Local Console Resource Conflict";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_LOCAL_CONSOLE_CONTROLLER_ERROR:
	      error_code_str = "Local Console Controller Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_LOCAL_CONSOLE_OUTPUT_ERROR:
	      error_code_str = "Local Console Output Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_ISA_IO_CONTROLLER_ERROR:
	      error_code_str = "ISA IO Controller Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_ISA_IO_RESOURCE_CONFLICT:
	      error_code_str = "ISA IO Resource Conflict";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_ISA_IO_CONTROLLER_ERROR2:
	      error_code_str = "ISA IO Controller Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_ISA_FLOPPY_CONTROLLER_ERROR:
	      error_code_str = "ISA Floppy Controller Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_ISA_FLOPPY_INPUT_ERROR:
	      error_code_str = "ISA Floppy Input Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_ISA_FLOPPY_OUTPUT_ERROR:
	      error_code_str = "ISA Floppy Output Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_USB_READ_ERROR:
	      error_code_str = "USB Read Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_USB_WRITE_ERROR:
	      error_code_str = "USB Write Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_USB_INTERFACE_ERROR:
	      error_code_str = "USB Interface Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_MOUSE_INTERFACE_ERROR:
	      error_code_str = "Mouse Interface Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_KEYBOARD_NOT_DETECTED:
	      error_code_str = "Keyboard not Detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_KEYBOARD_CONTROLLER_ERROR:
	      error_code_str = "Keyboard Controller Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_KEYBOARD_STUCK_KEY_ERROR:
	      error_code_str = "Keyboard Stuck Key Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_KEYBOARD_LOCKED_ERROR:
	      error_code_str = "Keyboard Locked Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_MEMORY_CORRECTABLE_ERROR:
	      error_code_str = "Memory Correctable Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_MEMORY_UNCORRECTABLE_ERROR:
	      error_code_str = "Memory Uncorrectable Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_MEMORY_NON_SPECIFIC_ERROR:
	      error_code_str = "Memory Non-Specific Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_MP_SERVICE_SELF_TEST_ERROR:
	      error_code_str = "MP Service Self Test Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_PCI_IO_CONTROLLER_ERROR:
	      error_code_str = "PCI IO Controller Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_PCI_IO_READ_ERROR:
	      error_code_str = "PCI IO Read Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_PCI_IO_WRITE_ERROR:
	      error_code_str = "PCI IO Write Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SERIAL_PORT_NOT_DETECTED:
	      error_code_str = "Serial Port not Detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SERIAL_PORT_CONTROLLER_ERROR:
	      error_code_str = "Serial Port Controller Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SERIAL_PORT_INPUT_ERROR:
	      error_code_str = "Serial Port Input Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SERIAL_PORT_OUTPUT_ERROR:
	      error_code_str = "Serial Port Output Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_MICROCODE_UPDATE_ERROR:
	      error_code_str = "Microcode Update Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_NO_MICROCODE_UPDATED:
	      error_code_str = "No Microcode Updated";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SATA_0_DEVICE_NOT_FOUND:
	      error_code_str = "SATA 0 Device not Found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SATA_1_DEVICE_NOT_FOUND:
	      error_code_str = "SATA 1 Device not Found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SATA_2_DEVICE_NOT_FOUND:
	      error_code_str = "SATA 2 Device not Found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SATA_3_DEVICE_NOT_FOUND:
	      error_code_str = "SATA 3 Device not Found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SATA_4_DEVICE_NOT_FOUND:
	      error_code_str = "SATA 4 Device not Found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SATA_5_DEVICE_NOT_FOUND:
	      error_code_str = "SATA 5 Device not Found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SPARING_MODE_IS_NOT_CONFIGURED:
	      error_code_str = "Sparing Mode is not Configured";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_MIRROR_MODE_IS_NOT_CONFIGURED:
	      error_code_str = "Mirror Mode is not Configured";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_SUPERVISER_AND_USER_PASSWORDS_CLEARED:
	      error_code_str = "Superviser and User passwords have been cleared";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_WISTRON_POST_ERROR_CODE_CMOS_BATTERY_FAULT:
	      error_code_str = "CMOS Battery Fault";
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
    }

  return (0);
}
