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
#include "freeipmi/record-format/ipmi-sel-oem-record-format.h"
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
#include "ipmi-sel-string-intel.h"
#include "ipmi-sel-string-intel-node-manager.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define INTEL_EVENT_BUFFER_LENGTH 4096

int
sel_string_output_intel_sensor_name (ipmi_sel_ctx_t ctx,
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
  
  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      int nmret;

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
    }
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_CPU_SEL_STATUS)
	{
	  uint8_t sel_clear;
	  uint8_t sel_rollover;

	  sel_clear = (system_event_record_data->event_data1 & IPMI_SENSOR_TYPE_OEM_INTEL_SEL_CLEAR_BITMASK);
	  sel_clear >>= IPMI_SENSOR_TYPE_OEM_INTEL_SEL_CLEAR_SHIFT;

	  sel_rollover = (system_event_record_data->event_data1 & IPMI_SENSOR_TYPE_OEM_INTEL_SEL_ROLLOVER_BITMASK);
	  sel_rollover >>= IPMI_SENSOR_TYPE_OEM_INTEL_SEL_ROLLOVER_SHIFT;

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
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_NONE_OF_THE_ABOVE:
	      hsc_str = "Active status bits are waiting to be read by one or more status commands.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_CML_ERROR:
	      hsc_str = "An error was detected on the I2C/PMBus interface.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_VIN_UV_FAULT:
	      hsc_str = "An undervoltage input fault was detected on the UV pin.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_IOUT_OC_FAULT:
	      hsc_str = "The hot swap controller detected an overcurrent condition.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_HOTSWAP_OFF:
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
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_POWER_GOOD:
	      hsc_str = "The voltage on the FLB pin is below the required threshold.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_MFR_STATUS:
	      hsc_str = "There are one or more active status bits to be read by STATUS_MFR_SPECIFIC.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_INPUT_STATUS:
	      hsc_str = "There are one or more active status bits to be read by STATUS_INPUT.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_IOUT_STATUS:
	      hsc_str = "There are one or more active status bits to be read by STATUS_IOUT.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_VOUT_STATUS:
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
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_IOUT_WARN2:
	      hsc_str = "An undercurrent or overcurrent condition on the output supply detected.";
	      break;
	      /* achu: HS_SHUTDOWN_CAUSE1 & HS_SHUTDOWN_CAUSE2 list 4 error messages
	       * with <00>, <01>, <10>, & <11> listed next to them.  I have no idea
	       * where these other bits come from.
	       *
	       * So all user gets is a generic "hotswap shutdown"
	       */
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_SHUTDOWN_CAUSE1:
	      hsc_str = "Hotswap shutdown";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_SHUTDOWN_CAUSE2:
	      hsc_str = "Hotswap shutdown";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_INLIM:
	      hsc_str = "The ADM1276 has actively limited current into the load.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_OV_CMP_OUT:
	      hsc_str = "Input Voltage to OV pin is above threshold.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_UV_CMP_OUT:
	      hsc_str = "Input voltage to UV pin is below threshold.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_FET_HEALTH_BAD:
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
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_PIN_OP_WARN:
	      hsc_str = "An overpower condition on the input supply was detected by power monitor.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_UV_FAULT:
	      hsc_str = "An undervoltage was detected on the UV pin.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_UV_WARN:
	      hsc_str = "An undervoltage condition on the input supply was detected by the power monitor.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_OV_WARN:
	      hsc_str = "An overvoltage condition on the input supply was detected by hte power monitor.";
	      break;
	    case IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_OV_FAULT:
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
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
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
    }

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      int nmret;

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
    }


  /* OEM Interpretation
   *
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR)))
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
    }

  /* OEM Interpretation
   *
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	       && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR
		    && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR)
		   || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2
		       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2)
		   || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR
		       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR)
		   || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR
		       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_OPI_FATAL_ERROR)
		   || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR_2
		       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_OPI_FATAL_ERROR_2)))
	      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_POST
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_QPI_LINK_WIDTH_REDUCED
		  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_QPI_LINK_WIDTH_REDUCED)))
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

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_QPI_CORRECTABLE_ERRORS
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_QPI_CORRECTABLE_ERRORS)
	{
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "QPI Correctable Errors Event = %02Xh",
		    system_event_record_data->offset_from_event_reading_type_code);
	  
	  return (1);
	}
      
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_CHIPSET_PROPRIETARY
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_CHIPSET_PROPRIETARY)
	{
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Chipset Proprietary Event = %02Xh",
		    system_event_record_data->offset_from_event_reading_type_code);
	  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_ERROR_EXTENSION
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_MEMORY_ERROR_EXTENSION)
	{
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Memory Error Extension Event = %02Xh",
		    system_event_record_data->offset_from_event_reading_type_code);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
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
    }

  return (0);
}

static void
_sel_string_output_intel_bus (ipmi_sel_ctx_t ctx,
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

static void
_sel_string_output_intel_pci_bus (ipmi_sel_ctx_t ctx,
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
_sel_string_output_intel_s2600jf_ras_mode (uint8_t event_data) 
{
  uint8_t ras_mode;
  char *ras_mode_str;

  ras_mode = (event_data & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600JF_RAS_MODE_BITMASK);
  ras_mode >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600JF_RAS_MODE_SHIFT;
	  
  switch (ras_mode)
    {
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600JF_RAS_MODE_NONE:
      ras_mode_str = "None"; 
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600JF_RAS_MODE_MIRRORING:
      ras_mode_str = "Mirroring";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600JF_RAS_MODE_LOCKSTEP:
      ras_mode_str = "Lockstep";
      break;
    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OR_EVENT_DATA3_OEM_INTEL_S2600JF_RAS_MODE_RANK_SPARING:
      ras_mode_str = "Rank Sparing";
      break;
    default:
      ras_mode_str = "Unknown";
    }

  return (ras_mode_str);
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
sel_string_output_intel_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
 	  _sel_string_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
      
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
	{
	  uint8_t logical_rank;
	  
	  logical_rank = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOGICAL_RANK_BITMASK);
	  logical_rank >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOGICAL_RANK_SHIFT;
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Logical Rank %u",
		    logical_rank);
	  
	  return (1);
	}
    }
  
  /* OEM Interpretation
   *
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCI_SENSOR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
	  _sel_string_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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
    }

  /* OEM Interpretation
   *
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MIRRORING_REDUNDANCY_STATE
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
	{
	  uint8_t mirroring_domain_channel;
	  uint8_t dimm_rank_number;
	  
	  mirroring_domain_channel = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_MIRRORING_DOMAIN_CHANNEL_BITMASK);
	  mirroring_domain_channel >>= IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_MIRRORING_DOMAIN_CHANNEL_SHIFT;
	  
	  dimm_rank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_DIMM_RANK_NUMBER_BITMASK);
	  dimm_rank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_DIMM_RANK_NUMBER_SHIFT;
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Mirroring Domain Channel Pair for Socket = %u, DIMM Rank Number = %u",
		    mirroring_domain_channel,
		    dimm_rank_number);
	  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_POST
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_RAS_CONFIGURATION_STATUS
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
	{
	  uint8_t config_error;
	  char *config_error_str;

	  config_error = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600JF_CONFIG_ERROR_BITMASK);
	  config_error >>= IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600JF_CONFIG_ERROR_SHIFT;
	  
	  switch (config_error)
	    {
	    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600JF_CONFIG_ERROR_NONE:
	      config_error_str = "None"; 
	      break;
	    case IPMI_SENSOR_MEMORY_DEVICE_ENABLED_EVENT_DATA2_OEM_INTEL_S2600JF_CONFIG_ERROR_INVALID_DIMM_CONFIG_FOR_RAS_MODE:
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

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_ECC_ERROR
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR))
	{
	  uint8_t dimm_rank_number;
	  
	  dimm_rank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_DIMM_RANK_NUMBER_BITMASK);
	  dimm_rank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_DIMM_RANK_NUMBER_SHIFT;
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "DIMM Rank Number = %u",
		    dimm_rank_number);
	  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_LEGACY_PCI_ERROR
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
	  _sel_string_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_SPARING_REDUNDANCY_STATE
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
	{
	  uint8_t sparing_domain_channel;
	  uint8_t dimm_rank_number;
	  char *sparing_domain_channel_str;

	  sparing_domain_channel = (system_event_record_data->event_data2 & IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_SPARING_DOMAIN_CHANNEL_BITMASK);
	  sparing_domain_channel >>= IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_SPARING_DOMAIN_CHANNEL_SHIFT;
	  
	  dimm_rank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_DIMM_RANK_NUMBER_BITMASK);
	  dimm_rank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_DIMM_RANK_NUMBER_SHIFT;
	  
	  switch (sparing_domain_channel)
	    {
	    case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_SPARING_DOMAIN_CHANNEL_A:
	      sparing_domain_channel_str = "A";
	      break;
	    case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_SPARING_DOMAIN_CHANNEL_B:
	      sparing_domain_channel_str = "B";
	      break;
	    case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_SPARING_DOMAIN_CHANNEL_C:
	      sparing_domain_channel_str = "C";
	      break;
	    case IPMI_SENSOR_MEMORY_REDUNDANCY_EVENT_DATA2_OEM_INTEL_S2600JF_SPARING_DOMAIN_CHANNEL_D:
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

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_POST
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_RAS_MODE_SELECT
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
	{
	  const char *ras_mode_str;
	  
	  ras_mode_str = _sel_string_output_intel_s2600jf_ras_mode (system_event_record_data->event_data2);
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Previous RAS Mode = %s",
		    ras_mode_str);
	  
	  return (1);
	}
    }
  
  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
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

	  device = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_DEVICE_NUMBER_BITMASK);
	  device >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_DEVICE_NUMBER_SHIFT;
	  
	  function = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_FUNCTION_NUMBER_BITMASK);
	  function >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_FUNCTION_NUMBER_SHIFT;
	  
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
    }

  /* OEM Interpretation
   *
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
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
 	  _sel_string_output_intel_pci_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
	{
	  _sel_string_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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

  /* OEM Interpretation
   *
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR)))
	{
	  _sel_string_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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
    }

  /* OEM Interpretation
   *
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR
	       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR)
	      || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2
		  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2)
	      || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR
		  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR)))
	{
	  _sel_string_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}

      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	       && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR
		    && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_OPI_FATAL_ERROR)
		   || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR_2
		       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_OPI_FATAL_ERROR_2)))
	      || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_POST
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_QPI_LINK_WIDTH_REDUCED
		  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_QPI_LINK_WIDTH_REDUCED)))
	{
	  uint8_t node_id;
	  char *node_id_str;
	  
	  node_id = system_event_record_data->event_data2;
	  
	  switch (node_id)
	    {
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600JF_NODE_ID_CPU_1:
	      node_id_str = "1";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600JF_NODE_ID_CPU_2:
	      node_id_str = "2";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600JF_NODE_ID_CPU_3:
	      node_id_str = "3";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_EVENT_DATA2_OEM_INTEL_S2600JF_NODE_ID_CPU_4:
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
    }

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      int nmret;

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
    }

  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
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
    }

  /* OEM Interpretation
   *
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
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
 	  _sel_string_output_intel_pci_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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

    }

  return (0);
}

static void
_sel_string_output_intel_device_function (ipmi_sel_ctx_t ctx,
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

  return (NULL);		/* NOT REACHED */
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

static void
_sel_string_output_intel_pci_device_function (ipmi_sel_ctx_t ctx,
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
sel_string_output_intel_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
	  _sel_string_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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

	  processor_socket = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_BITMASK);
	  processor_socket >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_SHIFT;

	  channel_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_NUMBER_BITMASK);
	  channel_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_NUMBER_SHIFT;

	  dimm_slot_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SLOT_ID_BITMASK);
	  dimm_slot_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SLOT_ID_SHIFT;

	  if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_1)
	    {
	      processor_socket_str = "1";
	      processor_socket_valid++;
	    }
	  else if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_2)
	    {
	      processor_socket_str = "2";
	      processor_socket_valid++;
	    }
	  else
	    processor_socket_str = "Unknown";

	  if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_A)
	    {
	      channel_number_char = 'A';
	      channel_number_valid++;
	    }
	  else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_B)
	    {
	      channel_number_char = 'B';
	      channel_number_valid++;
	    }
	  else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_C)
	    {
	      channel_number_char = 'C';
	      channel_number_valid++;
	    }

	  if (processor_socket_valid && channel_number_valid)
	    {
	      /* If we're on socket #2, the DIMMs jump from A-C, to D-F */
	      if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_2)
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

	  if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SOCKET_1)
	    {
	      dimm_slot_id_str = "1";
	      dimm_slot_id_valid++;
	    }
	  else if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SOCKET_2)
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
    }

  /* OEM Interpretation
   *
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCI_SENSOR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
	  _sel_string_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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
    }

  /* OEM Interpretation
   *
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	   && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
	   && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	   && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MIRRORING_REDUNDANCY_STATE
	       || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_SPARING_REDUNDANCY_STATE)
	   && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
	       || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_DEGRADED))
	  || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_ECC_ERROR
	      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
		  || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR)))
	{
	  uint8_t socket_id;
	  uint8_t channel;
	  uint8_t dimm;
	  char *socket_id_str;
	  char *channel_str;
	  char *dimm_str;

	  socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_BITMASK);
	  socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_SHIFT;
	  
	  channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_BITMASK);
	  channel >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_SHIFT;
  
	  dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_BITMASK);
	  dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_SHIFT;
	  
	  switch (socket_id)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_1:
	      socket_id_str = "1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_2:
	      socket_id_str = "2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_3:
	      socket_id_str = "3";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_4:
	      socket_id_str = "4";
	      break;
	    default:
	      socket_id_str = "Unknown";
	    }
	  
	  switch (channel)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_A:
	      channel_str = "A";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_B:
	      channel_str = "B";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_C:
	      channel_str = "C";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_D:
	      channel_str = "D";
	      break;
	    default:
	      channel_str = "Unknown";
	    }
	  
	  switch (dimm)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_1:
	      dimm_str = "1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_2:
	      dimm_str = "2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_3:
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

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_POST
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_RAS_CONFIGURATION_STATUS
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
	{
	  const char *ras_mode_str;

	  ras_mode_str = _sel_string_output_intel_s2600jf_ras_mode (system_event_record_data->event_data3);
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "RAS Mode = %s",
		    ras_mode_str);
	  
	  return (1);
	}
      
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_POST
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_RAS_MODE_SELECT
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_DISABLED
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_DEVICE_ENABLED_DEVICE_ENABLED))
	{
	  const char *ras_mode_str;
	  
	  ras_mode_str = _sel_string_output_intel_s2600jf_ras_mode (system_event_record_data->event_data3);
	  
	  snprintf (tmpbuf,
		    tmpbuflen,
		    "Selected RAS Mode = %s",
		    ras_mode_str);
	  
	  return (1);
	}
  
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_LEGACY_PCI_ERROR
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
	  _sel_string_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
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

    }
  
  /* OEM Interpretation
   *
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
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
	  _sel_string_output_intel_pci_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

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
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
	{
	  _sel_string_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR)))
	{
	  _sel_string_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR
	       && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR)
	      || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2
		  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2)
	      || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR
		  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR)))
	{
	  _sel_string_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      int nmret;

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
    }
  
  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
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
		    "Error ID = 0x%02X",
		    system_event_record_data->event_data2);

	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
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
	  _sel_string_output_intel_pci_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);

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
sel_string_output_intel_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
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
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CMOS_DATE_TIME_NOT_SET:
	      error_code_str = "CMOS date / time not set";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
	      error_code_str = "Password check failed";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_LOCKED_ERROR:
	      error_code_str = "Keyboard component encountered a locked error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_STUCK_KEY_ERROR:
	      error_code_str = "Keyboard component encountered a stuck key error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_FIXED_MEDIA_THE_SAS_RAID_FIRMWARE_CAN_NOT_RUN_PROPERLY:
	      error_code_str = "Fixed Media The SAS RAID firmware can not run properly. The user should attempt to reflash the firmware";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
	      error_code_str = "PCI component encountered a PERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT:
	      error_code_str = "PCI resource conflict";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
	      error_code_str = "PCI out of resources error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_CACHE_SIZE_MISMATCH_DETECTED:
	      error_code_str = "Processor 0x cache size mismatch detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_STEPPING_MISMATCH:
	      error_code_str = "Processor 0x stepping mismatch";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_MISMATCH_DETECTED:
	      error_code_str = "Processor 0x family mismatch detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_INTEL_QPI_SPEED_MISMATCH:
	      error_code_str = "Processor 0x Intel(R) QPI speed mismatch";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_MODEL_MISMATCH:
	      error_code_str = "Processor 0x model mismatch";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_SPEEDS_MISMATCHED:
	      error_code_str = "Processor 0x speeds mismatched";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_IS_NOT_SUPPORTED:
	      error_code_str = "Processor 0x family is not supported";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_AND_CHIPSET_STEPPING_CONFIGURATION_IS_UNSUPPORTED:
	      error_code_str = "Processor and chipset stepping configuration is unsupported";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CMOS_NVRAM_CONFIGURATION_CLEARED:
	      error_code_str = "CMOS/NVRAM Configuration Cleared";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
	      error_code_str = "Passwords cleared by jumper";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
	      error_code_str = "Password clear jumper is set";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
	      error_code_str = "Processor 01 unable to apply microcode update";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
	      error_code_str = "Processor 02 unable to apply microcode update";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_0X_MICROCODE_UPDATE_NOT_FOUND:
	      error_code_str = "Processor 0x microcode update not found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
	      error_code_str = "Watchdog timer failed on last boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
	      error_code_str = "OS boot watchdog timer failure";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
	      error_code_str = "Baseboard management controller failed self-test";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
	      error_code_str = "Baseboard management controller failed to respond";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
	      error_code_str = "Baseboard management controller in update mode";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SENSOR_DATA_RECORD_EMPTY:
	      error_code_str = "Sensor data record empty";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL:
	      error_code_str = "System event log full";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE:
	      error_code_str = "Memory component could not be configured in the selected RAS mode";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_POPULATION_ERROR:
	      error_code_str = "DIMM Population Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CLTT_CONFIGURATION_FAILURE_ERROR:
	      error_code_str = "CLTT Configuration Failure Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_A1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_A2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_B1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_B2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_C1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_C2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_D1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_D2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_E1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_E2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_F1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_F2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_DISABLED:
	      error_code_str = "DIMM_A1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_DISABLED:
	      error_code_str = "DIMM_A2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_DISABLED:
	      error_code_str = "DIMM_B1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_DISABLED:
	      error_code_str = "DIMM_B2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_DISABLED:
	      error_code_str = "DIMM_C1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_DISABLED:
	      error_code_str = "DIMM_C2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_DISABLED:
	      error_code_str = "DIMM_D1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_DISABLED:
	      error_code_str = "DIMM_D2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_DISABLED:
	      error_code_str = "DIMM_E1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_DISABLED:
	      error_code_str = "DIMM_E2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_DISABLED:
	      error_code_str = "DIMM_F1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_DISABLED:
	      error_code_str = "DIMM_F2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_A1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_A2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_B1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_B2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_C1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_C2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_D1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_D2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_E1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_E2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_F1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_F2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_A1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_A2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_A2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_B1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_B2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_B2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_C1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_C2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_C2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_D1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_D2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_D2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_E1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_E2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_E2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_F1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DIMM_F2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_F2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_CHIPSET_RECLAIM_OF_NON_CRITICAL_VARIABLES_COMPLETE:
	      error_code_str = "Chipset Reclaim of non critical variables complete";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_UNSPECIFIED_PROCESSOR_COMPONENT_HAS_ENCOUNTERED_A_NON_SPECIFIC_ERROR:
	      error_code_str = "Unspecified processor component has encountered a non specific error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_WAS_NOT_DETECTED:
	      error_code_str = "Keyboard component was not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Keyboard component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_MOUSE_COMPONENT_WAS_NOT_DETECTED:
	      error_code_str = "Mouse component was not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_MOUSE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Mouse component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Local Console component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
	      error_code_str = "Local Console component encountered an output error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
	      error_code_str = "Local Console component encountered a resource conflict error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Remote Console component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR:
	      error_code_str = "Remote Console component encountered an input error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
	      error_code_str = "Remote Console component encountered an output error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_WAS_NOT_DETECTED:
	      error_code_str = "Serial port component was not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
	      error_code_str = "Serial port component encountered a resource conflict error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_CONTROLLER_ERROR:
	      error_code_str = "Serial Port controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR:
	      error_code_str = "Serial Port component encountered an input error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
	      error_code_str = "Serial Port component encountered an output error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "LPC component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
	      error_code_str = "LPC component encountered a resource conflict error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_ATA_ATPI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "ATA/ATPI component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "PCI component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_READ_ERROR:
	      error_code_str = "PCI component encountered a read error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_WRITE_ERROR:
	      error_code_str = "PCI component encountered a write error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_UNSPECIFIED_SOFTWARE_COMPONENT_ENCOUNTERED_A_START_ERROR:
	      error_code_str = "Unspecified software component encountered a start error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PEI_CORE_COMPONENT_ENCOUNTERED_A_LOAD_ERROR:
	      error_code_str = "PEI Core component encountered a load error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PEI_MODULE_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "PEI module component encountered a illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_CORE_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "DXE core component encountered a illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "DXE boot services driver component encountered a illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_INVALID_CONFIGURATION:
	      error_code_str = "DXE boot services driver component encountered invalid configuration";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_SMM_DRIVER_COMPONENT_ENCOUNTERED_A_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "SMM driver component encountered a illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED:
	      error_code_str = "TPM device not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING:
	      error_code_str = "TPM device missing or not responding";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_FAILURE:
	      error_code_str = "TPM device failure";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST:
	      error_code_str = "TPM device failed self test";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_MISMATCH_ERROR:
	      error_code_str = "Processor component encountered a mismatch error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_LOW_VOLTAGE_ERROR:
	      error_code_str = "Processor component encountered a low voltage error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_HIGH_VOLTAGE_ERROR:
	      error_code_str = "Processor component encountered a high voltage error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
	      error_code_str = "PCI component encountered a SERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_ATA_ATPI_ATA_BUS_SMART_NOT_SUPPORTED:
	      error_code_str = "ATA/ATPI ATA bus SMART not supported";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_ATA_ATPI_ATA_SMART_IS_DISABLED:
	      error_code_str = "ATA/ATPI ATA SMART is disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
	      error_code_str = "PCI Express component encountered a PERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
	      error_code_str = "PCI Express component encountered a SERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_PCI_EXPRESS_IBIST_ERROR:
	      error_code_str = "PCI Express IBIST error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM:
	      error_code_str = "DXE boot services driver Not enough memory available to shadow a legacy option ROM";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_UNRECOGNIZED:
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
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_PARITY_ERROR
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
  
	  channel_information_validity = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_CHANNEL_INFORMATION_VALIDITY_BITMASK);
	  channel_information_validity >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_CHANNEL_INFORMATION_VALIDITY_SHIFT;
	  
	  dimm_information_validity = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_DIMM_INFORMATION_VALIDITY_BITMASK);
	  dimm_information_validity >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_DIMM_INFORMATION_VALIDITY_SHIFT;

	  error_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_ERROR_TYPE_BITMASK);
	  error_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_ERROR_TYPE_SHIFT;

	  processor_socket = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_BITMASK);
	  processor_socket >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_SHIFT;

	  channel_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_NUMBER_BITMASK);
	  channel_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_NUMBER_SHIFT;

	  dimm_slot_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SLOT_ID_BITMASK);
	  dimm_slot_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SLOT_ID_SHIFT;

	  if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_ERROR_TYPE_DATA_PARITY_ERROR)
	    error_type_str = "Data Parity Error";
	  else if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_ERROR_TYPE_ADDRESS_PARITY_ERROR)
	    error_type_str = "Address Parity Error";
	  else
	    error_type_str = "Unknown";

          if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_1)
            {
              processor_socket_str = "1";
              processor_socket_valid++;
            }
          else if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_2)
            {
              processor_socket_str = "2";
              processor_socket_valid++;
            }
          else
            processor_socket_str = "Unknown";

	  if (channel_information_validity == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_CHANNEL_INFORMATION_VALID)
	    {
	      if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_A)
		{
		  channel_number_char = 'A';
		  channel_number_valid++;
		}
	      else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_B)
		{
		  channel_number_char = 'B';
		  channel_number_valid++;
		}
	      else if (channel_number == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_CHANNEL_C)
		{
		  channel_number_char = 'C';
		  channel_number_valid++;
		}
	    }

	  if (processor_socket_valid && channel_number_valid)
            {
              /* If we're on socket #2, the DIMMs jump from A-C, to D-F */
              if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_2)
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

	  if (dimm_information_validity == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_DIMM_INFORMATION_VALID)
	    {
	      if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SOCKET_1)
		{
		  dimm_slot_id_str = "1";
		  dimm_slot_id_valid++;
		}
	      else if (dimm_slot_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DIMM_SOCKET_2)
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
	  && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATUS_INFORMATION_FOR_MEMORY_MIRRORING_MIRRORING_MODE
	      || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATUS_INFORMATION_FOR_MEMORY_MIRRORING_SPARING_MODE)
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_LOST)
	  && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
          && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
	{
	  uint8_t domain_instance_type;
	  uint8_t instance_id;

	  domain_instance_type = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DOMAIN_INSTANCE_TYPE_BITMASK);
	  domain_instance_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DOMAIN_INSTANCE_TYPE_SHIFT;

	  instance_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_INSTANCE_ID_BITMASK);
	  instance_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_INSTANCE_ID_SHIFT;
	  
	  if (domain_instance_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_DOMAIN_INSTANCE_TYPE_LOCAL)
	    {
	      uint8_t mirroring_domain_local_subinstance;
	      uint8_t socket_id;
	      char mirroring_domain_local_subinstance_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
	      char socket_id_buf[INTEL_EVENT_BUFFER_LENGTH + 1];

	      memset (mirroring_domain_local_subinstance_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);
	      memset (socket_id_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

	      mirroring_domain_local_subinstance = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_BITMASK);
	      mirroring_domain_local_subinstance >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_SHIFT;

	      socket_id = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_SOCKET_ID_BITMASK);
	      socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_SOCKET_ID_SHIFT;
	      
	      if (mirroring_domain_local_subinstance != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_UNUSED_FIELD)
		{
		  char *mirroring_domain_local_subinstance_str = NULL;

		  switch (mirroring_domain_local_subinstance)
		    {
		    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_0_1:
		      mirroring_domain_local_subinstance_str = "{Ch0, Ch1}";
		      break;
		    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_0_2:
		      mirroring_domain_local_subinstance_str = "{Ch0, Ch2}";
		      break;
		    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_1_2:
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
	      
	      if (socket_id != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_UNUSED_FIELD)
		{
		  if (socket_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_SOCKET_ID_APPLIES_TO_ALL_SOCKETS)
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

	      first_socket_id = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_GLOBAL_FIRST_SOCKET_ID_BITMASK);
	      first_socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_GLOBAL_FIRST_SOCKET_ID_SHIFT;

	      second_socket_id = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_GLOBAL_SECOND_SOCKET_ID_BITMASK);
	      second_socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_GLOBAL_SECOND_SOCKET_ID_SHIFT;

	      if (first_socket_id != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_UNUSED_FIELD)
		snprintf (first_socket_id_buf,
			  INTEL_EVENT_BUFFER_LENGTH,
			  ", First Socket ID = %u",
			  first_socket_id);

	      if (second_socket_id != IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_UNUSED_FIELD)
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
    }

  /* OEM Interpretation
   *
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
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
    }

  /* OEM Interpretation
   *
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_POST
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_BIOS_POST_ERROR
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
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_CMOS_DATE_TIME_NOT_SET:
	      error_code_str = "CMOS date / time not set";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PASSWORD_CHECK_FAILED:
	      error_code_str = "Password check failed";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_LOCKED_ERROR:
	      error_code_str = "Keyboard component encountered a locked error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_STUCK_KEY_ERROR:
	      error_code_str = "Keyboard component encountered a stuck key error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_FIXED_MEDIA_THE_SAS_RAID_FIRMWARE_CANNOT_RUN_PROPERLY:
	      error_code_str = "Fixed Media The SAS RAID firmware cannot run properly. The user should attempt to reflash the firmware";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
	      error_code_str = "PCI component encountered a PERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT:
	      error_code_str = "PCI resource conflict";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR:
	      error_code_str = "PCI out of resources error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_CACHE_SIZE_MISMATCH_DETECTED:
	      error_code_str = "Processor 0x cache size mismatch detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_STEPPING_MISMATCH:
	      error_code_str = "Processor 0x stepping mismatch";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_MISMATCH_DETECTED:
	      error_code_str = "Processor 0x family mismatch detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_INTEL_QPI_SPEED_MISMATCH:
	      error_code_str = "Processor 0x Intel(R) QPI speed mismatch";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_MODEL_MISMATCH:
	      error_code_str = "Processor 0x model mismatch";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_SPEEDS_MISMATCHED:
	      error_code_str = "Processor 0x speeds mismatched";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_FAMILY_IS_NOT_SUPPORTED:
	      error_code_str = "Processor 0x family is not supported";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_AND_CHIPSET_STEPPING_CONFIGURATION_IS_UNSUPPORTED:
	      error_code_str = "Processor and chipset stepping configuration is unsupported";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_CMOS_NVRAM_CONFIGURATION_CLEARED:
	      error_code_str = "CMOS/NVRAM Configuration Cleared";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER:
	      error_code_str = "Passwords cleared by jumper";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET:
	      error_code_str = "Password clear jumper is set";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE:
	      error_code_str = "Processor 01 unable to apply microcode update";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE:
	      error_code_str = "Processor 02 unable to apply microcode update";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_0X_MICROCODE_UPDATE_NOT_FOUND:
	      error_code_str = "Processor 0x microcode update not found";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT:
	      error_code_str = "Watchdog timer failed on last boot";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE:
	      error_code_str = "OS boot watchdog timer failure";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST:
	      error_code_str = "Baseboard management controller failed self-test";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND:
	      error_code_str = "Baseboard management controller failed to respond";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE:
	      error_code_str = "Baseboard management controller in update mode";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SENSOR_DATA_RECORD_EMPTY:
	      error_code_str = "Sensor data record empty";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SYSTEM_EVENT_LOG_FULL:
	      error_code_str = "System event log full";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_MEMORY_COMPONENT_COULD_NOT_BE_CONFIGURED_IN_THE_SELECTED_RAS_MODE:
	      error_code_str = "Memory component could not be configured in the selected RAS mode";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_POPULATION_ERROR:
	      error_code_str = "DIMM Population Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_CLTT_CONFIGURATION_FAILURE_ERROR:
	      error_code_str = "CLTT Configuration Failure Error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_A1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_A2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_B1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_B2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_C1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_C2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_D1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_D2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_E1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_E2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F1_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_F1 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F2_FAILED_SELF_TEST_BIST:
	      error_code_str = "DIMM_F2 failed Self Test (BIST)";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A1_DISABLED:
	      error_code_str = "DIMM_A1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A2_DISABLED:
	      error_code_str = "DIMM_A2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B1_DISABLED:
	      error_code_str = "DIMM_B1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B2_DISABLED:
	      error_code_str = "DIMM_B2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C1_DISABLED:
	      error_code_str = "DIMM_C1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C2_DISABLED:
	      error_code_str = "DIMM_C2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D1_DISABLED:
	      error_code_str = "DIMM_D1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D2_DISABLED:
	      error_code_str = "DIMM_D2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E1_DISABLED:
	      error_code_str = "DIMM_E1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E2_DISABLED:
	      error_code_str = "DIMM_E2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F1_DISABLED:
	      error_code_str = "DIMM_F1 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F2_DISABLED:
	      error_code_str = "DIMM_F2 Disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_A1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_A2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_B1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_B2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_C1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_C2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_D1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_D2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_E1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_E2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F1_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_F1 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F2_COMPONENT_ENCOUNTERED_A_SERIAL_PRESENCE_DETECTION_FAIL_ERROR:
	      error_code_str = "DIMM_F2 Component encountered a Serial Presence Detection (SPD) fail error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_A1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_A2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_A2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_B1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_B2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_B2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_C1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_C2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_C2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_D1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_D2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_D2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_E1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_E2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_E2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F1_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_F1 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DIMM_F2_UNCORRECTABLE_ECC_ERROR_ENCOUNTERED:
	      error_code_str = "DIMM_F2 Uncorrectable ECC error encountered";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_CHIPSET_RECLAIM_OF_NON_CRITICAL_VARIABLES_COMPLETE:
	      error_code_str = "Chipset Reclaim of non critical variables complete";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_UNSPECIFIED_PROCESSOR_COMPONENT_HAS_ENCOUNTERED_A_NONSPECIFIC_ERROR:
	      error_code_str = "Unspecified processor component has encountered a nonspecific error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_KEYBOARD_COMPONENT_WAS_NOT_DETECTED:
	      error_code_str = "Keyboard component was not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_KEYBOARD_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Keyboard component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_MOUSE_COMPONENT_WAS_NOT_DETECTED:
	      error_code_str = "Mouse component was not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_MOUSE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Mouse component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Local Console component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
	      error_code_str = "Local Console component encountered an output error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_LOCAL_CONSOLE_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
	      error_code_str = "Local Console component encountered a resource conflict error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "Remote Console component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR:
	      error_code_str = "Remote Console component encountered an input error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_REMOTE_CONSOLE_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
	      error_code_str = "Remote Console component encountered an output error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_WAS_NOT_DETECTED:
	      error_code_str = "Serial port component was not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
	      error_code_str = "Serial port component encountered a resource conflict error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SERIAL_PORT_CONTROLLER_ERROR:
	      error_code_str = "Serial Port controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_INPUT_ERROR:
	      error_code_str = "Serial Port component encountered an input error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SERIAL_PORT_COMPONENT_ENCOUNTERED_AN_OUTPUT_ERROR:
	      error_code_str = "Serial Port component encountered an output error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "LPC component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_LPC_COMPONENT_ENCOUNTERED_A_RESOURCE_CONFLICT_ERROR:
	      error_code_str = "LPC component encountered a resource conflict error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_ATA_ATPI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "ATA/ATPI component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_CONTROLLER_ERROR:
	      error_code_str = "PCI component encountered a controller error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_READ_ERROR:
	      error_code_str = "PCI component encountered a read error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_WRITE_ERROR:
	      error_code_str = "PCI component encountered a write error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_UNSPECIFIED_SOFTWARE_COMPONENT_ENCOUNTERED_A_START_ERROR:
	      error_code_str = "Unspecified software component encountered a start error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PEI_CORE_COMPONENT_ENCOUNTERED_A_LOAD_ERROR:
	      error_code_str = "PEI Core component encountered a load error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PEI_MODULE_COMPONENT_ENCOUNTERED_AN_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "PEI module component encountered an illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DXE_CORE_COMPONENT_ENCOUNTERED_AN_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "DXE core component encountered an illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_AN_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "DXE boot services driver component encountered an illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_COMPONENT_ENCOUNTERED_AN_INVALID_CONFIGURATION:
	      error_code_str = "DXE boot services driver component encountered an invalid configuration";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_SMM_DRIVER_COMPONENT_ENCOUNTERED_AN_ILLEGAL_SOFTWARE_STATE_ERROR:
	      error_code_str = "SMM driver component encountered an illegal software state error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED:
	      error_code_str = "TPM device not detected";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING:
	      error_code_str = "TPM device missing or not responding";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_TPM_DEVICE_FAILURE:
	      error_code_str = "TPM device failure";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST:
	      error_code_str = "TPM device failed self test";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_MISMATCH_ERROR:
	      error_code_str = "Processor component encountered a mismatch error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_LOW_VOLTAGE_ERROR:
	      error_code_str = "Processor component encountered a low voltage error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PROCESSOR_COMPONENT_ENCOUNTERED_A_HIGH_VOLTAGE_ERROR:
	      error_code_str = "Processor component encountered a high voltage error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
	      error_code_str = "PCI component encountered a SERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_ATA_ATPI_ATA_BUS_SMART_NOT_SUPPORTED:
	      error_code_str = "ATA/ATPI ATA bus SMART not supported";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_ATA_ATPI_ATA_SMART_IS_DISABLED:
	      error_code_str = "ATA/ATPI ATA SMART is disabled";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_PERR_ERROR:
	      error_code_str = "PCI Express component encountered a PERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_EXPRESS_COMPONENT_ENCOUNTERED_A_SERR_ERROR:
	      error_code_str = "PCI Express component encountered a SERR error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_PCI_EXPRESS_IBIST_ERROR:
	      error_code_str = "PCI Express IBIST error";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_NOT_ENOUGH_MEMORY_AVAILABLE_TO_SHADOW_A_LEGACY_OPTION_ROM:
	      error_code_str = "DXE boot services driver Not enough memory available to shadow a legacy option ROM";
	      break;
	    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_S2600JF_POST_ERROR_CODE_DXE_BOOT_SERVICES_DRIVER_UNRECOGNIZED:
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

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INTEL_S2600JF_BIOS_SMI_HANDLER
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_S2600JF_MEMORY_PARITY_ERROR
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

	  channel_valid = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_EVENT_DATA3_CHANNEL_VALID_BITMASK);
	  channel_valid >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_EVENT_DATA3_CHANNEL_VALID_SHIFT;

	  dimm_valid = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_EVENT_DATA3_DIMM_VALID_BITMASK);
	  dimm_valid >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_EVENT_DATA3_DIMM_VALID_SHIFT;

	  error_type = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_ERROR_TYPE_BITMASK);
	  error_type >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_ERROR_TYPE_SHIFT;

	  socket_id = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_BITMASK);
	  socket_id >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_SHIFT;
	  
	  channel = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_BITMASK);
	  channel >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_SHIFT;
  
	  dimm = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_BITMASK);
	  dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_SHIFT;
	  
	  switch (error_type)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_ERROR_TYPE_NOT_KNOWN:
	      error_type_str = "Not Known";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_S2600JF_ERROR_TYPE_ADDRESS_PARITY_ERROR:
	      error_type_str = "Address Parity Error";
	      break;
	    default:
	      error_type_str = "Unknown";
	    }

	  switch (socket_id)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_1:
	      socket_id_str = "1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_2:
	      socket_id_str = "2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_3:
	      socket_id_str = "3";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_SOCKET_ID_CPU_4:
	      socket_id_str = "4";
	      break;
	    default:
	      socket_id_str = "Unknown";
	    }
	  
	  switch (channel)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_A:
	      channel_str = "A";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_B:
	      channel_str = "B";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_C:
	      channel_str = "C";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_CHANNEL_D:
	      channel_str = "D";
	      break;
	    default:
	      channel_str = "Unknown";
	    }
	  
	  switch (dimm)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_1:
	      dimm_str = "1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_2:
	      dimm_str = "2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_S2600JF_DIMM_3:
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
    }

  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
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
    }

  /* OEM Interpretation
   *
   * Intel S2600KP
   * Intel S2600WT2
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
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
sel_string_output_intel_oem_record_data (ipmi_sel_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
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
    }

  return (0);
}
