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
#include "ipmi-sel-string-intel-s5500wb.h"
#include "ipmi-sel-string-intel-s2600jf.h"
#include "ipmi-sel-string-intel-s2600kp.h"
#include "ipmi-sel-string-intel-s2600wtt.h"
#include "ipmi-sel-string-intel-s2600wt2.h"
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
  
  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
     {
       if ((ret = sel_string_output_intel_s5500wb_sensor_name (ctx,
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
    }

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
       if ((ret = sel_string_output_intel_s2600jf_sensor_name (ctx,
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
    }

  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_sensor_name (ctx,
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
    }

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_sensor_name (ctx,
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
    }

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_sensor_name (ctx,
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

  /* 
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

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data1_class_oem (ctx,
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
    }

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data1_class_oem (ctx,
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
    }

  /* Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
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


  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID) 
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

  /* 
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data1_class_oem (ctx,
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
    }

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data1_class_oem (ctx,
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
    }

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data1_class_oem (ctx,
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
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data2_discrete_oem (ctx,
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
    }
  
  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID) 
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data2_discrete_oem (ctx,
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
    }
  
  /* 
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data2_discrete_oem (ctx,
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
    }

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data2_discrete_oem (ctx,
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
    }

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data2_discrete_oem (ctx,
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

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data2_class_oem (ctx,
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
    }

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data2_class_oem (ctx,
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
    }

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
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

  /* 
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data2_class_oem (ctx,
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
    }

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data2_class_oem (ctx,
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
    }

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data2_class_oem (ctx,
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
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data3_discrete_oem (ctx,
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
    }

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID) 
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data3_discrete_oem (ctx,
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
    }

  /* 
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
  
  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data3_discrete_oem (ctx,
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
    }

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data3_discrete_oem (ctx,
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
    }

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data3_discrete_oem (ctx,
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

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data3_class_oem (ctx,
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
    }

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data3_class_oem (ctx,
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
    }

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
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
  
  /* 
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data3_class_oem (ctx,
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
    }

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data3_class_oem (ctx,
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
    }

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data3_class_oem (ctx,
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

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data2_event_data3 (ctx,
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
    }

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID) 
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data2_event_data3 (ctx,
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
    }

  /* 
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data2_event_data3 (ctx,
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
    }

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data2_event_data3 (ctx,
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
    }

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data2_event_data3 (ctx,
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
