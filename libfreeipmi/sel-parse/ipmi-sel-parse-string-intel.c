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
#include "ipmi-sel-parse-string-intel-node-manager.h"
#include "ipmi-sel-parse-trace.h"
#include "ipmi-sel-parse-util.h"

#include "freeipmi-portability.h"

#define INTEL_EVENT_BUFFER_LENGTH 4096

int
ipmi_sel_parse_output_intel_sensor_name (ipmi_sel_parse_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
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
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      int nmret;

      if ((nmret = ipmi_sel_parse_output_intel_node_manager_sensor_name (ctx,
									 sel_parse_entry,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
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
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
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
    }

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      int nmret;

      if ((nmret = ipmi_sel_parse_output_intel_node_manager_event_data1_class_oem (ctx,
                                                                                   sel_parse_entry,
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
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_CORRECTABLE_ERROR
	  && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_CRC_ERROR_PERSISTENT)
	{
	  char *event_msg_str = NULL;
	  
	  if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_RECOVERABLE_ERROR)
	    event_msg_str = "Persistent Recoverable Error";
	  else if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_PARITY_ALERT)
	    event_msg_str = "Persistent Parity Alert";
	  else if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_PERSISTENT_PARITY_STATUS)
	    event_msg_str = "Persistent Parity Status";
	  else if (system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_QUANTA_QSSC_S4R_SPECIFIC_CORRECTABLE_MEMORY_ERROR_SMI_LINK_LANE_FAIL_OVER_EVENT)
	    event_msg_str = "SMI Link Lane Fail Over (LFO) Event";
	    
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
    }

  return (0);
}

static void
_ipmi_sel_parse_output_intel_bus (ipmi_sel_parse_ctx_t ctx,
				  char *tmpbuf,
				  unsigned int tmpbuflen,
				  unsigned int flags,
				  struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  
  snprintf (tmpbuf,
	    tmpbuflen,
	    "Bus %u",
	    system_event_record_data->event_data2);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
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
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
	  _ipmi_sel_parse_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
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
	  
	  if (event_special_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_INVALID_INFORMATION)
	    event_special_code_str = "Invalid Information"; 
	  else if (event_special_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_BOARD_HOT_REPLACED_WITH_MISMATCHED_OR_FAULTY_MEMORY)
	    event_special_code_str = "Memory Board hot-replaced with mismatched or faulty memory";
	  else if (event_special_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_HOT_PLUG_GENERIC_INITIALIZATION_ERROR)
	    event_special_code_str = "Memory Hot-plug generic initialization error";
	  else if (event_special_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_HOT_PLUG_TIMEOUT)
	    event_special_code_str = "Memory Hot-plug Timeout";
	  else if (event_special_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_USER_INITIATED_CANCELATION)
	    event_special_code_str = "User-initiated cancelation";
	  else
	    event_special_code_str = "Unknown";

	  if (event_special_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_EVENT_SPECIAL_CODE_MEMORY_HOT_PLUG_GENERIC_INITIALIZATION_ERROR)
	    {
	      if (error_sub_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_MEMORY_BIST_ERROR)
		error_sub_code_str = "Memory BIST Error";
	      else if (error_sub_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_SPD_ERROR)
		error_sub_code_str = "SPD Error";
	      else if (error_sub_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_CLTT_CONFIGURATION_ERROR)
		error_sub_code_str = "CLTT Configuration Error";
	      else if (error_sub_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_POPULATION_RULE_ERROR)
		error_sub_code_str = "Population Rule Error";
	      else if (error_sub_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_MISMATCHED_DIMM_ERROR)
		error_sub_code_str = "Mismatched DIMM Error";
	      else if (error_sub_code == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_SUB_CODE_OTHER_MEMORY_INITIALIZATION_ERRORS)
		error_sub_code_str = "Other Memory Initialization Errors";
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

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
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
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
	{
	  _ipmi_sel_parse_output_intel_bus (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
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
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      int nmret;

      if ((nmret = ipmi_sel_parse_output_intel_node_manager_event_data2_class_oem (ctx,
                                                                                   sel_parse_entry,
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

  return (0);
}

static void
_ipmi_sel_parse_output_intel_device_function (ipmi_sel_parse_ctx_t ctx,
					      char *tmpbuf,
					      unsigned int tmpbuflen,
					      unsigned int flags,
					      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t device, function;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
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
_ipmi_sel_parse_output_intel_quanta_qssc_s4r_memory_board (ipmi_sel_parse_ctx_t ctx,
							   char *tmpbuf,
							   unsigned int tmpbuflen,
							   unsigned int flags,
							   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t memory_board;
  char *memory_board_str;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  
  memory_board = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_BITMASK);
  memory_board >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_SHIFT;
  
  if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM1_SLOT)
    memory_board_str = "MEM1_SLOT";
  else if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM2_SLOT)
    memory_board_str = "MEM2_SLOT";
  else if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM3_SLOT)
    memory_board_str = "MEM3_SLOT";
  else if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM4_SLOT)
    memory_board_str = "MEM4_SLOT";
  else if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM5_SLOT)
    memory_board_str = "MEM5_SLOT";
  else if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM6_SLOT)
    memory_board_str = "MEM6_SLOT";
  else if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM7_SLOT)
    memory_board_str = "MEM7_SLOT";
  else if (memory_board == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_MEM8_SLOT)
    memory_board_str = "MEM8_SLOT";
  else
    memory_board_str = "Unknown";

  snprintf (tmpbuf,
	    tmpbuflen,
	    "%s",
	    memory_board_str);
}

static char *
_ipmi_sel_parse_output_intel_quanta_qssc_s4r_dimm_slot_str (uint8_t dimm_slot)
{
  if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1B)
    return "DIMM_1/B";
  else if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1A)
    return "DIMM_1/A";
  else if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2B)
    return "DIMM_2/B";
  else if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2A)
    return "DIMM_2/A";
  else if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1D)
    return "DIMM_1/D";
  else if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_1C)
    return "DIMM_1/C";
  else if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2D)
    return "DIMM_2/D";
  else if (dimm_slot == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_2C)
    return "DIMM_2/C";
  else
    return "Unknown";
}

static void
_ipmi_sel_parse_output_intel_quanta_qssc_s4r_dimm_slot (ipmi_sel_parse_ctx_t ctx,
							char *tmpbuf,
							unsigned int tmpbuflen,
							unsigned int flags,
							struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t dimm_slot;
  char *dimm_slot_str;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  
  dimm_slot = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_BITMASK);
  dimm_slot >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_DIMM_SLOT_SHIFT;

  dimm_slot_str = _ipmi_sel_parse_output_intel_quanta_qssc_s4r_dimm_slot_str (dimm_slot);
 
  snprintf (tmpbuf,
	    tmpbuflen,
	    "%s",
	    dimm_slot_str);
}

static void
_ipmi_sel_parse_output_intel_quanta_qssc_s4r_smi_link (ipmi_sel_parse_ctx_t ctx,
							char *tmpbuf,
							unsigned int tmpbuflen,
							unsigned int flags,
							struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  uint8_t smi_link;
  char *smi_link_str;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  
  smi_link = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_BITMASK);
  smi_link >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_SHIFT;

  if (smi_link == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_0)
    smi_link_str = "SMI_LINK0";
  else if (smi_link == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_1)
    smi_link_str = "SMI_LINK1";
  else
    smi_link_str = "Unknown";
  
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
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
   * Quanta QSSC-S4R/Appro GB812X-CN
   * * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR
	  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
	  && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
	      || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR))
	{
	  _ipmi_sel_parse_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
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
	  char *channel_number_str;
	  char channel_number_char = 0;	/* remove warning */
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
	  else
	    channel_number_str = "Unknown";
	  
	  if (processor_socket_valid && channel_number_valid)
	    {
	      /* If we're on socket #2, the DIMMs jump from A-C, to D-F */
	      if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_2)
		channel_number_char += 3;
	    }

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
			"DIMM = %c%s",
			channel_number_char,
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

	  if (domain_instance_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_DOMAIN_INSTANCE_TYPE_LOCAL_MEMORY_MIRRORING_INTRA_SOCKET)
	    domain_instance_str = "Local memory mirroring";
	  else if (domain_instance_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_DOMAIN_INSTANCE_TYPE_GLOBAL_MEMORY_MIRRORING_INTER_SOCKET)
	    domain_instance_str = "Global memory mirroring";
	  else
	    domain_instance_str = "Unknown";

	  if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_1_2)
	    instance_id_str = "{MEM1_SLOT, MEM2_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_3_4)
	    instance_id_str = "{MEM3_SLOT, MEM4_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_5_6)
	    instance_id_str = "{MEM5_SLOT, MEM6_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_7_8)
	    instance_id_str = "{MEM7_SLOT, MEM8_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_1_4)
	    instance_id_str = "{MEM1_SLOT, MEM4_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_3_2)
	    instance_id_str = "{MEM3_SLOT, MEM2_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_5_8)
	    instance_id_str = "{MEM5_SLOT, MEM8_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_7_6)
	    instance_id_str = "{MEM7_SLOT, MEM6_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_1_3)
	    instance_id_str = "{MEM1_SLOT, MEM3_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_2_4)
	    instance_id_str = "{MEM2_SLOT, MEM4_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_5_7)
	    instance_id_str = "{MEM5_SLOT, MEM7_SLOT}";
	  else if (instance_id == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_MIRRORING_INSTANCE_ID_6_8)
	    instance_id_str = "{MEM6_SLOT, MEM8_SLOT}";
	  else
	    instance_id_str = "Unknown";
	  
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
	  
	  _ipmi_sel_parse_output_intel_quanta_qssc_s4r_memory_board (ctx,
								     memory_board_buf,
								     INTEL_EVENT_BUFFER_LENGTH,
								     flags,
								     system_event_record_data);
	  
	  _ipmi_sel_parse_output_intel_quanta_qssc_s4r_dimm_slot (ctx,
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
	  
	  _ipmi_sel_parse_output_intel_quanta_qssc_s4r_memory_board (ctx,
								     memory_board_buf,
								     INTEL_EVENT_BUFFER_LENGTH,
								     flags,
								     system_event_record_data);

	  _ipmi_sel_parse_output_intel_quanta_qssc_s4r_smi_link (ctx,
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

	  _ipmi_sel_parse_output_intel_quanta_qssc_s4r_memory_board (ctx,
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
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
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
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
	  && ((system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR
	       && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR)
	      || (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
		  && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR)))
	{
	  _ipmi_sel_parse_output_intel_device_function (ctx, tmpbuf, tmpbuflen, flags, system_event_record_data);
	  
	  return (1);
	}
    }

  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      int nmret;

      if ((nmret = ipmi_sel_parse_output_intel_node_manager_event_data3_class_oem (ctx,
                                                                                   sel_parse_entry,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
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
	    error_code_str = "Password clear jumper is set";
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
	  char *channel_number_str;
	  char channel_number_char = 0;	/* remove warning */
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
	      else
		channel_number_str = "Unknown";
	    }
	  else
	    channel_number_str = "Unknown";

	  if (processor_socket_valid && channel_number_valid)
            {
              /* If we're on socket #2, the DIMMs jump from A-C, to D-F */
              if (processor_socket == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_PROCESSOR_SOCKET_2)
                channel_number_char += 3;
            }

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
	      if (ipmi_sel_parse_string_snprintf (buf,
						  buflen,
						  wlen,
						  "Error Type = %s, DIMM = %c%s",
						  error_type_str,
						  channel_number_char,
						  dimm_slot_id_str))
		(*oem_rv) = 1;
	      else
		(*oem_rv) = 0;
	    }
	  else
	    {
	      if (ipmi_sel_parse_string_snprintf (buf,
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

		  if (mirroring_domain_local_subinstance == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_0_1)
		    mirroring_domain_local_subinstance_str = "{Ch0, Ch1}";
		  else if (mirroring_domain_local_subinstance == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_0_2)
		    mirroring_domain_local_subinstance_str = "{Ch0, Ch2}";
		  else if (mirroring_domain_local_subinstance == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_LOCAL_MIRRORING_DOMAIN_LOCAL_SUBINSTANCE_CHANNEL_1_2)
		    mirroring_domain_local_subinstance_str = "{Ch1, Ch2}";
		  else
		    mirroring_domain_local_subinstance_str = "Unknown";

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

	      if (ipmi_sel_parse_string_snprintf (buf,
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

	      if (ipmi_sel_parse_string_snprintf (buf,
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

	      if (sparing_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_LOCAL_MEMORY_SPARING_SPARING_TYPE_DIMM_SPARING)
		sparing_type_str = "DIMM Sparing";
	      else if (sparing_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_SPARING_DOMAIN_INSTANCE_TYPE_LOCAL_MEMORY_SPARING_SPARING_TYPE_RANK_SPARING)
		sparing_type_str = "Rank Sparing";
	      else
		sparing_type_str = "Unknown";

	      snprintf (sparing_type_buf,
			INTEL_EVENT_BUFFER_LENGTH,
			", Sparing Type = %s",
			sparing_type_str);

	      if (spared_dimm_information == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_1B_LOCKSTEP_DIMM_1D)
		spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
	      else if (spared_dimm_information == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_1A_LOCKSTEP_DIMM_1C)
		spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
	      else if (spared_dimm_information == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_2B_LOCKSTEP_DIMM_2D)
		spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
	      else if (spared_dimm_information == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INTEL_QUANTA_QSSC_S4R_SPARED_DIMM_INFORMATION_LOCAL_SPARING_DIMM_2A_LOCKSTEP_DIMM_2C)
		spared_dimm_information_str = "DIMM_1B lock steep with DIMM 1D";
	      else
		spared_dimm_information_str = "Unknown";

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

	  if (ipmi_sel_parse_string_snprintf (buf,
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

	  if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_MIRROR)
	    error_type_str = "Mirror";
	  else if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_SPARE)
	    error_type_str = "Spare";
	  else if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_INTERLEAVE)
	    error_type_str = "Interleave";
	  else if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_HEMISPHERE)
	    error_type_str = "Hemisphere";
	  else if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_POPULATION)
	    error_type_str = "Population";
	  else if (error_type == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INTEL_QUANTA_QSSC_S4R_ERROR_TYPE_DEVICE_MISMATCH)
	    error_type_str = "Device Mismatch";
	  else
	    error_type_str = "Unknown";

	  _ipmi_sel_parse_output_intel_quanta_qssc_s4r_memory_board (ctx,
								     memory_board_buf,
								     INTEL_EVENT_BUFFER_LENGTH,
								     flags,
								     system_event_record_data);
	  
	  /* Technically Intel docs do not say 0 vs. 1 for true vs. false.  Gotta guess */
	  if (dimm_slot_valid)
	    _ipmi_sel_parse_output_intel_quanta_qssc_s4r_dimm_slot (ctx,
								    dimm_slot_buf,
								    INTEL_EVENT_BUFFER_LENGTH,
								    flags,
								    system_event_record_data);

	  if (ipmi_sel_parse_string_snprintf (buf,
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
	  char error_code_buf[INTEL_EVENT_BUFFER_LENGTH + 1];
	  uint16_t error_code_type;

	  memset (error_code_buf, '\0', INTEL_EVENT_BUFFER_LENGTH + 1);

	  error_code = system_event_record_data->event_data2;
	  error_code |= (system_event_record_data->event_data3 << 8);

	  error_code_type = (error_code & IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_BITMASK);
	  error_code_type >>= IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_SHIFT;

	  if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_CMOS_DATE_TIME_NOT_SET)
	    error_code_str = "CMOS Date/Time not set";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PASSWORD_CHECK_FAILED)
	    error_code_str = "Password check failed";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_KEYBOARD_LOCKED_ERROR)
	    error_code_str = "Keyboard locked error";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_KEYBOARD_STUCK_KEY_ERROR)
	    error_code_str = "Keyboard stuck key error";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_THE_SAS_RAID_FIRMWARE_CANNOT_RUN_PROPERLY)
	    error_code_str = "The SAS RAID firmware can not run properly. The user should attempt to reflash the firmware";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PCI_PARITY_ERROR)
	    error_code_str = "PCI Parity Error (PERR)";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PCI_RESOURCE_CONFLICT_ERROR)
	    error_code_str = "PCI resource conflict error";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PCI_OUT_OF_RESOURCES_ERROR)
	    error_code_str = "PCI out of resources error";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_CACHE_SIZE_MISMATCH_DETECTED)
	    error_code_str = "Processor cache size mismatch detected";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_STEPPING_MISMATCH)
	    error_code_str = "Processor stepping mismatch";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_FAMILY_MISMATCH_DETECTED)
	    error_code_str = "Processor family mismatch detected";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_INTEL_QPI_SPEED_MISMATCH)
	    error_code_str = "Processor Intel(R) QPI speed mismatch";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_AND_CHIPSET_STEPPING_CONFIGURATION_IS_UNSUPPORTED)
	    error_code_str = "Processor and chipset stepping configuration is unsupported";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_CMOS_NVRAM_CONFIGURATION_CLEARED)
	    error_code_str = "CMOS/NVRAM configuration cleared";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PASSWORDS_CLEARED_BY_JUMPER)
	    error_code_str = "Passwords cleared by jumper";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PASSWORD_CLEAR_JUMPER_IS_SET)
	    error_code_str = "Password clear jumper is set";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_DISABLED)
 	    error_code_str = "Processor Disabled";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_FRB_3_TIMEOUT)
 	    error_code_str = "Processor FRB-3 timeout";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_01_UNABLE_TO_APPLY_MICROCODE_UPDATE)
	    error_code_str = "Processor 01 unable to apply microcode update";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_02_UNABLE_TO_APPLY_MICROCODE_UPDATE)
	    error_code_str = "Processor 02 unable to apply microcode update";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_03_UNABLE_TO_APPLY_MICROCODE_UPDATE)
	    error_code_str = "Processor 03 unable to apply microcode update";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_04_UNABLE_TO_APPLY_MICROCODE_UPDATE)
	    error_code_str = "Processor 04 unable to apply microcode update";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_BUILD_IN_SELF_TEST_FAILURE)
	    error_code_str = "Processor Build-In Self Test (BIST) failure";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_PROCESSOR_MICROCODE_UPDATE_NOT_FOUND)
	    error_code_str = "Processor microcode update not found";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_WATCHDOG_TIMER_FAILED_ON_LAST_BOOT)
	    error_code_str = "Watchdog timer failed on last boot";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_OS_BOOT_WATCHDOG_TIMER_FAILURE)
	    error_code_str = "OS boot watchdog timer failure";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_SELF_TEST)
	    error_code_str = "Baseboard Management Controller failed self-test";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_FAILED_TO_RESPOND)
	    error_code_str = "Baseboard Management Controller failed to respond";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_IN_UPDATE_MODE)
	    error_code_str = "Baseboard Management Controller in update mode";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_SENSOR_DATA_RECORD_EMPTY)
	    error_code_str = "Baseboard Management Controller Sensor Data Record empty";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_BASEBOARD_MANAGEMENT_CONTROLLER_SYSTEM_EVENT_LOG_FULL)
	    error_code_str = "Baseboard Management Controller System event log full";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_CHIPSET_RECLAIM_OF_NON_CRITICAL_VARIABLES_COMPLETE)
	    error_code_str = "Chipset Reclaim of non critical variables complete";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_NOT_DETECTED)
	    error_code_str = "TPM device not detected";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_MISSING_OR_NOT_RESPONDING)
	    error_code_str = "TPM device missing or not responding";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_FAILURE)
	    error_code_str = "TPM device failure";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TPM_DEVICE_FAILED_SELF_TEST)
	    error_code_str = "TPM device failed self test";
	  else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_MEMORY_WAS_NOT_CONFIGURED_FOR_THE_SELECTED_MEMORY_RAS_CONFIGURATION)
	    error_code_str = "Memory was not configured for the selected Memory RAS configuration";
	  else if (error_code_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY)
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
	      
	      if (memory_error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_INVALID_TYPE_ERROR)
		memory_error_code_str = "Memory invalid type error";
	      else if (memory_error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_DISABLED)
		memory_error_code_str = "Memory disabled";
	      else if (memory_error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_MISMATCH_ERROR)
		memory_error_code_str = "Memory mismatch error";
	      else if (memory_error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_TRAINING_ERROR)
		memory_error_code_str = "Memory Training Failed";
	      else if (memory_error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_TOO_MANY_DIMM_TYPES)
		memory_error_code_str = "Too many DIMM Types";
	      else if (memory_error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_MEMORY_BIST_FAILED)
		memory_error_code_str = "Memory BIST Failed";
	      else if (memory_error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_ERROR_CODE_SPD_FAILED)
		memory_error_code_str = "SPD Failed";
	      else
		memory_error_code_str = "Unknown Memory Failure";
	      
	      if (cpu_socket == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_1)
		cpu_socket_str = "CPU_1";
	      else if (cpu_socket == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_2)
		cpu_socket_str = "CPU_2";
	      else if (cpu_socket == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_3)
		cpu_socket_str = "CPU_3";
	      else if (cpu_socket == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INTEL_QUANTA_QSSC_S4R_POST_ERROR_CODE_TYPE_MEMORY_CPU_SOCKET_4)
		cpu_socket_str = "CPU_4";
	      else
		cpu_socket_str = "Unknown CPU Socket";
	      
	      dimm_slot_str = _ipmi_sel_parse_output_intel_quanta_qssc_s4r_dimm_slot_str (dimm_slot);
	      
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
    }

  return (0);
}
