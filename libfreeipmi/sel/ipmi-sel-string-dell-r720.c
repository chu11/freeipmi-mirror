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
#include "ipmi-sel-string-dell-common.h"
#include "ipmi-sel-string-dell-r720.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define DELL_EVENT_BUFFER_LENGTH 4096

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_r720_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);
  assert (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720);

  if ((ret = sel_string_output_dell_poweredge_event_data1_class_sensor_specific_discrete (ctx,
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
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_r720_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720);

  /* 
   * From Dell Code
   *
   * Dell Poweredge R720
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_FAILURE)
    {
      ret = ipmi_get_oem_generic_event_message (ctx->manufacturer_id,
                                                ctx->product_id,
                                                system_event_record_data->event_type_code,
                                                system_event_record_data->offset_from_event_reading_type_code,
                                                tmpbuf,
                                                tmpbuflen);

      if (ret > 0)
        return (1);
    }

  if ((ret = sel_string_output_dell_oem_diagnostic_event_data_common_event_data1_class_oem (ctx,
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

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_r720_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720);

  if ((ret = sel_string_output_dell_poweredge_event_data2_discrete_oem (ctx,
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

  /* From Dell Spec */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR_QPI_LINK_DEGRADE
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_QPI_LINK_ERROR_SENSOR)
    {
      uint8_t reporting_agent_id;
      uint8_t reporting_agent_link_id;
      uint8_t partner_agent_id;
      uint8_t partner_link_id;

      reporting_agent_id = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_REPORTING_AGENT_ID_BITMASK);
      reporting_agent_id >>= IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_REPORTING_AGENT_ID_SHIFT;

      reporting_agent_link_id = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_REPORTING_AGENT_LINK_ID_BITMASK);
      reporting_agent_link_id >>= IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_REPORTING_AGENT_LINK_ID_SHIFT;

      partner_agent_id = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_PARTNER_AGENT_ID_BITMASK);
      partner_agent_id >>= IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_PARTNER_AGENT_ID_SHIFT;

      partner_link_id = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_PARTNER_LINK_ID_BITMASK);
      partner_link_id >>= IPMI_OEM_DELL_EVENT_DATA2_QPI_LINK_ERROR_PARTNER_LINK_ID_SHIFT;

      snprintf (tmpbuf,
		tmpbuflen,
		"Reporting Agent ID %u, Reporting Agent Link ID %u, Partner Agent ID %u, Partner Link ID %u",
		reporting_agent_id,
		reporting_agent_link_id,
		partner_agent_id,
		partner_link_id);

      return (1);
    }

  /* From Dell Spec */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_CRITICAL_STOP_DURING_OS_LOAD
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_TXT_SX_SENTER_COMMAND_ERROR)
    {
      uint8_t error_code;
      char *error_code_str;

      error_code = system_event_record_data->event_data2;

      switch (error_code)
	{
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ERROR_STATUS_VT_CONFIGURATION_INVALID:
	  error_code_str = "VT Configuration Invalid";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ERROR_STATUS_TPM_CONFIGURATION_INVALID:
	  error_code_str = "TPM Configuration Invalid";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ERROR_STATUS_FIT_CRTM_ERROR:
	  error_code_str = "FIT/CRTM Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ERROR_STATUS_BIOS_ACM_ERROR:
	  error_code_str = "BIOS ACM Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ERROR_STATUS_SINIT_ACM_ERROR:
	  error_code_str = "SINIT ACM Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ERROR_STATUS_TPM_PROVISIONING_ERROR:
	  error_code_str = "TPM Provisioning Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_LEGACY_SHUTDOWN:
	  error_code_str = "Legacy Shutdown";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_BAD_ACM_TYPE:
	  error_code_str = "Bad ACM type";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_UNSUPPORTED_ACM:
	  error_code_str = "Unsupported ACM";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_AUTHENTICATE_FAIL:
	  error_code_str = "Authenticate Fail";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_BAD_ACM_FORMAT:
	  error_code_str = "BAD ACM Format";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_UNEXPECTED_HITM:
	  error_code_str = "Unexpected HITM";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_ILLEGAL_EVENT:
	  error_code_str = "Illegal Event";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_BAD_JOIN_FORMAT:
	  error_code_str = "Bad JOIN Format";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_UNRECOVERABLE_MC_ERROR:
	  error_code_str = "Unrecoverable MC Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_VMX_ABORT:
	  error_code_str = "VMX Abort";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_ACM_CORRUPT:
	  error_code_str = "ACM Corrupt";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_PROCESSOR_ERROR_STATUS_ILLEGAL_VID_RATIO:
	  error_code_str = "Illegal VID Ratio";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_BIOS_ACM_ENTRY_POINT:
	  error_code_str = "BIOS ACM Entry Point";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_HEAP_UNINITIALIZED:
	  error_code_str = "HEAP Uninitialized";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_DPR_UNINITIALIZED:
	  error_code_str = "DPR Uninitialized";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_HEAP_ABOVE_TOP_OF_LOWER_MEMORY:
	  error_code_str = "Heap above top of lower memory";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_PCIEXBAR_SET_TO_INVALID_VALUE:
	  error_code_str = "PCIEXBAR set to invalid value";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LT_HEAP_IS_ABOVE_4GB:
	  error_code_str = "LT heap is above 4GB";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LT_HEAP_IS_NOT_IN_DPR_REGION:
	  error_code_str = "LT heap is not in DPR region";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TXT_DISABLED_BY_MEANS_OF_TXT_POLICY_BIT:
	  error_code_str = "TXT disabled by means of TXT policy bit";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_EITHER_STARTUP_ACM_WAS_CALLED_FROM_BIOS_OR_CPU_OFFSET_IN_PCIEXBAR_IS_TOO_LARGE:
	  error_code_str = "Either startup ACM was called from BIOS or CPU offset in PCIEXBAR is too large";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_NON_SUPPORTED_DEVICE_ID:
	  error_code_str = "Non-supported Device.ID";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MEMORY_IS_NOT_LOCKED:
	  error_code_str = "Memory is not locked";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_SENTER_USED_TO_LAUNCH_AC_MODULE:
	  error_code_str = "SENTER used to launch AC module";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_START_MTRR_CHECK:
	  error_code_str = "Start MTRR Check";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_RULE_1_ERROR:
	  error_code_str = "MTRR Rule 1 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_RULE_2_ERROR:
	  error_code_str = "MTRR Rule 2 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_RULE_3_ERROR:
	  error_code_str = "MTRR Rule 3 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_RULE_4_ERROR:
	  error_code_str = "MTRR Rule 4 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_RULE_5_ERROR:
	  error_code_str = "MTRR Rule 5 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_RULE_6_ERROR:
	  error_code_str = "MTRR Rule 6 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_PCIEXBAR_SET_TO_INVALID_SIZE:
	  error_code_str = "PCIEXBAR set to invalid size";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TPM_EXTEND_ATTEMPT:
	  error_code_str = "TPM_Extend attempt";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TPM_ACCESS_REGISTER_CONTENTS_INVALID:
	  error_code_str = "TPM access register contents invalid";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_UNABLE_TO_GET_ACCESS_TO_THE_LOCALITY:
	  error_code_str = "Unable to get access to the locality";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_OUTPUT_BUFFER_FOR_THE_TPM_RESPONSE_TOO_SHORT:
	  error_code_str = "Output buffer for the TPM response too short";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_INPUT_PARAMETER_FOR_THE_FUNCTION_INVALID:
	  error_code_str = "Input parameter for the function invalid";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_INVALID_RESPONSE_FOR_THE_TPM:
	  error_code_str = "Invalid response for the TPM";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TIME_OUT_FOR_TPM_RESPONSE:
	  error_code_str = "Time out for TPM response";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TPM_RETURNED_AN_ERROR:
	  error_code_str = "TPM returned an error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TPM_NV_RAM_NOT_LOCKED:
	  error_code_str = "TPM NV RAM not locked";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TPM_IS_DISABLED:
	  error_code_str = "TPM is disabled";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TPM_IS_DEACTIVATED:
	  error_code_str = "TPM is deactivated";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TPM_NV_INDICES_INCORRECTLY_DEFINED:
	  error_code_str = "TPM NV indices incorrectly defined";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LCP_NO_PO_POLICY_DATA_DEFINED:
	  error_code_str = "LCP No PO Policy Data defined";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LPC_MLE_MISMATC:
	  error_code_str = "LPC MLE Mismatc";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LPC_PLATFORM_CONFIG_MISMATC:
	  error_code_str = "LPC Platform Config Mismatc";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LPC_SINIT_REVOKED:
	  error_code_str = "LPC SINIT Revoked";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LPC_NPW_NOT_ALLOWED:
	  error_code_str = "LPC NPW Not Allowed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LPC_PO_POLICY_INTEGRITY_FAILED:
	  error_code_str = "LPC PO Policy Integrity Failed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LPC_PS_POLICY_INTEGRITY_FAILED:
	  error_code_str = "LPC PS Policy Integrity Failed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_INTERRUPT_OCCURED:
	  error_code_str = "Interrupt Occured";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_CPU_NOT_SUPPORTED_BY_THIS_ACM:
	  error_code_str = "CPU not supported by this ACM";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_ONE_OF_THE_FIT_TABLE_CHECKS_FAILED:
	  error_code_str = "One of the FIT table checks failed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_VERIFY_BIOS_FAILED:
	  error_code_str = "Verify BIOS failed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_BIOS_POLICY_FAILURE:
	  error_code_str = "BIOS policy failure";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MEMORY_LOCKED_WHEN_CLEARSECRETS_IS_CALLED:
	  error_code_str = "Memory locked when ClearSecrets is called";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_BIOS_UNTRUSTED_WHEN_CLEARSECRETS_IS_CALLED:
	  error_code_str = "BIOS untrusted when ClearSecrets is called";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MEMORY_LOCKED_WHEN_CLEARSECRETS_IS_CALLED2:
	  error_code_str = "Memory locked when ClearSecrets is called";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_FIT_POLICY_TPM_OR_TXT_POLICY_DATA_BAD:
	  error_code_str = "FIT policy (TPM or TXT policy) data bad";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_FIT_TABLE_END_NOT_BELOW_4GB:
	  error_code_str = "FIT table end not below 4GB";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_DIDNT_FIND_BIOS_STARTUP_RECORD_THAT_INCLUDES_RESET_VECTOR:
	  error_code_str = "Didn't find BIOS startup record that includes reset vector";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_DIDNT_FIND_BIOS_STARTUP_RECORD_THAT_INCLUDES_FIT_POINTER:
	  error_code_str = "Didn't find BIOS startup record that includes FIT pointer";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_FOUND_OVERLAP_BETWEEN_BIOS_STARTUP_REGIONS:
	  error_code_str = "Found overlap between BIOS startup regions";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_FOUND_OVERLAP_BETWEEN_A_BIOS_STARTUP_REGION_AND_THE_STARTUP_ACM:
	  error_code_str = "found overlap between a BIOS startup region and the startup ACM";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LT_SPAD_ERROR_CONDITION_FOUND_IN_SCHECK_CALL:
	  error_code_str = "LT SPAD Error Condition found in SCHECK call";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LT_SPAD_ERROR_CONDITION_FOUND_IN_CLEAR_SECRETS_CALL:
	  error_code_str = "LT SPAD Error Condition found in CLEAR SECRETS call";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LT_SPAD_ERROR_CONDITION_FOUND_IN_UNLOCK_CONFIG_CALL:
	  error_code_str = "LT SPAD Error Condition found in UNLOCK CONFIG call";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_LT_SPAD_ERROR_CONDITION_FOUND_IN_RESET_AUX_CALL:
	  error_code_str = "LT SPAD Error Condition Found in RESET AUX call";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_MASK_ERROR_EX_ONLY:
	  error_code_str = "MTRR mask error (-EX only)";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_MTRR_MAP_ERROR_EX_ONLY:
	  error_code_str = "MTRR MAP error (-EX only)";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_TIMEOUT_WHILE_ACQUIRING_SHARED_RESOURCE_EX_ONLY:
	  error_code_str = "Timeout while acquiring shared resource (-EX only)";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_BAD_RESOURCE_NUMBER_SPECIFIED_WHEN_ACQUIRING_SHARED:
	  error_code_str = "Bad resource number specified when acquiring shared";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_BIOS_ACM_ERROR_STATUS_CPU_REV_IS_NOT_SUPPORTED_BY_THIS_ACM:
	  error_code_str = "CPU rev is not supported by this ACM";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_INVALID_SINIT_EXIT_ADDRESS:
	  error_code_str = "Invalid SINIT Exit Address";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LOCK_CONFIG_CALL_TO_BIOS_ACM_NOT_DONE_BY_ALL_SOCKETS:
	  error_code_str = "Lock config call to BIOS ACM not done by all sockets";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MLE_JOIN_ABOVE_4GB:
	  error_code_str = "MLE Join Above 4GB";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_HEAP_UNINITIALIZED:
	  error_code_str = "Heap Uninitialized";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DPR_UNINITIALIZED:
	  error_code_str = "DPR Uninitialized";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_HEAP_REGION_DEFINED_ABOVE_4GB:
	  error_code_str = "Heap region defined above 4GB";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TXT_MEMORY_REGION_DISABLED:
	  error_code_str = "TXT Memory Region Disabled";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_NON_SUPPORTED_DEVICE_ID:
	  error_code_str = "Non-supported Device.ID";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MEMORY_IS_NOT_LOCKED:
	  error_code_str = "Memory is not locked";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SENTER_USED_TO_LAUNCH_THE_AC_MODULE:
	  error_code_str = "SENTER used to launch the AC module";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_START_MTRR_CHECK:
	  error_code_str = "Start MTRR Check";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MTRR_RULE_1_ERROR:
	  error_code_str = "MTRR Rule 1 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MTRR_RULE_2_ERROR:
	  error_code_str = "MTRR Rule 2 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MTRR_RULE_3_ERROR:
	  error_code_str = "MTRR Rule 3 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MTRR_RULE_4_ERROR:
	  error_code_str = "MTRR Rule 4 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MTRR_RULE_5_ERROR:
	  error_code_str = "MTRR Rule 5 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MTRR_RULE_6_ERROR:
	  error_code_str = "MTRR Rule 6 Error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_INVALID_MTRR_MASK_VALUE:
	  error_code_str = "Invalid MTRR mask value";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_INVALID_MTRR_MAPPING:
	  error_code_str = "Invalid MTRR mapping";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DPR_SIZE_ERROR:
	  error_code_str = "DPR Size error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_HEAP_BASE_ERROR:
	  error_code_str = "Heap Base error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_HEAP_SIZE_ERROR:
	  error_code_str = "Heap Size error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SINIT_BASE_ERROR:
	  error_code_str = "Sinit Base error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SINIT_SIZE_ERROR:
	  error_code_str = "Sinit Size error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_BAR_OVERLAP:
	  error_code_str = "BAR overlap";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DPR_NOT_LOCKED:
	  error_code_str = "DPR not Locked";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_BAR_SIZE_ERROR:
	  error_code_str = "BAR Size error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_PMR_NOT_PROGRAMMED:
	  error_code_str = "PMR Not programmed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SINIT_LOC_ERROR:
	  error_code_str = "SINIT Loc error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MCHBAR_LOCATED_ABOVE_4GB:
	  error_code_str = "MCHBAR located above 4GB";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SINIT_MEMORY_SPACE_LOCATED_ABOVE_4GB:
	  error_code_str = "SINIT memory space located above 4GB";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_VERSION_ERROR:
	  error_code_str = "OS Data Version error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_PMR_LOW_ERROR:
	  error_code_str = "OS Data PMR Low error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_PMR_LOW_BASE_ERROR:
	  error_code_str = "OS Data PMR Low Base error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_PMR_LOW_SIZE_ERROR:
	  error_code_str = "OS Data PMR Low Size error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_PMR_HIGH_ERROR:
	  error_code_str = "OS Data PMR High error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_PMR_OVERLAP:
	  error_code_str = "OS Data PMR Overlap";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_ABOVE_PHYSICAL_MEMORY:
	  error_code_str = "OS Data Above Physical Memory";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_SIZE_ERROR:
	  error_code_str = "OS Data Size error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OS_DATA_REQ_CAPABILITIES_ERROR:
	  error_code_str = "OS Data Req Capabilities error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_POLICY_NOT_IN_DMA_PROTECTED_REGION:
	  error_code_str = "LCP Policy Not in DMA Protected Region";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_RSDP_NOT_FOUND:
	  error_code_str = "RSDP not found";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_RSDP_CHECKSUM_ERROR:
	  error_code_str = "RSDP checksum error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_RSDT_CHECKSUM_ERROR:
	  error_code_str = "RSDT checksum error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DMAR_NOT_FOUND:
	  error_code_str = "DMAR not found";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DMAR_CHECKSUM_ERROR:
	  error_code_str = "DMAR checksum error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DRHD_BAR_NOT_FOUND:
	  error_code_str = "DRHD BAR not found";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DMAR_LENGTH_ERROR:
	  error_code_str = "DMAR Length error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DRHD_DEV_SCOPE_ERROR:
	  error_code_str = "DRHD Dev Scope error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_RMRR_NOT_FOUND:
	  error_code_str = "RMRR not found";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DPR_DMAR_OVERLAP:
	  error_code_str = "DPR DMAR Overlap";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MSEG_SIZE_ERROR:
	  error_code_str = "MSEG Size error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MSEG_BASE_ERROR:
	  error_code_str = "MSEG Base error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_MSEG_DIFF_BASE_ERROR:
	  error_code_str = "MSEG Diff Base error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_EXTEND_ATTEMPT:
	  error_code_str = "TPM_Extend attempt";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_ACCESS_REGISTER_CONTENTS_INVALID:
	  error_code_str = "TPM access register contents invalid";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_UNABLE_TO_GET_ACCESS_TO_THE_LOCALITY:
	  error_code_str = "Unable to get access to the locality";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_OUTPUT_BUFFER_FOR_THE_TPM_RESPONSE_TOO_SHORT:
	  error_code_str = "Output buffer for the TPM response too short";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_INPUT_PARAMETER_FOR_THE_FUNCTION_INVALID:
	  error_code_str = "Input parameter for the function invalid";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_INVALID_RESPONSE_FOR_THE_TPM:
	  error_code_str = "Invalid response for the TPM";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TIME_OUT_FOR_TPM_RESPONSE:
	  error_code_str = "Time out for TPM response";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_RETURNED_AN_ERROR:
	  error_code_str = "TPM returned an error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_NV_RAM_NOT_LOCKED:
	  error_code_str = "TPM NV RAM not locked";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_IS_DISABLED:
	  error_code_str = "TPM is disabled";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_IS_DEACTIVATED:
	  error_code_str = "TPM is deactivated";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_NV_INDICES_INCORRECTLY_DEFINED:
	  error_code_str = "TPM NV indices incorrectly defined";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_PCR17_NOT_VALID:
	  error_code_str = "PCR17 not valid";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_VALUE_IN_PCR17_DOES_NOT_MATCH_EXPECTED_RESULT:
	  error_code_str = "Value in PCR17 does not match expected result";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_VALUE_IN_PCR18_DOES_NOT_MATCH_EXPECTED_RESULT:
	  error_code_str = "Value in PCR18 does not match expected result";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_TPM_STARTUP_HAS_ALREADY_BEEN_RUN:
	  error_code_str = "TPM startup has already been run";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_TOLM_ERROR:
	  error_code_str = "SCHECK Tolm error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_ALIAS_ERROR:
	  error_code_str = "SCHECK Alias error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_TOHM_ERROR:
	  error_code_str = "SCHECK Tohm error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_SAD_RULES_ERROR:
	  error_code_str = "SCHECK SAD Rules error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_TSEG_ERROR:
	  error_code_str = "SCHECK TSEG error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_PCIEX_BAR_ERROR:
	  error_code_str = "SCHECK PCIEX Bar error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_MAD_OVERLAP:
	  error_code_str = "SCHECK MAD Overlap";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_UNCORE_IIO_SAD_ERROR:
	  error_code_str = "SCHECK Uncore IIO SAD error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_TAD_RULES_ERROR:
	  error_code_str = "SCHECK TAD Rules error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_SAG_ERROR:
	  error_code_str = "SCHECK SAG error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_RIR_ERROR:
	  error_code_str = "SCHECK RIR error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_SMRR_ERROR:
	  error_code_str = "SCHECK SMRR error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_SCHECK_CHANNEL_MAPPER_ERROR:
	  error_code_str = "SCHECK Channel Mapper error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_DMA_REMAP_ERROR:
	  error_code_str = "DMA Remap error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_ME_VT_POLICY_ERROR:
	  error_code_str = "ME VT Policy error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_NO_PO_POLICY_DATA_DEFINE:
	  error_code_str = "LCP NO PO Policy Data define";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_MLE_MISMATCH:
	  error_code_str = "LCP MLE mismatch";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_PLATFORM_CONFIG_MISMATCH:
	  error_code_str = "LCP Platform Config Mismatch";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_SINIT_REVOKED:
	  error_code_str = "LCP Sinit Revoked";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_NPW_NOT_ALLOWED:
	  error_code_str = "LCP NPW Not Allowed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_PO_POLICY_INTEGRITY_FAILED:
	  error_code_str = "LCP PO Policy Integrity Failed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_LCP_PS_POLICY_INTEGRITY_FAILED:
	  error_code_str = "LCP PS Policy Integrity Failed";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_INTERRUPT_ERROR:
	  error_code_str = "Interrupt error";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_VTD_TABLE_LARGER_THAN_HEAP_SIZE:
	  error_code_str = "VTd table larger than heap size";
	  break;
	case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_OEM_DELL_TXT_ERROR_CODE_SINIT_ACM_ERROR_STATUS_CPU_NOT_SUPPORTED_BY_THIS_ACM:
	  error_code_str = "CPU not supported by this ACM";
	  break;
	default:
	  error_code_str = "Unknown";
	  break;
	}

      snprintf (tmpbuf,
		tmpbuflen,
		"TXT Error Code = %s",
		error_code_str);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_r720_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720);

  if ((ret = sel_string_output_dell_oem_diagnostic_event_data_common_event_data2_class_oem (ctx,
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
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_r720_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720);

  if ((ret = sel_string_output_dell_poweredge_event_data3_discrete_oem (ctx,
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

  /* From Dell Spec */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR_QPI_LINK_DEGRADE
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_QPI_LINK_ERROR_SENSOR)
    {
      uint8_t reporting_agent_type;
      uint8_t partner_agent_type;
      uint8_t error_type;
      char *reporting_agent_type_str;
      char *partner_agent_type_str;
      char *error_type_str;

      reporting_agent_type = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_REPORTING_AGENT_TYPE_BITMASK);
      reporting_agent_type >>= IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_REPORTING_AGENT_TYPE_SHIFT;

      partner_agent_type = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_PARTNER_AGENT_TYPE_BITMASK);
      partner_agent_type >>= IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_PARTNER_AGENT_TYPE_SHIFT;

      error_type = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_ERROR_TYPE_BITMASK);
      error_type >>= IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_ERROR_TYPE_SHIFT;

      switch (reporting_agent_type)
	{
	case IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_AGENT_TYPE_CPU:
	  reporting_agent_type_str = "CPU";
	  break;
	case IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_AGENT_TYPE_IOH:
	  reporting_agent_type_str = "IOH";
	  break;
	default:
	  reporting_agent_type_str = "Unknown";
	  break;
	}

      switch (partner_agent_type)
	{
	case IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_AGENT_TYPE_CPU:
	  partner_agent_type_str = "CPU";
	  break;
	case IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_AGENT_TYPE_IOH:
	  partner_agent_type_str = "IOH";
	  break;
	default:
	  partner_agent_type_str = "Unknown";
	  break;
	}

      switch (error_type)
	{
	case IPMI_OEM_DELL_EVENT_DATA3_QPI_LINK_ERROR_ERROR_TYPE_LINK_WIDTH_DEGRADED:
	  error_type_str = "Link Width Degraded";
	  break;
	default:
	  error_type_str = "Unknown";
	  break;
	}

      snprintf (tmpbuf,
		tmpbuflen,
		"Reporting Agent Type = %s, Partner Agent Type = %s, Error Type = %s",
		reporting_agent_type_str,
		partner_agent_type_str,
		error_type_str);
	  
      return (1);
    }

  /* From Dell Spec */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP_CRITICAL_STOP_DURING_OS_LOAD
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_TXT_SX_SENTER_COMMAND_ERROR)
    {
      uint8_t error_type;
      char *error_type_str;

      error_type = system_event_record_data->event_data3;

      switch (error_type)
	{
	case IPMI_OEM_DELL_EVENT_DATA3_TXT_ERROR_TYPE_BIOS_TXT_ERROR:
	  error_type_str = "BIOS TXT Error";
	  break;
	case IPMI_OEM_DELL_EVENT_DATA3_TXT_ERROR_TYPE_PROCESSOR_FIT_TXT_ERROR:
	  error_type_str = "Processor/FIT TXT Error";
	  break;
	case IPMI_OEM_DELL_EVENT_DATA3_TXT_ERROR_TYPE_BIOS_ACM_TXT_ERROR:
	  error_type_str = "BIOS ACM TXT Error";
	  break;
	case IPMI_OEM_DELL_EVENT_DATA3_TXT_ERROR_TYPE_SINIT_ACM_TXT_ERROR:
	  error_type_str = "SINI ACM TXT Error";
	  break;
	case IPMI_OEM_DELL_EVENT_DATA3_TXT_ERROR_TYPE_UNRECOGNIZED_TXT_ERROR:
	  error_type_str = "Unrecognized TXT Error";
	  break;
	default:
	  error_type_str = "Unknown";
	  break;
	}

      snprintf (tmpbuf,
		tmpbuflen,
		"TXT Error Type = %s",
		error_type_str);
	  
      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_r720_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720);

  if ((ret = sel_string_output_dell_oem_diagnostic_event_data_event_data3_class_oem (ctx,
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
sel_string_output_dell_r720_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);
  assert (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720);

  if ((ret = sel_string_output_dell_poweredge_event_data2_event_data3 (ctx,
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

  /* From Dell Spec and Dell Code
   *
   * Data2
   * [7:4] - 0Eh = Use upper Nibble of Data1
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
		 || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PRESENCE_DETECTED
		 || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CONFIGURATION_ERROR
		 || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CRITICAL_OVERTEMPERATURE))
	    || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED
		&& system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_CORRECTABLE_MEMORY_ERROR_LOGGING_DISABLED)))
       || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
	   && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	   && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT
	       || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_REDUNDANCY_LOST))
       || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
	   && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	   && (system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_CRITICAL_FROM_OK
	       || system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_CRITICAL_FROM_LESS_SEVERE))
       || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_FAILURE
	   && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
	   && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_FAILURE_MEMORY_FAILED_TO_TRANSITION_TO_ONLINE))
      && (((system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_BITMASK) >> IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_SHIFT) == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_USE_DATA1_UPPER_NIBBLE)
      && ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION)
    {
      uint8_t memory_card;

      memory_card = (system_event_record_data->event_data2_flag << 2) | system_event_record_data->event_data3_flag;

      if (memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA1_OEM_DELL_DIMMS_PER_PACKAGE_3)
	{
	  uint8_t dimms_per_node;

	  /* For future expansion, I know it looks silly right now */
	  if (memory_card == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA1_OEM_DELL_DIMMS_PER_PACKAGE_3)
	    dimms_per_node = 3;
              
	  if (_dell_calculate_dimm_location (ctx,
					     sel_entry,
					     sel_record_type,
					     buf,
					     buflen,
					     flags,
					     wlen,
					     system_event_record_data,
					     oem_rv,
					     dimms_per_node) > 0)
	    return (1);
	}
    }

  return (0);
}
