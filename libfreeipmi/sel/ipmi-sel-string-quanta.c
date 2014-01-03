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
#include "ipmi-sel-string-intel-node-manager.h"
#include "ipmi-sel-string-quanta.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define QUANTA_EVENT_BUFFER_LENGTH 4096

int
sel_string_output_quanta_sensor_name (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA);
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
   * Quanta S99Q/Dell FS12-TY
   */
  if (ctx->product_id == IPMI_QUANTA_PRODUCT_ID_S99Q)
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
sel_string_output_quanta_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Quanta S99Q/Dell FS12-TY
   */
  if (ctx->product_id == IPMI_QUANTA_PRODUCT_ID_S99Q)
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

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_quanta_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA);
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
   * Quanta S99Q/Dell FS12-TY
   */
  if (ctx->product_id == IPMI_QUANTA_PRODUCT_ID_S99Q)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_QUANTA_ERROR
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_QUANTA_PCI_SENSORID
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
        {
          uint8_t device, function;
          
          device = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA2_DEVICE_NUMBER_BITMASK);
          device >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA2_DEVICE_NUMBER_SHIFT;
          
          function = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA2_FUNCTION_NUMBER_BITMASK);
          function >>= IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA2_FUNCTION_NUMBER_SHIFT;
          
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Device %u, Function %u",
                    device,
                    function);

          return (1);
        }

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_QUANTA_ERROR
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_QUANTA_QPI_SENSORID
              || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_QUANTA_INT_SENSORID)
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Local Error Bit %u",
                    system_event_record_data->event_data2);

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
sel_string_output_quanta_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Quanta S99Q/Dell FS12-TY
   */
  if (ctx->product_id == IPMI_QUANTA_PRODUCT_ID_S99Q)
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

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_quanta_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA);
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
   * Quanta S99Q/Dell FS12-TY
   */
  if (ctx->product_id == IPMI_QUANTA_PRODUCT_ID_S99Q)
    {
      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_QUANTA_ERROR
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_QUANTA_MEMORY
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_SPARE))
        {
          char dimmbuf[QUANTA_EVENT_BUFFER_LENGTH];
          char *dimm_str = NULL;

	  switch (system_event_record_data->event_data3)
	    {
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_A0:
	      dimm_str = "DIMM A0";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_A1:
	      dimm_str = "DIMM A1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_A2:
	      dimm_str = "DIMM A2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_B0:
	      dimm_str = "DIMM B0";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_B1:
	      dimm_str = "DIMM B1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_B2:
	      dimm_str = "DIMM B2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_C0:
	      dimm_str = "DIMM C0";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_C1:
	      dimm_str = "DIMM C1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_C2:
	      dimm_str = "DIMM C2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_D0:
	      dimm_str = "DIMM D0";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_D1:
	      dimm_str = "DIMM D1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_D2:
	      dimm_str = "DIMM D2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_E0:
	      dimm_str = "DIMM E0";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_E1:
	      dimm_str = "DIMM E1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_E2:
	      dimm_str = "DIMM E2";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_F0:
	      dimm_str = "DIMM F0";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_F1:
	      dimm_str = "DIMM F1";
	      break;
	    case IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_QUANTA_S99Q_DIMM_F2:
	      dimm_str = "DIMM F2";
	      break;
	    default:
              snprintf (dimmbuf,
                        QUANTA_EVENT_BUFFER_LENGTH,
                        "Error DIMM %u",
                        system_event_record_data->event_data3);
              dimm_str = dimmbuf;
	    }
          
          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    dimm_str);
          
          return (1);
        }

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_QUANTA_ERROR
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_QUANTA_PCI_SENSORID
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Bus Number %u",
                    system_event_record_data->event_data3);

          return (1);
        }

      if (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_QUANTA_ERROR
          && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
          && (system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_QUANTA_QPI_SENSORID
              || system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_QUANTA_INT_SENSORID)
          && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
          && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_CORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_UNCORRECTABLE_ERROR
              || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
        {
          char *errstr = NULL;

	  switch (system_event_record_data->event_data3)
	    {
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA3_QPI0_ERROR:
	      errstr = "QPI[0] Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA3_QPI1_ERROR:
	      errstr = "QPI[1] Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA3_QPI2_ERROR:
	      errstr = "QPI[2] Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA3_QPI3_ERROR:
	      errstr = "QPI[3] Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA3_MISCELLANEOUS_ERROR:
	      errstr = "Miscellaneous Error";
	      break;
	    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_OEM_QUANTA_EVENT_DATA3_IOH_CORE_ERROR:
	      errstr = "IOH Core Error";
	      break;
	    default:
	      errstr = "Unknown Error";
	    }

          snprintf (tmpbuf,
                    tmpbuflen,
                    "%s",
                    errstr);
          
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
sel_string_output_quanta_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Quanta S99Q/Dell FS12-TY
   */
  if (ctx->product_id == IPMI_QUANTA_PRODUCT_ID_S99Q)
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

  return (0);
}
