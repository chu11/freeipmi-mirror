/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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
#include "freeipmi/record-format/ipmi-sdr-oem-record-format.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/sdr/ipmi-sdr-oem.h"
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
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* 
 * Intel Node Manager
 *
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Intel S2600JF/Appro 512X
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 * Quanta QSSC-S4R/Appro GB812X-CN
 *
 * Should be called from ipmi-sel-string-VENDOR.c files, not
 * from ipmi-sel-string.c.
 */

struct intel_node_manager_sdr_callback
{
  ipmi_sel_ctx_t ctx;
  int found;
};

static int
_intel_node_manager_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
				  uint8_t record_type,
				  const void *sdr_record,
				  unsigned int sdr_record_len,
				  void *arg)
{
  struct intel_node_manager_sdr_callback *sdr_callback_arg;
  ipmi_sel_ctx_t ctx;
  int ret;
  int rv = -1;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  sdr_callback_arg = (struct intel_node_manager_sdr_callback *)arg;
  ctx = sdr_callback_arg->ctx;

  if (record_type != IPMI_SDR_FORMAT_OEM_RECORD)
    {
      rv = 0;
      goto cleanup;
    }

  if ((ret = ipmi_sdr_oem_parse_intel_node_manager (sdr_ctx,
                                                    sdr_record,
                                                    sdr_record_len,
						    NULL,
						    NULL,
						    NULL,
						    &ctx->intel_node_manager.nm_health_event_sensor_number,
						    &ctx->intel_node_manager.nm_exception_event_sensor_number,
						    &ctx->intel_node_manager.nm_operational_capabilities_sensor_number,
						    &ctx->intel_node_manager.nm_alert_threshold_exceeded_sensor_number)) < 0)
    goto cleanup;
  
  if (ret)
    {
      ctx->intel_node_manager.node_manager_data_parsed = 1;
      ctx->intel_node_manager.node_manager_data_found = 1;
      sdr_callback_arg->found = 1;
    }
  rv = ret;

 cleanup:
  return (rv);
}

/* return (1) - Node Manager SDR entry found
 * return (0) - No Node Manager SDR entry found
 * return (-1) - error, cleanup and return error
 */
static int
_intel_node_manager_init (ipmi_sel_ctx_t ctx)
{
  struct intel_node_manager_sdr_callback sdr_callback_arg;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);

  if (!ctx->sdr_ctx)
    return (0);
  
  if (ctx->intel_node_manager.node_manager_data_parsed)
    return (ctx->intel_node_manager.node_manager_data_found);
  
  sdr_callback_arg.ctx = ctx;
  sdr_callback_arg.found = 0;

  if (ipmi_sdr_cache_iterate (ctx->sdr_ctx,
			      _intel_node_manager_sdr_callback,
			      &sdr_callback_arg) < 0)
    goto cleanup;
	      
  if (!sdr_callback_arg.found)
    {
      ctx->intel_node_manager.node_manager_data_parsed = 1;
      ctx->intel_node_manager.node_manager_data_found = 0;
    }

  rv = sdr_callback_arg.found;
 cleanup:
  return (rv);
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
sel_string_output_intel_node_manager_sensor_name (ipmi_sel_ctx_t ctx,
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
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  /* achu: As far as I can tell, SDR is required to have Node Manager
   * OEM SDR, but is not required to have "Event Only" records for the
   * individual event types.  It appears most motherboards do have SDR
   * entries for these events, but it's best to cover all cases if
   * necessary.  In upper level calls, if an SDR entry is available,
   * it will be found before reaching this point.
   */

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_NODE_MANAGER)
    {
      int nm_found;

      if ((nm_found = _intel_node_manager_init (ctx)) < 0)
	return (-1);
      
      if (!nm_found)
	goto out;

      /* achu: On the first motherboards I found w/ NM support, these are the sensor names I've seen.
       *
       * NM Capabilities
       * NM Exception
       * NM Health
       * NM Threshold
       *
       * So we'll copy them.  I've found no common name for the
       * IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
       * event, so we'll call it "NM Firmware".
       */

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_health_event_sensor_number)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "NM Health"))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;

          return (1);
        }
      else if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT
	       && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_exception_event_sensor_number)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "NM Exception"))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
	  
          return (1);
        }
      else if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT
               && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_operational_capabilities_sensor_number)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "NM Capabilities"))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;

          return (1);
        }
      else if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED
               && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_alert_threshold_exceeded_sensor_number)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "NM Threshold"))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;

          return (1);
        }
      else if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
               && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "NM Firmware"))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;

          return (1);
        }
    }
  
 out:
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_node_manager_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_NODE_MANAGER)
    {
      int nm_found;
      uint8_t node_manager_policy_event;
      
      if ((nm_found = _intel_node_manager_init (ctx)) < 0)
	return (-1);
      
      if (!nm_found)
	goto out;

      node_manager_policy_event = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_BITMASK;
      node_manager_policy_event >>= IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_SHIFT;

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_exception_event_sensor_number
          && node_manager_policy_event == IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_POLICY_CORRECTION_TIME_EXCEEDED)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Policy Correction Time Exceeded");
          
          return (1);
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_health_event_sensor_number
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_SENSOR_NODE_MANAGER)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Sensor Node Manager");
	  
          return (1);
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_operational_capabilities_sensor_number)
        {
          uint8_t policy_interface_capability;
          uint8_t monitoring_capability;
          uint8_t power_limiting_capability;

          policy_interface_capability = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POLICY_INTERFACE_CAPABILITY_BITMASK;
          policy_interface_capability >>= IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POLICY_INTERFACE_CAPABILITY_SHIFT;

          monitoring_capability = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_MONITORING_CAPABILITY_BITMASK;
          monitoring_capability >>= IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_MONITORING_CAPABILITY_SHIFT;

          power_limiting_capability = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POWER_LIMITING_CAPABILITY_BITMASK;
          power_limiting_capability >>= IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POWER_LIMITING_CAPABILITY_SHIFT;
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_alert_threshold_exceeded_sensor_number)
        {
          node_manager_policy_event = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_BITMASK;
          node_manager_policy_event >>= IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_SHIFT;

          if (node_manager_policy_event == IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_THRESHOLD_EXCEEDED)
            {
              uint8_t threshold_number;

              threshold_number = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA1_THRESHOLD_NUMBER_BITMASK;
              threshold_number >>= IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA1_THRESHOLD_NUMBER_SHIFT;

              snprintf (tmpbuf,
                        tmpbuflen,
                        "Threshold Exceeded, Threshold Number = %u",
                        threshold_number);

              return (1);
              
            }
          else /* node_manager_policy_event == IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_POLICY_CORRECTION_TIME_EXCEEDED */
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Policy Correction Time Exceeded");

              return (1);
            }
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_FIRMWARE_STATUS)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Firmware Status");
	  
          return (1);
        }
    }

 out:
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_node_manager_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_NODE_MANAGER)
    {
      int nm_found;
      uint8_t node_manager_policy_event;
      
      if ((nm_found = _intel_node_manager_init (ctx)) < 0)
	return (-1);
      
      if (!nm_found)
	goto out;

      node_manager_policy_event = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_BITMASK;
      node_manager_policy_event >>= IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_SHIFT;

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_exception_event_sensor_number
          && node_manager_policy_event == IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_POLICY_CORRECTION_TIME_EXCEEDED)
        {
          uint8_t domain_id;
          
          domain_id = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA2_DOMAIN_ID_BITMASK);
          domain_id >>= IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA2_DOMAIN_ID_SHIFT;
          
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Domain ID = %u",
                    domain_id);
          
          return (1);
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_health_event_sensor_number
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_SENSOR_NODE_MANAGER)
        {
          uint8_t domain_id;
          uint8_t error_type;
          char *error_type_str;
          
          domain_id = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_DOMAIN_ID_BITMASK);
          domain_id >>= IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_DOMAIN_ID_SHIFT;
          
          error_type = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_BITMASK);
          error_type >>= IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_SHIFT;
          
	  switch (error_type)
	    {
	    case IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_POLICY_MISCONFIGURATION:
	      error_type_str = "Policy Misconfiguration";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_POWER_SENSOR_READING_FAILURE:
	      error_type_str = "Power Sensor Reading Failure";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_INLET_TEMPERATURE_READING_FAILURE:
	      error_type_str = "Inlet Temperature Reading Failure";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_HOST_COMMUNICATION_ERROR:
	      error_type_str = "Host Communication error";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_REAL_TIME_CLOCK_SYNCHRONIZATION_FAILURE:
	      error_type_str = "Real-time clock synchronization failure";
	      break;
	    default:
	      error_type_str = "Unknown";
	    }
          
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Domain ID = %u, Error Type = %s",
                    domain_id,
                    error_type_str);
          
          return (1);
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_alert_threshold_exceeded_sensor_number)
        {
          uint8_t domain_id;
          
          domain_id = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA2_DOMAIN_ID_BITMASK);
          domain_id >>= IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED_EVENT_DATA2_DOMAIN_ID_SHIFT;
          
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Domain ID = %u",
                    domain_id);
          
          return (1);
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_FIRMWARE_STATUS)
        {
          uint8_t health_event;
          char *health_event_str;
          
          health_event = system_event_record_data->event_data2;
          
	  switch (health_event)
	    {
	    case IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FORCED_GPIO_RECOVER:
	      health_event_str = "Forced GPIO recovery";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_IMAGE_EXECUTION_FAILED:
	      health_event_str = "Image execution failed";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FLASH_ERASE_ERROR:
	      health_event_str = "Flash erase error";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FLASH_CORRUPTED:
	      health_event_str = "Flash corrupted";
	      break;
	    case IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_INTERNAL_ERROR:
	      health_event_str = "Internal error";
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
    }

 out:
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_node_manager_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_NODE_MANAGER)
    {
      int nm_found;
      uint8_t node_manager_policy_event;
      
      if ((nm_found = _intel_node_manager_init (ctx)) < 0)
	return (-1);
      
      if (!nm_found)
	goto out;

      node_manager_policy_event = system_event_record_data->offset_from_event_reading_type_code & IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_BITMASK;
      node_manager_policy_event >>= IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_SHIFT;

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_exception_event_sensor_number
          && node_manager_policy_event == IPMI_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT_EVENT_DATA1_NODE_MANAGER_POLICY_EVENT_POLICY_CORRECTION_TIME_EXCEEDED)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Policy ID = %u",
                    system_event_record_data->event_data3);
          
          return (1);
        }
 
      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_health_event_sensor_number
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_SENSOR_NODE_MANAGER
          && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
        {
          uint8_t error_type;
          
          error_type = (system_event_record_data->event_data2 & IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_BITMASK);
          error_type >>= IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_SHIFT;
          
          if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_POLICY_MISCONFIGURATION)
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Policy ID = %u",
                        system_event_record_data->event_data3);
	      
              return (1);
            }
          else if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_POWER_SENSOR_READING_FAILURE)
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Power Sensor Address = %02Xh",
                        system_event_record_data->event_data3);
              
              return (1);
            }
          else if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_INLET_TEMPERATURE_READING_FAILURE)
            {
              snprintf (tmpbuf,
                        tmpbuflen,
                        "Inlet Sensor Address = %02Xh",
                        system_event_record_data->event_data3);
	      
              return (1);
            }
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED
	  && system_event_record_data->sensor_number == ctx->intel_node_manager.nm_alert_threshold_exceeded_sensor_number)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Policy ID = %u",
                    system_event_record_data->event_data3);
          
          return (1);
        }

      if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
	  && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH
          && system_event_record_data->offset_from_event_reading_type_code == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_FIRMWARE_STATUS)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Extended error code = %02Xh",
                    system_event_record_data->event_data3);
	  
          return (1);
        }
    }
  
 out:
  return (0);
}

