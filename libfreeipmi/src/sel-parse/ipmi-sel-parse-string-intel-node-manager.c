/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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
#include "freeipmi/record-format/ipmi-sdr-oem-record-format.h"
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
#include "ipmi-sel-parse-string-intel-node-manager.h"
#include "ipmi-sel-parse-trace.h"
#include "ipmi-sel-parse-util.h"

#include "freeipmi-portability.h"

/* 
 * Intel Node Manager
 *
 * http://download.intel.com/support/motherboards/server/s5500wb/sb/s5500wb_tps_1_0.pdf
 *
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 *
 * Should be called from ipmi-sel-parse-string-VENDOR.c files, not
 * from ipmi-sel-parse-string.c.
 */

/* return (1) - Node Manager SDR entry found
 * return (0) - No Node Manager SDR entry found
 * return (-1) - error, cleanup and return error
 */
static int
_intel_node_manager_init (ipmi_sel_parse_ctx_t ctx)
{
  fiid_obj_t obj_oem_record = NULL;
  uint16_t record_count;
  int found = 0;
  int rv = -1;
  unsigned int i;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  
  if (ctx->intel_node_manager.node_manager_data_parsed)
    return (ctx->intel_node_manager.node_manager_data_found);
  
  if (ipmi_sdr_cache_record_count (ctx->sdr_cache_ctx, &record_count) < 0)
    {
      SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_SDR_CACHE_ERROR);
      goto cleanup;
    }
  
  if (ipmi_sdr_cache_first (ctx->sdr_cache_ctx) < 0)
    {
      SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_SDR_CACHE_ERROR);
      goto cleanup;
    }

  if (!(obj_oem_record = fiid_obj_create (tmpl_sdr_oem_intel_node_manager_record)))
    {
      SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (ctx->sdr_cache_ctx))
    {
      uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
      int sdr_record_len;
      int expected_record_len;
      uint16_t record_id;
      uint8_t record_type;
      uint8_t record_subtype;
      uint8_t version_number;
      uint64_t val;
      
      if ((sdr_record_len = ipmi_sdr_cache_record_read (ctx->sdr_cache_ctx,
                                                        sdr_record,
                                                        IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
        {
          SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_SDR_CACHE_ERROR);
          goto cleanup;
        }
      
      /* Shouldn't be possible */
      if (!sdr_record_len)
        continue;
      
      if (ipmi_sdr_parse_record_id_and_type (ctx->sdr_parse_ctx,
                                             sdr_record,
                                             sdr_record_len,
                                             &record_id,
                                             &record_type) < 0)
        {
          if (ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INVALID_SDR_RECORD
              || ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INCOMPLETE_SDR_RECORD)
            continue;
          else
            SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      
      if (record_type != IPMI_SDR_FORMAT_OEM_RECORD)
        continue;

      if ((expected_record_len = fiid_template_len_bytes (tmpl_sdr_oem_intel_node_manager_record)) < 0)
        {
          SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
          goto cleanup;
        }
      
      if (expected_record_len < sdr_record_len)
        continue;

      if (fiid_obj_clear (obj_oem_record) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      
      if (fiid_obj_set_all (obj_oem_record,
                            sdr_record,
                            sdr_record_len) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }

      /* achu: Node Manager documentation states that OEM ID in the
       * SDR record should be Intel's, but I've seen motherboards w/
       * alternate OEM identifiers, so don't bother checking.
       */

      if (FIID_OBJ_GET (obj_oem_record,
                        "record_subtype",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      record_subtype = val;

      if (record_subtype != IPMI_SDR_OEM_INTEL_NODE_MANAGER_RECORD_SUBTYPE_NM_DISCOVERY)
        continue;

      if (FIID_OBJ_GET (obj_oem_record,
                        "version_number",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      version_number = val;

      if (version_number != IPMI_SDR_OEM_INTEL_NODE_MANAGER_DISCOVERY_VERSION)
        continue;

      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_health_event_sensor_number",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      ctx->intel_node_manager.nm_health_event_sensor_number = val;

      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_exception_event_sensor_number",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      ctx->intel_node_manager.nm_exception_event_sensor_number = val;

      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_operational_capabilities_sensor_number",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      ctx->intel_node_manager.nm_operational_capabilities_sensor_number = val;

      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_alert_threshold_exceeded_sensor_number",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      ctx->intel_node_manager.nm_alert_threshold_exceeded_sensor_number = val;
      
      ctx->intel_node_manager.node_manager_data_parsed = 1;
      ctx->intel_node_manager.node_manager_data_found = 1;
      found = 1;
      break;
    }

  rv = found;
 cleanup:
  fiid_obj_destroy (obj_oem_record);
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
ipmi_sel_parse_output_intel_node_manager_sensor_name (ipmi_sel_parse_ctx_t ctx,
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
          if (ipmi_sel_parse_string_snprintf (buf,
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
          if (ipmi_sel_parse_string_snprintf (buf,
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
          if (ipmi_sel_parse_string_snprintf (buf,
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
          if (ipmi_sel_parse_string_snprintf (buf,
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
          if (ipmi_sel_parse_string_snprintf (buf,
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_node_manager_event_data1_class_oem (ipmi_sel_parse_ctx_t ctx,
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_node_manager_event_data2_class_oem (ipmi_sel_parse_ctx_t ctx,
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
          
          if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_POLICY_MISCONFIGURATION)
            error_type_str = "Policy Misconfiguration";
          else if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_POWER_SENSOR_READING_FAILURE)
            error_type_str = "Power Sensor Reading Failure";
          else if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_INLET_TEMPERATURE_READING_FAILURE)
            error_type_str = "Inlet Temperature Reading Failure";
          else if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_HOST_COMMUNICATION_ERROR)
            error_type_str = "Host Communication error";
          else if (error_type == IPMI_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT_EVENT_DATA2_ERROR_TYPE_REAL_TIME_CLOCK_SYNCHRONIZATION_FAILURE)
            error_type_str = "Real-time clock synchronization failure";
          else
            error_type_str = "Unknown";
          
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
          
          if (health_event == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FORCED_GPIO_RECOVER)
            health_event_str = "Forced GPIO recovery";
          else if (health_event == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_IMAGE_EXECUTION_FAILED)
            health_event_str = "Image execution failed";
          else if (health_event == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FLASH_ERASE_ERROR)
            health_event_str = "Flash erase error";
          else if (health_event == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_FLASH_CORRUPTED)
            health_event_str = "Flash corrupted";
          else if (health_event == IPMI_OEM_INTEL_NODE_MANAGER_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH_EVENT_EVENT_DATA2_INTERNAL_ERROR)
            health_event_str = "Internal error";
          else
            health_event_str = "Unknown";
          
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
ipmi_sel_parse_output_intel_node_manager_event_data3_class_oem (ipmi_sel_parse_ctx_t ctx,
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

