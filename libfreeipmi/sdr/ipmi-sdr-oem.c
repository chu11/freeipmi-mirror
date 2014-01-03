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
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/sdr/ipmi-sdr-oem.h"

#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/record-format/ipmi-sdr-oem-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

/* return (1) - is oem intel node manager, fully parsed                                                                                     
 * return (0) - is not oem intel node manager                                                                                               
 * return (-1) - error                                                                                                                      
 */
int
ipmi_sdr_oem_parse_intel_node_manager (ipmi_sdr_ctx_t ctx,
				       const void *sdr_record,
				       unsigned int sdr_record_len,
				       uint8_t *nm_device_slave_address,
				       uint8_t *sensor_owner_lun,
				       uint8_t *channel_number,
				       uint8_t *nm_health_event_sensor_number,
				       uint8_t *nm_exception_event_sensor_number,
				       uint8_t *nm_operational_capabilities_sensor_number,
				       uint8_t *nm_alert_threshold_exceeded_sensor_number)
{
  uint8_t sdr_record_buf[IPMI_SDR_MAX_RECORD_LENGTH];
  int sdr_record_buf_len;
  fiid_obj_t obj_oem_record = NULL;
  int expected_record_len;
  void *sdr_record_to_use;
  unsigned int sdr_record_len_to_use;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!sdr_record || !sdr_record_len)
    {
      if (ctx->operation == IPMI_SDR_OPERATION_READ_CACHE
	  && !sdr_record
	  && !sdr_record_len)
	{
	  if ((sdr_record_buf_len = ipmi_sdr_cache_record_read (ctx,
								sdr_record_buf,
								IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
	    {
	      SDR_SET_INTERNAL_ERRNUM (ctx);
	      return (-1);
	    }
	  sdr_record_to_use = sdr_record_buf;
	  sdr_record_len_to_use = sdr_record_buf_len;
	}
      else
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
	  return (-1);
	}
    }
  else
    {
      sdr_record_to_use = (void *)sdr_record;
      sdr_record_len_to_use = sdr_record_len;
    }

  if ((expected_record_len = fiid_template_len_bytes (tmpl_sdr_oem_intel_node_manager_record)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (sdr_record_len_to_use < expected_record_len)
    {
      rv = 0;
      goto cleanup;
    }

  if (!(obj_oem_record = fiid_obj_create (tmpl_sdr_oem_intel_node_manager_record)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_oem_record,
                        sdr_record_to_use,
			expected_record_len) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
      goto cleanup;
    }

  /* achu: Node Manager documentation states that OEM ID in the
   * SDR record should be Intel's, but I've seen motherboards w/o
   * it, so don't bother checking.
   */

  if (FIID_OBJ_GET (obj_oem_record,
                    "record_subtype",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
      goto cleanup;
    }
  
  if (val != IPMI_SDR_OEM_INTEL_NODE_MANAGER_RECORD_SUBTYPE_NM_DISCOVERY)
    {
      rv = 0;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_oem_record,
                    "version_number",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
      goto cleanup;
    }

  if (val != IPMI_SDR_OEM_INTEL_NODE_MANAGER_DISCOVERY_VERSION)
    {
      rv = 0;
      goto cleanup;
    }
     
  if (nm_device_slave_address)
    {
      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_device_slave_address",
                        &val) < 0)
        {
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      (*nm_device_slave_address) = val;
    }

  if (sensor_owner_lun)
    {
      if (FIID_OBJ_GET (obj_oem_record,
                        "sensor_owner_lun",
                        &val) < 0)
        {
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      (*sensor_owner_lun) = val;
    }

  if (channel_number)
    {
      if (FIID_OBJ_GET (obj_oem_record,
                        "channel_number",
                        &val) < 0)
        {
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      (*channel_number) = val;
    }

  if (nm_health_event_sensor_number)
    {
      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_health_event_sensor_number",
                        &val) < 0)
        {
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      (*nm_health_event_sensor_number) = val;
    }

  if (nm_exception_event_sensor_number)
    {
      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_exception_event_sensor_number",
                        &val) < 0)
        {
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      (*nm_exception_event_sensor_number) = val;
    }

  if (nm_operational_capabilities_sensor_number)
    {
      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_operational_capabilities_sensor_number",
                        &val) < 0)
        {
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      (*nm_operational_capabilities_sensor_number) = val;
    }
  
  if (nm_alert_threshold_exceeded_sensor_number)
    {
      if (FIID_OBJ_GET (obj_oem_record,
                        "nm_alert_threshold_exceeded_sensor_number",
                        &val) < 0)
        {
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_oem_record);
          goto cleanup;
        }
      (*nm_alert_threshold_exceeded_sensor_number) = val;
    }

  sdr_check_read_status (ctx);

  rv = 1;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_oem_record);
  return (rv);
}
