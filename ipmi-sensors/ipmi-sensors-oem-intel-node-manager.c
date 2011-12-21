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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-sensors.h"
#include "ipmi-sensors-oem-intel-node-manager.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sensor-common.h"

/* return (1) - is oem intel node manager, fully parsed
 * return (0) - is not oem intel node manager
 * return (-1) - error
 */
int
ipmi_sensors_oem_parse_intel_node_manager (ipmi_sensors_state_data_t *state_data,
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
  fiid_obj_t obj_oem_record = NULL;
  int expected_record_len;
  uint8_t record_subtype;
  uint8_t version_number;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (sdr_record);
  assert (sdr_record_len);

  if ((expected_record_len = fiid_template_len_bytes (tmpl_sdr_oem_intel_node_manager_record)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_template_len_bytes: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (expected_record_len < sdr_record_len)
    {
      rv = 0;
      goto cleanup;
    }

  if (!(obj_oem_record = fiid_obj_create (tmpl_sdr_oem_intel_node_manager_record)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_oem_record,
                        sdr_record,
                        sdr_record_len) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set_all: %s\n",
                       fiid_obj_errormsg (obj_oem_record));
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
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'record_subtype': %s\n",
                       fiid_obj_errormsg (obj_oem_record));
      goto cleanup;
    }
  record_subtype = val;
  
  if (record_subtype != IPMI_SDR_OEM_INTEL_NODE_MANAGER_RECORD_SUBTYPE_NM_DISCOVERY)
    {
      rv = 0;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_oem_record,
                    "version_number",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'version_number': %s\n",
                       fiid_obj_errormsg (obj_oem_record));
      goto cleanup;
    }
  version_number = val;

  if (version_number != IPMI_SDR_OEM_INTEL_NODE_MANAGER_DISCOVERY_VERSION)
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'nm_device_slave_address': %s\n",
                           fiid_obj_errormsg (obj_oem_record));
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'sensor_owner_lun': %s\n",
                           fiid_obj_errormsg (obj_oem_record));
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'channel_number': %s\n",
                           fiid_obj_errormsg (obj_oem_record));
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'nm_health_event_sensor_number': %s\n",
                           fiid_obj_errormsg (obj_oem_record));
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'nm_exception_event_sensor_number': %s\n",
                           fiid_obj_errormsg (obj_oem_record));
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'nm_operational_capabilities_sensor_number': %s\n",
                           fiid_obj_errormsg (obj_oem_record));
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'nm_alert_threshold_exceeded_sensor_number': %s\n",
                           fiid_obj_errormsg (obj_oem_record));
          goto cleanup;
        }
      (*nm_alert_threshold_exceeded_sensor_number) = val;
    }

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_oem_record);
  return (rv);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sensors_oem_intel_node_manager_output_oem_record (ipmi_sensors_state_data_t *state_data,
						       const void *sdr_record,
						       unsigned int sdr_record_len)
{
  assert (state_data);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (state_data->prog_data->args->verbose_count >= 2);
  assert (state_data->prog_data->args->interpret_oem_data);

  if (state_data->intel_node_manager.node_manager_data_found)
    {
      uint8_t nm_device_slave_address;
      uint8_t sensor_owner_lun;
      uint8_t channel_number;
      uint8_t nm_health_event_sensor_number;
      uint8_t nm_exception_event_sensor_number;
      uint8_t nm_operational_capabilities_sensor_number;
      uint8_t nm_alert_threshold_exceeded_sensor_number;
      int ret;
      
      if ((ret = ipmi_sensors_oem_parse_intel_node_manager (state_data,
                                                            sdr_record,
                                                            sdr_record_len,
                                                            &nm_device_slave_address,
                                                            &sensor_owner_lun,
                                                            &channel_number,
                                                            &nm_health_event_sensor_number,
                                                            &nm_exception_event_sensor_number,
                                                            &nm_operational_capabilities_sensor_number,
                                                            &nm_alert_threshold_exceeded_sensor_number)) < 0)
        return (-1);
      
      if (ret)
	{
	  pstdout_printf (state_data->pstate,
			  "Node Manager Device Slave Address: %Xh\n",
			  nm_device_slave_address);
	  pstdout_printf (state_data->pstate,
			  "Sensor Owner LUN: %Xh\n",
			  sensor_owner_lun);
	  pstdout_printf (state_data->pstate,
			  "Channel Number: %Xh\n",
			  channel_number);
	  pstdout_printf (state_data->pstate,
			  "Node Manager Health Event Sensor Number: %u\n",
			  nm_health_event_sensor_number);
	  pstdout_printf (state_data->pstate,
			  "Node Manager Exception Event Sensor Number: %u\n",
			  nm_exception_event_sensor_number);
	  pstdout_printf (state_data->pstate,
			  "Node Manager Operational Capabilities Sensor Number: %u\n",
			  nm_operational_capabilities_sensor_number);
	  pstdout_printf (state_data->pstate,
			  "Node Manager Alert Threshold Exceeded Sensor Number: %u\n",
			  nm_alert_threshold_exceeded_sensor_number);

	  return (1);
	}
    }

  return (0);
}


