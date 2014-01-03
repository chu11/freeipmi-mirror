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

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sensors_oem_intel_node_manager_output_oem_record (ipmi_sensors_state_data_t *state_data)
{
  assert (state_data);
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
      
      if ((ret = ipmi_sdr_oem_parse_intel_node_manager (state_data->sdr_ctx,
							NULL,
							0,
							&nm_device_slave_address,
							&sensor_owner_lun,
							&channel_number,
							&nm_health_event_sensor_number,
							&nm_exception_event_sensor_number,
							&nm_operational_capabilities_sensor_number,
							&nm_alert_threshold_exceeded_sensor_number)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_oem_parse_intel_node_manager: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  return (-1);
	}
      
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


