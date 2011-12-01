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

#ifndef _IPMI_SENSORS_OEM_INTEL_NODE_MANAGER_H
#define _IPMI_SENSORS_OEM_INTEL_NODE_MANAGER_H

#include "ipmi-sensors.h"

/* return (1) - is oem intel node manager, fully parsed
 * return (0) - is not oem intel node manager
 * return (-1) - error
 */
int ipmi_sensors_oem_parse_intel_node_manager (ipmi_sensors_state_data_t *state_data,
                                               const void *sdr_record,
                                               unsigned int sdr_record_len,
                                               uint8_t *nm_device_slave_address,
                                               uint8_t *sensor_owner_lun,
                                               uint8_t *channel_number,
                                               uint8_t *nm_health_event_sensor_number,
                                               uint8_t *nm_exception_event_sensor_number,
                                               uint8_t *nm_operational_capabilities_sensor_number,
                                               uint8_t *node_manager_alert_threshold_exceeded_sensor_number);

#endif
