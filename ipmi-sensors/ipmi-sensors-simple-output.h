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

#ifndef IPMI_SENSORS_SIMPLE_OUTPUT_H
#define IPMI_SENSORS_SIMPLE_OUTPUT_H

#include "ipmi-sensors.h"

int ipmi_sensors_simple_output_setup (ipmi_sensors_state_data_t *state_data);

int ipmi_sensors_simple_output (ipmi_sensors_state_data_t *state_data,
                                uint8_t sensor_number,
                                double *sensor_reading,
                                int event_message_output_type,
                                uint16_t sensor_event_bitmask,
                                char **event_message_list,
                                unsigned int event_message_list_len);

#endif /* IPMI_SENSORS_SIMPLE_OUTPUT_H */
