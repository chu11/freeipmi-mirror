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

#ifndef IPMI_SENSORS_OEM_QUANTA_H
#define IPMI_SENSORS_OEM_QUANTA_H

#include "ipmi-sensors.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int ipmi_sensors_oem_quanta_output_oem_record (ipmi_sensors_state_data_t *state_data,
					       uint32_t oem_record_manufacturer_id,
					       const uint8_t *oem_data,
					       unsigned int oem_data_len);

#endif /* IPMI_SENSORS_OEM_QUANTA_H */
