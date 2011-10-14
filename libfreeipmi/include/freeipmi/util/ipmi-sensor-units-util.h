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


#ifndef _IPMI_SENSOR_UNITS_UTIL_H
#define _IPMI_SENSOR_UNITS_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* returns length written into buffer on success, -1 on error */
int ipmi_sensor_units_string (uint8_t sensor_units_percentage,
                              uint8_t sensor_units_modifier,
                              uint8_t sensor_units_rate,
                              uint8_t sensor_base_unit_type,
                              uint8_t sensor_modifier_unit_type,
                              char *buf,
                              unsigned int buflen,
                              unsigned int abbreviated_units_flag);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_SENSOR_UNITS_UTIL_H */
