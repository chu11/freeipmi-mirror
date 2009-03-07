/*
   Copyright (C) 2003-2009 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/


#ifndef _IPMI_SENSOR_UNITS_UTIL_H
#define _IPMI_SENSOR_UNITS_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

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
