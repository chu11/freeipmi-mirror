/* 
   ipmi-sensor-utils.h - IPMI Sensor utility procedures

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/


#ifndef _IPMI_SENSOR_UTILS_H
#define _IPMI_SENSOR_UTILS_H

#include <stdint.h>

int ipmi_sensor_decode_value (char r_exponent, 
			      char b_exponent, 
			      short m, 
			      short b, 
			      char linear, 
			      uint8_t analog_data_format, 
			      uint8_t raw_data,
			      double *value);

#endif /* _IPMI_SENSOR_UTILS_H */
