/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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


#ifndef _IPMI_SENSOR_UTIL_H
#define _IPMI_SENSOR_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

int ipmi_get_threshold_message (uint8_t offset, char *buf, unsigned int buflen);

int ipmi_sensor_decode_value (int8_t r_exponent, 
			      int8_t b_exponent, 
                              int16_t m,
                              int16_t b,
                              uint8_t linearization,
			      uint8_t analog_data_format, 
			      uint8_t raw_data,
			      double *value);

int ipmi_sensor_decode_raw_value (int8_t r_exponent, 
                                  int8_t b_exponent, 
                                  int16_t m, 
                                  int16_t b, 
                                  uint8_t linearization, 
                                  uint8_t analog_data_format, 
                                  double value,
                                  uint8_t *raw_data);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_SENSOR_UTIL_H */
