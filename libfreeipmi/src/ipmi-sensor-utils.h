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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/


#ifndef _IPMI_SENSOR_UTILS_H
#define _IPMI_SENSOR_UTILS_H

double ipmi_sensor_decode_value_old (char r_exponent, 
				     char b_exponent, 
				     int m, 
				     int b, 
				     int linear, 
				     int is_signed, 
				     uint64_t raw_data);
double ipmi_sensor_decode_value (char r_exponent, 
				 char b_exponent, 
				 short m, 
				 short b, 
				 char linear, 
				 uint8_t analog_data_format, 
				 uint8_t raw_data);
void ipmi_sensor_get_decode_params_old (uint8_t *sensor_record, 
				    int *is_signed, char *r_exponent, char *b_exponent, 
				    uint64_t *linear, int *b, int *m);
void ipmi_sensor_get_decode_params (uint8_t *sensor_record, 
				    uint8_t *is_signed, 
				    char *r_exponent, 
				    char *b_exponent, 
				    char *linear, 
				    short *b, 
				    short *m);

#endif
