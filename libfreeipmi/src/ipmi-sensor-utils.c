/* 
   ipmi-sensor-utils.c - IPMI Sensor utility procedures

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "freeipmi/ipmi-sensor-utils.h"
#include "freeipmi/fiid.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"

int
ipmi_sensor_decode_value (char r_exponent, 
			  char b_exponent, 
			  short m, 
			  short b, 
			  char linear, 
			  uint8_t analog_data_format, 
			  uint8_t raw_data,
			  double *value)
{
  double dval = 0.0;
  
  /* XXX need to define analog data format somewhere */
  ERR_EINVAL (value && (analog_data_format == 0x00
			|| analog_data_format == 0x01
			|| analog_data_format == 0x02));

  if (analog_data_format == 0x00)
    dval = (double) raw_data;
  else if (analog_data_format == 0x01)
    {
      if (raw_data & 0x80)
        raw_data++;
      dval = (double) ((char) raw_data);
    }
  else
    dval = (double) ((char) raw_data);
    
  dval *= (double) m;
  dval += (b * pow (10, b_exponent));
  dval *= pow (10, r_exponent);

  *value = dval;
  return (0);
}


