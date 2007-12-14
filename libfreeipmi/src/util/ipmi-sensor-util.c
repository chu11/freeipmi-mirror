/* 
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/ipmi-sensor-units-spec.h"

#include "libcommon/ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

int
ipmi_sensor_decode_value (int8_t r_exponent, 
			  int8_t b_exponent, 
			  int16_t m, 
			  int16_t b, 
			  uint8_t linearization, 
			  uint8_t analog_data_format, 
			  uint8_t raw_data,
			  double *value)
{
  double dval = 0.0;
  
  ERR_EINVAL (value 
	      && IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format)
              && IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization));
    
  if (analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_UNSIGNED)
    dval = (double) raw_data;
  else if (analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_1S_COMPLEMENT)
    {
      if (raw_data & 0x80)
        raw_data++;
      dval = (double) ((char) raw_data);
    }
  else /* analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_2S_COMPLEMENT */
    dval = (double) ((char) raw_data);
    
  dval *= (double) m;
  dval += (b * pow (10, b_exponent));
  dval *= pow (10, r_exponent);

  if (linearization == IPMI_SDR_LINEARIZATION_LN)
    dval = log(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG10)
    dval = log10(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG2)
    dval = log2(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_E)
    dval = exp(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP10)
    dval = exp10(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP2)
    dval = exp2(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_INVERSE)
    {
      if (dval != 0.0)
        dval = 1.0 / dval;
    }
  else if (linearization == IPMI_SDR_LINEARIZATION_SQR)
    dval = pow(dval, 2.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBE)
    dval = pow(dval, 3.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_SQRT)
    dval = sqrt(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBERT)
    dval = cbrt(dval);
  
  *value = dval;
  return (0);
}
