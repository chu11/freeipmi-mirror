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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"

#include "libcommon/ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

/* 
 * Threshold Comparsion status
 */
static char *threshold_comparison_status_desc[] =
  {
    "At or Below (<=) Lower Non-Critical Threshold",
    "At or Below (<=) Lower Critical Threshold",
    "At or Below (<=) Lower Non-Recoverable Threshold",
    "At or Above (>=) Upper Non-Critical Threshold",
    "At or Above (>=) Upper Critical Threshold",
    "At or Above (>=) Upper Non-Recoverable Threshold",
    NULL,
  };
static int threshold_comparison_status_desc_max = 0x5;

int 
ipmi_get_threshold_message (uint8_t offset, char *buf, unsigned int buflen)
{
  int rv;

  ERR_EINVAL(buf && buflen);
  ERR_EINVAL((offset <= threshold_comparison_status_desc_max));

  rv = snprintf(buf, buflen, threshold_comparison_status_desc[offset]);
  /* -1 to account for '\0' */
  ERR_ENOSPC(!(rv >= (buflen - 1)));

  return (0);
}

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

int
ipmi_sensor_decode_raw_value (int8_t r_exponent, 
                              int8_t b_exponent, 
                              int16_t m, 
                              int16_t b, 
                              uint8_t linearization, 
                              uint8_t analog_data_format, 
                              double value,
                              uint8_t *raw_data)
{
  double dval;
  uint8_t rval;

  ERR_EINVAL (value 
	      && IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format)
              && IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization));
    
  dval = value;
  
  /* achu:
   *
   * b/c I always forget:
   *
   * y = log_b(x) == x = b^y
   *
   * log_b(x) = log_k(x)/log(k(b)
   */
  /* achu: the macros M_E or M_El for 'e' is questionably portable.
   * Folks online suggest just using exp(1.0) in its place.  Sounds
   * good to me.
   */
  if (linearization == IPMI_SDR_LINEARIZATION_LN)
    dval = exp(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG10)
    dval = exp10(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG2)
    dval = exp2(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_E)
    dval = (log(dval)/log(exp(1.0)));
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP10)
    dval = (log(dval)/log(10));
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP2)
    dval = (log(dval)/log(2));
  else if (linearization == IPMI_SDR_LINEARIZATION_INVERSE)
    {
      if (dval != 0.0)
        dval = 1.0 / dval;
    }
  else if (linearization == IPMI_SDR_LINEARIZATION_SQR)
    dval = sqrt(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBE)
    dval = cbrt(dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_SQRT)
    dval = pow(dval, 2.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBERT)
    dval = pow(dval, 3.0);

  dval = (dval / pow (10, r_exponent));
  dval = (dval - (b * pow (10, b_exponent)));
  if (m)
    dval = (dval / m);

  /* Floating point arithmetic cannot guarantee us a perfect
   * conversion of raw to value and back to raw.  This can
   * fix things.
   */
  if ((dval - (int)dval) >= 0.5)
    dval = ceil(dval);
  else
    dval = floor(dval);
 
  if (analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_UNSIGNED)
    rval = (uint8_t) dval;
  else if (analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_1S_COMPLEMENT)
    {
      rval = (char)dval;
      if (rval & 0x80)
        rval--;
    }
  else /* analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_2S_COMPLEMENT */
    rval = (char)dval;
    
  *raw_data = rval;
  return (0);
}
