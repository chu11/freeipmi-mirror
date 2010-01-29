/*
  Copyright (C) 2003-2010 FreeIPMI Core Team

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
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define SENSORS_BUFLEN             1024
#define SENSORS_MAX_EVENT_MESSAGES   16

#define SENSORS_UNRECOGNIZED_EVENT "Unrecognized Event = %04Xh"

#define SENSORS_OEM_EVENT          "OEM Event = %04Xh"

#define SENSORS_OK_EVENT           "OK"

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
  if (!buf
      || !buflen
      || offset > threshold_comparison_status_desc_max)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (snprintf (buf, buflen, threshold_comparison_status_desc[offset]));
}

const char *
ipmi_get_sensor_type_string (uint8_t sensor_type)
{
  if (IPMI_SENSOR_TYPE_VALID (sensor_type))
    return (ipmi_sensor_types[sensor_type]);

  if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
    return (ipmi_oem_sensor_type);

  return (NULL);
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

  if (!value
      || !IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format)
      || !IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_UNSIGNED)
    dval = (double) raw_data;
  else if (analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_1S_COMPLEMENT)
    {
      if (raw_data & 0x80)
        raw_data++;
      dval = (double)((char) raw_data);
    }
  else /* analog_data_format == IPMI_SDR_ANALOG_DATA_FORMAT_2S_COMPLEMENT */
    dval = (double)((char) raw_data);

  dval *= (double) m;
  dval += (b * pow (10, b_exponent));
  dval *= pow (10, r_exponent);

  if (linearization == IPMI_SDR_LINEARIZATION_LN)
    dval = log (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG10)
    dval = log10 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG2)
    dval = log2 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_E)
    dval = exp (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP10)
    dval = exp10 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP2)
    dval = exp2 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_INVERSE)
    {
      if (dval != 0.0)
        dval = 1.0 / dval;
    }
  else if (linearization == IPMI_SDR_LINEARIZATION_SQR)
    dval = pow (dval, 2.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBE)
    dval = pow (dval, 3.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_SQRT)
    dval = sqrt (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBERT)
    dval = cbrt (dval);

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

  if (!value
      || !IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format)
      || !IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

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
    dval = exp (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG10)
    dval = exp10 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG2)
    dval = exp2 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_E)
    dval = (log (dval)/log (exp (1.0)));
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP10)
    dval = (log (dval)/log (10));
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP2)
    dval = (log (dval)/log (2));
  else if (linearization == IPMI_SDR_LINEARIZATION_INVERSE)
    {
      if (dval != 0.0)
        dval = 1.0 / dval;
    }
  else if (linearization == IPMI_SDR_LINEARIZATION_SQR)
    dval = sqrt (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBE)
    dval = cbrt (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_SQRT)
    dval = pow (dval, 2.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBERT)
    dval = pow (dval, 3.0);

  dval = (dval / pow (10, r_exponent));
  dval = (dval - (b * pow (10, b_exponent)));
  if (m)
    dval = (dval / m);

  /* Floating point arithmetic cannot guarantee us a perfect
   * conversion of raw to value and back to raw.  This can
   * fix things.
   */
  if ((dval - (int)dval) >= 0.5)
    dval = ceil (dval);
  else
    dval = floor (dval);

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

int
ipmi_sensor_decode_tolerance (int8_t r_exponent,
                              int16_t m,
                              uint8_t linearization,
                              uint8_t raw_data,
                              double *value)
{
  double dval = 0.0;

  if (!value
      || !IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* note no analog_data format, tolerance always stored as unsigned */

  dval = (double) raw_data;

  dval *= (double) m;
  dval /= 2.0;
  dval += (dval * pow (10, r_exponent));

  if (linearization == IPMI_SDR_LINEARIZATION_LN)
    dval = log (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG10)
    dval = log10 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_LOG2)
    dval = log2 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_E)
    dval = exp (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP10)
    dval = exp10 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_EXP2)
    dval = exp2 (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_INVERSE)
    {
      if (dval != 0.0)
        dval = 1.0 / dval;
    }
  else if (linearization == IPMI_SDR_LINEARIZATION_SQR)
    dval = pow (dval, 2.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBE)
    dval = pow (dval, 3.0);
  else if (linearization == IPMI_SDR_LINEARIZATION_SQRT)
    dval = sqrt (dval);
  else if (linearization == IPMI_SDR_LINEARIZATION_CUBERT)
    dval = cbrt (dval);

  *value = dval;
  return (0);
}

int
ipmi_sensor_decode_accuracy (uint16_t accuracy_raw,
                             uint8_t accuracy_exp,
                             double *value)
{
  double dval = 0.0;

  if (!value)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  dval = (accuracy_raw * pow (10, accuracy_exp)) / 100.0;

  *value = dval;
  return (0);
}

int
ipmi_sensor_decode_resolution (int8_t r_exponent,
                               int16_t m,
                               double *value)
{
  double dval = 0.0;

  if (!value)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  dval = abs (m * pow (10, r_exponent));

  *value = dval;
  return (0);
}

int
ipmi_get_sensor_event_messages (uint8_t event_reading_type_code,
                                uint8_t sensor_type, /* ignored if not relevant for event_reading_type_code */
                                uint16_t sensor_event_bitmask, /* ignored if not relevant for event_reading_type_code */
                                uint32_t manufacturer_id, /* ignored if INTERPRET_OEM_DATA not set */
                                uint16_t product_id, /* ignored if INTERPRET_OEM_DATA not set */
                                char ***event_messages,
                                unsigned int *event_messages_count,
                                unsigned int flags)
{
  char **tmp_event_messages_ptr = NULL;
  char *tmp_event_messages[SENSORS_MAX_EVENT_MESSAGES];
  unsigned int tmp_event_messages_count = 0;
  char buf[SENSORS_BUFLEN + 1];
  int event_reading_type_code_class;
  uint16_t bitmask;
  unsigned int i;
  int len;

  if (!event_messages
      || !event_messages_count
      || (flags & ~(IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_SHORT
                    | IPMI_GET_SENSOR_EVENT_MESSAGES_OK_IF_NO_EVENT
                    | IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA)))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  memset (buf, '\0', SENSORS_BUFLEN + 1);

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      int j;

      /* achu: multiple threshold flags can be set (e.g. if we pass
       * the critical threshold, we've also passed the non-critical
       * threshold).  It makes no sense to output multiple in this
       * case, so one message is returned at the max.  Luckily for us
       * (and due to smarts by the IPMI specification authors) if we
       * go from high bits to low bits, we will read the flags in the
       * correct order for output.
       *
       * If you're confused why were use 'ipmi_get_threshold_message'
       * instead of 'ipmi_get_generic_event_message' (b/c this is
       * presumably event_reading_type_code == 0x01), the reason is
       * b/c this is for sensors, not sel events.  In other words, the
       * result we care about comes from the Get Sensor Reading
       * command.
       */

      /* use 'j' instead of 'i', b/c needs to be signed integer */
      for (j = 5; j >= 0; j--)
        {
          bitmask = 0x1 << j;
          
          if (sensor_event_bitmask & bitmask)
            {
              memset (buf, '\0', SENSORS_BUFLEN + 1);
              
              if ((len = ipmi_get_threshold_message (j,
                                                     buf,
                                                     SENSORS_BUFLEN)) < 0)
                goto cleanup;
              
              if (len)
                {
                  if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
                    {
                      SET_ERRNO (ENOMEM);
                      goto cleanup;
                    }

                  tmp_event_messages_count++;
                  break;
                }
            }
        }
    }
  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
           || event_reading_type_code_class ==  IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE
           || (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM
               && flags & IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
               && ((manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
                    && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
                        || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
                    && event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS))))

    {
      for (i = 0; i < 16; i++)
        {
          bitmask = 0x1 << i;

          if (sensor_event_bitmask & bitmask)
            {
              memset (buf, '\0', SENSORS_BUFLEN + 1);

              if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE)
                {
                  if (flags & IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_SHORT)
                    len = ipmi_get_generic_event_message_short (event_reading_type_code,
                                                                i,
                                                                buf,
                                                                SENSORS_BUFLEN);
                  else
                    len = ipmi_get_generic_event_message (event_reading_type_code,
                                                          i,
                                                          buf,
                                                          SENSORS_BUFLEN);
                }
              else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
                {
                  if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
                    {
                      if (flags & IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
                          && (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
                              && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
                                  || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
                              && (sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS
                                  || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
                                  || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
                                  || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR
                                  || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE)))
                        {
                          len = ipmi_get_oem_sensor_type_message (manufacturer_id,
                                                                  product_id,
                                                                  sensor_type,
                                                                  i,
                                                                  buf,
                                                                  SENSORS_BUFLEN);
                        }
                      else
                        goto oem_default_output;
                    }
                  else
                    {
                      if (flags & IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_SHORT)
                        len = ipmi_get_sensor_type_message_short (sensor_type,
                                                                  i,
                                                                  buf,
                                                                  SENSORS_BUFLEN);
                      else
                        len = ipmi_get_sensor_type_message (sensor_type,
                                                            i,
                                                            buf,
                                                            SENSORS_BUFLEN);
                    }
                }
              else
                len = ipmi_get_oem_generic_event_message (manufacturer_id,
                                                          product_id,
                                                          event_reading_type_code,
                                                          i,
                                                          buf,
                                                          SENSORS_BUFLEN);

              if (len < 0)
                {
                  snprintf (buf,
                            SENSORS_BUFLEN,
                            SENSORS_UNRECOGNIZED_EVENT,
                            bitmask);

                  if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
                    {
                      SET_ERRNO (ENOMEM);
                      goto cleanup;
                    }

                  tmp_event_messages_count++;
                  continue;
                }
              
              if (len)
                {
                  if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
                    {
                      SET_ERRNO (ENOMEM);
                      goto cleanup;
                    }
                  
                  tmp_event_messages_count++;
                  continue;
                }
            }
        }
    }
  /* OEM Interpretation
   *
   * Supermicro X8DTH
   */
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM
           && flags & IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
           && (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
               || manufacturer_id ==  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
           && product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTH
           && event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC)
    {
      len = ipmi_get_oem_sensor_event_bitmask_message (manufacturer_id,
                                                       product_id,
                                                       event_reading_type_code,
                                                       sensor_type,
                                                       sensor_event_bitmask,
                                                       buf,
                                                       SENSORS_BUFLEN);

      if (len)
        {
          if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
            {
              SET_ERRNO (ENOMEM);
              goto cleanup;
            }
          
          tmp_event_messages_count++;
        }
      else
        goto oem_default_output;
    }
  else /* OEM Event */
    {
    oem_default_output:

      memset (buf, '\0', SENSORS_BUFLEN + 1);
      
      snprintf (buf,
                SENSORS_BUFLEN,
                SENSORS_OEM_EVENT,
                sensor_event_bitmask);
      
      if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
        {
          SET_ERRNO (ENOMEM);
          goto cleanup;
        }

      tmp_event_messages_count++;
    }

  if (!tmp_event_messages_count
      && (flags & IPMI_GET_SENSOR_EVENT_MESSAGES_OK_IF_NO_EVENT))
    {     
      if (!(tmp_event_messages[0] = strdup (SENSORS_OK_EVENT)))
        {
          SET_ERRNO (ENOMEM);
          goto cleanup;
        }

      tmp_event_messages_count++;
    }
    
  if (tmp_event_messages_count)
    {
      if (!(tmp_event_messages_ptr = (char **) malloc (sizeof (char *) * (tmp_event_messages_count + 1))))
        {
          SET_ERRNO (ENOMEM);
          goto cleanup;
        }
      
      for (i = 0; i < tmp_event_messages_count; i++)
        tmp_event_messages_ptr[i] = tmp_event_messages[i];

      tmp_event_messages_ptr[tmp_event_messages_count] = NULL;
    }

  (*event_messages) = tmp_event_messages_ptr;
  (*event_messages_count) = tmp_event_messages_count;
  
  return (0);

 cleanup:
  if (tmp_event_messages_ptr)
    free (tmp_event_messages_ptr);
  for (i = 0; i < tmp_event_messages_count; i++)
    free (tmp_event_messages[i]);
  return (-1);
}
