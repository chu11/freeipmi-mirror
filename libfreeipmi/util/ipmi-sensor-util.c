/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-entity-ids-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/util/ipmi-entity-ids-util.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define IPMI_SENSOR_UTIL_MODIFIER_BUFLEN 1024

#define IPMI_SENSOR_UTIL_CHARS_IN_ALPHABET 26

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

const char *
ipmi_get_oem_sensor_type_string (uint8_t sensor_type,
                                 uint8_t event_reading_code,
                                 uint32_t manufacturer_id,
                                 uint16_t product_id)
{
  if (IPMI_SENSOR_TYPE_VALID (sensor_type))
    return (ipmi_sensor_types[sensor_type]);

  if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
    {
      if ((manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU)
          && (product_id >= IPMI_FUJITSU_PRODUCT_ID_MIN
              && product_id <= IPMI_FUJITSU_PRODUCT_ID_MAX))
        {
          if (event_reading_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC) 
            {
              switch (sensor_type)
                {
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_I2C_BUS:
                  return ("OEM I2C Bus");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_SYSTEM_POWER_CONSUMPTION:
                  return ("OEM Power Consumption");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_STATUS:
                  return ("OEM Memory Status");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_CONFIG:
                  return ("OEM Memory Config");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY:
                  return ("OEM Memory");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_FAN_STATUS:
                  return ("OEM Fan Status");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_STATUS:
                  return ("OEM PSU Status");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_REDUNDANCY:
                  return ("OEM PSU Redundancy");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_COMMUNICATION:
                  return ("OEM Communication");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_FLASH:
                  return ("OEM Flash");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_EVENT:
                  return ("OEM Event");
                case IPMI_SENSOR_TYPE_OEM_FUJITSU_CONFIG_BACKUP:
                  return ("OEM Config Backup");
                default:
                  /* fall into generic case below */
                  break;
                }
            } 
          else if (event_reading_code == IPMI_EVENT_READING_TYPE_CODE_THRESHOLD)
            {
              /* Currently only one combination */
              if (sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_I2C_BUS)
                return ("OEM I2C Bus");
            }
          
        }
      
      return (ipmi_oem_sensor_type);
    }
  
  return (NULL);
}

int
ipmi_sensor_entity_sensor_name_string (ipmi_sdr_ctx_t sdr_ctx,
				       const void *sdr_record,
				       unsigned int sdr_record_len,
				       uint8_t sensor_number,
				       unsigned int flags,
				       char *buf,
				       unsigned int buflen)
{
  char id_string[IPMI_SDR_MAX_ID_STRING_LENGTH + 1];
  char device_id_string[IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH + 1];
  char *id_string_ptr = NULL;
  uint8_t entity_id, entity_instance, entity_instance_type;
  const char *entity_id_str;
  uint8_t record_type;
  unsigned int flags_mask = (IPMI_SENSOR_ENTITY_SENSOR_NAME_FLAGS_ALWAYS_OUTPUT_INSTANCE_NUMBER
			     | IPMI_SENSOR_ENTITY_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS);
  ipmi_sdr_ctx_t local_sdr_ctx = NULL;
  int rv = -1;

  if (((sdr_record_len && !sdr_record_len)
       || (!sdr_record && sdr_record_len))
      || (!sdr_ctx && !sdr_record_len && !sdr_record_len)
      || (flags & ~flags_mask)
      || !buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (sdr_ctx)
    {
      /* check that sdr_ctx is open for reading if supplied */
      if (ipmi_sdr_cache_first (sdr_ctx) < 0)
	{
	  SET_ERRNO (EINVAL);
	  return (-1);
	}

      local_sdr_ctx = sdr_ctx;
    }
  else
    {
      if (!(local_sdr_ctx = ipmi_sdr_ctx_create ()))
	{
	  SET_ERRNO (errno);
	  return (-1);
	}
    }

  memset (buf, '\0', buflen);

  if (ipmi_sdr_parse_record_id_and_type (local_sdr_ctx,
					 sdr_record,
					 sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }
  
  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      memset (id_string, '\0', IPMI_SDR_MAX_ID_STRING_LENGTH + 1);

      if (ipmi_sdr_parse_id_string (local_sdr_ctx,
				    sdr_record,
				    sdr_record_len,
				    id_string,
                                    IPMI_SDR_MAX_ID_STRING_LENGTH) < 0)
        {
	  SET_ERRNO (EINVAL);
	  goto cleanup;
        }

      id_string_ptr = id_string;
    }
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD
           || record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    {
      memset (device_id_string, '\0', IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH + 1);

      if (ipmi_sdr_parse_device_id_string (local_sdr_ctx,
					   sdr_record,
					   sdr_record_len,
                                           device_id_string,
                                           IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH) < 0)
        {
	  SET_ERRNO (EINVAL);
	  goto cleanup;
        }
      
      id_string_ptr = device_id_string;
    }
  else
    SET_ERRNO (EINVAL);

  if (ipmi_sdr_parse_entity_id_instance_type (local_sdr_ctx,
					      sdr_record,
					      sdr_record_len,
                                              &entity_id,
                                              &entity_instance,
                                              &entity_instance_type) < 0)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  /* Table 39-1
   *
   * "It is recommended that console software subtract 60h when
   * presenting device-relative Entity Instance values, and present
   * the Entity Instance number along with an ID for the device
   * providing the interface to the entity."
   *
   * achu: For the time being we do not output the device providing
   * the interface, only the right instance number.  Adjust later if
   * necessary.
   */

  if (IPMI_ENTITY_INSTANCE_DEVICE_RELATIVE (entity_instance))
    entity_instance -= IPMI_ENTITY_INSTANCE_DEVICE_RELATIVE_MIN;

  entity_id_str = ipmi_get_entity_id_string (entity_id);

  /* a few special cases, for entity_ids are special, the vendor has
   * specifically stated there is no "entity" associated with this sdr
   * record
   */
  if (entity_id == IPMI_ENTITY_ID_UNSPECIFIED
      || entity_id == IPMI_ENTITY_ID_OTHER
      || entity_id == IPMI_ENTITY_ID_UNKNOWN)
    {
      snprintf (buf,
                buflen,
                "%s",
                id_string_ptr);
    }
  else
    {
      if (sdr_ctx)
	{
	  if (ipmi_sdr_stats_compile (local_sdr_ctx) < 0)
	    {
	      SET_ERRNO (EINVAL);
	      goto cleanup;
	    }
	}

      if (sdr_ctx
	  && ipmi_sdr_stats_entity_instance_unique (local_sdr_ctx, entity_id) > 1)
        {
          /* special case if sensor sharing is involved */
          if ((record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
               || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
              && !(flags & IPMI_SENSOR_ENTITY_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS))
            {
              uint8_t share_count;
              uint8_t id_string_instance_modifier_type;
              uint8_t id_string_instance_modifier_offset;
              uint8_t entity_instance_sharing;

              if (ipmi_sdr_parse_sensor_record_sharing (local_sdr_ctx,
							sdr_record,
							sdr_record_len,
                                                        &share_count,
                                                        &id_string_instance_modifier_type,
                                                        &id_string_instance_modifier_offset,
                                                        &entity_instance_sharing) < 0)
                {
		  SET_ERRNO (EINVAL);
		  goto cleanup;
                }

              if (share_count > 1
                  && entity_instance_sharing == IPMI_SDR_ENTITY_INSTANCE_INCREMENTS_FOR_EACH_SHARED_RECORD)
                {
                  uint8_t sensor_number_base;
                  uint8_t sensor_number_offset;

                  if (ipmi_sdr_parse_sensor_number (local_sdr_ctx,
						    sdr_record,
						    sdr_record_len,
						    &sensor_number_base) < 0)
                    {
		      SET_ERRNO (EINVAL);
		      goto cleanup;
                    }

                  /* I guess it's a bug if the sensor number passed in is bad */
                  if (sensor_number >= sensor_number_base)
                    sensor_number_offset = sensor_number - sensor_number_base;
                  else
                    goto fallthrough;

                  if (id_string_instance_modifier_type == IPMI_SDR_ID_STRING_INSTANCE_MODIFIER_TYPE_ALPHA)
                    {
                      char modifierbuf[IPMI_SENSOR_UTIL_MODIFIER_BUFLEN];

                      memset (modifierbuf, '\0', IPMI_SENSOR_UTIL_MODIFIER_BUFLEN);
                      
                      /* IPMI spec example is:
                       *
                       * "If the modifier = alpha, offset=0
                       * corresponds to 'A', offset=25 corresponses to
                       * 'Z', and offset = 26 corresponds to 'AA', for
                       * offset=26 the sensors could be identified as:
                       * Temp AA, Temp AB, Temp AC."
                       *
                       * achu note: id_string_instance_modifier_type
                       * is a 7 bit field, so we cannot reach a
                       * situation of 'AAA' or 'AAB'.  The max is
                       * 'EX':
                       *
                       * 'A' + (127/26) = 4 => 'E'
                       * 'A' + (127 % 26) = 23 => 'X'
                       */

                      if ((id_string_instance_modifier_type + sensor_number_offset) < IPMI_SENSOR_UTIL_CHARS_IN_ALPHABET)
                        snprintf (buf,
                                  buflen,
                                  "%s %s %c",
                                  entity_id_str,
                                  id_string_ptr,
                                  'A' + ((id_string_instance_modifier_type + sensor_number_offset)/IPMI_SENSOR_UTIL_CHARS_IN_ALPHABET));
                      else
                        snprintf (buf,
                                  buflen,
                                  "%s %s %c%c",
                                  entity_id_str,
                                  id_string_ptr,
                                  'A' + ((id_string_instance_modifier_type + sensor_number_offset)/IPMI_SENSOR_UTIL_CHARS_IN_ALPHABET),
                                  'A' + (id_string_instance_modifier_type % IPMI_SENSOR_UTIL_CHARS_IN_ALPHABET));
                    }
                  else
                    {
                      /* IPMI spec example is:
                       *
                       * "Suppose sensor ID is 'Temp' for 'Temperature
                       * Sensor', share count = 3, ID string instance
                       * modifier = numeric, instance modifier offset
                       * = 5 - then the sensors oculd be identified
                       * as: Temp 5, Temp 6, Temp 7"
                       */
                      snprintf (buf,
                                buflen,
                                "%s %s %u",
                                entity_id_str,
                                id_string_ptr,
                                id_string_instance_modifier_offset + sensor_number_offset);
                    }

                  goto out;
                }
            }

        fallthrough:
          snprintf (buf,
                    buflen,
                    "%s %u %s",
                    entity_id_str,
                    entity_instance,
                    id_string_ptr);
        }
      else
	{
	  if (flags & IPMI_SENSOR_ENTITY_SENSOR_NAME_FLAGS_ALWAYS_OUTPUT_INSTANCE_NUMBER)
	    snprintf (buf,
		      buflen,
                    "%s %u %s",
		      entity_id_str,
		      entity_instance,
		      id_string_ptr);
	  else
	    snprintf (buf,
		      buflen,
		      "%s %s",
		      entity_id_str,
		      id_string_ptr);
	}
    }

 out:
  rv = 0;
 cleanup:
  if (!sdr_ctx)
    ipmi_sdr_ctx_destroy (local_sdr_ctx);
  return (rv);
}

int
ipmi_sensor_units_string (uint8_t sensor_units_percentage,
                          uint8_t sensor_units_modifier,
                          uint8_t sensor_units_rate,
                          uint8_t sensor_base_unit_type,
                          uint8_t sensor_modifier_unit_type,
                          char *buf,
                          unsigned int buflen,
                          unsigned int abbreviated_units_flag)
{
  const char **sensor_units = NULL;
  int offset = 0;
  int rv = -1;

  if (!IPMI_SDR_PERCENTAGE_VALID(sensor_units_percentage)
      || !IPMI_SDR_MODIFIER_UNIT_VALID(sensor_units_modifier)
      || !IPMI_SENSOR_RATE_UNIT_VALID(sensor_units_rate)
      || !IPMI_SENSOR_UNIT_VALID(sensor_base_unit_type)
      || !IPMI_SENSOR_UNIT_VALID(sensor_modifier_unit_type)
      || !buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* achu: I assume this must be the case */
  if (sensor_units_modifier != IPMI_SDR_MODIFIER_UNIT_NONE
      && sensor_units_rate != IPMI_SENSOR_RATE_UNIT_NONE)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* achu: I don't understand percentages exactly, I'll just
   * assume it's a percent sign infront of everything else.
   */
  if (sensor_units_percentage == IPMI_SDR_PERCENTAGE_YES)
    {
      /* Special case, nothing to add to the end of the percentage,
       * and the base unit is unspecified, just output '%' only.
       */
      if (sensor_units_modifier == IPMI_SDR_MODIFIER_UNIT_NONE
	  && sensor_units_rate == IPMI_SENSOR_RATE_UNIT_NONE
	  && sensor_base_unit_type == IPMI_SENSOR_UNIT_UNSPECIFIED)
	{
	  rv = snprintf (buf,
			 buflen,
			 "%%");
	  return (rv);
	}
      else
	offset = snprintf (buf,
			   buflen,
			   "%% ");
    }

  if (abbreviated_units_flag)
    sensor_units = (const char **)ipmi_sensor_units_abbreviated;
  else
    sensor_units = (const char **)ipmi_sensor_units;

  if (sensor_units_modifier == IPMI_SDR_MODIFIER_UNIT_NONE
      && sensor_units_rate == IPMI_SENSOR_RATE_UNIT_NONE)
    {
      rv = snprintf (buf + offset,
                     buflen,
                     "%s",
                     sensor_units[sensor_base_unit_type]);
      return (rv);
    }
  
  if (sensor_units_rate != IPMI_SENSOR_RATE_UNIT_NONE)
    {
      /* Special case, RPM is inheritly per minute
       *
       * If vendor specifies a rate other than "per minute", that's
       * probably a bug in their SDR.
       */
      if (sensor_base_unit_type == IPMI_SENSOR_UNIT_RPM
          && sensor_units_rate == IPMI_SENSOR_RATE_UNIT_PER_MINUTE)
        rv = snprintf (buf + offset,
                       buflen,
                       "%s",
                       sensor_units[sensor_base_unit_type]);
      else
        rv = snprintf (buf + offset,
                       buflen,
                       "%s %s",
                       sensor_units[sensor_base_unit_type],
                       ipmi_sensor_rate_units[sensor_units_rate]);
      return (rv);
    }
  
  /* else sensor_units_modifier != IPMI_SDR_MODIFIER_UNIT_NONE */

  if (sensor_units_modifier == IPMI_SDR_MODIFIER_UNIT_DIVIDE)
    rv = snprintf (buf + offset,
                   buflen,
                   "%s / %s",
                   sensor_units[sensor_base_unit_type],
                   sensor_units[sensor_modifier_unit_type]);
  else
    rv = snprintf (buf + offset,
                   buflen,
                   "%s * %s",
                   sensor_units[sensor_base_unit_type],
                   sensor_units[sensor_modifier_unit_type]);

  return (rv);
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

  switch (linearization)
    {
    case IPMI_SDR_LINEARIZATION_LN:
      dval = log (dval);
      break;
    case IPMI_SDR_LINEARIZATION_LOG10:
      dval = log10 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_LOG2:
      dval = log2 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_E:
      dval = exp (dval);
      break;
    case IPMI_SDR_LINEARIZATION_EXP10:
      dval = exp10 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_EXP2:
      dval = exp2 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_INVERSE:
      if (dval != 0.0)
	dval = 1.0 / dval;
      break;
    case IPMI_SDR_LINEARIZATION_SQR:
      dval = pow (dval, 2.0);
      break;
    case IPMI_SDR_LINEARIZATION_CUBE:
      dval = pow (dval, 3.0);
      break;
    case IPMI_SDR_LINEARIZATION_SQRT:
      dval = sqrt (dval);
      break;
    case IPMI_SDR_LINEARIZATION_CUBERT:
      dval = cbrt (dval);
      break;
    }

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
  switch (linearization)
    {
    case IPMI_SDR_LINEARIZATION_LN:
      dval = exp (dval);
      break;
    case IPMI_SDR_LINEARIZATION_LOG10:
      dval = exp10 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_LOG2:
      dval = exp2 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_E:
      dval = (log (dval)/log (exp (1.0)));
      break;
    case IPMI_SDR_LINEARIZATION_EXP10:
      dval = (log (dval)/log (10));
      break;
    case IPMI_SDR_LINEARIZATION_EXP2:
      dval = (log (dval)/log (2));
      break;
    case IPMI_SDR_LINEARIZATION_INVERSE:
      if (dval != 0.0)
        dval = 1.0 / dval;
      break;
    case IPMI_SDR_LINEARIZATION_SQR:
      dval = sqrt (dval);
      break;
    case IPMI_SDR_LINEARIZATION_CUBE:
      dval = cbrt (dval);
      break;
    case IPMI_SDR_LINEARIZATION_SQRT:
      dval = pow (dval, 2.0);
      break;
    case IPMI_SDR_LINEARIZATION_CUBERT:
      dval = pow (dval, 3.0);
      break;
    }

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

  switch (linearization)
    {
    case IPMI_SDR_LINEARIZATION_LN:
      dval = log (dval);
      break;
    case IPMI_SDR_LINEARIZATION_LOG10:
      dval = log10 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_LOG2:
      dval = log2 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_E:
      dval = exp (dval);
      break;
    case IPMI_SDR_LINEARIZATION_EXP10:
      dval = exp10 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_EXP2:
      dval = exp2 (dval);
      break;
    case IPMI_SDR_LINEARIZATION_INVERSE:
      if (dval != 0.0)
	dval = 1.0 / dval;
      break;
    case IPMI_SDR_LINEARIZATION_SQR:
      dval = pow (dval, 2.0);
      break;
    case IPMI_SDR_LINEARIZATION_CUBE:
      dval = pow (dval, 3.0);
      break;
    case IPMI_SDR_LINEARIZATION_SQRT:
      dval = sqrt (dval);
      break;
    case IPMI_SDR_LINEARIZATION_CUBERT:
      dval = cbrt (dval);
      break;
    }

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

