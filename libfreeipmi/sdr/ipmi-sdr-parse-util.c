/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sdr/ipmi-sdr.h"

#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-entity-ids-spec.h"
#include "freeipmi/util/ipmi-entity-ids-util.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "freeipmi-portability.h"

#define IPMI_SDR_MODIFIER_BUFLEN 1024

#define IPMI_SDR_CHARS_IN_ALPHABET 26

#define IPMI_SDR_ENTITY_NAME_BUFLEN 1024

static int
_get_shared_sensor_name (ipmi_sdr_ctx_t ctx,
			 const void *sdr_record,
			 unsigned int sdr_record_len,
			 uint8_t sensor_number,
			 const char *id_string,
			 char *buf,
			 unsigned int buflen)
{
  uint8_t share_count;
  uint8_t id_string_instance_modifier_type;
  uint8_t id_string_instance_modifier_offset;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (id_string);

  if (ipmi_sdr_parse_sensor_record_sharing (ctx,
					    sdr_record,
					    sdr_record_len,
					    &share_count,
					    &id_string_instance_modifier_type,
					    &id_string_instance_modifier_offset,
					    NULL) < 0)
    return (-1);
      
  if (share_count > 1)
    {
      uint8_t sensor_number_base;
      uint8_t sensor_number_offset;
      
      if (ipmi_sdr_parse_sensor_number (ctx,
					sdr_record,
					sdr_record_len,
					&sensor_number_base) < 0)
	return (-1);
      
      /* I guess it's a bug if the sensor number passed in is bad */
      if (sensor_number >= sensor_number_base)
	sensor_number_offset = sensor_number - sensor_number_base;
      else
	goto fallthrough;
      
      if (id_string_instance_modifier_type == IPMI_SDR_ID_STRING_INSTANCE_MODIFIER_TYPE_ALPHA)
	{
	  char modifierbuf[IPMI_SDR_MODIFIER_BUFLEN];
	  
	  memset (modifierbuf, '\0', IPMI_SDR_MODIFIER_BUFLEN);
              
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
	      
	  if ((id_string_instance_modifier_type + sensor_number_offset) < IPMI_SDR_CHARS_IN_ALPHABET)
	    snprintf (buf,
		      buflen,
		      "%s %c",
		      id_string,
		      'A' + ((id_string_instance_modifier_type + sensor_number_offset)/IPMI_SDR_CHARS_IN_ALPHABET));
	  else
	    snprintf (buf,
		      buflen,
		      "%s %c%c",
		      id_string,
		      'A' + ((id_string_instance_modifier_type + sensor_number_offset)/IPMI_SDR_CHARS_IN_ALPHABET),
		      'A' + (id_string_instance_modifier_type % IPMI_SDR_CHARS_IN_ALPHABET));
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
		    "%s %u",
		    id_string,
		    id_string_instance_modifier_offset + sensor_number_offset);
	}
	  
      return (0);
    }
  
 fallthrough:
  snprintf (buf,
	    buflen,
	    "%s",
	    id_string);

  return (0);
}

int
ipmi_sdr_parse_sensor_name (ipmi_sdr_ctx_t ctx,
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
  uint8_t record_type;
  unsigned int flags_mask = (IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS);

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  if (((sdr_record_len && !sdr_record_len)
       || (!sdr_record && sdr_record_len))
      || (flags & ~flags_mask)
      || !buf
      || !buflen)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  memset (buf, '\0', buflen);

  if (ipmi_sdr_parse_record_id_and_type (ctx,
					 sdr_record,
					 sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    return (-1);

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      memset (id_string, '\0', IPMI_SDR_MAX_ID_STRING_LENGTH + 1);

      if (ipmi_sdr_parse_id_string (ctx,
				    sdr_record,
				    sdr_record_len,
				    id_string,
                                    IPMI_SDR_MAX_ID_STRING_LENGTH) < 0)
	return (-1);

      id_string_ptr = id_string;
    }
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD
	   || record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD
           || record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    {
      memset (device_id_string, '\0', IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH + 1);

      if (ipmi_sdr_parse_device_id_string (ctx,
					   sdr_record,
					   sdr_record_len,
                                           device_id_string,
                                           IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH) < 0)
	return (-1);
      
      id_string_ptr = device_id_string;
    }
  else
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      return (-1);
    }

  /* special case if sensor sharing is involved */
  if ((record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
       || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
      && !(flags & IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS))
    {
      if (_get_shared_sensor_name (ctx,
				   sdr_record,
				   sdr_record_len,
				   sensor_number,
				   id_string_ptr,
				   buf,
				   buflen) < 0)
	return (-1);
    }
  else
    snprintf (buf,
	      buflen,
	      "%s",
	      id_string_ptr);

  return (0);
}

int
ipmi_sdr_parse_entity_sensor_name (ipmi_sdr_ctx_t ctx,
				   const void *sdr_record,
				   unsigned int sdr_record_len,
				   uint8_t sensor_number,
				   unsigned int flags,
				   char *buf,
				   unsigned int buflen)
{
  char id_string[IPMI_SDR_MAX_ID_STRING_LENGTH + 1];
  char device_id_string[IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH + 1];
  char entity_name_buf[IPMI_SDR_ENTITY_NAME_BUFLEN + 1];
  char *id_string_ptr = NULL;
  uint8_t entity_id, entity_instance, entity_instance_type;
  const char *entity_id_str;
  uint8_t record_type;
  unsigned int flags_mask = (IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS
			     | IPMI_SDR_SENSOR_NAME_FLAGS_ALWAYS_OUTPUT_INSTANCE_NUMBER);

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  if (((sdr_record_len && !sdr_record_len)
       || (!sdr_record && sdr_record_len))
      || (flags & ~flags_mask)
      || !buf
      || !buflen)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  memset (buf, '\0', buflen);
  memset (entity_name_buf, '\0', IPMI_SDR_ENTITY_NAME_BUFLEN + 1);

  if (ipmi_sdr_parse_record_id_and_type (ctx,
					 sdr_record,
					 sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    return (-1);

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      memset (id_string, '\0', IPMI_SDR_MAX_ID_STRING_LENGTH + 1);

      if (ipmi_sdr_parse_id_string (ctx,
				    sdr_record,
				    sdr_record_len,
				    id_string,
                                    IPMI_SDR_MAX_ID_STRING_LENGTH) < 0)
	return (-1);

      id_string_ptr = id_string;
    }
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD
           || record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    {
      memset (device_id_string, '\0', IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH + 1);

      if (ipmi_sdr_parse_device_id_string (ctx,
					   sdr_record,
					   sdr_record_len,
                                           device_id_string,
                                           IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH) < 0)
	return (-1);
      
      id_string_ptr = device_id_string;
    }
  else
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      return (-1);
    }

  if (ipmi_sdr_parse_entity_id_instance_type (ctx,
					      sdr_record,
					      sdr_record_len,
                                              &entity_id,
                                              &entity_instance,
                                              &entity_instance_type) < 0)
    return (-1);

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
    snprintf (buf,
	      buflen,
	      "%s",
	      id_string_ptr);
  else
    {
      if (ipmi_sdr_stats_compile (ctx) < 0)
	return (-1);

      if (ipmi_sdr_stats_entity_instance_unique (ctx, entity_id) > 1)
        {
          /* special case if sensor sharing is involved */
          if ((record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
               || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
              && !(flags & IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS))
            {
	      uint8_t share_count;
	      uint8_t entity_instance_sharing;
	      char sensor_name_buf[IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1];

	      if (ipmi_sdr_parse_sensor_record_sharing (ctx,
							sdr_record,
							sdr_record_len,
							&share_count,
							NULL,
							NULL,
							&entity_instance_sharing) < 0)
		return (-1);
      
	      if (share_count > 1
		  && entity_instance_sharing == IPMI_SDR_ENTITY_INSTANCE_INCREMENTS_FOR_EACH_SHARED_RECORD)
		{
		  uint8_t sensor_number_base;
      
		  if (ipmi_sdr_parse_sensor_number (ctx,
						    sdr_record,
						    sdr_record_len,
						    &sensor_number_base) < 0)
		    return (-1);
      
		  /* I guess it's a bug if the sensor number passed in is bad */
		  if (sensor_number >= sensor_number_base)
		    entity_instance += (sensor_number - sensor_number_base);
		  else
		    goto fallthrough;
		}

	      memset (sensor_name_buf, '\0', IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1);
	      
	      if (_get_shared_sensor_name (ctx,
					   sdr_record,
					   sdr_record_len,
					   sensor_number,
					   id_string_ptr,
					   sensor_name_buf,
					   IPMI_SDR_MAX_SENSOR_NAME_LENGTH) < 0)
		return (-1);

	      snprintf (entity_name_buf,
			IPMI_SDR_ENTITY_NAME_BUFLEN,
			"%s %u",
			entity_id_str,
			entity_instance);

	      /* In odd chance the strings end up identical */
	      if (!strcasecmp (entity_name_buf, sensor_name_buf))
		snprintf (buf,
			  buflen,
			  "%s",
			  sensor_name_buf);
	      else
		snprintf (buf,
			  buflen,
			  "%s %s",
			  entity_name_buf,
			  sensor_name_buf);
	    }
	  else
	    {
	    fallthrough:
	      snprintf (entity_name_buf,
			IPMI_SDR_ENTITY_NAME_BUFLEN,
			"%s %u",
			entity_id_str,
			entity_instance);
	      
	      /* In odd chance the strings end up identical */
	      if (!strcasecmp (entity_name_buf, id_string_ptr))
		snprintf (buf,
			  buflen,
			  "%s",
			  id_string_ptr);
	      else
		snprintf (buf,
			  buflen,
			  "%s %s",
			  entity_name_buf,
			  id_string_ptr);
	    }
	}
      else
	{
	  if (flags & IPMI_SDR_SENSOR_NAME_FLAGS_ALWAYS_OUTPUT_INSTANCE_NUMBER)
	    {
	      snprintf (entity_name_buf,
			IPMI_SDR_ENTITY_NAME_BUFLEN,
			"%s %u",
			entity_id_str,
			entity_instance);
	      
	      /* In odd chance the strings end up identical */
	      if (!strcasecmp (entity_name_buf, id_string_ptr))
		snprintf (buf,
			  buflen,
			  "%s",
			  id_string_ptr);
	      else
		snprintf (buf,
			  buflen,
			  "%s %s",
			  entity_name_buf,
			  id_string_ptr);
	    }
	  else
	    {
	      /* In odd chance the strings end up identical */
	      if (!strcasecmp (entity_id_str, id_string_ptr))
		snprintf (buf,
			  buflen,
			  "%s",
			  id_string_ptr);
	      else
		snprintf (buf,
			  buflen,
			  "%s %s",
			  entity_id_str,
			  id_string_ptr);
	    }
	}
    }

  return (0);
}
