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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "tool-sensor-common.h"
#include "tool-sdr-cache-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define RECORD_ID_BUFLEN       64

#define SENSOR_TYPE_BUFLEN     1024

#define SENSOR_UNITS_BUFLEN    1024

#define SENSOR_FMT_BUFLEN      1024

#define SENSOR_MODIFIER_BUFLEN 1024

#define SENSOR_CHARS_IN_ALPHA  26

#define SENSORS_SENSOR_NAME_LENGTH 16

static void
_str_replace_char (char *str, char chr, char with)
{
  char *p = NULL;
  char *s = NULL;
  
  assert (str);
  
  for (s = str;
       (p = strchr (s, chr));
       s = p + 1)
    *p = with;
}

const char *
get_sensor_type_output_string (unsigned int sensor_type)
{
  const char *sensor_type_str;

  if ((sensor_type_str = ipmi_get_sensor_type_string (sensor_type)))
    return (sensor_type_str);

  return (UNRECOGNIZED_SENSOR_TYPE);
}

const char * 
get_oem_sensor_type_output_string (uint8_t sensor_type,
                                   uint8_t event_reading_code,
                                   uint32_t manufacturer_id,
                                   uint16_t product_id)
{
  const char *sensor_type_str;

  if ((sensor_type_str = ipmi_get_oem_sensor_type_string (sensor_type,
                                                          event_reading_code,
                                                          manufacturer_id,
                                                          product_id)))
    return (sensor_type_str);
  
  return (UNRECOGNIZED_SENSOR_TYPE);
}

static void
_get_sensor_type_cmdline_string (char *sensor_type)
{
  assert (sensor_type);

  _str_replace_char (sensor_type, ' ', '_');
  _str_replace_char (sensor_type, '/', '_');
}

static int
_output_sensor_type (const char *sensor_type_str)
{
  char *tmpstr = NULL;

  assert (sensor_type_str);

  if (!(tmpstr = (char *)strdup (sensor_type_str)))
    {
      perror ("strdup");
      return (-1);
    }

  _get_sensor_type_cmdline_string (tmpstr);
  
  printf ("%s\n", tmpstr);
  
  free (tmpstr);
  return (0);
}

int
parse_sensor_types (const char *special_string,
		    char sensor_types[MAX_SENSOR_TYPES][MAX_SENSOR_TYPES_STRING_LENGTH+1],
		    unsigned int *sensor_types_length,
		    const char *arg)
{
  char *strtmp;
  char *tok;

  assert (special_string);
  assert (sensor_types);
  assert (sensor_types_length);
  assert (arg);

  if (!(strtmp = strdup (arg)))
    {
      perror (arg);
      return (-1);
    }

  tok = strtok (strtmp, " ,");
  while (tok && (*sensor_types_length) < MAX_SENSOR_TYPES)
    {
      if (!strcasecmp (tok, special_string))
	{
	  (*sensor_types_length) = 0;
	  break;
	}
      strncpy (sensor_types[(*sensor_types_length)],
	       tok,
	       MAX_SENSOR_TYPES_STRING_LENGTH);
      (*sensor_types_length)++;
      tok = strtok (NULL, " ,");
    }

  free (strtmp);
  return (0);
}

int
list_sensor_types (void)
{
  unsigned int i;

  for (i = IPMI_SENSOR_TYPE_TEMPERATURE; i <= IPMI_SENSOR_TYPE_FRU_STATE; i++)
    {
      assert (ipmi_sensor_types[i]);

      if (_output_sensor_type (ipmi_sensor_types[i]) < 0)
        return (-1);
    }
  
  if (_output_sensor_type (ipmi_oem_sensor_type) < 0)
    return (-1);
  
  return (0);
}

int
valid_sensor_types (char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                    unsigned int sensor_types_length)
{
  unsigned int i;

  assert (sensor_types);
  assert (sensor_types_length);

  for (i = 0; i < sensor_types_length; i++)
    {
      int j = 0;
      int found = 0;
      int value;
      char *ptr = NULL;

      errno = 0;
      value = strtol (sensor_types[i], &ptr, 0);

      if (!errno
          && !ptr[0]
          && (IPMI_SENSOR_TYPE_VALID ((uint8_t)value)
              || IPMI_SENSOR_TYPE_IS_OEM ((uint8_t)value)))
        found++;

      if (!found)
        {
          while (ipmi_sensor_types[j])
            {
              char sensor_type_cmdline[MAX_SENSOR_TYPES_STRING_LENGTH];
              
              strcpy (sensor_type_cmdline, ipmi_sensor_types[j]);
              _get_sensor_type_cmdline_string (sensor_type_cmdline);
              
              if (!strcasecmp (sensor_types[i], ipmi_sensor_types[j])
                  || !strcasecmp (sensor_types[i], sensor_type_cmdline))
                {
                  found++;
                  break;
                }
              j++;
            }
        }
      
      if (!found)
	{
	  char sensor_type_cmdline[MAX_SENSOR_TYPES_STRING_LENGTH];
          
	  strcpy (sensor_type_cmdline, ipmi_oem_sensor_type);
	  _get_sensor_type_cmdline_string (sensor_type_cmdline);
              
	  if (!strcasecmp (sensor_types[i], ipmi_oem_sensor_type)
	      || !strcasecmp (sensor_types[i], sensor_type_cmdline))
	    found++;
	}
      
      if (!found)
        {
          fprintf (stderr,
		   "invalid sensor type '%s'\n",
		   sensor_types[i]);
          return (-1);
        }
    }

  return (0);
}

int
get_sensor_units_output_string (pstdout_state_t pstate,
                                ipmi_sdr_ctx_t sdr_ctx,
                                char *sensor_units_buf,
                                unsigned int sensor_units_buflen,
                                unsigned int non_abbreviated_units_flag)
{
  uint8_t sensor_units_percentage;
  uint8_t sensor_units_modifier;
  uint8_t sensor_units_rate;
  uint8_t sensor_base_unit_type;
  uint8_t sensor_modifier_unit_type;
  int sensor_units_ret = 0;
  int rv = -1;

  assert (sdr_ctx);
  assert (sensor_units_buf);
  assert (sensor_units_buflen);

  if (ipmi_sdr_parse_sensor_units (sdr_ctx,
                                   NULL,
				   0,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_units: %s\n",
                       ipmi_sdr_ctx_errormsg (sdr_ctx));
      goto cleanup;
    }
  
  memset (sensor_units_buf, '\0', sensor_units_buflen);
  sensor_units_ret = ipmi_sensor_units_string (sensor_units_percentage,
                                               sensor_units_modifier,
                                               sensor_units_rate,
                                               sensor_base_unit_type,
                                               sensor_modifier_unit_type,
                                               sensor_units_buf,
                                               sensor_units_buflen,
                                               !non_abbreviated_units_flag);
  
  if (sensor_units_ret <= 0)
    snprintf (sensor_units_buf,
              sensor_units_buflen,
              "%s",
              ipmi_sensor_units[IPMI_SENSOR_UNIT_UNSPECIFIED]);
  
  rv = 0;
 cleanup:
  return rv;
}

static int
_sensor_type_strcmp (pstdout_state_t pstate,
                     const char *sensor_type_str_input,
                     uint8_t sensor_type)
{
  const char *sensor_type_str;
  char *tmpstr = NULL;
  int rv = -1;
  int value;
  char *ptr = NULL;

  assert (sensor_type_str_input);
  
  errno = 0;
  value = strtol (sensor_type_str_input, &ptr, 0);

  /* should be validated earlier - this in the only check to do */
  if (!errno
      && !ptr[0])
    {
      if ((uint8_t)value == sensor_type)
        rv = 1;
      else
        rv = 0;
      goto cleanup;
    }

  /* Don't use get_sensor_type_output_string() - want NULL if invalid */
  sensor_type_str = ipmi_get_sensor_type_string (sensor_type);
  
  if (!sensor_type_str)
    { 
      rv = 0;
      goto cleanup;
    } 
  
  if (!(tmpstr = strdup (sensor_type_str)))
    { 
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "strdup: %s\n",
                       strerror (errno));
      goto cleanup;
    } 
  
  _get_sensor_type_cmdline_string (tmpstr);
  
  if (!strcasecmp (sensor_type_str_input, sensor_type_str)
      || !strcasecmp (sensor_type_str_input, tmpstr))
    rv = 1;
  else
    rv = 0;
  
 cleanup:
  free (tmpstr);
  return (rv);
}

int
sensor_type_listed (pstdout_state_t pstate,
                    uint8_t sensor_type,
                    char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                    unsigned int sensor_types_length)
{
  unsigned int i;

  assert (sensor_types);
  assert (sensor_types_length);

  for (i = 0; i < sensor_types_length; i++)
    {
      int ret;

      if ((ret = _sensor_type_strcmp (pstate,
                                      sensor_types[i],
                                      sensor_type)) < 0)
        return (-1);

      if (ret)
        return (1);
    }
  
  return (0);
}

int
sensor_type_listed_sdr (pstdout_state_t pstate,
                        ipmi_sdr_ctx_t sdr_ctx,
                        char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                        unsigned int sensor_types_length)
{
  uint8_t record_type;
  uint8_t sensor_type;

  assert (sdr_ctx);
  assert (sensor_types);
  assert (sensor_types_length);

  if (ipmi_sdr_parse_record_id_and_type (sdr_ctx,
                                         NULL,
                                         0,
                                         NULL,
                                         &record_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_ctx_errormsg (sdr_ctx));
      return (-1);
    }

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    return (0);

  if (ipmi_sdr_parse_sensor_type (sdr_ctx,
				  NULL,
				  0,
                                  &sensor_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_ctx_errormsg (sdr_ctx));
      return (-1);
    }

  return (sensor_type_listed (pstate,
                              sensor_type,
                              sensor_types,
                              sensor_types_length));
}

static void
_sensor_column_width_init (struct sensor_column_width *column_width)
{
  assert (column_width);

  column_width->record_id = 0;
  column_width->sensor_name = 0;
  column_width->sensor_type = 0;
  column_width->sensor_units = 0;
}

static void
_sensor_column_width_finish (struct sensor_column_width *column_width)
{
  assert (column_width);

  if (column_width->record_id < strlen(SENSORS_HEADER_RECORD_ID_STR))
    column_width->record_id = strlen(SENSORS_HEADER_RECORD_ID_STR);
  if (column_width->sensor_name < strlen(SENSORS_HEADER_NAME_STR))
    column_width->sensor_name = strlen(SENSORS_HEADER_NAME_STR);
  if (column_width->sensor_type < strlen(SENSORS_HEADER_TYPE_STR))
    column_width->sensor_type = strlen(SENSORS_HEADER_TYPE_STR);
  if (column_width->sensor_units < strlen(SENSORS_HEADER_UNITS_STR))
    column_width->sensor_units = strlen(SENSORS_HEADER_UNITS_STR);
}

static int
_store_column_widths (pstdout_state_t pstate,
                      ipmi_sdr_ctx_t sdr_ctx,
                      uint8_t sensor_number,
                      unsigned int non_abbreviated_units,
		      unsigned int shared_sensors,
                      unsigned int count_event_only_records,
                      unsigned int count_device_locator_records,
                      unsigned int count_oem_records,
		      int entity_sensor_names,
                      struct sensor_column_width *column_width)
{
  char sensor_name[IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1];
  char record_id_buf[RECORD_ID_BUFLEN + 1];
  uint16_t record_id;
  uint8_t record_type;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  int len;

  assert (sdr_ctx);
  assert (column_width);

  if (ipmi_sdr_parse_record_id_and_type (sdr_ctx,
					 NULL,
					 0,
                                         &record_id,
                                         &record_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_ctx_errormsg (sdr_ctx));
      return (-1);
    }

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      && (!count_event_only_records
          || record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
      && (!count_device_locator_records
          || (record_type != IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD
	      && record_type != IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD
              && record_type != IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD))
      && (!count_oem_records
          || record_type != IPMI_SDR_FORMAT_OEM_RECORD))
    return (0);

  memset (record_id_buf, '\0', RECORD_ID_BUFLEN + 1);
  snprintf (record_id_buf, RECORD_ID_BUFLEN, "%u", record_id);

  len = strlen (record_id_buf);
  if (len > column_width->record_id)
    column_width->record_id = len;

  /* Done, only need to calculate record id column for OEM records */
  if (record_type == IPMI_SDR_FORMAT_OEM_RECORD)
    return (0);

  memset (sensor_name, '\0', IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1);

  if (entity_sensor_names)
    {
      if (ipmi_sdr_parse_entity_sensor_name (sdr_ctx,
					     NULL,
					     0,
					     sensor_number,
					     !shared_sensors ? IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS : 0,
					     sensor_name,
					     IPMI_SDR_MAX_SENSOR_NAME_LENGTH) < 0)
	{
	  if (ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD)
	    goto normal_sensor_name;

	  PSTDOUT_FPRINTF (pstate,
			   stderr,
			   "ipmi_sdr_parse_entity_sensor_name: %s\n",
			   ipmi_sdr_ctx_errormsg (sdr_ctx));
	  return (-1);
	}

      len = strlen (sensor_name);
      if (len > column_width->sensor_name)
        column_width->sensor_name = len;
    }
  else
    {
    normal_sensor_name:
      if (ipmi_sdr_parse_sensor_name (sdr_ctx,
				      NULL,
				      0,
				      sensor_number,
				      !shared_sensors ? IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS : 0,
				      sensor_name,
				      IPMI_SDR_MAX_SENSOR_NAME_LENGTH) < 0)
	{
	  PSTDOUT_FPRINTF (pstate,
			   stderr,
			   "ipmi_sdr_parse_sensor_name: %s\n",
			   ipmi_sdr_ctx_errormsg (sdr_ctx));
	  return (-1);
	}

      len = strlen (sensor_name);
      if (len > column_width->sensor_name)
        column_width->sensor_name = len;
    }

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      if (ipmi_sdr_parse_sensor_type (sdr_ctx,
				      NULL,
				      0,
                                      &sensor_type) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sdr_parse_sensor_type: %s\n",
                           ipmi_sdr_ctx_errormsg (sdr_ctx));
          return (-1);
        }

      len = strlen (get_sensor_type_output_string (sensor_type));
      if (len > column_width->sensor_type)
        column_width->sensor_type = len;

      if (ipmi_sdr_parse_event_reading_type_code (sdr_ctx,
						  NULL,
						  0,
                                                  &event_reading_type_code) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sdr_parse_event_reading_type_code: %s\n",
                           ipmi_sdr_ctx_errormsg (sdr_ctx));
          return (-1);
        }

      if (ipmi_event_reading_type_code_class (event_reading_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
        {
          char sensor_units_buf[SENSOR_UNITS_BUFLEN + 1];
          
          memset (sensor_units_buf, '\0', SENSOR_UNITS_BUFLEN + 1);
          if (get_sensor_units_output_string (pstate,
                                              sdr_ctx,
                                              sensor_units_buf,
                                              SENSOR_UNITS_BUFLEN,
                                              non_abbreviated_units) < 0)
            return (-1);
          
          len = strlen (sensor_units_buf);
          if (len > column_width->sensor_units)
            column_width->sensor_units = len;
        }
    }
  
  return (0);
}

static int
_store_column_widths_shared (pstdout_state_t pstate,
                             ipmi_sdr_ctx_t sdr_ctx,
                             unsigned int non_abbreviated_units,
                             unsigned int count_event_only_records,
                             unsigned int count_device_locator_records,
                             unsigned int count_oem_records,
			     int entity_sensor_names,
                             struct sensor_column_width *column_width)
{
  uint8_t record_type;
  uint8_t share_count;
  uint8_t sensor_number_base;
  int i;

  assert (sdr_ctx);
  assert (column_width);

  if (ipmi_sdr_parse_record_id_and_type (sdr_ctx,
					 NULL,
					 0,
                                         NULL,
                                         &record_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_ctx_errormsg (sdr_ctx));
      return (-1);
    }

  if (record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      if (_store_column_widths (pstate,
                                sdr_ctx,
                                0,
                                non_abbreviated_units,
				0, /* shared_sensors */
                                count_event_only_records,
                                count_device_locator_records,
                                count_oem_records,
				entity_sensor_names,
                                column_width) < 0)
        return (-1);
      return (0);
    }
  
  if (ipmi_sdr_parse_sensor_record_sharing (sdr_ctx,
					    NULL,
					    0,
                                            &share_count,
                                            NULL,
                                            NULL,
                                            NULL) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_record_sharing: %s\n",
                       ipmi_sdr_ctx_errormsg (sdr_ctx));
      return (-1);
    }
  
  if (share_count <= 1)
    {
      if (_store_column_widths (pstate,
                                sdr_ctx,
                                0,
                                non_abbreviated_units,
				0, /* shared_sensors */
                                count_event_only_records,
                                count_device_locator_records,
                                count_oem_records,
				entity_sensor_names,
                                column_width) < 0)
        return (-1);
      return (0);
    }

  if (ipmi_sdr_parse_sensor_number (sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number_base) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (sdr_ctx));
      return (-1);
    }

  
  /* IPMI spec gives the following example:
   *
   * "If the starting sensor number was 10, and the share
   * count was 3, then sensors 10, 11, and 12 would share
   * the record"
   */
  for (i = 0; i < share_count; i++)
    {
      uint8_t sensor_number;
      
      sensor_number = sensor_number_base + i;
      
      if (_store_column_widths (pstate,
                                sdr_ctx,
                                sensor_number,
                                non_abbreviated_units,
				1, /* shared_sensors */
                                count_event_only_records,
                                count_device_locator_records,
                                count_oem_records,
				entity_sensor_names,
                                column_width) < 0)
        return (-1);
    }

  return (0);
}

int
calculate_column_widths (pstdout_state_t pstate,
                         ipmi_sdr_ctx_t sdr_ctx,
                         char sensor_types[][MAX_SENSOR_TYPES_STRING_LENGTH+1],
                         unsigned int sensor_types_length,
                         unsigned int record_ids[],
                         unsigned int record_ids_length,
                         unsigned int non_abbreviated_units,
                         unsigned int shared_sensors,
                         unsigned int count_event_only_records,
                         unsigned int count_device_locator_records,
                         unsigned int count_oem_records,
			 int entity_sensor_names,
                         struct sensor_column_width *column_width)
{
  int rv = -1;
  int i;

  assert (sdr_ctx);
  assert (column_width);

  _sensor_column_width_init (column_width);

  if (record_ids && record_ids_length)
    {
      for (i = 0; i < record_ids_length; i++)
        {
          if (ipmi_sdr_cache_search_record_id (sdr_ctx, record_ids[i]) < 0)
            {
              if (ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_NOT_FOUND)
                continue;
              else
                {
                  PSTDOUT_FPRINTF (pstate,
                                   stderr,
                                   "ipmi_sdr_cache_search_record_id: %s\n",
                                   ipmi_sdr_ctx_errormsg (sdr_ctx));
                  goto cleanup;
                }
            }

          if (shared_sensors)
            {
              if (_store_column_widths_shared (pstate,
                                               sdr_ctx,
                                               non_abbreviated_units,
                                               count_event_only_records,
                                               count_device_locator_records,
                                               count_oem_records,
					       entity_sensor_names,
                                               column_width) < 0)
                goto cleanup;
            }
          else
            {
              if (_store_column_widths (pstate,
                                        sdr_ctx,
                                        0,
                                        non_abbreviated_units,
					0, /* shared_sensors */
                                        count_event_only_records,
                                        count_device_locator_records,
                                        count_oem_records,
					entity_sensor_names,
                                        column_width) < 0)
                goto cleanup;
            }
        }
    }
  else
    {
      uint16_t record_count;

      if (ipmi_sdr_cache_record_count (sdr_ctx, &record_count) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sdr_cache_record_count: %s\n",
                           ipmi_sdr_ctx_errormsg (sdr_ctx));
          goto cleanup;
        }

      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (sdr_ctx))
        {
          int ret;

          if (sensor_types && sensor_types_length)
            {
              if ((ret = sensor_type_listed_sdr (pstate,
                                                 sdr_ctx,
                                                 sensor_types,
                                                 sensor_types_length)) < 0)
                goto cleanup;
            }
          else
            ret = 1;            /* accept all */

          if (ret)
            {
              if (shared_sensors)
                {
                  if (_store_column_widths_shared (pstate,
                                                   sdr_ctx,
                                                   non_abbreviated_units,
                                                   count_event_only_records,
                                                   count_device_locator_records,
                                                   count_oem_records,
						   entity_sensor_names,
                                                   column_width) < 0)
                    goto cleanup;
                }
              else
                {
                  if (_store_column_widths (pstate,
                                            sdr_ctx,
                                            0,
                                            non_abbreviated_units,
					    0, /* shared_sensors */
                                            count_event_only_records,
                                            count_device_locator_records,
                                            count_oem_records,
					    entity_sensor_names,
                                            column_width) < 0)
                    goto cleanup;
                }
            }
        }
    }

  rv = 0;
  _sensor_column_width_finish (column_width);
 cleanup:
  ipmi_sdr_cache_first (sdr_ctx);
  return (rv);
}

int
calculate_column_widths_ignored_sdr_cache (unsigned int non_abbreviated_units,
					   struct sensor_column_width *column_width)
{
  assert (column_width);

  /* Ignoring the SDR cache?  Gotta make some guesses */
  column_width->sensor_name = SENSORS_SENSOR_NAME_LENGTH;
  column_width->sensor_type = strlen (ipmi_sensor_types[IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS]);
  if (non_abbreviated_units)
    column_width->sensor_units = strlen (ipmi_sensor_units[IPMI_SENSOR_UNIT_DEGREES_C]);
  else
    column_width->sensor_units = strlen (ipmi_sensor_units[IPMI_SENSOR_UNIT_RPM]);

  return (0);
}

