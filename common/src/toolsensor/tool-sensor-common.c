/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
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

#define RECORD_ID_BUFLEN    64

#define SENSOR_GROUP_BUFLEN 1024

#define SENSOR_UNITS_BUFLEN 1024

#define SENSOR_FMT_BUFLEN   1024

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
get_sensor_group_output_string (unsigned int sensor_type)
{
  const char *sensor_group;

  if ((sensor_group = ipmi_get_sensor_type_string (sensor_type)))
    return (sensor_group);

  return (UNRECOGNIZED_SENSOR_GROUP);
}

void
get_sensor_group_cmdline_string (char *sensor_group)
{
  assert (sensor_group);

  _str_replace_char (sensor_group, ' ', '_');
  _str_replace_char (sensor_group, '/', '_');
}

int
display_sensor_group_cmdline (pstdout_state_t pstate, 
                              unsigned int sensor_type)
{
  const char *sensor_group;
  char *tmpstr = NULL;

  sensor_group = get_sensor_group_output_string (sensor_type);

  if (!(tmpstr = strdupa (sensor_group)))
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "strdupa: %s\n",
                       strerror (errno));
      return (-1);
    }

  get_sensor_group_cmdline_string (tmpstr);

  PSTDOUT_PRINTF (pstate, "%s\n", tmpstr);
  return (0);
}

int
get_sensor_units_output_string (pstdout_state_t pstate,
                                ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                                uint8_t *sdr_record,
                                unsigned int sdr_record_len,
                                char *sensor_units_buf,
                                unsigned int sensor_units_buflen,
                                unsigned int abbreviated_units_flag)
{
  uint8_t sensor_units_percentage;
  uint8_t sensor_units_modifier;
  uint8_t sensor_units_rate;
  uint8_t sensor_base_unit_type;
  uint8_t sensor_modifier_unit_type;
  int sensor_units_ret = 0;
  int rv = -1;

  assert (sdr_parse_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (sensor_units_buf);
  assert (sensor_units_buflen);

  if (ipmi_sdr_parse_sensor_units (sdr_parse_ctx,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_units: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (sdr_parse_ctx));
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
                                               abbreviated_units_flag);
  
  if (sensor_units_ret <= 0)
    snprintf (sensor_units_buf,
              sensor_units_buflen,
              "%s",
              ipmi_sensor_units[IPMI_SENSOR_UNIT_UNSPECIFIED]);
  
  rv = 0;
 cleanup:
  return rv;
}

int
display_string_cmdline (pstdout_state_t pstate, 
                        const char *str)
{
  char *tmpstr;

  assert (str);

  if (!(tmpstr = strdupa (str)))
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "strdupa: %s\n",
                       strerror (errno));
      return (-1);
    } 

  get_sensor_group_cmdline_string (tmpstr);

  PSTDOUT_PRINTF (pstate, "%s\n", tmpstr);
  return (0);
}

int
sensor_group_strcmp (pstdout_state_t pstate,
                     const char *sensor_group_str_input,
                     unsigned int sensor_type)
{
  const char *sensor_group_str;
  char *tmpstr;

  assert (sensor_group_str_input);

  /* Don't use get_sensor_group_output_string() - want NULL if invalid */
  sensor_group_str = ipmi_get_sensor_type_string (sensor_type);

  if (!sensor_group_str)
    return (0);

  if (!(tmpstr = strdupa (sensor_group_str)))
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "strdupa: %s\n",
                       strerror (errno));
      return (-1);
    }

  get_sensor_group_cmdline_string (tmpstr);

  if (!strcasecmp (sensor_group_str_input, sensor_group_str)
      || !strcasecmp (sensor_group_str_input, tmpstr))
    return (1);
  
  return (0);
}

int
is_sdr_sensor_group_listed (pstdout_state_t pstate,
                            ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            char groups[][MAX_SENSOR_GROUPS_STRING_LENGTH+1],
                            unsigned int groups_length)
{
  uint16_t record_id;
  uint8_t record_type;
  uint8_t sensor_type;
  int i;

  assert (sdr_parse_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (groups);
  assert (groups_length);

  if (ipmi_sdr_parse_record_id_and_type (sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         &record_id,
                                         &record_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (sdr_parse_ctx));
      return (-1);
    }

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    return (0);

  if (ipmi_sdr_parse_sensor_type (sdr_parse_ctx,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (sdr_parse_ctx));
      return (-1);
    }

  for (i = 0; i < groups_length; i++)
    {
      int ret;

      if ((ret = sensor_group_strcmp (pstate,
                                      groups[i],
                                      sensor_type)) < 0)
        return (-1);

      if (ret)
        return (1);
    }

  return (0);
}

void
output_sensor_headers (pstdout_state_t pstate,
                       int quiet_readings,
                       int output_sensor_state,
                       int comma_separated_output,
                       struct sensor_column_width *column_width)
{
  char fmt[SENSOR_FMT_BUFLEN + 1];

  assert (column_width);

  memset (fmt, '\0', SENSOR_FMT_BUFLEN + 1);
  if (comma_separated_output)
    snprintf (fmt,
              SENSOR_FMT_BUFLEN,
              "%%s,%%s,%%s");
  else
    snprintf (fmt,
              SENSOR_FMT_BUFLEN,
              "%%-%ds | %%-%ds | %%-%ds",
              column_width->record_id,
              column_width->sensor_name,
              column_width->sensor_group);

  PSTDOUT_PRINTF (pstate,
                  fmt,
                  SENSORS_HEADER_RECORD_ID_STR,
                  SENSORS_HEADER_NAME_STR,
                  SENSORS_HEADER_GROUP_STR);

  if (output_sensor_state)
    {
      if (comma_separated_output)
        PSTDOUT_PRINTF (pstate,
                        ",%s",
                        SENSORS_HEADER_STATE_STR);
      else
        PSTDOUT_PRINTF (pstate,
                        " | %s   ",
                        SENSORS_HEADER_STATE_STR);
    }
  
  if (!quiet_readings)
    {
      if (comma_separated_output)
        PSTDOUT_PRINTF (pstate,
                        ",%s",
                        SENSORS_HEADER_READING_STR);
      else
        PSTDOUT_PRINTF (pstate,
                        " | %s   ",
                        SENSORS_HEADER_READING_STR);
      
      memset (fmt, '\0', SENSOR_FMT_BUFLEN + 1);
      if (comma_separated_output)
        snprintf (fmt,
                  SENSOR_FMT_BUFLEN,
                  ",%%s");
      else
        snprintf (fmt,
                  SENSOR_FMT_BUFLEN,
                  " | %%-%ds",
                  column_width->sensor_units);
      
      PSTDOUT_PRINTF (pstate,
                      fmt,
                      SENSORS_HEADER_UNITS_STR);
    }

  if (comma_separated_output)
    PSTDOUT_PRINTF (pstate,
                    ",%s\n",
                    SENSORS_HEADER_EVENT_STR);
  else
    PSTDOUT_PRINTF (pstate,
                    " | %s\n",
                    SENSORS_HEADER_EVENT_STR);
}

static void
_sensor_column_width_init (struct sensor_column_width *column_width)
{
  assert (column_width);

  column_width->record_id = 0;
  column_width->sensor_name = 0;
  column_width->sensor_group = 0;
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
  if (column_width->sensor_group < strlen(SENSORS_HEADER_GROUP_STR))
    column_width->sensor_group = strlen(SENSORS_HEADER_GROUP_STR);
  if (column_width->sensor_units < strlen(SENSORS_HEADER_UNITS_STR))
    column_width->sensor_units = strlen(SENSORS_HEADER_UNITS_STR);
}

static int
_store_column_widths (pstdout_state_t pstate,
                      ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                      uint8_t *sdr_record,
                      unsigned int sdr_record_len,
                      unsigned int abbreviated_units,
                      struct sensor_column_width *column_width)
{
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  char record_id_buf[RECORD_ID_BUFLEN + 1];
  uint16_t record_id;
  uint8_t record_type;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  int len;

  assert (sdr_parse_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (column_width);

  if (ipmi_sdr_parse_record_id_and_type (sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         &record_id,
                                         &record_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (sdr_parse_ctx));
      return (-1);
    }

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    return (0);

  memset (record_id_buf, '\0', RECORD_ID_BUFLEN + 1);
  snprintf (record_id_buf, RECORD_ID_BUFLEN, "%u", record_id);

  len = strlen (record_id_buf);
  if (len > column_width->record_id)
    column_width->record_id = len;

  memset (id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);
  if (ipmi_sdr_parse_id_string (sdr_parse_ctx,
                                sdr_record,
                                sdr_record_len,
                                id_string,
                                IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_id_string: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (sdr_parse_ctx));
      return (-1);
    }

  len = strlen (id_string);
  if (len > column_width->sensor_name)
    column_width->sensor_name = len;

  if (ipmi_sdr_parse_sensor_type (sdr_parse_ctx,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_type) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (sdr_parse_ctx));
      return (-1);
    }

  len = strlen (get_sensor_group_output_string (sensor_type));
  if (len > column_width->sensor_group)
    column_width->sensor_group = len;


  if (ipmi_sdr_parse_event_reading_type_code (sdr_parse_ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (sdr_parse_ctx));
      return (-1);
    }

  if (ipmi_event_reading_type_code_class (event_reading_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      char sensor_units_buf[SENSOR_UNITS_BUFLEN + 1];

      memset (sensor_units_buf, '\0', SENSOR_UNITS_BUFLEN + 1);
      if (get_sensor_units_output_string (pstate,
                                          sdr_parse_ctx,
                                          sdr_record,
                                          sdr_record_len,
                                          sensor_units_buf,
                                          SENSOR_UNITS_BUFLEN,
                                          abbreviated_units) < 0)
        return (-1);

      len = strlen (sensor_units_buf);
      if (len > column_width->sensor_units)
        column_width->sensor_units = len;
    }
  
  return (0);
}

int
calculate_column_widths (pstdout_state_t pstate,
                         ipmi_sdr_cache_ctx_t sdr_cache_ctx,
                         ipmi_sdr_parse_ctx_t sdr_parse_ctx,
                         char groups[][MAX_SENSOR_GROUPS_STRING_LENGTH+1],
                         unsigned int groups_length,
                         unsigned int record_ids[],
                         unsigned int record_ids_length,
                         unsigned int abbreviated_units,
                         struct sensor_column_width *column_width)
{
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  int sdr_record_len = 0;
  int rv = -1;
  int i;

  assert (sdr_cache_ctx);
  assert (sdr_parse_ctx);
  assert (column_width);

  _sensor_column_width_init (column_width);

  if (record_ids && record_ids_length)
    {
      for (i = 0; i < record_ids_length; i++)
        {
          if (ipmi_sdr_cache_search_record_id (sdr_cache_ctx, record_ids[i]) < 0)
            {
              if (ipmi_sdr_cache_ctx_errnum (sdr_cache_ctx) == IPMI_SDR_CACHE_ERR_NOT_FOUND)
                continue;
              else
                {
                  PSTDOUT_FPRINTF (pstate,
                                   stderr,
                                   "ipmi_sdr_cache_search_record_id: %s\n",
                                   ipmi_sdr_cache_ctx_errormsg (sdr_cache_ctx));
                  goto cleanup;
                }
            }

          if ((sdr_record_len = ipmi_sdr_cache_record_read (sdr_cache_ctx,
                                                            sdr_record,
                                                            IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "ipmi_sdr_cache_record_read: %s\n",
                               ipmi_sdr_cache_ctx_errormsg (sdr_cache_ctx));
              goto cleanup;
            }

          /* Shouldn't be possible */
          if (!sdr_record_len)
            continue;

          if (_store_column_widths (pstate,
                                    sdr_parse_ctx,
                                    sdr_record,
                                    sdr_record_len,
                                    abbreviated_units,
                                    column_width) < 0)
            goto cleanup;
        }
    }
  else
    {
      uint16_t record_count;

      if (ipmi_sdr_cache_record_count (sdr_cache_ctx, &record_count) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sdr_cache_record_count: %s\n",
                           ipmi_sdr_cache_ctx_errormsg (sdr_cache_ctx));
          goto cleanup;
        }

      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (sdr_cache_ctx))
        {
          int ret;

          if ((sdr_record_len = ipmi_sdr_cache_record_read (sdr_cache_ctx,
                                                            sdr_record,
                                                            IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "ipmi_sdr_cache_record_read: %s\n",
                               ipmi_sdr_cache_ctx_errormsg (sdr_cache_ctx));
              goto cleanup;
            }

          /* Shouldn't be possible */
          if (!sdr_record_len)
            continue;

          if (groups && groups_length)
            {
              if ((ret = is_sdr_sensor_group_listed (pstate,
                                                     sdr_parse_ctx,
                                                     sdr_record,
                                                     sdr_record_len,
                                                     groups,
                                                     groups_length)) < 0)
                goto cleanup;
            }
          else
            ret = 1;            /* accept all */

          if (ret)
            {
              if (_store_column_widths (pstate,
                                        sdr_parse_ctx,
                                        sdr_record,
                                        sdr_record_len,
                                        abbreviated_units,
                                        column_width) < 0)
                goto cleanup;
            }
        }
    }

  rv = 0;
  _sensor_column_width_finish (column_width);
 cleanup:
  ipmi_sdr_cache_first (sdr_cache_ctx);
  return (rv);
}
