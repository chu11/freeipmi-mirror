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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>

#include "freeipmi/api/ipmi-sel-cmds-api.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

#include "ipmi-sel-entry.h"

#include "debug-util.h"
#include "freeipmi-portability.h"
#include "tool-fiid-wrappers.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define SEL_RECORD_TYPE_UNKNOWN_RECORD             0x0
#define SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD        0x1
#define SEL_RECORD_TYPE_TIMESTAMPED_OEM_RECORD     0x2
#define SEL_RECORD_TYPE_NON_TIMESTAMPED_OEM_RECORD 0x3

static int
_find_sdr_record(ipmi_sel_state_data_t *state_data,
                 uint8_t sensor_number,
                 uint8_t generator_id_type,
                 uint8_t generator_id,
                 uint8_t *sdr_record,
                 unsigned int *sdr_record_len,
                 uint8_t *sdr_record_type,
                 uint8_t *sdr_event_reading_type_code)
{
  uint8_t tmp_sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  int tmp_sdr_record_len;
  uint8_t tmp_record_type;
  uint8_t tmp_event_reading_type_code;
  
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(sdr_record_type);
  assert(sdr_event_reading_type_code);

  if (ipmi_sdr_cache_search_sensor(state_data->ipmi_sdr_cache_ctx, 
                                   sensor_number,
                                   (generator_id << 1) | generator_id_type) < 0)
    {
      if (ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx) != IPMI_SDR_CACHE_CTX_ERR_NOT_FOUND)
        { 
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_sdr_cache_search_record_id: %s\n",
                          ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
          return -1;
        }
      else
        /* can't find it */
        return 0;
    }

  memset(sdr_record, '\0', (*sdr_record_len));
  
  memset(tmp_sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);
  
  if ((tmp_sdr_record_len = ipmi_sdr_cache_record_read(state_data->ipmi_sdr_cache_ctx,
                                                       tmp_sdr_record,
                                                       IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_sdr_cache_record_read: %s\n",
                      ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
      return -1;
    }

  if (sdr_cache_get_record_id_and_type (state_data->pstate,
                                        tmp_sdr_record,
                                        tmp_sdr_record_len,
                                        NULL,
                                        &tmp_record_type) < 0)
    return -1;
  
  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             tmp_sdr_record,
                                             tmp_sdr_record_len,
                                             &tmp_event_reading_type_code) < 0)
    return -1;
  
  if ((*sdr_record_len) < tmp_sdr_record_len)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "buffer too small: %d\n", 
                      (*sdr_record_len));
      return -1;
    }
              
  memcpy(sdr_record, tmp_sdr_record, tmp_sdr_record_len);
  (*sdr_record_len) = tmp_sdr_record_len;
  *sdr_record_type = tmp_record_type;
  *sdr_event_reading_type_code = tmp_event_reading_type_code;
  
  return 1;
}

static double
_round_double2 (double d)
{
  double r = 0.0;

  r = (d - (long) d) * 100.0;

  if ((r - (long) r) > 0.5)
    return ((long) d + (((long) r + 1) / 100.0));

  return ((long) d + ((long) r / 100.0));
}

static int
_get_sel_record_type (uint8_t record_type)
{
  if (IPMI_SEL_RECORD_TYPE_IS_EVENT(record_type))
    return SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD;
  
  if (IPMI_SEL_RECORD_TYPE_IS_TIMESTAMPED_OEM(record_type))
    return SEL_RECORD_TYPE_TIMESTAMPED_OEM_RECORD;
  
  if (IPMI_SEL_RECORD_TYPE_IS_NON_TIMESTAMPED_OEM(record_type))
    return SEL_RECORD_TYPE_NON_TIMESTAMPED_OEM_RECORD;
  
  return SEL_RECORD_TYPE_UNKNOWN_RECORD;
}

static int
_decode_sensor_value (ipmi_sel_state_data_t *state_data,
                      uint8_t *sdr_record,
                      unsigned int sdr_record_len,
                      uint8_t raw_data,
                      double *reading,
                      uint8_t *sensor_unit)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(reading);
  assert(sensor_unit);

  if (sdr_cache_get_sensor_decoding_data(state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         &r_exponent,
                                         &b_exponent,
                                         &m,
                                         &b,
                                         &linearization,
                                         &analog_data_format) < 0)
    return -1;
  
  /* if the sensor is not analog, this is most likely a bug in the
   * SDR, since we shouldn't be decoding a non-threshold sensor.
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format))
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Attempting to decode non-analog sensor\n");
      return -1;
    }

  /* if the sensor is non-linear, I just don't know what to do */
  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization))
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Cannot decode non-linear sensor\n");
      return -1;
    }
  
  if (ipmi_sensor_decode_value (r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                raw_data,
                                reading) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sensor_decode_value: %s\n",
                       strerror(errno));
      return -1;
    }

  if (sdr_cache_get_sensor_unit (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 sensor_unit) < 0)
    return -1;

  return 0;
}

static int 
_get_sel_system_event_record (ipmi_sel_state_data_t *state_data,
                              uint8_t *record_data, 
                              uint32_t record_data_len,
                              uint16_t *stored_record_id,
                              char **timestamp,
                              char **sensor_info,
                              char **event_message,
                              char **event_data2_message,
                              char **event_data3_message)
{
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len;
  uint8_t sdr_record_type = 0;  /* init 0 to remove warnings */
  uint8_t sdr_event_reading_type_code = 0; /* init 0 to remove warnings */
  uint16_t record_id;
  uint32_t timestamp_val;
  uint8_t generator_id_type;
  uint8_t generator_id;
  uint8_t channel_number;
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_type_code;
  uint8_t event_dir;
  uint8_t offset_from_event_reading_type_code;
  uint8_t event_data2_flag;
  uint8_t event_data3_flag;
  uint8_t event_data2;
  uint8_t event_data3;
  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;
  int sdr_record_found = 0;

  assert (state_data);
  assert (record_data);
  assert (stored_record_id);
  assert (timestamp);
  assert (sensor_info);
  assert (event_message);
  assert (event_data2_message);
  assert (event_data3_message);

  _FIID_OBJ_CREATE (obj, tmpl_sel_system_event_record);

  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  if (state_data->prog_data->args->common.debug)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
      char *prefix = NULL;

      debug_hdr_str(DEBUG_UTIL_TYPE_NONE,
                    DEBUG_UTIL_DIRECTION_NONE,
                    "SEL Event Record",
                    hdrbuf,
                    DEBUG_UTIL_HDR_BUFLEN);

      if (state_data->hostname)
        prefix = state_data->hostname;
      if (state_data->prog_data->args->hostrange.always_prefix && !state_data->hostname)
        prefix = "localhost";

      ipmi_obj_dump (STDERR_FILENO,
                     prefix,
                     hdrbuf,
                     NULL,
                     obj);
    }

  _FIID_OBJ_GET (obj, "record_id", &val);
  record_id = val;

  _FIID_OBJ_GET (obj, "timestamp", &val);
  timestamp_val = val;
  
  _FIID_OBJ_GET (obj, "generator_id.id_type", &val);
  generator_id_type = val;
  
  _FIID_OBJ_GET (obj, "generator_id.id", &val);
  generator_id = val;
  
  _FIID_OBJ_GET (obj, "channel_number", &val);
  channel_number = val;
  
  _FIID_OBJ_GET (obj, "sensor_type", &val);
  sensor_type = val;
  
  _FIID_OBJ_GET (obj, "sensor_number", &val);
  sensor_number = val;
  
  _FIID_OBJ_GET (obj, "event_type_code", &val);
  event_type_code = val;
  
  _FIID_OBJ_GET (obj, "event_dir", &val);
  event_dir = val;
  
  _FIID_OBJ_GET (obj, "offset_from_event_reading_type_code", &val);
  offset_from_event_reading_type_code = val;
  
  _FIID_OBJ_GET (obj, "event_data2_flag", &val);
  event_data2_flag = val;
  
  _FIID_OBJ_GET (obj, "event_data3_flag", &val);
  event_data3_flag = val;
  
  _FIID_OBJ_GET (obj, "event_data2", &val);
  event_data2 = val;
  
  _FIID_OBJ_GET (obj, "event_data3", &val);
  event_data3 = val;

  *stored_record_id = record_id;
  
  {
    char buffer[256];
    time_t t;
    struct tm tmp;

    t = timestamp_val;
    localtime_r (&t, &tmp);
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", &tmp);
    
    if (!((*timestamp) = strdup (buffer)))
      {
        pstdout_perror(state_data->pstate, 
                       "strdup");
        goto cleanup;
      }
  }
  
  if (!state_data->prog_data->args->sdr.ignore_sdr_cache)
    {
      sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
      if ((sdr_record_found = _find_sdr_record(state_data, 
                                               sensor_number,
                                               generator_id_type,
                                               generator_id,
                                               sdr_record,
                                               &sdr_record_len,
                                               &sdr_record_type,
                                               &sdr_event_reading_type_code)) < 0)
        goto cleanup;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro H8QME with SIMSO daughter card.
   *
   * The slave address is reportedly incorrectly by having the 
   * generator_id be shifted over by one.  This is a special
   * "try again" corner case.
   */
  if (!sdr_record_found && generator_id == IPMI_SLAVE_ADDRESS_BMC)
    {
      if ((sdr_record_found = _find_sdr_record(state_data, 
                                               sensor_number,
                                               generator_id_type,
                                               (generator_id >> 1),
                                               sdr_record,
                                               &sdr_record_len,
                                               &sdr_record_type,
                                               &sdr_event_reading_type_code)) < 0)
        goto cleanup;
    }

  if (sdr_record_found)
    {
      char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
      
      memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);
      
      if (sdr_cache_get_id_string(state_data->pstate,
                                  sdr_record,
                                  sdr_record_len,
                                  id_string,
                                  IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
        goto cleanup;

      if (asprintf (sensor_info, 
                    "%s %s", 
                    sensor_group (sensor_type), 
                    id_string) < 0)
        {
          /* asprintf can leave pointer in an unknown state */
          *sensor_info = NULL;  
          pstdout_perror(state_data->pstate, "asprintf");
          goto cleanup;
        }
    }
  else
    {
      if (asprintf (sensor_info, 
                    "%s #%d", 
                    sensor_group (sensor_type), 
                    sensor_number) < 0)
        {
          /* asprintf can leave pointer in an unknown state */
          *sensor_info = NULL;  
          pstdout_perror(state_data->pstate, "asprintf");
          goto cleanup;
        }
    }      

  {
    char buffer[1024];
    int rv;

    switch (sensor_classify (event_type_code))
      {
      case SENSOR_CLASS_THRESHOLD:
        /* Don't use ipmi_get_threshold_message, b/c we didn't call
         * get_sensor_reading.  Fall through to below.
         */
      case SENSOR_CLASS_GENERIC_DISCRETE:
        rv = ipmi_get_generic_event_message(event_type_code,
                                            offset_from_event_reading_type_code,
                                            buffer, 
                                            1024);
        break;
      case SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
        rv = ipmi_get_sensor_type_code_message(sensor_type,
                                               offset_from_event_reading_type_code,
                                               buffer,
                                               1024);
        break;
      case SENSOR_CLASS_OEM:
      default:
        snprintf(buffer, 1024, "Event Type Code = %02Xh", event_type_code);
        rv = 0;
        break;
      }

    if (!rv)
      {
        if (!((*event_message) = strdup(buffer)))
          {
            pstdout_perror (state_data->pstate, "strdup");
            goto cleanup;
          }
      }
  }

  switch (sensor_classify (event_type_code))
    {
    case SENSOR_CLASS_THRESHOLD:
      {
        switch (event_data2_flag)
          {
          case IPMI_SEL_TRIGGER_READING:
            if (sdr_record_found
                && sdr_record_type == IPMI_SDR_FORMAT_FULL_RECORD
                && sensor_classify (sdr_event_reading_type_code) == SENSOR_CLASS_THRESHOLD)
              {
                double reading;
                uint8_t sensor_unit;

                if (_decode_sensor_value (state_data,
                                          sdr_record,
                                          sdr_record_len,
                                          event_data2,
                                          &reading,
                                          &sensor_unit) < 0)
                  {
                    /* If the decode fails, we will just output hex */
                    if (asprintf (event_data2_message, 
                                  "Trigger reading = %02Xh", 
                                  event_data2) < 0)
                      {
                        /* asprintf can leave pointer in an unknown state */
                        *event_data2_message = NULL;  
                        pstdout_perror(state_data->pstate, "asprintf");
                        goto cleanup;
                      }
                  }
                else
                  {
                    if (asprintf (event_data2_message,
                                  "Reading = %.2f %s",
                                  _round_double2 (reading),
                                  ipmi_sensor_units_abbreviated[sensor_unit]) < 0)
                      {
                        /* asprintf can leave pointer in an unknown state */
                        *event_data2_message = NULL;  
                        pstdout_perror(state_data->pstate, "asprintf");
                        goto cleanup;
                      }
                  }
              }
            else
              {
                if (asprintf (event_data2_message, 
                              "Trigger reading = %02Xh", 
                              event_data2) < 0)
                  {
                    /* asprintf can leave pointer in an unknown state */
                    *event_data2_message = NULL;  
                    pstdout_perror(state_data->pstate, "asprintf");
                    goto cleanup;
                  }
              }
            break;
          case IPMI_SEL_OEM_CODE:
            if (asprintf (event_data2_message, 
                          "OEM code = %02Xh", 
                          event_data2) < 0)
              {
                /* asprintf can leave pointer in an unknown state */
                *event_data2_message = NULL;  
                pstdout_perror(state_data->pstate, "asprintf");
                goto cleanup;
              }
            break;
          case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
            {
              char buffer[1024];
              int rv;

              rv = ipmi_get_event_data2_message (sensor_type, 
                                                 offset_from_event_reading_type_code, 
                                                 event_data2,
                                                 buffer,
                                                 1024);
              if (!rv)
                {
                  if (!((*event_data2_message) = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
            }
            break;
          }
	
        switch (event_data3_flag)
          {
          case IPMI_SEL_TRIGGER_THRESHOLD_VALUE:
            if (sdr_record_found
                && sdr_record_type == IPMI_SDR_FORMAT_FULL_RECORD
                && sensor_classify (sdr_event_reading_type_code) == SENSOR_CLASS_THRESHOLD)
              {
                double reading;
                uint8_t sensor_unit;
                
                if (_decode_sensor_value (state_data,
                                          sdr_record,
                                          sdr_record_len,
                                          event_data3,
                                          &reading,
                                          &sensor_unit) < 0)
                  {
                    /* If the decode fails, we will just output hex */
                    if (asprintf (event_data2_message, 
                                  "Trigger reading = %02Xh", 
                                  event_data2) < 0)
                      {
                        /* asprintf can leave pointer in an unknown state */
                        *event_data2_message = NULL;  
                        pstdout_perror(state_data->pstate, "asprintf");
                        goto cleanup;
                      }
                  }
                else
                  {
                    if (asprintf (event_data3_message,
                                  "Threshold = %.2f %s",
                                  _round_double2 (reading),
                                  ipmi_sensor_units_abbreviated[sensor_unit]) < 0)
                      {
                        /* asprintf can leave pointer in an unknown state */
                        *event_data3_message = NULL;  
                        pstdout_perror(state_data->pstate, "asprintf");
                        goto cleanup;
                      }
                  }
              }
            else
              {
                if (asprintf (event_data3_message, 
                              "Trigger reading = %02Xh", 
                              event_data3) < 0)
                  {
                    /* asprintf can leave pointer in an unknown state */
                    *event_data3_message = NULL;  
                    pstdout_perror(state_data->pstate, "asprintf");
                    goto cleanup;
                  }
              }
            break;
          case IPMI_SEL_OEM_CODE:
            if (asprintf (event_data3_message, 
                          "OEM code = %02Xh", 
                          event_data3) < 0)
              {
                /* asprintf can leave pointer in an unknown state */
                *event_data3_message = NULL;  
                pstdout_perror(state_data->pstate, "asprintf");
                goto cleanup;
              }
            break;
          case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
            {
              char buffer[1024];

              if (!ipmi_get_event_data3_message (sensor_type, 
                                                 offset_from_event_reading_type_code, 
                                                 event_data2,
                                                 event_data3,
                                                 buffer,
                                                 1024))
                {
                  if (!((*event_data3_message) = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
            }
            break;
          }
        
        break;
      }
    case SENSOR_CLASS_GENERIC_DISCRETE:
    case SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      {
        switch (event_data2_flag)
          {
          case IPMI_SEL_OEM_CODE:
            if (asprintf (event_data2_message,
                          "OEM code = %02Xh",
                          event_data2) < 0)
              {
                /* asprintf can leave pointer in an unknown state */
                *event_data2_message = NULL;  
                pstdout_perror(state_data->pstate, "asprintf");
                goto cleanup;
              }
            break;
          case IPMI_SEL_PREV_STATE_SEVERITY:
            break;
          case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
            {
              char buffer[1024];
              
              if (!ipmi_get_event_data2_message (sensor_type, 
                                                 offset_from_event_reading_type_code, 
                                                 event_data2,
                                                 buffer,
                                                 1024))
                {
                  if (!((*event_data2_message) = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
            }
          }
	
        switch (event_data3_flag)
          {
          case IPMI_SEL_OEM_CODE:
            if (asprintf (event_data3_message,
                          "OEM code = %02Xh",
                          event_data3) < 0)
              {
                /* asprintf can leave pointer in an unknown state */
                *event_data3_message = NULL;  
                pstdout_perror(state_data->pstate, "asprintf");
                goto cleanup;
              }
            break;
          case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
            {
              char buffer[1024];

              if (!ipmi_get_event_data3_message (sensor_type, 
                                                 offset_from_event_reading_type_code, 
                                                 event_data2,
                                                 event_data3,
                                                 buffer,
                                                 1024))
                {
                  if (!((*event_data3_message) = strdup(buffer)))
                    {
                      pstdout_perror (state_data->pstate, "strdup");
                      goto cleanup;
                    }
                }
            }
            break;
          }
	
        break;
      }
    case SENSOR_CLASS_OEM:
      {
        if (asprintf (event_data2_message, 
                      "Event Data2 = %02Xh", 
                      event_data2) < 0)
          {
            /* asprintf can leave pointer in an unknown state */
            *event_data2_message = NULL;  
            pstdout_perror(state_data->pstate, "asprintf");
            goto cleanup;
          }
        if (asprintf (event_data3_message, 
                      "Event Data3 = %02Xh", 
                      event_data3) < 0)
          {
            /* asprintf can leave pointer in an unknown state */
            *event_data3_message = NULL;  
            pstdout_perror(state_data->pstate, "asprintf");
            goto cleanup;
          }
        break;
      }
    default:
      break;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj);
  return (rv);
}

static int 
_get_sel_timestamped_oem_record (ipmi_sel_state_data_t *state_data,
                                 uint8_t *record_data, 
                                 uint32_t record_data_len,
                                 uint16_t *stored_record_id,
                                 char **timestamp,
                                 char **sensor_info,
                                 char **event_message,
                                 char **event_data2_message,
                                 char **event_data3_message)
{
  uint16_t record_id;
  uint32_t timestamp_val;
  uint32_t manufacturer_id;
  uint64_t oem_defined;
  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;

  assert (state_data);
  assert (record_data);
  assert (stored_record_id);
  assert (timestamp);
  assert (sensor_info);
  assert (event_message);
  assert (event_data2_message);
  assert (event_data3_message);

  _FIID_OBJ_CREATE (obj, tmpl_sel_timestamped_oem_record);
  
  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  _FIID_OBJ_GET (obj, "record_id", &val);
  record_id = val;

  _FIID_OBJ_GET (obj, "timestamp", &val);
  timestamp_val = val;
  
  _FIID_OBJ_GET (obj, "manufacturer_id", &val);
  manufacturer_id = val;
  
  _FIID_OBJ_GET (obj, "oem_defined", &val);
  oem_defined = val;

  *stored_record_id = record_id;

  {
    char buffer[256];
    time_t t;
    struct tm tmp;
        
    t = timestamp_val;
    localtime_r (&t, &tmp);
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", &tmp);
        
    if (!((*timestamp) = strdup (buffer)))
      {
        pstdout_perror(state_data->pstate, "strdup");
        goto cleanup;
      }
  }

  asprintf (sensor_info, 
            "Manufacturer ID %02Xh", 
            manufacturer_id);

  asprintf (event_message, 
            "OEM Defined = " FI_64 "Xh",
            oem_defined);
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj);
  return (rv);
}

static int
_get_sel_non_timestamped_oem_record (ipmi_sel_state_data_t *state_data,
                                     uint8_t *record_data, 
                                     uint32_t record_data_len, 
                                     uint16_t *stored_record_id,
                                     char **timestamp,
                                     char **sensor_info,
                                     char **event_message,
                                     char **event_data2_message,
                                     char **event_data3_message)
{
  fiid_obj_t obj = NULL;
  uint16_t record_id;
  uint64_t val;
  int8_t rv = -1;
  uint8_t buf[1024];
  int32_t len;
  char *str = NULL;
  char *tmp_str = NULL;
  int i;

  assert (state_data);
  assert (record_data);
  assert (stored_record_id);
  assert (timestamp);
  assert (sensor_info);
  assert (event_message);
  assert (event_data2_message);
  assert (event_data3_message);

  _FIID_OBJ_CREATE (obj, tmpl_sel_non_timestamped_oem_record);

  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  _FIID_OBJ_GET (obj, "record_id", &val);
  record_id = val;

  *stored_record_id = record_id;

  memset(buf, '\0', 1024);
  _FIID_OBJ_GET_DATA_LEN (len,
                          obj,
                          "oem_defined",
                          buf,
                          1024);

  for (i = 0; i < len; i++)
    {
      tmp_str = str;
      if (str)
        {
          str = NULL;
          asprintf (&str, "%s %02X", tmp_str, buf[i]);
          free (tmp_str);
        }
      else
        asprintf (&str, "%02X", buf[i]);
    }

  if (str)
    {
      asprintf (event_message, "OEM defined = %s", str);
      free (str);
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj);
  return (rv);
}

static int
_parse_sel_record (ipmi_sel_state_data_t *state_data, 
                   uint8_t *record_data,
                   uint32_t record_data_len,
                   uint16_t *stored_record_id,
                   char **timestamp,
                   char **sensor_info,
                   char **event_message,
                   char **event_data2_message,
                   char **event_data3_message)
{
  fiid_obj_t obj = NULL;
  uint8_t record_type;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (record_data);
  assert (record_data_len);
  assert (stored_record_id);
  assert (timestamp);
  assert (sensor_info);
  assert (event_message);
  assert (event_data2_message);
  assert (event_data3_message);

  _FIID_OBJ_CREATE (obj, tmpl_sel_record_header);

  _FIID_OBJ_SET_ALL(obj, record_data, record_data_len);

  _FIID_OBJ_GET (obj, "record_type", &val);
  record_type = val;

  /* IPMI Workaround
   *
   * HP DL 380 G5
   *
   * Motherboard is reporting SEL Records of record type 0x00, which
   * is not a valid record type.
   */
  if (state_data->prog_data->args->assume_system_event_records
      && (record_type == 0x00
          || record_type == 0x01))
    record_type = 0x02;

  switch (_get_sel_record_type (record_type))
    {
    case SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD:
      rv = _get_sel_system_event_record (state_data, 
                                         record_data, 
                                         record_data_len, 
                                         stored_record_id,
                                         timestamp,
                                         sensor_info,
                                         event_message,
                                         event_data2_message,
                                         event_data3_message);
      break;
    case SEL_RECORD_TYPE_TIMESTAMPED_OEM_RECORD:
      rv = _get_sel_timestamped_oem_record (state_data, 
                                            record_data, 
                                            record_data_len, 
                                            stored_record_id,
                                            timestamp,
                                            sensor_info,
                                            event_message,
                                            event_data2_message,
                                            event_data3_message);
      break;
    case SEL_RECORD_TYPE_NON_TIMESTAMPED_OEM_RECORD:
      rv = _get_sel_non_timestamped_oem_record (state_data,
                                                record_data, 
                                                record_data_len,
                                                stored_record_id,
                                                timestamp,
                                                sensor_info,
                                                event_message,
                                                event_data2_message,
                                                event_data3_message);
      break;
    default:
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "Unknown SEL Record Type: %X\n", 
                      record_type);
      break;
    }

 cleanup:
  _FIID_OBJ_DESTROY(obj);
  return (rv);
}

int
ipmi_sel_get_entry (ipmi_sel_state_data_t *state_data, 
                    uint16_t record_id, 
                    uint16_t *next_record_id,
                    uint16_t *stored_record_id,
                    char **timestamp,
                    char **sensor_info,
                    char **event_message,
                    char **event_data2_message,
                    char **event_data3_message)
{
  uint8_t record_data[IPMI_SEL_RECORD_SIZE];
  uint32_t record_data_len = IPMI_SEL_RECORD_SIZE;
  fiid_obj_t obj_reserve_sel_rs = NULL;
  fiid_obj_t obj_get_sel_entry_rs = NULL;
  unsigned int reservation_cancelled = 0;
  uint64_t val;
  int32_t len;
  int rv = -1;
  
  assert (state_data);
  assert (next_record_id);
  assert (stored_record_id);
  assert (timestamp);
  assert (sensor_info);
  assert (event_message);
  assert (event_data2_message);
  assert (event_data3_message);
  
  *stored_record_id = 0;
  *timestamp = NULL;
  *sensor_info = NULL;
  *event_message = NULL;
  *event_data2_message = NULL;
  *event_data3_message = NULL;
  
  _FIID_OBJ_CREATE(obj_reserve_sel_rs, tmpl_cmd_reserve_sel_rs);
  _FIID_OBJ_CREATE(obj_get_sel_entry_rs, tmpl_cmd_get_sel_entry_rs);
 
  while (1)
    {
      /*
       *
       * IPMI spec states in section 31.4.1:
       *
       * "A Requester must issue a 'Reserve SEL' command prior to issuing
       * any of the following SEL commands. Note that the 'Reserve SEL'
       * command only needs to be reissued if the reservation is
       * canceled. ... Get SEL Entry command (if 'get' is from an offset
       * other than 00h)".
       *
       * Since we always use an offset of 00h, presumably we should never
       * need reserve the SEL before the get_sel_entry call.
       *
       * However, some machines may need it due to compliance issues.
       * I don't think using a reservation ID all of the time hurts
       * anything, so we'll just use it all of the time. If there's an
       * error along the way, we'll just ignore it.
       */
      
      if (record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY || reservation_cancelled)
        {
          if (ipmi_cmd_reserve_sel (state_data->ipmi_ctx, obj_reserve_sel_rs) < 0)
            {
              if (state_data->prog_data->args->common.debug)
                pstdout_fprintf(state_data->pstate,
                                stderr,
                                "ipmi_cmd_reserve_sel: %s\n",
                                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));

              state_data->reservation_id = 0;
              goto get_sel_entry;
            }
          
          _FIID_OBJ_GET(obj_reserve_sel_rs, "reservation_id", &val);
          state_data->reservation_id = val;
        }
      
    get_sel_entry:
      if (ipmi_cmd_get_sel_entry (state_data->ipmi_ctx, 
                                  state_data->reservation_id,
                                  record_id, 
                                  0,
                                  IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
                                  obj_get_sel_entry_rs) < 0)
        {
          if (ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code(obj_get_sel_entry_rs,
                                            IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
              if (reservation_cancelled)
                {
                  pstdout_fprintf(state_data->pstate,
                                  stderr,
                                  "Reservation Cancelled multiple times\n");
                  goto cleanup;
                }
              reservation_cancelled++;
              continue;
            }
          
          /* If the sel is empty, don't bother outputting an error
           * message, it's not a real error.
           */
          if (!(record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
                && ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
                && ipmi_check_completion_code(obj_get_sel_entry_rs,
                                              IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1))
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_get_sel_entry: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          
          goto cleanup;
        }
      
      break;
    }
  
  _FIID_OBJ_GET (obj_get_sel_entry_rs, "next_record_id", &val);
  *next_record_id = val;
  
  _FIID_OBJ_GET_DATA_LEN (len,
                          obj_get_sel_entry_rs, 
                          "record_data", 
                          record_data,
                          record_data_len);
  record_data_len = len;
  
  if (_parse_sel_record (state_data,
                         record_data, 
                         record_data_len, 
                         stored_record_id,
                         timestamp,
                         sensor_info,
                         event_message,
                         event_data2_message,
                         event_data3_message) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_reserve_sel_rs);
  _FIID_OBJ_DESTROY(obj_get_sel_entry_rs);
  if (rv < 0)
    {
      if (*timestamp)
        free(*timestamp);
      if (*sensor_info)
        free(*sensor_info);
      if (*event_message)
        free(*event_message);
      if (*event_data2_message)
        free(*event_data2_message);
      if (*event_data3_message)
        free(*event_data3_message);
    }
  return rv;
}
