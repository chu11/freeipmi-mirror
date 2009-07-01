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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "freeipmi/api/ipmi-sensor-cmds-api.h"
#include "freeipmi/cmds/ipmi-sensor-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-system-software-id-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-sensor-util.h"

#include "ipmi-sensors.h"
#include "ipmi-sensors-util.h"

#include "freeipmi-portability.h"
#include "tool-fiid-wrappers.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_SENSORS_OK_MSG   "OK"

int
_sensor_reading_corner_case_checks (struct ipmi_sensors_state_data *state_data,
                                    uint8_t sensor_number,
                                    uint8_t record_id,
                                    fiid_obj_t obj_get_sensor_reading_rs)
{
  assert(state_data);
  assert(obj_get_sensor_reading_rs);

  if (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                 IPMI_COMP_CODE_NODE_BUSY) == 1)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Sensor number 0x%X data in record %u is busy, can't retrieve\n",
                        sensor_number,
                        record_id);
      return 0;
    }
  else if (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                      IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
    {
      /* A sensor listed by the SDR is not present.  Skip it's
       * output, don't error out.
       */
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Sensor number 0x%X data in record %u not present\n",
                        sensor_number,
                        record_id);
      return 0;
    }
  else if (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                      IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Sensor number 0x%X data in record %u cannot be retrieved\n",
                        sensor_number,
                        record_id);
      return 0;
    }
  else if ((ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                       IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1)
           || (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                          IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "SDR record %u contains invalid data\n",
                        record_id);
      return 0;
    }
  else if (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                      IPMI_COMP_CODE_COMMAND_CANNOT_RESPOND) == 1)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "SDR record %u cannot be read\n",
                        record_id);
      return 0;
    }
  else if (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                      IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "SDR record %u cannot be read\n",
                        record_id);
      return 0;
    }

  return -1;
}

int
_get_sensor_reading (struct ipmi_sensors_state_data *state_data,
                     char ***event_message_list,
                     unsigned int *event_message_list_len,
                     uint8_t sensor_number,
                     uint8_t record_id,
                     fiid_obj_t obj_get_sensor_reading_rs)
{
  int rv = -1;

  assert(state_data);
  assert(event_message_list);
  assert(event_message_list_len);
  assert(obj_get_sensor_reading_rs);

  if (ipmi_cmd_get_sensor_reading (state_data->ipmi_ctx, 
                                   sensor_number, 
                                   obj_get_sensor_reading_rs) < 0)
    {
      if (!_sensor_reading_corner_case_checks(state_data, 
                                              sensor_number,
                                              record_id,
                                              obj_get_sensor_reading_rs))
        rv = 0;
      else
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_sensor_reading: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  rv = 1;
 cleanup:
  return rv;
}

int
_get_sensor_reading_ipmb (struct ipmi_sensors_state_data *state_data,
                          char ***event_message_list,
                          unsigned int *event_message_list_len,
                          uint8_t slave_address,
                          uint8_t lun,
                          uint8_t channel_number,
                          uint8_t sensor_number,
                          uint8_t record_id,
                          fiid_obj_t obj_get_sensor_reading_rs)
{
  int rv = -1;

  assert(state_data);
  assert(event_message_list);
  assert(event_message_list_len);
  assert(obj_get_sensor_reading_rs);

  if (state_data->prog_data->args->bridge_sensors
      && channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB)
    {
      if (ipmi_cmd_get_sensor_reading_ipmb (state_data->ipmi_ctx,
                                            slave_address,
                                            lun,
                                            sensor_number,
                                            obj_get_sensor_reading_rs) < 0)
        {
          if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE)
            {
              if (state_data->prog_data->args->common.debug)
                pstdout_fprintf(state_data->pstate,
                                stderr,
                                "Sensor number 0x%X data in record %u cannot be bridged on the selected interface\n",
                                sensor_number,
                                record_id);
              
              /* make status message "na" so "unknown" isn't output */
              if (get_msg_message_list (state_data,
                                        event_message_list,
                                        event_message_list_len,
                                        "NA") < 0)
                goto cleanup;
              
              rv = 0;
            }
          else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_MESSAGE_TIMEOUT)
            {
              /* ipmb message timed out, but we can still continue, not a fatal error */
              if (state_data->prog_data->args->common.debug)
                pstdout_fprintf(state_data->pstate,
                                stderr,
                                "Sensor number 0x%X data in record %u ipmb message timed out\n",
                                sensor_number,
                                record_id);
              
              rv = 0;
            }
          else if (!_sensor_reading_corner_case_checks(state_data, 
                                                       sensor_number,
                                                       record_id,
                                                       obj_get_sensor_reading_rs))
            rv = 0;
          else
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_get_sensor_reading_ipmb: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          goto cleanup;
        }
    }
  else
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Sensor number 0x%X in record %u is not owned by the BMC\n",
                        sensor_number,
                        record_id);
      
      /* make status message "na" so "unknown" isn't output */
      if (get_msg_message_list (state_data,
                                event_message_list,
                                event_message_list_len,
                                "NA") < 0)
        goto cleanup;
      
      rv = 0;
      goto cleanup;
    }

  rv = 1;
 cleanup:
  return rv;
}

int
sensor_reading (struct ipmi_sensors_state_data *state_data,
                uint8_t *sdr_record,
                unsigned int sdr_record_len,
                double **reading,
                char ***event_message_list,
                unsigned int *event_message_list_len)
{ 
  uint16_t record_id;
  uint8_t record_type;
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  int sensor_class;
  double *tmp_reading = NULL;
  uint64_t val;
  int rv = -1;
  fiid_obj_t obj_get_sensor_reading_rs = NULL;  
  uint64_t sensor_event_bitmask1 = 0;
  uint64_t sensor_event_bitmask2 = 0;
  uint64_t sensor_event_bitmask = 0;
  int8_t sensor_event_bitmask1_len;
  int8_t sensor_event_bitmask2_len;
  uint8_t sensor_owner_id_type;
  uint8_t sensor_owner_id;
  uint8_t sensor_owner_lun;
  uint8_t channel_number;
  uint8_t slave_address;
  int ret;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(reading);
  assert(event_message_list);
  assert(event_message_list_len);

  *reading = NULL;
  *event_message_list = NULL;
  *event_message_list_len = 0;

  if (sdr_cache_get_record_id_and_type(state_data->pstate,
                                       sdr_record,
                                       sdr_record_len,
                                       &record_id,
                                       &record_type) < 0)
    return -1;

  /* can't get reading for this sdr entry. don't output an error
   * though, since this isn't really an error.  The tool will output
   * something appropriate as it sees fit.
   */
  if (record_type != IPMI_SDR_FORMAT_FULL_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_RECORD)
    return 0;
 
  if (sdr_cache_get_sensor_number (state_data->pstate,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_number) < 0)
    return -1;
    
  if (sdr_cache_get_sensor_type (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_type) < 0)
    return -1;

  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    return -1;

  if (sdr_cache_get_sensor_owner_id (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &sensor_owner_id_type,
                                     &sensor_owner_id) < 0)
    return -1;

  if (sdr_cache_get_sensor_owner_lun (state_data->pstate,
                                      sdr_record,
                                      sdr_record_len,
                                      &sensor_owner_lun,
                                      &channel_number) < 0)
    return -1;

  if (sensor_owner_id_type == IPMI_SDR_SENSOR_OWNER_ID_TYPE_SYSTEM_SOFTWARE_ID)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Sensor number 0x%X is a system software sensor\n",
                        sensor_number);
      rv = 0;
      goto cleanup;
    }

  slave_address = (sensor_owner_id << 1) | sensor_owner_id_type;

  _FIID_OBJ_CREATE(obj_get_sensor_reading_rs, tmpl_cmd_get_sensor_reading_rs);

  if (slave_address == IPMI_SLAVE_ADDRESS_BMC)
    {
      if ((ret = _get_sensor_reading (state_data,
                                      event_message_list,
                                      event_message_list_len,
                                      sensor_number,
                                      record_id,
                                      obj_get_sensor_reading_rs)) < 0)
        goto cleanup;

      if (!ret)
        {
          rv = 0;
          goto cleanup;
        }
    }
  else
    {
      if ((ret = _get_sensor_reading_ipmb (state_data,
                                           event_message_list,
                                           event_message_list_len,
                                           slave_address,
                                           sensor_owner_lun,
                                           channel_number,
                                           sensor_number,
                                           record_id,
                                           obj_get_sensor_reading_rs)) < 0)
        goto cleanup;

      if (!ret)
        {
          rv = 0;
          goto cleanup;
        }
    }

  _FIID_OBJ_GET (obj_get_sensor_reading_rs,
                 "reading_state",
                 &val);
  
  if (val == IPMI_SENSOR_READING_STATE_UNAVAILABLE)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Sensor reading unavailable\n");

      /* make status message "na" so "unknown" isn't output */
      if (get_msg_message_list (state_data,
                                event_message_list,
                                event_message_list_len,
                                "NA") < 0)
        goto cleanup;
      
      rv = 0;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_get_sensor_reading_rs,
                 "sensor_scanning",
                 &val);

  if (val == IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Sensor scanning disabled\n");

      /* make status message "na" so "unknown" isn't output */
      if (get_msg_message_list (state_data,
                                event_message_list,
                                event_message_list_len,
                                "NA") < 0)
        goto cleanup;
      
      rv = 0;
      goto cleanup;
    }

  /* achu:
   * 
   * Note: I don't bother checking the "all_event_messages" flag from
   * the get_sensor_reading response.  If that stuff is turned off,
   * the bitmasks should be zeroed out.
   *
   * Hopefully this doesn't bite me later on.
   */

  _FIID_OBJ_GET_WITH_RETURN_VALUE (obj_get_sensor_reading_rs,
                                   "sensor_event_bitmask1",
                                   &sensor_event_bitmask1,
                                   sensor_event_bitmask1_len);
  
  _FIID_OBJ_GET_WITH_RETURN_VALUE (obj_get_sensor_reading_rs,
                                   "sensor_event_bitmask2",
                                   &sensor_event_bitmask2,
                                   sensor_event_bitmask2_len);
 
  /* 
   * IPMI Workaround (achu)
   *
   * Discovered on Dell 2950.
   *
   * It seems the sensor_event_bitmask may not be returned by the server
   * at all for some sensors.  Under this situation, there's not
   * much that can be done.  Since there is no sensor_event_bitmask, we
   * just assume that no states have been asserted and the
   * sensor_event_bitmask = 0;
   */

  if (!sensor_event_bitmask1_len && !sensor_event_bitmask2_len)
    sensor_event_bitmask = 0;
  else if (sensor_event_bitmask1_len && sensor_event_bitmask2_len)
    sensor_event_bitmask = sensor_event_bitmask1 | (sensor_event_bitmask2 << 8);
  else if (sensor_event_bitmask1_len && !sensor_event_bitmask2_len)
    sensor_event_bitmask = sensor_event_bitmask1;
  else
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "invalid sensor_event_bitmask condition\n");
      goto cleanup;
    }
  
  sensor_class = sensor_classify (event_reading_type_code);

  if (sensor_class == SENSOR_CLASS_THRESHOLD)
    {
      _FIID_OBJ_GET (obj_get_sensor_reading_rs, "sensor_reading", &val);

      if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
        {
          int8_t r_exponent, b_exponent;
          int16_t m, b;
          uint8_t linearization, analog_data_format;

          if (sdr_cache_get_sensor_decoding_data(state_data->pstate,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 &r_exponent,
                                                 &b_exponent,
                                                 &m,
                                                 &b,
                                                 &linearization,
                                                 &analog_data_format) < 0)
            goto cleanup;

          /* if the sensor is not analog, this is most likely a bug in the
           * SDR, since we shouldn't be decoding a non-threshold sensor.
           */
          if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format))
            {
              if (state_data->prog_data->args->common.debug)
                pstdout_fprintf(state_data->pstate,
                                stderr,
                                "Attempting to decode non-analog sensor\n");

              /* still get event messages */
              if (get_threshold_message_list (state_data,
                                              event_message_list,
                                              event_message_list_len,
                                              sensor_event_bitmask,
                                              IPMI_SENSORS_OK_MSG) < 0)
                goto cleanup;

              rv = 0;
              goto cleanup;
            }
          
          /* if the sensor is non-linear, I just don't know what to do, 
           * let the tool figure out what to output.
           */
          if (!IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization))
            {
              if (state_data->prog_data->args->common.debug)
                pstdout_fprintf(state_data->pstate,
                                stderr,
                                "Cannot decode non-linear sensor\n");

              /* still get event messages */
              if (get_threshold_message_list (state_data,
                                              event_message_list,
                                              event_message_list_len,
                                              sensor_event_bitmask,
                                              IPMI_SENSORS_OK_MSG) < 0)
                goto cleanup;

              rv = 0;
              goto cleanup;
            }
             
          if (!(tmp_reading = (double *)malloc(sizeof(double))))
            {
              pstdout_perror(state_data->pstate, "malloc");
              goto cleanup;
            }
          
	  if (ipmi_sensor_decode_value (r_exponent, 
                                        b_exponent, 
                                        m, 
                                        b, 
                                        linearization, 
                                        analog_data_format, 
                                        (uint8_t) val,
                                        tmp_reading) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror(errno));
              goto cleanup;
            }
	}
      /* else
       *
       * I guess there is a mistake, it should have been listed as a non-threshold
       * sensor?  We'll still fall through to grab event messages, since maybe we
       * can still output something.
       */
           
      if (get_threshold_message_list (state_data,
                                      event_message_list,
                                      event_message_list_len,
                                      sensor_event_bitmask,
                                      IPMI_SENSORS_OK_MSG) < 0)
        goto cleanup;
      
      rv = 1;
    }
  else if (sensor_class == SENSOR_CLASS_GENERIC_DISCRETE
           || sensor_class ==  SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE
           || sensor_class == SENSOR_CLASS_OEM)
    {               
      if (sensor_class == SENSOR_CLASS_GENERIC_DISCRETE)
        {
          if (get_generic_event_message_list (state_data,
                                              event_message_list,
                                              event_message_list_len, 
                                              event_reading_type_code, 
                                              sensor_event_bitmask,
                                              IPMI_SENSORS_OK_MSG) < 0)
            goto cleanup;

          rv = 1;
        }
      else if (sensor_class == SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE)
        {
          if (get_sensor_specific_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len,
                                                      sensor_type, 
                                                      sensor_event_bitmask,
                                                      IPMI_SENSORS_OK_MSG) < 0)
            goto cleanup;

          rv = 1;
        }
      else if (sensor_class == SENSOR_CLASS_OEM)
        {
          char *event_message = NULL;
          char **tmp_event_message_list = NULL;

          if (asprintf (&event_message, 
                        "OEM State = %04Xh", 
                        (uint16_t) sensor_event_bitmask) < 0)
            {
              pstdout_perror(state_data->pstate, "asprintf");
              goto cleanup;
            }

          if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * 2)))
            {
              pstdout_perror(state_data->pstate, "malloc");
              goto cleanup;
            }
      
          tmp_event_message_list[0] = event_message;
          tmp_event_message_list[1] = NULL;

          *event_message_list = tmp_event_message_list;
          *event_message_list_len = 1;
          
          rv = 1;
        }
    }
  else
    rv = 0;

  if (rv > 0)
    *reading = tmp_reading;
 cleanup:
  _FIID_OBJ_DESTROY(obj_get_sensor_reading_rs);
  if (rv <= 0)
    {
      if (tmp_reading)
        free(tmp_reading);
    }
  return (rv);
}
