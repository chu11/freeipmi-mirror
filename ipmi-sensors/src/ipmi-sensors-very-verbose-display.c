/*
  Copyright (C) 2003-2008 FreeIPMI Core Team
  
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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"

#include "ipmi-sensors.h"
#include "ipmi-sensors-display-common.h"
#include "ipmi-sensors-util.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"
#include "tool-sensor-common.h"

#define IPMI_SENSORS_OEM_DATA_LEN 1024

#define IPMI_SENSORS_NONE_MSG     "NONE"

static int
_output_very_verbose_record_type_and_id (ipmi_sensors_state_data_t *state_data,
                                         uint8_t record_type,
                                         uint16_t record_id)
{
  assert(state_data);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n", 
                  record_type);

  return 0;
}

static int
_output_very_verbose_header (ipmi_sensors_state_data_t *state_data,
                             uint8_t *sdr_record,
                             unsigned int sdr_record_len,
                             uint8_t record_type,
                             uint16_t record_id)
{
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  uint8_t sensor_owner_id_type, sensor_owner_id;
  uint8_t sensor_owner_lun, channel_number;
  uint8_t entity_id, entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

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
  
  memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);

  if (sdr_cache_get_id_string (state_data->pstate,
                               sdr_record,
                               sdr_record_len,
                               id_string,
                               IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
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
  
  if (sdr_cache_get_entity_id_instance_type (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &entity_id,
                                             &entity_instance,
                                             NULL) < 0)
    return -1;

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;
  
  pstdout_printf (state_data->pstate, 
                  "ID String: %s\n", 
                  id_string);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n",
                  sensor_group (sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  sensor_number);
  if (sensor_owner_id_type)
    pstdout_printf (state_data->pstate,
                    "System Software ID: %Xh\n",
                    sensor_owner_id);
  else
    pstdout_printf (state_data->pstate,
                    "IPMB Slave Address: %Xh\n",
                    sensor_owner_id);
  pstdout_printf (state_data->pstate,
                  "Sensor Owner ID: %Xh\n",
                  (sensor_owner_id << 1) | sensor_owner_id_type);
  pstdout_printf (state_data->pstate,
                  "Sensor Owner LUN: %Xh\n",
                  sensor_owner_lun);
  pstdout_printf (state_data->pstate,
                  "Channel Number: %Xh\n",
                  channel_number);
  pstdout_printf (state_data->pstate, 
                  "Entity ID: %d\n", 
                  entity_id);
  pstdout_printf (state_data->pstate, 
                  "Entity Instance: %d\n", 
                  entity_instance);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  event_reading_type_code);
  

  return 0;
}

static int
_output_very_verbose_hysteresis (ipmi_sensors_state_data_t *state_data,
                                 uint8_t *sdr_record,
                                 unsigned int sdr_record_len,
                                 uint8_t record_type)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t positive_going_threshold_hysteresis_raw = 0;
  uint8_t negative_going_threshold_hysteresis_raw = 0;
  uint8_t sensor_number;
  uint8_t sensor_unit;
  uint8_t hysteresis_support;
  uint64_t val;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  /* achu: first lets check if we have anything to output */
  if (sdr_cache_get_sensor_capabilities (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         NULL,
                                         &hysteresis_support,
                                         NULL,
                                         NULL) < 0)
    goto cleanup;

  if (hysteresis_support == IPMI_SDR_NO_HYSTERESIS_SUPPORT
      || hysteresis_support == IPMI_SDR_FIXED_UNREADABLE_HYSTERESIS_SUPPORT)
    {
      rv = 0;
      goto cleanup;
    }

  /* achu:
   *
   * I will admit I'm not entirely sure what the best way is to get
   * hysteresis.  It seems the information is stored/retrievable in
   * the SDR and through an IPMI command.
   *
   * We will try to read it via IPMI like we do with thresholds, since
   * a change to the hysteresis may not be written to the SDR.
   */

  if (sdr_cache_get_sensor_number (state_data->pstate,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_number) < 0)
    goto cleanup;

  if (sdr_cache_get_sensor_unit (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_unit) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sensor_hysteresis_rs);

  if (ipmi_cmd_get_sensor_hysteresis (state_data->ipmi_ctx,
                                      sensor_number,
                                      IPMI_SENSOR_HYSTERESIS_MASK,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_sensor_hysteresis: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      
      if ((ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID)
          && (ipmi_check_completion_code(obj_cmd_rs,
                                         IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1
              || ipmi_check_completion_code(obj_cmd_rs,
                                            IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1))
        {
          /* The hysteresis cannot be gathered for one reason or
           * another, maybe b/c its a OEM sensor or something.  Output
           * "NA" stuff in output_raw.
           */
          goto output_raw;
        }
      
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs,
                 "positive_going_threshold_hysteresis_value",
                 &val);
  positive_going_threshold_hysteresis_raw = val;
  
  _FIID_OBJ_GET (obj_cmd_rs,
                 "negative_going_threshold_hysteresis_value",
                 &val);
  negative_going_threshold_hysteresis_raw = val;

  /* achu: Well, compact records don't have the values to compute a
   * hysteresis value.  Perhaps that's a typo in the spec?  We just
   * output the integer values?  That's the best guess I can make.
   */
  
  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      double positive_going_threshold_hysteresis_real;
      double negative_going_threshold_hysteresis_real;

      if (positive_going_threshold_hysteresis_raw
          || negative_going_threshold_hysteresis_raw)
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
           *
           * Don't return an error.  Output integer value.
           */
          if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format))
            goto output_raw;
          
          /* if the sensor is non-linear, I just don't know what to do
           *
           * Don't return an error.  Output integer value.
           */
          if (!IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization))
            goto output_raw;
          
          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        positive_going_threshold_hysteresis_raw,
                                        &positive_going_threshold_hysteresis_real) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror(errno));
              goto cleanup;
            }
          
          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        negative_going_threshold_hysteresis_raw,
                                        &negative_going_threshold_hysteresis_real) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror(errno));
              goto cleanup;
            }
        }
          
      if (positive_going_threshold_hysteresis_raw)
        pstdout_printf (state_data->pstate, 
                        "Positive Hysteresis: %f %s\n", 
                        positive_going_threshold_hysteresis_real,
                        ipmi_sensor_units[sensor_unit]);
      else
        pstdout_printf (state_data->pstate,
                        "Positive Hysteresis: %s\n", 
                        "NA");

      if (negative_going_threshold_hysteresis_raw)
        pstdout_printf (state_data->pstate, 
                        "Negative Hysteresis: %f %s\n", 
                        negative_going_threshold_hysteresis_real,
                        ipmi_sensor_units[sensor_unit]);
      else
        pstdout_printf (state_data->pstate,
                        "Negative Hysteresis: %s\n", 
                        "NA");
    }
  else
    {          
    output_raw:
      if (positive_going_threshold_hysteresis_raw)
        pstdout_printf (state_data->pstate, 
                        "Positive Hysteresis: %d %s\n", 
                        positive_going_threshold_hysteresis_raw,
                        ipmi_sensor_units[sensor_unit]);
      else
        pstdout_printf (state_data->pstate,
                        "Positive Hysteresis: %s\n", 
                        "NA");

      if (negative_going_threshold_hysteresis_raw)
        pstdout_printf (state_data->pstate, 
                        "Negative Hysteresis: %d %s\n", 
                        negative_going_threshold_hysteresis_raw,
                        ipmi_sensor_units[sensor_unit]);
      else
        pstdout_printf (state_data->pstate,
                        "Negative Hysteresis: %s\n", 
                        "NA");
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

static int
_output_very_verbose_event_enable (ipmi_sensors_state_data_t *state_data,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint8_t record_type)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sensor_number;
  int sensor_class;
  uint64_t val;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  char **assertion_event_message_list = NULL;
  unsigned int assertion_event_message_list_len = 0;
  char **deassertion_event_message_list = NULL;
  unsigned int deassertion_event_message_list_len = 0;
  int8_t field_len;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  /* achu:
   *
   * I will admit I'm not entirely sure what the best way is to get
   * event enables.  It seems the information is stored/retrievable in
   * the SDR and through an IPMI command.
   *
   * We will try to read it via IPMI like we do with thresholds, since
   * a change to the event enables may not be written to the SDR.
   */

  if (sdr_cache_get_sensor_number (state_data->pstate,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_number) < 0)
    goto cleanup;

  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  sensor_class = sensor_classify (event_reading_type_code);

  if (sensor_class != SENSOR_CLASS_THRESHOLD
      && sensor_class != SENSOR_CLASS_GENERIC_DISCRETE
      && sensor_class !=  SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Cannot handle event enables for event type reading code: 0x%X\n",
                        event_reading_type_code);
      rv = 0;
      goto cleanup;
    }

  if (sensor_class == SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE)
    {
      if (sdr_cache_get_sensor_type (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &sensor_type) < 0)
        goto cleanup;
    }
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sensor_event_enable_rs);
  
  if (ipmi_cmd_get_sensor_event_enable (state_data->ipmi_ctx,
                                        sensor_number,
                                        obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_sensor_hysteresis: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      
      if ((ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID)
          && (ipmi_check_completion_code(obj_cmd_rs,
                                         IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1
              || ipmi_check_completion_code(obj_cmd_rs,
                                            IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1))
        {
          /* The event enables cannot be gathered for one reason or
           * another, maybe b/c its a OEM sensor or something.  Just
           * don't output this info.
           */
          rv = 0;
          goto cleanup;
        }
      
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs,
                 "all_event_messages",
                 &val);
  
  if (val == IPMI_SENSOR_ALL_EVENT_MESSAGES_DISABLE)
    {
      pstdout_printf (state_data->pstate,
                      "Assertion Events Enabled: [All Event Messages Disabled]\n");
      pstdout_printf (state_data->pstate,
                      "Deassertion Events Enabled: [All Event Messages Disabled]\n");
      rv = 0;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs,
                 "scanning_on_this_sensor",
                 &val);

  if (val == IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE)
    {
      pstdout_printf (state_data->pstate,
                      "Assertion Events Enabled: [Sensor Scanning Disabled]\n");
      pstdout_printf (state_data->pstate,
                      "Deassertion Events Enabled: [Sensor Scanning Disabled]\n");
      rv = 0;
      goto cleanup;
    }

  /* achu: According to the spec, bytes 3-6 of the packet should exist
   * if all event messages are not disabled and sensor scanning is not
   * disabled.
   */

  _FIID_OBJ_GET_WITH_RETURN_VALUE (obj_cmd_rs,
                                   "assertion_event_bitmask",
                                   &val,
                                   field_len);
  if (field_len)
    {
      if (sensor_class == SENSOR_CLASS_THRESHOLD
          || sensor_class == SENSOR_CLASS_GENERIC_DISCRETE)
        {
          if (get_generic_event_message_list (state_data,
                                              &assertion_event_message_list,
                                              &assertion_event_message_list_len,
                                              event_reading_type_code,
                                              (uint16_t)val,
                                              IPMI_SENSORS_NONE_MSG) < 0)
            goto cleanup;
        }
      else
        {
          if (get_sensor_specific_event_message_list (state_data,
                                                      &assertion_event_message_list,
                                                      &assertion_event_message_list_len,
                                                      sensor_type,
                                                      (uint16_t)val,
                                                      IPMI_SENSORS_NONE_MSG) < 0)
            goto cleanup;
        }

      if (ipmi_sensors_output_event_message_list (state_data,
                                                  assertion_event_message_list,
                                                  assertion_event_message_list_len,
                                                  "Assertion Events Enabled: ",
                                                  1) < 0)
        goto cleanup;
    }

  _FIID_OBJ_GET_WITH_RETURN_VALUE (obj_cmd_rs,
                                   "deassertion_event_bitmask",
                                   &val,
                                   field_len);
  if (field_len)
    {
      if (sensor_class == SENSOR_CLASS_THRESHOLD
          || sensor_class == SENSOR_CLASS_GENERIC_DISCRETE)
        {
          if (get_generic_event_message_list (state_data,
                                              &deassertion_event_message_list,
                                              &deassertion_event_message_list_len,
                                              event_reading_type_code,
                                              (uint16_t)val,
                                              IPMI_SENSORS_NONE_MSG) < 0)
            goto cleanup;
        }
      else
        {
          if (get_sensor_specific_event_message_list (state_data,
                                                      &deassertion_event_message_list,
                                                      &deassertion_event_message_list_len,
                                                      sensor_type,
                                                      (uint16_t)val,
                                                      IPMI_SENSORS_NONE_MSG) < 0)
            goto cleanup;
        }
      
      if (ipmi_sensors_output_event_message_list (state_data,
                                                  deassertion_event_message_list,
                                                  deassertion_event_message_list_len,
                                                  "Deassertion Events Enabled: ",
                                                  1) < 0)
        goto cleanup;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

static int 
sensors_display_very_verbose_full_record (ipmi_sensors_state_data_t *state_data,
                                          uint8_t *sdr_record,
                                          unsigned int sdr_record_len,
                                          uint8_t record_type,
                                          uint16_t record_id,
                                          double *reading,
                                          char **event_message_list,
                                          unsigned int event_message_list_len)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  uint8_t event_reading_type_code;
  int sensor_class;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header (state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   record_type,
                                   record_id) < 0)
    return -1;

  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    return -1;

  sensor_class = sensor_classify (event_reading_type_code);

  if (sensor_class == SENSOR_CLASS_THRESHOLD)
    {
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
      
      pstdout_printf (state_data->pstate, 
                      "B: %d\n", 
                      b);
      pstdout_printf (state_data->pstate,
                      "M: %d\n", 
                      m);
      pstdout_printf (state_data->pstate, 
                      "R Exponent: %d\n", 
                      r_exponent);
      pstdout_printf (state_data->pstate, 
                      "B Exponent: %d\n", 
                      b_exponent);
      pstdout_printf (state_data->pstate, 
                      "Linearization: %d\n", 
                      linearization);
      pstdout_printf (state_data->pstate, 
                      "Analog Data Format: %d\n", 
                      analog_data_format);

      if (ipmi_sensors_output_verbose_thresholds (state_data,
                                                  sdr_record,
                                                  sdr_record_len) < 0)
        return -1;
 
      if (ipmi_sensors_output_verbose_sensor_reading_ranges (state_data,
                                                             sdr_record,
                                                             sdr_record_len) < 0)
        return -1;
    }

  if (_output_very_verbose_hysteresis (state_data,
                                       sdr_record,
                                       sdr_record_len,
                                       record_type) < 0)
    return -1;

  if (_output_very_verbose_event_enable (state_data,
                                         sdr_record,
                                         sdr_record_len,
                                         record_type) < 0)
    return -1;

  if (ipmi_sensors_output_verbose_sensor_reading (state_data,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  reading) < 0)
    return -1;
  
  if (ipmi_sensors_output_verbose_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

static int 
sensors_display_very_verbose_compact_record (ipmi_sensors_state_data_t *state_data,
                                             uint8_t *sdr_record,
                                             unsigned int sdr_record_len,
                                             uint8_t record_type,
                                             uint16_t record_id,
                                             char **event_message_list,
                                             unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header (state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   record_type,
                                   record_id) < 0)
    return -1;

  if (_output_very_verbose_hysteresis (state_data,
                                       sdr_record,
                                       sdr_record_len,
                                       record_type) < 0)
    return -1;

  if (_output_very_verbose_event_enable (state_data,
                                         sdr_record,
                                         sdr_record_len,
                                         record_type) < 0)
    return -1;

  if (ipmi_sensors_output_verbose_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_event_only_record (ipmi_sensors_state_data_t *state_data,
                                                uint8_t *sdr_record,
                                                unsigned int sdr_record_len,
                                                uint8_t record_type,
                                                uint16_t record_id)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header (state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   record_type,
                                   record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_entity_association_record (ipmi_sensors_state_data_t *state_data,
                                                        uint8_t *sdr_record,
                                                        unsigned int sdr_record_len,
                                                        uint8_t record_type,
                                                        uint16_t record_id)
{
  uint8_t container_entity_id;
  uint8_t container_entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_container_entity (state_data->pstate,
                                      sdr_record,
                                      sdr_record_len,
                                      &container_entity_id,
                                      &container_entity_instance) < 0)
    return -1;
  
  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Container Entity ID: %d\n", 
                  container_entity_id);
  pstdout_printf (state_data->pstate, 
                  "Container Entity Instance: %d\n", 
                  container_entity_instance);

  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

static int 
sensors_display_very_verbose_device_relative_entity_association_record (ipmi_sensors_state_data_t *state_data,
                                                                        uint8_t *sdr_record,
                                                                        unsigned int sdr_record_len,
                                                                        uint8_t record_type,
                                                                        uint16_t record_id)
{
  uint8_t container_entity_id;
  uint8_t container_entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_container_entity (state_data->pstate,
                                      sdr_record,
                                      sdr_record_len,
                                      &container_entity_id,
                                      &container_entity_instance) < 0)
    return -1;
  
  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Container Entity ID: %d\n", 
                  container_entity_id);
  pstdout_printf (state_data->pstate, 
                  "Container Entity Instance: %d\n", 
                  container_entity_instance);

  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

static int
_output_very_verbose_header2 (ipmi_sensors_state_data_t *state_data,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len,
                              uint8_t record_type,
                              uint16_t record_id)
{
  char device_id_string[IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING + 1];

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  memset(device_id_string, '\0', IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING + 1);

  if (sdr_cache_get_device_id_string (state_data->pstate,
                                      sdr_record,
                                      sdr_record_len,
                                      device_id_string,
                                      IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING) < 0)
    return -1;

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Device ID String: %s\n", 
                  device_id_string);
  
  return 0;
}

static int
_output_device_type_and_modifier (ipmi_sensors_state_data_t *state_data,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len)
{
  uint8_t device_type;
  uint8_t device_type_modifier;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_device_type (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &device_type,
                                 &device_type_modifier) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Device Type: %Xh\n", 
                  device_type);

  pstdout_printf (state_data->pstate, 
                  "Device Type Modifier: %Xh\n", 
                  device_type_modifier);

  return 0;
}

static int
_output_entity_id_and_instance (ipmi_sensors_state_data_t *state_data,
                                uint8_t *sdr_record,
                                unsigned int sdr_record_len)
{
  uint8_t entity_id;
  uint8_t entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_entity_id_and_instance (state_data->pstate,
                                            sdr_record,
                                            sdr_record_len,
                                            &entity_id,
                                            &entity_instance) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Entity ID: %d\n", 
                  entity_id);
  
  pstdout_printf (state_data->pstate, 
                  "Entity Instance: %d\n", 
                  entity_instance);

  return 0;
}

static int 
sensors_display_very_verbose_general_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                            uint8_t *sdr_record,
                                                            unsigned int sdr_record_len,
                                                            uint8_t record_type,
                                                            uint16_t record_id)
{
  uint8_t direct_access_address;
  uint8_t channel_number;
  uint8_t device_slave_address;
  uint8_t private_bus_id;
  uint8_t lun_for_master_write_read_command;
  uint8_t address_span;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header2 (state_data,
                                    sdr_record,
                                    sdr_record_len,
                                    record_type,
                                    record_id) < 0)
    return -1;

  if (sdr_cache_get_general_device_locator_parameters (state_data->pstate,
                                                       sdr_record,
                                                       sdr_record_len,
                                                       &direct_access_address,
                                                       &channel_number,
                                                       &device_slave_address,
                                                       &private_bus_id,
                                                       &lun_for_master_write_read_command,
                                                       &address_span) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Direct Access Address: %Xh\n", 
                  direct_access_address);
  pstdout_printf (state_data->pstate, 
                  "Channel Number: %Xh\n", 
                  channel_number);
  pstdout_printf (state_data->pstate, 
                  "Direct Slave Address: %Xh\n", 
                  device_slave_address);
  pstdout_printf (state_data->pstate,
                  "Private Bus ID: %Xh\n",
                  private_bus_id);
  pstdout_printf (state_data->pstate, 
                  "LUN for Master Write-Read Command: %Xh\n", 
                  lun_for_master_write_read_command);
  pstdout_printf (state_data->pstate, 
                  "Address Span: %d\n", 
                  address_span);

  if (_output_device_type_and_modifier (state_data,
                                        sdr_record,
                                        sdr_record_len) < 0)
    return -1;

  if (_output_entity_id_and_instance (state_data,
                                      sdr_record,
                                      sdr_record_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_fru_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                        uint8_t *sdr_record,
                                                        unsigned int sdr_record_len,
                                                        uint8_t record_type,
                                                        uint16_t record_id)
{
  uint8_t direct_access_address;
  uint8_t logical_fru_device_device_slave_address;
  uint8_t private_bus_id;
  uint8_t lun_for_master_write_read_fru_command;
  uint8_t logical_physical_fru_device;
  uint8_t channel_number;
  uint8_t fru_entity_id;
  uint8_t fru_entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header2 (state_data,
                                    sdr_record,
                                    sdr_record_len,
                                    record_type,
                                    record_id) < 0)
    return -1;

  if (sdr_cache_get_fru_device_locator_parameters (state_data->pstate,
                                                   sdr_record,
                                                   sdr_record_len,
                                                   &direct_access_address,
                                                   &logical_fru_device_device_slave_address,
                                                   &private_bus_id,
                                                   &lun_for_master_write_read_fru_command,
                                                   &logical_physical_fru_device,
                                                   &channel_number) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Direct Access Address: %Xh\n", 
                  direct_access_address);

  if (logical_physical_fru_device)
    pstdout_printf (state_data->pstate, 
                    "FRU Device ID: %Xh\n", 
                    logical_fru_device_device_slave_address);
  else
    pstdout_printf (state_data->pstate, 
                    "Device Slave Address: %Xh\n", 
                    logical_fru_device_device_slave_address);

  pstdout_printf (state_data->pstate,
                  "Private Bus ID: %Xh\n",
                  private_bus_id);

  pstdout_printf (state_data->pstate, 
                  "LUN for Master Write-Read or FRU Command: %Xh\n", 
                  lun_for_master_write_read_fru_command);

  pstdout_printf (state_data->pstate, 
                  "Channel Number: %Xh\n", 
                  channel_number);

  if (_output_device_type_and_modifier (state_data,
                                        sdr_record,
                                        sdr_record_len) < 0)
    return -1;

  if (sdr_cache_get_fru_entity_id_and_instance (state_data->pstate,
                                                sdr_record,
                                                sdr_record_len,
                                                &fru_entity_id,
                                                &fru_entity_instance) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "FRU Entity ID: %d\n", 
                  fru_entity_id);
  
  pstdout_printf (state_data->pstate, 
                  "FRU Entity Instance: %d\n", 
                  fru_entity_instance);

    
  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_management_controller_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                                          uint8_t *sdr_record,
                                                                          unsigned int sdr_record_len,
                                                                          uint8_t record_type,
                                                                          uint16_t record_id)
{
  uint8_t device_slave_address;
  uint8_t channel_number;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header2 (state_data,
                                    sdr_record,
                                    sdr_record_len,
                                    record_type,
                                    record_id) < 0)
    return -1;

  if (sdr_cache_get_management_controller_device_locator_parameters (state_data->pstate,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     &device_slave_address,
                                                                     &channel_number) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Device Slave Address: %Xh\n", 
                  device_slave_address);

  pstdout_printf (state_data->pstate, 
                  "Channel Number: %Xh\n", 
                  channel_number);

  if (_output_entity_id_and_instance (state_data,
                                      sdr_record,
                                      sdr_record_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int
_output_manufacturer_id (ipmi_sensors_state_data_t *state_data,
                         uint8_t *sdr_record,
                         unsigned int sdr_record_len)
{
  uint32_t manufacturer_id;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_manufacturer_id (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &manufacturer_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate,
                  "Manufacturer ID: %Xh\n", 
                  manufacturer_id);

  return 0;
}

static int 
sensors_display_very_verbose_management_controller_information_record (ipmi_sensors_state_data_t *state_data,
                                                                       uint8_t *sdr_record,
                                                                       unsigned int sdr_record_len,
                                                                       uint8_t record_type,
                                                                       uint16_t record_id)
{
  uint16_t product_id;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  if (_output_manufacturer_id (state_data,
                               sdr_record,
                               sdr_record_len) < 0)
    return -1;

  if (sdr_cache_get_product_id (state_data->pstate,
                                sdr_record,
                                sdr_record_len,
                                &product_id) < 0)
    return -1;
  
  pstdout_printf (state_data->pstate,
                  "Product ID: %Xh\n", 
                  product_id);

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_bmc_message_channel_info_record (ipmi_sensors_state_data_t *state_data,
                                                              uint8_t *sdr_record,
                                                              unsigned int sdr_record_len,
                                                              uint8_t record_type,
                                                              uint16_t record_id)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_oem_record (ipmi_sensors_state_data_t *state_data,
                                         uint8_t *sdr_record,
                                         unsigned int sdr_record_len,
                                         uint8_t record_type,
                                         uint16_t record_id)
{
  uint8_t oem_data[IPMI_SENSORS_OEM_DATA_LEN];
  unsigned int oem_data_len = IPMI_SENSORS_OEM_DATA_LEN;
  int i;
  
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;


  if (_output_manufacturer_id (state_data,
                               sdr_record,
                               sdr_record_len) < 0)
    return -1;

  if (sdr_cache_get_oem_data (state_data->pstate,
                              sdr_record,
                              sdr_record_len,
                              oem_data,
                              &oem_data_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "OEM Data: ");
  
  for (i = 0; i < oem_data_len; i++)
    pstdout_printf (state_data->pstate, 
                    "%02X ", 
                    oem_data[i]);
  pstdout_printf (state_data->pstate, "\n");

  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

int 
ipmi_sensors_display_very_verbose (ipmi_sensors_state_data_t *state_data,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   double *reading,
                                   char **event_message_list,
                                   unsigned int event_message_list_len)
{
  uint16_t record_id;
  uint8_t record_type;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_record_id_and_type(state_data->pstate,
                                       sdr_record,
                                       sdr_record_len,
                                       &record_id,
                                       &record_type) < 0)
    return -1;

  switch (record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return sensors_display_very_verbose_full_record (state_data,
                                                       sdr_record,
                                                       sdr_record_len,
                                                       record_type,
                                                       record_id,
                                                       reading,
                                                       event_message_list,
                                                       event_message_list_len);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_very_verbose_compact_record (state_data,
                                                          sdr_record,
                                                          sdr_record_len,
                                                          record_type,
                                                          record_id,
                                                          event_message_list,
                                                          event_message_list_len);
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return sensors_display_very_verbose_event_only_record (state_data,
                                                             sdr_record,
                                                             sdr_record_len,
                                                             record_type,
                                                             record_id);
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      return sensors_display_very_verbose_entity_association_record (state_data,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     record_type,
                                                                     record_id);

    case IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD:
      return sensors_display_very_verbose_device_relative_entity_association_record (state_data,
                                                                                     sdr_record,
                                                                                     sdr_record_len,
                                                                                     record_type,
                                                                                     record_id);
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_general_device_locator_record (state_data,
                                                                         sdr_record,
                                                                         sdr_record_len,
                                                                         record_type,
                                                                         record_id);
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_fru_device_locator_record (state_data,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     record_type,
                                                                     record_id);
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_management_controller_device_locator_record (state_data,
                                                                                       sdr_record,
                                                                                       sdr_record_len,
                                                                                       record_type,
                                                                                       record_id);
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD:
      return sensors_display_very_verbose_management_controller_information_record (state_data,
                                                                                    sdr_record,
                                                                                    sdr_record_len,
                                                                                    record_type,
                                                                                    record_id);
      break;
    case IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD:
      return sensors_display_very_verbose_bmc_message_channel_info_record (state_data,
                                                                           sdr_record,
                                                                           sdr_record_len,
                                                                           record_type,
                                                                           record_id);
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      return sensors_display_very_verbose_oem_record (state_data,
                                                      sdr_record,
                                                      sdr_record_len,
                                                      record_type,
                                                      record_id);
    default:
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "Unknown Record Type: %X\n",
                      record_type);
      break;
    }
  
  return (0);
}
