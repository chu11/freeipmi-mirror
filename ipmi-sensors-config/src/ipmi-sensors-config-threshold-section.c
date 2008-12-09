/* 
   Copyright (C) 2008 FreeIPMI Core Team
   
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
#include <assert.h>

#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-sensor-event-enable-common.h"
#include "ipmi-sensors-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

/* 3% range is what we'll go with right now */
#define THRESHOLD_RANGE_MIN_MULTIPLIER 0.97
#define THRESHOLD_RANGE_MAX_MULTIPLIER 1.03

static config_err_t
_get_sdr_decoding_data(ipmi_sensors_config_state_data_t *state_data,
                       uint8_t *sdr_record,
                       unsigned int sdr_record_len,
                       int8_t *r_exponent,
                       int8_t *b_exponent,
                       int16_t *m,
                       int16_t *b,
                       uint8_t *linearization,
                       uint8_t *analog_data_format)
{
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(r_exponent);
  assert(b_exponent);
  assert(m);
  assert(b);
  assert(linearization);
  assert(analog_data_format);

  if (sdr_cache_get_sensor_decoding_data(NULL,
                                         sdr_record,
                                         sdr_record_len,
                                         r_exponent,
                                         b_exponent,
                                         m,
                                         b,
                                         linearization,
                                         analog_data_format) < 0)
    goto cleanup;

  /* if the sensor is not analog, this is most likely a bug in the
   * SDR, since we shouldn't be decoding a non-threshold sensor.
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(*analog_data_format))
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Attempting to decode non-analog threshold\n");
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  /* if the sensor is non-linear, I just don't know what to do */
  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR(*linearization))
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Cannot decode non-linear threshold\n");
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return rv;
}
                       
static config_err_t
_decode_value(ipmi_sensors_config_state_data_t *state_data,
              uint8_t *sdr_record,
              unsigned int sdr_record_len,
              uint64_t value_raw,
              double *value_calc)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(value_calc);

  if ((ret = _get_sdr_decoding_data (state_data,
                                     sdr_record,
                                     sdr_record_len,
                                     &r_exponent,
                                     &b_exponent,
                                     &m,
                                     &b,
                                     &linearization,
                                     &analog_data_format)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }                                     

  if (ipmi_sensor_decode_value (r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                value_raw,
                                value_calc) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_sensor_decode_value: %s\n",
                         strerror(errno));
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return rv;
}

static config_err_t
_decode_value_raw(ipmi_sensors_config_state_data_t *state_data,
                  uint8_t *sdr_record,
                  unsigned int sdr_record_len,
                  const char *threshold_input,
                  uint8_t *threshold_raw)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  double threshold_value;
  char *ptr;
  
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(threshold_input);
  assert(threshold_raw);

  if ((ret = _get_sdr_decoding_data (state_data,
                                     sdr_record,
                                     sdr_record_len,
                                     &r_exponent,
                                     &b_exponent,
                                     &m,
                                     &b,
                                     &linearization,
                                     &analog_data_format)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }                                     

  threshold_value = strtod(threshold_input, &ptr);
  if (*ptr != '\0')
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Invalid input: %s\n",
                         threshold_input);
      /* fatal error, should have been validated earlier */
      goto cleanup;
    }

  if (ipmi_sensor_decode_raw_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linearization,
                                    analog_data_format,
                                    threshold_value,
                                    threshold_raw) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_sensor_decode_value: %s\n",
                         strerror(errno));
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return rv;
}

static config_err_t
threshold_checkout (const char *section_name,
                    struct config_keyvalue *kv,
                    void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  char *readable_str;
  char *threshold_str;
  uint64_t readable;
  uint64_t threshold_raw;
  double threshold_calc;
  uint8_t sensor_number;

  if ((ret = get_sdr_record(state_data,
                            section_name,
                            sdr_record,
                            &sdr_record_len)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (sdr_cache_get_sensor_number(NULL,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_number) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sensor_thresholds_rs);

  if (ipmi_cmd_get_sensor_thresholds (state_data->ipmi_ctx,
                                      sensor_number,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_sensor_thresholds: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));

      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;

      /*
       * IPMI Workaround (achu)
       *
       * Discovered on HP DL585
       *
       * Seems that the HP machine doesn't support the "Get Sensor
       * Thresholds" command.  99% of the time if a command is invalid
       * on a remote machine, that's a fatal error and we should exit.
       * I suppose this is an exception though.  We can continue on
       * even if this command isn't supported.  The user just doesn't
       * get to configure these thresholds.
       */
      if ((ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND)
          && (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_COMMAND_INVALID) == 1))
        rv = CONFIG_ERR_NON_FATAL_ERROR;

      goto cleanup;
    }

  if (!strcasecmp(kv->key->key_name, "Lower_Non_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.lower_non_critical_threshold";
      threshold_str = "lower_non_critical_threshold";
    }
  else if (!strcasecmp(kv->key->key_name, "Lower_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.lower_critical_threshold";
      threshold_str = "lower_critical_threshold";
    }
  else if (!strcasecmp(kv->key->key_name, "Lower_Non_Recoverable_Threshold"))
    {
      readable_str = "readable_thresholds.lower_non_recoverable_threshold";
      threshold_str = "lower_non_recoverable_threshold";
    }
  else if (!strcasecmp(kv->key->key_name, "Upper_Non_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.upper_non_critical_threshold";
      threshold_str = "upper_non_critical_threshold";
    }
  else if (!strcasecmp(kv->key->key_name, "Upper_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.upper_critical_threshold";
      threshold_str = "upper_critical_threshold";
    }
  else if (!strcasecmp(kv->key->key_name, "Upper_Non_Recoverable_Threshold"))
    {
      readable_str = "readable_thresholds.upper_non_recoverable_threshold";
      threshold_str = "upper_non_recoverable_threshold";
    }
  else
    /* unknown key_name - fatal error */
    goto cleanup;

  _FIID_OBJ_GET (obj_cmd_rs, readable_str, &readable);

  if (!readable)
    {
      /* Inconsistency w/ the SDR, should be readable */
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "%s:%s - threshold not readable\n",
                        section_name,
                        kv->key->key_name);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, threshold_str, &threshold_raw);

  if ((ret = _decode_value(state_data,
                           sdr_record,
                           sdr_record_len,
                           threshold_raw,
                           &threshold_calc)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (config_section_update_keyvalue_output_double(state_data->pstate,
                                                   kv, 
                                                   threshold_calc) < 0)
    goto cleanup;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
threshold_commit (const char *section_name,
                  const struct config_keyvalue *kv,
                  void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t *lower_non_critical_threshold_ptr = NULL;
  uint8_t *lower_critical_threshold_ptr = NULL;
  uint8_t *lower_non_recoverable_threshold_ptr = NULL;
  uint8_t *upper_non_critical_threshold_ptr = NULL;
  uint8_t *upper_critical_threshold_ptr = NULL;
  uint8_t *upper_non_recoverable_threshold_ptr = NULL;
  uint8_t threshold_raw;
  uint8_t sensor_number;

  if ((ret = get_sdr_record(state_data,
                            section_name,
                            sdr_record,
                            &sdr_record_len)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (sdr_cache_get_sensor_number(NULL,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_number) < 0)
    goto cleanup;

  if ((ret = _decode_value_raw(state_data,
                               sdr_record,
                               sdr_record_len,
                               kv->value_input,
                               &threshold_raw)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp(kv->key->key_name, "Lower_Non_Critical_Threshold"))
    lower_non_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp(kv->key->key_name, "Lower_Critical_Threshold"))
    lower_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp(kv->key->key_name, "Lower_Non_Recoverable_Threshold"))
    lower_non_recoverable_threshold_ptr = &threshold_raw;
  else if (!strcasecmp(kv->key->key_name, "Upper_Non_Critical_Threshold"))
    upper_non_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp(kv->key->key_name, "Upper_Critical_Threshold"))
    upper_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp(kv->key->key_name, "Upper_Non_Recoverable_Threshold"))
    upper_non_recoverable_threshold_ptr = &threshold_raw;
  else
    /* unknown key_name - fatal error */
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_sensor_thresholds_rs);

  if (ipmi_cmd_set_sensor_thresholds (state_data->ipmi_ctx,
                                      sensor_number,
                                      lower_non_critical_threshold_ptr,
                                      lower_critical_threshold_ptr,
                                      lower_non_recoverable_threshold_ptr,
                                      upper_non_critical_threshold_ptr,
                                      upper_critical_threshold_ptr,
                                      upper_non_recoverable_threshold_ptr,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_sensor_thresholds: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_get_hysteresis (ipmi_sensors_config_state_data_t *state_data,
                 uint8_t sensor_number,
                 uint8_t *positive_going_threshold_hysteresis_value,
                 uint8_t *negative_going_threshold_hysteresis_value)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint64_t val;

  assert(state_data);
  assert(positive_going_threshold_hysteresis_value);
  assert(negative_going_threshold_hysteresis_value);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sensor_hysteresis_rs);

  if (ipmi_cmd_get_sensor_hysteresis (state_data->ipmi_ctx,
                                      sensor_number,
                                      IPMI_SENSOR_HYSTERESIS_MASK,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_sensor_hysteresis: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET (obj_cmd_rs, "positive_going_threshold_hysteresis_value", &val);
  *positive_going_threshold_hysteresis_value = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "negative_going_threshold_hysteresis_value", &val);
  *negative_going_threshold_hysteresis_value = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
hysteresis_threshold_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t positive_going_threshold_hysteresis_value;
  uint8_t negative_going_threshold_hysteresis_value;
  uint8_t value_raw;
  double value_calc;
  uint8_t hysteresis_support;
  uint8_t sensor_number;
  
  if ((ret = get_sdr_record(state_data,
                            section_name,
                            sdr_record,
                            &sdr_record_len)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (sdr_cache_get_sensor_capabilities (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         NULL,
                                         &hysteresis_support,
                                         NULL,
                                         NULL) < 0)
    goto cleanup;

  /* achu: shouldn't hit this, was calculated during section setup.
   * verbose mode should hit 'undefined' checkout
   */
  if (hysteresis_support != IPMI_SDR_READABLE_HYSTERESIS_SUPPORT
      && hysteresis_support != IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (sdr_cache_get_sensor_number(NULL,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_number) < 0)
    goto cleanup;

  if ((ret = _get_hysteresis (state_data,
                              sensor_number,
                              &positive_going_threshold_hysteresis_value,
                              &negative_going_threshold_hysteresis_value)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp(kv->key->key_name, "Positive_Going_Threshold_Hysteresis"))
    value_raw = positive_going_threshold_hysteresis_value;
  else if (!strcasecmp(kv->key->key_name, "Negative_Going_Threshold_Hysteresis"))
    value_raw = negative_going_threshold_hysteresis_value;
  else
    /* unknown key_name - fatal error */
    goto cleanup;
   
  /* 0 means hysteresis is not used, so don't decode */
  if (value_raw == 0)
    {
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv, 
                                                "None") < 0)
        goto cleanup;
    }
  else
    {
      if ((ret = _decode_value(state_data,
                               sdr_record,
                               sdr_record_len,
                               value_raw,
                               &value_calc)) != CONFIG_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }
  
      if (config_section_update_keyvalue_output_double(state_data->pstate,
                                                       kv, 
                                                       value_calc) < 0)
        goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static config_err_t
hysteresis_threshold_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t positive_going_threshold_hysteresis_value;
  uint8_t negative_going_threshold_hysteresis_value;
  uint8_t value_raw;
  uint8_t hysteresis_support;
  uint8_t sensor_number;

  if ((ret = get_sdr_record(state_data,
                            section_name,
                            sdr_record,
                            &sdr_record_len)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (sdr_cache_get_sensor_capabilities (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         NULL,
                                         &hysteresis_support,
                                         NULL,
                                         NULL) < 0)
    goto cleanup;

  /* achu: shouldn't hit this, was calculated during section setup.
   */
  if (hysteresis_support != IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (sdr_cache_get_sensor_number(NULL,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_number) < 0)
    goto cleanup;
  
  if ((ret = _get_hysteresis (state_data,
                              sensor_number,
                              &positive_going_threshold_hysteresis_value,
                              &negative_going_threshold_hysteresis_value)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  /* "None" means hysteresis is not used, so don't decode */
  
  if (!strcasecmp(kv->value_input, "None"))
    value_raw = 0;
  else
    {
      if ((ret = _decode_value_raw(state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   kv->value_input,
                                   &value_raw)) != CONFIG_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }
    }

  if (!strcasecmp(kv->key->key_name, "Positive_Going_Threshold_Hysteresis"))
    positive_going_threshold_hysteresis_value = value_raw;
  else if (!strcasecmp(kv->key->key_name, "Negative_Going_Threshold_Hysteresis" ))
    negative_going_threshold_hysteresis_value = value_raw;
  else
    /* unknown key_name - fatal error */
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_sensor_hysteresis_rs);

  if (ipmi_cmd_set_sensor_hysteresis (state_data->ipmi_ctx,
                                      sensor_number,
                                      IPMI_SENSOR_HYSTERESIS_MASK,
                                      positive_going_threshold_hysteresis_value,
                                      negative_going_threshold_hysteresis_value,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_sensor_hysteresis: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

/* achu:
 *
 * The range of potential inputs is limited by the sensor decoding
 * values and the "range" of values it can convert the threshold-raw
 * value into.  Also, the threshold raw data is a 1 byte field, which
 * may be signed or unsigned.
 *
 * Outside of some math I currently don't want to think about, there
 * is really no way to determine if the raw data that is calculated by
 * ipmi_sensor_decode_raw_value() is within range at the end.  So the
 * way that we'll check for a valid input range is to get the raw
 * value, then convert is back to a calculated value.  If we get a
 * value that is reasonably close to what the user input, we'll
 * consider the input from the user legit.
 */
static config_validate_t
_floating_point_in_range(const char *section_name, 
                         const char *key_name,
                         const char *value,
                         double value_input,
                         void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_validate_t rv = CONFIG_VALIDATE_FATAL_ERROR;
  config_err_t ret;
  uint8_t threshold_raw;
  uint8_t sensor_number;
  double threshold_calc;
  double threshold_range_min, threshold_range_max;
  
  if ((ret = get_sdr_record(state_data,
                            section_name,
                            sdr_record,
                            &sdr_record_len)) != CONFIG_ERR_SUCCESS)
    {
      if (ret == CONFIG_ERR_NON_FATAL_ERROR)
        rv = CONFIG_VALIDATE_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (sdr_cache_get_sensor_number(NULL,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_number) < 0)
    goto cleanup;
  
  if ((ret = _decode_value_raw(state_data,
                               sdr_record,
                               sdr_record_len,
                               value,
                               &threshold_raw)) != CONFIG_ERR_SUCCESS)
    {
      if (ret == CONFIG_ERR_NON_FATAL_ERROR)
        rv = CONFIG_VALIDATE_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if ((ret = _decode_value(state_data,
                           sdr_record,
                           sdr_record_len,
                           threshold_raw,
                           &threshold_calc)) != CONFIG_ERR_SUCCESS)
    {
      if (ret == CONFIG_ERR_NON_FATAL_ERROR)
        rv = CONFIG_VALIDATE_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  threshold_range_min = threshold_calc * THRESHOLD_RANGE_MIN_MULTIPLIER;
  threshold_range_max = threshold_calc * THRESHOLD_RANGE_MAX_MULTIPLIER;

  /* achu: technically shouldn't compare doubles to constants, but I
   * think its ok here.
   */
  if ((value_input >= 0.0
       && (value_input < threshold_range_min
           || value_input > threshold_range_max))
      || (value_input < 0.0
          && (value_input > threshold_range_min
              || value_input < threshold_range_max)))
    rv = CONFIG_VALIDATE_OUT_OF_RANGE_VALUE;
  else
    rv = CONFIG_VALIDATE_VALID_VALUE;
  
 cleanup:
  return rv;
}

config_validate_t 
threshold_validate(const char *section_name, 
                   const char *key_name,
                   const char *value,
                   void *arg)
{
  double conv;
  char *endptr;

  assert(value);

  conv = strtod(value, &endptr);

  if (*endptr)
    return CONFIG_VALIDATE_INVALID_VALUE;

  return _floating_point_in_range(section_name,
                                  key_name,
                                  value,
                                  conv,
                                  arg);
}

config_validate_t 
threshold_validate_positive(const char *section_name, 
                            const char *key_name,
                            const char *value,
                            void *arg)
{
  double conv;
  char *endptr;

  assert(value);

  conv = strtod(value, &endptr);

  if (*endptr)
    return CONFIG_VALIDATE_INVALID_VALUE;

  if (conv < 0.0)
    return CONFIG_VALIDATE_OUT_OF_RANGE_VALUE;

  return _floating_point_in_range(section_name,
                                  key_name,
                                  value,
                                  conv,
                                  arg);
}

config_validate_t 
hysteresis_threshold_validate(const char *section_name, 
                              const char *key_name,
                              const char *value,
                              void *arg)
{
  double conv;
  char *endptr;

  assert(value);

  if (!strcasecmp(value, "None"))
    return CONFIG_VALIDATE_VALID_VALUE;

  conv = strtod(value, &endptr);

  if (*endptr)
    return CONFIG_VALIDATE_INVALID_VALUE;

  return _floating_point_in_range(section_name,
                                  key_name,
                                  value,
                                  conv,
                                  arg);
}

config_validate_t 
hysteresis_threshold_validate_positive(const char *section_name, 
                                       const char *key_name,
                                       const char *value,
                                       void *arg)
{
  double conv;
  char *endptr;

  assert(value);

  if (!strcasecmp(value, "None"))
    return CONFIG_VALIDATE_VALID_VALUE;
  
  conv = strtod(value, &endptr);

  if (*endptr)
    return CONFIG_VALIDATE_INVALID_VALUE;

  if (conv < 0.0)
    return CONFIG_VALIDATE_OUT_OF_RANGE_VALUE;

  return _floating_point_in_range(section_name,
                                  key_name,
                                  value,
                                  conv,
                                  arg);
}

static int
_setup_threshold_key (ipmi_sensors_config_state_data_t *state_data,
                      struct config_section *section,
                      const char *description,
                      const char *key_name,
                      uint8_t threshold_readable,
                      uint8_t threshold_settable,
                      Key_Validate threshold_validate_ptr)
{
  unsigned int flags = 0;

  assert(state_data);
  assert(description);
  assert(key_name);
  assert(threshold_validate_ptr);

  if (threshold_readable
      || state_data->prog_data->args->config_args.verbose)
    {
      if (!threshold_readable)
        flags |= CONFIG_UNDEFINED;
      
      if (!threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }
      
      if (config_section_add_key (state_data->pstate,
                                  section,
                                  key_name,
                                  description,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  threshold_validate_ptr) < 0)
        goto cleanup;
    }
  
  return 0;

 cleanup:
  return -1;
}

static int
_setup_threshold_fields (ipmi_sensors_config_state_data_t *state_data,
                         uint8_t *sdr_record,
                         unsigned int sdr_record_len,
                         struct config_section *section,
                         const char *description,
                         uint8_t sensor_unit)
{
  uint8_t lower_non_critical_threshold_settable = 0;
  uint8_t lower_critical_threshold_settable = 0;
  uint8_t lower_non_recoverable_threshold_settable = 0;
  uint8_t upper_non_critical_threshold_settable = 0;
  uint8_t upper_critical_threshold_settable = 0;
  uint8_t upper_non_recoverable_threshold_settable = 0;
  uint8_t lower_non_critical_threshold_readable = 0;
  uint8_t lower_critical_threshold_readable = 0;
  uint8_t lower_non_recoverable_threshold_readable = 0;
  uint8_t upper_non_critical_threshold_readable = 0;
  uint8_t upper_critical_threshold_readable = 0;
  uint8_t upper_non_recoverable_threshold_readable = 0;
  Key_Validate threshold_validate_ptr = NULL;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);
  assert(description);

  /* We will adjust this list as necessary later on.  Many
   * measurements could technically be negative (i.e. temperature)
   * even though its unrealistic for IPMI's sake.  Others, I'm just
   * not sure about.
   */
  if (sensor_unit == IPMI_SENSOR_UNIT_RPM)
    threshold_validate_ptr = threshold_validate_positive;
  else
    threshold_validate_ptr = threshold_validate;

  if (sdr_cache_get_threshold_readable (NULL,
                                        sdr_record,
                                        sdr_record_len,
                                        &lower_non_critical_threshold_readable,
                                        &lower_critical_threshold_readable,
                                        &lower_non_recoverable_threshold_readable,
                                        &upper_non_critical_threshold_readable,
                                        &upper_critical_threshold_readable,
                                        &upper_non_recoverable_threshold_readable) < 0)
    goto cleanup;

  if (sdr_cache_get_threshold_settable (NULL,
                                        sdr_record,
                                        sdr_record_len,
                                        &lower_non_critical_threshold_settable,
                                        &lower_critical_threshold_settable,
                                        &lower_non_recoverable_threshold_settable,
                                        &upper_non_critical_threshold_settable,
                                        &upper_critical_threshold_settable,
                                        &upper_non_recoverable_threshold_settable) < 0)
    goto cleanup;

  if (_setup_threshold_key (state_data,
                            section,
                            description,
                            "Lower_Non_Critical_Threshold",
                            lower_non_critical_threshold_readable,
                            lower_non_critical_threshold_settable,
                            threshold_validate_ptr) < 0)
    goto cleanup;

  if (_setup_threshold_key (state_data,
                            section,
                            description,
                            "Lower_Critical_Threshold",
                            lower_critical_threshold_readable,
                            lower_critical_threshold_settable,
                            threshold_validate_ptr) < 0)
    goto cleanup;

  if (_setup_threshold_key (state_data,
                            section,
                            description,
                            "Lower_Non_Recoverable_Threshold",
                            lower_non_recoverable_threshold_readable,
                            lower_non_recoverable_threshold_settable,
                            threshold_validate_ptr) < 0)
    goto cleanup;

  if (_setup_threshold_key (state_data,
                            section,
                            description,
                            "Upper_Non_Critical_Threshold",
                            upper_non_critical_threshold_readable,
                            upper_non_critical_threshold_settable,
                            threshold_validate_ptr) < 0)
    goto cleanup;

  if (_setup_threshold_key (state_data,
                            section,
                            description,
                            "Upper_Critical_Threshold",
                            upper_critical_threshold_readable,
                            upper_critical_threshold_settable,
                            threshold_validate_ptr) < 0)
    goto cleanup;

  if (_setup_threshold_key (state_data,
                            section,
                            description,
                            "Upper_Non_Recoverable_Threshold",
                            upper_non_recoverable_threshold_readable,
                            upper_non_recoverable_threshold_settable,
                            threshold_validate_ptr) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return rv;
}

static int
_setup_threshold_hysteresis_fields (ipmi_sensors_config_state_data_t *state_data,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    struct config_section *section,
                                    const char *description,
                                    uint8_t sensor_unit,
                                    uint8_t hysteresis_support)
{
  unsigned int flags = 0;
  char description_hysteresis[CONFIG_MAX_DESCRIPTION_LEN];
  Key_Validate hysteresis_threshold_validate_ptr = NULL;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);
  assert(description);
  assert(hysteresis_support == IPMI_SDR_READABLE_HYSTERESIS_SUPPORT
         || hysteresis_support == IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT
         || state_data->prog_data->args->config_args.verbose);

  if (hysteresis_support == IPMI_SDR_READABLE_HYSTERESIS_SUPPORT)
    {
      flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
      flags |= CONFIG_READABLE_ONLY;
    }
  else if (hysteresis_support == IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT)
    flags = 0;                  /* no change, can read/write */
  else /* state_data->prog_data->args->config_args.verbose */
    flags = CONFIG_UNDEFINED;
    
  memset(description_hysteresis, '\0', CONFIG_MAX_DESCRIPTION_LEN);
  snprintf(description_hysteresis, 
           CONFIG_MAX_DESCRIPTION_LEN,
           "%s; 'None' to not use hysteresis",
           description);

  /* We will adjust this list as necessary later on.  Many
   * measurements could technically be negative (i.e. temperature)
   * even though its unrealistic for IPMI's sake.  Others, I'm just
   * not sure about.
   */
  if (sensor_unit == IPMI_SENSOR_UNIT_RPM)
    hysteresis_threshold_validate_ptr = hysteresis_threshold_validate_positive;
  else
    hysteresis_threshold_validate_ptr = hysteresis_threshold_validate;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Positive_Going_Threshold_Hysteresis",
                              description_hysteresis,
                              flags,
                              hysteresis_threshold_checkout,
                              hysteresis_threshold_commit,
                              hysteresis_threshold_validate_ptr) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Negative_Going_Threshold_Hysteresis",
                              description_hysteresis,
                              flags,
                              hysteresis_threshold_checkout,
                              hysteresis_threshold_commit,
                              hysteresis_threshold_validate_ptr) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return rv;
}

config_err_t
ipmi_sensors_config_threshold_section (ipmi_sensors_config_state_data_t *state_data,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       struct config_section **section_ptr)
{
  struct config_section *section = NULL;
  char section_name[CONFIG_MAX_SECTION_NAME_LEN];
  uint8_t threshold_access_support = 0;
  uint8_t hysteresis_support = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t sensor_type, sensor_unit;
  char description[CONFIG_MAX_DESCRIPTION_LEN];

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section_ptr);

  if ((ret = create_section_name (state_data,
                                  sdr_record,
                                  sdr_record_len,
                                  section_name,
                                  CONFIG_MAX_SECTION_NAME_LEN)) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "create_section_name: %s\n",
                        strerror(errno));
      rv = ret;
      goto cleanup;
    }

  if (!(section = config_section_create (state_data->pstate,
                                         section_name,
                                         NULL,
                                         NULL,
                                         0,
                                         NULL,
                                         NULL)))
    goto cleanup;

  if (sdr_cache_get_sensor_capabilities (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         &threshold_access_support,
                                         &hysteresis_support,
                                         NULL,
                                         NULL) < 0)
    goto cleanup;

  if (sdr_cache_get_sensor_type (NULL,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_type) < 0)
    goto cleanup;

  if (sdr_cache_get_sensor_unit (NULL,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_unit) < 0)
    goto cleanup;

  memset(description, '\0', CONFIG_MAX_DESCRIPTION_LEN);
  if (IPMI_SENSOR_UNIT_VALID(sensor_unit)
      && sensor_unit != IPMI_SENSOR_UNIT_UNSPECIFIED)
    snprintf(description, 
             CONFIG_MAX_DESCRIPTION_LEN,
             "Give valid input for sensor type = %s; units = %s",
             sensor_group (sensor_type),
             ipmi_sensor_units[sensor_unit]);
  else
    snprintf(description, 
             CONFIG_MAX_DESCRIPTION_LEN,
             "Give valid input for sensor type = %s",
             sensor_group (sensor_type));

  if (setup_sensor_event_enable_fields (state_data,
                                        sdr_record,
                                        sdr_record_len,
                                        section) < 0)
    goto cleanup;

  if (threshold_access_support == IPMI_SDR_READABLE_THRESHOLDS_SUPPORT
      || threshold_access_support == IPMI_SDR_READABLE_SETTABLE_THRESHOLDS_SUPPORT
      || state_data->prog_data->args->config_args.verbose)
    {
      if (_setup_threshold_fields (state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   section,
                                   description,
                                   sensor_unit) < 0)
        goto cleanup;
    }

  if (hysteresis_support == IPMI_SDR_READABLE_HYSTERESIS_SUPPORT
      || hysteresis_support == IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT
      || state_data->prog_data->args->config_args.verbose)
    {
      if (_setup_threshold_hysteresis_fields (state_data,
                                              sdr_record,
                                              sdr_record_len,
                                              section,
                                              description,
                                              sensor_unit,
                                              hysteresis_support) < 0)
        goto cleanup;
    }

  *section_ptr = section;
  return CONFIG_ERR_SUCCESS;

 cleanup:
  if (section)
    config_section_destroy(state_data->pstate, section);
  return rv;
}
