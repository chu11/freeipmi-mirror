/* 
   Copyright (C) 2007-2008 FreeIPMI Core Team
   
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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-sensors-config.h"
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
_calculate_threshold(ipmi_sensors_config_state_data_t *state_data,
                     uint8_t *sdr_record,
                     unsigned int sdr_record_len,
                     uint64_t threshold_raw,
                     double *threshold_calc)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(threshold_calc);

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
                                threshold_raw,
                                threshold_calc) < 0)
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

  if (Fiid_obj_get (state_data->pstate,
                    obj_cmd_rs,
                    readable_str, 
                    &readable) < 0)
    goto cleanup;

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

  if (Fiid_obj_get (state_data->pstate,
                    obj_cmd_rs, 
                    threshold_str, 
                    &threshold_raw) < 0)
    goto cleanup;

  if ((ret = _calculate_threshold(state_data,
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
_calculate_threshold_raw(ipmi_sensors_config_state_data_t *state_data,
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

  if ((ret = _calculate_threshold_raw(state_data,
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
_threshold_in_range(const char *section_name, 
                    const char *key_name,
                    const char *value,
                    double threshold_input,
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

  if ((ret = _calculate_threshold_raw(state_data,
                                      sdr_record,
                                      sdr_record_len,
                                      value,
                                      &threshold_raw)) != CONFIG_ERR_SUCCESS)
    {
      if (ret == CONFIG_ERR_NON_FATAL_ERROR)
        rv = CONFIG_VALIDATE_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((ret = _calculate_threshold(state_data,
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
  if ((threshold_input >= 0.0
       && (threshold_input < threshold_range_min
           || threshold_input > threshold_range_max))
      || (threshold_input < 0.0
          && (threshold_input > threshold_range_min
              || threshold_input < threshold_range_max)))
    rv = CONFIG_VALIDATE_OUT_OF_RANGE_VALUE;
  else
    rv = CONFIG_VALIDATE_VALID_VALUE;
  
 cleanup:
  return rv;
}

config_validate_t 
threshold_floating_point(const char *section_name, 
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

  return _threshold_in_range(section_name,
                             key_name,
                             value,
                             conv,
                             arg);
}

config_validate_t 
threshold_floating_point_positive(const char *section_name, 
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

  return _threshold_in_range(section_name,
                             key_name,
                             value,
                             conv,
                             arg);
}

config_err_t
ipmi_sensors_config_threshold_section (ipmi_sensors_config_state_data_t *state_data,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       struct config_section **section_ptr)
{
  struct config_section *section = NULL;
  char section_name[CONFIG_MAX_SECTION_NAME_LEN];
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint16_t record_id;
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
  unsigned int flags;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t sensor_type, sensor_unit;
  char desc[CONFIG_MAX_DESCRIPTION_LEN];
  Key_Validate validate_ptr = NULL;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section_ptr);

  if (sdr_cache_get_record_id_and_type (NULL,
                                        sdr_record,
                                        sdr_record_len,
                                        &record_id,
                                        NULL) < 0)
    goto cleanup;

  memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);

  if (sdr_cache_get_id_string (NULL,
                               sdr_record,
                               sdr_record_len,
                               id_string,
                               IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
    goto cleanup;

  if ((ret = convert_id_string (state_data, id_string)) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr, 
                        "convert_id_string: %s\n",
                        strerror(errno));
      rv = ret;
      goto cleanup;
    }

  /* We will name sections by record_id then name, since id_strings
   * could be identical.
   */
  /* achu: I know CONFIG_MAX_SECTION_NAME_LEN is much larger than
   * IPMI_SDR_CACHE_MAX_ID_STRING.  We should probably do some math
   * instead of just using macros flat out though.
   */
  if (strlen(id_string) > 0)
    snprintf(section_name,
             CONFIG_MAX_SECTION_NAME_LEN,
             "%u_%s",
             record_id,
             id_string);
  else
    /* I guess its conceivable the sensor won't have a name, so we
     * make one up.
     */
    snprintf(section_name,
             CONFIG_MAX_SECTION_NAME_LEN,
             "%u_%s",
             record_id,
             "Unknown_Sensor_Name");

  if (!(section = config_section_create (state_data->pstate,
                                         section_name,
                                         NULL,
                                         NULL,
                                         0)))
    goto cleanup;

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

  memset(desc, '\0', CONFIG_MAX_DESCRIPTION_LEN);
  snprintf(desc, 
           CONFIG_MAX_DESCRIPTION_LEN,
           "Give valid input for sensor type = %s; units = %s",
           sensor_group (sensor_type),
           ipmi_sensor_units[sensor_unit]);

  /* We will adjust this list as necessary later on.  Many
   * measurements could technically be negative (i.e. temperature)
   * even though its unrealistic for IPMI's sake.  Others, I'm just
   * not sure about.
   */
  if (sensor_unit == IPMI_SENSOR_UNIT_RPM)
    validate_ptr = threshold_floating_point_positive;
  else
    validate_ptr = threshold_floating_point;

  if (lower_non_critical_threshold_readable
      || state_data->prog_data->args->config_args.verbose)
    {
      flags = 0;

      if (!lower_non_critical_threshold_readable)
        flags |= CONFIG_UNDEFINED;

      if (!lower_non_critical_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Lower_Non_Critical_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  validate_ptr) < 0)
        goto cleanup;
    }

  if (lower_critical_threshold_readable
      || state_data->prog_data->args->config_args.verbose)
    {
      flags = 0;

      if (!lower_critical_threshold_readable)
        flags |= CONFIG_UNDEFINED;

      if (!lower_critical_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Lower_Critical_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  validate_ptr) < 0)
        goto cleanup;
    }

  if (lower_non_recoverable_threshold_readable
      || state_data->prog_data->args->config_args.verbose)
    {
      flags = 0;

      if (!lower_non_recoverable_threshold_readable)
        flags |= CONFIG_UNDEFINED;

      if (!lower_non_recoverable_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Lower_Non_Recoverable_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  validate_ptr) < 0)
        goto cleanup;
    }

  if (upper_non_critical_threshold_readable
      || state_data->prog_data->args->config_args.verbose)
    {
      flags = 0;

      if (!upper_non_critical_threshold_readable)
        flags |= CONFIG_UNDEFINED;

      if (!upper_non_critical_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Upper_Non_Critical_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  validate_ptr) < 0)
        goto cleanup;
    }

  if (upper_critical_threshold_readable
      || state_data->prog_data->args->config_args.verbose)
    {
      flags = 0;

      if (!upper_critical_threshold_readable)
        flags |= CONFIG_UNDEFINED;

      if (!upper_critical_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Upper_Critical_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  validate_ptr) < 0)
        goto cleanup;
    }

  if (upper_non_recoverable_threshold_readable
      || state_data->prog_data->args->config_args.verbose)
    {
      flags = 0;

      if (!upper_non_recoverable_threshold_readable)
        flags |= CONFIG_UNDEFINED;

      if (!upper_non_recoverable_threshold_settable)
        {
          flags |= CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= CONFIG_READABLE_ONLY;
        }

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Upper_Non_Recoverable_Threshold",
                                  desc,
                                  flags,
                                  threshold_checkout,
                                  threshold_commit,
                                  validate_ptr) < 0)
        goto cleanup;
    }

  *section_ptr = section;
  return CONFIG_ERR_SUCCESS;

 cleanup:
  if (section)
    config_section_destroy(state_data->pstate, section);
  return rv;
}
