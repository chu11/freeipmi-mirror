/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "ipmi-config.h"
#include "ipmi-config-section.h"
#include "ipmi-config-category-sensors-sensor-event-enable-common.h"
#include "ipmi-config-category-sensors-utils.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

/* 3% range is what we'll go with right now */
#define THRESHOLD_RANGE_MIN_MULTIPLIER 0.97
#define THRESHOLD_RANGE_MAX_MULTIPLIER 1.03

#define UNRECOGNIZED_SENSOR_TYPE "Unrecognized"

#define IPMI_CONFIG_CATEGORY_SENSORS_UNITS_BUFLEN 1024

static ipmi_config_err_t
_get_sdr_decoding_data (ipmi_config_state_data_t *state_data,
                        int8_t *r_exponent,
                        int8_t *b_exponent,
                        int16_t *m,
                        int16_t *b,
                        uint8_t *linearization,
                        uint8_t *analog_data_format)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (r_exponent);
  assert (b_exponent);
  assert (m);
  assert (b);
  assert (linearization);
  assert (analog_data_format);

  if (ipmi_sdr_parse_sensor_decoding_data (state_data->sdr_ctx,
					   NULL,
					   0,
                                           r_exponent,
                                           b_exponent,
                                           m,
                                           b,
                                           linearization,
                                           analog_data_format) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_decoding_data: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  /* if the sensor is not analog, this is most likely a bug in the
   * SDR, since we shouldn't be decoding a non-threshold sensor.
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (*analog_data_format))
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Attempting to decode non-analog threshold\n");
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  /* if the sensor is non-linear, I just don't know what to do */
  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (*linearization))
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Cannot decode non-linear threshold\n");
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
_decode_value (ipmi_config_state_data_t *state_data,
               uint8_t value_raw,
               double *value_calc)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (value_calc);

  if ((ret = _get_sdr_decoding_data (state_data,
                                     &r_exponent,
                                     &b_exponent,
                                     &m,
                                     &b,
                                     &linearization,
                                     &analog_data_format)) != IPMI_CONFIG_ERR_SUCCESS)
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
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sensor_decode_value: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
_decode_value_raw (ipmi_config_state_data_t *state_data,
                   const char *threshold_input,
                   uint8_t *threshold_raw)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  double threshold_value;
  char *ptr;

  assert (state_data);
  assert (threshold_input);
  assert (threshold_raw);

  if ((ret = _get_sdr_decoding_data (state_data,
                                     &r_exponent,
                                     &b_exponent,
                                     &m,
                                     &b,
                                     &linearization,
                                     &analog_data_format)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  errno = 0;
  threshold_value = strtod (threshold_input, &ptr);
  if (errno
      || ptr[0] != '\0')
    {
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
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sensor_decode_raw_value: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
threshold_checkout (ipmi_config_state_data_t *state_data,
		    const char *section_name,
                    struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  char *readable_str;
  char *threshold_str;
  uint8_t readable;
  uint8_t threshold_raw;
  uint64_t val;
  double threshold_calc;
  uint8_t sensor_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sensor_thresholds_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sensor_thresholds (state_data->ipmi_ctx,
                                      sensor_number,
                                      obj_cmd_rs) < 0)
    {
      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

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
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
          && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INVALID_COMMAND) == 1)
        rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_sensor_thresholds: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (!strcasecmp (kv->key->key_name, "Lower_Non_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.lower_non_critical_threshold";
      threshold_str = "lower_non_critical_threshold";
    }
  else if (!strcasecmp (kv->key->key_name, "Lower_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.lower_critical_threshold";
      threshold_str = "lower_critical_threshold";
    }
  else if (!strcasecmp (kv->key->key_name, "Lower_Non_Recoverable_Threshold"))
    {
      readable_str = "readable_thresholds.lower_non_recoverable_threshold";
      threshold_str = "lower_non_recoverable_threshold";
    }
  else if (!strcasecmp (kv->key->key_name, "Upper_Non_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.upper_non_critical_threshold";
      threshold_str = "upper_non_critical_threshold";
    }
  else if (!strcasecmp (kv->key->key_name, "Upper_Critical_Threshold"))
    {
      readable_str = "readable_thresholds.upper_critical_threshold";
      threshold_str = "upper_critical_threshold";
    }
  else if (!strcasecmp (kv->key->key_name, "Upper_Non_Recoverable_Threshold"))
    {
      readable_str = "readable_thresholds.upper_non_recoverable_threshold";
      threshold_str = "upper_non_recoverable_threshold";
    }
  else
    /* unknown key_name - fatal error */
    goto cleanup;

  if (FIID_OBJ_GET (obj_cmd_rs, readable_str, &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: '%s': %s\n",
                       readable_str,
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  readable = val;

  if (!readable)
    {
      /* Inconsistency w/ the SDR, should be readable */
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "%s:%s - threshold not readable\n",
                         section_name,
                         kv->key->key_name);

      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, threshold_str, &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: '%s': %s\n",
                       threshold_str,
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  threshold_raw = val;

  if ((ret = _decode_value (state_data,
                            threshold_raw,
                            &threshold_calc)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output_double (state_data,
							 kv,
							 threshold_calc) < 0)
    goto cleanup;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
threshold_commit (ipmi_config_state_data_t *state_data,
		  const char *section_name,
                  const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t *lower_non_critical_threshold_ptr = NULL;
  uint8_t *lower_critical_threshold_ptr = NULL;
  uint8_t *lower_non_recoverable_threshold_ptr = NULL;
  uint8_t *upper_non_critical_threshold_ptr = NULL;
  uint8_t *upper_critical_threshold_ptr = NULL;
  uint8_t *upper_non_recoverable_threshold_ptr = NULL;
  uint8_t threshold_raw;
  uint8_t sensor_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if ((ret = _decode_value_raw (state_data,
                                kv->value_input,
                                &threshold_raw)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp (kv->key->key_name, "Lower_Non_Critical_Threshold"))
    lower_non_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp (kv->key->key_name, "Lower_Critical_Threshold"))
    lower_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp (kv->key->key_name, "Lower_Non_Recoverable_Threshold"))
    lower_non_recoverable_threshold_ptr = &threshold_raw;
  else if (!strcasecmp (kv->key->key_name, "Upper_Non_Critical_Threshold"))
    upper_non_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp (kv->key->key_name, "Upper_Critical_Threshold"))
    upper_critical_threshold_ptr = &threshold_raw;
  else if (!strcasecmp (kv->key->key_name, "Upper_Non_Recoverable_Threshold"))
    upper_non_recoverable_threshold_ptr = &threshold_raw;
  else
    /* unknown key_name - fatal error */
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sensor_thresholds_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

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
      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_sensor_thresholds: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_hysteresis (ipmi_config_state_data_t *state_data,
                 uint8_t sensor_number,
                 uint8_t *positive_going_threshold_hysteresis_value,
                 uint8_t *negative_going_threshold_hysteresis_value)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  uint64_t val;

  assert (state_data);
  assert (positive_going_threshold_hysteresis_value);
  assert (negative_going_threshold_hysteresis_value);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sensor_hysteresis_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sensor_hysteresis (state_data->ipmi_ctx,
                                      sensor_number,
                                      IPMI_SENSOR_HYSTERESIS_MASK,
                                      obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_sensor_hysteresis: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "positive_going_threshold_hysteresis_value", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'positive_going_threshold_hysteresis_value': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  *positive_going_threshold_hysteresis_value = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "negative_going_threshold_hysteresis_value", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'negative_going_threshold_hysteresis_value': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  *negative_going_threshold_hysteresis_value = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
hysteresis_threshold_checkout (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t positive_going_threshold_hysteresis_value;
  uint8_t negative_going_threshold_hysteresis_value;
  uint8_t value_raw;
  double value_calc;
  uint8_t hysteresis_support;
  uint8_t sensor_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_capabilities (state_data->sdr_ctx,
					  NULL,
					  0,
                                          NULL,
                                          NULL,
                                          &hysteresis_support,
                                          NULL,
                                          NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_capabilities: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  /* achu: shouldn't hit this, was calculated during section setup.
   * verbose mode should hit 'undefined' checkout
   */
  if (hysteresis_support != IPMI_SDR_READABLE_HYSTERESIS_SUPPORT
      && hysteresis_support != IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT)
    {
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if ((ret = _get_hysteresis (state_data,
                              sensor_number,
                              &positive_going_threshold_hysteresis_value,
                              &negative_going_threshold_hysteresis_value)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp (kv->key->key_name, "Positive_Going_Threshold_Hysteresis"))
    value_raw = positive_going_threshold_hysteresis_value;
  else if (!strcasecmp (kv->key->key_name, "Negative_Going_Threshold_Hysteresis"))
    value_raw = negative_going_threshold_hysteresis_value;
  else
    /* unknown key_name - fatal error */
    goto cleanup;

  /* 0 means hysteresis is not used, so don't decode */
  if (value_raw == 0)
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
						      kv,
						      "None") < 0)
        goto cleanup;
    }
  else
    {
      if ((ret = _decode_value (state_data,
                                value_raw,
                                &value_calc)) != IPMI_CONFIG_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }

      if (ipmi_config_section_update_keyvalue_output_double (state_data,
							     kv,
							     value_calc) < 0)
        goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
hysteresis_threshold_commit (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t positive_going_threshold_hysteresis_value;
  uint8_t negative_going_threshold_hysteresis_value;
  uint8_t value_raw;
  uint8_t hysteresis_support;
  uint8_t sensor_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_capabilities (state_data->sdr_ctx,
					  NULL,
					  0,
                                          NULL,
                                          NULL,
                                          &hysteresis_support,
                                          NULL,
                                          NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_capabilities: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  /* achu: shouldn't hit this, was calculated during section setup.
   */
  if (hysteresis_support != IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT)
    {
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if ((ret = _get_hysteresis (state_data,
                              sensor_number,
                              &positive_going_threshold_hysteresis_value,
                              &negative_going_threshold_hysteresis_value)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  /* "None" means hysteresis is not used, so don't decode */

  if (!strcasecmp (kv->value_input, "None"))
    value_raw = 0;
  else
    {
      if ((ret = _decode_value_raw (state_data,
                                    kv->value_input,
                                    &value_raw)) != IPMI_CONFIG_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }
    }

  if (!strcasecmp (kv->key->key_name, "Positive_Going_Threshold_Hysteresis"))
    positive_going_threshold_hysteresis_value = value_raw;
  else if (!strcasecmp (kv->key->key_name, "Negative_Going_Threshold_Hysteresis" ))
    negative_going_threshold_hysteresis_value = value_raw;
  else
    /* unknown key_name - fatal error */
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sensor_hysteresis_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_sensor_hysteresis (state_data->ipmi_ctx,
                                      sensor_number,
                                      IPMI_SENSOR_HYSTERESIS_MASK,
                                      positive_going_threshold_hysteresis_value,
                                      negative_going_threshold_hysteresis_value,
                                      obj_cmd_rs) < 0)
    {
      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_sensor_hysteresis: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
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
static ipmi_config_validate_t
_floating_point_in_range (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const char *key_name,
                          const char *value,
                          double value_input)
{
  ipmi_config_validate_t rv = IPMI_CONFIG_VALIDATE_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t threshold_raw;
  uint8_t sensor_number;
  double threshold_calc;
  double threshold_range_min, threshold_range_max;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR)
        rv = IPMI_CONFIG_VALIDATE_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if ((ret = _decode_value_raw (state_data,
                                value,
                                &threshold_raw)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR)
        rv = IPMI_CONFIG_VALIDATE_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((ret = _decode_value (state_data,
                            threshold_raw,
                            &threshold_calc)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR)
        rv = IPMI_CONFIG_VALIDATE_NON_FATAL_ERROR;
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
    rv = IPMI_CONFIG_VALIDATE_VALUE_CANNOT_BE_ENCODED_ACCURATELY;
  else
    rv = IPMI_CONFIG_VALIDATE_VALID_VALUE;

 cleanup:
  return (rv);
}

ipmi_config_validate_t
threshold_validate (ipmi_config_state_data_t *state_data,
		    const char *section_name,
                    const char *key_name,
                    const char *value)
{
  double conv;
  char *endptr;

  assert (section_name);
  assert (key_name);
  assert (value);

  errno = 0;
  conv = strtod (value, &endptr);
  if (errno
      || endptr[0] != '\0')
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);

  return (_floating_point_in_range (state_data,
				    section_name,
                                    key_name,
                                    value,
                                    conv));
}

ipmi_config_validate_t
threshold_validate_positive (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const char *key_name,
                             const char *value)
{
  double conv;
  char *endptr;

  assert (section_name);
  assert (key_name);
  assert (value);

  errno = 0;
  conv = strtod (value, &endptr);
  if (errno
      || endptr[0] != '\0')
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);

  if (conv < 0.0)
    return (IPMI_CONFIG_VALIDATE_OUT_OF_RANGE_VALUE);

  return (_floating_point_in_range (state_data,
				    section_name,
                                    key_name,
                                    value,
                                    conv));
}

ipmi_config_validate_t
hysteresis_threshold_validate (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const char *key_name,
                               const char *value)
{
  double conv;
  char *endptr;

  assert (section_name);
  assert (key_name);
  assert (value);

  if (!strcasecmp (value, "None"))
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);

  errno = 0;
  conv = strtod (value, &endptr);
  if (errno
      || endptr[0] != '\0')
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);

  return (_floating_point_in_range (state_data,
				    section_name,
                                    key_name,
                                    value,
                                    conv));
}

ipmi_config_validate_t
hysteresis_threshold_validate_positive (ipmi_config_state_data_t *state_data,
					const char *section_name,
                                        const char *key_name,
                                        const char *value)
{
  double conv;
  char *endptr;

  assert (section_name);
  assert (key_name);
  assert (value);

  if (!strcasecmp (value, "None"))
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);

  errno = 0;
  conv = strtod (value, &endptr);
  if (errno
      || endptr[0] != '\0')
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);

  if (conv < 0.0)
    return (IPMI_CONFIG_VALIDATE_OUT_OF_RANGE_VALUE);

  return (_floating_point_in_range (state_data,
				    section_name,
                                    key_name,
                                    value,
                                    conv));
}

static int
_setup_threshold_key (ipmi_config_state_data_t *state_data,
                      struct ipmi_config_section *section,
                      const char *description,
                      const char *key_name,
                      uint8_t threshold_readable,
                      uint8_t threshold_settable,
                      Key_Validate threshold_validate_ptr)
{
  unsigned int flags = 0;

  assert (state_data);
  assert (description);
  assert (key_name);
  assert (threshold_validate_ptr);

  if (threshold_readable
      || state_data->prog_data->args->verbose_count)
    {
      if (!threshold_readable)
        flags |= IPMI_CONFIG_UNDEFINED;

      if (!threshold_settable)
        {
          flags |= IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
          flags |= IPMI_CONFIG_READABLE_ONLY;
        }

      if (ipmi_config_section_add_key (state_data,
				       section,
				       key_name,
				       description,
				       flags,
				       threshold_checkout,
				       threshold_commit,
				       threshold_validate_ptr) < 0)
        goto cleanup;
    }

  return (0);

 cleanup:
  return (-1);
}

static int
_setup_threshold_fields (ipmi_config_state_data_t *state_data,
                         struct ipmi_config_section *section,
                         const char *description,
                         uint8_t sensor_base_unit_type)
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

  assert (state_data);
  assert (section);
  assert (description);

  /* We will adjust this list as necessary later on.  Many
   * measurements could technically be negative (e.g. temperature)
   * even though its unrealistic for IPMI's sake.  Others, I'm just
   * not sure about.
   */
  if (sensor_base_unit_type == IPMI_SENSOR_UNIT_RPM)
    threshold_validate_ptr = threshold_validate_positive;
  else
    threshold_validate_ptr = threshold_validate;

  if (ipmi_sdr_parse_threshold_readable (state_data->sdr_ctx,
					 NULL,
					 0,
                                         &lower_non_critical_threshold_readable,
                                         &lower_critical_threshold_readable,
                                         &lower_non_recoverable_threshold_readable,
                                         &upper_non_critical_threshold_readable,
                                         &upper_critical_threshold_readable,
                                         &upper_non_recoverable_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_threshold_settable: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_threshold_settable (state_data->sdr_ctx,
					 NULL,
					 0,
                                         &lower_non_critical_threshold_settable,
                                         &lower_critical_threshold_settable,
                                         &lower_non_recoverable_threshold_settable,
                                         &upper_non_critical_threshold_settable,
                                         &upper_critical_threshold_settable,
                                         &upper_non_recoverable_threshold_settable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_threshold_settable: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

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
  return (rv);
}

static int
_setup_threshold_hysteresis_fields (ipmi_config_state_data_t *state_data,
                                    struct ipmi_config_section *section,
                                    const char *description,
                                    uint8_t sensor_base_unit_type,
                                    uint8_t hysteresis_support)
{
  unsigned int flags = 0;
  char description_hysteresis[IPMI_CONFIG_MAX_DESCRIPTION_LEN];
  Key_Validate hysteresis_threshold_validate_ptr = NULL;
  int rv = -1;

  assert (state_data);
  assert (section);
  assert (description);
  assert (hysteresis_support == IPMI_SDR_READABLE_HYSTERESIS_SUPPORT
          || hysteresis_support == IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT
          || state_data->prog_data->args->verbose_count);

  if (hysteresis_support == IPMI_SDR_READABLE_HYSTERESIS_SUPPORT)
    {
      flags |= IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT;
      flags |= IPMI_CONFIG_READABLE_ONLY;
    }
  else if (hysteresis_support == IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT)
    flags = 0;                  /* no change, can read/write */
  else /* state_data->prog_data->args->verbose_count */
    flags = IPMI_CONFIG_UNDEFINED;

  memset (description_hysteresis, '\0', IPMI_CONFIG_MAX_DESCRIPTION_LEN);
  snprintf (description_hysteresis,
            IPMI_CONFIG_MAX_DESCRIPTION_LEN,
            "%s; 'None' to not use hysteresis",
            description);

  /* We will adjust this list as necessary later on.  Many
   * measurements could technically be negative (e.g. temperature)
   * even though its unrealistic for IPMI's sake.  Others, I'm just
   * not sure about.
   */
  if (sensor_base_unit_type == IPMI_SENSOR_UNIT_RPM)
    hysteresis_threshold_validate_ptr = hysteresis_threshold_validate_positive;
  else
    hysteresis_threshold_validate_ptr = hysteresis_threshold_validate;

  if (ipmi_config_section_add_key (state_data,
				   section,
				   "Positive_Going_Threshold_Hysteresis",
				   description_hysteresis,
				   flags,
				   hysteresis_threshold_checkout,
				   hysteresis_threshold_commit,
				   hysteresis_threshold_validate_ptr) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
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
  return (rv);
}

ipmi_config_err_t
ipmi_config_sensors_threshold_section (ipmi_config_state_data_t *state_data,
                                       struct ipmi_config_section **section_ptr)
{
  struct ipmi_config_section *section = NULL;
  char section_name[IPMI_CONFIG_MAX_SECTION_NAME_LEN];
  uint8_t threshold_access_support = 0;
  uint8_t hysteresis_support = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t sensor_type;
  uint8_t sensor_units_percentage;
  uint8_t sensor_units_modifier;
  uint8_t sensor_units_rate;
  uint8_t sensor_base_unit_type;
  uint8_t sensor_modifier_unit_type;
  char description[IPMI_CONFIG_MAX_DESCRIPTION_LEN];
  char sensor_units_buf[IPMI_CONFIG_CATEGORY_SENSORS_UNITS_BUFLEN+1];
  int sensor_units_ret;
  const char *sensor_type_str = NULL;

  assert (state_data);
  assert (section_ptr);

  if ((ret = ipmi_config_sensors_create_section_name (state_data,
						      section_name,
						      IPMI_CONFIG_MAX_SECTION_NAME_LEN)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_config_sensors_create_section_name: %s\n",
                         strerror (errno));

      goto cleanup;
    }

  if (!(section = ipmi_config_section_create (state_data,
					      section_name,
					      NULL,
					      NULL,
					      0,
					      NULL,
					      NULL)))
    goto cleanup;

  if (ipmi_sdr_parse_sensor_capabilities (state_data->sdr_ctx,
					  NULL,
					  0,
                                          NULL,
                                          &threshold_access_support,
                                          &hysteresis_support,
                                          NULL,
                                          NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_capabilities: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				  NULL,
				  0,
                                  &sensor_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_units (state_data->sdr_ctx,
				   NULL,
				   0,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_unit: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  memset (sensor_units_buf, '\0', IPMI_CONFIG_CATEGORY_SENSORS_UNITS_BUFLEN);
  sensor_units_ret = ipmi_sensor_units_string (sensor_units_percentage,
                                               sensor_units_modifier,
                                               sensor_units_rate,
                                               sensor_base_unit_type,
                                               sensor_modifier_unit_type,
                                               sensor_units_buf,
                                               IPMI_CONFIG_CATEGORY_SENSORS_UNITS_BUFLEN,
                                               0);

  sensor_type_str = ipmi_get_sensor_type_string (sensor_type);

  memset (description, '\0', IPMI_CONFIG_MAX_DESCRIPTION_LEN);
  if (sensor_units_ret > 0)
    snprintf (description,
              IPMI_CONFIG_MAX_DESCRIPTION_LEN,
              "Give valid input for sensor type = %s; units = %s",
              sensor_type_str ? sensor_type_str : UNRECOGNIZED_SENSOR_TYPE,
              sensor_units_buf);
  else
    snprintf (description,
              IPMI_CONFIG_MAX_DESCRIPTION_LEN,
              "Give valid input for sensor type = %s",
              sensor_type_str ? sensor_type_str : UNRECOGNIZED_SENSOR_TYPE);
  
  if (setup_sensor_event_enable_fields (state_data, section) < 0)
    goto cleanup;

  if (threshold_access_support == IPMI_SDR_READABLE_THRESHOLDS_SUPPORT
      || threshold_access_support == IPMI_SDR_READABLE_SETTABLE_THRESHOLDS_SUPPORT
      || state_data->prog_data->args->verbose_count)
    {
      if (_setup_threshold_fields (state_data,
                                   section,
                                   description,
                                   sensor_base_unit_type) < 0)
        goto cleanup;
    }

  if (hysteresis_support == IPMI_SDR_READABLE_HYSTERESIS_SUPPORT
      || hysteresis_support == IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT
      || state_data->prog_data->args->verbose_count)
    {
      if (_setup_threshold_hysteresis_fields (state_data,
                                              section,
                                              description,
                                              sensor_base_unit_type,
                                              hysteresis_support) < 0)
        goto cleanup;
    }

  *section_ptr = section;
  return (IPMI_CONFIG_ERR_SUCCESS);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (rv);
}
