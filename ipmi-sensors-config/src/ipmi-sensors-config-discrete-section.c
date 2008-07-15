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
#endif /* HAVE_CONFIG_H */

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

static config_err_t
_get_hysteresis (ipmi_sensors_config_state_data_t *state_data,
                 uint8_t sensor_number,
                 uint8_t *positive_going_discrete_hysteresis_value,
                 uint8_t *negative_going_discrete_hysteresis_value)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint64_t val;

  assert(state_data);
  assert(positive_going_discrete_hysteresis_value);
  assert(negative_going_discrete_hysteresis_value);

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
  
  _FIID_OBJ_GET (obj_cmd_rs, "positive_going_discrete_hysteresis_value", &val);
  *positive_going_discrete_hysteresis_value = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "negative_going_discrete_hysteresis_value", &val);
  *negative_going_discrete_hysteresis_value = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
hysteresis_discrete_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t positive_going_discrete_hysteresis_value;
  uint8_t negative_going_discrete_hysteresis_value;
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
                              &positive_going_discrete_hysteresis_value,
                              &negative_going_discrete_hysteresis_value)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!strcasecmp(kv->key->key_name, "Positive_Going_Discrete_Hysteresis"))
    value_raw = positive_going_discrete_hysteresis_value;
  else if (!strcasecmp(kv->key->key_name, "Negative_Going_Discrete_Hysteresis"))
    value_raw = negative_going_discrete_hysteresis_value;
  else
    /* unknown key_name - fatal error */
    goto cleanup;
   
  if (config_section_update_keyvalue_output_int(state_data->pstate,
                                                kv, 
                                                value_raw) < 0)
    goto cleanup;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static config_err_t
hysteresis_discrete_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t positive_going_discrete_hysteresis_value;
  uint8_t negative_going_discrete_hysteresis_value;
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
                              &positive_going_discrete_hysteresis_value,
                              &negative_going_discrete_hysteresis_value)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  value_raw = atoi(kv->value_input);
  
  if (!strcasecmp(kv->key->key_name, "Positive_Going_Discrete_Hysteresis"))
    positive_going_discrete_hysteresis_value = value_raw;
  else if (!strcasecmp(kv->key->key_name, "Negative_Going_Discrete_Hysteresis" ))
    negative_going_discrete_hysteresis_value = value_raw;
  else
    /* unknown key_name - fatal error */
    goto cleanup;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_sensor_hysteresis_rs);
  
  if (ipmi_cmd_set_sensor_hysteresis (state_data->ipmi_ctx,
                                      sensor_number,
                                      IPMI_SENSOR_HYSTERESIS_MASK,
                                      positive_going_discrete_hysteresis_value,
                                      negative_going_discrete_hysteresis_value,
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

static int
_setup_discrete_hysteresis (ipmi_sensors_config_state_data_t *state_data,
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            struct config_section *section,
                            uint8_t hysteresis_support)
{
  unsigned int flags = 0;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);
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
    
  if (config_section_add_key (state_data->pstate,
                              section,
                              "Positive_Going_Discrete_Hysteresis",
                              "Possible values: 1-255, 0 to not use hysteresis",
                              flags,
                              hysteresis_discrete_checkout,
                              hysteresis_discrete_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Negative_Going_Discrete_Hysteresis",
                              "Possible values: 1-255, 0 to not use hysteresis",
                              flags,
                              hysteresis_discrete_checkout,
                              hysteresis_discrete_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return rv;
}

config_err_t
ipmi_sensors_config_discrete_section (ipmi_sensors_config_state_data_t *state_data,
                                      uint8_t *sdr_record,
                                      unsigned int sdr_record_len,
                                      struct config_section **section_ptr)
{
  struct config_section *section = NULL;
  char section_name[CONFIG_MAX_SECTION_NAME_LEN];
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint16_t record_id;
  uint8_t hysteresis_support = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

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

  if (sdr_cache_get_sensor_capabilities (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         NULL,
                                         &hysteresis_support,
                                         NULL,
                                         NULL) < 0)
    goto cleanup;

  if (hysteresis_support == IPMI_SDR_READABLE_HYSTERESIS_SUPPORT
      || hysteresis_support == IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT
      || state_data->prog_data->args->config_args.verbose)
    {
      if (_setup_discrete_hysteresis (state_data,
                                      sdr_record,
                                      sdr_record_len,
                                      section,
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
