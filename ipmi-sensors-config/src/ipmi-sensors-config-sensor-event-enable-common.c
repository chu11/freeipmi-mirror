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

/* convenience structs */

struct sensor_event_bits {
  uint8_t bit0;
  uint8_t bit1;
  uint8_t bit2;
  uint8_t bit3;
  uint8_t bit4;
  uint8_t bit5;
  uint8_t bit6;
  uint8_t bit7;
  uint8_t bit8;
  uint8_t bit9;
  uint8_t bit10;
  uint8_t bit11;
  uint8_t bit12;
  uint8_t bit13;
  uint8_t bit14;
};

struct sensor_event_enable_data {
  uint8_t all_event_messages;
  uint8_t scanning_on_this_sensor;
  struct sensor_event_bits assertion;
  struct sensor_event_bits deassertion;
};

#define KEY_NAME_MAX_LEN 1024

typedef int (*Sdr_event_flags_func)(pstdout_state_t pstate,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint8_t *event_state_0,
                                    uint8_t *event_state_1,
                                    uint8_t *event_state_2,
                                    uint8_t *event_state_3,
                                    uint8_t *event_state_4,
                                    uint8_t *event_state_5,
                                    uint8_t *event_state_6,
                                    uint8_t *event_state_7,
                                    uint8_t *event_state_8,
                                    uint8_t *event_state_9,
                                    uint8_t *event_state_10,
                                    uint8_t *event_state_11,
                                    uint8_t *event_state_12,
                                    uint8_t *event_state_13,
                                    uint8_t *event_state_14);

typedef int (*Sdr_threshold_event_flags_func)(pstdout_state_t pstate,
                                              uint8_t *sdr_record,
                                              unsigned int sdr_record_len,
                                              uint8_t *lower_non_critical_going_low,
                                              uint8_t *lower_non_critical_going_high,
                                              uint8_t *lower_critical_going_low,
                                              uint8_t *lower_critical_going_high,
                                              uint8_t *lower_non_recoverable_going_low,
                                              uint8_t *lower_non_recoverable_going_high,
                                              uint8_t *upper_non_critical_going_low,
                                              uint8_t *upper_non_critical_going_high,
                                              uint8_t *upper_critical_going_low,
                                              uint8_t *upper_critical_going_high,
                                              uint8_t *upper_non_recoverable_going_low,
                                              uint8_t *upper_non_recoverable_going_high);

static void
_clear_event_bits (struct sensor_event_bits *bits)
{
  assert(bits);

  bits->bit0 = 0;
  bits->bit1 = 0;
  bits->bit2 = 0;
  bits->bit3 = 0;
  bits->bit4 = 0;
  bits->bit5 = 0;
  bits->bit6 = 0;
  bits->bit7 = 0;
  bits->bit8 = 0;
  bits->bit9 = 0;
  bits->bit10 = 0;
  bits->bit11 = 0;
  bits->bit12 = 0;
  bits->bit13 = 0;
  bits->bit14 = 0;
}

static void
_clear_assertion_bits (struct sensor_event_enable_data *data)
{
  assert(data);

  _clear_event_bits (&(data->assertion));
}

static void
_clear_deassertion_bits (struct sensor_event_enable_data *data)
{
  assert(data);

  _clear_event_bits (&(data->deassertion));
}

static void
_copy_bitmask_into_bits (struct sensor_event_bits *bits,
                         uint16_t bitmask)
{
  assert(bits);

  bits->bit0 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit1 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit2 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit3 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit4 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit5 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit6 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit7 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit8 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit9 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit10 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit11 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit12 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit13 = bitmask & 0x1;
  bitmask >>= 1;

  bits->bit14 = bitmask & 0x1;
  bitmask >>= 1;
}                         

static void
_copy_assertion_bitmask_into_bits (struct sensor_event_enable_data *data, 
                                   uint16_t bitmask)
{
  assert(data);

  _copy_bitmask_into_bits (&(data->assertion), bitmask);
}

static void
_copy_deassertion_bitmask_into_bits (struct sensor_event_enable_data *data,
                                     uint16_t bitmask)
{
  assert(data);

  _copy_bitmask_into_bits (&(data->deassertion), bitmask);
}

static config_err_t
_get_sensor_event_enable (ipmi_sensors_config_state_data_t *state_data,
                          const char *section_name,
                          struct sensor_event_enable_data *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t sensor_number;
  int8_t field_len;
  uint64_t val;

  assert(state_data);
  assert(section_name);
  assert(data);

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

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sensor_event_enable_rs);

  if (ipmi_cmd_get_sensor_event_enable (state_data->ipmi_ctx,
                                        sensor_number,
                                        obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_sensor_event_enable: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "all_event_messages", &val);
  data->all_event_messages = val;

  _FIID_OBJ_GET (obj_cmd_rs, "scanning_on_this_sensor", &val);
  data->scanning_on_this_sensor = val;

  if (data->all_event_messages == IPMI_SENSOR_ALL_EVENT_MESSAGES_DISABLE)
    {
      _clear_assertion_bits(data);
      _clear_deassertion_bits(data);
      goto out;
    }

  _FIID_OBJ_GET_WITH_RETURN_VALUE (obj_cmd_rs,
                                   "assertion_event_bitmask",
                                   &val,
                                   field_len);
  if (field_len)
    _copy_assertion_bitmask_into_bits (data, val);
  else
    _clear_assertion_bits(data);

  _FIID_OBJ_GET_WITH_RETURN_VALUE (obj_cmd_rs,
                                   "deassertion_event_bitmask",
                                   &val,
                                   field_len);
  if (field_len)
    _copy_deassertion_bitmask_into_bits (data, val);
  else
    _clear_deassertion_bits(data);

 out:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_sensor_event_enable (ipmi_sensors_config_state_data_t *state_data,
                          const char *section_name,
                          struct sensor_event_enable_data *data,
                          uint8_t event_message_action)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t sensor_number;

  assert(state_data);
  assert(section_name);
  assert(data);

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

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_sensor_event_enable_rs);

  if (ipmi_cmd_set_sensor_event_enable (state_data->ipmi_ctx,
                                        sensor_number,
                                        event_message_action,
                                        data->scanning_on_this_sensor,
                                        data->all_event_messages,
                                        0,
                                        0,
                                        obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_sensor_event_enable: %s\n",
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

config_err_t
sensor_event_enable_enable_all_event_messages_checkout (const char *section_name,
                                                        struct config_keyvalue *kv,
                                                        void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  struct sensor_event_enable_data data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  memset(&data, '\0', sizeof(struct sensor_event_enable_data));
  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
    
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            data.all_event_messages ? "Yes" : "No") < 0)
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
sensor_event_enable_enable_all_event_messages_commit (const char *section_name,
                                                      const struct config_keyvalue *kv,
                                                      void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  struct sensor_event_enable_data data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  memset(&data, '\0', sizeof(struct sensor_event_enable_data));
  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  data.all_event_messages = same (kv->value_input, "yes");
  
  if ((ret = _set_sensor_event_enable (state_data,
                                       section_name,
                                       &data,
                                       IPMI_SENSOR_EVENT_MESSAGE_ACTION_DO_NOT_CHANGE_INDIVIDUAL_ENABLES)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
sensor_event_enable_enable_scanning_on_this_sensor_checkout (const char *section_name,
                                                             struct config_keyvalue *kv,
                                                             void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  struct sensor_event_enable_data data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  memset(&data, '\0', sizeof(struct sensor_event_enable_data));
  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
    
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            data.scanning_on_this_sensor ? "Yes" : "No") < 0)
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
sensor_event_enable_enable_scanning_on_this_sensor_commit (const char *section_name,
                                                           const struct config_keyvalue *kv,
                                                           void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  struct sensor_event_enable_data data;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  memset(&data, '\0', sizeof(struct sensor_event_enable_data));
  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  data.scanning_on_this_sensor = same (kv->value_input, "yes");
  
  if ((ret = _set_sensor_event_enable (state_data,
                                       section_name,
                                       &data,
                                       IPMI_SENSOR_EVENT_MESSAGE_ACTION_DO_NOT_CHANGE_INDIVIDUAL_ENABLES)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
threshold_event_enable_checkout (const char *section_name,
                                 struct config_keyvalue *kv,
                                 void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  struct sensor_event_enable_data data;
  struct sensor_event_bits *bits;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t event_reading_type_code;
  int sensor_class;
  uint8_t val;

  memset(&data, '\0', sizeof(struct sensor_event_enable_data));
  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = get_sdr_record(state_data,
                            section_name,
                            sdr_record,
                            &sdr_record_len)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (sdr_cache_get_event_reading_type_code (NULL,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;
      
  sensor_class = sensor_classify (event_reading_type_code);

  if (sensor_class != SENSOR_CLASS_THRESHOLD)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Attempting to checkout threshold event in non-threshold sensor\n");
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
    
  if (stristr(kv->key->key_name, "Deassertion"))
    bits = &(data.deassertion);
  else
    bits = &(data.assertion);

  if (stristr(kv->key->key_name, "Lower_Non_Critical_Going_Low"))
    val = bits->bit0;
  else if (stristr(kv->key->key_name, "Lower_Non_Critical_Going_High"))
    val = bits->bit1;
  else if (stristr(kv->key->key_name, "Lower_Critical_Going_Low")) 
    val = bits->bit2;
  else if (stristr(kv->key->key_name, "Lower_Critical_Going_High"))
    val = bits->bit3;
  else if (stristr(kv->key->key_name, "Lower_Non_Recoverable_Going_Low")) 
    val = bits->bit4;
  else if (stristr(kv->key->key_name, "Lower_Non_Recoverable_Going_High"))
    val = bits->bit5;
  else if (stristr(kv->key->key_name, "Upper_Non_Critical_Going_Low")) 
    val = bits->bit6;
  else if (stristr(kv->key->key_name, "Upper_Non_Critical_Going_High"))
    val = bits->bit7;
  else if (stristr(kv->key->key_name, "Upper_Critical_Going_Low")) 
    val = bits->bit8;
  else if (stristr(kv->key->key_name, "Upper_Critical_Going_High"))
    val = bits->bit9;
  else if (stristr(kv->key->key_name, "Upper_Non_Recoverable_Going_Low")) 
    val = bits->bit10;
  else if (stristr(kv->key->key_name, "Upper_Non_Recoverable_Going_High"))
    val = bits->bit11;
  else
    {
      if (state_data->prog_data->args->config_args.verbose)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
   
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            val ? "Yes" : "No") < 0)
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
threshold_event_enable_commit (const char *section_name,
                               const struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  /* XXX */
  return CONFIG_ERR_NON_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

int
_setup_threshold_event_enable_key (ipmi_sensors_config_state_data_t *state_data,
                                   struct config_section *section,
                                   const char *key_name,
                                   uint8_t event_supported)
{
  unsigned int flags = 0;

  assert(state_data);
  assert(section);
  assert(key_name);

  if (event_supported
      || state_data->prog_data->args->config_args.verbose)
    {
      if (!event_supported)
        flags |= CONFIG_UNDEFINED;

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  key_name,
                                  "Possible values: Yes/No",
                                  flags,
                                  threshold_event_enable_checkout,
                                  threshold_event_enable_commit,
                                  config_yes_no_validate) < 0)
        goto cleanup;
    }

  return 0;

 cleanup:
  return -1;
}

static int
_setup_threshold_event_enable_wrapper (ipmi_sensors_config_state_data_t *state_data,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       struct config_section *section,
                                       const char *type_str,
                                       Sdr_threshold_event_flags_func sdr_call)
{
  uint8_t lower_non_critical_going_low = 0;
  uint8_t lower_non_critical_going_high = 0;
  uint8_t lower_critical_going_low = 0;
  uint8_t lower_critical_going_high = 0;
  uint8_t lower_non_recoverable_going_low = 0;
  uint8_t lower_non_recoverable_going_high = 0;
  uint8_t upper_non_critical_going_low = 0;
  uint8_t upper_non_critical_going_high = 0;
  uint8_t upper_critical_going_low = 0;
  uint8_t upper_critical_going_high = 0;
  uint8_t upper_non_recoverable_going_low = 0; 
  uint8_t upper_non_recoverable_going_high = 0;
  char key_name[KEY_NAME_MAX_LEN];
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);
  assert(type_str);
  assert(sdr_call);

  if (((*sdr_call)(state_data->pstate,
                   sdr_record,
                   sdr_record_len,
                   &lower_non_critical_going_low,
                   &lower_non_critical_going_high,
                   &lower_critical_going_low,
                   &lower_critical_going_high,
                   &lower_non_recoverable_going_low,
                   &lower_non_recoverable_going_high,
                   &upper_non_critical_going_low,
                   &upper_non_critical_going_high,
                   &upper_critical_going_low,
                   &upper_critical_going_high,
                   &upper_non_recoverable_going_low,
                   &upper_non_recoverable_going_high)) < 0)
    goto cleanup;
                  
  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Lower_Non_Critical_Going_Low", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         lower_non_critical_going_low) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN,
           "Enable_%s_Event_Lower_Non_Critical_Going_High", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         lower_non_critical_going_high) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Lower_Critical_Going_Low", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         lower_critical_going_low) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Lower_Critical_Going_High", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         lower_critical_going_high) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Lower_Non_Recoverable_Going_Low", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         lower_non_recoverable_going_low) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Lower_Non_Recoverable_Going_High", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         lower_non_recoverable_going_high) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Upper_Non_Critical_Going_Low", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         upper_non_critical_going_low) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Upper_Non_Critical_Going_High", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         upper_non_critical_going_high) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Upper_Critical_Going_Low", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         upper_critical_going_low) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Upper_Critical_Going_High", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         upper_critical_going_high) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Upper_Non_Recoverable_Going_Low", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         upper_non_recoverable_going_low) < 0)
    goto cleanup;

  snprintf(key_name, 
           KEY_NAME_MAX_LEN, 
           "Enable_%s_Event_Upper_Non_Recoverable_Going_High", 
           type_str);
  if (_setup_threshold_event_enable_key (state_data,
                                         section,
                                         key_name,
                                         upper_non_recoverable_going_high) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return rv;
}

int
_setup_threshold_event_enable (ipmi_sensors_config_state_data_t *state_data,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               struct config_section *section)
{
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);

  if (_setup_threshold_event_enable_wrapper (state_data,
                                             sdr_record,
                                             sdr_record_len,
                                             section,
                                             "Assertion",
                                             &sdr_cache_get_threshold_assertion_supported) < 0)
    goto cleanup;

  if (_setup_threshold_event_enable_wrapper (state_data,
                                             sdr_record,
                                             sdr_record_len,
                                             section,
                                             "Deassertion",
                                             &sdr_cache_get_threshold_deassertion_supported) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return rv;
}

config_err_t
generic_event_enable_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  struct sensor_event_enable_data data;
  struct sensor_event_bits *bits;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t event_reading_type_code;
  int sensor_class;
  uint8_t val;

  memset(&data, '\0', sizeof(struct sensor_event_enable_data));
  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
    
  if ((ret = get_sdr_record(state_data,
                            section_name,
                            sdr_record,
                            &sdr_record_len)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (sdr_cache_get_event_reading_type_code(NULL,
                                            sdr_record,
                                            sdr_record_len,
                                            &event_reading_type_code) < 0)
    goto cleanup;

  sensor_class = sensor_classify (event_reading_type_code);

  if (sensor_class != SENSOR_CLASS_GENERIC_DISCRETE)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Attempting to checkout generic event in non-generic sensor\n");
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (stristr(kv->key->key_name, "Deassertion"))
    bits = &(data.deassertion);
  else
    bits = &(data.assertion);

  switch (event_reading_type_code)
    {
      /* achu: I'm sorry.  But these fields have no description in the
       * IPMI spec, so there is no macro for them.  So I'm hard coding
       * hex in.  Please see see Table 42-2 in the IPMI spec.
       */
      /* 
       * 0x02
       */
    case 0x02:
      if (stristr(kv->key->key_name, "Transition_to_Idle")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Transition_to_Active")) 
        val = bits->bit1;
      else if (stristr(kv->key->key_name, "Transition_to_Busy")) 
        val = bits->bit2;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x03
       */
    case 0x03:
      if (stristr(kv->key->key_name, "State_Deasserted")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "State_Asserted")) 
        val = bits->bit1;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x04
       */
    case 0x04:
      if (stristr(kv->key->key_name, "Predictive_Failure_Deasserted")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Predictive_Failure_Asserted")) 
        val = bits->bit1;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x05
       */
    case 0x05:
      if (stristr(kv->key->key_name, "Limit_Not_Exceeded")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Limit_Exceeded")) 
        val = bits->bit1;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x06
       */
    case 0x06:
      if (stristr(kv->key->key_name, "Performance_Met")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Performance_Lags")) 
        val = bits->bit1;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x07
       */
    case 0x07:
      if (stristr(kv->key->key_name, "Transition_to_OK")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Transition_to_Non_Critical_from_OK")) 
        val = bits->bit1;
      else if (stristr(kv->key->key_name, "Transition_to_Critical_from_Less_Severe")) 
        val = bits->bit2;
      else if (stristr(kv->key->key_name, "Transition_to_Non_Recoverable_from_Less_Severe")) 
        val = bits->bit3;
      else if (stristr(kv->key->key_name, "Transition_to_Non_Critical_from_More_Severe")) 
        val = bits->bit4;
      else if (stristr(kv->key->key_name, "Transition_to_Critical_from_Non_Recoverable")) 
        val = bits->bit5;
      else if (stristr(kv->key->key_name, "Transition_to_Non_Recoverable")) 
        val = bits->bit6;
      else if (stristr(kv->key->key_name, "Monitor")) 
        val = bits->bit7;
      else if (stristr(kv->key->key_name, "Informational")) 
        val = bits->bit8;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x08
       */
    case 0x08:
      if (stristr(kv->key->key_name, "Device_Removed_or_Device_Absent")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Device_Inserted_or_Device_Present")) 
        val = bits->bit1;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x09
       */
    case 0x09:
      if (stristr(kv->key->key_name, "Device_Disabled")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Device_Enabled")) 
        val = bits->bit1;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x0A
       */
    case 0x0A:
      if (stristr(kv->key->key_name, "Transition_to_Running")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Transition_to_In_Test")) 
        val = bits->bit1;
      else if (stristr(kv->key->key_name, "Transition_to_Power_Off")) 
        val = bits->bit2;
      else if (stristr(kv->key->key_name, "Transition_to_On_Line")) 
        val = bits->bit3;
      else if (stristr(kv->key->key_name, "Transition_to_Off_Line")) 
        val = bits->bit4;
      else if (stristr(kv->key->key_name, "Transition_to_Off_Duty")) 
        val = bits->bit5;
      else if (stristr(kv->key->key_name, "Transition_to_Degraded")) 
        val = bits->bit6;
      else if (stristr(kv->key->key_name, "Transition_to_Power_Save")) 
        val = bits->bit7;
      else if (stristr(kv->key->key_name, "Install_Error")) 
        val = bits->bit8;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x0B
       */
    case 0x0B:
      if (stristr(kv->key->key_name, "Fully_Redundant")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "Redundancy_Lost")) 
        val = bits->bit1;
      else if (stristr(kv->key->key_name, "Redundancy_Degraded")) 
        val = bits->bit2;
      else if (stristr(kv->key->key_name, "Entered_from_Redundancy_Degraded_or_Fully_Redundant")) 
        val = bits->bit3;
      else if (stristr(kv->key->key_name, "Entered_from_Non_Redundant_Insufficient_Resources")) 
        val = bits->bit4;
      else if (stristr(kv->key->key_name, "Non_Redundant_Insufficient_Resources")) 
        val = bits->bit5;
      else if (stristr(kv->key->key_name, "Redundancy_Degraded_from_Fully_Redundant")) 
        val = bits->bit6;
      else if (stristr(kv->key->key_name, "Redundancy_Degraded_from_Non_Redundant")) 
        val = bits->bit7;
      else
        goto unrecognized_key_name;
      break;
      /* 
       * 0x0C
       */
    case 0x0C:
      if (stristr(kv->key->key_name, "D0_Power_State")) 
        val = bits->bit0;
      else if (stristr(kv->key->key_name, "D1_Power_State")) 
        val = bits->bit1;
      else if (stristr(kv->key->key_name, "D2_Power_State")) 
        val = bits->bit2;
      else if (stristr(kv->key->key_name, "D3_Power_State")) 
        val = bits->bit3;
      else
        goto unrecognized_key_name;
      break;
    default:
    unrecognized_key_name:
      if (state_data->prog_data->args->config_args.verbose)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            val ? "Yes" : "No") < 0)
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
generic_event_enable_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  /* XXX */
  return CONFIG_ERR_NON_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

int
_setup_generic_event_enable_key (ipmi_sensors_config_state_data_t *state_data,
                                 struct config_section *section,
                                 const char *key_name,
                                 uint8_t event_supported)
{
  unsigned int flags = 0;

  assert(state_data);
  assert(section);
  assert(key_name);

  if (event_supported
      || state_data->prog_data->args->config_args.verbose)
    {
      if (!event_supported)
        flags |= CONFIG_UNDEFINED;

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  key_name,
                                  "Possible values: Yes/No",
                                  flags,
                                  generic_event_enable_checkout,
                                  generic_event_enable_commit,
                                  config_yes_no_validate) < 0)
        goto cleanup;
    }

  return 0;

 cleanup:
  return -1;
}

static int
_setup_generic_event_enable_wrapper (ipmi_sensors_config_state_data_t *state_data,
                                     uint8_t *sdr_record,
                                     unsigned int sdr_record_len,
                                     struct config_section *section,
                                     const char *type_str,
                                     Sdr_event_flags_func sdr_call,
                                     uint8_t event_reading_type_code)
{
  uint8_t event_state_0 = 0;
  uint8_t event_state_1 = 0;
  uint8_t event_state_2 = 0;
  uint8_t event_state_3 = 0;
  uint8_t event_state_4 = 0;
  uint8_t event_state_5 = 0;
  uint8_t event_state_6 = 0;
  uint8_t event_state_7 = 0;
  uint8_t event_state_8 = 0;
  uint8_t event_state_9 = 0;
  uint8_t event_state_10 = 0;
  uint8_t event_state_11 = 0;
  uint8_t event_state_12 = 0;
  uint8_t event_state_13 = 0;
  uint8_t event_state_14 = 0;
  char key_name[KEY_NAME_MAX_LEN];
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);
  assert(type_str);
  assert(sdr_call);

  if (((*sdr_call)(state_data->pstate,
                   sdr_record,
                   sdr_record_len,
                   &event_state_0,
                   &event_state_1,
                   &event_state_2,
                   &event_state_3,
                   &event_state_4,
                   &event_state_5,
                   &event_state_6,
                   &event_state_7,
                   &event_state_8,
                   &event_state_9,
                   &event_state_10,
                   &event_state_11,
                   &event_state_12,
                   &event_state_13,
                   &event_state_14)) < 0)
    goto cleanup;
  
  /* achu: making a generic "convert event message string into
   * key_name" for this tool was difficult.  There are too many
   * conversions and exceptions to the rules (abbreviations, slashes,
   * spaces, hypens, spaces w/ hyphens, single quotes, double quotes,
   * examples, parentheses, strings that are too long, wierd
   * capitalization, need lower cases, etc.)  that strings never
   * turned out right.  I decided to just hard code names in at the
   * end of the day.
   */

  switch (event_reading_type_code)
    {
      /* achu: I'm sorry.  But these fields have no description in the
       * IPMI spec, so there is no macro for them.  So I'm hard coding
       * hex in.  Please see see Table 42-2 in the IPMI spec.
       */
      /* 
       * 0x02
       */
    case 0x02:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Idle", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Active", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Busy", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_2) < 0)
        goto cleanup;
      break;
      /* 
       * 0x03
       */
    case 0x03:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_State_Deasserted", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_State_Asserted", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;
      break;
      /* 
       * 0x04
       */
    case 0x04:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Predictive_Failure_Deasserted", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Predictive_Failure_Asserted", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;
      break;
      /* 
       * 0x05
       */
    case 0x05:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Limit_Not_Exceeded", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Limit_Exceeded", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;
      break;
      /* 
       * 0x06
       */
    case 0x06:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Performance_Met", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Performance_Lags", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;
      break;
      /* 
       * 0x07
       */
    case 0x07:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_OK", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Non_Critical_from_OK", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Critical_from_Less_Severe", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Non_Recoverable_from_Less_Severe", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Non_Critical_from_More_Severe", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Critical_from_Non_Recoverable", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Non_Recoverable", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_6) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Monitor", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_7) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Informational", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_8) < 0)
        goto cleanup;

      break;
      /* 
       * 0x08
       */
    case 0x08:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Device_Removed_or_Device_Absent", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Device_Inserted_or_Device_Present", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;
      break;
      /* 
       * 0x09
       */
    case 0x09:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Device_Disabled", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Device_Enabled", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;
      break;
      /* 
       * 0x0A
       */
    case 0x0A:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Running", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_In_Test", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Power_Off", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_On_Line", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Off_Line", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Off_Duty", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Degraded", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_6) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Transition_to_Power_Save", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_7) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Install_Error", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_8) < 0)
        goto cleanup;
      break;
      /* 
       * 0x0B
       */
    case 0x0B:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Fully_Redundant", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Redundancy_Lost", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Redundancy_Degraded", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Entered_from_Redundancy_Degraded_or_Fully_Redundant", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Entered_from_Non_Redundant_Insufficient_Resources", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Non_Redundant_Insufficient_Resources", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Redundancy_Degraded_from_Fully_Redundant", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_6) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Redundancy_Degraded_from_Non_Redundant", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_7) < 0)
        goto cleanup;
      break;
      /* 
       * 0x0C
       */
    case 0x0C:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_D0_Power_State", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_D1_Power_State", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_D2_Power_State", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_D3_Power_State", 
               type_str);
      if (_setup_generic_event_enable_key (state_data,
                                           section,
                                           key_name,
                                           event_state_3) < 0)
        goto cleanup;
      break;
    default:
      if (state_data->prog_data->args->config_args.verbose)
        pstdout_printf (state_data->pstate,
                        "## Unable to handle event flags for event reading type code 0x%X\n",
                        event_reading_type_code);
    }

  rv = 0;
 cleanup:
  return rv;
}

int
_setup_generic_event_enable (ipmi_sensors_config_state_data_t *state_data,
                             uint8_t *sdr_record,
                             unsigned int sdr_record_len,
                             struct config_section *section,
                             uint8_t event_reading_type_code)
{
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);

  if (_setup_generic_event_enable_wrapper (state_data,
                                           sdr_record,
                                           sdr_record_len,
                                           section,
                                           "Assertion",
                                           &sdr_cache_get_assertion_supported,
                                           event_reading_type_code) < 0)
    goto cleanup;

  if (_setup_generic_event_enable_wrapper (state_data,
                                           sdr_record,
                                           sdr_record_len,
                                           section,
                                           "Deassertion",
                                           &sdr_cache_get_deassertion_supported,
                                           event_reading_type_code) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return rv;
}

config_err_t
specific_event_enable_checkout (const char *section_name,
                                struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  /* XXX */
  return CONFIG_ERR_NON_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

config_err_t
specific_event_enable_commit (const char *section_name,
                              const struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_sensors_config_state_data_t *state_data = (ipmi_sensors_config_state_data_t *)arg;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  
  /* XXX */
  return CONFIG_ERR_NON_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

int
_setup_specific_event_enable_key (ipmi_sensors_config_state_data_t *state_data,
                                  struct config_section *section,
                                  const char *key_name,
                                  uint8_t event_supported)
{
  unsigned int flags = 0;

  assert(state_data);
  assert(section);
  assert(key_name);

  if (event_supported
      || state_data->prog_data->args->config_args.verbose)
    {
      if (!event_supported)
        flags |= CONFIG_UNDEFINED;

      if (config_section_add_key (state_data->pstate,
                                  section,
                                  key_name,
                                  "Possible values: Yes/No",
                                  flags,
                                  specific_event_enable_checkout,
                                  specific_event_enable_commit,
                                  config_yes_no_validate) < 0)
        goto cleanup;
    }

  return 0;

 cleanup:
  return -1;
}

static int
_setup_specific_event_enable_wrapper (ipmi_sensors_config_state_data_t *state_data,
                                      uint8_t *sdr_record,
                                      unsigned int sdr_record_len,
                                      struct config_section *section,
                                      const char *type_str,
                                      Sdr_event_flags_func sdr_call)
{
  uint8_t event_state_0 = 0;
  uint8_t event_state_1 = 0;
  uint8_t event_state_2 = 0;
  uint8_t event_state_3 = 0;
  uint8_t event_state_4 = 0;
  uint8_t event_state_5 = 0;
  uint8_t event_state_6 = 0;
  uint8_t event_state_7 = 0;
  uint8_t event_state_8 = 0;
  uint8_t event_state_9 = 0;
  uint8_t event_state_10 = 0;
  uint8_t event_state_11 = 0;
  uint8_t event_state_12 = 0;
  uint8_t event_state_13 = 0;
  uint8_t event_state_14 = 0;
  char key_name[KEY_NAME_MAX_LEN];
  uint8_t sensor_type;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);
  assert(type_str);
  assert(sdr_call);

  if (((*sdr_call)(state_data->pstate,
                   sdr_record,
                   sdr_record_len,
                   &event_state_0,
                   &event_state_1,
                   &event_state_2,
                   &event_state_3,
                   &event_state_4,
                   &event_state_5,
                   &event_state_6,
                   &event_state_7,
                   &event_state_8,
                   &event_state_9,
                   &event_state_10,
                   &event_state_11,
                   &event_state_12,
                   &event_state_13,
                   &event_state_14)) < 0)
    goto cleanup;

  if (sdr_cache_get_sensor_type (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_type) < 0)
    goto cleanup;
  
  /* achu: making a generic "convert event message string into
   * key_name" for this tool was difficult.  There are too many
   * conversions and exceptions to the rules (abbreviations, slashes,
   * spaces, hypens, spaces w/ hyphens, single quotes, double quotes,
   * examples, parentheses, strings that are too long, wierd
   * capitalization, need lower cases, etc.)  that strings never
   * turned out right.  I decided to just hard code names in at the
   * end of the day.
   */

  switch (sensor_type)
    {
      /* 
       * PHYSICAL SECURITY CHASSIS INTRUSION
       */
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY_CHASSIS_INTRUSION:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_General_Chassis_Intrusion", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Drive_Bay_Intrusion", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_IO_Card_Area_Intrusion", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Processor_Area_Intrusion", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_LAN_Leash_Lost", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Unauthorized_Dock", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_FAN_Area_Intrusion", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      break;
      /* 
       * PLATFORM SECURITY VIOLATION ATTEMPT
       */
    case IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Secure_Mode_Violation_Attempt", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Pre_Boot_Password_Violation_User_Password", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Pre_Boot_Password_Violation_Attempt_Setup_Password", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Pre_Boot_Password_Violation_Network_Boot_Password", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Other_Pre_Boot_Password_Violation", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Out_of_Band_Access_Password_Violation", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      break;
      /* 
       * PROCESSOR
       */
    case IPMI_SENSOR_TYPE_PROCESSOR:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_IERR", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Thermal_Trip", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_FRB1_BIST_failure", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_FRB2_Hang_in_POST_Failure", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_FRB3_Processor_Startup_Initialization_Failure", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Configuration_Error", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_SM_BIOS_Uncorrectable_CPU_Complex_Error", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Processor_Presence_detected", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_7) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Processor_Disabled", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_8) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Terminator_Presence_Detected", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_9) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Processor_Automatically_Throttled", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_10) < 0)
        goto cleanup;

      break;
      /* 
       * POWER SUPPLY
       */
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Presence_Detected", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Power_Supply_Failure_Detected", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Predictive_Failure", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Power_Supply_Input_Lost_AC_DC", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Power_Supply_Input_Lost_or_Out_of_Range", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Power_Supply_Input_Out_of_Range_but_Present", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Configuration_Error", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;
      break;
      /* 
       * POWER UNIT
       */
    case IPMI_SENSOR_TYPE_POWER_UNIT:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Power_Off_or_Power_Down", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Power_Cycle", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_240VA_Power_Down", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Interlock_Power_Down", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_AC_Lost", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Soft_Power_Control_Failure", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Power_Unit_Failure_Detected", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Predictive_Failure", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_7) < 0)
        goto cleanup;

      break;
      /* 
       * MEMORY
       */
    case IPMI_SENSOR_TYPE_MEMORY:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Correctable_ECC", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Uncorrectable_ECC", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Parity", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Memory_Scrub_Failed", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Memory_Device_Disabled", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Correctable_ECC_Logging_Limit_Reached", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Presence_Detected", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Configuration_Error", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_7) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Spare", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_8) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Memory_Automatically_Throttled", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_9) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Critical_Overtemperature", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_10) < 0)
        goto cleanup;
      break;
      /* 
       * DRIVE SLOT
       */
    case IPMI_SENSOR_TYPE_DRIVE_SLOT:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Drive_Presence", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Drive_Fault", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Predictive_Failure", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Hot_Spare", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Consistency_Check_In_Progress", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_In_Critical_Array", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_In_Failed_Array", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Rebuild_or_Remap_In_Progress", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_7) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_Rebuild_or_Remap_Aborted", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_8) < 0)
        goto cleanup;

      break;
      /* 
       * SYSTEM FIRMWARE PROGRESS
       */
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_System_Firmware_Error", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_System_Firmware_Hang", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name, 
               KEY_NAME_MAX_LEN, 
               "Enable_%s_Event_System_Firmware_Progress", 
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      break;
      /* 
       * EVENT LOGGING DISABLED
       */
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Correctable_Memory_Error_Logging_Disabled",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Event_Type_Logging_Disabled",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Log_Area_Reset_or_Cleared",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_All_Event_Logging_Disabled",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_SEL_Full",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_SEL_Almost_Full",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      break;
      /* 
       * SYSTEM EVENT
       */
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_System_Reconfigured",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_OEM_System_Boot_Event",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Undetermined_System_Hardware_Failure",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Entry_Added_to_Auxiliary_Log",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_PEF_Action",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Timestamp_Clock_Synch",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      break;
      /* 
       * CRITICAL INTERRUPT
       */
    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Front_Panel_NMI_or_Diagnostic_Interrupt",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Bus_Timeout",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_IO_Channel_Check_NMI",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Software_NMI",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_PCI_PERR",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_PCI_SERR",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_EISA_Fail_Safe_Timeout",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Bus_Correctable_Error",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_7) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Bus_Uncorrectable_Error",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_8) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Fatal_NMI",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_9) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Bus_Fatal_Error",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_10) < 0)
        goto cleanup;
      
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Bus_Degraded",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_11) < 0)
        goto cleanup;

      break;
      /* 
       * SLOT CONNECTOR
       */
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Fault_Status_Asserted",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Identify_Status_Asserted",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Slot_Connector_Device_Installed_or_Attached",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Slot_Connector_Ready_for_Device_Installation",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Slot_Connector_Ready_for_Device_Removal",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Slot_Power_is_Off",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Slot_Connector_Device_Removal_Request",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Interlock_Asserted",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_7) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Slot_is_Disabled",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_8) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Slot_Holds_Spare_Device",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_9) < 0)
        goto cleanup;

      break;
      /* 
       * WATCHDOG2
       */
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Timer_Expired",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Hard_Reset",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Power_Down",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Power_Cycle",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      /* event state 4-7 are "reserved" and useless */

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Timer_Interrupt",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_8) < 0)
        goto cleanup;

      break;
      /* 
       * ENTITY PRESENCE
       */
    case IPMI_SENSOR_TYPE_ENTITY_PRESENCE:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Entity_Present",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Entity_Absent",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Entity_Disabled",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      break;
      /* 
       * MANAGEMENT SUBSYSTEM HEALTH
       */
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Sensor_Access_Degraded_or_Unavailable",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Controller_Access_Degraded_or_Unavailable",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Management_Controller_Off_Line",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Management_Controller_Unavailable",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Sensor_Failure",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Failure",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      break;
      /* 
       * BATTERY
       */
    case IPMI_SENSOR_TYPE_BATTERY:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Battery_Low",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Battery_Failed",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_Battery_Presence_Detected",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      break;
      /* 
       * FRU STATE
       */
    case IPMI_SENSOR_TYPE_FRU_STATE:
      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Not_Installed",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_0) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Inactive",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_1) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Activation_Requested",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_2) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Activation_In_Progress",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_3) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Active",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_4) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Deactivation_Requested",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_5) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Deactivation_In_Progress",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_6) < 0)
        goto cleanup;

      snprintf(key_name,
               KEY_NAME_MAX_LEN,
               "Enable_%s_Event_FRU_Communication_Lost",
               type_str);
      if (_setup_specific_event_enable_key (state_data,
                                            section,
                                            key_name,
                                            event_state_7) < 0)
        goto cleanup;

      break;
    default:
      if (state_data->prog_data->args->config_args.verbose)
        pstdout_printf (state_data->pstate,
                        "## Unable to handle event flags for sensor type 0x%X\n",
                        sensor_type);
    }

  rv = 0;
 cleanup:
  return rv;
}

int 
_setup_sensor_specific_event_enable (ipmi_sensors_config_state_data_t *state_data,
                                     uint8_t *sdr_record,
                                     unsigned int sdr_record_len,
                                     struct config_section *section,
                                     uint8_t event_reading_type_code)
{
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);

  if (_setup_specific_event_enable_wrapper (state_data,
                                            sdr_record,
                                            sdr_record_len,
                                            section,
                                            "Assertion",
                                            &sdr_cache_get_assertion_supported) < 0)
    goto cleanup;

  if (_setup_specific_event_enable_wrapper (state_data,
                                            sdr_record,
                                            sdr_record_len,
                                            section,
                                            "Deassertion",
                                            &sdr_cache_get_deassertion_supported) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return rv;
}

int
setup_sensor_event_enable_fields (ipmi_sensors_config_state_data_t *state_data,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len,
                                  struct config_section *section)
{
  uint8_t event_message_control_support = 0;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section);

  if (sdr_cache_get_sensor_capabilities (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         &event_message_control_support,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL) < 0)
    goto cleanup;

  /* achu: I'm not quite sure what IPMI_SDR_GLOBAL_DISABLE_ONLY means.
   * Does it mean all sensors on the motherboard can be disabled
   * together? But one alone cannot?  I'm going to assume that's what
   * it means. I'll have to come back to this if that's not the case.
   */
  if (event_message_control_support == IPMI_SDR_PER_EVENT_ENABLE_DISABLE_SUPPORT
      || event_message_control_support == IPMI_SDR_ENTIRE_SENSOR_ONLY)
    {
      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Enable_All_Event_Messages",
                                  "Possible values: Yes/No",
                                  0,
                                  sensor_event_enable_enable_all_event_messages_checkout,
                                  sensor_event_enable_enable_all_event_messages_commit,
                                  config_yes_no_validate) < 0)
        goto cleanup;
      
      if (config_section_add_key (state_data->pstate,
                                  section,
                                  "Enable_Scanning_On_This_Sensor",
                                  "Possible values: Yes/No",
                                  0,
                                  sensor_event_enable_enable_scanning_on_this_sensor_checkout,
                                  sensor_event_enable_enable_scanning_on_this_sensor_commit,
                                  config_yes_no_validate) < 0)
        goto cleanup;
    }

  if (event_message_control_support == IPMI_SDR_PER_EVENT_ENABLE_DISABLE_SUPPORT)
    {
      uint8_t event_reading_type_code;
      int sensor_class;

      if (sdr_cache_get_event_reading_type_code (NULL,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 &event_reading_type_code) < 0)
        goto cleanup;
      
      sensor_class = sensor_classify (event_reading_type_code);
      
      if (sensor_class == SENSOR_CLASS_THRESHOLD)
        {
          if (_setup_threshold_event_enable (state_data,
                                             sdr_record,
                                             sdr_record_len,
                                             section) < 0)
            goto cleanup;
        }
      else if (sensor_class == SENSOR_CLASS_GENERIC_DISCRETE)
        {
          if (_setup_generic_event_enable (state_data,
                                           sdr_record,
                                           sdr_record_len,
                                           section,
                                           event_reading_type_code) < 0)
            goto cleanup;
        }
      else if (sensor_class == SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE)
        {
          if (_setup_sensor_specific_event_enable (state_data,
                                                   sdr_record,
                                                   sdr_record_len,
                                                   section,
                                                   event_reading_type_code) < 0)
            goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  return rv;
}
