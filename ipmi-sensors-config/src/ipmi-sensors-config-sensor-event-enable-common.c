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

/* convenience struct */
struct sensor_event_enable_data {
  uint8_t all_event_messages;
  uint8_t scanning_on_this_sensor;
};

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

  rv = 0;
 cleanup:
  return rv;
}
