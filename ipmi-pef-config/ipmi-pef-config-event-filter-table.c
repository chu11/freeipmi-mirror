/*
 * Copyright (C) 2007-2013 FreeIPMI Core Team
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

#include "ipmi-pef-config.h"
#include "ipmi-pef-config-map.h"
#include "ipmi-pef-config-utils.h"
#include "ipmi-pef-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience struct */
struct event_filter_table {
  uint8_t filter_type;
  uint8_t enable_filter;
  uint8_t event_filter_action_alert;
  uint8_t event_filter_action_power_off;
  uint8_t event_filter_action_reset;
  uint8_t event_filter_action_power_cycle;
  uint8_t event_filter_action_oem;
  uint8_t event_filter_action_diagnostic_interrupt;
  uint8_t event_filter_action_group_control_operation;
  uint8_t alert_policy_number;
  uint8_t group_control_selector;
  uint8_t event_severity;
  uint8_t generator_id_byte_1;
  uint8_t generator_id_byte_2;
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_trigger;
  uint16_t event_data1_offset_mask;
  uint8_t event_data1_and_mask;
  uint8_t event_data1_compare1;
  uint8_t event_data1_compare2;
  uint8_t event_data2_and_mask;
  uint8_t event_data2_compare1;
  uint8_t event_data2_compare2;
  uint8_t event_data3_and_mask;
  uint8_t event_data3_compare1;
  uint8_t event_data3_compare2;
};

static int
_config_section_update_keyvalue_output_hex (pstdout_state_t pstate,
                                            struct config_keyvalue *kv,
                                            unsigned int value_output)
{
  char buf[CONFIG_PARSE_BUFLEN];

  assert (kv);

  sprintf (buf, "0x%02X", value_output);

  return (config_section_update_keyvalue_output (pstate, kv, buf));
}

static config_err_t
_get_event_filter_table (struct ipmi_pef_config_state_data *state_data,
                         const char *section_name,
                         struct event_filter_table *eft)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t filter_number;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (eft);

  filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_event_filter_table_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_event_filter_table (state_data->ipmi_ctx,
                                                                    IPMI_GET_PEF_PARAMETER,
                                                                    filter_number,
                                                                    IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                    obj_cmd_rs) < 0)
    {
      config_err_t ret;

      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_pef_configuration_parameters_event_filter_table: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

#if 0
  if (FIID_OBJ_GET (obj_cmd_rs, "filter_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'filter_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
#endif

  if (FIID_OBJ_GET (obj_cmd_rs, "filter_configuration.type", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'filter_configuration.type': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->filter_type = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "filter_configuration.filter", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'filter_configuration.filter': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->enable_filter = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_filter_action.alert", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_filter_action.alert': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_filter_action_alert = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_filter_action.power_off", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_filter_action.power_off': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_filter_action_power_off = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_filter_action.reset", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_filter_action.reset': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_filter_action_reset = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_filter_action.power_cycle", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_filter_action.power_cycle': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_filter_action_power_cycle = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_filter_action.oem", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_filter_action.oem': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_filter_action_oem = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_filter_action.diagnostic_interrupt", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_filter_action.diagnostic_interrupt': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_filter_action_diagnostic_interrupt = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_filter_action.group_control_operation", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_filter_action.group_control_operation': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_filter_action_group_control_operation = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "alert_policy_number.policy_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'alert_policy_number.policy_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->alert_policy_number = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "alert_policy_number.group_control_selector", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'alert_policy_number.group_control_selector': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->group_control_selector = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_severity", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_severity': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_severity = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "generator_id_byte1", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'generator_id_byte1': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->generator_id_byte_1 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "generator_id_byte2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'generator_id_byte2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->generator_id_byte_2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "sensor_type", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sensor_type': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->sensor_type = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "sensor_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sensor_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->sensor_number = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_trigger", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_trigger': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_trigger = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data1_offset_mask", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data1_offset_mask': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data1_offset_mask = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data1_AND_mask", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data1_AND_mask': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data1_and_mask = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data1_compare1", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data1_compare1': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data1_compare1 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data1_compare2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data1_compare2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data1_compare2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data2_AND_mask", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data2_AND_mask': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data2_and_mask = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data2_compare1", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data2_compare1': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data2_compare1 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data2_compare2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data2_compare2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data2_compare2 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data3_AND_mask", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data3_AND_mask': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data3_and_mask = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data3_compare1", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data3_compare1': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data3_compare1 = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "event_data3_compare2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_data3_compare2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  eft->event_data3_compare2 = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_event_filter_table (struct ipmi_pef_config_state_data *state_data,
                         const char *section_name,
                         struct event_filter_table *eft)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t filter_number;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (eft);

  filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_event_filter_table (state_data->ipmi_ctx,
                                                                    filter_number,
                                                                    eft->filter_type,
                                                                    eft->enable_filter,
                                                                    eft->event_filter_action_alert,
                                                                    eft->event_filter_action_power_off,
                                                                    eft->event_filter_action_reset,
                                                                    eft->event_filter_action_power_cycle,
                                                                    eft->event_filter_action_oem,
                                                                    eft->event_filter_action_diagnostic_interrupt,
                                                                    eft->event_filter_action_group_control_operation,
                                                                    eft->alert_policy_number,
                                                                    eft->group_control_selector,
                                                                    eft->event_severity,
                                                                    eft->generator_id_byte_1,
                                                                    eft->generator_id_byte_2,
                                                                    eft->sensor_type,
                                                                    eft->sensor_number,
                                                                    eft->event_trigger,
                                                                    eft->event_data1_offset_mask,
                                                                    eft->event_data1_and_mask,
                                                                    eft->event_data1_compare1,
                                                                    eft->event_data1_compare2,
                                                                    eft->event_data2_and_mask,
                                                                    eft->event_data2_compare1,
                                                                    eft->event_data2_compare2,
                                                                    eft->event_data3_and_mask,
                                                                    eft->event_data3_compare1,
                                                                    eft->event_data3_compare2,
                                                                    obj_cmd_rs) < 0)
    {
      config_err_t ret;

      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_pef_configuration_parameters_event_filter_table: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
filter_type_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             filter_type_string (eft.filter_type)) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
filter_type_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.filter_type = filter_type_number (kv->value_input);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
enable_filter_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.enable_filter ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
enable_filter_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.enable_filter = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_filter_action_alert_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.event_filter_action_alert ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_action_alert_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_filter_action_alert = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_filter_action_power_off_checkout (const char *section_name,
                                        struct config_keyvalue *kv,
                                        void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.event_filter_action_power_off ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_action_power_off_commit (const char *section_name,
                                      const struct config_keyvalue *kv,
                                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_filter_action_power_off = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_filter_action_reset_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.event_filter_action_reset ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_action_reset_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_filter_action_reset = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_filter_action_power_cycle_checkout (const char *section_name,
                                          struct config_keyvalue *kv,
                                          void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.event_filter_action_power_cycle ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_action_power_cycle_commit (const char *section_name,
                                        const struct config_keyvalue *kv,
                                        void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_filter_action_power_cycle = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_filter_action_oem_checkout (const char *section_name,
                                  struct config_keyvalue *kv,
                                  void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.event_filter_action_oem ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_action_oem_commit (const char *section_name,
                                const struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_filter_action_oem = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_filter_action_diagnostic_interrupt_checkout (const char *section_name,
                                                   struct config_keyvalue *kv,
                                                   void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.event_filter_action_diagnostic_interrupt ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_action_diagnostic_interrupt_commit (const char *section_name,
                                                 const struct config_keyvalue *kv,
                                                 void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_filter_action_diagnostic_interrupt = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_filter_action_group_control_operation_checkout (const char *section_name,
                                                      struct config_keyvalue *kv,
                                                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             eft.event_filter_action_group_control_operation ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_filter_action_group_control_operation_commit (const char *section_name,
                                                    const struct config_keyvalue *kv,
                                                    void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_filter_action_group_control_operation = same (kv->value_input, "yes");

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
alert_policy_number_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output_unsigned_int (state_data->pstate,
                                                          kv,
                                                          eft.alert_policy_number) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_policy_number_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.alert_policy_number = atoi (kv->value_input);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
group_control_selector_checkout (const char *section_name,
                                 struct config_keyvalue *kv,
                                 void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output_unsigned_int (state_data->pstate,
                                                          kv,
                                                          eft.group_control_selector) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
group_control_selector_commit (const char *section_name,
                               const struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.group_control_selector = atoi (kv->value_input);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_severity_checkout (const char *section_name,
                         struct config_keyvalue *kv,
                         void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             event_severity_string (eft.event_severity)) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_severity_commit (const char *section_name,
                       const struct config_keyvalue *kv,
                       void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  eft.event_severity = event_severity_number (kv->value_input);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
generator_id_byte_1_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.generator_id_byte_1) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
generator_id_byte_1_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.generator_id_byte_1 = strtol (kv->value_input, NULL, 0);


  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
generator_id_byte_2_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.generator_id_byte_2) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
generator_id_byte_2_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.generator_id_byte_2 = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
sensor_type_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;
  char *str;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* If string available, output that, else output OEM code */

  str = sensor_type_string (eft.sensor_type);
  if (str && strlen (str))
    {
      if (config_section_update_keyvalue_output (state_data->pstate,
                                                 kv,
                                                 str) < 0)
        return (CONFIG_ERR_FATAL_ERROR);
    }
  else
    {
      if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                      kv,
                                                      eft.sensor_type) < 0)
        return (CONFIG_ERR_FATAL_ERROR);
    }

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
sensor_type_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;
  int num;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  num = sensor_type_number (kv->value_input);
  
  if (num < 0)
    /* previously checked for correctness, so no error check */
    eft.sensor_type = strtol (kv->value_input, NULL, 0);
  else
    eft.sensor_type = num;
  
  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
sensor_number_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.sensor_number) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
sensor_number_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.sensor_number = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_trigger_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_trigger) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_trigger_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_trigger = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data1_offset_mask_checkout (const char *section_name,
                                  struct config_keyvalue *kv,
                                  void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data1_offset_mask) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data1_offset_mask_commit (const char *section_name,
                                const struct config_keyvalue *kv,
                                void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data1_offset_mask = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data1_and_mask_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data1_and_mask) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data1_and_mask_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data1_and_mask = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data1_compare1_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data1_compare1) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data1_compare1_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data1_compare1 = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data1_compare2_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data1_compare2) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data1_compare2_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data1_compare2 = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data2_and_mask_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data2_and_mask) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data2_and_mask_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data2_and_mask = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data2_compare1_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data2_compare1) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data2_compare1_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data2_compare1 = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data2_compare2_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data2_compare2) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data2_compare2_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data2_compare2 = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data3_and_mask_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data3_and_mask) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data3_and_mask_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data3_and_mask = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data3_compare1_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data3_compare1) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data3_compare1_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data3_compare1 = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

static config_err_t
event_data3_compare2_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (_config_section_update_keyvalue_output_hex (state_data->pstate,
                                                  kv,
                                                  eft.event_data3_compare2) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
event_data3_compare2_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  config_err_t ret;
  struct event_filter_table eft;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_event_filter_table (state_data,
                                      section_name,
                                      &eft)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  eft.event_data3_compare2 = strtol (kv->value_input, NULL, 0);

  return (_set_event_filter_table (state_data,
                                   section_name,
                                   &eft));
}

struct config_section *
ipmi_pef_config_event_filter_table_section_get (ipmi_pef_config_state_data_t *state_data, unsigned int num)
{
  struct config_section *section = NULL;
  char buf[CONFIG_MAX_SECTION_NAME_LEN];

  assert (state_data);
  assert (num);

  snprintf (buf, CONFIG_MAX_SECTION_NAME_LEN, "Event_Filter_%u", num);

  if (!(section = config_section_create (state_data->pstate,
                                         buf,
                                         NULL,
                                         NULL,
                                         0,
                                         NULL,
                                         NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Filter_Type",
                              "Possible values: Manufacturer_Pre_Configured/Software_Configurable/Reserved1/Reserved3",
                              0,
                              filter_type_checkout,
                              filter_type_commit,
                              filter_type_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Enable_Filter",
                              "Possible values: Yes/No",
                              0,
                              enable_filter_checkout,
                              enable_filter_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Action_Alert",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_alert_checkout,
                              event_filter_action_alert_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Action_Power_Off",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_power_off_checkout,
                              event_filter_action_power_off_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Action_Reset",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_reset_checkout,
                              event_filter_action_reset_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Action_Power_Cycle",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_power_cycle_checkout,
                              event_filter_action_power_cycle_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Action_Oem",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_oem_checkout,
                              event_filter_action_oem_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Action_Diagnostic_Interrupt",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_diagnostic_interrupt_checkout,
                              event_filter_action_diagnostic_interrupt_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Filter_Action_Group_Control_Operation",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_group_control_operation_checkout,
                              event_filter_action_group_control_operation_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_Policy_Number",
                              "Give a valid number",
                              0,
                              alert_policy_number_checkout,
                              alert_policy_number_commit,
                              config_number_range_four_bits) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Group_Control_Selector",
                              "Give a valid number",
                              0,
                              group_control_selector_checkout,
                              group_control_selector_commit,
                              config_number_range_three_bits) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Severity",
                              "Possible values: Unspecified/Monitor/Information/OK/Non_Critical/Critical/Non_Recoverable",
                              0,
                              event_severity_checkout,
                              event_severity_commit,
                              event_severity_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Generator_Id_Byte_1",
                              "Specify a hex Slave Address or Software ID from Event Message or 0xFF to Match Any",
                              0,
                              generator_id_byte_1_checkout,
                              generator_id_byte_1_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Generator_Id_Byte_2",
                              "Specify a hex Channel Number or LUN to match or 0xFF to Match Any",
                              0,
                              generator_id_byte_2_checkout,
                              generator_id_byte_2_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Sensor_Type",
                              "Specify a Sensor Type, via hex or see MAN page for string options",
                              0,
                              sensor_type_checkout,
                              sensor_type_commit,
                              sensor_type_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Sensor_Number",
                              "Specify a Sensor Number or 0xFF to Match Any",
                              0,
                              sensor_number_checkout,
                              sensor_number_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Trigger",
                              "Specify a Event/Reading Type Number or 0xFF to Match Any",
                              0,
                              event_trigger_checkout,
                              event_trigger_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data1_Offset_Mask",
                              "Give a valid number",
                              0,
                              event_data1_offset_mask_checkout,
                              event_data1_offset_mask_commit,
                              config_number_range_two_bytes) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data1_AND_Mask",
                              "Give a valid number",
                              0,
                              event_data1_and_mask_checkout,
                              event_data1_and_mask_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data1_Compare1",
                              "Give a valid number",
                              0,
                              event_data1_compare1_checkout,
                              event_data1_compare1_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data1_Compare2",
                              "Give a valid number",
                              0,
                              event_data1_compare2_checkout,
                              event_data1_compare2_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data2_AND_Mask",
                              "Give a valid number",
                              0,
                              event_data2_and_mask_checkout,
                              event_data2_and_mask_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data2_Compare1",
                              "Give a valid number",
                              0,
                              event_data2_compare1_checkout,
                              event_data2_compare1_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data2_Compare2",
                              "Give a valid number",
                              0,
                              event_data2_compare2_checkout,
                              event_data2_compare2_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data3_AND_Mask",
                              "Give a valid number",
                              0,
                              event_data3_and_mask_checkout,
                              event_data3_and_mask_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data3_Compare1",
                              "Give a valid number",
                              0,
                              event_data3_compare1_checkout,
                              event_data3_compare1_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Event_Data3_Compare2",
                              "Give a valid number",
                              0,
                              event_data3_compare2_checkout,
                              event_data3_compare2_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}
