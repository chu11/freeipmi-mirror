#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "ipmi-pef.h"
#include "ipmi-pef-common.h"
#include "ipmi-pef-diff.h"
#include "ipmi-pef-map.h"
#include "ipmi-pef-sections.h"
#include "ipmi-pef-utils.h"
#include "ipmi-pef-validate.h"
#include "ipmi-pef-wrapper.h"

static pef_err_t
event_filter_get (ipmi_pef_state_data_t *state_data,
                  uint8_t filter_number,
                  uint8_t *filter_type,
                  uint8_t *enable_filter,
                  uint8_t *event_filter_action_alert,
                  uint8_t *event_filter_action_power_off,
                  uint8_t *event_filter_action_reset,
                  uint8_t *event_filter_action_power_cycle,
                  uint8_t *event_filter_action_oem,
                  uint8_t *event_filter_action_diagnostic_interrupt,
                  uint8_t *event_filter_action_group_control_operation,
                  uint8_t *alert_policy_number,
                  uint8_t *group_control_selector,
                  uint8_t *event_severity,
                  uint8_t *generator_id_byte1,
                  uint8_t *generator_id_byte2,
                  uint8_t *sensor_type,
                  uint8_t *sensor_number,
                  uint8_t *event_trigger,
                  uint16_t *event_data1_offset_mask,
                  uint8_t *event_data1_AND_mask,
                  uint8_t *event_data1_compare1,
                  uint8_t *event_data1_compare2,
                  uint8_t *event_data2_AND_mask,
                  uint8_t *event_data2_compare1,
                  uint8_t *event_data2_compare2,
                  uint8_t *event_data3_AND_mask,
                  uint8_t *event_data3_compare1,
                  uint8_t *event_data3_compare2)
{
  uint8_t tmp_filter_type;
  uint8_t tmp_enable_filter;
  uint8_t tmp_event_filter_action_alert;
  uint8_t tmp_event_filter_action_power_off;
  uint8_t tmp_event_filter_action_reset;
  uint8_t tmp_event_filter_action_power_cycle;
  uint8_t tmp_event_filter_action_oem;
  uint8_t tmp_event_filter_action_diagnostic_interrupt;
  uint8_t tmp_event_filter_action_group_control_operation;
  uint8_t tmp_alert_policy_number;
  uint8_t tmp_group_control_selector;
  uint8_t tmp_event_severity;
  uint8_t tmp_generator_id_byte1;
  uint8_t tmp_generator_id_byte2;
  uint8_t tmp_sensor_type;
  uint8_t tmp_sensor_number;
  uint8_t tmp_event_trigger;
  uint16_t tmp_event_data1_offset_mask;
  uint8_t tmp_event_data1_AND_mask;
  uint8_t tmp_event_data1_compare1;
  uint8_t tmp_event_data1_compare2;
  uint8_t tmp_event_data2_AND_mask;
  uint8_t tmp_event_data2_compare1;
  uint8_t tmp_event_data2_compare2;
  uint8_t tmp_event_data3_AND_mask;
  uint8_t tmp_event_data3_compare1;
  uint8_t tmp_event_data3_compare2;
  pef_err_t ret;

  if ((ret = get_bmc_pef_conf_event_filter_table (state_data,
                                                  filter_number,
                                                  &tmp_filter_type,
                                                  &tmp_enable_filter,
                                                  &tmp_event_filter_action_alert,
                                                  &tmp_event_filter_action_power_off,
                                                  &tmp_event_filter_action_reset,
                                                  &tmp_event_filter_action_power_cycle,
                                                  &tmp_event_filter_action_oem,
                                                  &tmp_event_filter_action_diagnostic_interrupt,
                                                  &tmp_event_filter_action_group_control_operation,
                                                  &tmp_alert_policy_number,
                                                  &tmp_group_control_selector,
                                                  &tmp_event_severity,
                                                  &tmp_generator_id_byte1,
                                                  &tmp_generator_id_byte2,
                                                  &tmp_sensor_type,
                                                  &tmp_sensor_number,
                                                  &tmp_event_trigger,
                                                  &tmp_event_data1_offset_mask,
                                                  &tmp_event_data1_AND_mask,
                                                  &tmp_event_data1_compare1,
                                                  &tmp_event_data1_compare2,
                                                  &tmp_event_data2_AND_mask,
                                                  &tmp_event_data2_compare1,
                                                  &tmp_event_data2_compare2,
                                                  &tmp_event_data3_AND_mask,
                                                  &tmp_event_data3_compare1,
                                                  &tmp_event_data3_compare2)) != PEF_ERR_SUCCESS)
    return ret;

  if (filter_type)
    *filter_type = tmp_filter_type;
  if (enable_filter)
    *enable_filter = tmp_enable_filter;
  if (event_filter_action_alert)
    *event_filter_action_alert = tmp_event_filter_action_alert;
  if (event_filter_action_power_off)
    *event_filter_action_power_off = tmp_event_filter_action_power_off;
  if (event_filter_action_reset)
    *event_filter_action_reset = tmp_event_filter_action_reset;
  if (event_filter_action_power_cycle)
    *event_filter_action_power_cycle = tmp_event_filter_action_power_cycle;
  if (event_filter_action_oem)
    *event_filter_action_oem = tmp_event_filter_action_oem;
  if (event_filter_action_diagnostic_interrupt)
    *event_filter_action_diagnostic_interrupt = tmp_event_filter_action_diagnostic_interrupt;
  if (event_filter_action_group_control_operation)
    *event_filter_action_group_control_operation = tmp_event_filter_action_group_control_operation;
  if (alert_policy_number)
    *alert_policy_number = tmp_alert_policy_number;
  if (group_control_selector)
    *group_control_selector = tmp_group_control_selector;
  if (event_severity)
    *event_severity = tmp_event_severity;
  if (generator_id_byte1)
    *generator_id_byte1 = tmp_generator_id_byte1;
  if (generator_id_byte2)
    *generator_id_byte2 = tmp_generator_id_byte2;
  if (sensor_type)
    *sensor_type = tmp_sensor_type;
  if (sensor_number)
    *sensor_number = tmp_sensor_number;
  if (event_trigger)
    *event_trigger = tmp_event_trigger;
  if (event_data1_offset_mask)
    *event_data1_offset_mask = tmp_event_data1_offset_mask;
  if (event_data1_AND_mask)
    *event_data1_AND_mask = tmp_event_data1_AND_mask;
  if (event_data1_compare1)
    *event_data1_compare1 = tmp_event_data1_compare1;
  if (event_data1_compare2)
    *event_data1_compare2 = tmp_event_data1_compare2;
  if (event_data2_AND_mask)
    *event_data2_AND_mask = tmp_event_data2_AND_mask;
  if (event_data2_compare1)
    *event_data2_compare1 = tmp_event_data2_compare1;
  if (event_data2_compare2)
    *event_data2_compare2 = tmp_event_data2_compare2;
  if (event_data3_AND_mask)
    *event_data3_AND_mask = tmp_event_data3_AND_mask;
  if (event_data3_compare1)
    *event_data3_compare1 = tmp_event_data3_compare1;
  if (event_data3_compare2)
    *event_data3_compare2 = tmp_event_data3_compare2;

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_filter_set (ipmi_pef_state_data_t *state_data,
                  uint8_t filter_number,
                  uint8_t filter_type,
                  uint8_t filter_type_is_set,
                  uint8_t enable_filter,
                  uint8_t enable_filter_is_set,
                  uint8_t event_filter_action_alert,
                  uint8_t event_filter_action_alert_is_set,
                  uint8_t event_filter_action_power_off,
                  uint8_t event_filter_action_power_off_is_set,
                  uint8_t event_filter_action_reset,
                  uint8_t event_filter_action_reset_is_set,
                  uint8_t event_filter_action_power_cycle,
                  uint8_t event_filter_action_power_cycle_is_set,
                  uint8_t event_filter_action_oem,
                  uint8_t event_filter_action_oem_is_set,
                  uint8_t event_filter_action_diagnostic_interrupt,
                  uint8_t event_filter_action_diagnostic_interrupt_is_set,
                  uint8_t event_filter_action_group_control_operation,
                  uint8_t event_filter_action_group_control_operation_is_set,
                  uint8_t alert_policy_number,
                  uint8_t alert_policy_number_is_set,
                  uint8_t group_control_selector,
                  uint8_t group_control_selector_is_set,
                  uint8_t event_severity,
                  uint8_t event_severity_is_set,
                  uint8_t generator_id_byte1,
                  uint8_t generator_id_byte1_is_set,
                  uint8_t generator_id_byte2,
                  uint8_t generator_id_byte2_is_set,
                  uint8_t sensor_type,
                  uint8_t sensor_type_is_set,
                  uint8_t sensor_number,
                  uint8_t sensor_number_is_set,
                  uint8_t event_trigger,
                  uint8_t event_trigger_is_set,
                  uint16_t event_data1_offset_mask,
                  uint8_t event_data1_offset_mask_is_set,
                  uint8_t event_data1_AND_mask,
                  uint8_t event_data1_AND_mask_is_set,
                  uint8_t event_data1_compare1,
                  uint8_t event_data1_compare1_is_set,
                  uint8_t event_data1_compare2,
                  uint8_t event_data1_compare2_is_set,
                  uint8_t event_data2_AND_mask,
                  uint8_t event_data2_AND_mask_is_set,
                  uint8_t event_data2_compare1,
                  uint8_t event_data2_compare1_is_set,
                  uint8_t event_data2_compare2,
                  uint8_t event_data2_compare2_is_set,
                  uint8_t event_data3_AND_mask,
                  uint8_t event_data3_AND_mask_is_set,
                  uint8_t event_data3_compare1,
                  uint8_t event_data3_compare1_is_set,
                  uint8_t event_data3_compare2,
                  uint8_t event_data3_compare2_is_set)
{
  uint8_t tmp_filter_type;
  uint8_t tmp_enable_filter;
  uint8_t tmp_event_filter_action_alert;
  uint8_t tmp_event_filter_action_power_off;
  uint8_t tmp_event_filter_action_reset;
  uint8_t tmp_event_filter_action_power_cycle;
  uint8_t tmp_event_filter_action_oem;
  uint8_t tmp_event_filter_action_diagnostic_interrupt;
  uint8_t tmp_event_filter_action_group_control_operation;
  uint8_t tmp_alert_policy_number;
  uint8_t tmp_group_control_selector;
  uint8_t tmp_event_severity;
  uint8_t tmp_generator_id_byte1;
  uint8_t tmp_generator_id_byte2;
  uint8_t tmp_sensor_type;
  uint8_t tmp_sensor_number;
  uint8_t tmp_event_trigger;
  uint16_t tmp_event_data1_offset_mask;
  uint8_t tmp_event_data1_AND_mask;
  uint8_t tmp_event_data1_compare1;
  uint8_t tmp_event_data1_compare2;
  uint8_t tmp_event_data2_AND_mask;
  uint8_t tmp_event_data2_compare1;
  uint8_t tmp_event_data2_compare2;
  uint8_t tmp_event_data3_AND_mask;
  uint8_t tmp_event_data3_compare1;
  uint8_t tmp_event_data3_compare2;
  pef_err_t ret;

  if ((ret = get_bmc_pef_conf_event_filter_table (state_data,
                                                  filter_number,
                                                  &tmp_filter_type,
                                                  &tmp_enable_filter,
                                                  &tmp_event_filter_action_alert,
                                                  &tmp_event_filter_action_power_off,
                                                  &tmp_event_filter_action_reset,
                                                  &tmp_event_filter_action_power_cycle,
                                                  &tmp_event_filter_action_oem,
                                                  &tmp_event_filter_action_diagnostic_interrupt,
                                                  &tmp_event_filter_action_group_control_operation,
                                                  &tmp_alert_policy_number,
                                                  &tmp_group_control_selector,
                                                  &tmp_event_severity,
                                                  &tmp_generator_id_byte1,
                                                  &tmp_generator_id_byte2,
                                                  &tmp_sensor_type,
                                                  &tmp_sensor_number,
                                                  &tmp_event_trigger,
                                                  &tmp_event_data1_offset_mask,
                                                  &tmp_event_data1_AND_mask,
                                                  &tmp_event_data1_compare1,
                                                  &tmp_event_data1_compare2,
                                                  &tmp_event_data2_AND_mask,
                                                  &tmp_event_data2_compare1,
                                                  &tmp_event_data2_compare2,
                                                  &tmp_event_data3_AND_mask,
                                                  &tmp_event_data3_compare1,
                                                  &tmp_event_data3_compare2)) != PEF_ERR_SUCCESS)
    return ret;

  if (filter_type_is_set)
    tmp_filter_type = filter_type;
  if (enable_filter_is_set)
    tmp_enable_filter = enable_filter;
  if (event_filter_action_alert_is_set)
    tmp_event_filter_action_alert = event_filter_action_alert;
  if (event_filter_action_power_off_is_set)
    tmp_event_filter_action_power_off = event_filter_action_power_off;
  if (event_filter_action_reset_is_set)
    tmp_event_filter_action_reset = event_filter_action_reset;
  if (event_filter_action_power_cycle_is_set)
    tmp_event_filter_action_power_cycle = event_filter_action_power_cycle;
  if (event_filter_action_oem_is_set)
    tmp_event_filter_action_oem = event_filter_action_oem;
  if (event_filter_action_diagnostic_interrupt_is_set)
    tmp_event_filter_action_diagnostic_interrupt = event_filter_action_diagnostic_interrupt;
  if (event_filter_action_group_control_operation_is_set)
    tmp_event_filter_action_group_control_operation = event_filter_action_group_control_operation;
  if (alert_policy_number_is_set)
    tmp_alert_policy_number = alert_policy_number;
  if (group_control_selector_is_set)
    tmp_group_control_selector = group_control_selector;
  if (event_severity_is_set)
    tmp_event_severity = event_severity;
  if (generator_id_byte1_is_set)
    tmp_generator_id_byte1 = generator_id_byte1;
  if (generator_id_byte2_is_set)
    tmp_generator_id_byte2 = generator_id_byte2;
  if (sensor_type_is_set)
    tmp_sensor_type = sensor_type;
  if (sensor_number_is_set)
    tmp_sensor_number = sensor_number;
  if (event_trigger_is_set)
    tmp_event_trigger = event_trigger;
  if (event_data1_offset_mask_is_set)
    tmp_event_data1_offset_mask = event_data1_offset_mask;
  if (event_data1_AND_mask_is_set)
    tmp_event_data1_AND_mask = event_data1_AND_mask;
  if (event_data1_compare1_is_set)
    tmp_event_data1_compare1 = event_data1_compare1;
  if (event_data1_compare2_is_set)
    tmp_event_data1_compare2 = event_data1_compare2;
  if (event_data2_AND_mask_is_set)
    tmp_event_data2_AND_mask = event_data2_AND_mask;
  if (event_data2_compare1_is_set)
    tmp_event_data2_compare1 = event_data2_compare1;
  if (event_data2_compare2_is_set)
    tmp_event_data2_compare2 = event_data2_compare2;
  if (event_data3_AND_mask_is_set)
    tmp_event_data3_AND_mask = event_data3_AND_mask;
  if (event_data3_compare1_is_set)
    tmp_event_data3_compare1 = event_data3_compare1;
  if (event_data3_compare2_is_set)
    tmp_event_data3_compare2 = event_data3_compare2;

  if ((ret = set_bmc_pef_conf_event_filter_table (state_data,
                                                  filter_number,
                                                  tmp_filter_type,
                                                  tmp_enable_filter,
                                                  tmp_event_filter_action_alert,
                                                  tmp_event_filter_action_power_off,
                                                  tmp_event_filter_action_reset,
                                                  tmp_event_filter_action_power_cycle,
                                                  tmp_event_filter_action_oem,
                                                  tmp_event_filter_action_diagnostic_interrupt,
                                                  tmp_event_filter_action_group_control_operation,
                                                  tmp_alert_policy_number,
                                                  tmp_group_control_selector,
                                                  tmp_event_severity,
                                                  tmp_generator_id_byte1,
                                                  tmp_generator_id_byte2,
                                                  tmp_sensor_type,
                                                  tmp_sensor_number,
                                                  tmp_event_trigger,
                                                  tmp_event_data1_offset_mask,
                                                  tmp_event_data1_AND_mask,
                                                  tmp_event_data1_compare1,
                                                  tmp_event_data1_compare2,
                                                  tmp_event_data2_AND_mask,
                                                  tmp_event_data2_compare1,
                                                  tmp_event_data2_compare2,
                                                  tmp_event_data3_AND_mask,
                                                  tmp_event_data3_compare1,
                                                  tmp_event_data3_compare2)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_err_t
filter_type_checkout (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      struct keyvalue *kv)
{
  uint8_t filter_type;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &filter_type,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (filter_type_string (filter_type))))
    {
      perror("strdup");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
filter_type_commit (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           filter_type_number (kv->value), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
filter_type_diff (ipmi_pef_state_data_t *state_data,
                  const struct section *sect,
                  const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = filter_type_number (kv->value);
  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   filter_type_string (get_val));
    }
  return ret;
}

static pef_err_t
enable_filter_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t enable_filter;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               &enable_filter,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
enable_filter_action_alert_checkout (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     struct keyvalue *kv)
{
  uint8_t enable_filter_action_alert;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               &enable_filter_action_alert,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter_action_alert)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_action_alert_commit (ipmi_pef_state_data_t *state_data,
                                   const struct section *sect,
                                   const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_action_alert_diff (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
enable_filter_action_power_off_checkout (ipmi_pef_state_data_t *state_data,
                                         const struct section *sect,
                                         struct keyvalue *kv)
{
  uint8_t enable_filter_action_power_off;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               &enable_filter_action_power_off,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter_action_power_off)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_action_power_off_commit (ipmi_pef_state_data_t *state_data,
                                       const struct section *sect,
                                       const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_action_power_off_diff (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
enable_filter_action_reset_checkout (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     struct keyvalue *kv)
{
  uint8_t enable_filter_action_reset;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &enable_filter_action_reset,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter_action_reset)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_action_reset_commit (ipmi_pef_state_data_t *state_data,
                                   const struct section *sect,
                                   const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_action_reset_diff (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}


static pef_err_t
enable_filter_action_power_cycle_checkout (ipmi_pef_state_data_t *state_data,
                                           const struct section *sect,
                                           struct keyvalue *kv)
{
  uint8_t enable_filter_action_power_cycle;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &enable_filter_action_power_cycle,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter_action_power_cycle)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_action_power_cycle_commit (ipmi_pef_state_data_t *state_data,
                                         const struct section *sect,
                                         const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_action_power_cycle_diff (ipmi_pef_state_data_t *state_data,
                                       const struct section *sect,
                                       const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
enable_filter_action_oem_checkout (ipmi_pef_state_data_t *state_data,
                                   const struct section *sect,
                                   struct keyvalue *kv)
{
  uint8_t enable_filter_action_oem;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &enable_filter_action_oem,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter_action_oem)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_action_oem_commit (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_action_oem_diff (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
enable_filter_action_diagnostic_interrupt_checkout (ipmi_pef_state_data_t *state_data,
                                                    const struct section *sect,
                                                    struct keyvalue *kv)
{
  uint8_t enable_filter_action_diagnostic_interrupt;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &enable_filter_action_diagnostic_interrupt,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter_action_diagnostic_interrupt)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_action_diagnostic_interrupt_commit (ipmi_pef_state_data_t *state_data,
                                                  const struct section *sect,
                                                  const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_action_diagnostic_interrupt_diff (ipmi_pef_state_data_t *state_data,
                                                const struct section *sect,
                                                const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
enable_filter_action_group_control_operation_checkout (ipmi_pef_state_data_t *state_data,
                                                       const struct section *sect,
                                                       struct keyvalue *kv)
{
  uint8_t enable_filter_action_group_control_operation;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &enable_filter_action_group_control_operation,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable_filter_action_group_control_operation)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return PEF_ERR_FATAL_ERROR;
        }
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
enable_filter_action_group_control_operation_commit (ipmi_pef_state_data_t *state_data,
                                                     const struct section *sect,
                                                     const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           same (kv->value, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
enable_filter_action_group_control_operation_diff (ipmi_pef_state_data_t *state_data,
                                                   const struct section *sect,
                                                   const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
alert_policy_number_checkout (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  uint8_t alert_policy_number;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &alert_policy_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%u", alert_policy_number) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
alert_policy_number_commit (ipmi_pef_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t alert_policy_number;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           alert_policy_number, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
alert_policy_number_diff (ipmi_pef_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "%u", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
group_control_selector_checkout (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  uint8_t group_control_selector;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &group_control_selector,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%u", group_control_selector) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
group_control_selector_commit (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t group_control_selector;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           group_control_selector, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
group_control_selector_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "%u", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_severity_checkout (ipmi_pef_state_data_t *state_data,
                         const struct section *sect,
                         struct keyvalue *kv)
{
  uint8_t event_severity;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_severity,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (event_severity_string (event_severity))))
    {
      perror("strdup");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_severity_commit (ipmi_pef_state_data_t *state_data,
                       const struct section *sect,
                       const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_severity_number (kv->value), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_severity_diff (ipmi_pef_state_data_t *state_data,
                     const struct section *sect,
                     const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = event_severity_number (kv->value);
  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   event_severity_string (get_val));
    }
  return ret;
}

static pef_err_t
generator_id_byte_1_checkout (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  uint8_t generator_id_byte_1;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &generator_id_byte_1,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", generator_id_byte_1) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
generator_id_byte_1_commit (ipmi_pef_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t generator_id_byte_1;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           generator_id_byte_1, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
generator_id_byte_1_diff (ipmi_pef_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
generator_id_byte_2_checkout (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  uint8_t generator_id_byte_2;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &generator_id_byte_2,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", generator_id_byte_2) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
generator_id_byte_2_commit (ipmi_pef_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t generator_id_byte_2;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           generator_id_byte_2, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
generator_id_byte_2_diff (ipmi_pef_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
sensor_type_checkout (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      struct keyvalue *kv)
{
  uint8_t sensor_type;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &sensor_type,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", sensor_type) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
sensor_type_commit (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t sensor_type;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           sensor_type, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
sensor_type_diff (ipmi_pef_state_data_t *state_data,
                  const struct section *sect,
                  const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
sensor_number_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t sensor_number;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &sensor_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", sensor_number) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
sensor_number_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t sensor_number;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           sensor_number, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
sensor_number_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_trigger_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_trigger;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_trigger,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_trigger) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_trigger_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_trigger;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_trigger, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_trigger_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_1_offset_mask_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint16_t event_data_1_offset_mask;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_1_offset_mask,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_1_offset_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_1_offset_mask_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint16_t event_data_1_offset_mask;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_1_offset_mask, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_1_offset_mask_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint16_t get_val;
  uint16_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_1_and_mask_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_1_and_mask;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_1_and_mask,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_1_and_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_1_and_mask_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_1_and_mask;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_1_and_mask, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_1_and_mask_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_1_compare_1_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_1_compare_1;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_1_compare_1,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_1_compare_1) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_1_compare_1_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_1_compare_1;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_1_compare_1, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_1_compare_1_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_1_compare_2_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_1_compare_2;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_1_compare_2,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_1_compare_2) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_1_compare_2_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_1_compare_2;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_1_compare_2, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_1_compare_2_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_2_and_mask_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_2_and_mask;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_2_and_mask,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_2_and_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_2_and_mask_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_2_and_mask;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_2_and_mask, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_2_and_mask_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_2_compare_1_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_2_compare_1;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_2_compare_1,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_2_compare_1) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_2_compare_1_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_2_compare_1;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_2_compare_1, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_2_compare_1_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_2_compare_2_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_2_compare_2;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_2_compare_2,
                               NULL,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_2_compare_2) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_2_compare_2_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_2_compare_2;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_2_compare_2, 1,
                           0, 0,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_2_compare_2_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_3_and_mask_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_3_and_mask;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_3_and_mask,
                               NULL,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_3_and_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_3_and_mask_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_3_and_mask;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_3_and_mask, 1,
                           0, 0,
                           0, 0);
}

static pef_diff_t
event_data_3_and_mask_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_3_compare_1_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_3_compare_1;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_3_compare_1,
                               NULL)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_3_compare_1) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_3_compare_1_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_3_compare_1;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_3_compare_1, 1,
                           0, 0);
}

static pef_diff_t
event_data_3_compare_1_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val,
                              NULL)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data_3_compare_2_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t event_data_3_compare_2;
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_data_3_compare_2)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", event_data_3_compare_2) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data_3_compare_2_commit (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  uint8_t event_data_3_compare_2;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  return event_filter_set (state_data,
                           event_filter_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           event_data_3_compare_2, 1);
}

static pef_diff_t
event_data_3_compare_2_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              &get_val)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

struct section *
ipmi_pef_event_filter_table_section_get (ipmi_pef_state_data_t *state_data, int num)
{
  struct section *sect = NULL;
  char buf[64];

  if (num <= 0)
    {
      fprintf(stderr, "Invalid Num = %d\n", num);
      return NULL;
    }

  snprintf(buf, 64, "Event_Filter_%d", num);

  if (!(sect = ipmi_pef_section_create (state_data, buf)))
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Filter_Type",
                                     "Possible values: Manufacturer_Pre_Configured/Software_Configurable/Reserved1/Reserved3",
                                     0,
                                     filter_type_checkout,
                                     filter_type_commit,
                                     filter_type_diff,
                                     filter_type_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_checkout,
                                     enable_filter_commit,
                                     enable_filter_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter_Action_Alert",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_action_alert_checkout,
                                     enable_filter_action_alert_commit,
                                     enable_filter_action_alert_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter_Action_Power_Off",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_action_power_off_checkout,
                                     enable_filter_action_power_off_commit,
                                     enable_filter_action_power_off_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter_Action_Reset",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_action_reset_checkout,
                                     enable_filter_action_reset_commit,
                                     enable_filter_action_reset_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter_Action_Power_Cycle",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_action_power_cycle_checkout,
                                     enable_filter_action_power_cycle_commit,
                                     enable_filter_action_power_cycle_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter_Action_Oem",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_action_oem_checkout,
                                     enable_filter_action_oem_commit,
                                     enable_filter_action_oem_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter_Action_Diagnostic_Interrupt",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_action_diagnostic_interrupt_checkout,
                                     enable_filter_action_diagnostic_interrupt_commit,
                                     enable_filter_action_diagnostic_interrupt_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Enable_Filter_Action_Group_Control_Operation",
                                     "Possible values: Yes/No",
                                     0,
                                     enable_filter_action_group_control_operation_checkout,
                                     enable_filter_action_group_control_operation_commit,
                                     enable_filter_action_group_control_operation_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Alert_Policy_Number",
                                     "Give a valid number",
                                     0,
                                     alert_policy_number_checkout,
                                     alert_policy_number_commit,
                                     alert_policy_number_diff,
                                     number_range_four_bits) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Group_Control_Selector",
                                     "Give a valid number",
                                     0,
                                     group_control_selector_checkout,
                                     group_control_selector_commit,
                                     group_control_selector_diff,
                                     number_range_three_bits) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Severity",
                                     "Possible values: Unspecified/Monitor/Information/OK/Non_Critical/Critical/Non_Recoverable",
                                     0,
                                     event_severity_checkout,
                                     event_severity_commit,
                                     event_severity_diff,
                                     event_severity_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Generator_Id_Byte_1",
                                     "Specify a hex Slave Address or Software ID from Event Message or 0xFF to Match Any",
                                     0,
                                     generator_id_byte_1_checkout,
                                     generator_id_byte_1_commit,
                                     generator_id_byte_1_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Generator_Id_Byte_1",
                                     "Specify a hex Channel Number or LUN to match or 0xFF to Match Any",
                                     0,
                                     generator_id_byte_2_checkout,
                                     generator_id_byte_2_commit,
                                     generator_id_byte_2_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Sensor_Type",
                                     "Specify a Sensor Type Number or 0xFF to Match Any",
                                     0,
                                     sensor_type_checkout,
                                     sensor_type_commit,
                                     sensor_type_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;
  
  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Sensor_Number",
                                     "Specify a Sensor Number or 0xFF to Match Any",
                                     0,
                                     sensor_number_checkout,
                                     sensor_number_commit,
                                     sensor_number_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Trigger",
                                     "Specify a Event/Reading Type Number or 0xFF to Match Any",
                                     0,
                                     event_trigger_checkout,
                                     event_trigger_commit,
                                     event_trigger_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_1_Offset_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data_1_offset_mask_checkout,
                                     event_data_1_offset_mask_commit,
                                     event_data_1_offset_mask_diff,
                                     number_range_two_bytes) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_1_AND_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data_1_and_mask_checkout,
                                     event_data_1_and_mask_commit,
                                     event_data_1_and_mask_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_1_Compare_1",
                                     "Give a valid number",
                                     0,
                                     event_data_1_compare_1_checkout,
                                     event_data_1_compare_1_commit,
                                     event_data_1_compare_1_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_1_compare_2",
                                     "Give a valid number",
                                     0,
                                     event_data_1_compare_2_checkout,
                                     event_data_1_compare_2_commit,
                                     event_data_1_compare_2_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;
  
  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_2_AND_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data_2_and_mask_checkout,
                                     event_data_2_and_mask_commit,
                                     event_data_2_and_mask_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_2_Compare_1",
                                     "Give a valid number",
                                     0,
                                     event_data_2_compare_1_checkout,
                                     event_data_2_compare_1_commit,
                                     event_data_2_compare_1_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_2_compare_2",
                                     "Give a valid number",
                                     0,
                                     event_data_2_compare_2_checkout,
                                     event_data_2_compare_2_commit,
                                     event_data_2_compare_2_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_3_AND_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data_3_and_mask_checkout,
                                     event_data_3_and_mask_commit,
                                     event_data_3_and_mask_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_3_Compare_1",
                                     "Give a valid number",
                                     0,
                                     event_data_3_compare_1_checkout,
                                     event_data_3_compare_1_commit,
                                     event_data_3_compare_1_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data_3_compare_2",
                                     "Give a valid number",
                                     0,
                                     event_data_3_compare_2_checkout,
                                     event_data_3_compare_2_commit,
                                     event_data_3_compare_2_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  return sect;

 cleanup:
  if (sect)
    ipmi_pef_section_destroy(state_data, sect);
  return NULL;
}
