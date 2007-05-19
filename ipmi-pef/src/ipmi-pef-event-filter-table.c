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

static pef_err_t
event_filter_get (ipmi_pef_state_data_t *state_data,
                  uint8_t filter_number,
                  struct event_filter_table *eft)
{
  pef_err_t ret;

  if ((ret = get_bmc_pef_conf_event_filter_table (state_data,
                                                  filter_number,
                                                  &eft->filter_type,
                                                  &eft->enable_filter,
                                                  &eft->event_filter_action_alert,
                                                  &eft->event_filter_action_power_off,
                                                  &eft->event_filter_action_reset,
                                                  &eft->event_filter_action_power_cycle,
                                                  &eft->event_filter_action_oem,
                                                  &eft->event_filter_action_diagnostic_interrupt,
                                                  &eft->event_filter_action_group_control_operation,
                                                  &eft->alert_policy_number,
                                                  &eft->group_control_selector,
                                                  &eft->event_severity,
                                                  &eft->generator_id_byte_1,
                                                  &eft->generator_id_byte_2,
                                                  &eft->sensor_type,
                                                  &eft->sensor_number,
                                                  &eft->event_trigger,
                                                  &eft->event_data1_offset_mask,
                                                  &eft->event_data1_and_mask,
                                                  &eft->event_data1_compare1,
                                                  &eft->event_data1_compare2,
                                                  &eft->event_data2_and_mask,
                                                  &eft->event_data2_compare1,
                                                  &eft->event_data2_compare2,
                                                  &eft->event_data3_and_mask,
                                                  &eft->event_data3_compare1,
                                                  &eft->event_data3_compare2)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_filter_set (ipmi_pef_state_data_t *state_data,
                  uint8_t filter_number,
                  struct event_filter_table *eft)
{
  pef_err_t ret;

  if ((ret = set_bmc_pef_conf_event_filter_table (state_data,
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
                                                  eft->event_data3_compare2)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_err_t
filter_type_checkout (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;
  
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (filter_type_string (eft.filter_type))))
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.filter_type = filter_type_number (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
filter_type_diff (ipmi_pef_state_data_t *state_data,
                  const struct section *sect,
                  const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = filter_type_number (kv->value);
  if (passed_val == eft.filter_type)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   filter_type_string (eft.filter_type));
    }
  return ret;
}

static pef_err_t
enable_filter_checkout (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.enable_filter)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.enable_filter = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
enable_filter_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.enable_filter)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.enable_filter ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
event_filter_action_alert_checkout (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.event_filter_action_alert)
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
event_filter_action_alert_commit (ipmi_pef_state_data_t *state_data,
                                   const struct section *sect,
                                   const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_alert = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_filter_action_alert_diff (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.event_filter_action_alert)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.event_filter_action_alert ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
event_filter_action_power_off_checkout (ipmi_pef_state_data_t *state_data,
                                         const struct section *sect,
                                         struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.event_filter_action_power_off)
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
event_filter_action_power_off_commit (ipmi_pef_state_data_t *state_data,
                                       const struct section *sect,
                                       const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_power_off = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_filter_action_power_off_diff (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.event_filter_action_power_off)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.event_filter_action_power_off ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
event_filter_action_reset_checkout (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.event_filter_action_reset)
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
event_filter_action_reset_commit (ipmi_pef_state_data_t *state_data,
                                   const struct section *sect,
                                   const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_reset = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_filter_action_reset_diff (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.event_filter_action_reset)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.event_filter_action_reset ? "Yes" : "No");
    }
  return ret;
}


static pef_err_t
event_filter_action_power_cycle_checkout (ipmi_pef_state_data_t *state_data,
                                           const struct section *sect,
                                           struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.event_filter_action_power_cycle)
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
event_filter_action_power_cycle_commit (ipmi_pef_state_data_t *state_data,
                                         const struct section *sect,
                                         const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_power_cycle = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_filter_action_power_cycle_diff (ipmi_pef_state_data_t *state_data,
                                       const struct section *sect,
                                       const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.event_filter_action_power_cycle)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.event_filter_action_power_cycle ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
event_filter_action_oem_checkout (ipmi_pef_state_data_t *state_data,
                                   const struct section *sect,
                                   struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.event_filter_action_oem)
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
event_filter_action_oem_commit (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_oem = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_filter_action_oem_diff (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.event_filter_action_oem)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.event_filter_action_oem ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
event_filter_action_diagnostic_interrupt_checkout (ipmi_pef_state_data_t *state_data,
                                                    const struct section *sect,
                                                    struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.event_filter_action_diagnostic_interrupt)
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
event_filter_action_diagnostic_interrupt_commit (ipmi_pef_state_data_t *state_data,
                                                  const struct section *sect,
                                                  const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_diagnostic_interrupt = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_filter_action_diagnostic_interrupt_diff (ipmi_pef_state_data_t *state_data,
                                                const struct section *sect,
                                                const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.event_filter_action_diagnostic_interrupt)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.event_filter_action_diagnostic_interrupt ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
event_filter_action_group_control_operation_checkout (ipmi_pef_state_data_t *state_data,
                                                       const struct section *sect,
                                                       struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (eft.event_filter_action_group_control_operation)
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
event_filter_action_group_control_operation_commit (ipmi_pef_state_data_t *state_data,
                                                     const struct section *sect,
                                                     const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_group_control_operation = same (kv->value, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_filter_action_group_control_operation_diff (ipmi_pef_state_data_t *state_data,
                                                   const struct section *sect,
                                                   const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == eft.event_filter_action_group_control_operation)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   eft.event_filter_action_group_control_operation ? "Yes" : "No");
    }
  return ret;
}

static pef_err_t
alert_policy_number_checkout (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%u", eft.alert_policy_number) < 0)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.alert_policy_number = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
alert_policy_number_diff (ipmi_pef_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.alert_policy_number)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "%u", eft.alert_policy_number);
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
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%u", eft.group_control_selector) < 0)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.group_control_selector = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
group_control_selector_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.group_control_selector)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "%u", eft.group_control_selector);
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
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (event_severity_string (eft.event_severity))))
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_severity = event_severity_number (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_severity_diff (ipmi_pef_state_data_t *state_data,
                     const struct section *sect,
                     const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = event_severity_number (kv->value);
  if (passed_val == eft.event_severity)
    ret = PEF_DIFF_SAME;
  else
    {
      ret = PEF_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   event_severity_string (eft.event_severity));
    }
  return ret;
}

static pef_err_t
generator_id_byte_1_checkout (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.generator_id_byte_1) < 0)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.generator_id_byte_1 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
generator_id_byte_1_diff (ipmi_pef_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.generator_id_byte_1)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.generator_id_byte_1);
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
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.generator_id_byte_2) < 0)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.generator_id_byte_2 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
generator_id_byte_2_diff (ipmi_pef_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.generator_id_byte_2)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.generator_id_byte_2);
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
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.sensor_type) < 0)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.sensor_type = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
sensor_type_diff (ipmi_pef_state_data_t *state_data,
                  const struct section *sect,
                  const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.sensor_type)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.sensor_type);
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
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.sensor_number) < 0)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.sensor_number = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
sensor_number_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.sensor_number)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.sensor_number);
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
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_trigger) < 0)
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
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_trigger = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_trigger_diff (ipmi_pef_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_trigger)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_trigger);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data1_offset_mask_checkout (ipmi_pef_state_data_t *state_data,
                                   const struct section *sect,
                                   struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data1_offset_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data1_offset_mask_commit (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data1_offset_mask = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data1_offset_mask_diff (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint16_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data1_offset_mask)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data1_offset_mask);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data1_and_mask_checkout (ipmi_pef_state_data_t *state_data,
                                const struct section *sect,
                                struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data1_and_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data1_and_mask_commit (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data1_and_mask = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data1_and_mask_diff (ipmi_pef_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data1_and_mask)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data1_and_mask);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data1_compare1_checkout (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data1_compare1) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data1_compare1_commit (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data1_compare1 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data1_compare1_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data1_compare1)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data1_compare1);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data1_compare2_checkout (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data1_compare2) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data1_compare2_commit (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data1_compare2 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data1_compare2_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data1_compare2)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data1_compare2);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data2_and_mask_checkout (ipmi_pef_state_data_t *state_data,
                                const struct section *sect,
                                struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data2_and_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data2_and_mask_commit (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data2_and_mask = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data2_and_mask_diff (ipmi_pef_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data2_and_mask)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data2_and_mask);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data2_compare1_checkout (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data2_compare1) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data2_compare1_commit (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data2_compare1 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data2_compare1_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data2_compare1)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data2_compare1);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data2_compare2_checkout (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data2_compare2) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data2_compare2_commit (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data2_compare2 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data2_compare2_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data2_compare2)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data2_compare2);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data3_and_mask_checkout (ipmi_pef_state_data_t *state_data,
                                const struct section *sect,
                                struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data3_and_mask) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data3_and_mask_commit (ipmi_pef_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data3_and_mask = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data3_and_mask_diff (ipmi_pef_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data3_and_mask)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data3_and_mask);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data3_compare1_checkout (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data3_compare1) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data3_compare1_commit (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data3_compare1 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data3_compare1_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data3_compare1)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data3_compare1);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static pef_err_t
event_data3_compare2_checkout (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  pef_err_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "0x%02X", eft.event_data3_compare2) < 0)
    {
      perror("asprintf");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
event_data3_compare2_commit (ipmi_pef_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  pef_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((ret = get_number_of_event_filters (state_data,
                                          &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return ret;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  eft.event_data3_compare2 = atoi (kv->value);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != PEF_ERR_SUCCESS)
    return ret;

  return PEF_ERR_SUCCESS;
}

static pef_diff_t
event_data3_compare2_diff (ipmi_pef_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  pef_err_t rc;
  pef_diff_t ret;
  uint8_t event_filter_number;
  uint8_t number_of_event_filters;
  struct event_filter_table eft;

  event_filter_number = atoi (sect->section_name + strlen ("Event_Filter_"));

  if ((rc = get_number_of_event_filters (state_data,
                                         &number_of_event_filters)) != PEF_ERR_SUCCESS)
    return rc;

  if (event_filter_number > number_of_event_filters)
    return PEF_ERR_NON_FATAL_ERROR;

  if ((rc = event_filter_get (state_data,
                              event_filter_number,
                              &eft)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == eft.event_data3_compare2)
    ret = PEF_DIFF_SAME;
  else
    {
      char num[32];
      ret = PEF_DIFF_DIFFERENT;
      sprintf (num, "0x%02X", eft.event_data3_compare2);
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
                                     "Event_Filter_Action_Alert",
                                     "Possible values: Yes/No",
                                     0,
                                     event_filter_action_alert_checkout,
                                     event_filter_action_alert_commit,
                                     event_filter_action_alert_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Filter_Action_Power_Off",
                                     "Possible values: Yes/No",
                                     0,
                                     event_filter_action_power_off_checkout,
                                     event_filter_action_power_off_commit,
                                     event_filter_action_power_off_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Filter_Action_Reset",
                                     "Possible values: Yes/No",
                                     0,
                                     event_filter_action_reset_checkout,
                                     event_filter_action_reset_commit,
                                     event_filter_action_reset_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Filter_Action_Power_Cycle",
                                     "Possible values: Yes/No",
                                     0,
                                     event_filter_action_power_cycle_checkout,
                                     event_filter_action_power_cycle_commit,
                                     event_filter_action_power_cycle_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Filter_Action_Oem",
                                     "Possible values: Yes/No",
                                     0,
                                     event_filter_action_oem_checkout,
                                     event_filter_action_oem_commit,
                                     event_filter_action_oem_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Filter_Action_Diagnostic_Interrupt",
                                     "Possible values: Yes/No",
                                     0,
                                     event_filter_action_diagnostic_interrupt_checkout,
                                     event_filter_action_diagnostic_interrupt_commit,
                                     event_filter_action_diagnostic_interrupt_diff,
                                     yes_no_validate) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Filter_Action_Group_Control_Operation",
                                     "Possible values: Yes/No",
                                     0,
                                     event_filter_action_group_control_operation_checkout,
                                     event_filter_action_group_control_operation_commit,
                                     event_filter_action_group_control_operation_diff,
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
                                     "Event_Data1_Offset_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data1_offset_mask_checkout,
                                     event_data1_offset_mask_commit,
                                     event_data1_offset_mask_diff,
                                     number_range_two_bytes) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data1_AND_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data1_and_mask_checkout,
                                     event_data1_and_mask_commit,
                                     event_data1_and_mask_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data1_Compare1",
                                     "Give a valid number",
                                     0,
                                     event_data1_compare1_checkout,
                                     event_data1_compare1_commit,
                                     event_data1_compare1_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data1_Compare2",
                                     "Give a valid number",
                                     0,
                                     event_data1_compare2_checkout,
                                     event_data1_compare2_commit,
                                     event_data1_compare2_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;
  
  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data2_AND_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data2_and_mask_checkout,
                                     event_data2_and_mask_commit,
                                     event_data2_and_mask_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data2_Compare1",
                                     "Give a valid number",
                                     0,
                                     event_data2_compare1_checkout,
                                     event_data2_compare1_commit,
                                     event_data2_compare1_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data2_Compare2",
                                     "Give a valid number",
                                     0,
                                     event_data2_compare2_checkout,
                                     event_data2_compare2_commit,
                                     event_data2_compare2_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data3_AND_Mask",
                                     "Give a valid number",
                                     0,
                                     event_data3_and_mask_checkout,
                                     event_data3_and_mask_commit,
                                     event_data3_and_mask_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data3_Compare1",
                                     "Give a valid number",
                                     0,
                                     event_data3_compare1_checkout,
                                     event_data3_compare1_commit,
                                     event_data3_compare1_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Event_Data3_Compare2",
                                     "Give a valid number",
                                     0,
                                     event_data3_compare2_checkout,
                                     event_data3_compare2_commit,
                                     event_data3_compare2_diff,
                                     number_range_one_byte) < 0) 
    goto cleanup;

  return sect;

 cleanup:
  if (sect)
    ipmi_pef_section_destroy(state_data, sect);
  return NULL;
}
