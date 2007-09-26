#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-utils.h"
#include "pef-config-validate.h"
#include "pef-config-wrapper.h"

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
_config_section_update_keyvalue_output_hex(struct config_keyvalue *kv, 
                                           unsigned int value_output)
{
  char buf[CONFIG_PARSE_BUFLEN];
  
  assert(kv);

  sprintf(buf, "0x%02X", value_output);
  return config_section_update_keyvalue_output(kv, buf);
}

static config_err_t
event_filter_get (pef_config_state_data_t *state_data,
                  uint8_t filter_number,
                  struct event_filter_table *eft)
{
  config_err_t ret;

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
                                                  &eft->event_data3_compare2)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_set (pef_config_state_data_t *state_data,
                  uint8_t filter_number,
                  struct event_filter_table *eft)
{
  config_err_t ret;

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
                                                  eft->event_data3_compare2)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
filter_type_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(kv, filter_type_string (eft.filter_type)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
filter_type_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.filter_type = filter_type_number (kv->value_input);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_filter_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, eft.enable_filter ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_filter_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.enable_filter = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_alert_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, eft.event_filter_action_alert ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_alert_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_alert = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_power_off_checkout (const char *section_name,
                                        struct config_keyvalue *kv,
                                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, eft.event_filter_action_power_off ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_power_off_commit (const char *section_name,
                                      const struct config_keyvalue *kv,
                                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_power_off = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_reset_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, eft.event_filter_action_reset ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_reset_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_reset = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_power_cycle_checkout (const char *section_name,
                                          struct config_keyvalue *kv,
                                          void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, eft.event_filter_action_power_cycle ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_power_cycle_commit (const char *section_name,
                                        const struct config_keyvalue *kv,
                                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_power_cycle = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_oem_checkout (const char *section_name,
                                  struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, eft.event_filter_action_oem ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_oem_commit (const char *section_name,
                                const struct config_keyvalue *kv,
                                void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_oem = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_diagnostic_interrupt_checkout (const char *section_name,
                                                   struct config_keyvalue *kv,
                                                   void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, eft.event_filter_action_diagnostic_interrupt ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_diagnostic_interrupt_commit (const char *section_name,
                                                 const struct config_keyvalue *kv,
                                                 void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_diagnostic_interrupt = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_group_control_operation_checkout (const char *section_name,
                                                      struct config_keyvalue *kv,
                                                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(kv, eft.event_filter_action_group_control_operation ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_action_group_control_operation_commit (const char *section_name,
                                                    const struct config_keyvalue *kv,
                                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_filter_action_group_control_operation = same (kv->value_input, "yes");

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_policy_number_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, eft.alert_policy_number) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_policy_number_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.alert_policy_number = atoi (kv->value_input);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
group_control_selector_checkout (const char *section_name,
                                 struct config_keyvalue *kv,
                                 void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, eft.group_control_selector) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
group_control_selector_commit (const char *section_name,
                               const struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.group_control_selector = atoi (kv->value_input);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_severity_checkout (const char *section_name,
                         struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, event_severity_string (eft.event_severity)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_severity_commit (const char *section_name,
                       const struct config_keyvalue *kv,
                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_severity = event_severity_number (kv->value_input);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
generator_id_byte_1_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.generator_id_byte_1) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
generator_id_byte_1_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.generator_id_byte_1 = strtol(kv->value_input, NULL, 0);
  

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
generator_id_byte_2_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.generator_id_byte_2) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
generator_id_byte_2_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.generator_id_byte_2 = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sensor_type_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, sensor_type_string (eft.sensor_type)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sensor_type_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.sensor_type = sensor_type_number(kv->value_input);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sensor_number_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.sensor_number) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sensor_number_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.sensor_number = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_trigger_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_trigger) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_trigger_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_trigger = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_offset_mask_checkout (const char *section_name,
                                  struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data1_offset_mask) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_offset_mask_commit (const char *section_name,
                                const struct config_keyvalue *kv,
                                void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data1_offset_mask = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_and_mask_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data1_and_mask) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_and_mask_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data1_and_mask = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_compare1_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data1_compare1) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_compare1_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data1_compare1 = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_compare2_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data1_compare2) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data1_compare2_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data1_compare2 = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data2_and_mask_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data2_and_mask) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data2_and_mask_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data2_and_mask = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data2_compare1_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data2_compare1) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data2_compare1_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data2_compare1 = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data2_compare2_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data2_compare2) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data2_compare2_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data2_compare2 = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data3_and_mask_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data3_and_mask) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data3_and_mask_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data3_and_mask = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data3_compare1_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data3_compare1) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data3_compare1_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data3_compare1 = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data3_compare2_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t event_filter_number;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (_config_section_update_keyvalue_output_hex(kv, eft.event_data3_compare2) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_data3_compare2_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  struct event_filter_table eft;

  event_filter_number = atoi (section_name + strlen ("Event_Filter_"));

  if ((ret = event_filter_get (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  eft.event_data3_compare2 = strtol(kv->value_input, NULL, 0);

  if ((ret = event_filter_set (state_data,
                               event_filter_number,
                               &eft)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

struct config_section *
pef_config_event_filter_table_section_get (pef_config_state_data_t *state_data, int num)
{
  struct config_section *section = NULL;
  char buf[64];

  if (num <= 0)
    {
      fprintf(stderr, "Invalid Num = %d\n", num);
      return NULL;
    }

  snprintf(buf, 64, "Event_Filter_%d", num);

  if (!(section = config_section_create (buf, 
                                         NULL, 
                                         NULL, 
                                         0)))
    goto cleanup;

  if (config_section_add_key (section,
                              "Filter_Type",
                              "Possible values: Manufacturer_Pre_Configured/Software_Configurable/Reserved1/Reserved3",
                              0,
                              filter_type_checkout,
                              filter_type_commit,
                              filter_type_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Enable_Filter",
                              "Possible values: Yes/No",
                              0,
                              enable_filter_checkout,
                              enable_filter_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Action_Alert",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_alert_checkout,
                              event_filter_action_alert_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Action_Power_Off",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_power_off_checkout,
                              event_filter_action_power_off_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Action_Reset",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_reset_checkout,
                              event_filter_action_reset_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Action_Power_Cycle",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_power_cycle_checkout,
                              event_filter_action_power_cycle_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Action_Oem",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_oem_checkout,
                              event_filter_action_oem_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Action_Diagnostic_Interrupt",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_diagnostic_interrupt_checkout,
                              event_filter_action_diagnostic_interrupt_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Action_Group_Control_Operation",
                              "Possible values: Yes/No",
                              0,
                              event_filter_action_group_control_operation_checkout,
                              event_filter_action_group_control_operation_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_Policy_Number",
                              "Give a valid number",
                              0,
                              alert_policy_number_checkout,
                              alert_policy_number_commit,
                              config_number_range_four_bits) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Group_Control_Selector",
                              "Give a valid number",
                              0,
                              group_control_selector_checkout,
                              group_control_selector_commit,
                              config_number_range_three_bits) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Severity",
                              "Possible values: Unspecified/Monitor/Information/OK/Non_Critical/Critical/Non_Recoverable",
                              0,
                              event_severity_checkout,
                              event_severity_commit,
                              event_severity_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Generator_Id_Byte_1",
                              "Specify a hex Slave Address or Software ID from Event Message or 0xFF to Match Any",
                              0,
                              generator_id_byte_1_checkout,
                              generator_id_byte_1_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Generator_Id_Byte_1",
                              "Specify a hex Channel Number or LUN to match or 0xFF to Match Any",
                              0,
                              generator_id_byte_2_checkout,
                              generator_id_byte_2_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Sensor_Type",
                              "Specify a Sensor Type, For options see the MAN page",
                              0,
                              sensor_type_checkout,
                              sensor_type_commit,
                              sensor_type_validate) < 0) 
    goto cleanup;
  
  if (config_section_add_key (section,
                              "Sensor_Number",
                              "Specify a Sensor Number or 0xFF to Match Any",
                              0,
                              sensor_number_checkout,
                              sensor_number_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Trigger",
                              "Specify a Event/Reading Type Number or 0xFF to Match Any",
                              0,
                              event_trigger_checkout,
                              event_trigger_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data1_Offset_Mask",
                              "Give a valid number",
                              0,
                              event_data1_offset_mask_checkout,
                              event_data1_offset_mask_commit,
                              config_number_range_two_bytes) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data1_AND_Mask",
                              "Give a valid number",
                              0,
                              event_data1_and_mask_checkout,
                              event_data1_and_mask_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data1_Compare1",
                              "Give a valid number",
                              0,
                              event_data1_compare1_checkout,
                              event_data1_compare1_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data1_Compare2",
                              "Give a valid number",
                              0,
                              event_data1_compare2_checkout,
                              event_data1_compare2_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;
  
  if (config_section_add_key (section,
                              "Event_Data2_AND_Mask",
                              "Give a valid number",
                              0,
                              event_data2_and_mask_checkout,
                              event_data2_and_mask_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data2_Compare1",
                              "Give a valid number",
                              0,
                              event_data2_compare1_checkout,
                              event_data2_compare1_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data2_Compare2",
                              "Give a valid number",
                              0,
                              event_data2_compare2_checkout,
                              event_data2_compare2_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data3_AND_Mask",
                              "Give a valid number",
                              0,
                              event_data3_and_mask_checkout,
                              event_data3_and_mask_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data3_Compare1",
                              "Give a valid number",
                              0,
                              event_data3_compare1_checkout,
                              event_data3_compare1_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Data3_Compare2",
                              "Give a valid number",
                              0,
                              event_data3_compare2_checkout,
                              event_data3_compare2_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  return section;

 cleanup:
  if (section)
    config_section_destroy(section);
  return NULL;
}
