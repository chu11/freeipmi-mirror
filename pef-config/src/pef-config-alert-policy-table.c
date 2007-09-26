#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-utils.h"
#include "pef-config-validate.h"
#include "pef-config-wrapper.h"

static config_err_t
alert_policy_get (pef_config_state_data_t *state_data,
                  uint8_t alert_policy_entry_number,
                  uint8_t *policy_type,
                  uint8_t *policy_enabled,
                  uint8_t *policy_number,
                  uint8_t *destination_selector,
                  uint8_t *channel_number,
                  uint8_t *alert_string_set_selector,
                  uint8_t *event_specific_alert_string)
{
  uint8_t tmp_policy_type;
  uint8_t tmp_policy_enabled;
  uint8_t tmp_policy_number;
  uint8_t tmp_destination_selector;
  uint8_t tmp_channel_number;
  uint8_t tmp_alert_string_set_selector;
  uint8_t tmp_event_specific_alert_string;
  config_err_t ret;

  if ((ret = get_bmc_pef_conf_alert_policy_table (state_data,
                                                  alert_policy_entry_number,
                                                  &tmp_policy_type,
                                                  &tmp_policy_enabled,
                                                  &tmp_policy_number,
                                                  &tmp_destination_selector,
                                                  &tmp_channel_number,
                                                  &tmp_alert_string_set_selector,
                                                  &tmp_event_specific_alert_string)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (policy_type)
    *policy_type = tmp_policy_type;
  if (policy_enabled)
    *policy_enabled = tmp_policy_enabled;
  if (policy_number)
    *policy_number = tmp_policy_number;
  if (destination_selector)
    *destination_selector = tmp_destination_selector;
  if (channel_number)
    *channel_number = tmp_channel_number;
  if (alert_string_set_selector)
    *alert_string_set_selector = tmp_alert_string_set_selector;
  if (event_specific_alert_string)
    *event_specific_alert_string = tmp_event_specific_alert_string;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_policy_set (pef_config_state_data_t *state_data,
                  uint8_t alert_policy_entry_number,
                  uint8_t policy_type,
                  uint8_t policy_type_is_set,
                  uint8_t policy_enabled,
                  uint8_t policy_enabled_is_set,
                  uint8_t policy_number,
                  uint8_t policy_number_is_set,
                  uint8_t destination_selector,
                  uint8_t destination_selector_is_set,
                  uint8_t channel_number,
                  uint8_t channel_number_is_set,
                  uint8_t alert_string_set_selector,
                  uint8_t alert_string_set_selector_is_set,
                  uint8_t event_specific_alert_string,
                  uint8_t event_specific_alert_string_is_set)
{
  uint8_t tmp_policy_type;
  uint8_t tmp_policy_enabled;
  uint8_t tmp_policy_number;
  uint8_t tmp_destination_selector;
  uint8_t tmp_channel_number;
  uint8_t tmp_alert_string_set_selector;
  uint8_t tmp_event_specific_alert_string;
  config_err_t ret;

  if ((ret = get_bmc_pef_conf_alert_policy_table (state_data,
                                                  alert_policy_entry_number,
                                                  &tmp_policy_type,
                                                  &tmp_policy_enabled,
                                                  &tmp_policy_number,
                                                  &tmp_destination_selector,
                                                  &tmp_channel_number,
                                                  &tmp_alert_string_set_selector,
                                                  &tmp_event_specific_alert_string)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (policy_type_is_set)
    tmp_policy_type = policy_type;
  if (policy_enabled_is_set)
    tmp_policy_enabled = policy_enabled;
  if (policy_number_is_set)
    tmp_policy_number = policy_number;
  if (destination_selector_is_set)
    tmp_destination_selector = destination_selector;
  if (channel_number_is_set)
    tmp_channel_number = channel_number;
  if (alert_string_set_selector_is_set)
    tmp_alert_string_set_selector = alert_string_set_selector;
  if (event_specific_alert_string_is_set)
    tmp_event_specific_alert_string = event_specific_alert_string;

  if ((ret = set_bmc_pef_conf_alert_policy_table (state_data,
                                                  alert_policy_entry_number,
                                                  tmp_policy_type,
                                                  tmp_policy_enabled,
                                                  tmp_policy_number,
                                                  tmp_destination_selector,
                                                  tmp_channel_number,
                                                  tmp_alert_string_set_selector,
                                                  tmp_event_specific_alert_string)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
policy_type_checkout (const char *section_name,
                      struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t policy_type;
  config_err_t ret;
  uint8_t alert_policy_entry_number;
  
  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));
  
  if ((ret = alert_policy_get (state_data,
                               alert_policy_entry_number,
                               &policy_type,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (policy_type_string (policy_type))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
policy_type_commit (const char *section_name,
                    const struct config_keyvalue *kv,
                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_policy_entry_number;

  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));

  return alert_policy_set (state_data,
                           alert_policy_entry_number,
                           policy_type_number (kv->value_input), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static config_err_t
policy_enabled_checkout (const char *section_name,
                         struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t policy_enabled;
  config_err_t ret;
  uint8_t alert_policy_entry_number;
  
  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));
  
  if ((ret = alert_policy_get (state_data,
                               alert_policy_entry_number,
                               NULL,
                               &policy_enabled,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (policy_enabled ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
policy_enabled_commit (const char *section_name,
                       const struct config_keyvalue *kv,
                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_policy_entry_number;

  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));

  return alert_policy_set (state_data,
                           alert_policy_entry_number,
                           0, 0,
                           same (kv->value_input, "yes"), 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static config_err_t
policy_number_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t policy_number;
  config_err_t ret;
  uint8_t alert_policy_entry_number;
  
  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));
  
  if ((ret = alert_policy_get (state_data,
                               alert_policy_entry_number,
                               NULL,
                               NULL,
                               &policy_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%u", policy_number) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
policy_number_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_policy_entry_number;
  uint8_t policy_number;

  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));

  policy_number = atoi (kv->value_input);

  return alert_policy_set (state_data,
                           alert_policy_entry_number,
                           0, 0,
                           0, 0,
                           policy_number, 1,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0);
}

static config_err_t
destination_selector_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;
  config_err_t ret;
  uint8_t alert_policy_entry_number;
  
  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));
  
  if ((ret = alert_policy_get (state_data,
                               alert_policy_entry_number,
                               NULL,
                               NULL,
                               NULL,
                               &destination_selector,
                               NULL,
                               NULL,
                               NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%u", destination_selector) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
destination_selector_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_policy_entry_number;
  uint8_t destination_selector;

  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));

  destination_selector = atoi (kv->value_input);

  return alert_policy_set (state_data,
                           alert_policy_entry_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           destination_selector, 1,
                           0, 0,
                           0, 0,
                           0, 0);
}

static config_err_t
channel_number_checkout (const char *section_name,
                         struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t channel_number;
  config_err_t ret;
  uint8_t alert_policy_entry_number;
  
  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));
  
  if ((ret = alert_policy_get (state_data,
                               alert_policy_entry_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &channel_number,
                               NULL,
                               NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%u", channel_number) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
channel_number_commit (const char *section_name,
                       const struct config_keyvalue *kv,
                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_policy_entry_number;
  uint8_t channel_number;

  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));

  channel_number = atoi (kv->value_input);

  return alert_policy_set (state_data,
                           alert_policy_entry_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           channel_number, 1,
                           0, 0,
                           0, 0);
}

static config_err_t
alert_string_set_selector_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_string_set_selector;
  config_err_t ret;
  uint8_t alert_policy_entry_number;
  
  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));
  
  if ((ret = alert_policy_get (state_data,
                               alert_policy_entry_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &alert_string_set_selector,
                               NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%u", alert_string_set_selector) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_string_set_selector_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_policy_entry_number;
  uint8_t alert_string_set_selector;

  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));

  alert_string_set_selector = atoi (kv->value_input);

  return alert_policy_set (state_data,
                           alert_policy_entry_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           alert_string_set_selector, 1,
                           0, 0);
}

static config_err_t
event_specific_alert_string_checkout (const char *section_name,
                                      struct config_keyvalue *kv,
                                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_specific_alert_string;
  config_err_t ret;
  uint8_t alert_policy_entry_number;
  
  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));
  
  if ((ret = alert_policy_get (state_data,
                               alert_policy_entry_number,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               &event_specific_alert_string)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (event_specific_alert_string ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_specific_alert_string_commit (const char *section_name,
                                    const struct config_keyvalue *kv,
                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_policy_entry_number;

  alert_policy_entry_number = atoi (section_name + strlen ("Alert_Policy_"));

  return alert_policy_set (state_data,
                           alert_policy_entry_number,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           0, 0,
                           same (kv->value_input, "yes"), 1);
}

struct config_section *
pef_config_alert_policy_table_section_get (pef_config_state_data_t *state_data, int num)
{
  struct config_section *section = NULL;
  uint8_t lan_channel_number;
  char *strp = NULL;
  config_err_t ret;
  char buf[64];

  if (num <= 0)
    {
      fprintf(stderr, "Invalid Num = %d\n", num);
      return NULL;
    }

  snprintf(buf, 64, "Alert_Policy_%d", num);

  if (!(section = config_section_create (buf, 
                                         NULL, 
                                         NULL, 
                                         0)))
    goto cleanup;

  if (config_section_add_keyvalue (section,
                                   "Policy_Type",
                                   "Possible values: Always_Send_To_This_Destination/Proceed_To_Next_Entry/Do_Not_Proceed_Any_More_Entries/Proceed_To_Next_Entry_Different_Channel/Proceed_To_Next_Entry_Different_Destination_Type",
                                   0,
                                   policy_type_checkout,
                                   policy_type_commit,
                                   policy_type_validate) < 0) 
    goto cleanup;

  if (config_section_add_keyvalue (section,
                                   "Policy_Enabled",
                                   "Possible values: Yes/No",
                                   0,
                                   policy_enabled_checkout,
                                   policy_enabled_commit,
                                   config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_keyvalue (section,
                                   "Policy_Number",
                                   "Give a valid number",
                                   0,
                                   policy_number_checkout,
                                   policy_number_commit,
                                   config_number_range_four_bits) < 0) 
    goto cleanup;

  if (config_section_add_keyvalue (section,
                                   "Destination_Selector",
                                   "Give a valid number",
                                   0,
                                   destination_selector_checkout,
                                   destination_selector_commit,
                                   config_number_range_four_bits) < 0) 
    goto cleanup;

  ret = get_lan_channel_number (state_data, &lan_channel_number);
  if (ret == CONFIG_ERR_SUCCESS)
    asprintf(&strp, "Give a valid number (LAN = %u)", lan_channel_number);
  if (!strp)
    {
      if (!(strp = "Give a valid number\n"))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  if (config_section_add_keyvalue (section,
                                   "Channel_Number",
                                   strp,
                                   0,
                                   channel_number_checkout,
                                   channel_number_commit,
                                   config_number_range_four_bits) < 0) 
    goto cleanup;

  if (config_section_add_keyvalue (section,
                                   "Alert_String_Set_Selector",
                                   "Give a valid number",
                                   0,
                                   alert_string_set_selector_checkout,
                                   alert_string_set_selector_commit,
                                   config_number_range_seven_bits) < 0) 
    goto cleanup;

  if (config_section_add_keyvalue (section,
                                   "Event_Specific_Alert_String",
                                   "Possible values: Yes/No",
                                   0,
                                   event_specific_alert_string_checkout,
                                   event_specific_alert_string_commit,
                                   config_yes_no_validate) < 0) 
    goto cleanup;

  if (strp)
    free(strp);
  return section;

 cleanup:
  if (strp)
    free(strp);
  if (section)
    config_section_destroy(section);
  return NULL;
}

