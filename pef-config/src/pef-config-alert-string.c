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

/* achu: presumably there is no maximum.  We could read/write blocks
   forever based on block numbers.  However, we need to have some
   artificial max for the sake of pef-config.
*/
#define PEF_ALERT_STRING_MAX_LEN 64

static config_err_t
string_keys_get (pef_config_state_data_t *state_data,
                 uint8_t string_selector,
                 uint8_t *event_filter_number,
                 uint8_t *alert_string_set)
{
  uint8_t tmp_event_filter_number;
  uint8_t tmp_alert_string_set;
  config_err_t ret;

  if ((ret = get_pef_alert_string_keys (state_data,
                                        string_selector,
                                        &tmp_event_filter_number,
                                        &tmp_alert_string_set)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (event_filter_number)
    *event_filter_number = tmp_event_filter_number;
  if (alert_string_set)
    *alert_string_set = tmp_alert_string_set;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
string_keys_set (pef_config_state_data_t *state_data,
                 uint8_t string_selector,
                 uint8_t event_filter_number,
                 uint8_t event_filter_number_is_set,
                 uint8_t alert_string_set,
                 uint8_t alert_string_set_is_set)
{
  uint8_t tmp_event_filter_number;
  uint8_t tmp_alert_string_set;
  config_err_t ret;

  if ((ret = get_pef_alert_string_keys(state_data,
                                       string_selector,
                                       &tmp_event_filter_number,
                                       &tmp_alert_string_set)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (event_filter_number_is_set)
    tmp_event_filter_number = event_filter_number;
  if (alert_string_set_is_set)
    tmp_alert_string_set = alert_string_set;

  if ((ret = set_pef_alert_string_keys(state_data,
                                       string_selector,
                                       tmp_event_filter_number,
                                       tmp_alert_string_set)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_number_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  uint8_t string_selector;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  if ((ret = string_keys_get (state_data,
                              string_selector,
                              &event_filter_number,
                              NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, event_filter_number) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_number_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t string_selector;
  uint8_t event_filter_number;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  event_filter_number = atoi (kv->value_input);

  return string_keys_set (state_data,
                          string_selector,
                          event_filter_number, 1,
                          0, 0);
}

static config_err_t
alert_string_set_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_string_set;
  config_err_t ret;
  uint8_t string_selector;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  if ((ret = string_keys_get (state_data,
                              string_selector,
                              NULL,
                              &alert_string_set)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, alert_string_set) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_string_set_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t string_selector;
  uint8_t alert_string_set;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  alert_string_set = atoi (kv->value_input);

  return string_keys_set (state_data,
                          string_selector,
                          0, 0,
                          alert_string_set, 1);
}

static config_err_t
alert_string_checkout (const char *section_name,
                       struct config_keyvalue *kv,
                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_string[PEF_ALERT_STRING_MAX_LEN+1] = { 0, };
  config_err_t ret;
  uint8_t string_selector;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  if ((ret = get_pef_alert_string (state_data,
                                   string_selector,
                                   alert_string,
                                   PEF_ALERT_STRING_MAX_LEN+1)) != CONFIG_ERR_SUCCESS) 
    return ret;
		    
  if (config_section_update_keyvalue_output(kv, (char *)alert_string) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_string_commit (const char *section_name,
                     const struct config_keyvalue *kv,
                     void *arg)
{ 
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t string_selector;

  string_selector = atoi (section_name + strlen ("Alert_String_"));

  return set_pef_alert_string (state_data,
                               string_selector,
                               (uint8_t *)kv->value_input);
}

static config_validate_t
alert_string_validate (const char *section_name,
                       const char *key_name,
                       const char *value)
{
  if (strlen (value) <= PEF_ALERT_STRING_MAX_LEN)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

struct config_section *
pef_config_alert_string_section_get (pef_config_state_data_t *state_data, int num)
{
  struct config_section *section = NULL;
  char buf[64];

  if (num <= 0)
    {
      fprintf(stderr, "Invalid Num = %d\n", num);
      return NULL;
    }

  snprintf(buf, 64, "Alert_String_%d", num);

  if (!(section = config_section_create (buf, 
                                         NULL, 
                                         NULL, 
                                         0)))
    goto cleanup;

  if (config_section_add_key (section,
                              "Event_Filter_Number",
                              "Give valid number",
                              0,
                              event_filter_number_checkout,
                              event_filter_number_commit,
                              config_number_range_seven_bits) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_String_Set",
                              "Give valid number",
                              0,
                              alert_string_set_checkout,
                              alert_string_set_commit,
                              config_number_range_seven_bits) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_String",
                              "Give string. Max 64 chars.",
                              0,
                              alert_string_checkout,
                              alert_string_commit,
                              alert_string_validate) < 0) 
    goto cleanup;

  return section;

 cleanup:
  if (section)
    config_section_destroy(section);
  return NULL;
}

