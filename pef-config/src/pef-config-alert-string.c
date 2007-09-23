#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "pef-config.h"
#include "pef-config-common.h"
#include "pef-config-diff.h"
#include "pef-config-map.h"
#include "pef-config-sections.h"
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
event_filter_number_checkout (const struct config_section *section,
                              struct config_keyvalue *kv,
                              void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t event_filter_number;
  config_err_t ret;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((ret = get_number_of_alert_strings (state_data,
                                          &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  if ((ret = string_keys_get (state_data,
                              string_selector,
                              &event_filter_number,
                              NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value, "%u", event_filter_number) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
event_filter_number_commit (const struct config_section *section,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;
  config_err_t ret;
  uint8_t event_filter_number;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((ret = get_number_of_alert_strings (state_data,
                                          &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  event_filter_number = atoi (kv->value);

  return string_keys_set (state_data,
                          string_selector,
                          event_filter_number, 1,
                          0, 0);
}

static config_diff_t
event_filter_number_diff (const struct config_section *section,
                          const struct config_keyvalue *kv,
                          void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((rc = get_number_of_alert_strings (state_data,
                                         &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return rc;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  if ((rc = string_keys_get (state_data,
                             string_selector,
                             &get_val,
                             NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else
    {
      char num[32];
      ret = CONFIG_DIFF_DIFFERENT;
      sprintf (num, "%u", get_val);
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   num);
    }
  return ret;
}

static config_err_t
alert_string_set_checkout (const struct config_section *section,
                           struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_string_set;
  config_err_t ret;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((ret = get_number_of_alert_strings (state_data,
                                          &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  if ((ret = string_keys_get (state_data,
                              string_selector,
                              NULL,
                              &alert_string_set)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value, "%u", alert_string_set) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_string_set_commit (const struct config_section *section,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;
  config_err_t ret;
  uint8_t alert_string_set;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((ret = get_number_of_alert_strings (state_data,
                                          &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  alert_string_set = atoi (kv->value);

  return string_keys_set (state_data,
                          string_selector,
                          0, 0,
                          alert_string_set, 1);
}

static config_diff_t
alert_string_set_diff (const struct config_section *section,
                       const struct config_keyvalue *kv,
                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((rc = get_number_of_alert_strings (state_data,
                                         &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return rc;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  if ((rc = string_keys_get (state_data,
                             string_selector,
                             NULL,
                             &get_val)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else
    {
      char num[32];
      ret = CONFIG_DIFF_DIFFERENT;
      sprintf (num, "%u", get_val);
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   num);
    }
  return ret;
}

static config_err_t
alert_string_checkout (const struct config_section *section,
                       struct config_keyvalue *kv,
                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_string[PEF_ALERT_STRING_MAX_LEN+1] = { 0, };
  config_err_t ret;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((ret = get_number_of_alert_strings (state_data,
                                          &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  if ((ret = get_pef_alert_string (state_data,
                                   string_selector,
                                   alert_string,
                                   PEF_ALERT_STRING_MAX_LEN+1)) != CONFIG_ERR_SUCCESS) 
    return ret;
		    
  if (!(kv->value = strdup ((char *)alert_string)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_string_commit (const struct config_section *section,
                     const struct config_keyvalue *kv,
                     void *arg)
{ 
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((ret = get_number_of_alert_strings (state_data,
                                          &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;

  if (!kv->value)
    return CONFIG_ERR_FATAL_ERROR;

  return set_pef_alert_string (state_data,
                               string_selector,
                               (uint8_t *)kv->value);
}

static config_diff_t
alert_string_diff (const struct config_section *section,
                   const struct config_keyvalue *kv,
                   void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_string[PEF_ALERT_STRING_MAX_LEN+1] = { 0, };
  config_err_t rc;
  config_diff_t ret;
  uint8_t string_selector;
  uint8_t number_of_alert_strings;

  string_selector = atoi (section->section_name + strlen ("Alert_String_"));

  if ((ret = get_number_of_alert_strings (state_data,
                                          &number_of_alert_strings)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (string_selector > number_of_alert_strings)
    return CONFIG_ERR_NON_FATAL_ERROR;
  
  if ((rc = get_pef_alert_string (state_data,
                                  string_selector,
                                  alert_string,
                                  PEF_ALERT_STRING_MAX_LEN+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }
  
  if (!kv->value || !same (kv->value, (char *)alert_string))
    ret = CONFIG_DIFF_DIFFERENT;
  else
    ret = CONFIG_DIFF_SAME;

  if (ret == CONFIG_DIFF_DIFFERENT)
    report_diff (section->section_name,
		 kv->key_name,
		 kv->value,
		 (char *)alert_string);
  return ret;
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

  if (!(section = pef_config_section_create (state_data, 
                                          buf, 
                                          NULL, 
                                          NULL, 
                                          0)))
    goto cleanup;

  if (pef_config_section_add_keyvalue (state_data,
                                       section,
                                       "Event_Filter_Number",
                                       "Give valid number",
                                       0,
                                       event_filter_number_checkout,
                                       event_filter_number_commit,
                                       event_filter_number_diff,
                                       config_number_range_seven_bits) < 0) 
    goto cleanup;

  if (pef_config_section_add_keyvalue (state_data,
                                       section,
                                       "Alert_String_Set",
                                       "Give valid number",
                                       0,
                                       alert_string_set_checkout,
                                       alert_string_set_commit,
                                       alert_string_set_diff,
                                       config_number_range_seven_bits) < 0) 
    goto cleanup;

  if (pef_config_section_add_keyvalue (state_data,
                                       section,
                                       "Alert_String",
                                       "Give string. Max 64 chars.",
                                       0,
                                       alert_string_checkout,
                                       alert_string_commit,
                                       alert_string_diff,
                                       alert_string_validate) < 0) 
    goto cleanup;

  return section;

 cleanup:
  if (section)
    pef_config_section_destroy(section);
  return NULL;
}

