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
#include "pef-config-wrapper.h"

static config_err_t
community_string_checkout (const struct config_section *section,
                           struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  config_err_t ret;

  if ((ret = get_bmc_community_string (state_data,
                                       community_string,
                                       IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != CONFIG_ERR_SUCCESS) 
    return ret;
		    
  if (!(kv->value = strdup ((char *)community_string)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
community_string_commit (const struct config_section *section,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return set_bmc_community_string (state_data,
                                   kv->value);
}

static config_diff_t
community_string_diff (const struct config_section *section,
                       const struct config_keyvalue *kv,
                       void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_community_string (state_data,
                                      community_string,
                                      IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (!kv->value || !same (kv->value, (char *)community_string))
    ret = CONFIG_DIFF_DIFFERENT;
  else
    ret = CONFIG_DIFF_SAME;

  if (ret == CONFIG_DIFF_DIFFERENT)
    report_diff (section->section_name,
		 kv->key_name,
		 kv->value,
		 (char *)community_string);
  return ret;
}

static config_validate_t
community_string_validate (const char *section_name,
                           const char *key_name,
                           const char *value)
{
  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return CONFIG_VALIDATE_INVALID_VALUE;
  return CONFIG_VALIDATE_VALID_VALUE;
}

struct config_section *
pef_config_community_string_section_get (pef_config_state_data_t *state_data)
{
  struct config_section *section = NULL;

  if (!(section = pef_config_section_create ("Community_String",
                                             NULL, 
                                             NULL, 
                                             0)))
    goto cleanup;

  if (pef_config_section_add_keyvalue (section,
                                       "Community_String",
                                       "Give valid string",
                                       0,
                                       community_string_checkout,
                                       community_string_commit,
                                       community_string_diff,
                                       community_string_validate) < 0) 
    goto cleanup;

  return section;

 cleanup:
  if (section)
    pef_config_section_destroy(section);
  return NULL;
}

