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
#include "pef-config-wrapper.h"

static config_err_t
community_string_checkout (const char *section_name,
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
		    
  if (config_section_update_keyvalue_output(kv, (char *)community_string) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
community_string_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  return set_bmc_community_string (state_data,
                                   kv->value_input);
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

  if (!(section = config_section_create ("Community_String",
                                         NULL, 
                                         NULL, 
                                         0)))
    goto cleanup;

  if (config_section_add_key (section,
                              "Community_String",
                              "Give valid string",
                              0,
                              community_string_checkout,
                              community_string_commit,
                              community_string_validate) < 0) 
    goto cleanup;

  return section;

 cleanup:
  if (section)
    config_section_destroy(section);
  return NULL;
}

