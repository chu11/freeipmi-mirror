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
#include "pef-config-map.h"
#include "pef-config-wrapper.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

static config_err_t
community_string_checkout (pef_config_state_data_t *state_data,
                           const struct config_section *sect,
                           struct config_keyvalue *kv)
{
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  config_err_t ret;

  if ((ret = get_bmc_community_string (state_data,
                                       community_string,
                                       IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != CONFIG_ERR_SUCCESS) 
    return ret;
		    
  if (!(kv->value_output = strdup ((char *)community_string)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
community_string_commit (pef_config_state_data_t *state_data,
                         const struct config_section *sect,
                         const struct config_keyvalue *kv)
{
  if (!kv->value_input)
    return CONFIG_ERR_FATAL_ERROR;

  return set_bmc_community_string (state_data,
                                   kv->value_input);
}

static pef_diff_t
community_string_diff (pef_config_state_data_t *state_data,
                       const struct config_section *sect,
                       const struct config_keyvalue *kv)
{
  char community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  config_err_t rc;
  pef_diff_t ret;

  if ((rc = get_bmc_community_string (state_data,
                                      community_string,
                                      IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  if (!kv->value_input || !same (kv->value_input, (char *)community_string))
    ret = PEF_DIFF_DIFFERENT;
  else
    ret = PEF_DIFF_SAME;

  if (ret == PEF_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value_input,
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
  struct config_section *sect = NULL;

  if (!(sect = config_section_create ("Community_String",
                                      NULL, 
                                      NULL, 
                                      0,
                                      NULL, /* XXX */
                                      NULL)))
    goto cleanup;

  if (config_section_add_key (sect,
                              "Community_String",
                              "Give valid string",
                              0,
                              community_string_validate) < 0) 
    goto cleanup;

  return sect;

 cleanup:
  if (sect)
    config_section_destroy(sect);
  return NULL;
}

