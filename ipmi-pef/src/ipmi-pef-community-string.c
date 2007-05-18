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
#include "ipmi-pef-wrapper.h"

static pef_err_t
community_string_checkout (ipmi_pef_state_data_t *state_data,
                           const struct section *sect,
                           struct keyvalue *kv)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  pef_err_t ret;

  if ((ret = get_bmc_lan_conf_community_string (state_data,
                                                community_string,
                                                IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != PEF_ERR_SUCCESS) 
    return ret;
		    
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ((char *)community_string)))
    {
      perror("strdup");
      return PEF_ERR_FATAL_ERROR;
    }

  return PEF_ERR_SUCCESS;
}

static pef_err_t
community_string_commit (ipmi_pef_state_data_t *state_data,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  if (!kv->value)
    return PEF_ERR_FATAL_ERROR;

  return set_bmc_lan_conf_community_string (state_data,
                                            (uint8_t *)kv->value);
}

static pef_diff_t
community_string_diff (ipmi_pef_state_data_t *state_data,
                       const struct section *sect,
                       const struct keyvalue *kv)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  pef_err_t rc;
  pef_diff_t ret;

  if ((rc = get_bmc_community_string (state_data,
                                      community_string,
                                      IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != PEF_ERR_SUCCESS)
    {
      if (rc == PEF_ERR_NON_FATAL_ERROR)
        return PEF_DIFF_NON_FATAL_ERROR;
      return PEF_DIFF_FATAL_ERROR;
    }

  if (!kv->value || !same (kv->value, (char *)community_string))
    ret = PEF_DIFF_DIFFERENT;
  else
    ret = PEF_DIFF_SAME;

  if (ret == PEF_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 (char *)community_string);
  return ret;
}

static pef_validate_t
community_string_validate (ipmi_pef_state_data_t *state_data,
                           const struct section *sect,
                           const char *value)
{
  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return PEF_VALIDATE_INVALID_VALUE;
  return PEF_VALIDATE_VALID_VALUE;
}

struct section *
ipmi_pef_community_string_section_get (ipmi_pef_state_data_t *state_data)
{
  struct section *sect = NULL;

  if (!(sect = ipmi_pef_section_create (state_data, "Community_String")))
    goto cleanup;

  if (ipmi_pef_section_add_keyvalue (state_data,
                                     sect,
                                     "Community_String",
                                     "Give valid string",
                                     0,
                                     community_string_checkout,
                                     community_string_commit,
                                     community_string_diff,
                                     community_string_validate) < 0) 
    goto cleanup;

  return sect;

 cleanup:
  if (sect)
    ipmi_pef_section_destroy(state_data, sect);
  return NULL;
}

