#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

static bmc_err_t
power_restore_policy_checkout (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t policy;
  bmc_err_t ret;

  if ((ret = get_bmc_power_restore_policy (state_data,
                                           &policy)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (power_restore_policy_string (policy))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
power_restore_policy_commit (bmc_config_state_data_t *state_data,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  return set_bmc_power_restore_policy (state_data,
				       power_restore_policy_number (kv->value));
}

static bmc_diff_t
power_restore_policy_diff (bmc_config_state_data_t *state_data,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_bmc_power_restore_policy (state_data,
                                          &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  passed_value = power_restore_policy_number (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   power_restore_policy_string (got_value));
    }

  return ret;
}

struct section *
bmc_misc_section_get (bmc_config_state_data_t *state_data)
{
  struct section *misc_section = NULL;

  if (!(misc_section = bmc_config_section_create (state_data, 
                                                  "Misc",
                                                  NULL,
                                                  0)))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       misc_section,
                                       "Power_Restore_Policy",
                                       "Possible values: Off_State_AC_Apply/Restore_State_AC_Apply/On_State_AC_Apply",
                                       BMC_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                                       power_restore_policy_checkout,
                                       power_restore_policy_commit,
                                       power_restore_policy_diff,
                                       power_restore_policy_number_validate) < 0)
    goto cleanup;

  return misc_section;

 cleanup:
  if (misc_section)
    bmc_config_section_destroy(state_data, misc_section);
  return NULL;
}
