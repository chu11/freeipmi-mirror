#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"
#include "bmc-validate.h"

static bmc_err_t
power_restore_policy_checkout (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t policy;
  bmc_err_t ret;

  if ((ret = get_bmc_power_restore_policy (args->dev,
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
power_restore_policy_commit (const struct bmc_config_arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  return set_bmc_power_restore_policy (args->dev,
				       power_restore_policy_number (kv->value));
}

static bmc_diff_t
power_restore_policy_diff (const struct bmc_config_arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_bmc_power_restore_policy (args->dev,
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
bmc_misc_section_get (struct bmc_config_arguments *args)
{
  struct section *misc_section = NULL;

  if (!(misc_section = bmc_section_create ("Misc")))
    goto cleanup;

  if (bmc_section_add_keyvalue (misc_section,
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
    bmc_section_destroy(misc_section);
  return NULL;
}
