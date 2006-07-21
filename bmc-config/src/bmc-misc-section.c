#include "bmc-common.h"
#include "bmc-diff.h"
#include "bmc-types.h"
#include "bmc-sections.h"
#include "bmc-config-api.h"

static int
power_restore_policy_number (const char *string)
{
  if (same (string, "off_state_ac_apply"))
    return IPMI_POWER_RESTORE_POLICY_POWERED_OFF_AFTER_AC_RETURNS;
  if (same (string, "restore_state_ac_apply"))
    return IPMI_POWER_RESTORE_POLICY_POWER_RESTORED_TO_STATE;
  if (same (string, "on_state_ac_apply"))
    return IPMI_POWER_RESTORE_POLICY_POWERS_UP_AFTER_AC_RETURNS;
  return -1;
}

static char *
power_restore_policy_string (uint8_t value)
{
  switch (value) {
  case IPMI_POWER_RESTORE_POLICY_POWERED_OFF_AFTER_AC_RETURNS:
    return "OFF_State_AC_Apply";
  case IPMI_POWER_RESTORE_POLICY_POWER_RESTORED_TO_STATE:
    return "Restore_State_AC_Apply";
  case IPMI_POWER_RESTORE_POLICY_POWERS_UP_AFTER_AC_RETURNS:
    return "ON_State_AC_Apply";
  }
  return "";
}


static int
power_restore_policy_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t policy;
  int ret;

  ret = get_bmc_power_restore_policy ((ipmi_device_t *)&args->dev,
				      &policy);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (power_restore_policy_string (policy));
  return 0;
}

static int
power_restore_policy_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  return set_bmc_power_restore_policy ((ipmi_device_t *)&args->dev,
				       power_restore_policy_number (kv->value));
}

static int
power_restore_policy_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  int ret;
  uint8_t got_value;
  uint8_t passed_value;
  
  ret = get_bmc_power_restore_policy ((ipmi_device_t *)&args->dev,
				      &got_value);
  
  if (ret != 0)
    return -1;
  
  passed_value = power_restore_policy_number (kv->value);

  if (passed_value == got_value) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 power_restore_policy_string (got_value));
  }
  return ret;
}

static int
power_restore_policy_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (power_restore_policy_number (value) == -1 ? 1 : 0);
}

struct section *
bmc_misc_section_get (struct arguments *args)
{
  struct section *misc_section = NULL;

  misc_section = (void *) calloc (1, sizeof (struct section));
  misc_section->section = strdup ("Misc");

  add_keyvalue (misc_section,
		"Power_Restore_Policy",
		"Possible values: OFF_State_AC_Apply/Restore_State_AC_Apply/ON_State_AC_Apply",
		power_restore_policy_checkout,
		power_restore_policy_commit,
		power_restore_policy_diff,
		power_restore_policy_validate);

  return misc_section;
}
