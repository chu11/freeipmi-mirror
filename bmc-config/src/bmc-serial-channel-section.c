
#include <stdio.h>
#include "bmc-diff.h"
#include "bmc-sections.h"
#include "bmc-config-api.h"

static char *
channel_access_mode_string (uint8_t mode)
{
  switch (mode) {
  case 0:
    return "Disabled";
  case 1:
    return "Pre_Boot_Only";
  case 2:
    return "Available_Always";
  case 3:
    return "Shared";
  }
  return "";
}

static int
channel_access_mode (const char *string)
{
  if (same (string, "disabled"))
    return 0;
  if (same (string, "pre_boot_only"))
    return 1;
  if (same (string, "available_always"))
    return 2;
  if (same (string, "shared"))
    return 3;
  return -1;
}

static char *
get_privilege_limit_string (uint8_t limit)
{
  switch (limit) {
  case 1:
    return "Callback";
  case 2:
    return "User";
  case 3:
    return "Operator";
  case 4:
    return "Administrator";
  case 5:
    return "OEM_Proprietary";
  case 0xf:
    return "NO_Access";
  }
  return "";
}

static uint8_t
get_privilege_limit_number (const char *value)
{
  if (same (value, "callback"))
    return 1;
  if (same (value, "user"))
    return 2;
  if (same (value, "operator"))
    return 3;
  if (same (value, "administrator"))
    return 4;
  if (same (value, "oem_proprietary"))
    return 5;
  if (same (value, "no_access"))
    return 0xf;
  return 0;
}


/* volatile */

static int
serial_channel_volatile_access_set (ipmi_device_t *dev,
				 uint8_t access_mode,
				 uint8_t access_mode_is_set,
				 uint8_t user_level_authentication,
				 uint8_t user_level_authentication_is_set,
				 uint8_t per_message_authentication,
				 uint8_t per_message_authentication_is_set,
				 uint8_t pef_alerting,
				 uint8_t pef_alerting_is_set,
				 uint8_t channel_privilege_limit,
				 uint8_t channel_privilege_limit_is_set)
{
  uint8_t tmp_access_mode;
  uint8_t tmp_user_level_authentication;
  uint8_t tmp_per_message_authentication;
  uint8_t tmp_pef_alerting;
  uint8_t tmp_channel_privilege_limit;
  int ret;
  
  ret = get_bmc_serial_channel_volatile_access (dev,
					     &tmp_access_mode,
					     &tmp_user_level_authentication,
					     &tmp_per_message_authentication,
					     &tmp_pef_alerting,
					     &tmp_channel_privilege_limit);
  if (ret != 0)
    return -1;

  if (access_mode_is_set)
    tmp_access_mode = access_mode;
  if (user_level_authentication_is_set)
    tmp_user_level_authentication = user_level_authentication;
  if (per_message_authentication_is_set)
    tmp_per_message_authentication = per_message_authentication;
  if (pef_alerting_is_set)
    tmp_pef_alerting = pef_alerting;
  if (channel_privilege_limit_is_set)
    tmp_channel_privilege_limit = channel_privilege_limit;

  ret = set_bmc_serial_channel_volatile_access (dev,
					     tmp_access_mode,
					     tmp_user_level_authentication,
					     tmp_per_message_authentication,
					     tmp_pef_alerting,
					     tmp_channel_privilege_limit);

  if (ret != 0)
    return -1;

  return 0;
}


/* access_mode */

static int
volatile_access_mode_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &get_val,
					     &foo,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  kv->value = strdup (channel_access_mode_string (get_val));
  return 0;
}

static int
volatile_access_mode_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = channel_access_mode (kv->value);
  return serial_channel_volatile_access_set ((ipmi_device_t *)&args->dev,
					  commit_val, 1,
					  0, 0,
					  0, 0,
					  0, 0,
					  0, 0);
}

static int
volatile_access_mode_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &get_val,
					     &foo,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = channel_access_mode (kv->value);
  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 channel_access_mode_string (get_val));
  }
  return ret;
}

static int
volatile_access_mode_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (channel_access_mode (value) >= 0) ? 0 : 1;
}


/* enable_user_level_auth */

static int
volatile_enable_user_level_auth_checkout (const struct arguments *args,
					  const struct section *sect,
					  struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &get_val,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (get_val)
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");
  return 0;
}

static int
volatile_enable_user_level_auth_commit (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_volatile_access_set ((ipmi_device_t *)&args->dev,
					  0, 0,
					  commit_val, 1,
					  0, 0,
					  0, 0,
					  0, 0);
}

static int
volatile_enable_user_level_auth_diff (const struct arguments *args,
				      const struct section *sect,
				      const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &get_val,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_val ? "Yes" : "No");
  }
  return ret;
}

static int
volatile_enable_user_level_auth_validate (const struct arguments *args,
					  const struct section *sect,
					  const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}


/* enable_per_message_auth */

static int
volatile_enable_per_msg_auth_checkout (const struct arguments *args,
				       const struct section *sect,
				       struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &get_val,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (get_val)
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");
  return 0;
}

static int
volatile_enable_per_msg_auth_commit (const struct arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_volatile_access_set ((ipmi_device_t *)&args->dev,
					  0, 0,
					  0, 0,
					  commit_val, 1,
					  0, 0,
					  0, 0);
}

static int
volatile_enable_per_msg_auth_diff (const struct arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &get_val,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_val ? "Yes" : "No");
  }
  return ret;
}

static int
volatile_enable_per_msg_auth_validate (const struct arguments *args,
					  const struct section *sect,
					  const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}


/* enable_pef_alerting */


static int
volatile_enable_pef_alerting_checkout (const struct arguments *args,
				       const struct section *sect,
				       struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &get_val,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (get_val)
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");
  return 0;
}

static int
volatile_enable_pef_alerting_commit (const struct arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_volatile_access_set ((ipmi_device_t *)&args->dev,
					  0, 0,
					  0, 0,
					  0, 0,
					  commit_val, 1,
					  0, 0);
}

static int
volatile_enable_pef_alerting_diff (const struct arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &get_val,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");
  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_val ? "Yes" : "No");
  }
  return ret;
}

static int
volatile_enable_pef_alerting_validate (const struct arguments *args,
				       const struct section *sect,
				       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* channel_privilege_level */


static int
volatile_channel_priv_limit_checkout (const struct arguments *args,
				      const struct section *sect,
				      struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &foo,
					     &get_val);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (get_privilege_limit_string (get_val));

  return 0;
}

static int
volatile_channel_priv_limit_commit (const struct arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = get_privilege_limit_number (kv->value);
  return serial_channel_volatile_access_set ((ipmi_device_t *)&args->dev,
					  0, 0,
					  0, 0,
					  0, 0,
					  0, 0,
					  commit_val, 1);
}

static int
volatile_channel_priv_limit_diff (const struct arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &foo,
					     &get_val);
  if (ret != 0)
    return -1;

  passed_val = get_privilege_limit_number (kv->value);

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_privilege_limit_string (get_val));
  }
  return ret;
}

static int
volatile_channel_priv_limit_validate (const struct arguments *args,
				      const struct section *sect,
				      const char *value)
{
  int level = get_privilege_limit_number (value);
  return (level > 0) ? 0 : 1;
}


/* non volatile */


static int
serial_channel_non_volatile_access_set (ipmi_device_t *dev,
				 uint8_t access_mode,
				 uint8_t access_mode_is_set,
				 uint8_t user_level_authentication,
				 uint8_t user_level_authentication_is_set,
				 uint8_t per_message_authentication,
				 uint8_t per_message_authentication_is_set,
				 uint8_t pef_alerting,
				 uint8_t pef_alerting_is_set,
				 uint8_t channel_privilege_limit,
				 uint8_t channel_privilege_limit_is_set)
{
  uint8_t tmp_access_mode;
  uint8_t tmp_user_level_authentication;
  uint8_t tmp_per_message_authentication;
  uint8_t tmp_pef_alerting;
  uint8_t tmp_channel_privilege_limit;
  int ret;
  
  ret = get_bmc_serial_channel_non_volatile_access (dev,
					     &tmp_access_mode,
					     &tmp_user_level_authentication,
					     &tmp_per_message_authentication,
					     &tmp_pef_alerting,
					     &tmp_channel_privilege_limit);
  if (ret != 0)
    return -1;

  if (access_mode_is_set)
    tmp_access_mode = access_mode;
  if (user_level_authentication_is_set)
    tmp_user_level_authentication = user_level_authentication;
  if (per_message_authentication_is_set)
    tmp_per_message_authentication = per_message_authentication;
  if (pef_alerting_is_set)
    tmp_pef_alerting = pef_alerting;
  if (channel_privilege_limit_is_set)
    tmp_channel_privilege_limit = channel_privilege_limit;

  ret = set_bmc_serial_channel_non_volatile_access (dev,
					     tmp_access_mode,
					     tmp_user_level_authentication,
					     tmp_per_message_authentication,
					     tmp_pef_alerting,
					     tmp_channel_privilege_limit);

  if (ret != 0)
    return -1;

  return 0;
}


/* access_mode */

static int
non_volatile_access_mode_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
					     &get_val,
					     &foo,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  kv->value = strdup (channel_access_mode_string (get_val));
  return 0;
}

static int
non_volatile_access_mode_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = channel_access_mode (kv->value);
  return serial_channel_non_volatile_access_set ((ipmi_device_t *)&args->dev,
					  commit_val, 1,
					  0, 0,
					  0, 0,
					  0, 0,
					  0, 0);
}

static int
non_volatile_access_mode_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
					     &get_val,
					     &foo,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = channel_access_mode (kv->value);

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 channel_access_mode_string (get_val));
  }
  return ret;
}

static int
non_volatile_access_mode_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (channel_access_mode (value) >= 0) ? 0 : 1;
}


/* enable_user_level_auth */

static int
non_volatile_enable_user_level_auth_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &get_val,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (get_val)
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");
  return 0;
}

static int
non_volatile_enable_user_level_auth_commit (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_non_volatile_access_set ((ipmi_device_t *)&args->dev,
					  0, 0,
					  commit_val, 1,
					  0, 0,
					  0, 0,
					  0, 0);
}

static int
non_volatile_enable_user_level_auth_diff (const struct arguments *args,
				      const struct section *sect,
				      const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &get_val,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_val ? "Yes" : "No");
  }
  return ret;
}

static int
non_volatile_enable_user_level_auth_validate (const struct arguments *args,
					      const struct section *sect,
					      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}


/* enable_per_message_auth */

static int
non_volatile_enable_per_msg_auth_checkout (const struct arguments *args,
				       const struct section *sect,
				       struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &get_val,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (get_val)
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");
  return 0;
}

static int
non_volatile_enable_per_msg_auth_commit (const struct arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_non_volatile_access_set ((ipmi_device_t *)&args->dev,
					  0, 0,
					  0, 0,
					  commit_val, 1,
					  0, 0,
					  0, 0);
}

static int
non_volatile_enable_per_msg_auth_diff (const struct arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &get_val,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_val ? "Yes" : "No");
  }
  return ret;
}

static int
non_volatile_enable_per_msg_auth_validate (const struct arguments *args,
					   const struct section *sect,
					   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}


/* enable_pef_alerting */


static int
non_volatile_enable_pef_alerting_checkout (const struct arguments *args,
					   const struct section *sect,
					   struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &get_val,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (get_val)
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");
  return 0;
}

static int
non_volatile_enable_pef_alerting_commit (const struct arguments *args,
					 const struct section *sect,
					 const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_non_volatile_access_set ((ipmi_device_t *)&args->dev,
						 0, 0,
						 0, 0,
						 0, 0,
						 commit_val, 1,
						 0, 0);
}

static int
non_volatile_enable_pef_alerting_diff (const struct arguments *args,
				       const struct section *sect,
				       const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
						    &foo,
						    &foo,
						    &foo,
						    &get_val,
						    &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");
  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_val ? "Yes" : "No");
  }
  return ret;
}

static int
non_volatile_enable_pef_alerting_validate (const struct arguments *args,
					   const struct section *sect,
					   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* channel_privilege_level */


static int
non_volatile_channel_priv_limit_checkout (const struct arguments *args,
					  const struct section *sect,
					  struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  int ret;
  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
						    &foo,
						    &foo,
						    &foo,
						    &foo,
						    &get_val);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (get_privilege_limit_string (get_val));

  return 0;
}

static int
non_volatile_channel_priv_limit_commit (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = get_privilege_limit_number (kv->value);
  return serial_channel_non_volatile_access_set ((ipmi_device_t *)&args->dev,
						 0, 0,
						 0, 0,
						 0, 0,
						 0, 0,
						 commit_val, 1);
}

static int
non_volatile_channel_priv_limit_diff (const struct arguments *args,
				      const struct section *sect,
				      const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_serial_channel_non_volatile_access ((ipmi_device_t *)&args->dev,
						 &foo,
						 &foo,
						 &foo,
						 &foo,
						 &get_val);
  if (ret != 0)
    return -1;

  passed_val = get_privilege_limit_number (kv->value);
  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 0;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_privilege_limit_string (get_val));
  }
  return ret;
}

static int
non_volatile_channel_priv_limit_validate (const struct arguments *args,
					  const struct section *sect,
					  const char *value)
{
  int level = get_privilege_limit_number (value);
  return (level > 0) ? 0 : 1;
}


struct section *
bmc_serial_channel_section_get (struct arguments *args)
{
  struct section * serial_channel_section = NULL;

  serial_channel_section = (void *) calloc (1, sizeof (struct section));
  serial_channel_section->section = strdup ("Serial_Channel");

  add_keyvalue (serial_channel_section,
		"Volatile_Access_Mode",
		"Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
		volatile_access_mode_checkout,
		volatile_access_mode_commit,
		volatile_access_mode_diff,
		volatile_access_mode_validate);

  add_keyvalue (serial_channel_section,
		"Volatile_Enable_User_Level_Auth",
		"Possible values: Yes/No",
		volatile_enable_user_level_auth_checkout,
		volatile_enable_user_level_auth_commit,
		volatile_enable_user_level_auth_diff,
		volatile_enable_user_level_auth_validate);

  add_keyvalue (serial_channel_section,
		"Volatile_Enable_Per_Message_Auth",
		"Possible values: Yes/No",
		volatile_enable_per_msg_auth_checkout,
		volatile_enable_per_msg_auth_commit,
		volatile_enable_per_msg_auth_diff,
		volatile_enable_per_msg_auth_validate);

  add_keyvalue (serial_channel_section,
		"Volatile_Enable_Pef_Alerting",
		"Possible values: Yes/No",
		volatile_enable_pef_alerting_checkout,
		volatile_enable_pef_alerting_commit,
		volatile_enable_pef_alerting_diff,
		volatile_enable_pef_alerting_validate);

  add_keyvalue (serial_channel_section,
		"Volatile_Channel_Privilege_Limit",
		"Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/NO_Access",
		volatile_channel_priv_limit_checkout,
		volatile_channel_priv_limit_commit,
		volatile_channel_priv_limit_diff,
		volatile_channel_priv_limit_validate);

  add_keyvalue (serial_channel_section,
		"Non_Volatile_Access_Mode",
		"Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
		non_volatile_access_mode_checkout,
		non_volatile_access_mode_commit,
		non_volatile_access_mode_diff,
		non_volatile_access_mode_validate);

  add_keyvalue (serial_channel_section,
		"Non_Volatile_Enable_User_Level_Auth",
		"Possible values: Yes/No",
		non_volatile_enable_user_level_auth_checkout,
		non_volatile_enable_user_level_auth_commit,
		non_volatile_enable_user_level_auth_diff,
		non_volatile_enable_user_level_auth_validate);

  add_keyvalue (serial_channel_section,
		"Non_Volatile_Enable_Per_Message_Auth",
		"Possible values: Yes/No",
		non_volatile_enable_per_msg_auth_checkout,
		non_volatile_enable_per_msg_auth_commit,
		non_volatile_enable_per_msg_auth_diff,
		non_volatile_enable_per_msg_auth_validate);

  add_keyvalue (serial_channel_section,
		"Non_Volatile_Enable_Pef_Alerting",
		"Possible values: Yes/No",
		non_volatile_enable_pef_alerting_checkout,
		non_volatile_enable_pef_alerting_commit,
		non_volatile_enable_pef_alerting_diff,
		non_volatile_enable_pef_alerting_validate);

  add_keyvalue (serial_channel_section,
		"Non_Volatile_Channel_Privilege_Limit",
		"Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/NO_Access",
		non_volatile_channel_priv_limit_checkout,
		non_volatile_channel_priv_limit_commit,
		non_volatile_channel_priv_limit_diff,
		non_volatile_channel_priv_limit_validate);

  return serial_channel_section;
}

