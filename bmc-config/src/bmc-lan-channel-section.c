#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"

/* volatile */

static int
lan_channel_volatile_access_set (ipmi_device_t dev,
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
  
  ret = get_bmc_lan_channel_volatile_access (dev,
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

  ret = set_bmc_lan_channel_volatile_access (dev,
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
  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &get_val,
					     &foo,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (channel_access_mode_string (get_val))))
    {
      perror("strdup");
      exit(1);
    }
  return 0;
}

static int
volatile_access_mode_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = channel_access_mode (kv->value);
  return lan_channel_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &get_val,
					     &foo,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = channel_access_mode (kv->value);

  if (passed_val == get_val)
    ret = 0; 
  else
    {
      report_diff (sect->section, 
                   kv->key,
                   kv->value,
                   channel_access_mode_string (get_val));
      ret = 1;
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
  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &get_val,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
volatile_enable_user_level_auth_commit (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t commit_val;

  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  commit_val = same (kv->value, "yes");
  return lan_channel_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &get_val,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  passed_val = same (kv->value, "yes");
  if (passed_val == get_val)
    ret = 0;
  else
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
      ret = 1;
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
  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &get_val,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
volatile_enable_per_msg_auth_commit (const struct arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t commit_val;

  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  commit_val = same (kv->value, "yes");
  return lan_channel_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &get_val,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  passed_val = same (kv->value, "yes");
  if (passed_val == get_val)
    ret = 0;
  else
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
      ret = 1;
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
  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &get_val,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
volatile_enable_pef_alerting_commit (const struct arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t commit_val;

  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  commit_val = same (kv->value, "yes");
  return lan_channel_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &get_val,
					     &foo);
  if (ret != 0)
    return -1;

  /* achu: Backwards values in this command are handled in bmc-config-api.c */
  passed_val = same (kv->value, "yes");
  if (passed_val == get_val)
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
      ret = 1;
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
  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &foo,
					     &get_val);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (privilege_level_string (get_val))))
    {
      perror("strdup");
      exit(1);
    }

  return 0;
}

static int
volatile_channel_priv_limit_commit (const struct arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = privilege_level_number (kv->value);
  return lan_channel_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &foo,
					     &get_val);
  if (ret != 0)
    return -1;

  passed_val = privilege_level_number (kv->value);

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   privilege_level_string (get_val));
      ret = 1;
    }

  return ret;
}

static int
volatile_channel_priv_limit_validate (const struct arguments *args,
				      const struct section *sect,
				      const char *value)
{
  int level = privilege_level_number (value);
  return (level > 0) ? 0 : 1;
}


/* non volatile */


static int
lan_channel_non_volatile_access_set (ipmi_device_t dev,
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
  
  ret = get_bmc_lan_channel_non_volatile_access (dev,
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

  ret = set_bmc_lan_channel_non_volatile_access (dev,
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
  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
					     &get_val,
					     &foo,
					     &foo,
					     &foo,
					     &foo);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (channel_access_mode_string (get_val))))
    {
      perror("strdup");
      exit(1);
    }
  return 0;
}

static int
non_volatile_access_mode_commit (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = channel_access_mode (kv->value);
  return lan_channel_non_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
						 &get_val,
						 &foo,
						 &foo,
						 &foo,
						 &foo);
  if (ret != 0)
    return -1;

  passed_val = channel_access_mode (kv->value);
  if (passed_val == get_val)
    ret = 0;
  else
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
      ret = 1;
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
  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
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
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
non_volatile_enable_user_level_auth_commit (const struct arguments *args,
					    const struct section *sect,
					    const struct keyvalue *kv)
{
  uint8_t commit_val;
  
  commit_val = same (kv->value, "yes");
  return lan_channel_non_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
						 &foo,
						 &get_val,
						 &foo,
						 &foo,
						 &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");
  if (passed_val == get_val)
     ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
      ret = 1;
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
  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
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
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
non_volatile_enable_per_msg_auth_commit (const struct arguments *args,
					 const struct section *sect,
					 const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return lan_channel_non_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &get_val,
					     &foo,
					     &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
      ret = 1;
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
  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
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
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
non_volatile_enable_pef_alerting_commit (const struct arguments *args,
					 const struct section *sect,
					 const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return lan_channel_non_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
						 &foo,
						 &foo,
						 &foo,
						 &get_val,
						 &foo);
  if (ret != 0)
    return -1;

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
      ret = 1;
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
  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
					     &foo,
					     &foo,
					     &foo,
					     &foo,
					     &get_val);
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (privilege_level_string (get_val))))
    {
      perror("strdup");
      exit(1);
    }

  return 0;
}

static int
non_volatile_channel_priv_limit_commit (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = privilege_level_number (kv->value);
  return lan_channel_non_volatile_access_set (args->dev,
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

  ret = get_bmc_lan_channel_non_volatile_access (args->dev,
						 &foo,
						 &foo,
						 &foo,
						 &foo,
						 &get_val);
  if (ret != 0)
    return -1;

  passed_val = privilege_level_number (kv->value);
  
  if (passed_val == get_val)
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   privilege_level_string (get_val));
      ret = 1;
    }
  
  return ret;
}

static int
non_volatile_channel_priv_limit_validate (const struct arguments *args,
					  const struct section *sect,
					  const char *value)
{
  int level = privilege_level_number (value);
  return (level > 0) ? 0 : 1;
}


struct section *
bmc_lan_channel_section_get (struct arguments *args)
{
  struct section * lan_channel_section = NULL;

  if (!(lan_channel_section = (void *) calloc (1, sizeof (struct section))))
    {
      perror("calloc");
      exit(1);
    }

  if (!(lan_channel_section->section = strdup ("Lan_Channel")))
    {
      perror("strdup");
      exit(1);
    }

  add_keyvalue (lan_channel_section,
		"Volatile_Access_Mode",
		"Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                0,
		volatile_access_mode_checkout,
		volatile_access_mode_commit,
		volatile_access_mode_diff,
		volatile_access_mode_validate);

  add_keyvalue (lan_channel_section,
		"Volatile_Enable_User_Level_Auth",
		"Possible values: Yes/No",
                0,
		volatile_enable_user_level_auth_checkout,
		volatile_enable_user_level_auth_commit,
		volatile_enable_user_level_auth_diff,
		volatile_enable_user_level_auth_validate);

  add_keyvalue (lan_channel_section,
		"Volatile_Enable_Per_Message_Auth",
		"Possible values: Yes/No",
                0,
		volatile_enable_per_msg_auth_checkout,
		volatile_enable_per_msg_auth_commit,
		volatile_enable_per_msg_auth_diff,
		volatile_enable_per_msg_auth_validate);

  add_keyvalue (lan_channel_section,
		"Volatile_Enable_Pef_Alerting",
		"Possible values: Yes/No",
                0,
		volatile_enable_pef_alerting_checkout,
		volatile_enable_pef_alerting_commit,
		volatile_enable_pef_alerting_diff,
		volatile_enable_pef_alerting_validate);

  add_keyvalue (lan_channel_section,
		"Volatile_Channel_Privilege_Limit",
		"Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                0,
		volatile_channel_priv_limit_checkout,
		volatile_channel_priv_limit_commit,
		volatile_channel_priv_limit_diff,
		volatile_channel_priv_limit_validate);

  add_keyvalue (lan_channel_section,
		"Non_Volatile_Access_Mode",
		"Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                0,
		non_volatile_access_mode_checkout,
		non_volatile_access_mode_commit,
		non_volatile_access_mode_diff,
		non_volatile_access_mode_validate);

  add_keyvalue (lan_channel_section,
		"Non_Volatile_Enable_User_Level_Auth",
		"Possible values: Yes/No",
                0,
		non_volatile_enable_user_level_auth_checkout,
		non_volatile_enable_user_level_auth_commit,
		non_volatile_enable_user_level_auth_diff,
		non_volatile_enable_user_level_auth_validate);

  add_keyvalue (lan_channel_section,
		"Non_Volatile_Enable_Per_Message_Auth",
		"Possible values: Yes/No",
                0,
		non_volatile_enable_per_msg_auth_checkout,
		non_volatile_enable_per_msg_auth_commit,
		non_volatile_enable_per_msg_auth_diff,
		non_volatile_enable_per_msg_auth_validate);

  add_keyvalue (lan_channel_section,
		"Non_Volatile_Enable_Pef_Alerting",
		"Possible values: Yes/No",
                0,
		non_volatile_enable_pef_alerting_checkout,
		non_volatile_enable_pef_alerting_commit,
		non_volatile_enable_pef_alerting_diff,
		non_volatile_enable_pef_alerting_validate);

  add_keyvalue (lan_channel_section,
		"Non_Volatile_Channel_Privilege_Limit",
		"Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                0,
		non_volatile_channel_priv_limit_checkout,
		non_volatile_channel_priv_limit_commit,
		non_volatile_channel_priv_limit_diff,
		non_volatile_channel_priv_limit_validate);

  return lan_channel_section;
}

