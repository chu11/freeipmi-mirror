#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-sections.h"

/* callback_none */

static int
callback_none_checkout (const struct arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_none)
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
callback_none_commit (const struct arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.callback.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
callback_none_diff (const struct arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.callback.type_none == same (kv->value, "yes")) 
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.callback.type_none ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
callback_none_validate (const struct arguments *args,
			const struct section *sect,
			const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* callback_md2 */

static int
callback_md2_checkout (const struct arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_md2)
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
callback_md2_commit (const struct arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.callback.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
callback_md2_diff (const struct arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.callback.type_md2 == same (kv->value, "yes")) 
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.callback.type_md2 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
callback_md2_validate (const struct arguments *args,
			const struct section *sect,
			const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* callback_md5 */

static int
callback_md5_checkout (const struct arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_md5)
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
callback_md5_commit (const struct arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.callback.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
callback_md5_diff (const struct arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.callback.type_md5 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.callback.type_md5 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
callback_md5_validate (const struct arguments *args,
			const struct section *sect,
			const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* callback_straight_password */

static int
callback_straight_password_checkout (const struct arguments *args,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_straight_password)
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
callback_straight_password_commit (const struct arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.callback.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
callback_straight_password_diff (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.callback.type_straight_password == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.callback.type_straight_password ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
callback_straight_password_validate (const struct arguments *args,
				     const struct section *sect,
				     const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* callback_oem_proprietary */

static int
callback_oem_proprietary_checkout (const struct arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_oem_proprietary)
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
callback_oem_proprietary_commit (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.callback.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
callback_oem_proprietary_diff (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.callback.type_oem_proprietary == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.callback.type_oem_proprietary ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
callback_oem_proprietary_validate (const struct arguments *args,
				   const struct section *sect,
				   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* user */

/* user_none */

static int
user_none_checkout (const struct arguments *args,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_none)
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
user_none_commit (const struct arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.user.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
user_none_diff (const struct arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.user.type_none == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.user.type_none ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
user_none_validate (const struct arguments *args,
		    const struct section *sect,
		    const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* user_md2 */

static int
user_md2_checkout (const struct arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_md2)
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
user_md2_commit (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.user.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
user_md2_diff (const struct arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.user.type_md2 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.user.type_md2 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
user_md2_validate (const struct arguments *args,
			const struct section *sect,
			const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* user_md5 */

static int
user_md5_checkout (const struct arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_md5)
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
user_md5_commit (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.user.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
user_md5_diff (const struct arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.user.type_md5 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.user.type_md5 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
user_md5_validate (const struct arguments *args,
		   const struct section *sect,
		   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* user_straight_password */

static int
user_straight_password_checkout (const struct arguments *args,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_straight_password)
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
user_straight_password_commit (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.user.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
user_straight_password_diff (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.user.type_straight_password == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.user.type_straight_password ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
user_straight_password_validate (const struct arguments *args,
				 const struct section *sect,
				 const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* user_oem_proprietary */

static int
user_oem_proprietary_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_oem_proprietary)
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
user_oem_proprietary_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.user.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
user_oem_proprietary_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.user.type_oem_proprietary == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.user.type_oem_proprietary ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
user_oem_proprietary_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* operator */

/* operator_none */

static int
operator_none_checkout (const struct arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_none)
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
operator_none_commit (const struct arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.operator.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
operator_none_diff (const struct arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.operator.type_none == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.operator.type_none ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
operator_none_validate (const struct arguments *args,
			const struct section *sect,
			const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* operator_md2 */

static int
operator_md2_checkout (const struct arguments *args,
		       const struct section *sect,
		       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_md2)
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
operator_md2_commit (const struct arguments *args,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.operator.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
operator_md2_diff (const struct arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.operator.type_md2 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.operator.type_md2 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
operator_md2_validate (const struct arguments *args,
		       const struct section *sect,
		       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* operator_md5 */

static int
operator_md5_checkout (const struct arguments *args,
		       const struct section *sect,
		       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_md5)
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
operator_md5_commit (const struct arguments *args,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.operator.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
operator_md5_diff (const struct arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.operator.type_md5 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.operator.type_md5 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
operator_md5_validate (const struct arguments *args,
		       const struct section *sect,
		       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* operator_straight_password */

static int
operator_straight_password_checkout (const struct arguments *args,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_straight_password)
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
operator_straight_password_commit (const struct arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.operator.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
operator_straight_password_diff (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.operator.type_straight_password == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.operator.type_straight_password ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
operator_straight_password_validate (const struct arguments *args,
				     const struct section *sect,
				     const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* operator_oem_proprietary */

static int
operator_oem_proprietary_checkout (const struct arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_oem_proprietary)
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
operator_oem_proprietary_commit (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.operator.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
operator_oem_proprietary_diff (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.operator.type_oem_proprietary == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.operator.type_oem_proprietary ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
operator_oem_proprietary_validate (const struct arguments *args,
				   const struct section *sect,
				   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* admin */


/* admin_none */

static int
admin_none_checkout (const struct arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_none)
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
admin_none_commit (const struct arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.admin.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
admin_none_diff (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.admin.type_none == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.admin.type_none ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
admin_none_validate (const struct arguments *args,
		     const struct section *sect,
		     const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* admin_md2 */

static int
admin_md2_checkout (const struct arguments *args,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_md2)
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
admin_md2_commit (const struct arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.admin.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
admin_md2_diff (const struct arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.admin.type_md2 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.admin.type_md2 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
admin_md2_validate (const struct arguments *args,
			const struct section *sect,
			const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* admin_md5 */

static int
admin_md5_checkout (const struct arguments *args,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_md5)
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
admin_md5_commit (const struct arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.admin.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
admin_md5_diff (const struct arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.admin.type_md5 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.admin.type_md5 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
admin_md5_validate (const struct arguments *args,
		    const struct section *sect,
		    const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* admin_straight_password */

static int
admin_straight_password_checkout (const struct arguments *args,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_straight_password)
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
admin_straight_password_commit (const struct arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.admin.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
admin_straight_password_diff (const struct arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.admin.type_straight_password == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.admin.type_straight_password ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
admin_straight_password_validate (const struct arguments *args,
				  const struct section *sect,
				  const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* admin_oem_proprietary */

static int
admin_oem_proprietary_checkout (const struct arguments *args,
				const struct section *sect,
				struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_oem_proprietary)
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
admin_oem_proprietary_commit (const struct arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.admin.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
admin_oem_proprietary_diff (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.admin.type_oem_proprietary == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.admin.type_oem_proprietary ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
admin_oem_proprietary_validate (const struct arguments *args,
				const struct section *sect,
				const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}


/* oem */


/* oem_none */

static int
oem_none_checkout (const struct arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_none)
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
oem_none_commit (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.oem.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
oem_none_diff (const struct arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.oem.type_none == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.oem.type_none ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
oem_none_validate (const struct arguments *args,
		   const struct section *sect,
		   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* oem_md2 */

static int
oem_md2_checkout (const struct arguments *args,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_md2)
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
oem_md2_commit (const struct arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.oem.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
oem_md2_diff (const struct arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.oem.type_md2 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.oem.type_md2 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
oem_md2_validate (const struct arguments *args,
		  const struct section *sect,
		  const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* oem_md5 */

static int
oem_md5_checkout (const struct arguments *args,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_md5)
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
oem_md5_commit (const struct arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.oem.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
oem_md5_diff (const struct arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.oem.type_md5 == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.oem.type_md5 ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
oem_md5_validate (const struct arguments *args,
		  const struct section *sect,
		  const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* oem_straight_password */

static int
oem_straight_password_checkout (const struct arguments *args,
				const struct section *sect,
				struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_straight_password)
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
oem_straight_password_commit (const struct arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.oem.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
oem_straight_password_diff (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.oem.type_straight_password == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.oem.type_straight_password ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
oem_straight_password_validate (const struct arguments *args,
				const struct section *sect,
				const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* oem_oem_proprietary */

static int
oem_oem_proprietary_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_oem_proprietary)
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
oem_oem_proprietary_commit (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  auth.oem.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static int
oem_oem_proprietary_diff (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  int ret;

  ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
						      &auth);

  if (ret != 0)
    return -1;

  if (auth.oem.type_oem_proprietary == same (kv->value, "yes"))
    ret = 0;
  else 
    {
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   auth.oem.type_oem_proprietary ? "Yes" : "No");
      ret = 1;
    }
  return ret;
}

static int
oem_oem_proprietary_validate (const struct arguments *args,
			      const struct section *sect,
			      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

struct section *
bmc_lan_conf_auth_section_get (struct arguments *args)
{
  struct section *lan_conf_auth_section = NULL;

  if (!(lan_conf_auth_section = (void *) calloc (1, sizeof (struct section))))
    {
      perror("calloc");
      exit(1);
    }
  if (!(lan_conf_auth_section->section = strdup ("Lan_Conf_Auth")))
    {
      perror("strdup");
      exit(1);
    }

  add_keyvalue (lan_conf_auth_section,
		"Callback_Enable_Auth_Type_None",
		"Possible values: Yes/No",
                0,
		callback_none_checkout,
		callback_none_commit,
		callback_none_diff,
		callback_none_validate);

  add_keyvalue (lan_conf_auth_section,
		"Callback_Enable_Auth_Type_MD2",
		"Possible values: Yes/No",
                0,
		callback_md2_checkout,
		callback_md2_commit,
		callback_md2_diff,
		callback_md2_validate);

  add_keyvalue (lan_conf_auth_section,
		"Callback_Enable_Auth_Type_MD5",
		"Possible values: Yes/No",
                0,
		callback_md5_checkout,
		callback_md5_commit,
		callback_md5_diff,
		callback_md5_validate);

  add_keyvalue (lan_conf_auth_section,
		"Callback_Enable_Auth_Type_Straight_Password",
		"Possible values: Yes/No",
                0,
		callback_straight_password_checkout,
		callback_straight_password_commit,
		callback_straight_password_diff,
		callback_straight_password_validate);

  add_keyvalue (lan_conf_auth_section,
		"Callback_Enable_Auth_Type_OEM_Proprietary",
		"Possible values: Yes/No",
                0,
		callback_oem_proprietary_checkout,
		callback_oem_proprietary_commit,
		callback_oem_proprietary_diff,
		callback_oem_proprietary_validate);


  add_keyvalue (lan_conf_auth_section,
		"User_Enable_Auth_Type_None",
		"Possible values: Yes/No",
                0,
		user_none_checkout,
		user_none_commit,
		user_none_diff,
		user_none_validate);

  add_keyvalue (lan_conf_auth_section,
		"User_Enable_Auth_Type_MD2",
		"Possible values: Yes/No",
                0,
		user_md2_checkout,
		user_md2_commit,
		user_md2_diff,
		user_md2_validate);

  add_keyvalue (lan_conf_auth_section,
		"User_Enable_Auth_Type_MD5",
		"Possible values: Yes/No",
                0,
		user_md5_checkout,
		user_md5_commit,
		user_md5_diff,
		user_md5_validate);

  add_keyvalue (lan_conf_auth_section,
		"User_Enable_Auth_Type_Straight_Password",
		"Possible values: Yes/No",
                0,
		user_straight_password_checkout,
		user_straight_password_commit,
		user_straight_password_diff,
		user_straight_password_validate);

  add_keyvalue (lan_conf_auth_section,
		"User_Enable_Auth_Type_OEM_Proprietary",
		"Possible values: Yes/No",
                0,
		user_oem_proprietary_checkout,
		user_oem_proprietary_commit,
		user_oem_proprietary_diff,
		user_oem_proprietary_validate);


  add_keyvalue (lan_conf_auth_section,
		"Operator_Enable_Auth_Type_None",
		"Possible values: Yes/No",
                0,
		operator_none_checkout,
		operator_none_commit,
		operator_none_diff,
		operator_none_validate);

  add_keyvalue (lan_conf_auth_section,
		"Operator_Enable_Auth_Type_MD2",
		"Possible values: Yes/No",
                0,
		operator_md2_checkout,
		operator_md2_commit,
		operator_md2_diff,
		operator_md2_validate);

  add_keyvalue (lan_conf_auth_section,
		"Operator_Enable_Auth_Type_MD5",
		"Possible values: Yes/No",
                0,
		operator_md5_checkout,
		operator_md5_commit,
		operator_md5_diff,
		operator_md5_validate);

  add_keyvalue (lan_conf_auth_section,
		"Operator_Enable_Auth_Type_Straight_Password",
		"Possible values: Yes/No",
                0,
		operator_straight_password_checkout,
		operator_straight_password_commit,
		operator_straight_password_diff,
		operator_straight_password_validate);

  add_keyvalue (lan_conf_auth_section,
		"Operator_Enable_Auth_Type_OEM_Proprietary",
		"Possible values: Yes/No",
                0,
		operator_oem_proprietary_checkout,
		operator_oem_proprietary_commit,
		operator_oem_proprietary_diff,
		operator_oem_proprietary_validate);


  add_keyvalue (lan_conf_auth_section,
		"Admin_Enable_Auth_Type_None",
		"Possible values: Yes/No",
                0,
		admin_none_checkout,
		admin_none_commit,
		admin_none_diff,
		admin_none_validate);

  add_keyvalue (lan_conf_auth_section,
		"Admin_Enable_Auth_Type_MD2",
		"Possible values: Yes/No",
                0,
		admin_md2_checkout,
		admin_md2_commit,
		admin_md2_diff,
		admin_md2_validate);

  add_keyvalue (lan_conf_auth_section,
		"Admin_Enable_Auth_Type_MD5",
		"Possible values: Yes/No",
                0,
		admin_md5_checkout,
		admin_md5_commit,
		admin_md5_diff,
		admin_md5_validate);

  add_keyvalue (lan_conf_auth_section,
		"Admin_Enable_Auth_Type_Straight_Password",
		"Possible values: Yes/No",
                0,
		admin_straight_password_checkout,
		admin_straight_password_commit,
		admin_straight_password_diff,
		admin_straight_password_validate);

  add_keyvalue (lan_conf_auth_section,
		"Admin_Enable_Auth_Type_OEM_Proprietary",
		"Possible values: Yes/No",
                0,
		admin_oem_proprietary_checkout,
		admin_oem_proprietary_commit,
		admin_oem_proprietary_diff,
		admin_oem_proprietary_validate);


  add_keyvalue (lan_conf_auth_section,
		"OEM_Enable_Auth_Type_None",
		"Possible values: Yes/No",
                0,
		oem_none_checkout,
		oem_none_commit,
		oem_none_diff,
		oem_none_validate);

  add_keyvalue (lan_conf_auth_section,
		"OEM_Enable_Auth_Type_MD2",
		"Possible values: Yes/No",
                0,
		oem_md2_checkout,
		oem_md2_commit,
		oem_md2_diff,
		oem_md2_validate);

  add_keyvalue (lan_conf_auth_section,
		"OEM_Enable_Auth_Type_MD5",
		"Possible values: Yes/No",
                0,
		oem_md5_checkout,
		oem_md5_commit,
		oem_md5_diff,
		oem_md5_validate);

  add_keyvalue (lan_conf_auth_section,
		"OEM_Enable_Auth_Type_Straight_Password",
		"Possible values: Yes/No",
                0,
		oem_straight_password_checkout,
		oem_straight_password_commit,
		oem_straight_password_diff,
		oem_straight_password_validate);

  add_keyvalue (lan_conf_auth_section,
		"OEM_Enable_Auth_Type_OEM_Proprietary",
		"Possible values: Yes/No",
                0,
		oem_oem_proprietary_checkout,
		oem_oem_proprietary_commit,
		oem_oem_proprietary_diff,
		oem_oem_proprietary_validate);

  return lan_conf_auth_section;
}
