#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-sections.h"

/* callback_none */

static bmc_err_t
callback_none_checkout (const struct bmc_config_arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_none)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
callback_none_commit (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.callback.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
callback_none_diff (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.callback.type_none == same (kv->value, "yes")) 
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.callback.type_none ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
callback_none_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{  
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* callback_md2 */

static bmc_err_t
callback_md2_checkout (const struct bmc_config_arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_md2)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
callback_md2_commit (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.callback.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
callback_md2_diff (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.callback.type_md2 == same (kv->value, "yes")) 
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.callback.type_md2 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
callback_md2_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* callback_md5 */

static bmc_err_t
callback_md5_checkout (const struct bmc_config_arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_md5)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
callback_md5_commit (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.callback.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
callback_md5_diff (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.callback.type_md5 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.callback.type_md5 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
callback_md5_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* callback_straight_password */

static bmc_err_t
callback_straight_password_checkout (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_straight_password)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
callback_straight_password_commit (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.callback.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
callback_straight_password_diff (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.callback.type_straight_password == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.callback.type_straight_password ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
callback_straight_password_validate (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* callback_oem_proprietary */

static bmc_err_t
callback_oem_proprietary_checkout (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.callback.type_oem_proprietary)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
callback_oem_proprietary_commit (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.callback.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
callback_oem_proprietary_diff (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.callback.type_oem_proprietary == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.callback.type_oem_proprietary ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
callback_oem_proprietary_validate (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* user */

/* user_none */

static bmc_err_t
user_none_checkout (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS) 
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_none)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
user_none_commit (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.user.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
user_none_diff (const struct bmc_config_arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.user.type_none == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.user.type_none ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
user_none_validate (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* user_md2 */

static bmc_err_t
user_md2_checkout (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_md2)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
user_md2_commit (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.user.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
user_md2_diff (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.user.type_md2 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.user.type_md2 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
user_md2_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* user_md5 */

static bmc_err_t
user_md5_checkout (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_md5)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
user_md5_commit (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.user.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
user_md5_diff (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.user.type_md5 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.user.type_md5 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
user_md5_validate (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* user_straight_password */

static bmc_err_t
user_straight_password_checkout (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_straight_password)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
user_straight_password_commit (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.user.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
user_straight_password_diff (const struct bmc_config_arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.user.type_straight_password == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.user.type_straight_password ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
user_straight_password_validate (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* user_oem_proprietary */

static bmc_err_t
user_oem_proprietary_checkout (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.user.type_oem_proprietary)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
user_oem_proprietary_commit (const struct bmc_config_arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.user.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
user_oem_proprietary_diff (const struct bmc_config_arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.user.type_oem_proprietary == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.user.type_oem_proprietary ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
user_oem_proprietary_validate (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* operator */

/* operator_none */

static bmc_err_t
operator_none_checkout (const struct bmc_config_arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_none)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
operator_none_commit (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.operator.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
operator_none_diff (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.operator.type_none == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.operator.type_none ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
operator_none_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* operator_md2 */

static bmc_err_t
operator_md2_checkout (const struct bmc_config_arguments *args,
		       const struct section *sect,
		       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_md2)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
operator_md2_commit (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.operator.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
operator_md2_diff (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.operator.type_md2 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.operator.type_md2 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
operator_md2_validate (const struct bmc_config_arguments *args,
		       const struct section *sect,
		       const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* operator_md5 */

static bmc_err_t
operator_md5_checkout (const struct bmc_config_arguments *args,
		       const struct section *sect,
		       struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_md5)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
operator_md5_commit (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.operator.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
operator_md5_diff (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.operator.type_md5 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.operator.type_md5 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
operator_md5_validate (const struct bmc_config_arguments *args,
		       const struct section *sect,
		       const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* operator_straight_password */

static bmc_err_t
operator_straight_password_checkout (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_straight_password)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
operator_straight_password_commit (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.operator.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
operator_straight_password_diff (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.operator.type_straight_password == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.operator.type_straight_password ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
operator_straight_password_validate (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* operator_oem_proprietary */

static bmc_err_t
operator_oem_proprietary_checkout (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.operator.type_oem_proprietary)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
operator_oem_proprietary_commit (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.operator.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
operator_oem_proprietary_diff (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.operator.type_oem_proprietary == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.operator.type_oem_proprietary ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
operator_oem_proprietary_validate (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* admin */


/* admin_none */

static bmc_err_t
admin_none_checkout (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_none)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
admin_none_commit (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.admin.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
admin_none_diff (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.admin.type_none == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.admin.type_none ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
admin_none_validate (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* admin_md2 */

static bmc_err_t
admin_md2_checkout (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_md2)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
admin_md2_commit (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.admin.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
admin_md2_diff (const struct bmc_config_arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.admin.type_md2 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.admin.type_md2 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
admin_md2_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* admin_md5 */

static bmc_err_t
admin_md5_checkout (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_md5)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
admin_md5_commit (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.admin.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
admin_md5_diff (const struct bmc_config_arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.admin.type_md5 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.admin.type_md5 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
admin_md5_validate (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* admin_straight_password */

static bmc_err_t
admin_straight_password_checkout (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_straight_password)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
admin_straight_password_commit (const struct bmc_config_arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.admin.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
admin_straight_password_diff (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.admin.type_straight_password == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.admin.type_straight_password ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
admin_straight_password_validate (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* admin_oem_proprietary */

static bmc_err_t
admin_oem_proprietary_checkout (const struct bmc_config_arguments *args,
				const struct section *sect,
				struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.admin.type_oem_proprietary)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
admin_oem_proprietary_commit (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.admin.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
admin_oem_proprietary_diff (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.admin.type_oem_proprietary == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.admin.type_oem_proprietary ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
admin_oem_proprietary_validate (const struct bmc_config_arguments *args,
				const struct section *sect,
				const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* oem */

/* oem_none */

static bmc_err_t
oem_none_checkout (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_none)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
oem_none_commit (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.oem.type_none = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
oem_none_diff (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.oem.type_none == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.oem.type_none ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
oem_none_validate (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* oem_md2 */

static bmc_err_t
oem_md2_checkout (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_md2)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
oem_md2_commit (const struct bmc_config_arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.oem.type_md2 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
oem_md2_diff (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.oem.type_md2 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.oem.type_md2 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
oem_md2_validate (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* oem_md5 */

static bmc_err_t
oem_md5_checkout (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_md5)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
oem_md5_commit (const struct bmc_config_arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.oem.type_md5 = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
oem_md5_diff (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.oem.type_md5 == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.oem.type_md5 ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
oem_md5_validate (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* oem_straight_password */

static bmc_err_t
oem_straight_password_checkout (const struct bmc_config_arguments *args,
				const struct section *sect,
				struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_straight_password)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
oem_straight_password_commit (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.oem.type_straight_password = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
oem_straight_password_diff (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.oem.type_straight_password == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.oem.type_straight_password ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
oem_straight_password_validate (const struct bmc_config_arguments *args,
				const struct section *sect,
				const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* oem_oem_proprietary */

static bmc_err_t
oem_oem_proprietary_checkout (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (auth.oem.type_oem_proprietary)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
oem_oem_proprietary_commit (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                           &auth)) != BMC_ERR_SUCCESS)
    return ret;

  auth.oem.type_oem_proprietary = same (kv->value, "yes");

  return set_bmc_lan_conf_authentication_type_enables (args->dev,
						       &auth);
}

static bmc_diff_t
oem_oem_proprietary_diff (const struct bmc_config_arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  struct bmc_authentication_level auth;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_authentication_type_enables (args->dev,
                                                          &auth)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (auth.oem.type_oem_proprietary == same (kv->value, "yes"))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   auth.oem.type_oem_proprietary ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t 
oem_oem_proprietary_validate (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

struct section *
bmc_lan_conf_auth_section_get (struct bmc_config_arguments *args)
{
  struct section *lan_conf_auth_section = NULL;

  if (!(lan_conf_auth_section = bmc_section_create("Lan_Conf_Auth")))
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Callback_Enable_Auth_Type_None",
				"Possible values: Yes/No",
				0,
				callback_none_checkout,
				callback_none_commit,
				callback_none_diff,
				callback_none_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Callback_Enable_Auth_Type_MD2",
				"Possible values: Yes/No",
				0,
				callback_md2_checkout,
				callback_md2_commit,
				callback_md2_diff,
				callback_md2_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Callback_Enable_Auth_Type_MD5",
				"Possible values: Yes/No",
				0,
				callback_md5_checkout,
				callback_md5_commit,
				callback_md5_diff,
				callback_md5_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Callback_Enable_Auth_Type_Straight_Password",
				"Possible values: Yes/No",
				0,
				callback_straight_password_checkout,
				callback_straight_password_commit,
				callback_straight_password_diff,
				callback_straight_password_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Callback_Enable_Auth_Type_OEM_Proprietary",
				"Possible values: Yes/No",
				0,
				callback_oem_proprietary_checkout,
				callback_oem_proprietary_commit,
				callback_oem_proprietary_diff,
				callback_oem_proprietary_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"User_Enable_Auth_Type_None",
				"Possible values: Yes/No",
				0,
				user_none_checkout,
				user_none_commit,
				user_none_diff,
				user_none_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"User_Enable_Auth_Type_MD2",
				"Possible values: Yes/No",
				0,
				user_md2_checkout,
				user_md2_commit,
				user_md2_diff,
				user_md2_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"User_Enable_Auth_Type_MD5",
				"Possible values: Yes/No",
				0,
				user_md5_checkout,
				user_md5_commit,
				user_md5_diff,
				user_md5_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"User_Enable_Auth_Type_Straight_Password",
				"Possible values: Yes/No",
				0,
				user_straight_password_checkout,
				user_straight_password_commit,
				user_straight_password_diff,
				user_straight_password_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"User_Enable_Auth_Type_OEM_Proprietary",
				"Possible values: Yes/No",
				0,
				user_oem_proprietary_checkout,
				user_oem_proprietary_commit,
				user_oem_proprietary_diff,
				user_oem_proprietary_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Operator_Enable_Auth_Type_None",
				"Possible values: Yes/No",
				0,
				operator_none_checkout,
				operator_none_commit,
				operator_none_diff,
				operator_none_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Operator_Enable_Auth_Type_MD2",
				"Possible values: Yes/No",
				0,
				operator_md2_checkout,
				operator_md2_commit,
				operator_md2_diff,
				operator_md2_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Operator_Enable_Auth_Type_MD5",
				"Possible values: Yes/No",
				0,
				operator_md5_checkout,
				operator_md5_commit,
				operator_md5_diff,
				operator_md5_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Operator_Enable_Auth_Type_Straight_Password",
				"Possible values: Yes/No",
				0,
				operator_straight_password_checkout,
				operator_straight_password_commit,
				operator_straight_password_diff,
				operator_straight_password_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Operator_Enable_Auth_Type_OEM_Proprietary",
				"Possible values: Yes/No",
				0,
				operator_oem_proprietary_checkout,
				operator_oem_proprietary_commit,
				operator_oem_proprietary_diff,
				operator_oem_proprietary_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Admin_Enable_Auth_Type_None",
				"Possible values: Yes/No",
				0,
				admin_none_checkout,
				admin_none_commit,
				admin_none_diff,
				admin_none_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Admin_Enable_Auth_Type_MD2",
				"Possible values: Yes/No",
				0,
				admin_md2_checkout,
				admin_md2_commit,
				admin_md2_diff,
				admin_md2_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Admin_Enable_Auth_Type_MD5",
				"Possible values: Yes/No",
				0,
				admin_md5_checkout,
				admin_md5_commit,
				admin_md5_diff,
				admin_md5_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Admin_Enable_Auth_Type_Straight_Password",
				"Possible values: Yes/No",
				0,
				admin_straight_password_checkout,
				admin_straight_password_commit,
				admin_straight_password_diff,
				admin_straight_password_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"Admin_Enable_Auth_Type_OEM_Proprietary",
				"Possible values: Yes/No",
				0,
				admin_oem_proprietary_checkout,
				admin_oem_proprietary_commit,
				admin_oem_proprietary_diff,
				admin_oem_proprietary_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"OEM_Enable_Auth_Type_None",
				"Possible values: Yes/No",
				0,
				oem_none_checkout,
				oem_none_commit,
				oem_none_diff,
				oem_none_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"OEM_Enable_Auth_Type_MD2",
				"Possible values: Yes/No",
				0,
				oem_md2_checkout,
				oem_md2_commit,
				oem_md2_diff,
				oem_md2_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"OEM_Enable_Auth_Type_MD5",
				"Possible values: Yes/No",
				0,
				oem_md5_checkout,
				oem_md5_commit,
				oem_md5_diff,
				oem_md5_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"OEM_Enable_Auth_Type_Straight_Password",
				"Possible values: Yes/No",
				0,
				oem_straight_password_checkout,
				oem_straight_password_commit,
				oem_straight_password_diff,
				oem_straight_password_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_auth_section,
				"OEM_Enable_Auth_Type_OEM_Proprietary",
				"Possible values: Yes/No",
				0,
				oem_oem_proprietary_checkout,
				oem_oem_proprietary_commit,
				oem_oem_proprietary_diff,
				oem_oem_proprietary_validate) < 0)
    goto cleanup;

  return lan_conf_auth_section;

 cleanup:
  if (lan_conf_auth_section)
    bmc_section_destroy(lan_conf_auth_section);
  return NULL;
}
