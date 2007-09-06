#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

int
bmc_get_num_users (bmc_config_state_data_t *state_data)
{
  uint8_t users = 0;
  bmc_err_t ret;

  if ((ret = get_bmc_max_users (state_data, &users)) != BMC_ERR_SUCCESS)
    return (-1);
  return (int)users;
}

/* username */

static bmc_err_t
username_checkout (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };
  bmc_err_t ret;

  userid = atoi (sect->section_name + strlen ("User"));
  if ((ret = get_bmc_username (state_data,
                               userid,
                               username,
                               IPMI_MAX_USER_NAME_LENGTH+1)) != BMC_ERR_SUCCESS) 
    return ret;
		    
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ((char *)username)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
username_commit (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid;
  userid = atoi (sect->section_name + strlen ("User"));

  if (!kv->value)
    return BMC_ERR_FATAL_ERROR;

  return set_bmc_username (state_data,
			   userid,
			   (uint8_t *)kv->value);
}

static bmc_diff_t
username_diff (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };
  bmc_err_t rc;
  bmc_diff_t ret;

  userid = atoi (sect->section_name + strlen ("User"));
  if ((rc = get_bmc_username (state_data,
                              userid,
                              username,
                              IPMI_MAX_USER_NAME_LENGTH+1)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (userid == 1) 
    {
      if (! kv->value || same (kv->value, "null") || same (kv->value, "anonymous")) 
        ret = BMC_DIFF_SAME;
      else 
        ret = BMC_DIFF_DIFFERENT;
    } 
  else 
    {
      if (!kv->value || !same (kv->value, (char *)username))
        ret = BMC_DIFF_DIFFERENT;
      else
        ret = BMC_DIFF_SAME;
    }

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 (char *)username);
  return ret;
}

static bmc_validate_t
username_validate (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const char *value)
{
  uint8_t userid;
  userid = atoi (sect->section_name + strlen ("User"));

  if (userid == 1) 
    {
      if (!value || same (value, "null") || same (value, "anonymous"))
        return BMC_VALIDATE_VALID_VALUE;
      else
        return BMC_VALIDATE_INVALID_VALUE;
    } 

  if (!value || strlen (value) > IPMI_MAX_USER_NAME_LENGTH)
    return BMC_VALIDATE_INVALID_VALUE;
  return BMC_VALIDATE_VALID_VALUE;
}

/* enable_user */

static bmc_err_t
enable_user_checkout (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  bmc_err_t ret;
  
  if ((ret = get_bmc_user_lan_channel_access (state_data,
                                              userid,
                                              &tmp_user_ipmi_messaging,
                                              &tmp_user_link_authentication,
                                              &tmp_user_restricted_to_callback,
                                              &tmp_privilege_limit,
                                              &tmp_session_limit,
                                              &tmp_user_id_enable_status)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  /* 
   * Older IPMI implementations cannot get the value, but new ones
   * can.  If it cannot be checked out, the line will be commented out
   * later on.
   */
  if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED)
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else /* tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED */
    {
      if (!(kv->value = strdup ("")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
enable_user_commit (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  return set_bmc_enable_user (state_data,
			      userid,
			      same (kv->value, "yes"));
}

static bmc_diff_t
enable_user_diff (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_user_lan_channel_access (state_data,
                                             userid,
                                             &tmp_user_ipmi_messaging,
                                             &tmp_user_link_authentication,
                                             &tmp_user_restricted_to_callback,
                                             &tmp_privilege_limit,
                                             &tmp_session_limit,
                                             &tmp_user_id_enable_status)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  /* Cant get, assume equal */
  if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED)
    ret = BMC_DIFF_SAME;
  else
    {
      passed_val = same (kv->value, "Yes");

      if ((passed_val && tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
          || (!passed_val && tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED))
        ret = BMC_DIFF_SAME;
      else
        {
          ret = BMC_DIFF_DIFFERENT;
          report_diff (sect->section_name,
                       kv->key,
                       kv->value,
                       (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED) ? "Yes" : "No");
        }
    }

  return ret;
}

static bmc_err_t
password_checkout (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ("")))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
password_commit (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return set_bmc_user_password (state_data,
				userid, (uint8_t *)kv->value);
}

static bmc_diff_t
password_diff (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  bmc_diff_t ret;

  ret = check_bmc_user_password (state_data,
                                 userid,
                                 (uint8_t *)kv->value);

  if (ret == BMC_DIFF_FATAL_ERROR || ret == BMC_DIFF_NON_FATAL_ERROR)
    return ret;

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 "<something else>");
  return ret;
}

static bmc_validate_t
password_validate (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const char *value)
{
  if (strlen (value) <= IPMI_1_5_MAX_PASSWORD_LENGTH)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* password20 */

static bmc_err_t
password20_checkout (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  bmc_diff_t ret;

  /* achu: password can't be checked out, but we should make sure IPMI
   * 2.0 exists on the system.
   */
  if (kv->value)
    free (kv->value);

  if ((ret = check_bmc_user_password20 (state_data,
                                        userid,
                                        (uint8_t *)"foobar")) == BMC_DIFF_FATAL_ERROR)
    return BMC_ERR_FATAL_ERROR;

  if (ret == BMC_DIFF_NON_FATAL_ERROR)
    return BMC_ERR_NON_FATAL_ERROR;

  if (!(kv->value = strdup ("")))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
password20_commit (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return set_bmc_user_password20 (state_data,
				  userid,
				  (uint8_t *)kv->value);
}

static bmc_diff_t
password20_diff (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  bmc_diff_t ret;

  ret = check_bmc_user_password20 (state_data,
                                   userid,
                                   (uint8_t *)kv->value);

  if (ret == BMC_DIFF_FATAL_ERROR || ret == BMC_DIFF_NON_FATAL_ERROR)
    return ret;

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 "<something else>");
  return ret;
}

static bmc_validate_t
password20_validate (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     const char *value)
{
  if (strlen (value) <= IPMI_2_0_MAX_PASSWORD_LENGTH)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* lan_enable_ipmi_msgs */

static bmc_err_t
lan_channel_get (bmc_config_state_data_t *state_data,
		 uint8_t userid,
		 uint8_t *user_ipmi_messaging,
		 uint8_t *user_link_authentication,
		 uint8_t *user_restricted_to_callback,
		 uint8_t *privilege_limit,
		 uint8_t *session_limit)
{
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  bmc_err_t ret;
  
  if ((ret = get_bmc_user_lan_channel_access (state_data,
                                              userid,
                                              &tmp_user_ipmi_messaging,
                                              &tmp_user_link_authentication,
                                              &tmp_user_restricted_to_callback,
                                              &tmp_privilege_limit,
                                              &tmp_session_limit,
                                              &tmp_user_id_enable_status)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (user_ipmi_messaging)
    *user_ipmi_messaging = tmp_user_ipmi_messaging;
  if (user_link_authentication)
    *user_link_authentication = tmp_user_link_authentication;
  if (user_restricted_to_callback)
    *user_restricted_to_callback = tmp_user_restricted_to_callback;
  if (privilege_limit)
    *privilege_limit = tmp_privilege_limit;
  if (session_limit)
    *session_limit = tmp_session_limit;

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
lan_channel_set (bmc_config_state_data_t *state_data,
		 uint8_t userid,
		 uint8_t user_ipmi_messaging,
		 uint8_t user_ipmi_messaging_is_set,
		 uint8_t user_link_authentication,
		 uint8_t user_link_authentication_is_set,
		 uint8_t user_restricted_to_callback,
		 uint8_t user_restricted_to_callback_is_set,
		 uint8_t privilege_limit,
		 uint8_t privilege_limit_is_set,
		 uint8_t session_limit,
		 uint8_t session_limit_is_set)
{
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  bmc_err_t ret;
  
  if ((ret = get_bmc_user_lan_channel_access (state_data,
                                              userid,
                                              &tmp_user_ipmi_messaging,
                                              &tmp_user_link_authentication,
                                              &tmp_user_restricted_to_callback,
                                              &tmp_privilege_limit,
                                              &tmp_session_limit,
                                              &tmp_user_id_enable_status)) != BMC_ERR_SUCCESS)
    return ret;

  if (user_ipmi_messaging_is_set)
    tmp_user_ipmi_messaging = user_ipmi_messaging;
  if (user_link_authentication_is_set)
    tmp_user_link_authentication = user_link_authentication;
  if (user_restricted_to_callback_is_set)
    tmp_user_restricted_to_callback = user_restricted_to_callback;
  if (privilege_limit_is_set)
    tmp_privilege_limit = privilege_limit;
  if (session_limit_is_set)
    tmp_session_limit = session_limit;

  if ((ret = set_bmc_user_lan_channel_access (state_data,
                                              userid,
                                              tmp_user_ipmi_messaging,
                                              tmp_user_link_authentication,
                                              tmp_user_restricted_to_callback,
                                              tmp_privilege_limit,
                                              tmp_session_limit)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}


/* lan_enable_ipmi_msgs */

static bmc_err_t
lan_enable_ipmi_msgs_checkout (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              &get_val,
                              0,
                              0,
                              0,
                              0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
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
lan_enable_ipmi_msgs_commit (bmc_config_state_data_t *state_data,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
			  userid,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0,
			  0, 0,
			  0, 0);
}

static bmc_diff_t
lan_enable_ipmi_msgs_diff (bmc_config_state_data_t *state_data,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                              userid,
                              &get_val,
                              0,
                              0,
                              0,
                              0)) != BMC_ERR_SUCCESS) 
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}
  
/* lan_enable_link_auth */

static bmc_err_t
lan_enable_link_auth_checkout (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              0,
                              &get_val,
                              0,
                              0,
                              0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
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
lan_enable_link_auth_commit (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          same (kv->value, "yes"), 1,
                          0, 0,
                          0, 0,
                          0, 0);
}

static bmc_diff_t
lan_enable_link_auth_diff (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             &get_val,
                             0,
                             0,
                             0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* LAN_enable_restricted_to_callback */

static bmc_err_t
lan_enable_restricted_to_callback_checkout (bmc_config_state_data_t *state_data,
                                            const struct section *sect,
                                            struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              0,
                              0,
                              &get_val,
                              0,
                              0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
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
lan_enable_restricted_to_callback_commit (bmc_config_state_data_t *state_data,
                                          const struct section *sect,
                                          const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          0, 0,
                          same (kv->value, "yes"), 1,
                          0, 0,
                          0, 0);
}

static bmc_diff_t
lan_enable_restricted_to_callback_diff (bmc_config_state_data_t *state_data,
                                        const struct section *sect,
                                        const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             0,
                             &get_val,
                             0,
                             0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) 
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* privilege_limit */

static bmc_err_t
lan_privilege_limit_checkout (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              0,
                              0,
                              0,
                              &get_val,
                              0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (get_privilege_limit_string (get_val))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
lan_privilege_limit_commit (bmc_config_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          0, 0,
                          0, 0,
                          get_privilege_limit_number (kv->value), 1,
                          0, 0);
}

static bmc_diff_t
lan_privilege_limit_diff (bmc_config_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             0,
                             0,
                             &get_val,
                             0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = get_privilege_limit_number (kv->value);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_privilege_limit_string (get_val));
    }
  return ret;
}

  
/* lan_session_limit */

static bmc_err_t
lan_session_limit_checkout (bmc_config_state_data_t *state_data,
                            const struct section *sect,
                            struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  int ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              0,
                              0,
                              0,
                              0,
                              &get_val)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", get_val) < 0)
    {
      perror("asprintf");
      return BMC_ERR_NON_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
lan_session_limit_commit (bmc_config_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          0, 0,
                          0, 0,
                          0, 0,
                          strtol (kv->value, NULL, 0), 1);
}

static bmc_diff_t
lan_session_limit_diff (bmc_config_state_data_t *state_data,
                        const struct section *sect,
                        const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             0,
                             0,
                             0,
                             &get_val)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      sprintf (num, "%d", get_val);
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

/* sol_payload_access */

static bmc_err_t
sol_payload_access_checkout (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             struct keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t have_access;
  bmc_err_t ret;

  if ((ret = get_bmc_user_payload_access (state_data,
                                          userid,
                                          &have_access,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (have_access)
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
sol_payload_access_commit (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t operation;

  if (same (kv->value, "yes"))
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_ENABLE;
  else
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_DISABLE;

  return set_bmc_user_payload_access (state_data,
                                      userid,
                                      operation,
                                      1, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

static bmc_diff_t
sol_payload_access_diff (bmc_config_state_data_t *state_data,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t have_access;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_user_payload_access (state_data,
                                         userid,
                                         &have_access,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");
  
  if (passed_value == have_access)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   have_access ? "Yes" : "No");
    }
  return ret;
}

/* serial_enable_ipmi_msgs */

static bmc_err_t
serial_channel_get (bmc_config_state_data_t *state_data,
                    uint8_t userid,
                    uint8_t *user_ipmi_messaging,
                    uint8_t *user_link_authentication,
                    uint8_t *user_restricted_to_callback,
                    uint8_t *privilege_limit,
                    uint8_t *session_limit)
{
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  bmc_err_t ret;
  
  if ((ret = get_bmc_user_serial_channel_access (state_data,
                                                 userid,
                                                 &tmp_user_ipmi_messaging,
                                                 &tmp_user_link_authentication,
                                                 &tmp_user_restricted_to_callback,
                                                 &tmp_privilege_limit,
                                                 &tmp_session_limit,
                                                 &tmp_user_id_enable_status)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (user_ipmi_messaging)
    *user_ipmi_messaging = tmp_user_ipmi_messaging;
  if (user_link_authentication)
    *user_link_authentication = tmp_user_link_authentication;
  if (user_restricted_to_callback)
    *user_restricted_to_callback = tmp_user_restricted_to_callback;
  if (privilege_limit)
    *privilege_limit = tmp_privilege_limit;
  if (session_limit)
    *session_limit = tmp_session_limit;

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
serial_channel_set (bmc_config_state_data_t *state_data,
                    uint8_t userid,
                    uint8_t user_ipmi_messaging,
                    uint8_t user_ipmi_messaging_is_set,
                    uint8_t user_link_authentication,
                    uint8_t user_link_authentication_is_set,
                    uint8_t user_restricted_to_callback,
                    uint8_t user_restricted_to_callback_is_set,
                    uint8_t privilege_limit,
                    uint8_t privilege_limit_is_set,
                    uint8_t session_limit,
                    uint8_t session_limit_is_set)
{
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  bmc_err_t ret;
  
  if ((ret = get_bmc_user_serial_channel_access (state_data,
                                                 userid,
                                                 &tmp_user_ipmi_messaging,
                                                 &tmp_user_link_authentication,
                                                 &tmp_user_restricted_to_callback,
                                                 &tmp_privilege_limit,
                                                 &tmp_session_limit,
                                                 &tmp_user_id_enable_status)) != BMC_ERR_SUCCESS)
    return ret;

  if (user_ipmi_messaging_is_set)
    tmp_user_ipmi_messaging = user_ipmi_messaging;
  if (user_link_authentication_is_set)
    tmp_user_link_authentication = user_link_authentication;
  if (user_restricted_to_callback_is_set)
    tmp_user_restricted_to_callback = user_restricted_to_callback;
  if (privilege_limit_is_set)
    tmp_privilege_limit = privilege_limit;
  if (session_limit_is_set)
    tmp_session_limit = session_limit;

  ret = set_bmc_user_serial_channel_access (state_data,
                                            userid,
                                            tmp_user_ipmi_messaging,
                                            tmp_user_link_authentication,
                                            tmp_user_restricted_to_callback,
                                            tmp_privilege_limit,
                                            tmp_session_limit);

  return ret;
}


/* serial_enable_ipmi_msgs */

static bmc_err_t
serial_enable_ipmi_msgs_checkout (bmc_config_state_data_t *state_data,
                                  const struct section *sect,
                                  struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;
  
  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 &get_val,
                                 0,
                                 0,
                                 0,
                                 0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
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
serial_enable_ipmi_msgs_commit (bmc_config_state_data_t *state_data,
                                const struct section *sect,
                                const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             same (kv->value, "yes"), 1,
                             0, 0,
                             0, 0,
                             0, 0,
                             0, 0);
}

static bmc_diff_t
serial_enable_ipmi_msgs_diff (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                &get_val,
                                0,
                                0,
                                0,
                                0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}
  
/* serial_enable_link_auth */

static bmc_err_t
serial_enable_link_auth_checkout (bmc_config_state_data_t *state_data,
                                  const struct section *sect,
                                  struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;

  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 &get_val,
                                 0,
                                 0,
                                 0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
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
serial_enable_link_auth_commit (bmc_config_state_data_t *state_data,
                                const struct section *sect,
                                const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             same (kv->value, "yes"), 1,
                             0, 0,
                             0, 0,
                             0, 0);
}

static bmc_diff_t
serial_enable_link_auth_diff (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                &get_val,
                                0,
                                0,
                                0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* serial_enable_restricted_to_callback */

static bmc_err_t
serial_enable_restricted_to_callback_checkout (bmc_config_state_data_t *state_data,
                                               const struct section *sect,
                                               struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;

  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 0,
                                 &get_val,
                                 0,
                                 0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
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
serial_enable_restricted_to_callback_commit (bmc_config_state_data_t *state_data,
                                             const struct section *sect,
                                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             0, 0,
                             same (kv->value, "yes"), 1,
                             0, 0,
                             0, 0);
}

static bmc_diff_t
serial_enable_restricted_to_callback_diff (bmc_config_state_data_t *state_data,
                                           const struct section *sect,
                                           const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                0,
                                &get_val,
                                0,
                                0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* privilege_limit */

static bmc_err_t
serial_privilege_limit_checkout (bmc_config_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;

  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 0,
                                 0,
                                 &get_val,
                                 0)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (get_privilege_limit_string (get_val))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
serial_privilege_limit_commit (bmc_config_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             0, 0,
                             0, 0,
                             get_privilege_limit_number (kv->value), 1,
                             0, 0);
}

static bmc_diff_t
serial_privilege_limit_diff (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                0,
                                0,
                                &get_val,
                                0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = get_privilege_limit_number (kv->value);
  
  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_privilege_limit_string (get_val));
    }
  return ret;
}

  
/* serial_session_limit */

static bmc_err_t
serial_session_limit_checkout (bmc_config_state_data_t *state_data,
                               const struct section *sect,
                               struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  bmc_err_t ret;
  
  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 0,
                                 0,
                                 0,
                                 &get_val)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (kv->value)
    free (kv->value);
  
  if (asprintf (&kv->value, "%d", get_val) < 0)
    {
      perror("asprintf");
      return BMC_ERR_NON_FATAL_ERROR;
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
serial_session_limit_commit (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             0, 0,
                             0, 0,
                             0, 0,
                             strtol (kv->value, NULL, 0), 1);
}

static bmc_diff_t
serial_session_limit_diff (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  unsigned long int passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                0,
                                0,
                                0,
                                &get_val)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

struct section *
bmc_user_section_get (bmc_config_state_data_t *state_data, int userid)
{
  struct section *user_section = NULL;
  char buf[64];

  if (userid <= 0)
    {
      fprintf(stderr, "Invalid Userid = %d\n", userid);
      return NULL;
    }

  snprintf(buf, 64, "User%d", userid);

  if (!(user_section = bmc_config_section_create(state_data, 
                                                 buf,
                                                 NULL,
                                                 0)))
    goto cleanup;

  /* userid 1 is the NULL username, so comment it out by default */
  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Username",
                                       "Give Username",
                                       (userid == 1) ? BMC_CHECKOUT_KEY_COMMENTED_OUT : 0,
                                       username_checkout,
                                       username_commit,
                                       username_diff,
                                       username_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Enable_User",
                                       "Possible values: Yes/No or blank to not set",
                                       BMC_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                                       enable_user_checkout,
                                       enable_user_commit,
                                       enable_user_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Password",
                                       "Give password or blank to clear. MAX 16 chars.",
                                       BMC_CHECKOUT_KEY_COMMENTED_OUT,
                                       password_checkout,
                                       password_commit,
                                       password_diff,
                                       password_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Password20",
                                       "Give password for IPMI 2.0 or blank to clear. MAX 20 chars.",
                                       BMC_CHECKOUT_KEY_COMMENTED_OUT,
                                       password20_checkout,
                                       password20_commit,
                                       password20_diff,
                                       password20_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Lan_Enable_IPMI_Msgs",
                                       "Possible values: Yes/No",
                                       0,
                                       lan_enable_ipmi_msgs_checkout,
                                       lan_enable_ipmi_msgs_commit,
                                       lan_enable_ipmi_msgs_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Lan_Enable_Link_Auth",
                                       "Possible values: Yes/No",
                                       0,
                                       lan_enable_link_auth_checkout,
                                       lan_enable_link_auth_commit,
                                       lan_enable_link_auth_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Lan_Enable_Restricted_to_Callback",
                                       "Possible values: Yes/No",
                                       0,
                                       lan_enable_restricted_to_callback_checkout,
                                       lan_enable_restricted_to_callback_commit,
                                       lan_enable_restricted_to_callback_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Lan_Enable_Restrict_to_Callback",
                                       "Possible values: Yes/No",
                                       BMC_DO_NOT_CHECKOUT,
                                       lan_enable_restricted_to_callback_checkout,
                                       lan_enable_restricted_to_callback_commit,
                                       lan_enable_restricted_to_callback_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Lan_Privilege_Limit",
                                       "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                                       0,
                                       lan_privilege_limit_checkout,
                                       lan_privilege_limit_commit,
                                       lan_privilege_limit_diff,
                                       get_privilege_limit_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Lan_Session_Limit",
                                       "Possible values: 0-255, 0 is unlimited",
                                       BMC_DO_NOT_CHECKOUT,
                                       lan_session_limit_checkout,
                                       lan_session_limit_commit,
                                       lan_session_limit_diff,
                                       number_range_one_byte) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "SOL_Payload_Access",
                                       "Possible values: Yes/No",
                                       0,
                                       sol_payload_access_checkout,
                                       sol_payload_access_commit,
                                       sol_payload_access_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Serial_Enable_IPMI_Msgs",
                                       "Possible values: Yes/No",
                                       0,
                                       serial_enable_ipmi_msgs_checkout,
                                       serial_enable_ipmi_msgs_commit,
                                       serial_enable_ipmi_msgs_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Serial_Enable_Link_Auth",
                                       "Possible values: Yes/No",
                                       0,
                                       serial_enable_link_auth_checkout,
                                       serial_enable_link_auth_commit,
                                       serial_enable_link_auth_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Serial_Enable_Restricted_to_Callback",
                                       "Possible values: Yes/No",
                                       0,
                                       serial_enable_restricted_to_callback_checkout,
                                       serial_enable_restricted_to_callback_commit,
                                       serial_enable_restricted_to_callback_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Serial_Enable_Restrict_to_Callback",
                                       "Possible values: Yes/No",
                                       BMC_DO_NOT_CHECKOUT,
                                       serial_enable_restricted_to_callback_checkout,
                                       serial_enable_restricted_to_callback_commit,
                                       serial_enable_restricted_to_callback_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Serial_Privilege_Limit",
                                       "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                                       0,
                                       serial_privilege_limit_checkout,
                                       serial_privilege_limit_commit,
                                       serial_privilege_limit_diff,
                                       get_privilege_limit_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Serial_Session_Limit",
                                       "Possible values: 0-255, 0 is unlimited",
                                       BMC_DO_NOT_CHECKOUT,
                                       serial_session_limit_checkout,
                                       serial_session_limit_commit,
                                       serial_session_limit_diff,
                                       number_range_one_byte) < 0)
    goto cleanup;

  return user_section;

 cleanup:
  if (user_section)
    bmc_config_section_destroy(state_data, user_section);
  return NULL;
}
