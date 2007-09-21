#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

/* username */

static config_err_t
username_checkout (bmc_config_state_data_t *state_data,
		   const struct config_section *sect,
		   struct config_keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };
  config_err_t ret;

  userid = atoi (sect->section_name + strlen ("User"));
  if ((ret = get_bmc_username (state_data,
                               userid,
                               username,
                               IPMI_MAX_USER_NAME_LENGTH+1)) != CONFIG_ERR_SUCCESS) 
    return ret;
		    
  if (!(kv->value_output = strdup ((char *)username)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
username_commit (bmc_config_state_data_t *state_data,
		 const struct config_section *sect,
		 const struct config_keyvalue *kv)
{
  uint8_t userid;
  userid = atoi (sect->section_name + strlen ("User"));

  if (!kv->value_input)
    return CONFIG_ERR_FATAL_ERROR;

  return set_bmc_username (state_data,
			   userid,
			   (uint8_t *)kv->value_input);
}

static bmc_diff_t
username_diff (bmc_config_state_data_t *state_data,
	       const struct config_section *sect,
	       const struct config_keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };
  config_err_t rc;
  bmc_diff_t ret;

  userid = atoi (sect->section_name + strlen ("User"));
  if ((rc = get_bmc_username (state_data,
                              userid,
                              username,
                              IPMI_MAX_USER_NAME_LENGTH+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (userid == 1) 
    {
      if (! kv->value_input || same (kv->value_input, "null") || same (kv->value_input, "anonymous")) 
        ret = BMC_DIFF_SAME;
      else 
        ret = BMC_DIFF_DIFFERENT;
    } 
  else 
    {
      if (!kv->value_input || !same (kv->value_input, (char *)username))
        ret = BMC_DIFF_DIFFERENT;
      else
        ret = BMC_DIFF_SAME;
    }

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value_input,
		 (char *)username);
  return ret;
}

static config_validate_t
username_validate (const char *section_name,
                   const char *key_name,
		   const char *value)
{
  uint8_t userid;
  userid = atoi (section_name + strlen ("User"));

  if (userid == 1) 
    {
      if (!value || same (value, "null") || same (value, "anonymous"))
        return CONFIG_VALIDATE_VALID_VALUE;
      else
        return CONFIG_VALIDATE_INVALID_VALUE;
    } 

  if (!value || strlen (value) > IPMI_MAX_USER_NAME_LENGTH)
    return CONFIG_VALIDATE_INVALID_VALUE;
  return CONFIG_VALIDATE_VALID_VALUE;
}

/* enable_user */

static config_err_t
enable_user_checkout (bmc_config_state_data_t *state_data,
		      const struct config_section *sect,
		      struct config_keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  config_err_t ret;
  
  if ((ret = get_bmc_user_lan_channel_access (state_data,
                                              userid,
                                              &tmp_user_ipmi_messaging,
                                              &tmp_user_link_authentication,
                                              &tmp_user_restricted_to_callback,
                                              &tmp_privilege_limit,
                                              &tmp_session_limit,
                                              &tmp_user_id_enable_status)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* 
   * Older IPMI implementations cannot get the value, but new ones
   * can.  If it cannot be checked out, the line will be commented out
   * later on.
   */
  if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED)
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else /* tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED */
    {
      if (!(kv->value_output = strdup ("")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_user_commit (bmc_config_state_data_t *state_data,
		    const struct config_section *sect,
		    const struct config_keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  return set_bmc_enable_user (state_data,
			      userid,
			      same (kv->value_input, "yes"));
}

static bmc_diff_t
enable_user_diff (bmc_config_state_data_t *state_data,
		  const struct config_section *sect,
		  const struct config_keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_user_lan_channel_access (state_data,
                                             userid,
                                             &tmp_user_ipmi_messaging,
                                             &tmp_user_link_authentication,
                                             &tmp_user_restricted_to_callback,
                                             &tmp_privilege_limit,
                                             &tmp_session_limit,
                                             &tmp_user_id_enable_status)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  /* Cant get, assume equal */
  if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED)
    ret = BMC_DIFF_SAME;
  else
    {
      passed_val = same (kv->value_input, "Yes");

      if ((passed_val && tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
          || (!passed_val && tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED))
        ret = BMC_DIFF_SAME;
      else
        {
          ret = BMC_DIFF_DIFFERENT;
          report_diff (sect->section_name,
                       kv->key,
                       kv->value_input,
                       (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED) ? "Yes" : "No");
        }
    }

  return ret;
}

static config_err_t
password_checkout (bmc_config_state_data_t *state_data,
		   const struct config_section *sect,
		   struct config_keyvalue *kv)
{
  if (!(kv->value_output = strdup ("")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
password_commit (bmc_config_state_data_t *state_data,
		 const struct config_section *sect,
		 const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return set_bmc_user_password (state_data,
				userid, (uint8_t *)kv->value_input);
}

static bmc_diff_t
password_diff (bmc_config_state_data_t *state_data,
	       const struct config_section *sect,
	       const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  bmc_diff_t ret;

  ret = check_bmc_user_password (state_data,
                                 userid,
                                 (uint8_t *)kv->value_input);

  if (ret == BMC_DIFF_FATAL_ERROR || ret == BMC_DIFF_NON_FATAL_ERROR)
    return ret;

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value_input,
		 "<something else>");
  return ret;
}

static config_validate_t
password_validate (const char *section_name,
                   const char *key_name,
		   const char *value)
{
  if (strlen (value) <= IPMI_1_5_MAX_PASSWORD_LENGTH)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

/* password20 */

static config_err_t
password20_checkout (bmc_config_state_data_t *state_data,
		     const struct config_section *sect,
		     struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  bmc_diff_t ret;

  /* achu: password can't be checked out, but we should make sure IPMI
   * 2.0 exists on the system.
   */
  if ((ret = check_bmc_user_password20 (state_data,
                                        userid,
                                        (uint8_t *)"foobar")) == BMC_DIFF_FATAL_ERROR)
    return CONFIG_ERR_FATAL_ERROR;

  if (ret == BMC_DIFF_NON_FATAL_ERROR)
    return CONFIG_ERR_NON_FATAL_ERROR;

  if (!(kv->value_output = strdup ("")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
password20_commit (bmc_config_state_data_t *state_data,
		   const struct config_section *sect,
		   const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return set_bmc_user_password20 (state_data,
				  userid,
				  (uint8_t *)kv->value_input);
}

static bmc_diff_t
password20_diff (bmc_config_state_data_t *state_data,
		 const struct config_section *sect,
		 const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  bmc_diff_t ret;

  ret = check_bmc_user_password20 (state_data,
                                   userid,
                                   (uint8_t *)kv->value_input);

  if (ret == BMC_DIFF_FATAL_ERROR || ret == BMC_DIFF_NON_FATAL_ERROR)
    return ret;

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value_input,
		 "<something else>");
  return ret;
}

static config_validate_t
password20_validate (const char *section_name,
                     const char *key_name,
		     const char *value)
{
  if (strlen (value) <= IPMI_2_0_MAX_PASSWORD_LENGTH)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

/* lan_enable_ipmi_msgs */

static config_err_t
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
  config_err_t ret;
  
  if ((ret = get_bmc_user_lan_channel_access (state_data,
                                              userid,
                                              &tmp_user_ipmi_messaging,
                                              &tmp_user_link_authentication,
                                              &tmp_user_restricted_to_callback,
                                              &tmp_privilege_limit,
                                              &tmp_session_limit,
                                              &tmp_user_id_enable_status)) != CONFIG_ERR_SUCCESS)
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

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
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
  config_err_t ret;
  
  if ((ret = get_bmc_user_lan_channel_access (state_data,
                                              userid,
                                              &tmp_user_ipmi_messaging,
                                              &tmp_user_link_authentication,
                                              &tmp_user_restricted_to_callback,
                                              &tmp_privilege_limit,
                                              &tmp_session_limit,
                                              &tmp_user_id_enable_status)) != CONFIG_ERR_SUCCESS)
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
                                              tmp_session_limit)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}


/* lan_enable_ipmi_msgs */

static config_err_t
lan_enable_ipmi_msgs_checkout (bmc_config_state_data_t *state_data,
			       const struct config_section *sect,
			       struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              &get_val,
                              0,
                              0,
                              0,
                              0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (get_val)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_enable_ipmi_msgs_commit (bmc_config_state_data_t *state_data,
			     const struct config_section *sect,
			     const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
			  userid,
			  same (kv->value_input, "yes"), 1,
			  0, 0,
			  0, 0,
			  0, 0,
			  0, 0);
}

static bmc_diff_t
lan_enable_ipmi_msgs_diff (bmc_config_state_data_t *state_data,
			   const struct config_section *sect,
			   const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                              userid,
                              &get_val,
                              0,
                              0,
                              0,
                              0)) != CONFIG_ERR_SUCCESS) 
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value_input, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_val ? "Yes" : "No");
    }
  return ret;
}
  
/* lan_enable_link_auth */

static config_err_t
lan_enable_link_auth_checkout (bmc_config_state_data_t *state_data,
			       const struct config_section *sect,
			       struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              0,
                              &get_val,
                              0,
                              0,
                              0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (get_val)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_enable_link_auth_commit (bmc_config_state_data_t *state_data,
                             const struct config_section *sect,
                             const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          same (kv->value_input, "yes"), 1,
                          0, 0,
                          0, 0,
                          0, 0);
}

static bmc_diff_t
lan_enable_link_auth_diff (bmc_config_state_data_t *state_data,
                           const struct config_section *sect,
                           const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             &get_val,
                             0,
                             0,
                             0)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value_input, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* LAN_enable_restricted_to_callback */

static config_err_t
lan_enable_restricted_to_callback_checkout (bmc_config_state_data_t *state_data,
                                            const struct config_section *sect,
                                            struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              0,
                              0,
                              &get_val,
                              0,
                              0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (get_val)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_enable_restricted_to_callback_commit (bmc_config_state_data_t *state_data,
                                          const struct config_section *sect,
                                          const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          0, 0,
                          same (kv->value_input, "yes"), 1,
                          0, 0,
                          0, 0);
}

static bmc_diff_t
lan_enable_restricted_to_callback_diff (bmc_config_state_data_t *state_data,
                                        const struct config_section *sect,
                                        const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             0,
                             &get_val,
                             0,
                             0)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value_input, "Yes");

  if (passed_val == get_val) 
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* privilege_limit */

static config_err_t
lan_privilege_limit_checkout (bmc_config_state_data_t *state_data,
                              const struct config_section *sect,
                              struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;

  if ((ret = lan_channel_get (state_data,
                              userid,
                              0,
                              0,
                              0,
                              &get_val,
                              0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (get_privilege_limit_string (get_val))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_privilege_limit_commit (bmc_config_state_data_t *state_data,
                            const struct config_section *sect,
                            const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          0, 0,
                          0, 0,
                          get_privilege_limit_number (kv->value_input), 1,
                          0, 0);
}

static bmc_diff_t
lan_privilege_limit_diff (bmc_config_state_data_t *state_data,
                          const struct config_section *sect,
                          const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             0,
                             0,
                             &get_val,
                             0)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = get_privilege_limit_number (kv->value_input);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_privilege_limit_string (get_val));
    }
  return ret;
}

  
/* lan_session_limit */

static config_err_t
lan_session_limit_checkout (bmc_config_state_data_t *state_data,
                            const struct config_section *sect,
                            struct config_keyvalue *kv)
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
                              &get_val)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", get_val) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_NON_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_session_limit_commit (bmc_config_state_data_t *state_data,
                          const struct config_section *sect,
                          const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return lan_channel_set (state_data,
                          userid,
                          0, 0,
                          0, 0,
                          0, 0,
                          0, 0,
                          strtol (kv->value_input, NULL, 0), 1);
}

static bmc_diff_t
lan_session_limit_diff (bmc_config_state_data_t *state_data,
                        const struct config_section *sect,
                        const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = lan_channel_get (state_data,
                             userid,
                             0,
                             0,
                             0,
                             0,
                             &get_val)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value_input);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      sprintf (num, "%d", get_val);
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

/* sol_payload_access */

static config_err_t
sol_payload_access_checkout (bmc_config_state_data_t *state_data,
                             const struct config_section *sect,
                             struct config_keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t have_access;
  config_err_t ret;

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
                                          NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (have_access)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sol_payload_access_commit (bmc_config_state_data_t *state_data,
                           const struct config_section *sect,
                           const struct config_keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t operation;

  if (same (kv->value_input, "yes"))
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
                         const struct config_section *sect,
                         const struct config_keyvalue *kv)
{
  int userid = atoi (sect->section_name + strlen ("User"));
  uint8_t have_access;
  uint8_t passed_value;
  config_err_t rc;
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
                                         NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes");
  
  if (passed_value == have_access)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   have_access ? "Yes" : "No");
    }
  return ret;
}

/* serial_enable_ipmi_msgs */

static config_err_t
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
  config_err_t ret;
  
  if ((ret = get_bmc_user_serial_channel_access (state_data,
                                                 userid,
                                                 &tmp_user_ipmi_messaging,
                                                 &tmp_user_link_authentication,
                                                 &tmp_user_restricted_to_callback,
                                                 &tmp_privilege_limit,
                                                 &tmp_session_limit,
                                                 &tmp_user_id_enable_status)) != CONFIG_ERR_SUCCESS)
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

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
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
  config_err_t ret;
  
  if ((ret = get_bmc_user_serial_channel_access (state_data,
                                                 userid,
                                                 &tmp_user_ipmi_messaging,
                                                 &tmp_user_link_authentication,
                                                 &tmp_user_restricted_to_callback,
                                                 &tmp_privilege_limit,
                                                 &tmp_session_limit,
                                                 &tmp_user_id_enable_status)) != CONFIG_ERR_SUCCESS)
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

static config_err_t
serial_enable_ipmi_msgs_checkout (bmc_config_state_data_t *state_data,
                                  const struct config_section *sect,
                                  struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;
  
  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 &get_val,
                                 0,
                                 0,
                                 0,
                                 0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (get_val)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_enable_ipmi_msgs_commit (bmc_config_state_data_t *state_data,
                                const struct config_section *sect,
                                const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             same (kv->value_input, "yes"), 1,
                             0, 0,
                             0, 0,
                             0, 0,
                             0, 0);
}

static bmc_diff_t
serial_enable_ipmi_msgs_diff (bmc_config_state_data_t *state_data,
                              const struct config_section *sect,
                              const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                &get_val,
                                0,
                                0,
                                0,
                                0)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value_input, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_val ? "Yes" : "No");
    }
  return ret;
}
  
/* serial_enable_link_auth */

static config_err_t
serial_enable_link_auth_checkout (bmc_config_state_data_t *state_data,
                                  const struct config_section *sect,
                                  struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;

  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 &get_val,
                                 0,
                                 0,
                                 0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (get_val)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_enable_link_auth_commit (bmc_config_state_data_t *state_data,
                                const struct config_section *sect,
                                const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             same (kv->value_input, "yes"), 1,
                             0, 0,
                             0, 0,
                             0, 0);
}

static bmc_diff_t
serial_enable_link_auth_diff (bmc_config_state_data_t *state_data,
                              const struct config_section *sect,
                              const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                &get_val,
                                0,
                                0,
                                0)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value_input, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* serial_enable_restricted_to_callback */

static config_err_t
serial_enable_restricted_to_callback_checkout (bmc_config_state_data_t *state_data,
                                               const struct config_section *sect,
                                               struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;

  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 0,
                                 &get_val,
                                 0,
                                 0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (get_val)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_enable_restricted_to_callback_commit (bmc_config_state_data_t *state_data,
                                             const struct config_section *sect,
                                             const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             0, 0,
                             same (kv->value_input, "yes"), 1,
                             0, 0,
                             0, 0);
}

static bmc_diff_t
serial_enable_restricted_to_callback_diff (bmc_config_state_data_t *state_data,
                                           const struct config_section *sect,
                                           const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                0,
                                &get_val,
                                0,
                                0)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value_input, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* privilege_limit */

static config_err_t
serial_privilege_limit_checkout (bmc_config_state_data_t *state_data,
                                 const struct config_section *sect,
                                 struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;

  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 0,
                                 0,
                                 &get_val,
                                 0)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (get_privilege_limit_string (get_val))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_privilege_limit_commit (bmc_config_state_data_t *state_data,
                               const struct config_section *sect,
                               const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             0, 0,
                             0, 0,
                             get_privilege_limit_number (kv->value_input), 1,
                             0, 0);
}

static bmc_diff_t
serial_privilege_limit_diff (bmc_config_state_data_t *state_data,
                             const struct config_section *sect,
                             const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                0,
                                0,
                                &get_val,
                                0)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = get_privilege_limit_number (kv->value_input);
  
  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   get_privilege_limit_string (get_val));
    }
  return ret;
}

  
/* serial_session_limit */

static config_err_t
serial_session_limit_checkout (bmc_config_state_data_t *state_data,
                               const struct config_section *sect,
                               struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  config_err_t ret;
  
  if ((ret = serial_channel_get (state_data,
                                 userid,
                                 0,
                                 0,
                                 0,
                                 0,
                                 &get_val)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (asprintf (&kv->value_output, "%d", get_val) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_NON_FATAL_ERROR;
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_session_limit_commit (bmc_config_state_data_t *state_data,
                             const struct config_section *sect,
                             const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  return serial_channel_set (state_data,
                             userid,
                             0, 0,
                             0, 0,
                             0, 0,
                             0, 0,
                             strtol (kv->value_input, NULL, 0), 1);
}

static bmc_diff_t
serial_session_limit_diff (bmc_config_state_data_t *state_data,
                           const struct config_section *sect,
                           const struct config_keyvalue *kv)
{
  uint8_t userid = atoi (sect->section_name + strlen ("User"));
  uint8_t get_val;
  unsigned long int passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = serial_channel_get (state_data,
                                userid,
                                0,
                                0,
                                0,
                                0,
                                &get_val)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = atoi (kv->value_input);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

struct config_section *
bmc_config_user_section_get (bmc_config_state_data_t *state_data, int userid)
{
  struct config_section *user_section = NULL;
  char section_name[64];
  char *section_comment = 
    "In the following User sections, users should configure usernames, "
    "passwords, and access rights for IPMI over LAN communication.  "
    "Usernames can be set to any string with the exception of User1, which "
    "is a fixed to the \"anonymous\" username in IPMI."
    "\n"
    "For IPMI over LAN access for a username, set \"Enable_User\" to "
    "\"Yes\", \"Lan_Enable_IPMI_Msgs\" to \"Yes\", "
    "and \"Lan_Privilege_Limit\" to a privilege level.  The "
    "privilege level is used to limit various IPMI operations for "
    "individual usernames.  It is recommened that atleast one username be "
    "created with a privilege limit \"Administrator\", so all system "
    "functions are available to atleast one username via IPMI over LAN.  "
    "For security reasons, we recommend not enabling the \"anonymous\" "
    "User1."
    "\n"
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), a "
    "\"Password20\" and \"SOL_Payload_Access\" field may be listed below.  "
    "\"Password20\" may be used to set up to a 20 byte password for the "
    "username rather than a maximum 16 byte password.  Its use is optional.  "
    "Set the \"SOL_Payload_Access\" field to \"Yes\" or \"No\" to enable or disable "
    "this username's ability to access SOL."
    "\n"
    "Please do not forget to uncomment those fields, such as \"Password\", "
    "that may be commented out during the checkout.";

  if (userid <= 0)
    {
      fprintf(stderr, "Invalid Userid = %d\n", userid);
      return NULL;
    }

  snprintf(section_name, 64, "User%d", userid);

  if (userid == 1)
    {
      if (!(user_section = config_section_create(section_name,
                                                 "UserX",
                                                 section_comment,
                                                 0,
                                                 NULL, /* XXX */
                                                 NULL)))
        goto cleanup;
    }
  else
    {
      if (!(user_section = config_section_create(section_name,
                                                 NULL,
                                                 NULL,
                                                 0,
                                                 NULL, /* XXX */
                                                 NULL)))
        goto cleanup;
    }

  /* userid 1 is the NULL username, so comment it out by default */
  if (config_section_add_key (user_section,
                              "Username",
                              "Give Username",
                              (userid == 1) ? CONFIG_CHECKOUT_KEY_COMMENTED_OUT : 0,
                              username_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Enable_User",
                              "Possible values: Yes/No or blank to not set",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Password",
                              "Give password or blank to clear. MAX 16 chars.",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                              password_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Password20",
                              "Give password for IPMI 2.0 or blank to clear. MAX 20 chars.",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                              password20_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Lan_Enable_IPMI_Msgs",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Lan_Enable_Link_Auth",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Lan_Enable_Restricted_to_Callback",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (config_section_add_key (user_section,
                              "Lan_Enable_Restrict_to_Callback",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Lan_Privilege_Limit",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                              0,
                              get_privilege_limit_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Lan_Session_Limit",
                              "Possible values: 0-255, 0 is unlimited",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "SOL_Payload_Access",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Serial_Enable_IPMI_Msgs",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Serial_Enable_Link_Auth",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Serial_Enable_Restricted_to_Callback",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (config_section_add_key (user_section,
                              "Serial_Enable_Restrict_to_Callback",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Serial_Privilege_Limit",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                              0,
                              get_privilege_limit_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (user_section,
                              "Serial_Session_Limit",
                              "Possible values: 0-255, 0 is unlimited",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return user_section;

 cleanup:
  if (user_section)
    config_section_destroy(user_section);
  return NULL;
}
