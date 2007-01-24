#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"

int
bmc_get_num_users (struct bmc_config_arguments *args)
{
  uint8_t users = 0;
  if (get_bmc_max_users (args->dev, &users) < 0)
    return (-1);
  return (int)users;
}

/* username */

static int
username_checkout (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };

  userid = atoi (sect->section + strlen ("User"));
  if (get_bmc_username (args->dev,
			userid,
			username,
			IPMI_MAX_USER_NAME_LENGTH+1))
    return -1;
		    
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ((char *)username)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
username_commit (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid;
  userid = atoi (sect->section + strlen ("User"));

  if (!kv->value)
    return -1;
  return set_bmc_username (args->dev,
			   userid,
			   (uint8_t *)kv->value);
}

static int
username_diff (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };
  int ret;

  userid = atoi (sect->section + strlen ("User"));
  if (get_bmc_username (args->dev,
			userid,
			username,
			IPMI_MAX_USER_NAME_LENGTH+1))
    return -1;

  if (userid == 1) 
    {
      if (! kv->value || same (kv->value, "null") || same (kv->value, "anonymous")) 
        ret = 0;
      else 
        ret = 1;
    } 
  else 
    {
      if (!kv->value || !same (kv->value, (char *)username))
        ret = 1;
      else
        ret = 0;
    }

  if (ret == 1)
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 (char *)username);
  return ret;
}

static int
username_validate (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const char *value)
{
  uint8_t userid;
  userid = atoi (sect->section + strlen ("User"));

  if (userid == 1) 
    {
      if (!value || same (value, "null") || same (value, "anonymous"))
        return 0;
      else
        return -1;
    } 
  else 
    {
      if (!value || strlen (value) > 16)
        return -1;
      else
        return 0;
    }
  return 0;
}

/* enable_user */

static int
enable_user_checkout (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  uint8_t ret;
  
  ret = get_bmc_user_lan_channel_access (args->dev,
					 userid,
					 &tmp_user_ipmi_messaging,
					 &tmp_user_link_authentication,
					 &tmp_user_restricted_to_callback,
					 &tmp_privilege_limit,
					 &tmp_session_limit,
                                         &tmp_user_id_enable_status);

  if (ret != 0)
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
          return -1;
        }
    }
  else if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED)
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }
  else /* tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED */
    {
      if (!(kv->value = strdup ("")))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

static int
enable_user_commit (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  return set_bmc_enable_user (args->dev,
			      userid,
			      same (kv->value, "yes"));
}

static int
enable_user_diff (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t tmp_user_ipmi_messaging;
  uint8_t tmp_user_link_authentication;
  uint8_t tmp_user_restricted_to_callback;
  uint8_t tmp_privilege_limit;
  uint8_t tmp_session_limit;
  uint8_t tmp_user_id_enable_status;
  uint8_t ret;
  uint8_t passed_val;

  ret = get_bmc_user_lan_channel_access (args->dev,
					 userid,
					 &tmp_user_ipmi_messaging,
					 &tmp_user_link_authentication,
					 &tmp_user_restricted_to_callback,
					 &tmp_privilege_limit,
					 &tmp_session_limit,
                                         &tmp_user_id_enable_status);

  if (ret != 0)
    return ret;
  
  /* Cant get, assume equal */
  if (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED)
    ret = 0;
  else
    {
      passed_val = same (kv->value, "Yes");

      if (passed_val && tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
        ret = 0;
      else if (!passed_val && tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED)
        ret = 0;
      else
        {
          ret = 1;
          report_diff (sect->section,
                       kv->key,
                       kv->value,
                       (tmp_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED) ? "Yes" : "No");
        }
    }

  return ret;
}

static int
enable_user_validate (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

static int
password_checkout (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ("")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
password_commit (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return set_bmc_user_password (args->dev,
				userid, (uint8_t *)kv->value);
}

static int
password_diff (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  int ret;
  ret = check_bmc_user_password (args->dev,
				 userid,
				 (uint8_t *)kv->value);
  if (!ret)
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 "<something else>");
  return ret;
}

static int
password_validate (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const char *value)
{
  return (strlen (value) <= 16) ? 0 : 1;
}

/* password20 */

static int
password20_checkout (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));

  /* achu: password can't be checked out, but we should make sure IPMI
   * 2.0 exists on the system.
   */
  if (kv->value)
    free (kv->value);

  if (check_bmc_user_password20 (args->dev,
                                 userid,
                                 "foobar") < 0) 
    return -1;

  if (!(kv->value = strdup ("")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
password20_commit (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return set_bmc_user_password20 (args->dev,
				  userid,
				  (uint8_t *)kv->value);
}

static int
password20_diff (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  int ret = check_bmc_user_password20 (args->dev,
				       userid,
				       (uint8_t *)kv->value);

  if (!ret)
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 "<something else>");
  return ret;
}

static int
password20_validate (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const char *value)
{
  return (strlen (value) <= 20) ? 0 : 1;
}

/* lan_enable_ipmi_msgs */

static int
lan_channel_get (ipmi_device_t dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_lan_channel_access (dev,
					 userid,
					 &tmp_user_ipmi_messaging,
					 &tmp_user_link_authentication,
					 &tmp_user_restricted_to_callback,
					 &tmp_privilege_limit,
					 &tmp_session_limit,
                                         &tmp_user_id_enable_status);

  if (ret != 0)
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

  return 0;
}

static int
lan_channel_set (ipmi_device_t dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_lan_channel_access (dev,
					 userid,
					 &tmp_user_ipmi_messaging,
					 &tmp_user_link_authentication,
					 &tmp_user_restricted_to_callback,
					 &tmp_privilege_limit,
					 &tmp_session_limit,
                                         &tmp_user_id_enable_status);

  if (ret != 0)
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

  ret = set_bmc_user_lan_channel_access (dev,
					 userid,
					 tmp_user_ipmi_messaging,
					 tmp_user_link_authentication,
					 tmp_user_restricted_to_callback,
					 tmp_privilege_limit,
					 tmp_session_limit);

  return ret;
}


/* lan_enable_ipmi_msgs */

static int
lan_enable_ipmi_msgs_checkout (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get (args->dev,
			 userid,
			 &get_val,
			 0,
			 0,
			 0,
			 0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

static int
lan_enable_ipmi_msgs_commit (const struct bmc_config_arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set (args->dev,
			  userid,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0,
			  0, 0,
			  0, 0);
}

static int
lan_enable_ipmi_msgs_diff (const struct bmc_config_arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get (args->dev,
			 userid,
			 &get_val,
			 0,
			 0,
			 0,
			 0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}
  
static int
lan_enable_ipmi_msgs_validate (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* lan_enable_link_auth */

static int
lan_enable_link_auth_checkout (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get (args->dev,
			 userid,
			 0,
			 &get_val,
			 0,
			 0,
			 0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

static int
lan_enable_link_auth_commit (const struct bmc_config_arguments *args,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set (args->dev,
                          userid,
                          0, 0,
                          same (kv->value, "yes"), 1,
                          0, 0,
                          0, 0,
                          0, 0);
}

static int
lan_enable_link_auth_diff (const struct bmc_config_arguments *args,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get (args->dev,
                         userid,
                         0,
                         &get_val,
                         0,
                         0,
                         0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static int
lan_enable_link_auth_validate (const struct bmc_config_arguments *args,
                               const struct section *sect,
                               const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* lan_enable_restricted_to_callback */

static int
lan_enable_restricted_to_callback_checkout (const struct bmc_config_arguments *args,
                                            const struct section *sect,
                                            struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get (args->dev,
                         userid,
                         0,
                         0,
                         &get_val,
                         0,
                         0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

static int
lan_enable_restricted_to_callback_commit (const struct bmc_config_arguments *args,
                                          const struct section *sect,
                                          const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set (args->dev,
                          userid,
                          0, 0,
                          0, 0,
                          same (kv->value, "yes"), 1,
                          0, 0,
                          0, 0);
}

static int
lan_enable_restricted_to_callback_diff (const struct bmc_config_arguments *args,
                                        const struct section *sect,
                                        const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get (args->dev,
                         userid,
                         0,
                         0,
                         &get_val,
                         0,
                         0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) 
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static int
lan_enable_restricted_to_callback_validate (const struct bmc_config_arguments *args,
                                            const struct section *sect,
                                            const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* privilege_limit */

static int
lan_privilege_limit_checkout (const struct bmc_config_arguments *args,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get (args->dev,
                         userid,
                         0,
                         0,
                         0,
                         &get_val,
                         0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (get_privilege_limit_string (get_val))))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
lan_privilege_limit_commit (const struct bmc_config_arguments *args,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set (args->dev,
                          userid,
                          0, 0,
                          0, 0,
                          0, 0,
                          get_privilege_limit_number (kv->value), 1,
                          0, 0);
}

static int
lan_privilege_limit_diff (const struct bmc_config_arguments *args,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get (args->dev,
                         userid,
                         0,
                         0,
                         0,
                         &get_val,
                         0);

  if (ret != 0)
    return ret;

  passed_val = get_privilege_limit_number (kv->value);

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_privilege_limit_string (get_val));
    }
  return ret;
}

  
static int
lan_privilege_limit_validate (const struct bmc_config_arguments *args,
                              const struct section *sect,
                              const char *value)
{
  return (get_privilege_limit_number (value) > 0) ? 0 : 1;
}

/* lan_session_limit */

static int
lan_session_limit_checkout (const struct bmc_config_arguments *args,
                            const struct section *sect,
                            struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get (args->dev,
                         userid,
                         0,
                         0,
                         0,
                         0,
                         &get_val);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", get_val);

  return 0;
}

static int
lan_session_limit_commit (const struct bmc_config_arguments *args,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set (args->dev,
                          userid,
                          0, 0,
                          0, 0,
                          0, 0,
                          0, 0,
                          strtol (kv->value, NULL, 0), 1);
}

static int
lan_session_limit_diff (const struct bmc_config_arguments *args,
                        const struct section *sect,
                        const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get (args->dev,
                         userid,
                         0,
                         0,
                         0,
                         0,
                         &get_val);

  if (ret != 0)
    return ret;

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      char num[32];
      sprintf (num, "%d", get_val);
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

  
static int
lan_session_limit_validate (const struct bmc_config_arguments *args,
                            const struct section *sect,
                            const char *value)
{
  long int conv;
  char *endptr;
  conv = strtol (value, &endptr, 0);
  if (*endptr)
    return -1;
  if (conv < 0 || conv > 255)
    return 0;
  return 0;
}

/* sol_payload_access */


static int
sol_payload_access_checkout (const struct bmc_config_arguments *args,
                             const struct section *sect,
                             struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t have_access;
  int ret;

  ret = get_bmc_user_payload_access (args->dev,
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
                                     NULL);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (have_access)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }
  return 0;
}

static int
sol_payload_access_commit (const struct bmc_config_arguments *args,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t operation;

  if (same (kv->value, "yes"))
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_ENABLE;
  else
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_DISABLE;

  return set_bmc_user_payload_access (args->dev,
                                      userid,
                                      operation,
                                      1, 
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

static int
sol_payload_access_diff (const struct bmc_config_arguments *args,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t have_access;
  uint8_t passed_value;
  int ret;

  ret = get_bmc_user_payload_access (args->dev,
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
                                     NULL);
  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes");
  
  if (passed_value == have_access)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   have_access ? "Yes" : "No");
    }
  return ret;
}

static int
sol_payload_access_validate (const struct bmc_config_arguments *args,
                             const struct section *sect,
                             const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* serial_enable_ipmi_msgs */

static int
serial_channel_get (ipmi_device_t dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_serial_channel_access (dev,
                                            userid,
                                            &tmp_user_ipmi_messaging,
                                            &tmp_user_link_authentication,
                                            &tmp_user_restricted_to_callback,
                                            &tmp_privilege_limit,
                                            &tmp_session_limit,
                                            &tmp_user_id_enable_status);

  if (ret != 0)
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

  return 0;
}

static int
serial_channel_set (ipmi_device_t dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_serial_channel_access (dev,
                                            userid,
                                            &tmp_user_ipmi_messaging,
                                            &tmp_user_link_authentication,
                                            &tmp_user_restricted_to_callback,
                                            &tmp_privilege_limit,
                                            &tmp_session_limit,
                                            &tmp_user_id_enable_status);

  if (ret != 0)
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

  ret = set_bmc_user_serial_channel_access (dev,
                                            userid,
                                            tmp_user_ipmi_messaging,
                                            tmp_user_link_authentication,
                                            tmp_user_restricted_to_callback,
                                            tmp_privilege_limit,
                                            tmp_session_limit);

  return ret;
}


/* serial_enable_ipmi_msgs */

static int
serial_enable_ipmi_msgs_checkout (const struct bmc_config_arguments *args,
                                  const struct section *sect,
                                  struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;
  
  ret = serial_channel_get (args->dev,
                            userid,
                            &get_val,
                            0,
                            0,
                            0,
                            0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

static int
serial_enable_ipmi_msgs_commit (const struct bmc_config_arguments *args,
                                const struct section *sect,
                                const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set (args->dev,
                             userid,
                             same (kv->value, "yes"), 1,
                             0, 0,
                             0, 0,
                             0, 0,
                             0, 0);
}

static int
serial_enable_ipmi_msgs_diff (const struct bmc_config_arguments *args,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            &get_val,
                            0,
                            0,
                            0,
                            0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}
  
static int
serial_enable_ipmi_msgs_validate (const struct bmc_config_arguments *args,
                                  const struct section *sect,
                                  const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* serial_enable_link_auth */

static int
serial_enable_link_auth_checkout (const struct bmc_config_arguments *args,
                                  const struct section *sect,
                                  struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            &get_val,
                            0,
                            0,
                            0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

static int
serial_enable_link_auth_commit (const struct bmc_config_arguments *args,
                                const struct section *sect,
                                const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set (args->dev,
                             userid,
                             0, 0,
                             same (kv->value, "yes"), 1,
                             0, 0,
                             0, 0,
                             0, 0);
}

static int
serial_enable_link_auth_diff (const struct bmc_config_arguments *args,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            &get_val,
                            0,
                            0,
                            0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static int
serial_enable_link_auth_validate (const struct bmc_config_arguments *args,
                                  const struct section *sect,
                                  const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* serial_enable_restricted_to_callback */

static int
serial_enable_restricted_to_callback_checkout (const struct bmc_config_arguments *args,
                                               const struct section *sect,
                                               struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            0,
                            &get_val,
                            0,
                            0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (get_val)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }

  return 0;
}

static int
serial_enable_restricted_to_callback_commit (const struct bmc_config_arguments *args,
                                             const struct section *sect,
                                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set (args->dev,
                             userid,
                             0, 0,
                             0, 0,
                             same (kv->value, "yes"), 1,
                             0, 0,
                             0, 0);
}

static int
serial_enable_restricted_to_callback_diff (const struct bmc_config_arguments *args,
                                           const struct section *sect,
                                           const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            0,
                            &get_val,
                            0,
                            0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static int
serial_enable_restricted_to_callback_validate (const struct bmc_config_arguments *args,
                                               const struct section *sect,
                                               const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* privilege_limit */

static int
serial_privilege_limit_checkout (const struct bmc_config_arguments *args,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            0,
                            0,
                            &get_val,
                            0);

  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (get_privilege_limit_string (get_val))))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
serial_privilege_limit_commit (const struct bmc_config_arguments *args,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set (args->dev,
                             userid,
                             0, 0,
                             0, 0,
                             0, 0,
                             get_privilege_limit_number (kv->value), 1,
                             0, 0);
}

static int
serial_privilege_limit_diff (const struct bmc_config_arguments *args,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            0,
                            0,
                            &get_val,
                            0);

  if (ret != 0)
    return ret;
  
  passed_val = get_privilege_limit_number (kv->value);
  
  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   get_privilege_limit_string (get_val));
    }
  return ret;
}

  
static int
serial_privilege_limit_validate (const struct bmc_config_arguments *args,
                                 const struct section *sect,
                                 const char *value)
{
  return (get_privilege_limit_number (value) > 0) ? 0 : 1;
}

/* serial_session_limit */

static int
serial_session_limit_checkout (const struct bmc_config_arguments *args,
                               const struct section *sect,
                               struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;
  
  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            0,
                            0,
                            0,
                            &get_val);
  
  if (ret != 0)
    return ret;

  if (kv->value)
    free (kv->value);
  
  asprintf (&kv->value, "%d", get_val);
  
  return 0;
}

static int
serial_session_limit_commit (const struct bmc_config_arguments *args,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set (args->dev,
                             userid,
                             0, 0,
                             0, 0,
                             0, 0,
                             0, 0,
                             strtol (kv->value, NULL, 0), 1);
}

static int
serial_session_limit_diff (const struct bmc_config_arguments *args,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  unsigned long int passed_val;
  int ret;

  ret = serial_channel_get (args->dev,
                            userid,
                            0,
                            0,
                            0,
                            0,
                            &get_val);

  if (ret != 0)
    return ret;

  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", get_val);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

  
static int
serial_session_limit_validate (const struct bmc_config_arguments *args,
                               const struct section *sect,
                               const char *value)
{
  long int conv;
  char *endptr;
  conv = strtol (value, &endptr, 0);
  if (*endptr)
    return -1;
  if (conv < 0 || conv > 255)
    return 1;
  return 0;
}


struct section *
bmc_user_section_get (struct bmc_config_arguments *args, int userid)
{
  struct section *this_section = NULL;

  if (userid < 0)
    {
      fprintf(stderr, "Invalid Userid = %d\n", userid);
      return NULL;
    }

  if (!(this_section = (void *) calloc (1, sizeof (*this_section))))
    {
      perror("calloc");
      return NULL;
    }
  asprintf ((char **)&this_section->section, "User%d", userid + 1);

  add_keyvalue (this_section,
                "Username",
                "Give Username",
                0,
                username_checkout,
                username_commit,
                username_diff,
                username_validate);

  add_keyvalue (this_section,
                "Enable_User",
                "Possible values: Yes/No or blank to not set",
                BMC_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                enable_user_checkout,
                enable_user_commit,
                enable_user_diff,
                enable_user_validate);

  add_keyvalue (this_section,
                "Password",
                "Give password or blank to clear. MAX 16 chars.",
                BMC_CHECKOUT_KEY_COMMENTED_OUT,
                password_checkout,
                password_commit,
                password_diff,
                password_validate);

  add_keyvalue (this_section,
                "Password20",
                "Give password for IPMI 2.0 or blank to clear. MAX 20 chars.",
                BMC_CHECKOUT_KEY_COMMENTED_OUT,
                password20_checkout,
                password20_commit,
                password20_diff,
                password20_validate);

  add_keyvalue (this_section,
                "Lan_Enable_IPMI_Msgs",
                "Possible values: Yes/No",
                0,
                lan_enable_ipmi_msgs_checkout,
                lan_enable_ipmi_msgs_commit,
                lan_enable_ipmi_msgs_diff,
                lan_enable_ipmi_msgs_validate);

  add_keyvalue (this_section,
                "Lan_Enable_Link_Auth",
                "Possible values: Yes/No",
                0,
                lan_enable_link_auth_checkout,
                lan_enable_link_auth_commit,
                lan_enable_link_auth_diff,
                lan_enable_link_auth_validate);

  add_keyvalue (this_section,
                "Lan_Enable_Restricted_to_Callback",
                "Possible values: Yes/No",
                0,
                lan_enable_restricted_to_callback_checkout,
                lan_enable_restricted_to_callback_commit,
                lan_enable_restricted_to_callback_diff,
                lan_enable_restricted_to_callback_validate);

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  add_keyvalue (this_section,
                "Lan_Enable_Restrict_to_Callback",
                "Possible values: Yes/No",
                BMC_DO_NOT_CHECKOUT,
                lan_enable_restricted_to_callback_checkout,
                lan_enable_restricted_to_callback_commit,
                lan_enable_restricted_to_callback_diff,
                lan_enable_restricted_to_callback_validate);

  add_keyvalue (this_section,
                "Lan_Privilege_Limit",
                "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                0,
                lan_privilege_limit_checkout,
                lan_privilege_limit_commit,
                lan_privilege_limit_diff,
                lan_privilege_limit_validate);

  add_keyvalue (this_section,
                "Lan_Session_Limit",
                "Possible values: 0-255, 0 is unlimited",
                BMC_DO_NOT_CHECKOUT,
                lan_session_limit_checkout,
                lan_session_limit_commit,
                lan_session_limit_diff,
                lan_session_limit_validate);

  add_keyvalue (this_section,
                "SOL_Payload_Access",
                "Possible values: Yes/No",
                0,
                sol_payload_access_checkout,
                sol_payload_access_commit,
                sol_payload_access_diff,
                sol_payload_access_validate);

  add_keyvalue (this_section,
                "Serial_Enable_IPMI_Msgs",
                "Possible values: Yes/No",
                0,
                serial_enable_ipmi_msgs_checkout,
                serial_enable_ipmi_msgs_commit,
                serial_enable_ipmi_msgs_diff,
                serial_enable_ipmi_msgs_validate);

  add_keyvalue (this_section,
                "Serial_Enable_Link_Auth",
                "Possible values: Yes/No",
                0,
                serial_enable_link_auth_checkout,
                serial_enable_link_auth_commit,
                serial_enable_link_auth_diff,
                serial_enable_link_auth_validate);

  add_keyvalue (this_section,
                "Serial_Enable_Restricted_to_Callback",
                "Possible values: Yes/No",
                0,
                serial_enable_restricted_to_callback_checkout,
                serial_enable_restricted_to_callback_commit,
                serial_enable_restricted_to_callback_diff,
                serial_enable_restricted_to_callback_validate);

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  add_keyvalue (this_section,
                "Serial_Enable_Restrict_to_Callback",
                "Possible values: Yes/No",
                BMC_DO_NOT_CHECKOUT,
                serial_enable_restricted_to_callback_checkout,
                serial_enable_restricted_to_callback_commit,
                serial_enable_restricted_to_callback_diff,
                serial_enable_restricted_to_callback_validate);

  add_keyvalue (this_section,
                "Serial_Privilege_Limit",
                "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                0,
                serial_privilege_limit_checkout,
                serial_privilege_limit_commit,
                serial_privilege_limit_diff,
                serial_privilege_limit_validate);

  add_keyvalue (this_section,
                "Serial_Session_Limit",
                "Possible values: 0-255, 0 is unlimited",
                BMC_DO_NOT_CHECKOUT,
                serial_session_limit_checkout,
                serial_session_limit_commit,
                serial_session_limit_diff,
                serial_session_limit_validate);

  return this_section;
}
