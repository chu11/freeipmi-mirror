
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bmc-diff.h"
#include "bmc-sections.h"
#include "bmc-types.h"


static int
get_num_users (struct arguments *args)
{
  int rv = -1;
  uint8_t users = 0;
  rv = get_bmc_max_users ((ipmi_device_t *)&args->dev, &users);
  if (rv != 0)
    return -1;
  return users;
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


/* username */


static int
username_checkout (const struct arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };

  userid = atoi (sect->section + strlen ("User"));
  if (get_bmc_username ((ipmi_device_t *)&args->dev,
			userid,
			username,
			IPMI_MAX_USER_NAME_LENGTH+1))
    return -1;
		    
  if (kv->value)
    free (kv->value);
  kv->value = strdup ((char *)username);
  return 0;
}

static int
username_commit (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid;
  userid = atoi (sect->section + strlen ("User"));

  if (!kv->value)
    return -1;
  return set_bmc_username ((ipmi_device_t *)&args->dev,
			   userid,
			   (uint8_t *)kv->value);
}

static int
username_diff (const struct arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  uint8_t userid;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1] = { 0, };
  int ret;

  userid = atoi (sect->section + strlen ("User"));
  if (get_bmc_username ((ipmi_device_t *)&args->dev,
			userid,
			username,
			IPMI_MAX_USER_NAME_LENGTH+1))
    return -1;

  if (userid == 1) {
    if (! kv->value || same (kv->value, "null") || same (kv->value, "anonymous")) {
      ret = 0;
    } else {
      ret = 1;
    }
  } else {
    if (!kv->value || !same (kv->value, (char *)username)) {
      ret = 1;
    } else {
      ret = 0;
    }
  }

  if (ret == 1)
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 (char *)username);
  return ret;
}

static int
username_validate (const struct arguments *args,
		   const struct section *sect,
		   const char *value)
{
  uint8_t userid;
  userid = atoi (sect->section + strlen ("User"));

  if (userid == 1) {
    if (!value || same (value, "null") || same (value, "anonymous")) {
      return 0;
    } else {
      return -1;
    }
  } else {
    if (!value || strlen (value) > 16) {
      return -1;
    } else {
      return 0;
    }
  }
  return 0;
}

/* enable_user */

static int
enable_user_checkout (const struct arguments *args,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  /* Cant get, always assume Yes */
  if (kv->value)
    free (kv->value);
  kv->value = strdup ("Yes");
  return 0;
}

static int
enable_user_commit (const struct arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  return set_bmc_enable_user ((ipmi_device_t *)&args->dev,
			      userid,
			      same (kv->value, "yes"));
}

static int
enable_user_diff (const struct arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  /* Cant get, always assume equal */
  return 0;
}

static int
enable_user_validate (const struct arguments *args,
		      const struct section *sect,
		      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* clear_password */

static int
clear_password_checkout (const struct arguments *args,
			 const struct section *sect,
			 struct keyvalue *kv)
{
  if (kv->value)
    free (kv->value);
  kv->value = strdup ("No");
  return 0;
}

static int
clear_password_commit (const struct arguments *args,
		       const struct section *sect,
		       const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  if (same (kv->value, "yes"))
    return set_bmc_user_password ((ipmi_device_t *)&args->dev,
				  userid,
				  NULL);
  return 0;
}

static int
clear_password_diff (const struct arguments *args,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  return 0;
}

static int
clear_password_validate (const struct arguments * args,
			 const struct section *sect,
			 const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* password */

static int
password_checkout (const struct arguments *args,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  if (kv->value)
    free (kv->value);
  kv->value = strdup ("");

  return 0;
}

static int
password_commit (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return set_bmc_user_password ((ipmi_device_t *)&args->dev,
				userid, (uint8_t *)kv->value);
}

static int
password_diff (const struct arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  int ret;
  ret = check_bmc_user_password ((ipmi_device_t *)&args->dev,
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
password_validate (const struct arguments *args,
		   const struct section *sect,
		   const char *value)
{
  return (strlen (value) <= 16) ? 0 : 1;
}

/* password20 */

static int
password20_checkout (const struct arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  if (kv->value)
    free (kv->value);
  kv->value = strdup ("");

  return 0;
}

static int
password20_commit (const struct arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return set_bmc_user_password20 ((ipmi_device_t *)&args->dev,
				  userid,
				  (uint8_t *)kv->value);
}

static int
password20_diff (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  int ret = check_bmc_user_password20 ((ipmi_device_t *)&args->dev,
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
password20_validate (const struct arguments *args,
		     const struct section *sect,
		     const char *value)
{
  return (strlen (value) <= 20) ? 0 : 1;
}

/* lan_enable_ipmi_msgs */

static int
lan_channel_get (ipmi_device_t *dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_lan_channel_access (dev,
					 userid,
					 &tmp_user_ipmi_messaging,
					 &tmp_user_link_authentication,
					 &tmp_user_restricted_to_callback,
					 &tmp_privilege_limit,
					 &tmp_session_limit);

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
lan_channel_set (ipmi_device_t *dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_lan_channel_access (dev,
					 userid,
					 &tmp_user_ipmi_messaging,
					 &tmp_user_link_authentication,
					 &tmp_user_restricted_to_callback,
					 &tmp_privilege_limit,
					 &tmp_session_limit);

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
lan_enable_ipmi_msgs_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
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
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");

  return 0;
}

static int
lan_enable_ipmi_msgs_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0,
			  0, 0,
			  0, 0);
}

static int
lan_enable_ipmi_msgs_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 &get_val,
			 0,
			 0,
			 0,
			 0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 get_val ? "Yes" : "No");
  }
  return ret;
}
  
static int
lan_enable_ipmi_msgs_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* lan_enable_link_auth */

static int
lan_enable_link_auth_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
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
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");

  return 0;
}

static int
lan_enable_link_auth_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  0, 0,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0,
			  0, 0);
}

static int
lan_enable_link_auth_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 0,
			 &get_val,
			 0,
			 0,
			 0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		  kv->key,
		  kv->value,
		  get_val ? "Yes" : "No");
  }
  return ret;
}

static int
lan_enable_link_auth_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* lan_enable_restricted_to_callback */

static int
lan_enable_restricted_to_callback_checkout (const struct arguments *args,
					    const struct section *sect,
					    struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
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
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");

  return 0;
}

static int
lan_enable_restricted_to_callback_commit (const struct arguments *args,
					  const struct section *sect,
					  const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  0, 0,
			  0, 0,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0);
}

static int
lan_enable_restricted_to_callback_diff (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 0,
			 0,
			 &get_val,
			 0,
			 0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		  kv->key,
		  kv->value,
		  get_val ? "Yes" : "No");
  }
  return ret;
}

static int
lan_enable_restricted_to_callback_validate (const struct arguments *args,
					    const struct section *sect,
					    const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* privilege_limit */

static int
lan_privilege_limit_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
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

  kv->value = strdup (get_privilege_limit_string (get_val));

  return 0;
}

static int
lan_privilege_limit_commit (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  0, 0,
			  0, 0,
			  0, 0,
			  get_privilege_limit_number (kv->value), 1,
			  0, 0);
}

static int
lan_privilege_limit_diff (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 0,
			 0,
			 0,
			 &get_val,
			 0);

  if (ret != 0)
    return ret;

  passed_val = get_privilege_limit_number (kv->value);

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		  kv->key,
		  kv->value,
		  get_privilege_limit_string (get_val));
  }
  return ret;
}

  
static int
lan_privilege_limit_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (get_privilege_limit_number (value) > 0) ? 0 : 1;
}

/* lan_session_limit */

static int
lan_session_limit_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
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
lan_session_limit_commit (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return lan_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  0, 0,
			  0, 0,
			  0, 0,
			  0, 0,
			  strtol (kv->value, NULL, 0), 1);
}

static int
lan_session_limit_diff (const struct arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = lan_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 0,
			 0,
			 0,
			 0,
			 &get_val);

  if (ret != 0)
    return ret;

  passed_val = atoi (kv->value);

  if (passed_val == get_val) {
    ret = 0;
  } else {
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
lan_session_limit_validate (const struct arguments *args,
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
sol_payload_access_checkout (const struct arguments *args,
			     const struct section *sect,
			     struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t have_access;
  int ret;

  ret = get_bmc_user_payload_access ((ipmi_device_t *)&args->dev,
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
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");
  return 0;
}

static int
sol_payload_access_commit (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t operation;

  if (same (kv->value, "yes"))
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_ENABLE;
  else
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_DISABLE;

  return set_bmc_user_payload_access ((ipmi_device_t *)&args->dev,
				      userid,
				      operation,
				      1, 
				      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

static int
sol_payload_access_diff (const struct arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  int userid = atoi (sect->section + strlen ("User"));
  uint8_t have_access;
  uint8_t passed_value;
  int ret;

  ret = get_bmc_user_payload_access ((ipmi_device_t *)&args->dev,
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
  
  if (passed_value == have_access) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 have_access ? "Yes" : "No");
  }
  return ret;
}

static int
sol_payload_access_validate (const struct arguments *args,
		      const struct section *sect,
		      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* serial_enable_ipmi_msgs */

static int
serial_channel_get (ipmi_device_t *dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_serial_channel_access (dev,
					    userid,
					    &tmp_user_ipmi_messaging,
					    &tmp_user_link_authentication,
					    &tmp_user_restricted_to_callback,
					    &tmp_privilege_limit,
					    &tmp_session_limit);

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
serial_channel_set (ipmi_device_t *dev,
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
  uint8_t ret;
  
  ret = get_bmc_user_serial_channel_access (dev,
					    userid,
					    &tmp_user_ipmi_messaging,
					    &tmp_user_link_authentication,
					    &tmp_user_restricted_to_callback,
					    &tmp_privilege_limit,
					    &tmp_session_limit);

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
serial_enable_ipmi_msgs_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
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
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");

  return 0;
}

static int
serial_enable_ipmi_msgs_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0,
			  0, 0,
			  0, 0);
}

static int
serial_enable_ipmi_msgs_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 &get_val,
			 0,
			 0,
			 0,
			 0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		  kv->key,
		  kv->value,
		  get_val ? "Yes" : "No");
  }
  return ret;
}
  
static int
serial_enable_ipmi_msgs_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* serial_enable_link_auth */

static int
serial_enable_link_auth_checkout (const struct arguments *args,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
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
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");

  return 0;
}

static int
serial_enable_link_auth_commit (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  0, 0,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0,
			  0, 0);
}

static int
serial_enable_link_auth_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 0,
			 &get_val,
			 0,
			 0,
			 0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		  kv->key,
		  kv->value,
		  get_val ? "Yes" : "No");
  }
  return ret;
}

static int
serial_enable_link_auth_validate (const struct arguments *args,
			       const struct section *sect,
			       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* serial_enable_restricted_to_callback */

static int
serial_enable_restricted_to_callback_checkout (const struct arguments *args,
					    const struct section *sect,
					    struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
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
    kv->value = strdup ("Yes");
  else
    kv->value = strdup ("No");

  return 0;
}

static int
serial_enable_restricted_to_callback_commit (const struct arguments *args,
					  const struct section *sect,
					  const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  0, 0,
			  0, 0,
			  same (kv->value, "yes"), 1,
			  0, 0,
			  0, 0);
}

static int
serial_enable_restricted_to_callback_diff (const struct arguments *args,
					const struct section *sect,
					const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 0,
			 0,
			 &get_val,
			 0,
			 0);

  if (ret != 0)
    return ret;

  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		  kv->key,
		  kv->value,
		  get_val ? "Yes" : "No");
  }
  return ret;
}

static int
serial_enable_restricted_to_callback_validate (const struct arguments *args,
					       const struct section *sect,
					       const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* privilege_limit */

static int
serial_privilege_limit_checkout (const struct arguments *args,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
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

  kv->value = strdup (get_privilege_limit_string (get_val));

  return 0;
}

static int
serial_privilege_limit_commit (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set ((ipmi_device_t *)&args->dev,
			  userid,
			  0, 0,
			  0, 0,
			  0, 0,
			  get_privilege_limit_number (kv->value), 1,
			  0, 0);
}

static int
serial_privilege_limit_diff (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
			 userid,
			 0,
			 0,
			 0,
			 &get_val,
			 0);

  if (ret != 0)
    return ret;

  passed_val = get_privilege_limit_number (kv->value);

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    report_diff (sect->section,
		  kv->key,
		  kv->value,
		  get_privilege_limit_string (get_val));
  }
  return ret;
}

  
static int
serial_privilege_limit_validate (const struct arguments *args,
				 const struct section *sect,
				 const char *value)
{
  return (get_privilege_limit_number (value) > 0) ? 0 : 1;
}

/* serial_session_limit */

static int
serial_session_limit_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
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
serial_session_limit_commit (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  return serial_channel_set ((ipmi_device_t *)&args->dev,
			     userid,
			     0, 0,
			     0, 0,
			     0, 0,
			     0, 0,
			     strtol (kv->value, NULL, 0), 1);
}

static int
serial_session_limit_diff (const struct arguments *args,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t userid = atoi (sect->section + strlen ("User"));
  uint8_t get_val;
  unsigned long int passed_val;
  int ret;

  ret = serial_channel_get ((ipmi_device_t *)&args->dev,
			    userid,
			    0,
			    0,
			    0,
			    0,
			    &get_val);

  if (ret != 0)
    return ret;

  passed_val = atoi (kv->value);

  if (passed_val == get_val) {
    ret = 0;
  } else {
    ret = 1;
    char num[32];
    sprintf (num, "%d", get_val);
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 num);
  }
  return ret;
}

  
static int
serial_session_limit_validate (const struct arguments *args,
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


static struct section *
get_user_section (int num, struct arguments *args)
{
  struct section *this_section = NULL;

  this_section = (void *) calloc (1, sizeof (*this_section));
  asprintf ((char **)&this_section->section, "User%d", num + 1);

  add_keyvalue (this_section,
		"Username",
		"Give Username",
		username_checkout,
		username_commit,
		username_diff,
		username_validate);

  add_keyvalue (this_section,
		"Enable_User",
		"Possible values: Yes/No or blank to not set",
		enable_user_checkout,
		enable_user_commit,
		enable_user_diff,
		enable_user_validate);

  add_keyvalue (this_section,
		"Clear_Password",
		"Possible values: Yes/No",
		clear_password_checkout,
		clear_password_commit,
		clear_password_diff,
		clear_password_validate);

  add_keyvalue (this_section,
		"Password",
		"Give password or blank to clear. MAX 16 chars.",
		password_checkout,
		password_commit,
		password_diff,
		password_validate);

  add_keyvalue (this_section,
		"Password20",
		"Give password for IPMI 2.0 or blank to clear. MAX 20 chars.",
		password20_checkout,
		password20_commit,
		password20_diff,
		password20_validate);

  add_keyvalue (this_section,
		"LAN_Enable_IPMI_Msgs",
		"Possible values: Yes/No",
		lan_enable_ipmi_msgs_checkout,
		lan_enable_ipmi_msgs_commit,
		lan_enable_ipmi_msgs_diff,
		lan_enable_ipmi_msgs_validate);

  add_keyvalue (this_section,
		"LAN_Enable_Link_Auth",
		"Possible values: Yes/No",
		lan_enable_link_auth_checkout,
		lan_enable_link_auth_commit,
		lan_enable_link_auth_diff,
		lan_enable_link_auth_validate);

  add_keyvalue (this_section,
		"LAN_Enable_Restricted_to_Callback",
		"Possible values: Yes/No",
		lan_enable_restricted_to_callback_checkout,
		lan_enable_restricted_to_callback_commit,
		lan_enable_restricted_to_callback_diff,
		lan_enable_restricted_to_callback_validate);

  add_keyvalue (this_section,
		"LAN_Privilege_Limit",
		"Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
		lan_privilege_limit_checkout,
		lan_privilege_limit_commit,
		lan_privilege_limit_diff,
		lan_privilege_limit_validate);

  add_keyvalue (this_section,
		"LAN_Session_Limit",
		"Possible values: 0-255, 0 is unlimited",
		lan_session_limit_checkout,
		lan_session_limit_commit,
		lan_session_limit_diff,
		lan_session_limit_validate);

  add_keyvalue (this_section,
		"SOL_Payload_Access",
		"Possible values: Yes/No",
		sol_payload_access_checkout,
		sol_payload_access_commit,
		sol_payload_access_diff,
		sol_payload_access_validate);

  add_keyvalue (this_section,
		"Serial_Enable_IPMI_Msgs",
		"Possible values: Yes/No",
		serial_enable_ipmi_msgs_checkout,
		serial_enable_ipmi_msgs_commit,
		serial_enable_ipmi_msgs_diff,
		serial_enable_ipmi_msgs_validate);

  add_keyvalue (this_section,
		"Serial_Enable_Link_Auth",
		"Possible values: Yes/No",
		serial_enable_link_auth_checkout,
		serial_enable_link_auth_commit,
		serial_enable_link_auth_diff,
		serial_enable_link_auth_validate);

  add_keyvalue (this_section,
		"Serial_Enable_Restricted_to_Callback",
		"Possible values: Yes/No",
		serial_enable_restricted_to_callback_checkout,
		serial_enable_restricted_to_callback_commit,
		serial_enable_restricted_to_callback_diff,
		serial_enable_restricted_to_callback_validate);

  add_keyvalue (this_section,
		"Serial_Privilege_Limit",
		"Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
		serial_privilege_limit_checkout,
		serial_privilege_limit_commit,
		serial_privilege_limit_diff,
		serial_privilege_limit_validate);

  add_keyvalue (this_section,
		"Serial_Session_Limit",
		"Possible values: 0-255, 0 is unlimited",
		serial_session_limit_checkout,
		serial_session_limit_commit,
		serial_session_limit_diff,
		serial_session_limit_validate);

  return this_section;
}

struct section *
bmc_user_sections_get (struct arguments *args)
{
  struct section * user_sections = NULL;
  int num_users = get_num_users (args);

  int i;

  for (i=0; i<num_users; i++) {
    add_section (user_sections, get_user_section (i, args));
  }

  return user_sections;
}

