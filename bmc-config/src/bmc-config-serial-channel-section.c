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
#include "bmc-config-diff.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

/* volatile */

static config_err_t
serial_channel_volatile_access_set (bmc_config_state_data_t *state_data,
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
  config_err_t ret;
  
  if ((ret = get_bmc_serial_channel_volatile_access (state_data,
                                                     &tmp_access_mode,
                                                     &tmp_user_level_authentication,
                                                     &tmp_per_message_authentication,
                                                     &tmp_pef_alerting,
                                                     &tmp_channel_privilege_limit)) != CONFIG_ERR_SUCCESS)
    return ret;

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

  if ((ret = set_bmc_serial_channel_volatile_access (state_data,
                                                     tmp_access_mode,
                                                     tmp_user_level_authentication,
                                                     tmp_per_message_authentication,
                                                     tmp_pef_alerting,
                                                     tmp_channel_privilege_limit)) != CONFIG_ERR_SUCCESS)

    return ret;

  return CONFIG_ERR_SUCCESS;
}


/* access_mode */

static config_err_t
volatile_access_mode_checkout (bmc_config_state_data_t *state_data,
			       const struct config_section *section,
			       struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_volatile_access (state_data,
                                                     &get_val,
                                                     &foo,
                                                     &foo,
                                                     &foo,
                                                     &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (channel_access_mode_string (get_val))))
    {
      perror("strdup");
      return -1;
    }
  return 0;
}

static config_err_t
volatile_access_mode_commit (bmc_config_state_data_t *state_data,
			     const struct config_section *section,
			     const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = channel_access_mode (kv->value);
  return serial_channel_volatile_access_set (state_data,
                                             commit_val, 1,
                                             0, 0,
                                             0, 0,
                                             0, 0,
                                             0, 0);
}

static config_diff_t
volatile_access_mode_diff (bmc_config_state_data_t *state_data,
			   const struct config_section *section,
			   const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_volatile_access (state_data,
                                                    &get_val,
                                                    &foo,
                                                    &foo,
                                                    &foo,
                                                    &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = channel_access_mode (kv->value);
  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   channel_access_mode_string (get_val));
    }
  return ret;
}

/* enable_user_level_auth */

static config_err_t
volatile_enable_user_level_auth_checkout (bmc_config_state_data_t *state_data,
					  const struct config_section *section,
					  struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_volatile_access (state_data,
                                                     &foo,
                                                     &get_val,
                                                     &foo,
                                                     &foo,
                                                     &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (get_val ? "Yes" : "No")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
volatile_enable_user_level_auth_commit (bmc_config_state_data_t *state_data,
					const struct config_section *section,
					const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_volatile_access_set (state_data,
                                             0, 0,
                                             commit_val, 1,
                                             0, 0,
                                             0, 0,
                                             0, 0);
}

static config_diff_t
volatile_enable_user_level_auth_diff (bmc_config_state_data_t *state_data,
				      const struct config_section *section,
				      const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_volatile_access (state_data,
                                                    &foo,
                                                    &get_val,
                                                    &foo,
                                                    &foo,
                                                    &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* enable_per_message_auth */

static config_err_t
volatile_enable_per_msg_auth_checkout (bmc_config_state_data_t *state_data,
				       const struct config_section *section,
				       struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_volatile_access (state_data,
                                                     &foo,
                                                     &foo,
                                                     &get_val,
                                                     &foo,
                                                     &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (get_val ? "Yes" : "No")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
volatile_enable_per_msg_auth_commit (bmc_config_state_data_t *state_data,
				     const struct config_section *section,
				     const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_volatile_access_set (state_data,
                                             0, 0,
                                             0, 0,
                                             commit_val, 1,
                                             0, 0,
                                             0, 0);
}

static config_diff_t
volatile_enable_per_msg_auth_diff (bmc_config_state_data_t *state_data,
				   const struct config_section *section,
				   const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_volatile_access (state_data,
                                                    &foo,
                                                    &foo,
                                                    &get_val,
                                                    &foo,
                                                    &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* enable_pef_alerting */

static config_err_t
volatile_enable_pef_alerting_checkout (bmc_config_state_data_t *state_data,
				       const struct config_section *section,
				       struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_volatile_access (state_data,
                                                     &foo,
                                                     &foo,
                                                     &foo,
                                                     &get_val,
                                                     &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (get_val ? "Yes" : "No")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
volatile_enable_pef_alerting_commit (bmc_config_state_data_t *state_data,
				     const struct config_section *section,
				     const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_volatile_access_set (state_data,
                                             0, 0,
                                             0, 0,
                                             0, 0,
                                             commit_val, 1,
                                             0, 0);
}

static config_diff_t
volatile_enable_pef_alerting_diff (bmc_config_state_data_t *state_data,
				   const struct config_section *section,
				   const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_volatile_access (state_data,
                                                    &foo,
                                                    &foo,
                                                    &foo,
                                                    &get_val,
                                                    &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "yes");
  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* channel_privilege_level */

static config_err_t
volatile_channel_priv_limit_checkout (bmc_config_state_data_t *state_data,
				      const struct config_section *section,
				      struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_volatile_access (state_data,
                                                     &foo,
                                                     &foo,
                                                     &foo,
                                                     &foo,
                                                     &get_val)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (privilege_level_string (get_val))))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
volatile_channel_priv_limit_commit (bmc_config_state_data_t *state_data,
				    const struct config_section *section,
				    const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = privilege_level_number (kv->value);
  return serial_channel_volatile_access_set (state_data,
                                             0, 0,
                                             0, 0,
                                             0, 0,
                                             0, 0,
                                             commit_val, 1);
}

static config_diff_t
volatile_channel_priv_limit_diff (bmc_config_state_data_t *state_data,
				  const struct config_section *section,
				  const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_volatile_access (state_data,
                                                    &foo,
                                                    &foo,
                                                    &foo,
                                                    &foo,
                                                    &get_val)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = privilege_level_number (kv->value);

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   privilege_level_string (get_val));
    }
  return ret;
}

/* non volatile */

static config_err_t
serial_channel_non_volatile_access_set (bmc_config_state_data_t *state_data,
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
  config_err_t ret;
  
  if ((ret = get_bmc_serial_channel_non_volatile_access (state_data,
                                                         &tmp_access_mode,
                                                         &tmp_user_level_authentication,
                                                         &tmp_per_message_authentication,
                                                         &tmp_pef_alerting,
                                                         &tmp_channel_privilege_limit)) != CONFIG_ERR_SUCCESS)
    return ret;

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

  if ((ret = set_bmc_serial_channel_non_volatile_access (state_data,
                                                         tmp_access_mode,
                                                         tmp_user_level_authentication,
                                                         tmp_per_message_authentication,
                                                         tmp_pef_alerting,
                                                         tmp_channel_privilege_limit)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}


/* access_mode */

static config_err_t
non_volatile_access_mode_checkout (bmc_config_state_data_t *state_data,
                                   const struct config_section *section,
                                   struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_non_volatile_access (state_data,
                                                         &get_val,
                                                         &foo,
                                                         &foo,
                                                         &foo,
                                                         &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (channel_access_mode_string (get_val))))
    {
      perror("strdup");
      return -1;
    }
  return 0;
}

static config_err_t
non_volatile_access_mode_commit (bmc_config_state_data_t *state_data,
                                 const struct config_section *section,
                                 const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = channel_access_mode (kv->value);
  return serial_channel_non_volatile_access_set (state_data,
                                                 commit_val, 1,
                                                 0, 0,
                                                 0, 0,
                                                 0, 0,
                                                 0, 0);
}

static config_diff_t
non_volatile_access_mode_diff (bmc_config_state_data_t *state_data,
                               const struct config_section *section,
                               const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_non_volatile_access (state_data,
                                                        &get_val,
                                                        &foo,
                                                        &foo,
                                                        &foo,
                                                        &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = channel_access_mode (kv->value);

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   channel_access_mode_string (get_val));
    }
  return ret;
}

/* enable_user_level_auth */

static config_err_t
non_volatile_enable_user_level_auth_checkout (bmc_config_state_data_t *state_data,
                                              const struct config_section *section,
                                              struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_non_volatile_access (state_data,
                                                         &foo,
                                                         &get_val,
                                                         &foo,
                                                         &foo,
                                                         &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (get_val ? "Yes" : "No")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
non_volatile_enable_user_level_auth_commit (bmc_config_state_data_t *state_data,
                                            const struct config_section *section,
                                            const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_non_volatile_access_set (state_data,
                                                 0, 0,
                                                 commit_val, 1,
                                                 0, 0,
                                                 0, 0,
                                                 0, 0);
}

static config_diff_t
non_volatile_enable_user_level_auth_diff (bmc_config_state_data_t *state_data,
                                          const struct config_section *section,
                                          const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_non_volatile_access (state_data,
                                                        &foo,
                                                        &get_val,
                                                        &foo,
                                                        &foo,
                                                        &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* enable_per_message_auth */

static config_err_t
non_volatile_enable_per_msg_auth_checkout (bmc_config_state_data_t *state_data,
                                           const struct config_section *section,
                                           struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_non_volatile_access (state_data,
                                                         &foo,
                                                         &foo,
                                                         &get_val,
                                                         &foo,
                                                         &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (get_val ? "Yes" : "No")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
non_volatile_enable_per_msg_auth_commit (bmc_config_state_data_t *state_data,
                                         const struct config_section *section,
                                         const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_non_volatile_access_set (state_data,
                                                 0, 0,
                                                 0, 0,
                                                 commit_val, 1,
                                                 0, 0,
                                                 0, 0);
}

static config_diff_t
non_volatile_enable_per_msg_auth_diff (bmc_config_state_data_t *state_data,
                                       const struct config_section *section,
                                       const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_non_volatile_access (state_data,
                                                        &foo,
                                                        &foo,
                                                        &get_val,
                                                        &foo,
                                                        &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "yes");

  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* enable_pef_alerting */

static config_err_t
non_volatile_enable_pef_alerting_checkout (bmc_config_state_data_t *state_data,
					   const struct config_section *section,
					   struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_non_volatile_access (state_data,
                                                         &foo,
                                                         &foo,
                                                         &foo,
                                                         &get_val,
                                                         &foo)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (get_val ? "Yes" : "No")))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
non_volatile_enable_pef_alerting_commit (bmc_config_state_data_t *state_data,
					 const struct config_section *section,
					 const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = same (kv->value, "yes");
  return serial_channel_non_volatile_access_set (state_data,
						 0, 0,
						 0, 0,
						 0, 0,
						 commit_val, 1,
						 0, 0);
}

static config_diff_t
non_volatile_enable_pef_alerting_diff (bmc_config_state_data_t *state_data,
				       const struct config_section *section,
				       const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_non_volatile_access (state_data,
                                                        &foo,
                                                        &foo,
                                                        &foo,
                                                        &get_val,
                                                        &foo)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = same (kv->value, "yes");
  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

/* channel_privilege_level */

static config_err_t
non_volatile_channel_priv_limit_checkout (bmc_config_state_data_t *state_data,
					  const struct config_section *section,
					  struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  config_err_t ret;

  if ((ret = get_bmc_serial_channel_non_volatile_access (state_data,
                                                         &foo,
                                                         &foo,
                                                         &foo,
                                                         &foo,
                                                         &get_val)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (privilege_level_string (get_val))))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static config_err_t
non_volatile_channel_priv_limit_commit (bmc_config_state_data_t *state_data,
					const struct config_section *section,
					const struct config_keyvalue *kv)
{
  uint8_t commit_val;

  commit_val = privilege_level_number (kv->value);
  return serial_channel_non_volatile_access_set (state_data,
						 0, 0,
						 0, 0,
						 0, 0,
						 0, 0,
						 commit_val, 1);
}

static config_diff_t
non_volatile_channel_priv_limit_diff (bmc_config_state_data_t *state_data,
				      const struct config_section *section,
				      const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t foo;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_serial_channel_non_volatile_access (state_data,
                                                        &foo,
                                                        &foo,
                                                        &foo,
                                                        &foo,
                                                        &get_val)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = privilege_level_number (kv->value);
  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   privilege_level_string (get_val));
    }
  return ret;
}

struct config_section *
bmc_config_serial_channel_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section * serial_channel_section = NULL;
  char *section_comment = 
    "In the Serial_Channel section, IPMI over Serial communication can be "
    "enabled or disabled.  "
    "In the below, \"Volatile\" configurations are immediately "
    "configured onto the BMC and will have immediate effect on the system.  "
    "\"Non_Volatile\" configurations are only available after the next "
    "system reset.  Generally, both the \"Volatile\" and \"Non_Volatile\" "
    "equivalent fields should be configured identically."
    "\n"
    "Most users will only be interested in IPMI over LAN, therefore serial "
    "communication can be disabled.  This can be done by setting "
    "\"Access_Mode\" to \"Disabled\".";

  if (!(serial_channel_section = bmc_config_section_create (state_data, 
                                                            "Serial_Channel",
                                                            "Serial_Channel",
                                                            section_comment,
                                                            0)))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Volatile_Access_Mode",
                                       "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                                       0,
                                       volatile_access_mode_checkout,
                                       volatile_access_mode_commit,
                                       volatile_access_mode_diff,
                                       channel_access_mode_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Volatile_Enable_User_Level_Auth",
                                       "Possible values: Yes/No",
                                       0,
                                       volatile_enable_user_level_auth_checkout,
                                       volatile_enable_user_level_auth_commit,
                                       volatile_enable_user_level_auth_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Volatile_Enable_Per_Message_Auth",
                                       "Possible values: Yes/No",
                                       0,
                                       volatile_enable_per_msg_auth_checkout,
                                       volatile_enable_per_msg_auth_commit,
                                       volatile_enable_per_msg_auth_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Volatile_Enable_Pef_Alerting",
                                       "Possible values: Yes/No",
                                       0,
                                       volatile_enable_pef_alerting_checkout,
                                       volatile_enable_pef_alerting_commit,
                                       volatile_enable_pef_alerting_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Volatile_Channel_Privilege_Limit",
                                       "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       volatile_channel_priv_limit_checkout,
                                       volatile_channel_priv_limit_commit,
                                       volatile_channel_priv_limit_diff,
                                       privilege_level_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Non_Volatile_Access_Mode",
                                       "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared",
                                       0,
                                       non_volatile_access_mode_checkout,
                                       non_volatile_access_mode_commit,
                                       non_volatile_access_mode_diff,
                                       channel_access_mode_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Non_Volatile_Enable_User_Level_Auth",
                                       "Possible values: Yes/No",
                                       0,
                                       non_volatile_enable_user_level_auth_checkout,
                                       non_volatile_enable_user_level_auth_commit,
                                       non_volatile_enable_user_level_auth_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Non_Volatile_Enable_Per_Message_Auth",
                                       "Possible values: Yes/No",
                                       0,
                                       non_volatile_enable_per_msg_auth_checkout,
                                       non_volatile_enable_per_msg_auth_commit,
                                       non_volatile_enable_per_msg_auth_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Non_Volatile_Enable_Pef_Alerting",
                                       "Possible values: Yes/No",
                                       0,
                                       non_volatile_enable_pef_alerting_checkout,
                                       non_volatile_enable_pef_alerting_commit,
                                       non_volatile_enable_pef_alerting_diff,
                                       config_yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       serial_channel_section,
                                       "Non_Volatile_Channel_Privilege_Limit",
                                       "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       non_volatile_channel_priv_limit_checkout,
                                       non_volatile_channel_priv_limit_commit,
                                       non_volatile_channel_priv_limit_diff,
                                       privilege_level_number_validate) < 0)
    goto cleanup;

  return serial_channel_section;

 cleanup:
  if (serial_channel_section)
    bmc_config_section_destroy(state_data, serial_channel_section);
  return NULL;
}

