#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-utils.h"
#include "bmc-config-validate.h"

struct user_section
{
  int userid;

  int lan_channel_number_loaded;
  int8_t lan_channel_number;
  int serial_channel_number_loaded;
  int8_t serial_channel_number;

  int username_loaded;
  int commit_username;
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1];

  int commit_enable_user;
  uint8_t enable_user;

  int commit_password;
  uint8_t password[IPMI_1_5_MAX_PASSWORD_LENGTH+1];

  int commit_password20;
  uint8_t password20[IPMI_2_0_MAX_PASSWORD_LENGTH+1];

  int lan_get_user_access_loaded;
  int commit_lan_set_user_access;
  uint8_t lan_user_ipmi_messaging;
  uint8_t lan_user_link_authentication;
  uint8_t lan_user_restricted_to_callback;
  uint8_t lan_privilege_limit;
  uint8_t lan_session_limit;
  uint8_t lan_user_id_enable_status;

  int serial_get_user_access_loaded;
  int commit_serial_set_user_access;
  uint8_t serial_user_ipmi_messaging;
  uint8_t serial_user_link_authentication;
  uint8_t serial_user_restricted_to_callback;
  uint8_t serial_privilege_limit;
  uint8_t serial_session_limit;
  uint8_t serial_user_id_enable_status;

  int sol_payload_access_loaded;
  int commit_sol_payload_access;
  uint8_t sol_payload_access;
};

static struct user_section *
_create_user_section(void)
{
  struct user_section *sect;

  if (!(sect = (struct user_section *)malloc(sizeof(struct user_section))))
    return NULL;
  memset(sect, '\0', sizeof(struct user_section));

  return sect;
}

static void
_destroy_user_section(struct user_section *user_sect)
{
  if (user_sect)
    free(user_sect);
}

static bmc_err_t
user_section_load_config (bmc_config_state_data_t *state_data,
                          struct section *sect)
{
  fiid_obj_t obj_cmd_rs = NULL;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret;
  struct user_section *user_sect;

  if (!(user_sect = _create_user_section()))
    goto cleanup;
  
  user_sect->userid = atoi (sect->section_name + strlen ("User"));

  if ((ret = get_lan_channel_number (state_data,
                                     &(user_sect->lan_channel_number))) == BMC_ERR_FATAL_ERROR)
    {
      rv = ret;
      goto cleanup;
    }
  if (ret == BMC_ERR_SUCCESS)
    user_sect->lan_channel_number_loaded++;

  if ((ret = get_serial_channel_number (state_data,
                                        &(user_sect->serial_channel_number))) == BMC_ERR_FATAL_ERROR)
    {
      rv = ret;
      goto cleanup;
    }
  if (ret == BMC_ERR_SUCCESS)
    user_sect->serial_channel_number_loaded++;

  if (state_data->prog_data->args->action == BMC_ACTION_CHECKOUT
      || state_data->prog_data->args->action == BMC_ACTION_DIFF)
    {
      if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_name_rs)))
        goto cleanup;
      
      if (!ipmi_cmd_get_user_name (state_data->dev,
                                   user_sect->userid,
                                   obj_cmd_rs))
        {
          if (user_sect->userid == 1)
            strcpy ((char *)user_sect->username, "NULL");
          else
            {
              if (fiid_obj_get_data (obj_cmd_rs,
                                     "user_name",
                                     user_sect->username,
                                     IPMI_MAX_USER_NAME_LENGTH+1) < 0)
                goto cleanup;
            }
          user_sect->username_loaded++;
        }

      if (obj_cmd_rs)
        fiid_obj_destroy(obj_cmd_rs);
    }

  if (state_data->prog_data->args->action == BMC_ACTION_CHECKOUT
      || state_data->prog_data->args->action == BMC_ACTION_DIFF
      || state_data->prog_data->args->action == BMC_ACTION_COMMIT)
    {
      uint64_t val;

      if (user_sect->lan_channel_number_loaded)
        {
          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_access_rs)))
            goto cleanup;
          
          if (!ipmi_cmd_get_user_access (state_data->dev,
                                         user_sect->lan_channel_number,
                                         user_sect->userid,
                                         obj_cmd_rs))
            { 
              if (fiid_obj_get (obj_cmd_rs, "user_ipmi_messaging", &val) < 0)
                goto cleanup;
              user_sect->lan_user_ipmi_messaging = (uint8_t) val;
              
              if (fiid_obj_get (obj_cmd_rs, "user_link_authentication", &val) < 0)
                goto cleanup;
              user_sect->lan_user_link_authentication = (uint8_t) val;
              
              if (fiid_obj_get (obj_cmd_rs, "user_restricted_to_callback", &val) < 0)
                goto cleanup;
              user_sect->lan_user_restricted_to_callback = (uint8_t) val;
              
              if (fiid_obj_get (obj_cmd_rs, "user_privilege_level_limit", &val) < 0)
                goto cleanup;
              user_sect->lan_privilege_limit = (uint8_t) val;
              
              /* XXX: achu: can't get this.  Just assume zero. */
              user_sect->lan_session_limit = 0;
              
              if (fiid_obj_get (obj_cmd_rs, "user_id_enable_status", &val) < 0)
                goto cleanup;
              user_sect->lan_user_id_enable_status = (uint8_t) val;
              
              user_sect->lan_get_user_access_loaded++;
            }

          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }

      if (user_sect->serial_channel_number_loaded)
        {
          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_access_rs)))
            goto cleanup;

          if (!ipmi_cmd_get_user_access (state_data->dev,
                                         user_sect->serial_channel_number,
                                         user_sect->userid,
                                         obj_cmd_rs))
            {
              if (fiid_obj_get (obj_cmd_rs, "user_ipmi_messaging", &val) < 0)
                goto cleanup;
              user_sect->serial_user_ipmi_messaging = (uint8_t) val;
              
              if (fiid_obj_get (obj_cmd_rs, "user_link_authentication", &val) < 0)
                goto cleanup;
              user_sect->serial_user_link_authentication = (uint8_t) val;
              
              if (fiid_obj_get (obj_cmd_rs, "user_restricted_to_callback", &val) < 0)
                goto cleanup;
              user_sect->serial_user_restricted_to_callback = (uint8_t) val;
              
              if (fiid_obj_get (obj_cmd_rs, "user_privilege_level_limit", &val) < 0)
                goto cleanup;
              user_sect->serial_privilege_limit = (uint8_t) val;
              
              /* XXX: achu: can't get this.  Just assume zero. */
              user_sect->serial_session_limit = 0;
              
              if (fiid_obj_get (obj_cmd_rs, "user_id_enable_status", &val) < 0)
                goto cleanup;
              user_sect->serial_user_id_enable_status = (uint8_t) val;
              
              user_sect->serial_get_user_access_loaded++;
            }

          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }

      if (user_sect->lan_channel_number_loaded)
        {
          uint64_t val;

          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_payload_access_rs)))
            goto cleanup;
          
          if (ipmi_cmd_get_user_payload_access (state_data->dev,
                                                user_sect->lan_channel_number,
                                                user_sect->userid,
                                                obj_cmd_rs) < 0)
            {
              rv = BMC_ERR_NON_FATAL_ERROR;
              goto cleanup;
            }
          
          if (fiid_obj_get (obj_cmd_rs, "standard_payload_1", &val) < 0)
            goto cleanup;

          user_sect->sol_payload_access = val;
          user_sect->sol_payload_access_loaded++;

          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }
    }
  
  sect->sectionptr = user_sect;
  return BMC_ERR_SUCCESS;

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  _destroy_user_section(user_sect);
  return rv;
}

static bmc_err_t
user_section_write_config (bmc_config_state_data_t *state_data,
                           struct section *sect)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;

  if (!sect->sectionptr)
    return BMC_ERR_SUCCESS;

  if (state_data->prog_data->args->action == BMC_ACTION_COMMIT)
    {
      struct user_section *user_sect;
  
      user_sect = (struct user_section *)sect->sectionptr;

      if (user_sect->commit_username)
        {
          fiid_obj_t obj_cmd_rs = NULL;

          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_name_rs)))
            goto cleanup;

          if (ipmi_cmd_set_user_name (state_data->dev,
                                      user_sect->userid,
                                      user_sect->username,
                                      (user_sect->username) ? strlen((char *)user_sect->username) : 0,
                                      obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->verbose)
                fprintf (stderr, "FATAL: Error commiting Username in `%s'\n", sect->section_name);
            }

          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }

      if (user_sect->commit_enable_user)
        {
          fiid_obj_t obj_cmd_rq = NULL;
          fiid_obj_t obj_cmd_rs = NULL;
          uint8_t password[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];
                  
          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
            goto cleanup;
          
          memset (password, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH);

          if (ipmi_cmd_set_user_password (state_data->dev,
                                          user_sect->userid,
                                          (user_sect->enable_user ? IPMI_PASSWORD_OPERATION_ENABLE_USER :
                                           IPMI_PASSWORD_OPERATION_DISABLE_USER),
                                          (char *)password,
                                          0,
                                          obj_cmd_rs) < 0)
            {
              /*
               * Workaround: achu: the IPMI spec says you don't have to set a
               * password when you enable/disable a user.  But some BMCs care
               * that you do (even though the password will be ignored).  So
               * we'll try again if the completion code is
               * IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID.
               */
              int8_t ret;

              if ((ret = ipmi_check_completion_code (obj_cmd_rs,
                                                     IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID)) < 0)
                {
                  if (state_data->prog_data->args->verbose)
                    fprintf (stderr, "FATAL: Error commiting Enable_User in `%s'\n", sect->section_name);
                  goto enable_user_cleanup;
                }

              if (!ret)
                {
                  if (state_data->prog_data->args->verbose)
                    fprintf (stderr, "FATAL: Error commiting Enable_User in `%s'\n", sect->section_name);
                  goto enable_user_cleanup;
                }

              if (!(obj_cmd_rq = fiid_obj_create(tmpl_cmd_set_user_password_rq)))
                goto cleanup;

              if (fill_cmd_set_user_password (user_sect->userid,
                                              (user_sect->enable_user ? IPMI_PASSWORD_OPERATION_ENABLE_USER :
                                               IPMI_PASSWORD_OPERATION_DISABLE_USER),
                                              (char *)password,
                                              0,
                                              obj_cmd_rq) < 0)
                goto cleanup;

              /* Force the password to be filled in */
              if (fiid_obj_set_data (obj_cmd_rq,
                                     "password",
                                     (char *)password, 
                                     IPMI_1_5_MAX_PASSWORD_LENGTH) < 0)
                goto cleanup;
              
              if (ipmi_cmd (state_data->dev,
                            IPMI_BMC_IPMB_LUN_BMC,
                            IPMI_NET_FN_APP_RQ,
                            obj_cmd_rq,
                            obj_cmd_rs) < 0)
                {
                  if (state_data->prog_data->args->verbose)
                    fprintf (stderr, "FATAL: Error commiting Enable_User in `%s'\n", sect->section_name);
                  goto enable_user_cleanup;
                }

              if (ipmi_check_completion_code_success(obj_cmd_rs) != 1)
                {
                  if (state_data->prog_data->args->verbose)
                    fprintf (stderr, "FATAL: Error commiting Enable_User in `%s'\n", sect->section_name);
                }
            }
          
        enable_user_cleanup:
          if (obj_cmd_rq)
            fiid_obj_destroy(obj_cmd_rq);
          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }

      if (user_sect->commit_password)
        {
          fiid_obj_t obj_cmd_rs = NULL;

          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
            goto cleanup;

          if (ipmi_cmd_set_user_password (state_data->dev,
                                          user_sect->userid,
                                          IPMI_PASSWORD_OPERATION_SET_PASSWORD,
                                          (char *)user_sect->password,
                                          (user_sect->password) ? strlen((char *)user_sect->password) : 0,
                                          obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->verbose)
                fprintf (stderr, "FATAL: Error commiting Password in `%s'\n", sect->section_name);
            }
          
          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }

      if (user_sect->commit_password20)
        {
          fiid_obj_t obj_cmd_rs = NULL;
          
          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
            goto cleanup;
          
          if (ipmi_cmd_set_user_password_v20 (state_data->dev,
                                              user_sect->userid,
                                              IPMI_PASSWORD_SIZE_20_BYTES,
                                              IPMI_PASSWORD_OPERATION_SET_PASSWORD,
                                              (char *)user_sect->password20,
                                              (user_sect->password20) ? strlen((char *)user_sect->password20) : 0,
                                              obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->verbose)
                fprintf (stderr, "FATAL: Error commiting Password20 in `%s'\n", sect->section_name);
            }
          
          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }

      if (user_sect->lan_channel_number_loaded
          && user_sect->lan_get_user_access_loaded
          && user_sect->commit_lan_set_user_access)
        {  
          fiid_obj_t obj_cmd_rs = NULL;

          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_access_rs)))
            goto cleanup;

          if (ipmi_cmd_set_user_access (state_data->dev,
                                        user_sect->lan_channel_number,
                                        user_sect->lan_user_ipmi_messaging,
                                        user_sect->lan_user_link_authentication,
                                        user_sect->lan_user_restricted_to_callback,
                                        user_sect->userid,
                                        user_sect->lan_privilege_limit,
                                        user_sect->lan_session_limit,
                                        obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->verbose)
                fprintf (stderr, "FATAL: Error commiting LAN User Access in `%s'\n", sect->section_name);
            }
          
          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }
      
      if (user_sect->serial_channel_number_loaded
          && user_sect->serial_get_user_access_loaded
          && user_sect->commit_lan_set_user_access)
        {
          fiid_obj_t obj_cmd_rs = NULL;

          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_access_rs)))
            goto cleanup;

          if (ipmi_cmd_set_user_access (state_data->dev,
                                        user_sect->serial_channel_number,
                                        user_sect->serial_user_ipmi_messaging,
                                        user_sect->serial_user_link_authentication,
                                        user_sect->serial_user_restricted_to_callback,
                                        user_sect->userid,
                                        user_sect->serial_privilege_limit,
                                        user_sect->serial_session_limit,
                                        obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->verbose)
                fprintf (stderr, "FATAL: Error commiting Serial User Access in `%s'\n", sect->section_name);
            }
          
          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }

      if (user_sect->lan_channel_number_loaded
          && user_sect->sol_payload_access_loaded
          && user_sect->commit_sol_payload_access)
        {
          fiid_obj_t obj_cmd_rs = NULL;
          uint8_t operation;
          
          if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_payload_access_rs)))
            goto cleanup;
          
          if (user_sect->sol_payload_access)
            operation = IPMI_SET_USER_PAYLOAD_OPERATION_ENABLE;
          else
            operation = IPMI_SET_USER_PAYLOAD_OPERATION_DISABLE;

          if (ipmi_cmd_set_user_payload_access (state_data->dev,
                                                user_sect->lan_channel_number,
                                                user_sect->userid,
                                                operation,
                                                1,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->verbose)
                fprintf (stderr, "FATAL: Error commiting SOL Payload Access `%s'\n", sect->section_name);
            }
          
          if (obj_cmd_rs)
            fiid_obj_destroy(obj_cmd_rs);
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static void
user_section_cleanup (bmc_config_state_data_t *state_data,
                      struct section *sect)
{
  _destroy_user_section(sect->sectionptr);
  sect->sectionptr = NULL;
}

static bmc_err_t
username_checkout (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->username_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ((char *)user_sect->username)))
    {
      perror("strdup");
      goto cleanup;
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
username_commit (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  /* Special Case for user id 1 == null username */
  if (user_sect->userid != 1)
    {
      if (kv->value)
        strncpy(user_sect->username, kv->value, IPMI_MAX_USER_NAME_LENGTH);
      else
        memset(user_sect->username, '\0', IPMI_MAX_USER_NAME_LENGTH);
      user_sect->commit_username++;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
username_diff (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->username_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (user_sect->userid == 1) 
    {
      if (! kv->value || same (kv->value, "null") || same (kv->value, "anonymous")) 
        rv = BMC_DIFF_SAME;
      else 
        rv = BMC_DIFF_DIFFERENT;
    } 
  else 
    {
      if (!kv->value || !same (kv->value, (char *)user_sect->username))
        rv = BMC_DIFF_DIFFERENT;
      else
        rv = BMC_DIFF_SAME;
    }

  if (rv == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 (char *)user_sect->username);

 cleanup:
  return rv;
}

static bmc_validate_t
username_validate (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const char *value)
{
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (user_sect->userid == 1) 
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

static bmc_err_t
enable_user_checkout (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  /* 
   * Older IPMI implementations cannot get the value, but new ones
   * can.  If it cannot be checked out, the line will be commented out
   * later on b/c BMC_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY flag
   * has been set.
   */
  if (user_sect->lan_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else if (user_sect->lan_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED)
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else /* user_sect->lan_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED */
    {
      if (!(kv->value = strdup ("")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
enable_user_commit (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  user_sect->enable_user = same (kv->value, "yes");
  user_sect->commit_enable_user++;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
enable_user_diff (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;
  
  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (kv->value)
    free (kv->value);

  /* Cant get, assume equal */
  if (user_sect->lan_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED)
    rv = BMC_DIFF_SAME;
  else
    {
      passed_val = same (kv->value, "Yes");

      if ((passed_val && user_sect->lan_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
          || (!passed_val && user_sect->lan_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED))
        rv = BMC_DIFF_SAME;
      else
        {
          rv = BMC_DIFF_DIFFERENT;
          report_diff (sect->section_name,
                       kv->key,
                       kv->value,
                       (user_sect->lan_user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED) ? "Yes" : "No");
        }
    }

 cleanup:
  return rv;
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
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (kv->value)
    strncpy(user_sect->password, kv->value, IPMI_1_5_MAX_PASSWORD_LENGTH);
  user_sect->commit_password++;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
password_diff (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const struct keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password (state_data->dev,
                                  user_sect->userid,
                                  IPMI_PASSWORD_OPERATION_TEST_PASSWORD,
                                  (char *)kv->value,
                                  (kv->value) ? strlen((char *)kv->value) : 0,
                                  obj_cmd_rs) < 0)
    {
      uint64_t comp_code;

      if (fiid_obj_get(obj_cmd_rs, "comp_code", &comp_code) < 0)
        {
          rv = BMC_DIFF_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT
          || comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT)
        {
          rv = BMC_DIFF_DIFFERENT;
          report_diff (sect->section_name,
                       kv->key,
                       kv->value,
                       "<something else>");
          goto cleanup;
        }
      else
        {
          rv = BMC_DIFF_NON_FATAL_ERROR;
          goto cleanup;
        }
    }

  rv = BMC_DIFF_SAME;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return rv;
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

bmc_diff_t
_check_bmc_user_password20 (bmc_config_state_data_t *state_data,
                            uint8_t userid,
                            uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password_v20 (state_data->dev,
                                      userid,
                                      IPMI_PASSWORD_SIZE_20_BYTES,
                                      IPMI_PASSWORD_OPERATION_TEST_PASSWORD,
                                      (char *)password,
                                      (password) ? strlen((char *)password) : 0,
                                      obj_cmd_rs) < 0)
    {
      uint64_t comp_code;

      if (fiid_obj_get(obj_cmd_rs, "comp_code", &comp_code) < 0)
        {
          rv = BMC_DIFF_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT
          || comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT)
        rv = BMC_DIFF_DIFFERENT;
      else
        rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = BMC_DIFF_SAME;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static bmc_err_t
password20_checkout (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  bmc_diff_t ret;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  /* achu: password can't be checked out.  But before giving the user
   * a prompt to enter an IPMI 2.0 password, we should make sure IPMI
   * 2.0 exists on the system first.
   */
  if (kv->value)
    free (kv->value);

  if ((ret = _check_bmc_user_password20 (state_data,
                                         user_sect->userid,
                                         "foobar")) == BMC_DIFF_FATAL_ERROR)
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
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (kv->value)
    strncpy(user_sect->password20, kv->value, IPMI_2_0_MAX_PASSWORD_LENGTH);
  user_sect->commit_password20++;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
password20_diff (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  bmc_diff_t rv;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  rv = _check_bmc_user_password20 (state_data,
                                   user_sect->userid,
                                   (uint8_t *)kv->value);

  if (rv == BMC_DIFF_FATAL_ERROR || rv == BMC_DIFF_NON_FATAL_ERROR)
    return rv;

  if (rv == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 "<something else>");
  return rv;
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

static bmc_err_t
lan_enable_ipmi_msgs_checkout (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (user_sect->lan_user_ipmi_messaging)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
lan_enable_ipmi_msgs_commit (bmc_config_state_data_t *state_data,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->lan_user_ipmi_messaging = same (kv->value, "yes");
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
lan_enable_ipmi_msgs_diff (bmc_config_state_data_t *state_data,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == user_sect->lan_user_ipmi_messaging)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   user_sect->lan_user_ipmi_messaging ? "Yes" : "No");
    }

 cleanup:
  return rv;
}
  
static bmc_err_t
lan_enable_link_auth_checkout (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (user_sect->lan_user_link_authentication)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
lan_enable_link_auth_commit (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->lan_user_link_authentication = same (kv->value, "yes");
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
lan_enable_link_auth_diff (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == user_sect->lan_user_link_authentication)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   user_sect->lan_user_link_authentication ? "Yes" : "No");
    }
 cleanup:
  return rv;
}

static bmc_err_t
lan_enable_restricted_to_callback_checkout (bmc_config_state_data_t *state_data,
                                            const struct section *sect,
                                            struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (user_sect->lan_user_restricted_to_callback)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
lan_enable_restricted_to_callback_commit (bmc_config_state_data_t *state_data,
                                          const struct section *sect,
                                          const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->lan_user_restricted_to_callback = same (kv->value, "yes");
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
lan_enable_restricted_to_callback_diff (bmc_config_state_data_t *state_data,
                                        const struct section *sect,
                                        const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == user_sect->lan_user_restricted_to_callback)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   user_sect->lan_user_restricted_to_callback ? "Yes" : "No");
    }
 cleanup:
  return rv;
}

static bmc_err_t
lan_privilege_limit_checkout (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (get_privilege_limit_string (user_sect->lan_privilege_limit))))
    {
      perror("strdup");
      goto cleanup;
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
lan_privilege_limit_commit (bmc_config_state_data_t *state_data,
                            const struct section *sect,
                            const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->lan_privilege_limit = get_privilege_limit_number (kv->value);
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
lan_privilege_limit_diff (bmc_config_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = get_privilege_limit_number (kv->value);

  if (passed_val == user_sect->lan_privilege_limit)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_privilege_limit_string (user_sect->lan_privilege_limit));
    }
 cleanup:
  return rv;
}
  
static bmc_err_t
lan_session_limit_checkout (bmc_config_state_data_t *state_data,
                            const struct section *sect,
                            struct keyvalue *kv)
{
  /* Here for backwards compatability */
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
lan_session_limit_commit (bmc_config_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  /* Here for backwards compatability */
  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
lan_session_limit_diff (bmc_config_state_data_t *state_data,
                        const struct section *sect,
                        const struct keyvalue *kv)
{
  /* Here for backwards compatability */
  return BMC_DIFF_SAME;
}

static bmc_err_t
serial_enable_ipmi_msgs_checkout (bmc_config_state_data_t *state_data,
                                  const struct section *sect,
                                  struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (user_sect->serial_user_ipmi_messaging)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
serial_enable_ipmi_msgs_commit (bmc_config_state_data_t *state_data,
                                const struct section *sect,
                                const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->serial_user_ipmi_messaging = same (kv->value, "yes");
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
serial_enable_ipmi_msgs_diff (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == user_sect->serial_user_ipmi_messaging)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   user_sect->serial_user_ipmi_messaging ? "Yes" : "No");
    }
 cleanup:
  return rv;
}
  
static bmc_err_t
serial_enable_link_auth_checkout (bmc_config_state_data_t *state_data,
                                  const struct section *sect,
                                  struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (user_sect->serial_user_link_authentication)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
serial_enable_link_auth_commit (bmc_config_state_data_t *state_data,
                                const struct section *sect,
                                const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->serial_user_link_authentication = same (kv->value, "yes");
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
serial_enable_link_auth_diff (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == user_sect->serial_user_link_authentication)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   user_sect->serial_user_link_authentication ? "Yes" : "No");
    }
 cleanup:
  return rv;
}

static bmc_err_t
serial_enable_restricted_to_callback_checkout (bmc_config_state_data_t *state_data,
                                               const struct section *sect,
                                               struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (user_sect->serial_user_restricted_to_callback)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
serial_enable_restricted_to_callback_commit (bmc_config_state_data_t *state_data,
                                             const struct section *sect,
                                             const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->serial_user_restricted_to_callback = same (kv->value, "yes");
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
serial_enable_restricted_to_callback_diff (bmc_config_state_data_t *state_data,
                                           const struct section *sect,
                                           const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = same (kv->value, "Yes");

  if (passed_val == user_sect->serial_user_restricted_to_callback)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   user_sect->serial_user_restricted_to_callback ? "Yes" : "No");
    }
 cleanup:
  return rv;
}

static bmc_err_t
serial_privilege_limit_checkout (bmc_config_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (get_privilege_limit_string (user_sect->serial_privilege_limit))))
    {
      perror("strdup");
      goto cleanup;
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_err_t
serial_privilege_limit_commit (bmc_config_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  user_sect->serial_privilege_limit = get_privilege_limit_number (kv->value);
  user_sect->commit_lan_set_user_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
serial_privilege_limit_diff (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->serial_get_user_access_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = get_privilege_limit_number (kv->value);

  if (passed_val == user_sect->serial_privilege_limit)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_privilege_limit_string (user_sect->serial_privilege_limit));
    }
 cleanup:
  return rv;
}

static bmc_err_t
serial_session_limit_checkout (bmc_config_state_data_t *state_data,
                               const struct section *sect,
                               struct keyvalue *kv)
{
  /* Here for backwards compatability */
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
serial_session_limit_commit (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  /* Here for backwards compatability */
  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
serial_session_limit_diff (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  /* Here for backwards compatability */
  return BMC_DIFF_SAME;
}

static bmc_err_t
sol_payload_access_checkout (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->sol_payload_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (user_sect->sol_payload_access)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          goto cleanup;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          goto cleanup;
        }
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv; 
}

static bmc_err_t
sol_payload_access_commit (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const struct keyvalue *kv)
{
  struct user_section *user_sect;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->sol_payload_access_loaded))
    {
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  user_sect->sol_payload_access = same (kv->value, "yes");
  user_sect->commit_sol_payload_access++;

  rv = BMC_ERR_SUCCESS;
 cleanup:
  return rv;
}

static bmc_diff_t
sol_payload_access_diff (bmc_config_state_data_t *state_data,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  uint8_t passed_val;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  struct user_section *user_sect;

  user_sect = (struct user_section *)sect->sectionptr;

  if (!(user_sect->lan_channel_number_loaded))
    {
      rv = BMC_DIFF_NON_FATAL_ERROR;
      goto cleanup;
    }

  passed_val = same (kv->value, "yes");

  if (passed_val == user_sect->sol_payload_access)
    rv = BMC_DIFF_SAME;
  else 
    {
      rv = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   user_sect->sol_payload_access ? "Yes" : "No");
    }
  
 cleanup:
  return rv;
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
                                                 user_section_load_config,
                                                 user_section_write_config,
                                                 user_section_cleanup)))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       user_section,
                                       "Username",
                                       "Give Username",
                                       0,
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

  return user_section;

 cleanup:
  if (user_section)
    bmc_config_section_destroy(state_data, user_section);
  return NULL;
}
