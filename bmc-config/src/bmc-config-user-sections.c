/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

/* convenience struct */
struct user_access {
  uint8_t user_ipmi_messaging;
  uint8_t user_link_authentication;
  uint8_t user_restricted_to_callback;
  uint8_t privilege_limit;
  uint8_t session_limit;
  uint8_t user_id_enable_status;
};

static config_err_t
_channel_info(bmc_config_state_data_t *state_data,
              const char *key_name,
              uint8_t *channel_number)
{
  config_err_t ret;

  assert(state_data);
  assert(key_name);
  assert(channel_number);
  
  if (stristr(key_name, "Lan")
      || stristr(key_name, "Enable_User"))
    {
      if ((ret = get_lan_channel_number (state_data,
                                         channel_number)) != CONFIG_ERR_SUCCESS)
        return ret;
    }
  else
    {
      if ((ret = get_serial_channel_number (state_data,
                                            channel_number)) != CONFIG_ERR_SUCCESS)
        return ret;
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_get_user_access(bmc_config_state_data_t *state_data,
                 const char *section_name,
                 const char *key_name,
                 struct user_access *ua,
                 unsigned int *username_not_set_yet)
{
  uint8_t userid;
  uint8_t channel_number;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert(state_data);
  assert(section_name);
  assert(key_name);
  assert(ua);
  assert(!(*username_not_set_yet));

  userid = atoi (section_name + strlen ("User"));
  
  if ((ret = _channel_info(state_data,
                           key_name,
                           &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_user_access_rs);

  if (ipmi_cmd_get_user_access (state_data->ipmi_ctx,
                                channel_number,
                                userid,
                                obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_user_access: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      /*
       * IPMI Workaround (achu)
       *
       * Discovered on Sun X4140
       *
       * Get Username and Get User Payload commands fail with CCh =
       * "Invalid data field in request" if a username was not set
       * previously.
       *
       * Although not seen on the Get User Access command, we're going
       * to assume it's possible on some other motherboards.
       */

      if (ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
        (*username_not_set_yet) = 1;

      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "user_ipmi_messaging", &val);
  ua->user_ipmi_messaging = val;

  _FIID_OBJ_GET (obj_cmd_rs, "user_link_authentication", &val);
  ua->user_link_authentication = val;

  _FIID_OBJ_GET (obj_cmd_rs, "user_restricted_to_callback", &val);
  ua->user_restricted_to_callback = val;

  _FIID_OBJ_GET (obj_cmd_rs, "user_privilege_level_limit", &val);
  ua->privilege_limit = val;

  /* XXX: no way to retrieve */
  ua->session_limit = 0;

  _FIID_OBJ_GET (obj_cmd_rs, "user_id_enable_status", &val);
  ua->user_id_enable_status = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_user_access (bmc_config_state_data_t *state_data,
                  const char *section_name,
                  const char *key_name,
                  struct user_access *ua)
{
  uint8_t userid;
  uint8_t channel_number;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert(state_data);
  assert(section_name);
  assert(key_name);
  assert(ua);

  userid = atoi (section_name + strlen ("User"));
  
  if ((ret = _channel_info(state_data,
                           key_name,
                           &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_access_rs);

  /* achu: special case, because the session limit cannot be
   * retrieved.  So if we're committing, we have to get the pre-loaded
   * value and commit it each time.
   */
  if (stristr(key_name, "Serial"))
    {    
      if (state_data->serial_user_session_limit_len)
        ua->session_limit = state_data->serial_user_session_limit[userid-1];
    }
  if (stristr(key_name, "Lan"))
    {
      if (state_data->lan_user_session_limit_len)
        ua->session_limit = state_data->lan_user_session_limit[userid-1];
    }
  
  if (ipmi_cmd_set_user_access (state_data->ipmi_ctx,
                                channel_number,
                                ua->user_ipmi_messaging,
                                ua->user_link_authentication,
                                ua->user_restricted_to_callback,
                                userid,
                                ua->privilege_limit,
                                ua->session_limit,
                                obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_user_access: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
username_checkout (const char *section_name,
		   struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint8_t userid;
  /* achu: *2 b/c of CONFIG_USERNAME_NOT_SET_YET_STR length */
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH*2+1];

  userid = atoi (section_name + strlen ("User"));
		    
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_user_name_rs);

  /* achu: *2 b/c of CONFIG_USERNAME_NOT_SET_YET_STR */
  memset(username, '\0', IPMI_MAX_USER_NAME_LENGTH*2+1);

  if (ipmi_cmd_get_user_name (state_data->ipmi_ctx,
                              userid,
                              obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_user_name: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;

      /* 
       * IPMI Workaround (achu)
       *
       * Discovered on Sun X4140
       *
       * Get Username and Get User Payload commands fail with CCh =
       * "Invalid data field in request" if a username was not set
       * previously.
       */

      if (ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
        {
          strcpy ((char *)username, CONFIG_USERNAME_NOT_SET_YET_STR);
          goto got_data;
        }

      goto cleanup; 
    }

  /* achu: check user_id == 1 after ipmi call to ensure the command can succeed */
  if (userid == 1)
    strcpy ((char *)username, "NULL");
  else
    _FIID_OBJ_GET_DATA (obj_cmd_rs,
                        "user_name",
                        username,
                        IPMI_MAX_USER_NAME_LENGTH);

 got_data:

  /* for backwards compatability with older bmc-configs */
  if (state_data->prog_data->args->config_args.action == CONFIG_ACTION_DIFF
      && userid == 1
      && same(kv->value_input, "anonymous"))
    {
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv,
                                                "anonymous") < 0)
        return CONFIG_ERR_FATAL_ERROR;
    }
  else
    {
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv, 
                                                (char *)username) < 0)
        return CONFIG_ERR_FATAL_ERROR;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
username_commit (const char *section_name,
		 const struct config_keyvalue *kv,
                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t userid = atoi (section_name + strlen ("User"));
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  /* can't change userid 1 */
  if (userid == 1)
    {
      /* anonymous for backwards compatability */
      if (same (kv->value_input, "NULL")
          || same (kv->value_input, "anonymous"))
        return CONFIG_ERR_SUCCESS;
      else
        return CONFIG_ERR_NON_FATAL_ERROR;
    }

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_name_rs);

  if (ipmi_cmd_set_user_name (state_data->ipmi_ctx,
                              userid,
                              kv->value_input,
                              strlen(kv->value_input),
                              obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_user_name: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_validate_t
username_validate (const char *section_name,
                   const char *key_name,
		   const char *value,
                   void *arg)
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

static config_err_t
enable_user_checkout (const char *section_name,
		      struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data,
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        ua.user_id_enable_status = IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED;
      else
        return ret;
    }

  /* 
   * Older IPMI implementations cannot get the value, but new ones
   * can.  If it cannot be checked out, the line will be commented out
   * later on.
   */
  if (ua.user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
    {
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv,
                                                "Yes") < 0)
        return CONFIG_ERR_FATAL_ERROR;
    }
  else if (ua.user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED)
    {
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv,
                                                "No") < 0)
        return CONFIG_ERR_FATAL_ERROR;
    }
  else /* ua.user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED */
    {
      if (config_section_update_keyvalue_output(state_data->pstate,
                                                kv, 
                                                "") < 0)
        return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_user_commit (const char *section_name,
		    const struct config_keyvalue *kv,
                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  int userid = atoi (section_name + strlen ("User"));
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  char password[IPMI_1_5_MAX_PASSWORD_LENGTH];
  uint8_t user_status;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_password_rs);

  if (same (kv->value_input, "yes"))
    user_status = IPMI_PASSWORD_OPERATION_ENABLE_USER;
  else
    user_status = IPMI_PASSWORD_OPERATION_DISABLE_USER;

  memset (password, 0, IPMI_1_5_MAX_PASSWORD_LENGTH);
  if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                  userid,
                                  user_status,
                                  password,
                                  0,
                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_user_password: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));

      /*
       * IPMI Workaround
       *
       * Forgotten/Undocumented Motherboard
       * Sun X4140
       *
       * The IPMI spec says you don't have to set a password when you
       * enable/disable a user.  But some BMCs care that you pass in
       * some random password length (even though the password will be
       * ignored)
       */

      if (ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID) == 1))
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_set_user_password: attempting workaround\n");

          _FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_user_password_rq);

          if (fill_cmd_set_user_password (userid,
                                          user_status,
                                          password,
                                          0,
                                          obj_cmd_rq) < 0)
            goto cleanup;
          
          /* Force the password to be filled in with a length */
          _FIID_OBJ_SET_DATA (obj_cmd_rq,
                              "password",
                              (uint8_t *)password,
                              IPMI_1_5_MAX_PASSWORD_LENGTH);
          
          if (ipmi_cmd (state_data->ipmi_ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_APP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
            {
              if (state_data->prog_data->args->config_args.common.debug)
                pstdout_fprintf(state_data->pstate,
                                stderr,
                                "ipmi_cmd: %s\n",
                                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
              if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
                rv = CONFIG_ERR_NON_FATAL_ERROR;
              goto cleanup;
            }

          if (ipmi_check_completion_code_success(obj_cmd_rs) != 1)
            {
              if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
                rv = CONFIG_ERR_NON_FATAL_ERROR;
              goto cleanup;
            }
        }
      /*
       * IPMI Workaround
       *
       * Dell Poweredge R610
       *
       * Dell says this can only go after a set password command.
       * Save info to possibly retry the enable_user after a password
       * is set.
       */
      else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
	       && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1))
        {
          if (state_data->enable_user_after_password_len)
            {
              state_data->enable_user_after_password[userid-1].enable_user_failed = 1;
              state_data->enable_user_after_password[userid-1].kv = (struct config_keyvalue *)kv;
            }

          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      else
        {
          if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
            rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }      
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rq);
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_check_bmc_user_password (bmc_config_state_data_t *state_data,
                          uint8_t userid,
                          char *password,
                          int *is_same)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(password);
  assert(is_same);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_password_rs);

  if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                  userid,
                                  IPMI_PASSWORD_OPERATION_TEST_PASSWORD,
                                  password,
                                  strlen(password),
                                  obj_cmd_rs) < 0)
    {
      uint64_t comp_code;

      /* don't use wrapper - non-fatal error */
      if (fiid_obj_get(obj_cmd_rs, 
                       "comp_code",
                       &comp_code) < 0)
        {
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT
          || comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT)
        {
          *is_same = 0;
          goto done;
        }
      else
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_set_user_password: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
            rv = CONFIG_ERR_NON_FATAL_ERROR;
        }
      goto cleanup;
    }
  else
    *is_same = 1;

 done:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
password_checkout (const char *section_name,
		   struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char *str = "";

  if (state_data->prog_data->args->config_args.action == CONFIG_ACTION_DIFF)
    {
      uint8_t userid = atoi (section_name + strlen ("User"));
      int is_same;
      config_err_t ret;

      /* special case for diff, since we can't get the password, and
       * return it, we'll check to see if the password is the same.
       * If it is, return the inputted password back for proper
       * diffing.
       */
      if ((ret = _check_bmc_user_password (state_data,
                                           userid,
                                           kv->value_input,
                                           &is_same)) != CONFIG_ERR_SUCCESS)
        return ret;

      if (is_same)
        str = kv->value_input;
      else
        str = "<something else>";
    } 

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            str) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
password_commit (const char *section_name,
		 const struct config_keyvalue *kv,
                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t userid = atoi (section_name + strlen ("User"));
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_password_rs);

  if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                  userid,
                                  IPMI_PASSWORD_OPERATION_SET_PASSWORD,
                                  kv->value_input,
                                  strlen(kv->value_input),
                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_user_password: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (state_data->enable_user_after_password_len
      && state_data->enable_user_after_password[userid-1].enable_user_failed)
    {
      config_err_t ret;

      /* ignore non-fatal error, consider success */
      ret = enable_user_commit (section_name,
                                state_data->enable_user_after_password[userid-1].kv,
                                state_data);
      if (ret == CONFIG_ERR_FATAL_ERROR)
        {
          rv = CONFIG_ERR_FATAL_ERROR;
          goto cleanup;
        }
      if (ret == CONFIG_ERR_SUCCESS)
        {
          /* now it has passed, reset to 0 just in case */
          state_data->enable_user_after_password[userid-1].enable_user_failed  = 0;
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "RETRY: Success on retry to commit `%s:%s'\n",
                           section_name,
                           state_data->enable_user_after_password[userid-1].kv->key->key_name);
        }
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_validate_t
password_validate (const char *section_name,
                   const char *key_name,
		   const char *value,
                   void *arg)
{
  if (strlen (value) <= IPMI_1_5_MAX_PASSWORD_LENGTH)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

static config_err_t
_check_bmc_user_password20 (bmc_config_state_data_t *state_data,
                            uint8_t userid,
                            char *password,
                            int *is_same)

{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(password);
  assert(is_same);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_password_rs);

  if (ipmi_cmd_set_user_password_v20 (state_data->ipmi_ctx,
                                      userid,
                                      IPMI_PASSWORD_SIZE_20_BYTES,
                                      IPMI_PASSWORD_OPERATION_TEST_PASSWORD,
                                      password,
                                      strlen(password),
                                      obj_cmd_rs) < 0)
    {
      uint64_t comp_code;

      /* don't use wrapper - non-fatal error */
      if (fiid_obj_get(obj_cmd_rs, 
                       "comp_code",
                       &comp_code) < 0)
        {
          if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
            rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT
          || comp_code == IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT)
        {
          *is_same = 0;
          goto done;
        }
      else
        {
          if (state_data->prog_data->args->config_args.common.debug)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_set_user_password_v20: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
            rv = CONFIG_ERR_NON_FATAL_ERROR;
        }
      goto cleanup;
    }
  else
    *is_same = 1;

 done:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
password20_checkout (const char *section_name,
		     struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t userid = atoi (section_name + strlen ("User"));
  char *str = "";
  config_err_t ret;
  int is_same;

  /* 
   * IPMI Workaround (achu)
   *
   * On some early Supermicro H8QME motherboards, if the username was
   * not yet set, a password test will automatically fail with 0xff =
   * "Unspecified Error".
   *
   * I don't feel I can work around this issue because 0xff is a
   * "really bad" error code.  Had to rely on the vendor to fix this
   * problem.
   */
  /* achu: password can't be checked out, but we should make sure IPMI
   * 2.0 exists on the system.
   */
  if ((ret = _check_bmc_user_password20 (state_data,
                                         userid,
                                         "foobar",
                                         &is_same)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (state_data->prog_data->args->config_args.action == CONFIG_ACTION_DIFF)
    {
      /* special case for diff, since we can't get the password, and
       * return it, we'll check to see if the password is the same.
       * If it is, return the inputted password back for proper
       * diffing.
       */
      if ((ret = _check_bmc_user_password20 (state_data,
                                             userid,
                                             kv->value_input,
                                             &is_same)) != CONFIG_ERR_SUCCESS)
        return (ret);
      
      if (is_same)
        str = kv->value_input;
      else
        str = "<S0m3th1ng el53>";
    } 

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            str) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
password20_commit (const char *section_name,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t userid = atoi (section_name + strlen ("User"));
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_password_rs);

  if (ipmi_cmd_set_user_password_v20 (state_data->ipmi_ctx,
                                      userid,
                                      IPMI_PASSWORD_SIZE_20_BYTES,
                                      IPMI_PASSWORD_OPERATION_SET_PASSWORD,
                                      kv->value_input,
                                      strlen(kv->value_input),
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_user_password_v20: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (state_data->enable_user_after_password_len
      && state_data->enable_user_after_password[userid-1].enable_user_failed)
    {
      config_err_t ret;

      /* ignore non-fatal error, consider success */
      ret = enable_user_commit (section_name,
                                state_data->enable_user_after_password[userid-1].kv,
                                state_data);
      if (ret == CONFIG_ERR_FATAL_ERROR)
        {
          rv = CONFIG_ERR_FATAL_ERROR;
          goto cleanup;
        }
      if (ret == CONFIG_ERR_SUCCESS)
        {
          /* now it has passed, reset to 0 just in case */
          state_data->enable_user_after_password[userid-1].enable_user_failed  = 0;
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "RETRY: Success on retry to commit`%s:%s'\n",
                           section_name,
                           state_data->enable_user_after_password[userid-1].kv->key->key_name);
        }
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);

}

static config_validate_t
password20_validate (const char *section_name,
                     const char *key_name,
		     const char *value,
                     void *arg)
{
  if (strlen (value) <= IPMI_2_0_MAX_PASSWORD_LENGTH)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

static config_err_t
lan_enable_ipmi_messaging_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ua.user_ipmi_messaging ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_enable_ipmi_messaging_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  ua.user_ipmi_messaging = same (kv->value_input, "yes");

  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}
  
static config_err_t
lan_enable_link_auth_checkout (const char *section_name,
			       struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ua.user_link_authentication ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_enable_link_auth_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  ua.user_link_authentication = same (kv->value_input, "yes");

  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}

static config_err_t
lan_enable_restricted_to_callback_checkout (const char *section_name,
                                            struct config_keyvalue *kv,
                                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ua.user_restricted_to_callback ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_enable_restricted_to_callback_commit (const char *section_name,
                                          const struct config_keyvalue *kv,
                                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  ua.user_restricted_to_callback = same (kv->value_input, "yes");

  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}

static config_err_t
lan_privilege_limit_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            get_privilege_limit_string (ua.privilege_limit)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_privilege_limit_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  ua.privilege_limit = get_privilege_limit_number (kv->value_input);

  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}
  
static config_err_t
lan_session_limit_checkout (const char *section_name,
                            struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;

  /* Special case: There is no way to check out this value */
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            "") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
lan_session_limit_commit (const char *section_name,
                          const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* Session limit field will be grabbed/set in _set_user_access */
  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}

static config_err_t
sol_payload_access_checkout (const char *section_name,
                             struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  int userid = atoi (section_name + strlen ("User"));
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_user_payload_access_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_user_payload_access (state_data->ipmi_ctx,
                                        channel_number,
                                        userid,
                                        obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_user_payload_access: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;

      /*
       * IPMI Workaround (achu)
       *
       * Discovered on Sun X4140
       *
       * Get Username and Get User Payload commands fail with CCh =
       * "Invalid data field in request" if a username was not set
       * previously.
       *
       * Although not seen on the Get User Access command, we're going
       * to assume it's possible on some other motherboards.
       */

      if (ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          goto out;
        }

      goto cleanup;
    }
  
  /* standard_payload_1 is the SOL payload type */
  _FIID_OBJ_GET (obj_cmd_rs, "standard_payload_1", &val);
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            val ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

 out:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
sol_payload_access_commit (const char *section_name,
                           const struct config_keyvalue *kv,
                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  int userid = atoi (section_name + strlen ("User"));
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t operation;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_user_payload_access_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (same (kv->value_input, "yes"))
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_ENABLE;
  else
    operation = IPMI_SET_USER_PAYLOAD_OPERATION_DISABLE;

  if (ipmi_cmd_set_user_payload_access (state_data->ipmi_ctx,
                                        channel_number,
                                        userid,
                                        operation,
                                        1, /* the sol payload */
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
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_user_payload_access: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
serial_enable_ipmi_messaging_checkout (const char *section_name,
                                       struct config_keyvalue *kv,
                                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ua.user_ipmi_messaging ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_enable_ipmi_messaging_commit (const char *section_name,
                                     const struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  ua.user_ipmi_messaging = same (kv->value_input, "yes");
  
  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}
  
static config_err_t
serial_enable_link_auth_checkout (const char *section_name,
                                  struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ua.user_link_authentication ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_enable_link_auth_commit (const char *section_name,
                                const struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  ua.user_link_authentication = same (kv->value_input, "yes");

  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}

static config_err_t
serial_enable_restricted_to_callback_checkout (const char *section_name,
                                               struct config_keyvalue *kv,
                                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ua.user_restricted_to_callback ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_enable_restricted_to_callback_commit (const char *section_name,
                                             const struct config_keyvalue *kv,
                                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  ua.user_restricted_to_callback = same (kv->value_input, "yes");

  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}

static config_err_t
serial_privilege_limit_checkout (const char *section_name,
                                 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (config_section_update_keyvalue_output(state_data->pstate,
                                                    kv,
                                                    CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return CONFIG_ERR_FATAL_ERROR;
          return CONFIG_ERR_SUCCESS;
        }
      else
        return ret;
    }

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            get_privilege_limit_string (ua.privilege_limit)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_privilege_limit_commit (const char *section_name,
                               const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  ua.privilege_limit = get_privilege_limit_number (kv->value_input);

  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}

static config_err_t
serial_session_limit_checkout (const char *section_name,
                               struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;

  /* Special case: There is no way to check out this value */
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            "") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
serial_session_limit_commit (const char *section_name,
                             const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  config_err_t ret;

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access(state_data, 
                              section_name,
                              kv->key->key_name,
                              &ua,
                              &username_not_set_yet)) != CONFIG_ERR_SUCCESS)
    return ret;

  /* Session limit field will be grabbed/set in _set_user_access */
  return _set_user_access (state_data,
                           section_name,
                           kv->key->key_name,
                           &ua);
}

struct config_section *
bmc_config_user_section_get (bmc_config_state_data_t *state_data, int userid)
{
  struct config_section *user_section = NULL;
  char section_name[CONFIG_MAX_SECTION_NAME_LEN];
  char *section_comment_text = 
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
    "User1.  For most users, \"Lan_Session_Limit\" can be set to 0 "
    "(or ignored) to support an unlimited number of simultaneous "
    "IPMI over LAN sessions. "
    "\n"
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), a "
    "\"Password20\" and \"SOL_Payload_Access\" field may be listed below.  "
    "\"Password20\" may be used to set up to a 20 byte password for the "
    "username rather than a maximum 16 byte password.  Its use is optional.  "
    "Set the \"SOL_Payload_Access\" field to \"Yes\" or \"No\" to enable or disable "
    "this username's ability to access SOL."
    "\n"
    "Please do not forget to uncomment those fields, such as \"Password\", "
    "that may be commented out during the checkout."
    "\n"
    "Some motherboards may require a \"Username\" to be configured prior to other "
    "fields being read/written.  If this is the case, those fields will be set to "
    "%s.";
  char section_comment[4096];
  unsigned int verbose_flags = 0;

  if (userid <= 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr, 
                      "Invalid Userid = %d\n", 
                      userid);
      return NULL;
    }

  snprintf(section_name, CONFIG_MAX_SECTION_NAME_LEN, "User%d", userid);

  if (userid == 1)
    {
      memset(section_comment, '\0', 4096);

      snprintf(section_comment,
               4096,
               section_comment_text,
               CONFIG_USERNAME_NOT_SET_YET_STR);

      if (!(user_section = config_section_create(state_data->pstate,
                                                 section_name,
                                                 "UserX",
                                                 section_comment,
                                                 0,
                                                 NULL,
                                                 NULL)))
        goto cleanup;
    }
  else
    {
      if (!(user_section = config_section_create(state_data->pstate,
                                                 section_name,
                                                 NULL,
                                                 NULL,
                                                 0,
                                                 NULL,
                                                 NULL)))
        goto cleanup;
    }

  if (state_data->prog_data->args->config_args.verbose)
    verbose_flags = 0;
  else
    verbose_flags = CONFIG_DO_NOT_CHECKOUT;

  /* userid 1 is the NULL username, so comment it out by default */
  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Username",
                              "Give Username",
                              (userid == 1) ? CONFIG_CHECKOUT_KEY_COMMENTED_OUT : 0 | CONFIG_USERNAME_NOT_SET_YET,
                              username_checkout,
                              username_commit,
                              username_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Password",
                              "Give password or blank to clear. MAX 16 chars.",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                              password_checkout,
                              password_commit,
                              password_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Password20",
                              "Give password for IPMI 2.0 or blank to clear. MAX 20 chars.",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                              password20_checkout,
                              password20_commit,
                              password20_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Enable_User",
                              "Possible values: Yes/No or blank to not set",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              enable_user_checkout,
                              enable_user_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Lan_Enable_IPMI_Msgs",
                              "Possible values: Yes/No",
                              CONFIG_USERNAME_NOT_SET_YET,
                              lan_enable_ipmi_messaging_checkout,
                              lan_enable_ipmi_messaging_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Lan_Enable_Link_Auth",
                              "Possible values: Yes/No",
                              CONFIG_USERNAME_NOT_SET_YET,
                              lan_enable_link_auth_checkout,
                              lan_enable_link_auth_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Lan_Enable_Restricted_to_Callback",
                              "Possible values: Yes/No",
                              CONFIG_USERNAME_NOT_SET_YET,
                              lan_enable_restricted_to_callback_checkout,
                              lan_enable_restricted_to_callback_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Lan_Enable_Restrict_to_Callback",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT | CONFIG_USERNAME_NOT_SET_YET,
                              lan_enable_restricted_to_callback_checkout,
                              lan_enable_restricted_to_callback_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Lan_Privilege_Limit",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                              CONFIG_USERNAME_NOT_SET_YET,
                              lan_privilege_limit_checkout,
                              lan_privilege_limit_commit,
                              get_privilege_limit_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Lan_Session_Limit",
                              "Possible values: 0-17, 0 is unlimited; May be reset to 0 if not specified",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY | CONFIG_USERNAME_NOT_SET_YET,
                              lan_session_limit_checkout,
                              lan_session_limit_commit,
                              config_number_range_four_bits) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "SOL_Payload_Access",
                              "Possible values: Yes/No",
                              CONFIG_USERNAME_NOT_SET_YET,
                              sol_payload_access_checkout,
                              sol_payload_access_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Serial_Enable_IPMI_Msgs",
                              "Possible values: Yes/No",
                              verbose_flags | CONFIG_USERNAME_NOT_SET_YET,
                              serial_enable_ipmi_messaging_checkout,
                              serial_enable_ipmi_messaging_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Serial_Enable_Link_Auth",
                              "Possible values: Yes/No",
                              verbose_flags | CONFIG_USERNAME_NOT_SET_YET,
                              serial_enable_link_auth_checkout,
                              serial_enable_link_auth_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Serial_Enable_Restricted_to_Callback",
                              "Possible values: Yes/No",
                              verbose_flags | CONFIG_USERNAME_NOT_SET_YET,
                              serial_enable_restricted_to_callback_checkout,
                              serial_enable_restricted_to_callback_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Serial_Enable_Restrict_to_Callback",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT | CONFIG_USERNAME_NOT_SET_YET,
                              serial_enable_restricted_to_callback_checkout,
                              serial_enable_restricted_to_callback_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Serial_Privilege_Limit",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                              verbose_flags | CONFIG_USERNAME_NOT_SET_YET,
                              serial_privilege_limit_checkout,
                              serial_privilege_limit_commit,
                              get_privilege_limit_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              user_section,
                              "Serial_Session_Limit",
                              "Possible values: 0-17, 0 is unlimited; May be reset to 0 if not specified",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY | verbose_flags | CONFIG_USERNAME_NOT_SET_YET,
                              serial_session_limit_checkout,
                              serial_session_limit_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return user_section;

 cleanup:
  if (user_section)
    config_section_destroy(state_data->pstate, user_section);
  return NULL;
}
