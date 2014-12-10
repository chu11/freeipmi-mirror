/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
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

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience struct */
struct user_access {
  uint8_t user_ipmi_messaging;
  uint8_t user_link_authentication;
  uint8_t user_restricted_to_callback;
  uint8_t privilege_limit;
  uint8_t session_limit;
  uint8_t user_id_enable_status;
};

#define NODE_BUSY_RETRY_COUNT 10

static ipmi_config_err_t enable_user_commit (ipmi_config_state_data_t *state_data,
					     const char *section_name,
                                             const struct ipmi_config_keyvalue *kv);

static ipmi_config_err_t
_channel_info (ipmi_config_state_data_t *state_data,
               const char *section_name,
               const char *key_name,
               uint8_t *channel_number)
{
  ipmi_config_err_t ret;
  char *ptr;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (channel_number);

  if ((ptr = stristr (key_name, "Channel_")))
    {
      (*channel_number) = atoi (ptr + strlen ("Channel_"));
      return (IPMI_CONFIG_ERR_SUCCESS);
    }

  if (stristr (key_name, "Lan")
      || stristr (key_name, "Enable_User")
      || stristr (key_name, "SOL"))
    {
      if ((ret = get_lan_channel_number (state_data,
                                         section_name,
                                         channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
        return (ret);
    }
  else
    {
      if ((ret = get_serial_channel_number (state_data,
                                            section_name,
                                            channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
        return (ret);
    }

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
_get_user_access (ipmi_config_state_data_t *state_data,
                  const char *section_name,
                  const char *key_name,
                  struct user_access *ua,
                  unsigned int *username_not_set_yet)
{
  uint8_t userid;
  uint8_t channel_number;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  int node_busy_retry_count = 0;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (ua);
  assert (!(*username_not_set_yet));

  userid = atoi (section_name + strlen ("User"));

  if ((ret = _channel_info (state_data,
                            section_name,
                            key_name,
                            &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_access_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* 
   * IPMI Workaround (achu)
   *
   * Quanta S99Q/Dell FS12-TY
   *
   * After committing some key user information (most notably User,
   * Password, or Enable_User), BMC seems to do something internally
   * that takes awhile.  Subsequent configuration related calls
   * timeout with IPMI_COMP_CODE_NODE_BUSY.  The loop below is to
   * retry appropriately to limit failures.
   *
   * I will limit this code to the sections the specific IPMI calls
   * that have exhibited problems, rather than all user section calls.
   * The calls that have had problems are:
   *
   * ipmi_cmd_get_user_access (Lan_Enable_IPMI_Msgs)
   *
   * ipmi_cmd_set_user_password (Enable_User)
   */

  while (node_busy_retry_count < NODE_BUSY_RETRY_COUNT)
    {
      if (ipmi_cmd_get_user_access (state_data->ipmi_ctx,
                                    channel_number,
                                    userid,
                                    obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BMC_BUSY
              && (ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_NODE_BUSY) == 1))
            {
              node_busy_retry_count++;

	      if (state_data->prog_data->args->common_args.debug)
		pstdout_fprintf (state_data->pstate,
				 stderr,
				 "ipmi_cmd_get_user_access: %s\n",
				 ipmi_ctx_errormsg (state_data->ipmi_ctx));
	      
              continue;
            }

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
          
          if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
              && (ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
            (*username_not_set_yet) = 1;
          
          if (ipmi_errnum_is_non_fatal (state_data,
                                        obj_cmd_rs,
                                        &ret))
            rv = ret;
          
	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_get_user_access: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  
          goto cleanup;
        }

      break;
    }

  if (node_busy_retry_count >= NODE_BUSY_RETRY_COUNT)
    {
      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "user_ipmi_messaging", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_ipmi_messaging': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ua->user_ipmi_messaging = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_link_authentication", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_link_authentication': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ua->user_link_authentication = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_restricted_to_callback", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_restricted_to_callback': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ua->user_restricted_to_callback = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_privilege_level_limit", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_privilege_level_limit': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ua->privilege_limit = val;

  /* XXX: no way to retrieve */
  ua->session_limit = 0;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_id_enable_status", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_id_enable_status': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ua->user_id_enable_status = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_user_access (ipmi_config_state_data_t *state_data,
                  const char *section_name,
                  const char *key_name,
                  struct user_access *ua,
                  uint8_t *comp_code)
{
  uint8_t userid;
  uint8_t channel_number;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  struct ipmi_config_section *section;
  struct ipmi_config_keyvalue *kvtmp;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (ua);
  /* comp_code not necessary for all functions */

  userid = atoi (section_name + strlen ("User"));

  if ((ret = _channel_info (state_data,
                            section_name,
                            key_name,
                            &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_user_access_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* achu: special case, because the session limit cannot be
   * retrieved.  So if we're committing, we have to get the session
   * limit value and commit it each time.
   */
  if ((section = ipmi_config_find_section (state_data, section_name)))
    {
      char keynametmp[IPMI_CONFIG_MAX_KEY_NAME_LEN + 1];
      int channel_flag = 0;
      int lan_flag = 0;

      memset (keynametmp, '\0', IPMI_CONFIG_MAX_KEY_NAME_LEN + 1);
              
      if (stristr (key_name, "Channel_"))
        channel_flag++;

      if (stristr (key_name, "Lan"))
        lan_flag++;

      if (channel_flag)
        snprintf (keynametmp,
                  IPMI_CONFIG_MAX_KEY_NAME_LEN,
                  "%s_Session_Limit_Channel_%u",
                  (lan_flag) ? "Lan" : "Serial",
                  channel_number);
      else
        snprintf (keynametmp,
                  IPMI_CONFIG_MAX_KEY_NAME_LEN,
                  "%s_Session_Limit",
                  (lan_flag) ? "Lan" : "Serial");
      
      if ((kvtmp = ipmi_config_find_keyvalue (section, keynametmp)))
        ua->session_limit = atoi (kvtmp->value_input);
    }
  else
    {
      /* This is a fatal error, we're already in this section,
       * it should be findable
       */
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Cannot find section '%s'\n",
		       section_name);
      
      goto cleanup;
    }

  if (ipmi_cmd_set_user_access (state_data->ipmi_ctx,
                                channel_number,
                                ua->user_ipmi_messaging,
                                ua->user_link_authentication,
                                ua->user_restricted_to_callback,
                                IPMI_CHANGE_BITS_YES,
                                userid,
                                ua->privilege_limit,
                                ua->session_limit,
                                obj_cmd_rs) < 0)
    {
      /* IPMI Workaround
       *
       * Supermicro X10DDW-i
       *
       * The motherboard contains an illegal starting privilege limit,
       * leading to an input error on this function.  Grab it and try
       * again.
       */

      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_PARAMETERS)
	{
	  if ((kvtmp = ipmi_config_find_keyvalue (section, "Lan_Privilege_Limit")))
	    ua->privilege_limit = get_privilege_limit_number (kvtmp->value_input);
	  
	  if (!(ipmi_cmd_set_user_access (state_data->ipmi_ctx,
					  channel_number,
					  ua->user_ipmi_messaging,
					  ua->user_link_authentication,
					  ua->user_restricted_to_callback,
					  IPMI_CHANGE_BITS_YES,
					  userid,
					  ua->privilege_limit,
					  ua->session_limit,
					  obj_cmd_rs) < 0))
	    goto out;
	}

      if (comp_code)
        {
          (*comp_code) = 0;
          if (FIID_OBJ_GET (obj_cmd_rs, "comp_code", &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: 'comp_code': %s\n",
                               fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }
          (*comp_code) = val;
        }

      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_user_access: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

 out:
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
username_checkout (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  uint8_t userid;
  /* achu: *2 b/c of IPMI_CONFIG_USERNAME_NOT_SET_YET_STR length */
  char username[IPMI_MAX_USER_NAME_LENGTH*2+1];

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  userid = atoi (section_name + strlen ("User"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_name_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* achu: *2 b/c of IPMI_CONFIG_USERNAME_NOT_SET_YET_STR */
  memset (username, '\0', IPMI_MAX_USER_NAME_LENGTH*2+1);

  if (ipmi_cmd_get_user_name (state_data->ipmi_ctx,
                              userid,
                              obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      /*
       * IPMI Workaround (achu)
       *
       * Discovered on Sun X4140, Inventec 5441/Dell Xanadu II,
       * Inventec 5442/Dell Xanadu III, Intel S5500WBV/Penguin Relion
       * 700
       *
       * Get Username and Get User Payload commands fail with CCh =
       * "Invalid data field in request" if a username was not set
       * previously.
       */

      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
        {
          strcpy (username, IPMI_CONFIG_USERNAME_NOT_SET_YET_STR);
	  
	  if (state_data->prog_data->args->common_args.debug)
	    pstdout_fprintf (state_data->pstate,
			     stderr,
			     "ipmi_cmd_get_user_name: %s\n",
			     ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto got_data;
        }

      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_user_name: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  /* achu: check user_id == 1 after ipmi call to ensure the command can succeed */
  if (userid == 1)
    strcpy (username, "NULL");
  else
    {
      if (fiid_obj_get_data (obj_cmd_rs,
                             "user_name",
                             username,
                             IPMI_MAX_USER_NAME_LENGTH) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'user_name': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
    }

 got_data:

  /* for backwards compatability with older ipmi-configs */
  if (state_data->prog_data->args->action == IPMI_CONFIG_ACTION_DIFF
      && userid == 1
      && same (kv->value_input, "anonymous"))
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
                                                      kv,
                                                      "anonymous") < 0)
        return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }
  else
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
                                                      kv,
                                                      username) < 0)
        return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
username_commit (ipmi_config_state_data_t *state_data,
		 const char *section_name,
                 const struct ipmi_config_keyvalue *kv)
{
  uint8_t userid;
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_get_user_name_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (kv);

  userid = atoi (section_name + strlen ("User"));

  /* can't change userid 1 */
  if (userid == 1)
    {
      /* anonymous for backwards compatability */
      if (same (kv->value_input, "NULL")
          || same (kv->value_input, "anonymous"))
        return (IPMI_CONFIG_ERR_SUCCESS);
      else
        return (IPMI_CONFIG_ERR_NON_FATAL_ERROR_READ_ONLY);
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_user_name_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_user_name (state_data->ipmi_ctx,
                              userid,
                              kv->value_input,
                              strlen (kv->value_input),
                              obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      /*
       * IPMI Workaround (achu)
       *
       * Discovered on Intel S5500WBV/Penguin Relion 700
       *
       * Set username command does not allow duplicate usernames to be
       * committed.
       *
       * If configured username identical to inputted one, don't
       * output error.
       */
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
        {
          char username[IPMI_MAX_USER_NAME_LENGTH+1];

          memset (username, '\0', IPMI_MAX_USER_NAME_LENGTH+1);

          if (!(obj_get_user_name_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_name_rs)))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_create: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (ipmi_cmd_get_user_name (state_data->ipmi_ctx,
                                      userid,
                                      obj_get_user_name_cmd_rs) < 0)
            {
	      if (state_data->prog_data->args->common_args.debug)
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "ipmi_cmd_get_user_name: %s\n",
                                 ipmi_ctx_errormsg (state_data->ipmi_ctx));
              goto error_out;
            }

          if (fiid_obj_get_data (obj_get_user_name_cmd_rs,
                                 "user_name",
                                 username,
                                 IPMI_MAX_USER_NAME_LENGTH) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get_data: 'user_name': %s\n",
                               fiid_obj_errormsg (obj_get_user_name_cmd_rs));
              goto cleanup;
            }
          
          if (!strcmp(username, kv->value_input))
            goto out;
        }

    error_out:
      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_user_name: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

 out:
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  fiid_obj_destroy (obj_get_user_name_cmd_rs);
  return (rv);
}

static ipmi_config_validate_t
username_validate (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   const char *key_name,
                   const char *value)
{
  uint8_t userid;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  userid = atoi (section_name + strlen ("User"));

  if (userid == 1)
    {
      if (!value || same (value, "null") || same (value, "anonymous"))
        return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
      else
        return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
    }

  if (!value || strlen (value) > IPMI_MAX_USER_NAME_LENGTH)
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
}

static ipmi_config_err_t
_check_bmc_user_password (ipmi_config_state_data_t *state_data,
                          uint8_t userid,
                          char *password,
                          uint8_t password_size,
                          int *is_same)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (password);
  assert (password_size == IPMI_PASSWORD_SIZE_16_BYTES
          || password_size == IPMI_PASSWORD_SIZE_20_BYTES);
  assert (is_same);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_user_password_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                  userid,
                                  password_size,
                                  IPMI_PASSWORD_OPERATION_TEST_PASSWORD,
                                  password,
                                  strlen (password),
                                  obj_cmd_rs) < 0)
    {
      uint8_t comp_code;
      uint64_t val;

      if (FIID_OBJ_GET (obj_cmd_rs, "comp_code", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'comp_code': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      comp_code = val;

      if (comp_code == IPMI_COMP_CODE_SET_USER_PASSWORD_COMMAND_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT
          || comp_code == IPMI_COMP_CODE_SET_USER_PASSWORD_COMMAND_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT)
        {
          *is_same = 0;
          goto done;
        }
      else
        {
          ipmi_config_err_t ret;

          if (ipmi_errnum_is_non_fatal (state_data,
                                        obj_cmd_rs,
                                        &ret))
            rv = ret;

	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_set_user_password: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));
        }
      goto cleanup;
    }
  else
    *is_same = 1;

 done:
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
password_checkout (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   struct ipmi_config_keyvalue *kv)
{
  char *str = "";

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if (state_data->prog_data->args->action == IPMI_CONFIG_ACTION_DIFF)
    {
      uint8_t userid;
      int is_same;
      ipmi_config_err_t ret;

      userid = atoi (section_name + strlen ("User"));

      /* special case for diff, since we can't get the password, and
       * return it, we'll check to see if the password is the same.
       * If it is, return the inputted password back for proper
       * diffing.
       */

      /* Password length validated before the diff */
      if (strlen (kv->value_input) > IPMI_1_5_MAX_PASSWORD_LENGTH)
        {
          if ((ret = _check_bmc_user_password (state_data,
                                               userid,
                                               kv->value_input,
                                               IPMI_PASSWORD_SIZE_20_BYTES,
                                               &is_same)) != IPMI_CONFIG_ERR_SUCCESS)
            return (ret);
        }
      else
        {
          if ((ret = _check_bmc_user_password (state_data,
                                               userid,
                                               kv->value_input,
                                               IPMI_PASSWORD_SIZE_16_BYTES,
                                               &is_same)) != IPMI_CONFIG_ERR_SUCCESS)
            return (ret);
        }

      if (is_same)
        str = kv->value_input;
      else
        str = "<something else>";
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
password_commit (ipmi_config_state_data_t *state_data,
		 const char *section_name,
                 const struct ipmi_config_keyvalue *kv)
{
  uint8_t userid;
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  userid = atoi (section_name + strlen ("User"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_user_password_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* Password length validated before the commit */
  if (strlen (kv->value_input) > IPMI_1_5_MAX_PASSWORD_LENGTH)
    {
      if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                      userid,
                                      IPMI_PASSWORD_SIZE_20_BYTES,
                                      IPMI_PASSWORD_OPERATION_SET_PASSWORD,
                                      kv->value_input,
                                      strlen (kv->value_input),
                                      obj_cmd_rs) < 0)
        {
          ipmi_config_err_t ret;

          if (ipmi_errnum_is_non_fatal (state_data,
                                        obj_cmd_rs,
                                        &ret))
            rv = ret;

	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_set_user_password: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));

          goto cleanup;
        }
    }
  else
    {
      if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                      userid,
                                      IPMI_PASSWORD_SIZE_16_BYTES,
                                      IPMI_PASSWORD_OPERATION_SET_PASSWORD,
                                      kv->value_input,
                                      strlen (kv->value_input),
                                      obj_cmd_rs) < 0)
        {
          if (ipmi_errnum_is_non_fatal (state_data,
                                        obj_cmd_rs,
                                        &ret))
            rv = ret;

	  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	      || state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_set_user_password: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));

          goto cleanup;
        }
    }

  if (state_data->enable_user_after_password_len
      && state_data->enable_user_after_password[userid-1].enable_user_failed)
    {
      ipmi_config_err_t ret;

      /* ignore non-fatal error, consider success */
      ret = enable_user_commit (state_data,
				section_name,
                                state_data->enable_user_after_password[userid-1].kv);
      if (ret == IPMI_CONFIG_ERR_FATAL_ERROR)
        {
          rv = IPMI_CONFIG_ERR_FATAL_ERROR;
          goto cleanup;
        }
      if (ret == IPMI_CONFIG_ERR_SUCCESS)
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

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_validate_t
password_validate (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   const char *key_name,
                   const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (strlen (value) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
  
  if (strlen (value) > IPMI_1_5_MAX_PASSWORD_LENGTH)
    {
      uint8_t userid;
      ipmi_config_err_t ret;
      int is_same;

      userid = atoi (section_name + strlen ("User"));

      /* if _check_bmc_user_password() fails, that means IPMI 2.0
       * isn't supported, so this password is too long 
       */
      if ((ret = _check_bmc_user_password (state_data,
                                           userid,
                                           "foobar",
                                           IPMI_PASSWORD_SIZE_20_BYTES,
                                           &is_same)) != IPMI_CONFIG_ERR_SUCCESS)
        return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);

      return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
    }

  if (strlen (value) <= IPMI_1_5_MAX_PASSWORD_LENGTH)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);

  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

static ipmi_config_err_t
password20_checkout (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     struct ipmi_config_keyvalue *kv)
{
  uint8_t userid;
  char *str = "";
  ipmi_config_err_t ret;
  int is_same;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  userid = atoi (section_name + strlen ("User"));

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
  if ((ret = _check_bmc_user_password (state_data,
                                       userid,
                                       "foobar",
                                       IPMI_PASSWORD_SIZE_20_BYTES,
                                       &is_same)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (state_data->prog_data->args->action == IPMI_CONFIG_ACTION_DIFF)
    {
      /* special case for diff, since we can't get the password, and
       * return it, we'll check to see if the password is the same.
       * If it is, return the inputted password back for proper
       * diffing.
       */
      if ((ret = _check_bmc_user_password (state_data,
                                           userid,
                                           kv->value_input,
                                           IPMI_PASSWORD_SIZE_20_BYTES,
                                           &is_same)) != IPMI_CONFIG_ERR_SUCCESS)
        return (ret);

      if (is_same)
        str = kv->value_input;
      else
        str = "<something else>";
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
password20_commit (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   const struct ipmi_config_keyvalue *kv)
{
  uint8_t userid;
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (kv);

  userid = atoi (section_name + strlen ("User"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_user_password_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                  userid,
                                  IPMI_PASSWORD_SIZE_20_BYTES,
                                  IPMI_PASSWORD_OPERATION_SET_PASSWORD,
                                  kv->value_input,
                                  strlen (kv->value_input),
                                  obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_user_password: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static ipmi_config_validate_t
password20_validate (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     const char *key_name,
                     const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (strlen (value) <= IPMI_2_0_MAX_PASSWORD_LENGTH)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

static ipmi_config_err_t
enable_user_checkout (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        ua.user_id_enable_status = IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED;
      else
        return (ret);
    }

  /*
   * Older IPMI implementations cannot get the value, but new ones
   * can.  If it cannot be checked out, the line will be commented out
   * later on.
   */
  if (ua.user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_ENABLED)
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
                                                      kv,
                                                      "Yes") < 0)
        return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }
  else if (ua.user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_DISABLED)
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
                                                      kv,
                                                      "No") < 0)
        return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }
  else /* ua.user_id_enable_status == IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED */
    {
      if (ipmi_config_section_update_keyvalue_output (state_data,
                                                      kv,
                                                      "") < 0)
        return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
enable_user_commit (ipmi_config_state_data_t *state_data,
		    const char *section_name,
                    const struct ipmi_config_keyvalue *kv)
{
  uint8_t userid;
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  char password[IPMI_1_5_MAX_PASSWORD_LENGTH];
  uint8_t user_status;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  int node_busy_retry_count = 0;

  assert (state_data);
  assert (section_name);
  assert (kv);

  userid = atoi (section_name + strlen ("User"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_user_password_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (same (kv->value_input, "yes"))
    user_status = IPMI_PASSWORD_OPERATION_ENABLE_USER;
  else
    user_status = IPMI_PASSWORD_OPERATION_DISABLE_USER;

  /* 
   * IPMI Workaround (achu)
   *
   * Quanta S99Q/Dell FS12-TY
   *
   * After committing some key user information (most notably User,
   * Password, or Enable_User), BMC seems to do something internally
   * that takes awhile.  Subsequent configuration related calls
   * timeout with IPMI_COMP_CODE_NODE_BUSY.  The loop below is to
   * retry appropriately to limit failures.
   *
   * I will limit this code to the sections the specific IPMI calls
   * that have exhibited problems, rather than all user section calls.
   * The calls that have had problems are:
   *
   * ipmi_cmd_get_user_access (Lan_Enable_IPMI_Msgs)
   *
   * ipmi_cmd_set_user_password (Enable_User)
   */

  while (node_busy_retry_count < NODE_BUSY_RETRY_COUNT)
    {
      memset (password, 0, IPMI_1_5_MAX_PASSWORD_LENGTH);
      if (ipmi_cmd_set_user_password (state_data->ipmi_ctx,
                                      userid,
                                      IPMI_PASSWORD_SIZE_16_BYTES,
                                      user_status,
                                      password,
                                      0,
                                      obj_cmd_rs) < 0)
        {
	  if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BMC_BUSY
              && (ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_NODE_BUSY) == 1))
            {
              node_busy_retry_count++;

	      if (state_data->prog_data->args->common_args.debug)
		pstdout_fprintf (state_data->pstate,
				 stderr,
				 "ipmi_cmd_set_user_password: %s\n",
				 ipmi_ctx_errormsg (state_data->ipmi_ctx));
	      
              continue;
            }

          /*
           * IPMI Workaround
           *
           * Forgotten/Undocumented Motherboard
           * Sun X4140
	   * Intel Windmill/Quanta Winterfell/Wiwynn Windmill
           *
           * The IPMI spec says you don't have to set a password when you
           * enable/disable a user.  But some BMCs care that you pass in
           * some random password length (even though the password will be
           * ignored)
           */
          
          if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	       && (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID) == 1))
	      || (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
		  && (ipmi_check_completion_code (obj_cmd_rs,
						  IPMI_COMP_CODE_SET_USER_PASSWORD_COMMAND_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT))))
            {
	      if (state_data->prog_data->args->common_args.debug)
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "ipmi_cmd_set_user_password: attempting workaround\n");
              
              if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_user_password_rq)))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "fiid_obj_create: %s\n",
                                   strerror (errno));
                  goto cleanup;
                }
              
              if (fill_cmd_set_user_password (userid,
                                              IPMI_PASSWORD_SIZE_16_BYTES,
                                              user_status,
                                              password,
                                              0,
                                              obj_cmd_rq) < 0)
		{
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "fill_cmd_set_user_password: %s\n",
                                   strerror (errno));

		  goto cleanup;
		}
              
              /* Force the password to be filled in with a length */
              if (fiid_obj_set_data (obj_cmd_rq,
                                     "password",
                                     password,
                                     IPMI_1_5_MAX_PASSWORD_LENGTH) < 0)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "fiid_obj_set_data: 'password': %s\n",
                                   fiid_obj_errormsg (obj_cmd_rq));

                  goto cleanup;
                }
              
              if (ipmi_cmd (state_data->ipmi_ctx,
                            IPMI_BMC_IPMB_LUN_BMC,
                            IPMI_NET_FN_APP_RQ,
                            obj_cmd_rq,
                            obj_cmd_rs) < 0)
                {
                  if (ipmi_errnum_is_non_fatal (state_data,
                                                obj_cmd_rs,
                                                &ret))
                    rv = ret;
                  
		  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
		      || state_data->prog_data->args->common_args.debug)
                    pstdout_fprintf (state_data->pstate,
                                     stderr,
                                     "ipmi_cmd: %s\n",
                                     ipmi_ctx_errormsg (state_data->ipmi_ctx));

                  goto cleanup;
                }

              if (ipmi_check_completion_code_success (obj_cmd_rs) != 1)
                {
                  if (ipmi_errnum_is_non_fatal (state_data,
                                                obj_cmd_rs,
                                                &ret))
                    rv = ret;
                  
		  if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
		      || state_data->prog_data->args->common_args.debug)
                    pstdout_fprintf (state_data->pstate,
                                     stderr,
                                     "ipmi_cmd: %s\n",
                                     ipmi_ctx_errormsg (state_data->ipmi_ctx));

                  goto cleanup;
                }
            }
          /*
           * IPMI Workaround
           *
           * Dell Poweredge R610
           * Dell Poweredge R710
           *
           * Dell says this can only go after a set password command, and
           * you configure a non-null password.  Save info to possibly
           * retry the enable_user after a password is set (if it is done
           * after the Enable_User config.
           */
          else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
                   && (ipmi_check_completion_code (obj_cmd_rs,
                                                   IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1))
            {
              if (state_data->enable_user_after_password_len)
                {
                  state_data->enable_user_after_password[userid-1].enable_user_failed = 1;
                  state_data->enable_user_after_password[userid-1].kv = (struct ipmi_config_keyvalue *)kv;
                }
              
              if (ipmi_errnum_is_non_fatal (state_data,
                                            obj_cmd_rs,
                                            &ret))
                rv = ret;
              
              goto cleanup;
            }
          else
            {
              if (ipmi_errnum_is_non_fatal (state_data,
                                            obj_cmd_rs,
                                            &ret))
                rv = ret;

	      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
		  || state_data->prog_data->args->common_args.debug)
		pstdout_fprintf (state_data->pstate,
				 stderr,
				 "ipmi_cmd_set_user_password: %s\n",
				 ipmi_ctx_errormsg (state_data->ipmi_ctx));

              goto cleanup;
            }
        }

      break;
    }

  if (node_busy_retry_count >= NODE_BUSY_RETRY_COUNT)
    {
      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
lan_enable_ipmi_messaging_checkout (ipmi_config_state_data_t *state_data,
				    const char *section_name,
                                    struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ua.user_ipmi_messaging ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lan_enable_ipmi_messaging_commit (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.user_ipmi_messaging = same (kv->value_input, "yes");

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
lan_enable_link_auth_checkout (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ua.user_link_authentication ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lan_enable_link_auth_commit (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.user_link_authentication = same (kv->value_input, "yes");

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
lan_enable_restricted_to_callback_checkout (ipmi_config_state_data_t *state_data,
					    const char *section_name,
                                            struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ua.user_restricted_to_callback ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lan_enable_restricted_to_callback_commit (ipmi_config_state_data_t *state_data,
					  const char *section_name,
                                          const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.user_restricted_to_callback = same (kv->value_input, "yes");

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
lan_privilege_limit_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  get_privilege_limit_string (ua.privilege_limit)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lan_privilege_limit_commit (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.privilege_limit = get_privilege_limit_number (kv->value_input);

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
lan_session_limit_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  /* Special case: There is no way to check out this value */
  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  "") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
lan_session_limit_commit (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  /* Session limit field will be grabbed/set in _set_user_access */
  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
sol_payload_access_checkout (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             struct ipmi_config_keyvalue *kv)
{
  uint8_t userid;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  userid = atoi (section_name + strlen ("User"));

  if ((ret = _channel_info (state_data,
                            section_name,
                            kv->key->key_name,
                            &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_payload_access_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_user_payload_access (state_data->ipmi_ctx,
                                        channel_number,
                                        userid,
                                        obj_cmd_rs) < 0)
    {
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

      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          goto out;
        }
      
      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_user_payload_access: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  /* standard_payload_1 is the SOL payload type */
  if (FIID_OBJ_GET (obj_cmd_rs, "standard_payload_1", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'standard_payload_1': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  val ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

 out:
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
sol_payload_access_commit (ipmi_config_state_data_t *state_data,
			   const char *section_name,
                           const struct ipmi_config_keyvalue *kv)
{
  uint8_t userid;
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint8_t operation;

  assert (state_data);
  assert (section_name);
  assert (kv);

  userid = atoi (section_name + strlen ("User"));

  if ((ret = _channel_info (state_data,
                            section_name,
                            kv->key->key_name,
                            &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_user_payload_access_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
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
      if (ipmi_errnum_is_non_fatal (state_data,
                                    obj_cmd_rs,
                                    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_user_payload_access: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
serial_enable_ipmi_messaging_checkout (ipmi_config_state_data_t *state_data,
				       const char *section_name,
                                       struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ua.user_ipmi_messaging ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
serial_enable_ipmi_messaging_commit (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.user_ipmi_messaging = same (kv->value_input, "yes");

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
serial_enable_link_auth_checkout (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ua.user_link_authentication ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
serial_enable_link_auth_commit (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.user_link_authentication = same (kv->value_input, "yes");

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
serial_enable_restricted_to_callback_checkout (ipmi_config_state_data_t *state_data,
					       const char *section_name,
                                               struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ua.user_restricted_to_callback ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
serial_enable_restricted_to_callback_commit (ipmi_config_state_data_t *state_data,
					     const char *section_name,
                                             const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.user_restricted_to_callback = same (kv->value_input, "yes");

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
serial_privilege_limit_checkout (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (username_not_set_yet)
        {
          if (ipmi_config_section_update_keyvalue_output (state_data,
                                                          kv,
                                                          IPMI_CONFIG_USERNAME_NOT_SET_YET_STR) < 0)
            return (IPMI_CONFIG_ERR_FATAL_ERROR);
          return (IPMI_CONFIG_ERR_SUCCESS);
        }
      else
        return (ret);
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  get_privilege_limit_string (ua.privilege_limit)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
serial_privilege_limit_commit (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ua.privilege_limit = get_privilege_limit_number (kv->value_input);

  return (_set_user_access (state_data,
                            section_name,
                            kv->key->key_name,
                            &ua,
                            NULL));
}

static ipmi_config_err_t
serial_session_limit_checkout (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               struct ipmi_config_keyvalue *kv)
{
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  /* Special case: There is no way to check out this value */
  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  "") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
serial_session_limit_commit (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const struct ipmi_config_keyvalue *kv)
{
  struct user_access ua;
  unsigned int username_not_set_yet = 0;
  ipmi_config_err_t ret;
  uint8_t comp_code = 0;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* ignore username_not_set_yet return value, if username_not_set_yet
   * conditions arise, we should get an error appriately (b/c the user
   * needs to configure the username first)
   */
  if ((ret = _get_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &username_not_set_yet)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  /* Session limit field will be grabbed/set in _set_user_access */
  /* IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD is special case for
   * this field, see IPMI spec.  "Return CCh 'invalid data field'
   * error completion code if an attempt is made to set this bit, but
   * the option is not supported."
   */
  if ((ret = _set_user_access (state_data,
                               section_name,
                               kv->key->key_name,
                               &ua,
                               &comp_code)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR_INVALID_UNSUPPORTED_CONFIG
          && comp_code == IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST)
        ret = IPMI_CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED;
      return (ret);
    }

  return (IPMI_CONFIG_ERR_SUCCESS);
}

struct ipmi_config_section *
ipmi_config_core_user_section_get (ipmi_config_state_data_t *state_data, unsigned int userid)
{
  struct ipmi_config_section *section = NULL;
  char section_name[IPMI_CONFIG_MAX_SECTION_NAME_LEN];
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
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), a"
    "\"SOL_Payload_Access\" field may be listed below.  "
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
  unsigned int serial_config_flags = 0;
  unsigned int config_flags;
  int index_max;
  int i;

  assert (state_data);
  assert (userid);

  snprintf (section_name, IPMI_CONFIG_MAX_SECTION_NAME_LEN, "User%u", userid);

  if (userid == 1)
    {
      memset (section_comment, '\0', 4096);

      snprintf (section_comment,
                4096,
                section_comment_text,
                IPMI_CONFIG_USERNAME_NOT_SET_YET_STR);

      if (!(section = ipmi_config_section_create (state_data,
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
      if (!(section = ipmi_config_section_create (state_data,
                                                  section_name,
                                                  NULL,
                                                  NULL,
                                                  0,
                                                  NULL,
                                                  NULL)))
        goto cleanup;
    }

  /* serial not checked out by default */

  if (!state_data->prog_data->args->verbose_count)
    serial_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;

  /* userid 1 is the NULL username, so comment it out by default */
  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Username",
                                   "Give Username",
                                   (userid == 1) ? (IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY) : IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                   username_checkout,
                                   username_commit,
                                   username_validate) < 0)
    goto cleanup;

  /* config Password before Enable_User, to remove the off-chance a
   * user is configured "on" before a password is set, allowing a null
   * connection to be established.
   */
  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Password",
                                   "Give password or blank to clear. MAX 16 chars (20 chars if IPMI 2.0 supported).",
                                   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   password_checkout,
                                   password_commit,
                                   password_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_User",
                                   "Possible values: Yes/No or blank to not set",
                                   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                                   enable_user_checkout,
                                   enable_user_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to earlier ipmi-config, now "absorbed" into Password */
  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Password20",
                                   "Give password for IPMI 2.0 or blank to clear. MAX 20 chars.",
                                   IPMI_CONFIG_DO_NOT_CHECKOUT | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   password20_checkout,
                                   password20_commit,
                                   password20_validate) < 0)
    goto cleanup;

  /* b/c loop goes from -1 and up, need to make unsigned int an int
   *
   * even if count is 0, make sure to go through loop atleast once, so
   * fields can fail appropriately for end user on operation
   * (e.g. checkout).
   */

  if (state_data->lan_channel_numbers_count > 1)
    index_max = state_data->lan_channel_numbers_count;
  else
    index_max = 0;

  for (i = -1; i < index_max; i++)
    {
      if (i < 0)
        config_flags = state_data->lan_base_config_flags;
      else
        config_flags = state_data->lan_channel_config_flags;

      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Lan_Enable_IPMI_Msgs",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     lan_enable_ipmi_messaging_checkout,
                                                     lan_enable_ipmi_messaging_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->lan_channel_numbers,
                                                     state_data->lan_channel_numbers_count) < 0)
        goto cleanup;

      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Lan_Enable_Link_Auth",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     lan_enable_link_auth_checkout,
                                                     lan_enable_link_auth_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->lan_channel_numbers,
                                                     state_data->lan_channel_numbers_count) < 0)
        goto cleanup;

      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Lan_Enable_Restricted_to_Callback",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     lan_enable_restricted_to_callback_checkout,
                                                     lan_enable_restricted_to_callback_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->lan_channel_numbers,
                                                     state_data->lan_channel_numbers_count) < 0)
        goto cleanup;
      
      /* achu: For backwards compatability to ipmi-config in 0.2.0 */
      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Lan_Enable_Restrict_to_Callback",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_DO_NOT_CHECKOUT | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     lan_enable_restricted_to_callback_checkout,
                                                     lan_enable_restricted_to_callback_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->lan_channel_numbers,
                                                     state_data->lan_channel_numbers_count) < 0)
        goto cleanup;

      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Lan_Privilege_Limit",
                                                     "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     lan_privilege_limit_checkout,
                                                     lan_privilege_limit_commit,
                                                     get_privilege_limit_number_validate,
                                                     i,
                                                     state_data->lan_channel_numbers,
                                                     state_data->lan_channel_numbers_count) < 0)
        goto cleanup;
      
      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Lan_Session_Limit",
                                                     "Possible values: 0-17, 0 is unlimited; May be reset to 0 if not specified",
                                                     config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     lan_session_limit_checkout,
                                                     lan_session_limit_commit,
                                                     number_range_four_bits_validate,
                                                     i,
                                                     state_data->lan_channel_numbers,
                                                     state_data->lan_channel_numbers_count) < 0)
        goto cleanup;
      
      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "SOL_Payload_Access",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     sol_payload_access_checkout,
                                                     sol_payload_access_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->lan_channel_numbers,
                                                     state_data->lan_channel_numbers_count) < 0)
        goto cleanup;
    }

  /* b/c loop goes from -1 and up, need to make unsigned int an int
   *
   * even if count is 0, make sure to go through loop atleast once, so
   * fields can fail appropriately for end user on operation
   * (e.g. checkout).
   */

  if (state_data->serial_channel_numbers_count > 1)
    index_max = state_data->serial_channel_numbers_count;
  else
    index_max = 0;

  for (i = -1; i < index_max; i++)
    {
      if (i < 0)
        config_flags = serial_config_flags | state_data->serial_base_config_flags;
      else
        config_flags = serial_config_flags | state_data->serial_channel_config_flags;

      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Serial_Enable_IPMI_Msgs",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     serial_enable_ipmi_messaging_checkout,
                                                     serial_enable_ipmi_messaging_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->serial_channel_numbers,
                                                     state_data->serial_channel_numbers_count) < 0)
        goto cleanup;

      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Serial_Enable_Link_Auth",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     serial_enable_link_auth_checkout,
                                                     serial_enable_link_auth_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->serial_channel_numbers,
                                                     state_data->serial_channel_numbers_count) < 0)
        goto cleanup;
      
      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Serial_Enable_Restricted_to_Callback",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     serial_enable_restricted_to_callback_checkout,
                                                     serial_enable_restricted_to_callback_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->serial_channel_numbers,
                                                     state_data->serial_channel_numbers_count) < 0)
        goto cleanup;

      /* achu: For backwards compatability to ipmi-config in 0.2.0 */
      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Serial_Enable_Restrict_to_Callback",
                                                     "Possible values: Yes/No",
                                                     config_flags | IPMI_CONFIG_DO_NOT_CHECKOUT | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     serial_enable_restricted_to_callback_checkout,
                                                     serial_enable_restricted_to_callback_commit,
                                                     yes_no_validate,
                                                     i,
                                                     state_data->serial_channel_numbers,
                                                     state_data->serial_channel_numbers_count) < 0)
        goto cleanup;

      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Serial_Privilege_Limit",
                                                     "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access",
                                                     config_flags | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     serial_privilege_limit_checkout,
                                                     serial_privilege_limit_commit,
                                                     get_privilege_limit_number_validate,
                                                     i,
                                                     state_data->serial_channel_numbers,
                                                     state_data->serial_channel_numbers_count) < 0)
        goto cleanup;
      
      if (ipmi_config_section_multi_channel_add_key (state_data,
                                                     section,
                                                     "Serial_Session_Limit",
                                                     "Possible values: 0-17, 0 is unlimited; May be reset to 0 if not specified",
                                                     config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY | IPMI_CONFIG_USERNAME_NOT_SET_YET,
                                                     serial_session_limit_checkout,
                                                     serial_session_limit_commit,
                                                     number_range_one_byte_validate,
                                                     i,
                                                     state_data->serial_channel_numbers,
                                                     state_data->serial_channel_numbers_count) < 0)
        goto cleanup;
    }

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}
