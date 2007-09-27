#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>

#include "bmc-config-wrapper.h"
#include "bmc-config-utils.h"

#include "bit-ops.h"
#include "freeipmi-portability.h"

static config_err_t 
set_bmc_user_access (bmc_config_state_data_t *state_data, 
		     uint8_t channel_number, 
		     uint8_t user_ipmi_messaging, 
		     uint8_t user_link_authentication, 
		     uint8_t user_restricted_to_callback, 
		     uint8_t userid, 
		     uint8_t privilege_limit, 
		     uint8_t session_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_access_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_access (state_data->dev, 
				channel_number, 
				user_ipmi_messaging, 
				user_link_authentication, 
				user_restricted_to_callback, 
				userid, 
				privilege_limit, 
				session_limit, 
				obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_user_access: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_username (bmc_config_state_data_t *state_data, 
		  uint8_t userid, 
		  uint8_t *username)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (userid == 1)
    return (0);
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_name_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_name (state_data->dev, 
			      userid, 
			      username, 
			      (username) ? strlen((char *)username) : 0,
			      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_user_name: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_enable_user (bmc_config_state_data_t *state_data, 
		     uint8_t userid, 
		     int user_status)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t password[IPMI_1_5_MAX_PASSWORD_LENGTH];
  int8_t ret, rv = -1;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;
  memset (password, 0, IPMI_1_5_MAX_PASSWORD_LENGTH);

  if (ipmi_cmd_set_user_password (state_data->dev, 
                                  userid, 
                                  (user_status ? IPMI_PASSWORD_OPERATION_ENABLE_USER :
                                   IPMI_PASSWORD_OPERATION_DISABLE_USER), 
                                  (char *)password, 
                                  0,
                                  obj_cmd_rs) < 0)
    {
      /* 
       * Workaround: achu: the IPMI spec says you don't have to set a
       * password when you enable/disable a user.  But some BMCs care that
       * you do (even though the password will be ignored)
       */
      if ((ret = ipmi_check_completion_code (obj_cmd_rs, 
                                             IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID)) < 0)
        {
          if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf(stderr,
                    "ipmi_cmd_set_user_password: %s\n",
                    ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (!ret)
        {
          if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf(stderr,
                    "ipmi_cmd_set_user_password: %s\n",
                    ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (!(obj_cmd_rq = Fiid_obj_create(tmpl_cmd_set_user_password_rq)))
	goto cleanup;
      
      if (fill_cmd_set_user_password (userid,
				      (user_status ? IPMI_PASSWORD_OPERATION_ENABLE_USER :
				       IPMI_PASSWORD_OPERATION_DISABLE_USER),
				      (char *)password,
				      0,
				      obj_cmd_rq) < 0)
	goto cleanup;
      
      /* Force the password to be filled in */
      if (Fiid_obj_set_data (obj_cmd_rq, 
                             "password", 
                             password, 
                             IPMI_1_5_MAX_PASSWORD_LENGTH) < 0)
	goto cleanup;
      
      if (ipmi_cmd (state_data->dev, 
		    IPMI_BMC_IPMB_LUN_BMC,
		    IPMI_NET_FN_APP_RQ,
		    obj_cmd_rq,
		    obj_cmd_rs) < 0)
        {
          if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf(stderr,
                    "ipmi_cmd: %s\n",
                    ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      
      if (ipmi_check_completion_code_success(obj_cmd_rs) != 1)
        {
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rq);
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_user_password (bmc_config_state_data_t *state_data, 
		       uint8_t userid, 
		       uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password (state_data->dev, 
                                  userid, 
                                  IPMI_PASSWORD_OPERATION_SET_PASSWORD, 
                                  (char *)password, 
                                  (password) ? strlen((char *)password) : 0,
                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_user_password: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_user_password20 (bmc_config_state_data_t *state_data, 
                         uint8_t userid, 
                         uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password_v20 (state_data->dev, 
                                      userid, 
                                      IPMI_PASSWORD_SIZE_20_BYTES,
                                      IPMI_PASSWORD_OPERATION_SET_PASSWORD, 
                                      (char *)password, 
                                      (password) ? strlen((char *)password) : 0,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_user_password_v20: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_user_lan_channel_access (bmc_config_state_data_t *state_data, 
				 uint8_t userid, 
				 uint8_t lan_user_ipmi_messaging, 
				 uint8_t lan_user_link_authentication, 
				 uint8_t lan_user_restricted_to_callback, 
				 uint8_t lan_privilege_limit, 
				 uint8_t lan_session_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_lan_channel_number (state_data, 
                                     &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return set_bmc_user_access (state_data, 
			      channel_number,
			      lan_user_ipmi_messaging, 
			      lan_user_link_authentication, 
			      lan_user_restricted_to_callback, 
			      userid, 
			      lan_privilege_limit, 
			      lan_session_limit);
}

config_err_t
set_bmc_user_payload_access (bmc_config_state_data_t *state_data,
                             uint8_t userid,
                             uint8_t operation,
                             uint8_t standard_payload_1,
                             uint8_t standard_payload_2,
                             uint8_t standard_payload_3,
                             uint8_t standard_payload_4,
                             uint8_t standard_payload_5,
                             uint8_t standard_payload_6,
                             uint8_t standard_payload_7,
                             uint8_t oem_payload_0,
                             uint8_t oem_payload_1,
                             uint8_t oem_payload_2,
                             uint8_t oem_payload_3,
                             uint8_t oem_payload_4,
                             uint8_t oem_payload_5,
                             uint8_t oem_payload_6,
                             uint8_t oem_payload_7)
{

  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_payload_access_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_set_user_payload_access (state_data->dev, 
                                        channel_number,
                                        userid, 
                                        operation,
                                        standard_payload_1,
                                        standard_payload_2,
                                        standard_payload_3,
                                        standard_payload_4,
                                        standard_payload_5,
                                        standard_payload_6,
                                        standard_payload_7,
                                        oem_payload_0,
                                        oem_payload_1,
                                        oem_payload_2,
                                        oem_payload_3,
                                        oem_payload_4,
                                        oem_payload_5,
                                        oem_payload_6,
                                        oem_payload_7,
                                        obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_user_payload_access: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_user_serial_channel_access (bmc_config_state_data_t *state_data, 
				    uint8_t userid, 
				    uint8_t serial_user_ipmi_messaging, 
				    uint8_t serial_user_link_authentication, 
				    uint8_t serial_user_restricted_to_callback, 
				    uint8_t serial_privilege_limit, 
				    uint8_t serial_session_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_serial_channel_number (state_data, 
                                        &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return set_bmc_user_access (state_data, 
			      channel_number, 
			      serial_user_ipmi_messaging, 
			      serial_user_link_authentication, 
			      serial_user_restricted_to_callback, 
			      userid, 
			      serial_privilege_limit, 
			      serial_session_limit);
}

static config_err_t 
get_bmc_user_access (bmc_config_state_data_t *state_data, 
		     uint8_t userid, 
		     uint8_t channel_number, 
		     uint8_t *user_ipmi_messaging, 
		     uint8_t *user_link_authentication, 
		     uint8_t *user_restricted_to_callback, 
		     uint8_t *privilege_limit, 
		     uint8_t *session_limit,
                     uint8_t *user_id_enable_status)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_user_access_rs)))
    goto cleanup;

  if (ipmi_cmd_get_user_access (state_data->dev, 
				channel_number, 
				userid, 
				obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_user_access: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "user_privilege_level_limit", &val) < 0)
    goto cleanup;
  *privilege_limit = (uint8_t) val;
  
  if (Fiid_obj_get (obj_cmd_rs, "user_ipmi_messaging", &val) < 0)
    goto cleanup;
  *user_ipmi_messaging = (uint8_t) val;
  
  if (Fiid_obj_get (obj_cmd_rs, "user_link_authentication", &val) < 0)
    goto cleanup;
  *user_link_authentication = (uint8_t) val;
  
  if (Fiid_obj_get (obj_cmd_rs, "user_restricted_to_callback", &val) < 0)
    goto cleanup;
  *user_restricted_to_callback = (uint8_t) val;
  
  /* XXX: Need to fix */
  *session_limit = 0;
 
  if (Fiid_obj_get (obj_cmd_rs, "user_id_enable_status", &val) < 0)
    goto cleanup;
  *user_id_enable_status = (uint8_t) val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_username (bmc_config_state_data_t *state_data, 
		  uint8_t userid, 
		  uint8_t *username,
		  uint32_t username_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_user_name_rs)))
    goto cleanup;

  if (ipmi_cmd_get_user_name (state_data->dev, 
			      userid, 
			      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_user_name: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  /* achu: after get_user_name call to ensure the command can succeed */
  if (userid == 1)
    strcpy ((char *)username, "NULL");
  else
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
			     "user_name", 
			     username,
			     username_len) < 0)
	goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_user_lan_channel_access (bmc_config_state_data_t *state_data, 
				 uint8_t userid, 
				 uint8_t *user_ipmi_messaging, 
				 uint8_t *user_link_authentication, 
				 uint8_t *user_restricted_to_callback, 
				 uint8_t *privilege_limit, 
				 uint8_t *session_limit,
                                 uint8_t *user_id_enable_status)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_lan_channel_number (state_data, 
                                     &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return get_bmc_user_access (state_data, 
			      userid, 
			      channel_number, 
			      user_ipmi_messaging, 
			      user_link_authentication, 
			      user_restricted_to_callback, 
			      privilege_limit, 
			      session_limit,
                              user_id_enable_status);
}

config_err_t
get_bmc_user_payload_access (bmc_config_state_data_t *state_data,
                             uint8_t userid,
                             uint8_t *standard_payload_1,
                             uint8_t *standard_payload_2,
                             uint8_t *standard_payload_3,
                             uint8_t *standard_payload_4,
                             uint8_t *standard_payload_5,
                             uint8_t *standard_payload_6,
                             uint8_t *standard_payload_7,
                             uint8_t *oem_payload_0,
                             uint8_t *oem_payload_1,
                             uint8_t *oem_payload_2,
                             uint8_t *oem_payload_3,
                             uint8_t *oem_payload_4,
                             uint8_t *oem_payload_5,
                             uint8_t *oem_payload_6,
                             uint8_t *oem_payload_7)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_user_payload_access_rs)))
    goto cleanup;
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_user_payload_access (state_data->dev, 
                                        channel_number,
                                        userid, 
                                        obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_user_payload_access: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (standard_payload_1)
    {
      if (Fiid_obj_get (obj_cmd_rs, "standard_payload_1", &val) < 0)
        goto cleanup;
      *standard_payload_1 = val;
    }
  
  if (standard_payload_2)
    {
      if (Fiid_obj_get (obj_cmd_rs, "standard_payload_2", &val) < 0)
        goto cleanup;
      *standard_payload_2 = val;
    }

  if (standard_payload_3)
    {
      if (Fiid_obj_get (obj_cmd_rs, "standard_payload_3", &val) < 0)
        goto cleanup;
      *standard_payload_3 = val;
    }

  if (standard_payload_4)
    {
      if (Fiid_obj_get (obj_cmd_rs, "standard_payload_4", &val) < 0)
        goto cleanup;
      *standard_payload_4 = val;
    }

  if (standard_payload_5)
    {
      if (Fiid_obj_get (obj_cmd_rs, "standard_payload_5", &val) < 0)
        goto cleanup;
      *standard_payload_5 = val;
    }

  if (standard_payload_6)
    {
      if (Fiid_obj_get (obj_cmd_rs, "standard_payload_6", &val) < 0)
        goto cleanup;
      *standard_payload_6 = val;
    }

  if (standard_payload_7)
    {
      if (Fiid_obj_get (obj_cmd_rs, "standard_payload_7", &val) < 0)
        goto cleanup;
      *standard_payload_7 = val;
    }

  if (oem_payload_0)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_0", &val) < 0)
        goto cleanup;
      *oem_payload_0 = val;
    }

  if (oem_payload_1)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_1", &val) < 0)
        goto cleanup;
      *oem_payload_1 = val;
    }

  if (oem_payload_2)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_2", &val) < 0)
        goto cleanup;
      *oem_payload_2 = val;
    }

  if (oem_payload_3)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_3", &val) < 0)
        goto cleanup;
      *oem_payload_3 = val;
    }

  if (oem_payload_4)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_4", &val) < 0)
        goto cleanup;
      *oem_payload_4 = val;
    }

  if (oem_payload_5)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_5", &val) < 0)
        goto cleanup;
      *oem_payload_5 = val;
    }

  if (oem_payload_6)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_6", &val) < 0)
        goto cleanup;
      *oem_payload_6 = val;
    }

  if (oem_payload_7)
    {
      if (Fiid_obj_get (obj_cmd_rs, "oem_payload_7", &val) < 0)
        goto cleanup;
      *oem_payload_7 = val;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_user_serial_channel_access (bmc_config_state_data_t *state_data, 
				    uint8_t userid, 
				    uint8_t *user_ipmi_messaging, 
				    uint8_t *user_link_authentication, 
				    uint8_t *user_restricted_to_callback, 
				    uint8_t *privilege_limit, 
				    uint8_t *session_limit,
                                    uint8_t *user_id_enable_status)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_serial_channel_number (state_data, 
                                        &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return get_bmc_user_access (state_data, 
			      userid, 
			      channel_number, 
			      user_ipmi_messaging, 
			      user_link_authentication, 
			      user_restricted_to_callback, 
			      privilege_limit, 
			      session_limit,
                              user_id_enable_status);
}

config_err_t
check_bmc_user_password (bmc_config_state_data_t *state_data, 
			 uint8_t userid, 
			 uint8_t *password,
                         int *is_same)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password (state_data->dev, 
                                  userid, 
                                  IPMI_PASSWORD_OPERATION_TEST_PASSWORD, 
                                  (char *)password, 
                                  (password) ? strlen((char *)password) : 0,
                                  obj_cmd_rs) < 0)
    {
      uint64_t comp_code;

      if (Fiid_obj_get(obj_cmd_rs, "comp_code", &comp_code) < 0)
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
          if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf(stderr,
                    "ipmi_cmd_set_user_password: %s\n",
                    ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
          rv = CONFIG_ERR_NON_FATAL_ERROR;
        }
      goto cleanup;
    }
  else
    *is_same = 1;

 done:  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
check_bmc_user_password20 (bmc_config_state_data_t *state_data, 
                           uint8_t userid, 
                           uint8_t *password,
                           int *is_same)
                         
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_user_password_rs)))
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

      if (Fiid_obj_get(obj_cmd_rs, "comp_code", &comp_code) < 0)
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
          if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf(stderr,
                    "ipmi_cmd_set_user_password_v20: %s\n",
                    ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
          rv = CONFIG_ERR_NON_FATAL_ERROR;
        }
      goto cleanup;
    }
  else
    *is_same = 1;

 done:
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}
