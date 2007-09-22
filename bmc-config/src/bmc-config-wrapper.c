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

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_access_rs)))
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
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_name_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_name (state_data->dev, 
			      userid, 
			      username, 
			      (username) ? strlen((char *)username) : 0,
			      obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
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
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (!ret)
        {
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (!(obj_cmd_rq = fiid_obj_create(tmpl_cmd_set_user_password_rq)))
	goto cleanup;
      
      if (fill_cmd_set_user_password (userid,
				      (user_status ? IPMI_PASSWORD_OPERATION_ENABLE_USER :
				       IPMI_PASSWORD_OPERATION_DISABLE_USER),
				      (char *)password,
				      0,
				      obj_cmd_rq) < 0)
	goto cleanup;
      
      /* Force the password to be filled in */
      if (fiid_obj_set_data (obj_cmd_rq, 
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
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_user_password (bmc_config_state_data_t *state_data, 
		       uint8_t userid, 
		       uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password (state_data->dev, 
                                  userid, 
                                  IPMI_PASSWORD_OPERATION_SET_PASSWORD, 
                                  (char *)password, 
                                  (password) ? strlen((char *)password) : 0,
                                  obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
set_bmc_user_password20 (bmc_config_state_data_t *state_data, 
                         uint8_t userid, 
                         uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password_v20 (state_data->dev, 
                                      userid, 
                                      IPMI_PASSWORD_SIZE_20_BYTES,
                                      IPMI_PASSWORD_OPERATION_SET_PASSWORD, 
                                      (char *)password, 
                                      (password) ? strlen((char *)password) : 0,
                                      obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_payload_access_rs)))
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
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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

config_err_t
set_pef_control (bmc_config_state_data_t *state_data,
                 uint8_t pef,
                 uint8_t pef_event_messages,
                 uint8_t pef_startup_delay,
                 uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (state_data->dev,
                                                             pef,
                                                             pef_event_messages,
                                                             pef_startup_delay,
                                                             pef_alert_startup_delay,
                                                             obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_pef_action_global_control (bmc_config_state_data_t *state_data,
                               uint8_t alert_action,
                               uint8_t power_down_action,
                               uint8_t reset_action,
                               uint8_t power_cycle_action,
                               uint8_t oem_action,
                               uint8_t diagnostic_interrupt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (state_data->dev,
                                                                           alert_action,
                                                                           power_down_action,
                                                                           reset_action,
                                                                           power_cycle_action,
                                                                           oem_action,
                                                                           diagnostic_interrupt,
                                                                           obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_pef_startup_delay (bmc_config_state_data_t *state_data,
                       uint8_t pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   pef_startup_delay,
                                                                   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_pef_alert_startup_delay (bmc_config_state_data_t *state_data,
                             uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         pef_alert_startup_delay,
                                                                         obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_sol_sol_enable(bmc_config_state_data_t *state_data,
		   uint8_t sol_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_enable (state_data->dev, 
							    channel_number,
							    sol_enable, 
							    obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_sol_sol_authentication(bmc_config_state_data_t *state_data,
			   uint8_t sol_privilege_level,
			   uint8_t force_sol_payload_authentication,
			   uint8_t force_sol_payload_encryption)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_authentication (state_data->dev, 
								    channel_number,
								    sol_privilege_level,
								    force_sol_payload_authentication,
								    force_sol_payload_encryption,
								    obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_sol_character_accumulate_interval_and_send_threshold(bmc_config_state_data_t *state_data,
							 uint8_t character_accumulate_interval,
							 uint8_t character_send_threshold)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (state_data->dev, 
												  channel_number,
												  character_accumulate_interval,
												  character_send_threshold,
												  obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_sol_sol_retry(bmc_config_state_data_t *state_data,
		  uint8_t retry_count,
		  uint8_t retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_retry (state_data->dev, 
							   channel_number,
							   retry_count,
							   retry_interval,
							   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_sol_sol_non_volatile_bit_rate(bmc_config_state_data_t *state_data,
				  uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (state_data->dev, 
									   channel_number,
									   bit_rate,
									   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_sol_sol_volatile_bit_rate(bmc_config_state_data_t *state_data,
			      uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (state_data->dev, 
								       channel_number,
								       bit_rate,
								       obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_sol_sol_payload_port_number(bmc_config_state_data_t *state_data,
				uint16_t port_number)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;  
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_payload_port_number (state_data->dev, 
									 channel_number,
                                                                         port_number,
									 obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_bmc_max_users (bmc_config_state_data_t *state_data, uint8_t *max_users)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_access_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_user_access (state_data->dev, 
				channel_number, 
				1, 
				obj_cmd_rs) < 0) 
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "max_channel_user_ids", &val) < 0) 
    goto cleanup;

  *max_users = (uint8_t) val;
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return rv;
}
			      
/* get_XXXX functions */
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

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_access_rs)))
    goto cleanup;

  if (ipmi_cmd_get_user_access (state_data->dev, 
				channel_number, 
				userid, 
				obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "user_privilege_level_limit", &val) < 0)
    goto cleanup;
  *privilege_limit = (uint8_t) val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_ipmi_messaging", &val) < 0)
    goto cleanup;
  *user_ipmi_messaging = (uint8_t) val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_link_authentication", &val) < 0)
    goto cleanup;
  *user_link_authentication = (uint8_t) val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_restricted_to_callback", &val) < 0)
    goto cleanup;
  *user_restricted_to_callback = (uint8_t) val;
  
  /* XXX: Need to fix */
  *session_limit = 0;
 
  if (fiid_obj_get (obj_cmd_rs, "user_id_enable_status", &val) < 0)
    goto cleanup;
  *user_id_enable_status = (uint8_t) val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_name_rs)))
    goto cleanup;

  if (ipmi_cmd_get_user_name (state_data->dev, 
			      userid, 
			      obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  /* achu: after get_user_name call to ensure the command can succeed */
  if (userid == 1)
    strcpy ((char *)username, "NULL");
  else
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
			     "user_name", 
			     username,
			     username_len) < 0)
	goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_user_payload_access_rs)))
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
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (standard_payload_1)
    {
      if (fiid_obj_get (obj_cmd_rs, "standard_payload_1", &val) < 0)
        goto cleanup;
      *standard_payload_1 = val;
    }
  
  if (standard_payload_2)
    {
      if (fiid_obj_get (obj_cmd_rs, "standard_payload_2", &val) < 0)
        goto cleanup;
      *standard_payload_2 = val;
    }

  if (standard_payload_3)
    {
      if (fiid_obj_get (obj_cmd_rs, "standard_payload_3", &val) < 0)
        goto cleanup;
      *standard_payload_3 = val;
    }

  if (standard_payload_4)
    {
      if (fiid_obj_get (obj_cmd_rs, "standard_payload_4", &val) < 0)
        goto cleanup;
      *standard_payload_4 = val;
    }

  if (standard_payload_5)
    {
      if (fiid_obj_get (obj_cmd_rs, "standard_payload_5", &val) < 0)
        goto cleanup;
      *standard_payload_5 = val;
    }

  if (standard_payload_6)
    {
      if (fiid_obj_get (obj_cmd_rs, "standard_payload_6", &val) < 0)
        goto cleanup;
      *standard_payload_6 = val;
    }

  if (standard_payload_7)
    {
      if (fiid_obj_get (obj_cmd_rs, "standard_payload_7", &val) < 0)
        goto cleanup;
      *standard_payload_7 = val;
    }

  if (oem_payload_0)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_0", &val) < 0)
        goto cleanup;
      *oem_payload_0 = val;
    }

  if (oem_payload_1)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_1", &val) < 0)
        goto cleanup;
      *oem_payload_1 = val;
    }

  if (oem_payload_2)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_2", &val) < 0)
        goto cleanup;
      *oem_payload_2 = val;
    }

  if (oem_payload_3)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_3", &val) < 0)
        goto cleanup;
      *oem_payload_3 = val;
    }

  if (oem_payload_4)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_4", &val) < 0)
        goto cleanup;
      *oem_payload_4 = val;
    }

  if (oem_payload_5)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_5", &val) < 0)
        goto cleanup;
      *oem_payload_5 = val;
    }

  if (oem_payload_6)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_6", &val) < 0)
        goto cleanup;
      *oem_payload_6 = val;
    }

  if (oem_payload_7)
    {
      if (fiid_obj_get (obj_cmd_rs, "oem_payload_7", &val) < 0)
        goto cleanup;
      *oem_payload_7 = val;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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
get_pef_control (bmc_config_state_data_t *state_data,
                 uint8_t *pef,
                 uint8_t *pef_event_messages,
                 uint8_t *pef_startup_delay,
                 uint8_t *pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (state_data->dev,
                                                             IPMI_GET_PEF_PARAMETER,
                                                             SET_SELECTOR,
                                                             BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef", &val) < 0)
    goto cleanup;
  *pef = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_event_messages", &val) < 0)
    goto cleanup;
  *pef_event_messages = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_pef_action_global_control (bmc_config_state_data_t *state_data,
                               uint8_t *alert_action,
                               uint8_t *power_down_action,
                               uint8_t *reset_action,
                               uint8_t *power_cycle_action,
                               uint8_t *oem_action,
                               uint8_t *diagnostic_interrupt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (state_data->dev,
                                                                           IPMI_GET_PEF_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "alert_action", &val) < 0)
    goto cleanup;
  *alert_action = val;

  if (fiid_obj_get (obj_cmd_rs, "power_down_action", &val) < 0)
    goto cleanup;
  *power_down_action = val;

  if (fiid_obj_get (obj_cmd_rs, "reset_action", &val) < 0)
    goto cleanup;
  *reset_action = val;

  if (fiid_obj_get (obj_cmd_rs, "power_cycle_action", &val) < 0)
    goto cleanup;
  *power_cycle_action = val;

  if (fiid_obj_get (obj_cmd_rs, "oem_action", &val) < 0)
    goto cleanup;
  *oem_action = val;

  if (fiid_obj_get (obj_cmd_rs, "diagnostic_interrupt", &val) < 0)
    goto cleanup;
  *diagnostic_interrupt = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_pef_startup_delay (bmc_config_state_data_t *state_data,
                       uint8_t *pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   SET_SELECTOR,
                                                                   BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_pef_alert_startup_delay (bmc_config_state_data_t *state_data,
                             uint8_t *pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_sol_sol_enable (bmc_config_state_data_t *state_data, 
		    uint8_t *sol_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_enable_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_enable (state_data->dev, 
							    channel_number, 
							    IPMI_GET_SOL_PARAMETER, 
							    SET_SELECTOR, 
							    BLOCK_SELECTOR, 
							    obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "sol_enable", &val) < 0)
    goto cleanup;
  *sol_enable = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_sol_sol_authentication (bmc_config_state_data_t *state_data, 
			    uint8_t *sol_privilege_level,
			    uint8_t *force_sol_payload_authentication,
			    uint8_t *force_sol_payload_encryption)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_authentication_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_authentication (state_data->dev, 
								    channel_number, 
								    IPMI_GET_SOL_PARAMETER, 
								    SET_SELECTOR, 
								    BLOCK_SELECTOR, 
								    obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "sol_privilege_level", &val) < 0)
    goto cleanup;
  *sol_privilege_level = val;

  if (fiid_obj_get (obj_cmd_rs, "force_sol_payload_authentication", &val) < 0)
    goto cleanup;
  *force_sol_payload_authentication = val;

  if (fiid_obj_get (obj_cmd_rs, "force_sol_payload_encryption", &val) < 0)
    goto cleanup;
  *force_sol_payload_encryption = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_sol_character_accumulate_interval_and_send_threshold (bmc_config_state_data_t *state_data, 
							  uint8_t *character_accumulate_interval,
							  uint8_t *character_send_threshold)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (state_data->dev, 
												  channel_number, 
												  IPMI_GET_SOL_PARAMETER, 
												  SET_SELECTOR, 
												  BLOCK_SELECTOR, 
												  obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "character_accumulate_interval", &val) < 0)
    goto cleanup;
  *character_accumulate_interval = val;

  if (fiid_obj_get (obj_cmd_rs, "character_send_threshold", &val) < 0)
    goto cleanup;
  *character_send_threshold = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_sol_sol_retry (bmc_config_state_data_t *state_data, 
		   uint8_t *retry_count,
		   uint8_t *retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_retry_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_retry (state_data->dev, 
							   channel_number, 
							   IPMI_GET_SOL_PARAMETER, 
							   SET_SELECTOR, 
							   BLOCK_SELECTOR, 
							   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "retry_count", &val) < 0)
    goto cleanup;
  *retry_count = val;

  if (fiid_obj_get (obj_cmd_rs, "retry_interval", &val) < 0)
    goto cleanup;
  *retry_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_sol_sol_non_volatile_bit_rate (bmc_config_state_data_t *state_data, 
				   uint8_t *bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate (state_data->dev, 
							   channel_number, 
							   IPMI_GET_SOL_PARAMETER, 
							   SET_SELECTOR, 
							   BLOCK_SELECTOR, 
							   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "bit_rate", &val) < 0)
    goto cleanup;
  *bit_rate = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_sol_sol_volatile_bit_rate (bmc_config_state_data_t *state_data, 
			       uint8_t *bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate (state_data->dev, 
								       channel_number, 
								       IPMI_GET_SOL_PARAMETER, 
								       SET_SELECTOR, 
								       BLOCK_SELECTOR, 
								       obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "bit_rate", &val) < 0)
    goto cleanup;
  *bit_rate = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_sol_sol_payload_port_number (bmc_config_state_data_t *state_data, 
				 uint16_t *port_number)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_payload_port_number_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_port_number (state_data->dev, 
									 channel_number, 
									 IPMI_GET_SOL_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "port_number", &val) < 0)
    goto cleanup;
  *port_number = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

/***********************************************************/
bmc_diff_t
check_bmc_user_password (bmc_config_state_data_t *state_data, 
			 uint8_t userid, 
			 uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password (state_data->dev, 
                                  userid, 
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

bmc_diff_t
check_bmc_user_password20 (bmc_config_state_data_t *state_data, 
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
