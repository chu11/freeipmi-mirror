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

static config_err_t 
set_bmc_channel_access (bmc_config_state_data_t *state_data, 
			uint8_t channel_number, 
			uint8_t set_option, 
			uint8_t access_mode, 
			uint8_t user_level_authentication, 
			uint8_t per_message_authentication, 
			uint8_t pef_alerting, 
			uint8_t channel_privilege_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_access_rs)))
    goto cleanup;

  if (ipmi_cmd_set_channel_access (state_data->dev, 
				   channel_number, 
				   access_mode, 
				   user_level_authentication, 
				   per_message_authentication, 
				   pef_alerting, 
				   (set_option ? IPMI_CHANNEL_ACCESS_SET_VOLATILE : 
				    IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE), 
				   channel_privilege_limit, 
				   (set_option ? IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VOLATILE : 
				    IPMI_PRIVILEGE_LEVEL_LIMIT_SET_NON_VOLATILE), 
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
set_bmc_lan_channel_volatile_access (bmc_config_state_data_t *state_data, 
				     uint8_t access_mode, 
				     uint8_t user_level_authentication, 
				     uint8_t per_message_authentication, 
				     uint8_t pef_alerting, 
				     uint8_t channel_privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_lan_channel_number (state_data, 
                                     &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return set_bmc_channel_access (state_data, 
				 channel_number, 
				 1, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

config_err_t 
set_bmc_lan_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
					 uint8_t access_mode, 
					 uint8_t user_level_authentication, 
					 uint8_t per_message_authentication, 
					 uint8_t pef_alerting, 
					 uint8_t channel_privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_lan_channel_number (state_data, 
                                     &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return set_bmc_channel_access (state_data, 
				 channel_number, 
				 0, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

config_err_t 
set_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
				    uint8_t ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address_source (state_data->dev, 
								   channel_number, 
								   ip_address_source, 
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
set_bmc_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
			     char *ip_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(ip_address, &ip_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address (state_data->dev, 
							    channel_number, 
							    ip_address_val, 
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
set_bmc_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
			      char *mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (ipmi_mac_address_string2int(mac_address, &mac_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_mac_address (state_data->dev, 
							     channel_number, 
							     mac_address_val, 
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
set_bmc_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
			      char *subnet_mask)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t subnet_mask_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(subnet_mask, &subnet_mask_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_subnet_mask (state_data->dev, 
							     channel_number, 
							     subnet_mask_val, 
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
set_bmc_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
					  char *default_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(default_gateway_address, &ip_address_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_address (state_data->dev, 
									 channel_number, 
									 ip_address_val, 
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
set_bmc_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
					      char *default_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_mac_address_string2int(default_gateway_mac_address, &mac_address_val) < 0)
    goto cleanup;
 
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address (state_data->dev, 
									     channel_number, 
									     mac_address_val, 
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
set_bmc_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
					 char *backup_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(backup_gateway_address, &ip_address_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address (state_data->dev, 
									channel_number, 
									ip_address_val, 
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
set_bmc_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
					     char *backup_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_mac_address_string2int(backup_gateway_mac_address, &mac_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (state_data->dev, 
									    channel_number, 
									    mac_address_val, 
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
set_bmc_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
			  uint32_t vlan_id,
			  uint8_t vlan_id_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_id (state_data->dev, 
							 channel_number, 
                                                         vlan_id,
							 vlan_id_enable, 
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
set_bmc_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
				uint8_t vlan_priority)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_priority (state_data->dev, 
							       channel_number, 
							       vlan_priority, 
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
set_bmc_lan_conf_authentication_type_enables (bmc_config_state_data_t *state_data, 
                                              uint8_t callback_level_none,
                                              uint8_t callback_level_md2,
                                              uint8_t callback_level_md5,
                                              uint8_t callback_level_straight_password,
                                              uint8_t callback_level_oem_proprietary,
                                              uint8_t user_level_none,
                                              uint8_t user_level_md2,
                                              uint8_t user_level_md5,
                                              uint8_t user_level_straight_password,
                                              uint8_t user_level_oem_proprietary,
                                              uint8_t operator_level_none,
                                              uint8_t operator_level_md2,
                                              uint8_t operator_level_md5,
                                              uint8_t operator_level_straight_password,
                                              uint8_t operator_level_oem_proprietary,
                                              uint8_t admin_level_none,
                                              uint8_t admin_level_md2,
                                              uint8_t admin_level_md5,
                                              uint8_t admin_level_straight_password,
                                              uint8_t admin_level_oem_proprietary,
                                              uint8_t oem_level_none,
                                              uint8_t oem_level_md2,
                                              uint8_t oem_level_md5,
                                              uint8_t oem_level_straight_password,
                                              uint8_t oem_level_oem_proprietary)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables (state_data->dev,
                                                                             channel_number, 
                                                                             callback_level_none,
                                                                             callback_level_md2,
                                                                             callback_level_md5,
                                                                             callback_level_straight_password,
                                                                             callback_level_oem_proprietary,
                                                                             user_level_none,
                                                                             user_level_md2,
                                                                             user_level_md5,
                                                                             user_level_straight_password,
                                                                             user_level_oem_proprietary,
                                                                             operator_level_none,
                                                                             operator_level_md2,
                                                                             operator_level_md5,
                                                                             operator_level_straight_password,
                                                                             operator_level_oem_proprietary,
                                                                             admin_level_none,
                                                                             admin_level_md2,
                                                                             admin_level_md5,
                                                                             admin_level_straight_password,
                                                                             admin_level_oem_proprietary,
                                                                             oem_level_none,
                                                                             oem_level_md2,
                                                                             oem_level_md5,
                                                                             oem_level_straight_password,
                                                                             oem_level_oem_proprietary,
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
set_bmc_lan_conf_bmc_generated_arp_control (bmc_config_state_data_t *state_data, 
					    uint8_t bmc_generated_gratuitous_arps, 
					    uint8_t bmc_generated_arp_responses)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (state_data->dev, 
									   channel_number, 
									   bmc_generated_gratuitous_arps, 
									   bmc_generated_arp_responses, 
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
set_bmc_lan_conf_gratuitous_arp_interval (bmc_config_state_data_t *state_data, 
					  uint8_t gratuitous_arp_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval (state_data->dev, 
									 channel_number, 
									 gratuitous_arp_interval, 
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
set_bmc_serial_channel_volatile_access (bmc_config_state_data_t *state_data, 
					uint8_t access_mode, 
					uint8_t user_level_authentication, 
					uint8_t per_message_authentication, 
					uint8_t pef_alerting, 
					uint8_t channel_privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_serial_channel_number (state_data, 
                                        &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return set_bmc_channel_access (state_data, 
				 channel_number, 
				 1, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

config_err_t 
set_bmc_serial_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
					    uint8_t access_mode, 
					    uint8_t user_level_authentication, 
					    uint8_t per_message_authentication, 
					    uint8_t pef_alerting, 
					    uint8_t channel_privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_serial_channel_number (state_data, 
                                        &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return set_bmc_channel_access (state_data, 
				 channel_number, 
				 0, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

config_err_t 
set_bmc_serial_conf_connection_mode (bmc_config_state_data_t *state_data, 
				     uint8_t basic_mode,
				     uint8_t ppp_mode,
				     uint8_t terminal_mode,
				     uint8_t connect_mode)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_connection_mode (state_data->dev, 
							       channel_number, 
							       basic_mode,
							       ppp_mode,
							       terminal_mode,
							       connect_mode,
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
set_bmc_serial_conf_page_blackout_interval (bmc_config_state_data_t *state_data, 
					    uint8_t page_blackout_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_page_blackout_interval (state_data->dev, 
								      channel_number, 
								      page_blackout_interval, 
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
set_bmc_serial_conf_call_retry_interval (bmc_config_state_data_t *state_data, 
					 uint8_t call_retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_call_retry_interval (state_data->dev, 
								   channel_number, 
								   call_retry_interval, 
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
set_bmc_serial_conf_ipmi_messaging_comm_settings (bmc_config_state_data_t *state_data, 
						  uint8_t dtr_hangup, 
						  uint8_t flow_control, 
						  uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (state_data->dev, 
									    channel_number, 
									    dtr_hangup, 
									    flow_control, 
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
set_bmc_power_restore_policy (bmc_config_state_data_t *state_data, 
			      uint8_t power_restore_policy)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_power_restore_policy_rs)))
    goto cleanup;

  if (ipmi_cmd_set_power_restore_policy (state_data->dev, 
					 power_restore_policy, 
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

static config_err_t
_rmcpplus_cipher_suite_id_privilege_setup(bmc_config_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_count_rs = NULL;
  fiid_obj_t obj_cmd_id_rs = NULL;
  fiid_obj_t obj_cmd_priv_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  int i;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!state_data->cipher_suite_entry_count)
    {
      if (!(obj_cmd_count_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs)))
	goto cleanup;

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support (state_data->dev, 
												   channel_number, 
												   IPMI_GET_LAN_PARAMETER, 
												   SET_SELECTOR, 
												   BLOCK_SELECTOR, 
												   obj_cmd_count_rs) < 0)
        {
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (fiid_obj_get (obj_cmd_count_rs, "cipher_suite_entry_count", &val) < 0)
	goto cleanup;

      state_data->cipher_suite_entry_count = val;

      if (state_data->cipher_suite_entry_count > CIPHER_SUITE_LEN)
	state_data->cipher_suite_entry_count = CIPHER_SUITE_LEN;
    }

  if (!state_data->cipher_suite_id_supported_set)
    {
      if (!(obj_cmd_id_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs)))
	goto cleanup;

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries (state_data->dev, 
											     channel_number, 
											     IPMI_GET_LAN_PARAMETER, 
											     SET_SELECTOR, 
											     BLOCK_SELECTOR, 
											     obj_cmd_id_rs) < 0)
        {
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      for (i = 0; i < state_data->cipher_suite_entry_count; i++)
	{
	  char *field = NULL;

	  if (i == 0)
	    field = "cipher_suite_id_entry_A";
	  else if (i == 1)
	    field = "cipher_suite_id_entry_B";
	  else if (i == 2)
	    field = "cipher_suite_id_entry_C";
	  else if (i == 3)
	    field = "cipher_suite_id_entry_D";
	  else if (i == 4)
	    field = "cipher_suite_id_entry_E";
	  else if (i == 5)
	    field = "cipher_suite_id_entry_F";
	  else if (i == 6)
	    field = "cipher_suite_id_entry_G";
	  else if (i == 7)
	    field = "cipher_suite_id_entry_H";
	  else if (i == 8)
	    field = "cipher_suite_id_entry_I";
	  else if (i == 9)
	    field = "cipher_suite_id_entry_J";
	  else if (i == 10)
	    field = "cipher_suite_id_entry_K";
	  else if (i == 11)
	    field = "cipher_suite_id_entry_L";
	  else if (i == 12)
	    field = "cipher_suite_id_entry_M";
	  else if (i == 13)
	    field = "cipher_suite_id_entry_N";
	  else if (i == 14)
	    field = "cipher_suite_id_entry_O";
	  else if (i == 15)
	    field = "cipher_suite_id_entry_P";

	  if (fiid_obj_get (obj_cmd_id_rs, field, &val) < 0)
	    goto cleanup;
	  
	  state_data->cipher_suite_id_supported[i] = val;
	}
      
      state_data->cipher_suite_id_supported_set++;
    }
  
  if (!state_data->cipher_suite_priv_set)
    {
      if (!(obj_cmd_priv_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs)))
	goto cleanup;

      if (ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (state_data->dev, 
												      channel_number, 
												      IPMI_GET_LAN_PARAMETER, 
												      SET_SELECTOR, 
												      BLOCK_SELECTOR, 
												      obj_cmd_priv_rs) < 0)
        {
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      for (i = 0; i < CIPHER_SUITE_LEN; i++)
	{
	  char *field = NULL;

	  if (i == 0)
	    field = "maximum_privilege_for_cipher_suite_1";
	  else if (i == 1)
	    field = "maximum_privilege_for_cipher_suite_2";
	  else if (i == 2)
	    field = "maximum_privilege_for_cipher_suite_3";
	  else if (i == 3)
	    field = "maximum_privilege_for_cipher_suite_4";
	  else if (i == 4)
	    field = "maximum_privilege_for_cipher_suite_5";
	  else if (i == 5)
	    field = "maximum_privilege_for_cipher_suite_6";
	  else if (i == 6)
	    field = "maximum_privilege_for_cipher_suite_7";
	  else if (i == 7)
	    field = "maximum_privilege_for_cipher_suite_8";
	  else if (i == 8)
	    field = "maximum_privilege_for_cipher_suite_9";
	  else if (i == 9)
	    field = "maximum_privilege_for_cipher_suite_10";
	  else if (i == 10)
	    field = "maximum_privilege_for_cipher_suite_11";
	  else if (i == 11)
	    field = "maximum_privilege_for_cipher_suite_12";
	  else if (i == 12)
	    field = "maximum_privilege_for_cipher_suite_13";
	  else if (i == 13)
	    field = "maximum_privilege_for_cipher_suite_14";
	  else if (i == 14)
	    field = "maximum_privilege_for_cipher_suite_15";
	  else if (i == 15)
	    field = "maximum_privilege_for_cipher_suite_16";

	  if (fiid_obj_get (obj_cmd_priv_rs, field, &val) < 0)
	    goto cleanup;
	  
	  state_data->cipher_suite_priv[i] = val;
	}
      
      state_data->cipher_suite_priv_set++;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_count_rs)
    fiid_obj_destroy(obj_cmd_count_rs);
  if (obj_cmd_id_rs)
    fiid_obj_destroy(obj_cmd_id_rs);
  if (obj_cmd_priv_rs)
    fiid_obj_destroy(obj_cmd_priv_rs);
  return (rv);
}

config_err_t
set_rmcpplus_cipher_suite_id_privilege (bmc_config_state_data_t *state_data,
					uint8_t cipher_suite_id,
					uint8_t privilege)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(cipher_suite_id < CIPHER_SUITE_LEN))
    goto cleanup;

  if ((ret = _rmcpplus_cipher_suite_id_privilege_setup(state_data)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (state_data->cipher_suite_entry_count
      && state_data->cipher_suite_id_supported_set
      && state_data->cipher_suite_priv_set)
    {
      uint8_t privs[CIPHER_SUITE_LEN];

      memset(privs, '\0', CIPHER_SUITE_LEN);
      memcpy(privs, state_data->cipher_suite_priv, CIPHER_SUITE_LEN);
      privs[cipher_suite_id] = privilege;

      if (ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels(state_data->dev,
												     channel_number,
												     privs[0],
												     privs[1],
												     privs[2],
												     privs[3],
												     privs[4],
												     privs[5],
												     privs[6],
												     privs[7],
												     privs[8],
												     privs[9],
												     privs[10],
												     privs[11],
												     privs[12],
												     privs[13],
												     privs[14],
												     privs[15],
												     obj_cmd_rs) < 0)
        {
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      
      state_data->cipher_suite_priv[cipher_suite_id] = privilege;
      rv = CONFIG_ERR_SUCCESS;
    }
  else
    rv = CONFIG_ERR_NON_FATAL_ERROR;
  
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_k_r(bmc_config_state_data_t *state_data,
        uint8_t *k_r,
        uint32_t k_r_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!k_r)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_security_keys_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_channel_security_keys (state_data->dev, 
                                          channel_number, 
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY,
                                          IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R,
					  k_r,
					  k_r_len,
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
set_k_g(bmc_config_state_data_t *state_data,
        uint8_t *k_g,
        uint32_t k_g_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_security_keys_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_channel_security_keys (state_data->dev, 
                                          channel_number, 
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY,
                                          IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G,
					  k_g,
					  k_g_len,
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

static config_err_t 
get_bmc_channel_access (bmc_config_state_data_t *state_data, 
			uint8_t channel_number, 
			uint8_t access_type, 
			uint8_t *access_mode, 
			uint8_t *user_level_authentication, 
			uint8_t *per_message_authentication, 
			uint8_t *pef_alerting, 
			uint8_t *privilege_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_channel_access_rs)))
    goto cleanup;

  if (ipmi_cmd_get_channel_access (state_data->dev, 
				   channel_number, 
				   (access_type ? IPMI_CHANNEL_ACCESS_GET_VOLATILE :
				    IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE), 
				   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "ipmi_messaging_access_mode", &val) < 0)
    goto cleanup;
  *access_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level_authentication", &val) < 0)
    goto cleanup;
  *user_level_authentication = (val ? 0 : 1);
  
  if (fiid_obj_get (obj_cmd_rs, "per_message_authentication", &val) < 0)
    goto cleanup;
  *per_message_authentication = (val ? 0 : 1);
  
  if (fiid_obj_get (obj_cmd_rs, "pef_alerting", &val) < 0)
    goto cleanup;
  *pef_alerting = (val ? 0 : 1);
  
  if (fiid_obj_get (obj_cmd_rs, "channel_privilege_level_limit", &val) < 0)
    goto cleanup;
  *privilege_limit = val;
  
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
get_bmc_lan_channel_volatile_access (bmc_config_state_data_t *state_data, 
				     uint8_t *access_mode, 
				     uint8_t *user_level_authentication, 
				     uint8_t *per_message_authentication, 
				     uint8_t *pef_alerting, 
				     uint8_t *privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_lan_channel_number (state_data, 
                                     &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return get_bmc_channel_access (state_data, 
				 channel_number, 
				 1, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

config_err_t 
get_bmc_lan_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
					 uint8_t *access_mode, 
					 uint8_t *user_level_authentication, 
					 uint8_t *per_message_authentication, 
					 uint8_t *pef_alerting, 
					 uint8_t *privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_lan_channel_number (state_data, 
                                     &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return get_bmc_channel_access (state_data, 
				 channel_number, 
				 0, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

config_err_t 
get_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
				    uint8_t *ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address_source (state_data->dev, 
								   channel_number, 
								   IPMI_GET_LAN_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "ip_address_source", &val) < 0)
    goto cleanup;
  *ip_address_source = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
			     char *ip_address,
                             unsigned int ip_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_ip_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address (state_data->dev, 
							    channel_number, 
							    IPMI_GET_LAN_PARAMETER, 
							    SET_SELECTOR, 
							    BLOCK_SELECTOR, 
							    obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (ip_address && ip_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "ip_address", 
                             ip_address_bytes,
                             4) < 0)
        goto cleanup;

      memset(ip_address, '\0', ip_address_len);
      snprintf (ip_address, 
                ip_address_len - 1,
                "%u.%u.%u.%u", 
                ip_address_bytes[0], 
                ip_address_bytes[1], 
                ip_address_bytes[2], 
                ip_address_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
			      char *mac_address,
                              unsigned int mac_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_mac_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (state_data->dev, 
							     channel_number, 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (mac_address && mac_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "mac_address", 
                             mac_address_bytes,
                             6) < 0)
        goto cleanup;

      memset(mac_address, '\0', mac_address_len);
      snprintf (mac_address, 
                mac_address_len - 1,
                "%02X:%02X:%02X:%02X:%02X:%02X", 
                mac_address_bytes[0], 
                mac_address_bytes[1], 
                mac_address_bytes[2], 
                mac_address_bytes[3], 
                mac_address_bytes[4], 
                mac_address_bytes[5]);
    }
  else
    goto cleanup;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
			      char *subnet_mask,
                              unsigned int subnet_mask_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t subnet_mask_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_subnet_mask (state_data->dev, 
							     channel_number, 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (subnet_mask && subnet_mask_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "subnet_mask", 
                             subnet_mask_bytes,
                             4) < 0)
        goto cleanup;

      memset(subnet_mask, '\0', subnet_mask_len);
      snprintf (subnet_mask, 
                subnet_mask_len - 1,
                "%u.%u.%u.%u", 
                subnet_mask_bytes[0], 
                subnet_mask_bytes[1], 
                subnet_mask_bytes[2], 
                subnet_mask_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
					  char *default_gateway_address,
                                          unsigned int default_gateway_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_address (state_data->dev, 
									 channel_number, 
									 IPMI_GET_LAN_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (default_gateway_address && default_gateway_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "ip_address", 
                             ip_address_bytes,
                             4) < 0)
        goto cleanup;

      memset(default_gateway_address, '\0', default_gateway_address_len);
      snprintf (default_gateway_address, 
                default_gateway_address_len - 1,
                "%u.%u.%u.%u", 
                ip_address_bytes[0], 
                ip_address_bytes[1], 
                ip_address_bytes[2], 
                ip_address_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
					      char *default_gateway_mac_address,
                                              unsigned int default_gateway_mac_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address (state_data->dev, 
									     channel_number, 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (default_gateway_mac_address && default_gateway_mac_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "mac_address", 
                             mac_address_bytes,
                             6) < 0)
        goto cleanup;

      memset(default_gateway_mac_address, '\0', default_gateway_mac_address_len);
      snprintf (default_gateway_mac_address, 
                default_gateway_mac_address_len - 1,
                "%02X:%02X:%02X:%02X:%02X:%02X", 
                mac_address_bytes[0], 
                mac_address_bytes[1], 
                mac_address_bytes[2], 
                mac_address_bytes[3], 
                mac_address_bytes[4], 
                mac_address_bytes[5]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
					 char *backup_gateway_address,
                                         unsigned int backup_gateway_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address (state_data->dev, 
									channel_number, 
									IPMI_GET_LAN_PARAMETER, 
									SET_SELECTOR, 
									BLOCK_SELECTOR, 
									obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (backup_gateway_address && backup_gateway_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "ip_address", 
                             ip_address_bytes,
                             4) < 0)
        goto cleanup;

      memset(backup_gateway_address, '\0', backup_gateway_address_len);
      snprintf (backup_gateway_address, 
                backup_gateway_address_len - 1,
                "%u.%u.%u.%u", 
                ip_address_bytes[0], 
                ip_address_bytes[1], 
                ip_address_bytes[2], 
                ip_address_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
					     char *backup_gateway_mac_address,
                                             unsigned int backup_gateway_mac_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address (state_data->dev, 
									    channel_number, 
									    IPMI_GET_LAN_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (backup_gateway_mac_address && backup_gateway_mac_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "mac_address", 
                             mac_address_bytes,
                             6) < 0)
        goto cleanup;

      memset(backup_gateway_mac_address, '\0', backup_gateway_mac_address_len);
      snprintf (backup_gateway_mac_address, 
                backup_gateway_mac_address_len - 1,
                "%02X:%02X:%02X:%02X:%02X:%02X", 
                mac_address_bytes[0], 
                mac_address_bytes[1], 
                mac_address_bytes[2], 
                mac_address_bytes[3], 
                mac_address_bytes[4], 
                mac_address_bytes[5]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
			  uint32_t *vlan_id,
			  uint8_t *vlan_id_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_id (state_data->dev, 
							 channel_number, 
							 IPMI_GET_LAN_PARAMETER, 
							 SET_SELECTOR, 
							 BLOCK_SELECTOR, 
							 obj_cmd_rs) < 0) 
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "vlan_id", &val) < 0)
    goto cleanup;
  *vlan_id = val;

  if (fiid_obj_get (obj_cmd_rs, "vlan_id_enable", &val) < 0)
    goto cleanup;
  *vlan_id_enable = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
				uint8_t *vlan_priority)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_priority (state_data->dev, 
							       channel_number, 
							       IPMI_GET_LAN_PARAMETER, 
							       SET_SELECTOR, 
							       BLOCK_SELECTOR, 
							       obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "vlan_priority", &val) < 0)
    goto cleanup;
  *vlan_priority = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_authentication_type_enables (bmc_config_state_data_t *state_data, 
                                              uint8_t *callback_level_none,
                                              uint8_t *callback_level_md2,
                                              uint8_t *callback_level_md5,
                                              uint8_t *callback_level_straight_password,
                                              uint8_t *callback_level_oem_proprietary,
                                              uint8_t *user_level_none,
                                              uint8_t *user_level_md2,
                                              uint8_t *user_level_md5,
                                              uint8_t *user_level_straight_password,
                                              uint8_t *user_level_oem_proprietary,
                                              uint8_t *operator_level_none,
                                              uint8_t *operator_level_md2,
                                              uint8_t *operator_level_md5,
                                              uint8_t *operator_level_straight_password,
                                              uint8_t *operator_level_oem_proprietary,
                                              uint8_t *admin_level_none,
                                              uint8_t *admin_level_md2,
                                              uint8_t *admin_level_md5,
                                              uint8_t *admin_level_straight_password,
                                              uint8_t *admin_level_oem_proprietary,
                                              uint8_t *oem_level_none,
                                              uint8_t *oem_level_md2,
                                              uint8_t *oem_level_md5,
                                              uint8_t *oem_level_straight_password,
                                              uint8_t *oem_level_oem_proprietary)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs)))
    goto cleanup;
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables (state_data->dev, 
									     channel_number, 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.none", &val) < 0)
    goto cleanup;
  *callback_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.md2", &val) < 0)
    goto cleanup;
  *callback_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.md5", &val) < 0)
    goto cleanup;
  *callback_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.straight_password", &val) < 0)
    goto cleanup;
  *callback_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "callback_level.oem_proprietary", &val) < 0)
    goto cleanup;
  *callback_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.none", &val) < 0)
    goto cleanup;
  *user_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.md2", &val) < 0)
    goto cleanup;
  *user_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.md5", &val) < 0)
    goto cleanup;
  *user_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.straight_password", &val) < 0)
    goto cleanup;
  *user_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "user_level.oem_proprietary", &val) < 0)
    goto cleanup;
  *user_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.none", &val) < 0)
    goto cleanup;
  *operator_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.md2", &val) < 0)
    goto cleanup;
  *operator_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.md5", &val) < 0)
    goto cleanup;
  *operator_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.straight_password", &val) < 0)
    goto cleanup;
  *operator_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "operator_level.oem_proprietary", &val) < 0)
    goto cleanup;
  *operator_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.none", &val) < 0)
    goto cleanup;
  *admin_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.md2", &val) < 0)
    goto cleanup;
  *admin_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.md5", &val) < 0)
    goto cleanup;
  *admin_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.straight_password", &val) < 0)
    goto cleanup;
  *admin_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "admin_level.oem_proprietary", &val) < 0)
    goto cleanup;
  *admin_level_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.none", &val) < 0)
    goto cleanup;
  *oem_level_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.md2", &val) < 0)
    goto cleanup;
  *oem_level_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.md5", &val) < 0)
    goto cleanup;
  *oem_level_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.straight_password", &val) < 0)
    goto cleanup;
  *oem_level_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, "oem_level.oem_proprietary", &val) < 0)
    goto cleanup;
  *oem_level_oem_proprietary = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_bmc_generated_arp_control (bmc_config_state_data_t *state_data, 
					    uint8_t *bmc_generated_gratuitous_arps, 
					    uint8_t *bmc_generated_arp_responses)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control (state_data->dev, 
									   channel_number, 
									   IPMI_GET_LAN_PARAMETER, 
									   SET_SELECTOR, 
									   BLOCK_SELECTOR, 
									   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "bmc_generated_gratuitous_arps", &val) < 0)
    goto cleanup;
  *bmc_generated_gratuitous_arps = val;
  
  if (fiid_obj_get (obj_cmd_rs, "bmc_generated_arp_responses", &val) < 0)
    goto cleanup;
  *bmc_generated_arp_responses = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_lan_conf_gratuitous_arp_interval (bmc_config_state_data_t *state_data, 
					  uint8_t *gratuitous_arp_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval (state_data->dev, 
									 channel_number, 
									 IPMI_GET_LAN_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "gratuitous_arp_interval", &val) < 0)
    goto cleanup;
  *gratuitous_arp_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_serial_channel_volatile_access (bmc_config_state_data_t *state_data, 
					uint8_t *access_mode, 
					uint8_t *user_level_authentication, 
					uint8_t *per_message_authentication, 
					uint8_t *pef_alerting, 
					uint8_t *privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_serial_channel_number (state_data, 
                                        &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return get_bmc_channel_access (state_data, 
				 channel_number, 
				 1, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

config_err_t 
get_bmc_serial_channel_non_volatile_access (bmc_config_state_data_t *state_data, 
					    uint8_t *access_mode, 
					    uint8_t *user_level_authentication, 
					    uint8_t *per_message_authentication, 
					    uint8_t *pef_alerting, 
					    uint8_t *privilege_limit)
{
  config_err_t ret;
  uint8_t channel_number;

  if ((ret = get_serial_channel_number (state_data, 
                                        &channel_number)) != CONFIG_ERR_SUCCESS)
    return ret;

  return get_bmc_channel_access (state_data, 
				 channel_number, 
				 0, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

config_err_t 
get_bmc_serial_conf_connection_mode (bmc_config_state_data_t *state_data, 
				     uint8_t *basic_mode, 
				     uint8_t *ppp_mode, 
				     uint8_t *terminal_mode, 
				     uint8_t *connect_mode)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_connection_mode_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_connection_mode (state_data->dev, 
							       channel_number, 
							       IPMI_GET_SERIAL_MODEM_PARAMETER, 
							       SET_SELECTOR, 
							       BLOCK_SELECTOR, 
							       obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "basic_mode", &val) < 0)
    goto cleanup;
  *basic_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, "ppp_mode", &val) < 0)
    goto cleanup;
  *ppp_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, "terminal_mode", &val) < 0)
    goto cleanup;
  *terminal_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, "connect_mode", &val) < 0)
    goto cleanup;
  *connect_mode = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_serial_conf_page_blackout_interval (bmc_config_state_data_t *state_data, 
					    uint8_t *page_blackout_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_page_blackout_interval (state_data->dev, 
								      channel_number, 
								      IPMI_GET_SERIAL_MODEM_PARAMETER, 
								      SET_SELECTOR, 
								      BLOCK_SELECTOR, 
								      obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "page_blackout_interval", &val) < 0)
    goto cleanup;
  *page_blackout_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_serial_conf_call_retry_interval (bmc_config_state_data_t *state_data, 
					 uint8_t *call_retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_call_retry_interval (state_data->dev, 
								   channel_number, 
								   IPMI_GET_SERIAL_MODEM_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "call_retry_interval", &val) < 0)
    goto cleanup;
  *call_retry_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t 
get_bmc_serial_conf_ipmi_messaging_comm_settings (bmc_config_state_data_t *state_data, 
						  uint8_t *dtr_hangup, 
						  uint8_t *flow_control, 
						  uint8_t *bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings (state_data->dev, 
									    channel_number, 
									    IPMI_GET_SERIAL_MODEM_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "dtr_hangup", &val) < 0)
    goto cleanup;
  *dtr_hangup = val;
  
  if (fiid_obj_get (obj_cmd_rs, "flow_control", &val) < 0)
    goto cleanup;
  *flow_control = val;
  
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
get_bmc_power_restore_policy (bmc_config_state_data_t *state_data, 
			      uint8_t *power_restore_policy)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_chassis_status_rs)))
    goto cleanup;

  if (ipmi_cmd_get_chassis_status (state_data->dev, obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "current_power_state.power_restore_policy", &val) < 0)
    goto cleanup;
  *power_restore_policy = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
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

config_err_t
get_rmcpplus_cipher_suite_id_privilege (bmc_config_state_data_t *state_data,
					uint8_t cipher_suite_id,
					uint8_t *privilege)
{
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t rc;

  if (!(cipher_suite_id < CIPHER_SUITE_LEN))
    goto cleanup;

  if ((rc = _rmcpplus_cipher_suite_id_privilege_setup(state_data)) != CONFIG_ERR_SUCCESS)
    {
      rv = rc;
      goto cleanup;
    }

  if (state_data->cipher_suite_entry_count
      && state_data->cipher_suite_id_supported_set
      && state_data->cipher_suite_priv_set)
    {
      int i, id_found = 0;
      
      for (i = 0; i < state_data->cipher_suite_entry_count; i++)
	{
	  if (state_data->cipher_suite_id_supported[i] == cipher_suite_id)
	    {
	      *privilege = state_data->cipher_suite_priv[cipher_suite_id];
	      id_found++;
	      break;
	    }
	}

      if (id_found)
	rv = CONFIG_ERR_SUCCESS;
      else
        rv = CONFIG_ERR_NON_FATAL_ERROR;
    }
  else
    rv = CONFIG_ERR_NON_FATAL_ERROR;
  
 cleanup:
  return (rv);
}

config_err_t
get_k_r(bmc_config_state_data_t *state_data,
        uint8_t *k_r,
        uint32_t k_r_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t buf[1024];
  uint32_t buf_len;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_security_keys_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_channel_security_keys (state_data->dev, 
                                          channel_number, 
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_READ_KEY,
                                          IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R,
                                          NULL,
                                          0,
                                          obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((buf_len = fiid_obj_get_data (obj_cmd_rs, "key_value", buf, 1024)) < 0)
    goto cleanup;

  if (k_r_len < buf_len)
    goto cleanup;
  memcpy(k_r, buf, buf_len);
 
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_k_g(bmc_config_state_data_t *state_data,
        uint8_t *k_g,
        uint32_t k_g_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t buf[1024];
  uint32_t buf_len;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_channel_security_keys_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_channel_security_keys (state_data->dev, 
                                          channel_number, 
                                          IPMI_CHANNEL_SECURITY_KEYS_OPERATION_READ_KEY,
                                          IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G,
                                          NULL,
                                          0,
                                          obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if ((buf_len = fiid_obj_get_data (obj_cmd_rs, "key_value", buf, 1024)) < 0)
    goto cleanup;

  if (k_g_len < buf_len)
    goto cleanup;
  memcpy(k_g, buf, buf_len);

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
