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

config_err_t 
set_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
				    uint8_t ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_ip_address_source: %s\n",
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_ip_address: %s\n",
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_mac_address: %s\n",
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_subnet_mask: %s\n",
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_default_gateway_address: %s\n",
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
 
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address: %s\n",
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address: %s\n",
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
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
set_bmc_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
			  uint32_t vlan_id,
			  uint8_t vlan_id_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_vlan_id: %s\n",
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
set_bmc_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
				uint8_t vlan_priority)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_vlan_priority: %s\n",
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_connection_mode: %s\n",
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
set_bmc_serial_conf_page_blackout_interval (bmc_config_state_data_t *state_data, 
					    uint8_t page_blackout_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_page_blackout_interval: %s\n",
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
set_bmc_serial_conf_call_retry_interval (bmc_config_state_data_t *state_data, 
					 uint8_t call_retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_call_retry_interval: %s\n",
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
set_bmc_serial_conf_ipmi_messaging_comm_settings (bmc_config_state_data_t *state_data, 
						  uint8_t dtr_hangup, 
						  uint8_t flow_control, 
						  uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
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
set_pef_control (bmc_config_state_data_t *state_data,
                 uint8_t pef,
                 uint8_t pef_event_messages,
                 uint8_t pef_startup_delay,
                 uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (state_data->dev,
                                                             pef,
                                                             pef_event_messages,
                                                             pef_startup_delay,
                                                             pef_alert_startup_delay,
                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_control: %s\n",
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control: %s\n",
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
set_pef_startup_delay (bmc_config_state_data_t *state_data,
                       uint8_t pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   pef_startup_delay,
                                                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay: %s\n",
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
set_pef_alert_startup_delay (bmc_config_state_data_t *state_data,
                             uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         pef_alert_startup_delay,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
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
get_bmc_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
				    uint8_t *ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_ip_address_source: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "ip_address_source", &val) < 0)
    goto cleanup;
  *ip_address_source = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_ip_address_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_ip_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (ip_address && ip_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
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
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_mac_address_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_mac_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (mac_address && mac_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
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
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_subnet_mask: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (subnet_mask && subnet_mask_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
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
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_default_gateway_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (default_gateway_address && default_gateway_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
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
  Fiid_obj_destroy(obj_cmd_rs);
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (default_gateway_mac_address && default_gateway_mac_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
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
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (backup_gateway_address && backup_gateway_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
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
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (backup_gateway_mac_address && backup_gateway_mac_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs, 
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
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_vlan_id: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "vlan_id", &val) < 0)
    goto cleanup;
  *vlan_id = val;

  if (Fiid_obj_get (obj_cmd_rs, "vlan_id_enable", &val) < 0)
    goto cleanup;
  *vlan_id_enable = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_vlan_priority: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "vlan_priority", &val) < 0)
    goto cleanup;
  *vlan_priority = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_connection_mode_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_connection_mode: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "basic_mode", &val) < 0)
    goto cleanup;
  *basic_mode = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "ppp_mode", &val) < 0)
    goto cleanup;
  *ppp_mode = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "terminal_mode", &val) < 0)
    goto cleanup;
  *terminal_mode = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "connect_mode", &val) < 0)
    goto cleanup;
  *connect_mode = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_page_blackout_interval: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "page_blackout_interval", &val) < 0)
    goto cleanup;
  *page_blackout_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_call_retry_interval: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "call_retry_interval", &val) < 0)
    goto cleanup;
  *call_retry_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
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
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "dtr_hangup", &val) < 0)
    goto cleanup;
  *dtr_hangup = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "flow_control", &val) < 0)
    goto cleanup;
  *flow_control = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "bit_rate", &val) < 0)
    goto cleanup;
  *bit_rate = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (state_data->dev,
                                                             IPMI_GET_PEF_PARAMETER,
                                                             SET_SELECTOR,
                                                             BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "pef", &val) < 0)
    goto cleanup;
  *pef = val;

  if (Fiid_obj_get (obj_cmd_rs, "pef_event_messages", &val) < 0)
    goto cleanup;
  *pef_event_messages = val;

  if (Fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;

  if (Fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
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

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (state_data->dev,
                                                                           IPMI_GET_PEF_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "alert_action", &val) < 0)
    goto cleanup;
  *alert_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "power_down_action", &val) < 0)
    goto cleanup;
  *power_down_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "reset_action", &val) < 0)
    goto cleanup;
  *reset_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "power_cycle_action", &val) < 0)
    goto cleanup;
  *power_cycle_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "oem_action", &val) < 0)
    goto cleanup;
  *oem_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "diagnostic_interrupt", &val) < 0)
    goto cleanup;
  *diagnostic_interrupt = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_pef_startup_delay (bmc_config_state_data_t *state_data,
                       uint8_t *pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   SET_SELECTOR,
                                                                   BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_pef_alert_startup_delay (bmc_config_state_data_t *state_data,
                             uint8_t *pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
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
