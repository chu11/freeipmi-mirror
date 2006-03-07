/* 
   bmc-conf2.c: BMC Config functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
*/

#include "common.h"

#include "bit-ops.h"

static int8_t 
set_bmc_user_access (ipmi_device_t *dev, 
		     uint8_t channel_number, 
		     uint8_t user_ipmi_messaging, 
		     uint8_t user_link_authentication, 
		     uint8_t user_restricted_to_callback, 
		     uint8_t userid, 
		     uint8_t privilege_limit, 
		     uint8_t session_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_user_access_rs)))
    goto cleanup;
  if (ipmi_cmd_set_user_access (dev, 
				channel_number, 
				user_ipmi_messaging, 
				user_link_authentication, 
				user_restricted_to_callback, 
				userid, 
				privilege_limit, 
				session_limit, 
				obj_cmd_rs) != 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static int8_t 
set_bmc_channel_access (ipmi_device_t *dev, 
			uint8_t channel_number, 
			uint8_t set_option, 
			uint8_t access_mode, 
			uint8_t user_level_authentication, 
			uint8_t per_message_authentication, 
			uint8_t pef_alerting, 
			uint8_t channel_privilege_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_channel_access_rs)))
    goto cleanup;

  if (ipmi_cmd_set_channel_access (dev, 
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
				   obj_cmd_rs) != 0)
    goto cleanup;
      
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_username (ipmi_device_t *dev, 
		  uint8_t userid, 
		  uint8_t *username)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (userid == 1)
    return (0);
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_user_name_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_name (dev, 
			      userid, 
			      (char *)username, 
			      (username) ? strlen((char *)username) : 0,
			      obj_cmd_rs) != 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_enable_user (ipmi_device_t *dev, 
		     uint8_t userid, 
		     int user_status)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t password[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];
  int8_t rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_user_password_rs)))
    goto cleanup;
  memset (password, 0, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
  if (ipmi_cmd_set_user_password (dev, 
				  userid, 
				  (user_status ? IPMI_PASSWORD_OPERATION_ENABLE_USER :
				   IPMI_PASSWORD_OPERATION_DISABLE_USER), 
				  (char *)password, 
				  0,
				  obj_cmd_rs) != 0)
    goto cleanup;
  
  

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_user_password (ipmi_device_t *dev, 
		       uint8_t userid, 
		       uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password (dev, 
				  userid, 
				  IPMI_PASSWORD_OPERATION_SET_PASSWORD, 
				  (char *)password, 
				  (password) ? strlen((char *)password) : 0,
				  obj_cmd_rs) != 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_user_lan_channel_access (ipmi_device_t *dev, 
				 uint8_t userid, 
				 uint8_t lan_user_ipmi_messaging, 
				 uint8_t lan_user_link_authentication, 
				 uint8_t lan_user_restricted_to_callback, 
				 uint8_t lan_privilege_limit, 
				 uint8_t lan_session_limit)
{
  return set_bmc_user_access (dev, 
			      get_lan_channel_number (), 
			      lan_user_ipmi_messaging, 
			      lan_user_link_authentication, 
			      lan_user_restricted_to_callback, 
			      userid, 
			      lan_privilege_limit, 
			      lan_session_limit);
}

int8_t 
set_bmc_user_serial_channel_access (ipmi_device_t *dev, 
				    uint8_t userid, 
				    uint8_t serial_user_ipmi_messaging, 
				    uint8_t serial_user_link_authentication, 
				    uint8_t serial_user_restricted_to_callback, 
				    uint8_t serial_privilege_limit, 
				    uint8_t serial_session_limit)
{
  return set_bmc_user_access (dev, 
			      get_serial_channel_number (), 
			      serial_user_ipmi_messaging, 
			      serial_user_link_authentication, 
			      serial_user_restricted_to_callback, 
			      userid, 
			      serial_privilege_limit, 
			      serial_session_limit);
}

int8_t 
set_bmc_lan_channel_volatile_access (ipmi_device_t *dev, 
				     uint8_t access_mode, 
				     uint8_t user_level_authentication, 
				     uint8_t per_message_authentication, 
				     uint8_t pef_alerting, 
				     uint8_t channel_privilege_limit)
{
  return set_bmc_channel_access (dev, 
				 get_lan_channel_number (), 
				 1, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

int8_t 
set_bmc_lan_channel_non_volatile_access (ipmi_device_t *dev, 
					 uint8_t access_mode, 
					 uint8_t user_level_authentication, 
					 uint8_t per_message_authentication, 
					 uint8_t pef_alerting, 
					 uint8_t channel_privilege_limit)
{
  return set_bmc_channel_access (dev, 
				 get_lan_channel_number (), 
				 0, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

int8_t 
set_bmc_lan_conf_ip_address_source (ipmi_device_t *dev, 
                                    uint8_t ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address_source (dev, 
								   get_lan_channel_number (), 
								   ip_address_source, 
								   obj_cmd_rs) != 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_lan_conf_ip_address (ipmi_device_t *dev, 
                             char *ip_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  int8_t rv = -1;
  
  if (ipmi_ipv4_address_string2int(ip_address, &ip_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address (dev, 
							    get_lan_channel_number (), 
							    ip_address_val, 
							    obj_cmd_rs) != 0)
    goto cleanup;
    
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_lan_conf_mac_address (ipmi_device_t *dev, 
                              char *mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  int8_t rv = -1;
  
  if (ipmi_mac_address_string2int(mac_address, &mac_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_mac_address (dev, 
							     get_lan_channel_number (), 
							     mac_address_val, 
							     obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_lan_conf_subnet_mask (ipmi_device_t *dev, 
			      char *subnet_mask)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t subnet_mask_val = 0;
  int8_t rv = -1;
  
  if (ipmi_ipv4_address_string2int(subnet_mask, &subnet_mask_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_subnet_mask (dev, 
							     get_lan_channel_number (), 
							     subnet_mask_val, 
							     obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv); 
}

int8_t 
set_bmc_lan_conf_default_gateway_address (ipmi_device_t *dev, 
                                          char *default_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  int8_t rv = -1;
  
  if (ipmi_ipv4_address_string2int(default_gateway_address, &ip_address_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_address (dev, 
									 get_lan_channel_number (), 
									 ip_address_val, 
									 obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_lan_conf_default_gateway_mac_address (ipmi_device_t *dev, 
                                              char *default_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  int8_t rv = -1;
  
  if (ipmi_mac_address_string2int(default_gateway_mac_address, &mac_address_val) < 0)
    goto cleanup;
 
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address (dev, 
									     get_lan_channel_number (), 
									     mac_address_val, 
									     obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_lan_conf_backup_gateway_address (ipmi_device_t *dev, 
                                         char *backup_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  int8_t rv = -1;
  
  if (ipmi_ipv4_address_string2int(backup_gateway_address, &ip_address_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address (dev, 
									get_lan_channel_number (), 
									ip_address_val, 
									obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_lan_conf_backup_gateway_mac_address (ipmi_device_t *dev, 
                                             char *backup_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  int8_t rv = -1;
  
  if (ipmi_mac_address_string2int(backup_gateway_mac_address, &mac_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (dev, 
									    get_lan_channel_number (), 
									    mac_address_val, 
									    obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_lan_conf_vlan_id (ipmi_device_t *dev, 
			  uint32_t vlan_id,
			  uint8_t vlan_id_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t *ptr, ls_val, ms_val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;
  
  ptr = (uint8_t *)&vlan_id;
#if WORDS_BIGENDIAN
  ls_val = ptr[3];
  ms_val = ptr[2];
#else
  ls_val = ptr[0];
  ms_val = ptr[1];
#endif

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_id (dev, 
							 get_lan_channel_number (), 
							 ls_val,
							 ms_val,
							 vlan_id_enable, 
							 obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_lan_conf_vlan_priority (ipmi_device_t *dev, 
				uint8_t vlan_priority)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_priority (dev, 
							       get_lan_channel_number (), 
							       vlan_priority, 
							       obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static int8_t
_fill_lan_set_authentication_type_enables (fiid_obj_t obj_data_rq,
                                           fiid_template_t l_tmpl_set_auth_authentication_type_enables,
                                           uint8_t channel_number,
                                           uint8_t callback_level,
                                           uint8_t user_level,
                                           uint8_t operator_level,
                                           uint8_t admin_level,
                                           uint8_t oem_level)
{
  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"cmd",
                    IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"channel_number",
                    channel_number) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"reserved1",
                    0) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"parameter_selector",
                    IPMI_LAN_PARAM_AUTHENTICATION_TYPE_ENABLES) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"callback_level",
                    callback_level) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"callback_level.reserved2",
                    0) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"user_level",
                    user_level) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"user_level.reserved2",
                    0) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"operator_level",
                    operator_level) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"operator_level.reserved2",
                    0) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"admin_level",
                    admin_level) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"admin_level.reserved2",
                    0) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"oem_level",
                    oem_level) < 0)
    return (-1);

  if (fiid_obj_set (obj_data_rq,
                    (uint8_t *)"oem_level.reserved2",
                    0) < 0)
    return (-1);

  return 0;
}

int8_t 
set_bmc_lan_conf_authentication_type_enables (ipmi_device_t *dev, 
                                              struct bmc_authentication_level *bmc_authentication_level)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_template_t l_tmpl_set_lan_configuration_parameters_authentication_type_enables_rq =
    {
      {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {6, "callback_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {2, "callback_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {6, "user_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {2, "user_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {6, "operator_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {2, "operator_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {6, "admin_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {2, "admin_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {6, "oem_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {2, "oem_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      {0, ""}
    };
  uint8_t callback_level = 0;
  uint8_t user_level = 0;
  uint8_t operator_level = 0;
  uint8_t admin_level = 0;
  uint8_t oem_level = 0;
  int8_t rv = -1;

  if (bmc_authentication_level->callback.type_none)
    callback_level = BIT_SET (callback_level, 0);
  if (bmc_authentication_level->callback.type_md2)
    callback_level = BIT_SET (callback_level, 1);
  if (bmc_authentication_level->callback.type_md5)
    callback_level = BIT_SET (callback_level, 2);
  if (bmc_authentication_level->callback.type_straight_password)
    callback_level = BIT_SET (callback_level, 4);
  if (bmc_authentication_level->callback.type_oem_proprietary)
    callback_level = BIT_SET (callback_level, 5);
  
  if (bmc_authentication_level->user.type_none)
    user_level = BIT_SET (user_level, 0);
  if (bmc_authentication_level->user.type_md2)
    user_level = BIT_SET (user_level, 1);
  if (bmc_authentication_level->user.type_md5)
    user_level = BIT_SET (user_level, 2);
  if (bmc_authentication_level->user.type_straight_password)
    user_level = BIT_SET (user_level, 4);
  if (bmc_authentication_level->user.type_oem_proprietary)
    user_level = BIT_SET (user_level, 5);
  
  if (bmc_authentication_level->operator.type_none)
    operator_level = BIT_SET (operator_level, 0);
  if (bmc_authentication_level->operator.type_md2)
    operator_level = BIT_SET (operator_level, 1);
  if (bmc_authentication_level->operator.type_md5)
    operator_level = BIT_SET (operator_level, 2);
  if (bmc_authentication_level->operator.type_straight_password)
    operator_level = BIT_SET (operator_level, 4);
  if (bmc_authentication_level->operator.type_oem_proprietary)
    operator_level = BIT_SET (operator_level, 5);
  
  if (bmc_authentication_level->admin.type_none)
    admin_level = BIT_SET (admin_level, 0);
  if (bmc_authentication_level->admin.type_md2)
    admin_level = BIT_SET (admin_level, 1);
  if (bmc_authentication_level->admin.type_md5)
    admin_level = BIT_SET (admin_level, 2);
  if (bmc_authentication_level->admin.type_straight_password)
    admin_level = BIT_SET (admin_level, 4);
  if (bmc_authentication_level->admin.type_oem_proprietary)
    admin_level = BIT_SET (admin_level, 5);
  
  if (bmc_authentication_level->oem.type_none)
    oem_level = BIT_SET (oem_level, 0);
  if (bmc_authentication_level->oem.type_md2)
    oem_level = BIT_SET (oem_level, 1);
  if (bmc_authentication_level->oem.type_md5)
    oem_level = BIT_SET (oem_level, 2);
  if (bmc_authentication_level->oem.type_straight_password)
    oem_level = BIT_SET (oem_level, 4);
  if (bmc_authentication_level->oem.type_oem_proprietary)
    oem_level = BIT_SET (oem_level, 5);

  if (!(obj_cmd_rq = fiid_obj_create(l_tmpl_set_lan_configuration_parameters_authentication_type_enables_rq)))
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (_fill_lan_set_authentication_type_enables (obj_cmd_rq,
                                                 l_tmpl_set_lan_configuration_parameters_authentication_type_enables_rq,
                                                 get_lan_channel_number (), 
                                                 callback_level,
                                                 user_level,
                                                 operator_level,
                                                 admin_level,
                                                 oem_level) < 0)
    goto cleanup;

  if (ipmi_cmd (dev,
                IPMI_BMC_IPMB_LUN_BMC,
                IPMI_NET_FN_TRANSPORT_RQ,
                obj_cmd_rq,
                obj_cmd_rs) < 0)
    goto cleanup;

  if (ipmi_check_completion_code_success (obj_cmd_rs) != 1)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_lan_conf_bmc_generated_arp_control (ipmi_device_t *dev, 
                                            uint8_t bmc_generated_gratuitous_arps, 
                                            uint8_t bmc_generated_arp_responses)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (dev, 
									   get_lan_channel_number (), 
									   bmc_generated_gratuitous_arps, 
									   bmc_generated_arp_responses, 
									   obj_cmd_rs) != 0)
    goto cleanup;
    
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);

}

int8_t 
set_bmc_lan_conf_gratuitous_arp_interval (ipmi_device_t *dev, 
                                          uint8_t gratuitous_arp_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval (dev, 
									 get_lan_channel_number (), 
									 gratuitous_arp_interval, 
									 obj_cmd_rs) != 0)
    goto cleanup;
    
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_serial_channel_volatile_access (ipmi_device_t *dev, 
					uint8_t access_mode, 
					uint8_t user_level_authentication, 
					uint8_t per_message_authentication, 
					uint8_t pef_alerting, 
					uint8_t channel_privilege_limit)
{
  return set_bmc_channel_access (dev, 
				 get_serial_channel_number (), 
				 1, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

int8_t 
set_bmc_serial_channel_non_volatile_access (ipmi_device_t *dev, 
					    uint8_t access_mode, 
					    uint8_t user_level_authentication, 
					    uint8_t per_message_authentication, 
					    uint8_t pef_alerting, 
					    uint8_t channel_privilege_limit)
{
  return set_bmc_channel_access (dev, 
				 get_serial_channel_number (), 
				 0, 
				 access_mode, 
				 (user_level_authentication ? 0 : 1), 
				 (per_message_authentication ? 0 : 1), 
				 (pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

int8_t 
set_bmc_serial_conf_connection_mode (ipmi_device_t *dev, 
                                     uint8_t basic_mode,
                                     uint8_t ppp_mode,
                                     uint8_t terminal_mode,
                                     uint8_t connect_mode)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_serial_modem_configuration_rs)))
    goto cleanup;

  if (ipmi_cmd_set_serial_modem_configuration_connection_mode (dev, 
							       get_serial_channel_number (), 
							       basic_mode,
							       ppp_mode,
							       terminal_mode,
							       connect_mode,
							       obj_cmd_rs) != 0)
    goto cleanup;
   
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
set_bmc_serial_conf_page_blackout_interval (ipmi_device_t *dev, 
					    uint8_t page_blackout_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_serial_modem_configuration_rs)))
    goto cleanup;

  if (ipmi_cmd_set_serial_modem_configuration_page_blackout_interval (dev, 
								      get_serial_channel_number (), 
								      page_blackout_interval, 
								      obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_serial_conf_call_retry_interval (ipmi_device_t *dev, 
                                         uint8_t call_retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_serial_modem_configuration_rs)))
    goto cleanup;

  if (ipmi_cmd_set_serial_modem_configuration_call_retry_interval (dev, 
								   get_serial_channel_number (), 
								   call_retry_interval, 
								   obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_serial_conf_ipmi_messaging_comm_settings (ipmi_device_t *dev, 
                                                  uint8_t dtr_hangup, 
                                                  uint8_t flow_control, 
                                                  uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_serial_modem_configuration_rs)))
    goto cleanup;

  if (ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (dev, 
									    get_serial_channel_number (), 
									    dtr_hangup, 
									    flow_control, 
									    bit_rate, 
									    obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_bmc_power_restore_policy (ipmi_device_t *dev, 
			      uint8_t power_restore_policy)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_power_restore_policy_rs)))
    goto cleanup;

  if (ipmi_cmd_set_power_restore_policy (dev, 
					 power_restore_policy, 
					 obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_pef_control (ipmi_device_t *dev, 
		 uint8_t pef, 
		 uint8_t pef_event_messages, 
		 uint8_t pef_startup_delay, 
		 uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (dev, 
							     pef, 
							     pef_event_messages, 
							     pef_startup_delay, 
							     pef_alert_startup_delay, 
							     obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_pef_action_global_control (ipmi_device_t *dev, 
			       uint8_t alert_action, 
			       uint8_t power_down_action, 
			       uint8_t reset_action, 
			       uint8_t power_cycle_action, 
			       uint8_t oem_action, 
			       uint8_t diagnostic_interrupt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (dev, 
									   alert_action, 
									   power_down_action, 
									   reset_action, 
									   power_cycle_action, 
									   oem_action, 
									   diagnostic_interrupt, 
									   obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_pef_startup_delay (ipmi_device_t *dev, 
		       uint8_t pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (dev, 
								   pef_startup_delay, 
								   obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);  
}

int8_t 
set_pef_alert_startup_delay (ipmi_device_t *dev, 
			     uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (dev, 
									 pef_alert_startup_delay, 
									 obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t
set_sol_sol_enable(ipmi_device_t *dev,
                   uint8_t sol_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_sol_configuration_parameters_sol_enable (dev, 
                                                            get_lan_channel_number (),
                                                            sol_enable, 
                                                            obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t
set_sol_sol_authentication(ipmi_device_t *dev,
                           uint8_t sol_privilege_level,
                           uint8_t force_sol_payload_authentication,
                           uint8_t force_sol_payload_encryption)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_sol_configuration_parameters_sol_authentication (dev, 
                                                                    get_lan_channel_number (),
                                                                    sol_privilege_level,
                                                                    force_sol_payload_authentication,
                                                                    force_sol_payload_encryption,
                                                                    obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t
set_sol_character_accumulate_interval_and_send_threshold(ipmi_device_t *dev,
                                                         uint8_t character_accumulate_interval,
                                                         uint8_t character_send_threshold)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (dev, 
                                                                                                  get_lan_channel_number (),
                                                                                                  character_accumulate_interval,
                                                                                                  character_send_threshold,
                                                                                                  obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t
set_sol_sol_retry(ipmi_device_t *dev,
                  uint8_t retry_count,
                  uint8_t retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if (ipmi_cmd_set_sol_configuration_parameters_sol_retry (dev, 
                                                           get_lan_channel_number (),
                                                           retry_count,
                                                           retry_interval,
                                                           obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t
set_sol_sol_non_volatile_bit_rate(ipmi_device_t *dev,
                                  uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if (ipmi_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (dev, 
                                                                           get_lan_channel_number (),
                                                                           bit_rate,
                                                                           obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t
set_sol_sol_volatile_bit_rate(ipmi_device_t *dev,
                              uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if (ipmi_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (dev, 
                                                                       get_lan_channel_number (),
                                                                       bit_rate,
                                                                       obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t
set_sol_sol_payload_port_number(ipmi_device_t *dev,
                                uint16_t port_number)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t *ptr, ls_val, ms_val;
  int8_t rv = -1;  

  ptr = (uint8_t *)&port_number;
#if WORDS_BIGENDIAN
  ls_val = ptr[1];
  ms_val = ptr[0];
#else
  ls_val = ptr[0];
  ms_val = ptr[1];
#endif

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if (ipmi_cmd_set_sol_configuration_parameters_sol_payload_port_number (dev, 
                                                                         get_lan_channel_number (),
                                                                         ls_val,
                                                                         ms_val,
                                                                         obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static int8_t 
get_bmc_user_access (ipmi_device_t *dev, 
		     uint8_t userid, 
		     uint8_t channel_number, 
		     uint8_t *user_ipmi_messaging, 
		     uint8_t *user_link_authentication, 
		     uint8_t *user_restricted_to_callback, 
		     uint8_t *privilege_limit, 
		     uint8_t *session_limit)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_user_access_rs)))
    goto cleanup;

  if (ipmi_cmd_get_user_access (dev, 
				channel_number, 
				userid, 
				obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_privilege_level_limit", 
		    &val) < 0)
    goto cleanup;
  *privilege_limit = (uint8_t) val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_ipmi_messaging", 
		    &val) < 0)
    goto cleanup;
  *user_ipmi_messaging = (uint8_t) val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_link_authentication", 
		    &val) < 0)
    goto cleanup;
  *user_link_authentication = (uint8_t) val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_restricted_to_callback", 
		    &val) < 0)
    goto cleanup;
  *user_restricted_to_callback = (uint8_t) val;
  
  *session_limit = 0;
 
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static int8_t 
get_bmc_channel_access (ipmi_device_t *dev, 
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
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_channel_access_rs)))
    goto cleanup;

  if (ipmi_cmd_get_channel_access (dev, 
				   channel_number, 
				   (access_type ? IPMI_CHANNEL_ACCESS_GET_VOLATILE :
				    IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE), 
				   obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"ipmi_messaging_access_mode", 
		    &val) < 0)
    goto cleanup;
  *access_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_level_authentication", 
		    &val) < 0)
    goto cleanup;
  *user_level_authentication = (val ? 0 : 1);
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"per_message_authentication", 
		    &val) < 0)
    goto cleanup;
  *per_message_authentication = (val ? 0 : 1);
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"pef_alerting", 
		    &val) < 0)
    goto cleanup;
  *pef_alerting = (val ? 0 : 1);
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"channel_privilege_level_limit", 
		    &val) < 0)
    goto cleanup;
  *privilege_limit = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_username (ipmi_device_t *dev, 
		  uint8_t userid, 
		  uint8_t *username,
                  uint32_t username_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (userid == 1)
    {
      strcpy ((char *)username, "NULL");
      return (0);
    }
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_user_name_rs)))
    goto cleanup;

  if (ipmi_cmd_get_user_name (dev, 
			      userid, 
			      obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"user_name", 
			 username,
			 username_len) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_user_lan_channel_access (ipmi_device_t *dev, 
				 uint8_t userid, 
				 uint8_t *user_ipmi_messaging, 
				 uint8_t *user_link_authentication, 
				 uint8_t *user_restricted_to_callback, 
				 uint8_t *privilege_limit, 
				 uint8_t *session_limit)
{
  return get_bmc_user_access (dev, 
			      userid, 
			      get_lan_channel_number (), 
			      user_ipmi_messaging, 
			      user_link_authentication, 
			      user_restricted_to_callback, 
			      privilege_limit, 
			      session_limit);
}

int8_t 
get_bmc_user_serial_channel_access (ipmi_device_t *dev, 
				    uint8_t userid, 
				    uint8_t *user_ipmi_messaging, 
				    uint8_t *user_link_authentication, 
				    uint8_t *user_restricted_to_callback, 
				    uint8_t *privilege_limit, 
				    uint8_t *session_limit)
{
  return get_bmc_user_access (dev, 
			      userid, 
			      get_serial_channel_number (), 
			      user_ipmi_messaging, 
			      user_link_authentication, 
			      user_restricted_to_callback, 
			      privilege_limit, 
			      session_limit);
}

int8_t 
get_bmc_lan_channel_volatile_access (ipmi_device_t *dev, 
				     uint8_t *access_mode, 
				     uint8_t *user_level_authentication, 
				     uint8_t *per_message_authentication, 
				     uint8_t *pef_alerting, 
				     uint8_t *privilege_limit)
{
  return get_bmc_channel_access (dev, 
				 get_lan_channel_number (), 
				 1, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

int8_t 
get_bmc_lan_channel_non_volatile_access (ipmi_device_t *dev, 
					 uint8_t *access_mode, 
					 uint8_t *user_level_authentication, 
					 uint8_t *per_message_authentication, 
					 uint8_t *pef_alerting, 
					 uint8_t *privilege_limit)
{
  return get_bmc_channel_access (dev, 
				 get_lan_channel_number (), 
				 0, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

int8_t 
get_bmc_lan_conf_ip_address_source (ipmi_device_t *dev, 
                                    uint8_t *ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_ip_address_source_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address_source (dev, 
								   get_lan_channel_number (), 
								   IPMI_GET_LAN_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"ip_address_source", 
		    &val) < 0)
    goto cleanup;
  *ip_address_source = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_ip_address (ipmi_device_t *dev, 
                             char *ip_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_ip_address_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address (dev, 
							    get_lan_channel_number (), 
							    IPMI_GET_LAN_PARAMETER, 
							    SET_SELECTOR, 
							    BLOCK_SELECTOR, 
							    obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"ip_address", 
			 ip_address_bytes,
			 4) < 0)
    goto cleanup;
  sprintf (ip_address, 
	   "%u.%u.%u.%u", 
	   ip_address_bytes[0], 
	   ip_address_bytes[1], 
	   ip_address_bytes[2], 
	   ip_address_bytes[3]);
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_mac_address (ipmi_device_t *dev, 
                              char *mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_mac_address_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (dev, 
							     get_lan_channel_number (), 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"mac_address", 
			 mac_address_bytes,
			 6) < 0)
    goto cleanup;
  sprintf (mac_address, 
	   "%02X:%02X:%02X:%02X:%02X:%02X", 
	   mac_address_bytes[0], 
	   mac_address_bytes[1], 
	   mac_address_bytes[2], 
	   mac_address_bytes[3], 
	   mac_address_bytes[4], 
	   mac_address_bytes[5]);
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_subnet_mask (ipmi_device_t *dev, 
			      char *subnet_mask)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t subnet_mask_bytes[4];
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_subnet_mask_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_subnet_mask (dev, 
							     get_lan_channel_number (), 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"subnet_mask", 
			 subnet_mask_bytes,
			 4) < 0)
    goto cleanup;
  sprintf (subnet_mask, 
	   "%u.%u.%u.%u", 
	   subnet_mask_bytes[0], 
	   subnet_mask_bytes[1], 
	   subnet_mask_bytes[2], 
	   subnet_mask_bytes[3]);
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_default_gateway_address (ipmi_device_t *dev, 
                                          char *default_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_default_gateway_address_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_address (dev, 
									 get_lan_channel_number (), 
									 IPMI_GET_LAN_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"ip_address", 
			 ip_address_bytes,
			 4) < 0)
    goto cleanup;
  sprintf (default_gateway_address, 
	   "%u.%u.%u.%u", 
	   ip_address_bytes[0], 
	   ip_address_bytes[1], 
	   ip_address_bytes[2], 
	   ip_address_bytes[3]);
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_default_gateway_mac_address (ipmi_device_t *dev, 
                                              char *default_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_default_gateway_mac_address_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address (dev, 
									     get_lan_channel_number (), 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"mac_address", 
			 mac_address_bytes,
			 6) < 0)
    goto cleanup;
  sprintf (default_gateway_mac_address, 
	   "%02X:%02X:%02X:%02X:%02X:%02X", 
	   mac_address_bytes[0], 
	   mac_address_bytes[1], 
	   mac_address_bytes[2], 
	   mac_address_bytes[3], 
	   mac_address_bytes[4], 
	   mac_address_bytes[5]);
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_backup_gateway_address (ipmi_device_t *dev, 
                                         char *backup_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_backup_gateway_address_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address (dev, 
									get_lan_channel_number (), 
									IPMI_GET_LAN_PARAMETER, 
									SET_SELECTOR, 
									BLOCK_SELECTOR, 
									obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"ip_address", 
			 ip_address_bytes,
			 4) < 0)
    goto cleanup;
  sprintf (backup_gateway_address, 
	   "%u.%u.%u.%u", 
	   ip_address_bytes[0], 
	   ip_address_bytes[1], 
	   ip_address_bytes[2], 
	   ip_address_bytes[3]);
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_backup_gateway_mac_address (ipmi_device_t *dev, 
                                             char *backup_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_backup_gateway_mac_address_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address (dev, 
									    get_lan_channel_number (), 
									    IPMI_GET_LAN_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get_data (obj_cmd_rs, 
			 (uint8_t *)"mac_address", 
			 mac_address_bytes,
			 6) < 0)
    goto cleanup;
  sprintf (backup_gateway_mac_address, 
	   "%02X:%02X:%02X:%02X:%02X:%02X", 
	   mac_address_bytes[0], 
	   mac_address_bytes[1], 
	   mac_address_bytes[2], 
	   mac_address_bytes[3], 
	   mac_address_bytes[4], 
	   mac_address_bytes[5]);
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_vlan_id (ipmi_device_t *dev, 
			  uint32_t *vlan_id,
			  uint8_t *vlan_id_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t ls_val, ms_val, val;
  uint8_t *ptr;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_vlan_id_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_id (dev, 
							 get_lan_channel_number (), 
							 IPMI_GET_LAN_PARAMETER, 
							 SET_SELECTOR, 
							 BLOCK_SELECTOR, 
							 obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"vlan_id_ls", 
		    &ls_val) < 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"vlan_id_ms", 
		    &ms_val) < 0)
    goto cleanup;
  
  ptr = (uint8_t *)vlan_id;
#if WORDS_BIGENDIAN
  ptr[3] = ls_val;
  ptr[2] = ms_val;
#else
  ptr[0] = ls_val;
  ptr[1] = ms_val;
#endif
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"vlan_id_enable", 
		    &val) < 0)
    goto cleanup;
  *vlan_id_enable = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_vlan_priority (ipmi_device_t *dev, 
				uint8_t *vlan_priority)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_vlan_priority_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_priority (dev, 
							       get_lan_channel_number (), 
							       IPMI_GET_LAN_PARAMETER, 
							       SET_SELECTOR, 
							       BLOCK_SELECTOR, 
							       obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"vlan_priority", 
		    &val) < 0)
    goto cleanup;
  *vlan_priority = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_authentication_type_enables (ipmi_device_t *dev, 
                                              struct bmc_authentication_level *bmc_authentication_level)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_authentication_type_enables_rs)))
    goto cleanup;
  
  if (ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables (dev, 
									     get_lan_channel_number (), 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"callback_level.none", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->callback.type_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"callback_level.md2", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->callback.type_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"callback_level.md5", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->callback.type_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"callback_level.straight_password", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->callback.type_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"callback_level.oem_proprietary", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->callback.type_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_level.none", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->user.type_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_level.md2", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->user.type_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_level.md5", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->user.type_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_level.straight_password", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->user.type_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"user_level.oem_proprietary", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->user.type_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"operator_level.none", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->operator.type_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"operator_level.md2", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->operator.type_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"operator_level.md5", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->operator.type_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"operator_level.straight_password", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->operator.type_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"operator_level.oem_proprietary", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->operator.type_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"admin_level.none", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->admin.type_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"admin_level.md2", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->admin.type_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"admin_level.md5", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->admin.type_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"admin_level.straight_password", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->admin.type_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"admin_level.oem_proprietary", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->admin.type_oem_proprietary = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"oem_level.none", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->oem.type_none = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"oem_level.md2", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->oem.type_md2 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"oem_level.md5", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->oem.type_md5 = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"oem_level.straight_password", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->oem.type_straight_password = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"oem_level.oem_proprietary", 
		    &val) < 0)
    goto cleanup;
  bmc_authentication_level->oem.type_oem_proprietary = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_bmc_generated_arp_control (ipmi_device_t *dev, 
                                            uint8_t *bmc_generated_gratuitous_arps, 
                                            uint8_t *bmc_generated_arp_responses)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_bmc_generated_arp_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control (dev, 
									   get_lan_channel_number (), 
									   IPMI_GET_LAN_PARAMETER, 
									   SET_SELECTOR, 
									   BLOCK_SELECTOR, 
									   obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"bmc_generated_gratuitous_arps", 
		    &val) < 0)
    goto cleanup;
  *bmc_generated_gratuitous_arps = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"bmc_generated_arp_responses", 
		    &val) < 0)
    goto cleanup;
  *bmc_generated_arp_responses = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_lan_conf_gratuitous_arp_interval (ipmi_device_t *dev, 
                                          uint8_t *gratuitous_arp_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_lan_configuration_parameters_gratuitous_arp_interval_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval (dev, 
									 get_lan_channel_number (), 
									 IPMI_GET_LAN_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"gratuitous_arp_interval", 
		    &val) < 0)
    goto cleanup;
  *gratuitous_arp_interval = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_serial_channel_volatile_access (ipmi_device_t *dev, 
					uint8_t *access_mode, 
					uint8_t *user_level_authentication, 
					uint8_t *per_message_authentication, 
					uint8_t *pef_alerting, 
					uint8_t *privilege_limit)
{
  return get_bmc_channel_access (dev, 
				 get_serial_channel_number (), 
				 1, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

int8_t 
get_bmc_serial_channel_non_volatile_access (ipmi_device_t *dev, 
					    uint8_t *access_mode, 
					    uint8_t *user_level_authentication, 
					    uint8_t *per_message_authentication, 
					    uint8_t *pef_alerting, 
					    uint8_t *privilege_limit)
{
  return get_bmc_channel_access (dev, 
				 get_serial_channel_number (), 
				 0, 
				 access_mode, 
				 user_level_authentication, 
				 per_message_authentication, 
				 pef_alerting, 
				 privilege_limit);
}

int8_t 
get_bmc_serial_conf_connection_mode (ipmi_device_t *dev, 
                                     uint8_t *basic_mode, 
                                     uint8_t *ppp_mode, 
                                     uint8_t *terminal_mode, 
                                     uint8_t *connect_mode)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_serial_modem_configuration_connection_mode_rs)))
    goto cleanup;

  if (ipmi_cmd_get_serial_modem_configuration_connection_mode (dev, 
							       get_serial_channel_number (), 
							       IPMI_GET_SERIAL_MODEM_PARAMETER, 
							       SET_SELECTOR, 
							       BLOCK_SELECTOR, 
							       obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"basic_mode", 
		    &val) < 0)
    goto cleanup;
  *basic_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"ppp_mode", 
		    &val) < 0)
    goto cleanup;
  *ppp_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"terminal_mode", 
		    &val) < 0)
    goto cleanup;
  *terminal_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"connect_mode", 
		    &val) < 0)
    goto cleanup;
  *connect_mode = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_serial_conf_page_blackout_interval (ipmi_device_t *dev, 
					    uint8_t *page_blackout_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_serial_modem_configuration_page_blackout_interval_rs)))
    goto cleanup;

  if (ipmi_cmd_get_serial_modem_configuration_page_blackout_interval (dev, 
								      get_serial_channel_number (), 
								      IPMI_GET_SERIAL_MODEM_PARAMETER, 
								      SET_SELECTOR, 
								      BLOCK_SELECTOR, 
								      obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"page_blackout_interval", 
		    &val) < 0)
    goto cleanup;
  *page_blackout_interval = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_serial_conf_call_retry_interval (ipmi_device_t *dev, 
                                         uint8_t *call_retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_serial_modem_configuration_call_retry_interval_rs)))
    goto cleanup;

  if (ipmi_cmd_get_serial_modem_configuration_call_retry_interval (dev, 
								   get_serial_channel_number (), 
								   IPMI_GET_SERIAL_MODEM_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"call_retry_interval", 
		    &val) < 0)
    goto cleanup;
  *call_retry_interval = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_serial_conf_ipmi_messaging_comm_settings (ipmi_device_t *dev, 
                                                  uint8_t *dtr_hangup, 
                                                  uint8_t *flow_control, 
                                                  uint8_t *bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs)))
    goto cleanup;

  if (ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings (dev, 
									    get_serial_channel_number (), 
									    IPMI_GET_SERIAL_MODEM_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"dtr_hangup", 
		    &val) < 0)
    goto cleanup;
  *dtr_hangup = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"flow_control", 
		    &val) < 0)
    goto cleanup;
  *flow_control = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"bit_rate", 
		    &val) < 0)
    goto cleanup;
  *bit_rate = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_bmc_power_restore_policy (ipmi_device_t *dev, 
			      uint8_t *power_restore_policy)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_chassis_status_rs)))
    goto cleanup;

  if (ipmi_cmd_get_chassis_status (dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"current_power_state.power_restore_policy", 
		    &val) < 0)
    goto cleanup;
  *power_restore_policy = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_pef_control (ipmi_device_t *dev, 
		 uint8_t *pef, 
		 uint8_t *pef_event_messages, 
		 uint8_t *pef_startup_delay, 
		 uint8_t *pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_pef_configuration_parameters_pef_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (dev, 
							     IPMI_GET_PEF_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"pef", 
		    &val) < 0)
    goto cleanup;
  *pef = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"pef_event_messages", 
		    &val) < 0)
    goto cleanup;
  *pef_event_messages = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"pef_startup_delay", 
		    &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"pef_alert_startup_delay", 
		    &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_pef_action_global_control (ipmi_device_t *dev, 
			       uint8_t *alert_action, 
			       uint8_t *power_down_action, 
			       uint8_t *reset_action, 
			       uint8_t *power_cycle_action, 
			       uint8_t *oem_action, 
			       uint8_t *diagnostic_interrupt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_pef_configuration_parameters_pef_action_global_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (dev, 
									   IPMI_GET_PEF_PARAMETER, 
									   SET_SELECTOR, 
									   BLOCK_SELECTOR, 
									   obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"alert_action", 
		    &val) < 0)
    goto cleanup;
  *alert_action = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"power_down_action", 
		    &val) < 0)
    goto cleanup;
  *power_down_action = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"reset_action", 
		    &val) < 0)
    goto cleanup;
  *reset_action = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"power_cycle_action", 
		    &val) < 0)
    goto cleanup;
  *power_cycle_action = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"oem_action", 
		    &val) < 0)
    goto cleanup;
  *oem_action = val;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"diagnostic_interrupt", 
		    &val) < 0)
    goto cleanup;
  *diagnostic_interrupt = val;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_pef_startup_delay (ipmi_device_t *dev, 
		       uint8_t *pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_pef_configuration_parameters_pef_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (dev, 
								   IPMI_GET_PEF_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"pef_startup_delay", 
		    &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_pef_alert_startup_delay (ipmi_device_t *dev, 
			     uint8_t *pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (dev, 
									 IPMI_GET_PEF_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"pef_alert_startup_delay", 
		    &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_sol_sol_enable (ipmi_device_t *dev, 
                    uint8_t *sol_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sol_configuration_parameters_sol_enable_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_enable (dev, 
                                                            get_lan_channel_number (), 
                                                            IPMI_GET_SOL_PARAMETER, 
                                                            SET_SELECTOR, 
                                                            BLOCK_SELECTOR, 
                                                            obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"sol_enable", 
		    &val) < 0)
    goto cleanup;
  *sol_enable = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_sol_sol_authentication (ipmi_device_t *dev, 
                            uint8_t *sol_privilege_level,
                            uint8_t *force_sol_payload_authentication,
                            uint8_t *force_sol_payload_encryption)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sol_configuration_parameters_sol_authentication_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_authentication (dev, 
                                                                    get_lan_channel_number (), 
                                                                    IPMI_GET_SOL_PARAMETER, 
                                                                    SET_SELECTOR, 
                                                                    BLOCK_SELECTOR, 
                                                                    obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"sol_privilege_level", 
		    &val) < 0)
    goto cleanup;
  *sol_privilege_level = val;

  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"force_sol_payload_authentication,", 
		    &val) < 0)
    goto cleanup;
  *force_sol_payload_authentication = val;

  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"force_sol_payload_encryption,", 
		    &val) < 0)
    goto cleanup;
  *force_sol_payload_encryption = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_sol_character_accumulate_interval_and_send_threshold (ipmi_device_t *dev, 
                                                          uint8_t *character_accumulate_interval,
                                                          uint8_t *character_send_threshold)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (dev, 
                                                                                                  get_lan_channel_number (), 
                                                                                                  IPMI_GET_SOL_PARAMETER, 
                                                                                                  SET_SELECTOR, 
                                                                                                  BLOCK_SELECTOR, 
                                                                                                  obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"character_accumulate_interval", 
		    &val) < 0)
    goto cleanup;
  *character_accumulate_interval = val;

  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"character_send_threshold", 
		    &val) < 0)
    goto cleanup;
  *character_send_threshold = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_sol_sol_retry (ipmi_device_t *dev, 
                   uint8_t *retry_count,
                   uint8_t *retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sol_configuration_parameters_sol_retry_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_retry (dev, 
                                                           get_lan_channel_number (), 
                                                           IPMI_GET_SOL_PARAMETER, 
                                                           SET_SELECTOR, 
                                                           BLOCK_SELECTOR, 
                                                           obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"retry_count", 
		    &val) < 0)
    goto cleanup;
  *retry_count = val;

  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"retry_interval", 
		    &val) < 0)
    goto cleanup;
  *retry_interval = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_sol_sol_non_volatile_bit_rate (ipmi_device_t *dev, 
                                   uint8_t *bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate (dev, 
                                                           get_lan_channel_number (), 
                                                           IPMI_GET_SOL_PARAMETER, 
                                                           SET_SELECTOR, 
                                                           BLOCK_SELECTOR, 
                                                           obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"bit_rate", 
		    &val) < 0)
    goto cleanup;
  *bit_rate = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_sol_sol_volatile_bit_rate (ipmi_device_t *dev, 
                               uint8_t *bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sol_configuration_parameters_sol_volatile_bit_rate_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate (dev, 
                                                                       get_lan_channel_number (), 
                                                                       IPMI_GET_SOL_PARAMETER, 
                                                                       SET_SELECTOR, 
                                                                       BLOCK_SELECTOR, 
                                                                       obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"bit_rate", 
		    &val) < 0)
    goto cleanup;
  *bit_rate = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int8_t 
get_sol_sol_payload_port_number (ipmi_device_t *dev, 
                                 uint16_t *port_number)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t ls_val, ms_val;
  uint8_t *ptr;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sol_configuration_parameters_sol_payload_port_number_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_port_number (dev, 
                                                                         get_lan_channel_number (), 
                                                                         IPMI_GET_SOL_PARAMETER, 
                                                                         SET_SELECTOR, 
                                                                         BLOCK_SELECTOR, 
                                                                         obj_cmd_rs) != 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"port_number_ls_byte", 
		    &ls_val) < 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"port_number_ms_byte", 
		    &ms_val) < 0)
    goto cleanup;
  
  ptr = (uint8_t *)port_number;
#if WORDS_BIGENDIAN
  ptr[1] = ls_val;
  ptr[0] = ms_val;
#else
  ptr[0] = ls_val;
  ptr[1] = ms_val;
#endif
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

/***********************************************************/
int8_t 
check_bmc_user_password (ipmi_device_t *dev, 
			 uint8_t userid, 
			 uint8_t *password)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_set_user_password_rs)))
    goto cleanup;

  if (ipmi_cmd_set_user_password (dev, 
				  userid, 
				  IPMI_PASSWORD_OPERATION_TEST_PASSWORD, 
				  (char *)password, 
				  (password) ? strlen((char *)password) : 0,
				  obj_cmd_rs) != 0)
    {
      uint64_t comp_code;

      if (fiid_obj_get(obj_cmd_rs,
                       (uint8_t *)"comp_code",
                       &comp_code) < 0)
        goto cleanup;

      if (comp_code == IPMI_PASSWORD_OPERATION_TEST_FAILED)
	rv = 0; /* false */
      else
	goto cleanup;; /* error */
    }
  
  rv = 1; /* true */
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}
