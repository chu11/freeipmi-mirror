/*
   ipmi-messaging-support-cmds-udm.h - IPMI UDM Message Support Commands

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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

#ifndef _IPMI_MESSAGING_SUPPORT_CMDS_UDM_H
#define	_IPMI_MESSAGING_SUPPORT_CMDS_UDM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid.h>
#include <freeipmi/udm/ipmi-udm.h>

int8_t ipmi_cmd_get_channel_authentication_capabilities (ipmi_device_t dev, 
                                                         uint8_t channel_number,
                                                         uint8_t maximum_privilege_level,
							 fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_get_channel_authentication_capabilities_v20 (ipmi_device_t dev, 
                                                             uint8_t channel_number,
                                                             uint8_t maximum_privilege_level,
                                                             uint8_t get_ipmi_v20_extended_data,
                                                             fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_get_session_challenge (ipmi_device_t dev, 
                                       uint8_t authentication_type,
                                       char *user_name,
                                       uint32_t user_name_len,
				       fiid_obj_t obj_cmd_rs);
  
int8_t ipmi_cmd_activate_session (ipmi_device_t dev, 
                                  uint8_t authentication_type,
                                  uint8_t maximum_privilege_level,
                                  uint8_t *challenge_string,
                                  uint32_t challenge_string_len,
                                  uint32_t initial_outbound_sequence_number,
				  fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_session_privilege_level (ipmi_device_t dev, 
                                             uint8_t privilege_level,
					     fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_close_session (ipmi_device_t dev, 
                               uint32_t close_session_id,
			       fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_channel_access (ipmi_device_t dev, 
				    uint8_t channel_number, 
				    uint8_t ipmi_messaging_access_mode, 
				    uint8_t user_level_authentication, 
				    uint8_t per_message_authentication, 
				    uint8_t pef_alerting, 
				    uint8_t channel_access_set, 
				    uint8_t channel_privilege_level_limit, 
				    uint8_t channel_privilege_level_limit_set, 
				    fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_get_channel_access (ipmi_device_t dev, 
				    uint8_t channel_number,
				    uint8_t channel_access_get,
				    fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_get_channel_info (ipmi_device_t dev, 
				  uint8_t channel_number,
				  fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_channel_security_keys (ipmi_device_t dev,
                                           uint8_t channel_number,
                                           uint8_t operation,
                                           uint8_t key_id,
                                           uint8_t *key_value,
                                           uint32_t key_value_len,
                                           fiid_obj_t obj_cmd_rq);

int8_t ipmi_cmd_set_user_access (ipmi_device_t dev, 
				 uint8_t channel_number,
				 uint8_t user_ipmi_messaging,
				 uint8_t user_link_authentication,
				 uint8_t user_restricted_to_callback,
				 uint8_t user_id,
				 uint8_t user_privilege_level_limit,
				 uint8_t user_session_number_limit,
				 fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_get_user_access (ipmi_device_t dev, 
				 uint8_t channel_number,
				 uint8_t user_id,
				 fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_user_name (ipmi_device_t dev, 
			       uint8_t user_id, 
			       char *user_name, 
			       unsigned int user_name_len,
			       fiid_obj_t obj_cmd_rs);
  
int8_t ipmi_cmd_get_user_name (ipmi_device_t dev, 
			       uint8_t user_id, 
			       fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_user_password (ipmi_device_t dev, 
				   uint8_t user_id, 
				   uint8_t operation, 
				   char *password,
				   unsigned int password_len,
				   fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_user_password_v20 (ipmi_device_t dev, 
                                       uint8_t user_id, 
                                       uint8_t password_size,
                                       uint8_t operation, 
                                       char *password,
                                       unsigned int password_len,
                                       fiid_obj_t obj_cmd_rs);

int8_t ipmi_get_channel_number (ipmi_device_t dev, 
				uint8_t channel_medium_type);
  
#ifdef __cplusplus
}
#endif

#endif /* _IPMI_MESSAGING_SUPPORT_CMDS_UDM_H */


