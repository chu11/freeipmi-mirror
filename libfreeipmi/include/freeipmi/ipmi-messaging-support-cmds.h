/*
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

#ifndef _IPMI_MESSAGING_SUPPORT_CMDS_H
#define	_IPMI_MESSAGING_SUPPORT_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid.h>

#define IPMI_MAX_USER_NAME_LENGTH           16
#define IPMI_1_5_MAX_PASSWORD_LENGTH        16
#define IPMI_2_0_MAX_PASSWORD_LENGTH        20
#define IPMI_MAX_PASSWORD_LENGTH            IPMI_2_0_MAX_PASSWORD_LENGTH
#define IPMI_CHALLENGE_STRING_LENGTH        16
#define IPMI_MAX_K_R_LENGTH                 20
#define IPMI_MAX_K_G_LENGTH                 20

#define IPMI_GET_IPMI_V20_EXTENDED_DATA     0x01
#define IPMI_GET_IPMI_V15_DATA              0x00

#define IPMI_GET_IPMI_DATA_VALID(__val) \
        (((__val) == IPMI_GET_IPMI_V20_EXTENDED_DATA \
	  || (__val) == IPMI_GET_IPMI_V15_DATA) ? 1 : 0)

#define IPMI_LIST_ALGORITHMS_BY_CIPHER_SUITE 0x1
#define IPMI_LIST_SUPPORTED_ALGORITHMS       0x0

#define IPMI_LIST_ALGORITHM_TYPE_VALID(__val) \
        (((__val) == IPMI_LIST_ALGORITHMS_BY_CIPHER_SUITE \
          || (__val) == IPMI_LIST_SUPPORTED_ALGORITHMS) ? 1 : 0)

#define IPMI_MESSAGING_ACCESS_MODE_DISABLED            0x0
#define IPMI_MESSAGING_ACCESS_MODE_PRE_BOOT_ONLY       0x1
#define IPMI_MESSAGING_ACCESS_MODE_ALWAYS_AVAILABLE    0x2
#define IPMI_MESSAGING_ACCESS_MODE_SHARED              0x3

#define IPMI_MESSAGING_ACCESS_MODE_VALID(__access_mode) \
        (((__access_mode) == IPMI_MESSAGING_ACCESS_MODE_DISABLED \
          || (__access_mode) == IPMI_MESSAGING_ACCESS_MODE_PRE_BOOT_ONLY \
          || (__access_mode) == IPMI_MESSAGING_ACCESS_MODE_ALWAYS_AVAILABLE \
          || (__access_mode) == IPMI_MESSAGING_ACCESS_MODE_SHARED) ? 1 : 0)

#define IPMI_USER_LEVEL_AUTHENTICATION_ENABLE     0x0
#define IPMI_USER_LEVEL_AUTHENTICATION_DISABLE    0x1

#define IPMI_USER_LEVEL_AUTHENTICATION_VALID(__val) \
        (((__val) == IPMI_USER_LEVEL_AUTHENTICATION_ENABLE \
          || (__val) == IPMI_USER_LEVEL_AUTHENTICATION_DISABLE) ? 1 : 0)

#define IPMI_PER_MESSAGE_AUTHENTICATION_ENABLE     0x0
#define IPMI_PER_MESSAGE_AUTHENTICATION_DISABLE    0x1

#define IPMI_PER_MESSAGE_AUTHENTICATION_VALID(__val) \
        (((__val) == IPMI_PER_MESSAGE_AUTHENTICATION_ENABLE \
          || (__val) == IPMI_PER_MESSAGE_AUTHENTICATION_DISABLE) ? 1 : 0)

#define IPMI_PEF_ALERTING_ENABLE     0x0
#define IPMI_PEF_ALERTING_DISABLE    0x1

#define IPMI_PEF_ALERTING_VALID(__val) \
        (((__val) == IPMI_PEF_ALERTING_ENABLE \
          || (__val) == IPMI_PEF_ALERTING_DISABLE) ? 1 : 0)

#define IPMI_CHANNEL_ACCESS_NO_CHANGE           0x0
#define IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE    0x1
#define IPMI_CHANNEL_ACCESS_SET_VOLATILE        0x2
#define IPMI_CHANNEL_ACCESS_RESERVED            0x3

#define IPMI_CHANNEL_ACCESS_VALID(__access_mode) \
        (((__access_mode) == IPMI_CHANNEL_ACCESS_NO_CHANGE \
          || (__access_mode) == IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE \
          || (__access_mode) == IPMI_CHANNEL_ACCESS_SET_VOLATILE) ? 1 : 0)

#define IPMI_PRIVILEGE_LEVEL_LIMIT_NO_CHANGE           0x0
#define IPMI_PRIVILEGE_LEVEL_LIMIT_SET_NON_VOLATILE    0x1
#define IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VOLATILE        0x2
#define IPMI_PRIVILEGE_LEVEL_LIMIT_RESERVED            0x3

#define IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VALID(__privilege_level_limit) \
        (((__privilege_level_limit) == IPMI_PRIVILEGE_LEVEL_LIMIT_NO_CHANGE \
          || (__privilege_level_limit) == IPMI_PRIVILEGE_LEVEL_LIMIT_SET_NON_VOLATILE \
          || (__privilege_level_limit) == IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VOLATILE) ? 1 : 0)

#define IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE    0x1
#define IPMI_CHANNEL_ACCESS_GET_VOLATILE        0x2

#define IPMI_CHANNEL_ACCESS_GET_VALID(__val) \
        (((__val) == IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE \
          || (__val) == IPMI_CHANNEL_ACCESS_GET_VOLATILE) ? 1 : 0)
  
#define IPMI_USER_RESTRICTED_TO_CALLBACK_ENABLE     0x1
#define IPMI_USER_RESTRICTED_TO_CALLBACK_DISABLE    0x0

#define IPMI_USER_RESTRICTED_TO_CALLBACK_VALID(__val) \
        (((__val) == IPMI_USER_RESTRICTED_TO_CALLBACK_ENABLE \
          || (__val) == IPMI_USER_RESTRICTED_TO_CALLBACK_DISABLE) ? 1 : 0)

#define IPMI_PRIVILEGE_LEVEL_LIMIT_VALID(__privilege_level) \
        (((__privilege_level) == IPMI_PRIVILEGE_LEVEL_CALLBACK \
          || (__privilege_level) == IPMI_PRIVILEGE_LEVEL_USER \
          || (__privilege_level) == IPMI_PRIVILEGE_LEVEL_OPERATOR \
          || (__privilege_level) == IPMI_PRIVILEGE_LEVEL_ADMIN \
          || (__privilege_level) == IPMI_PRIVILEGE_LEVEL_OEM \
	  || (__privilege_level) == IPMI_PRIVILEGE_LEVEL_NO_ACCESS) ? 1 : 0)

#define IPMI_USER_LINK_AUTHENTICATION_ENABLE     0x1
#define IPMI_USER_LINK_AUTHENTICATION_DISABLE    0x0

#define IPMI_USER_LINK_AUTHENTICATION_VALID(__val) \
        (((__val) == IPMI_USER_LINK_AUTHENTICATION_ENABLE \
          || (__val) == IPMI_USER_LINK_AUTHENTICATION_DISABLE) ? 1 : 0)

#define IPMI_USER_IPMI_MESSAGING_ENABLE     0x1
#define IPMI_USER_IPMI_MESSAGING_DISABLE    0x0

#define IPMI_USER_IPMI_MESSAGING_VALID(__val) \
        (((__val) == IPMI_USER_IPMI_MESSAGING_ENABLE \
          || (__val) == IPMI_USER_IPMI_MESSAGING_DISABLE) ? 1 : 0)

#define IPMI_CHANNEL_SECURITY_KEYS_OPERATION_READ_KEY 0x0
#define IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY  0x1
#define IPMI_CHANNEL_SECURITY_KEYS_OPERATION_LOCK_KEY 0x2

#define IPMI_CHANNEL_SECURITY_KEYS_OPERATION_VALID(__val) \
        (((__val) == IPMI_CHANNEL_SECURITY_KEYS_OPERATION_READ_KEY \
          || (__val) == IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY \
          || (__val) == IPMI_CHANNEL_SECURITY_KEYS_OPERATION_LOCK_KEY) ? 1 : 0)

#define IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R    0x0
#define IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G    0x1

#define IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_VALID(__val) \
        (((__val) == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R \
          || (__val) == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G) ? 1 : 0)

#define IPMI_CHANNEL_SECURITY_KEYS_LOCK_STATUS_KEY_IS_NOT_LOCKABLE 0x0
#define IPMI_CHANNEL_SECURITY_KEYS_LOCK_STATUS_KEY_IS_LOCKED       0x1
#define IPMI_CHANNEL_SECURITY_KEYS_LOCK_STATUS_KEY_IS_UNLOCKED     0x2

#define IPMI_PASSWORD_OPERATION_DISABLE_USER     0x0
#define IPMI_PASSWORD_OPERATION_ENABLE_USER      0x1
#define IPMI_PASSWORD_OPERATION_SET_PASSWORD     0x2
#define IPMI_PASSWORD_OPERATION_TEST_PASSWORD    0x3

#define IPMI_PASSWORD_OPERATION_VALID(__operation) \
        (((__operation) == IPMI_PASSWORD_OPERATION_DISABLE_USER \
	  || (__operation) == IPMI_PASSWORD_OPERATION_ENABLE_USER \
	  || (__operation) == IPMI_PASSWORD_OPERATION_SET_PASSWORD \
	  || (__operation) == IPMI_PASSWORD_OPERATION_TEST_PASSWORD) ? 1 : 0)

#define IPMI_PASSWORD_SIZE_16_BYTES 0x0
#define IPMI_PASSWORD_SIZE_20_BYTES 0x1

#define IPMI_PASSWORD_SIZE_VALID(__password_size) \
        (((__password_size) == IPMI_PASSWORD_SIZE_16_BYTES \
          || (__password_size) == IPMI_PASSWORD_SIZE_20_BYTES) ? 1 : 0)

#define IPMI_USER_ID_ENABLE_STATUS_UNSPECIFIED 0x0
#define IPMI_USER_ID_ENABLE_STATUS_ENABLED     0x1
#define IPMI_USER_ID_ENABLE_STATUS_DISABLED    0x2
  
extern fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rq;
extern fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rs;
extern fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_v20_rq;
extern fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_v20_rs;

extern fiid_template_t tmpl_cmd_get_channel_cipher_suites_rq;
extern fiid_template_t tmpl_cmd_get_channel_cipher_suites_rs;
extern fiid_template_t tmpl_cmd_get_channel_cipher_suites_list_supported_algorithms_rs;
extern fiid_template_t tmpl_cmd_get_channel_cipher_suites_rs;

extern fiid_template_t tmpl_cmd_get_session_challenge_rq;
extern fiid_template_t tmpl_cmd_get_session_challenge_rs;
extern fiid_template_t tmpl_cmd_activate_session_rq;
extern fiid_template_t tmpl_cmd_activate_session_rs;
extern fiid_template_t tmpl_cmd_set_session_privilege_level_rq;
extern fiid_template_t tmpl_cmd_set_session_privilege_level_rs;
extern fiid_template_t tmpl_cmd_close_session_rq;
extern fiid_template_t tmpl_cmd_close_session_rs;

extern fiid_template_t tmpl_cmd_set_channel_access_rq;
extern fiid_template_t tmpl_cmd_set_channel_access_rs;
extern fiid_template_t tmpl_cmd_get_channel_access_rq;
extern fiid_template_t tmpl_cmd_get_channel_access_rs;
extern fiid_template_t tmpl_cmd_get_channel_info_rq;
extern fiid_template_t tmpl_cmd_get_channel_info_rs;

extern fiid_template_t tmpl_cmd_set_channel_security_keys_rq;
extern fiid_template_t tmpl_cmd_set_channel_security_keys_rs;
extern fiid_template_t tmpl_cmd_set_user_access_rq;
extern fiid_template_t tmpl_cmd_set_user_access_rs;
extern fiid_template_t tmpl_cmd_get_user_access_rq;
extern fiid_template_t tmpl_cmd_get_user_access_rs;

extern fiid_template_t tmpl_cmd_set_user_name_rq;
extern fiid_template_t tmpl_cmd_set_user_name_rs;

extern fiid_template_t tmpl_cmd_get_user_name_rq;
extern fiid_template_t tmpl_cmd_get_user_name_rs;

extern fiid_template_t tmpl_cmd_set_user_password_rq;
extern fiid_template_t tmpl_cmd_set_user_password_v20_rq;
extern fiid_template_t tmpl_cmd_set_user_password_rs;

int8_t fill_cmd_get_channel_authentication_capabilities (uint8_t channel_number,
                                                         uint8_t maximum_privilege_level, 
                                                         fiid_obj_t obj_cmd_rq);
  
int8_t fill_cmd_get_channel_authentication_capabilities_v20 (uint8_t channel_number,
							     uint8_t maximum_privilege_level,
							     uint8_t get_ipmi_v20_extended_data,
							     fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_channel_cipher_suites (uint8_t channel_number,
                                           uint8_t payload_type,
                                           uint8_t list_index,
                                           uint8_t list_algorithm_type,
                                           fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_session_challenge (uint8_t authentication_type, 
				       char *user_name, 
				       uint32_t user_name_len, 
				       fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_activate_session (uint8_t authentication_type, 
				  uint8_t maximum_privilege_level, 
				  uint8_t *challenge_string, 
				  uint32_t challenge_string_len, 
				  uint32_t initial_outbound_sequence_number, 
				  fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_session_privilege_level (uint8_t privilege_level, 
                                             fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_close_session (uint32_t close_session_id, 
			       fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_channel_access (uint8_t channel_number, 
                                    uint8_t ipmi_messaging_access_mode, 
                                    uint8_t user_level_authentication, 
                                    uint8_t per_message_authentication, 
                                    uint8_t pef_alerting, 
                                    uint8_t channel_access_set, 
                                    uint8_t channel_privilege_level_limit, 
                                    uint8_t channel_privilege_level_limit_set,
                                    fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_channel_access (uint8_t channel_number,
                                    uint8_t channel_access_get,
                                    fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_channel_info (uint8_t channel_number, fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_channel_security_keys(uint8_t channel_number,
                                          uint8_t operation,
                                          uint8_t key_id,
                                          uint8_t *key_value,
                                          uint32_t key_value_len,
                                          fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_user_access (uint8_t channel_number,
                                 uint8_t user_ipmi_messaging,
                                 uint8_t user_link_authentication,
                                 uint8_t user_restricted_to_callback,
                                 uint8_t user_id,
                                 uint8_t user_privilege_level_limit,
                                 uint8_t user_session_number_limit,
                                 fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_user_access (uint8_t channel_number,
                                 uint8_t user_id,
                                 fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_user_name (uint8_t user_id, 
                               char *user_name,
                               unsigned int user_name_len,
                               fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_user_name (uint8_t user_id, fiid_obj_t obj_cmd_rq);
       
int8_t fill_cmd_set_user_password (uint8_t user_id, 
                                   uint8_t operation, 
                                   char *password,
                                   unsigned int password_len,
                                   fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_user_password_v20 (uint8_t user_id,
                                       uint8_t password_size,
                                       uint8_t operation,
                                       char *password,
                                       unsigned int password_len,
                                       fiid_obj_t obj_cmd_rq);

      
#ifdef __cplusplus
}
#endif

#endif /* ipmi-messaging-support-cmds.h */


