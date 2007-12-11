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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif	/* !TIME_WITH_SYS_TIME */

#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/ipmi-messaging-support-cmds.h"
#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-channel-spec.h"
#include "freeipmi/ipmi-comp-code-spec.h"
#include "freeipmi/ipmi-device-global-cmds.h"
#include "freeipmi/ipmi-ipmb-interface.h"
#include "freeipmi/ipmi-netfn-spec.h"
#include "freeipmi/ipmi-privilege-level-spec.h"
#include "freeipmi/api/ipmi-device-global-cmds-api.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cmd_get_channel_authentication_capabilities (ipmi_ctx_t ctx, 
                                                  uint8_t channel_number,
                                                  uint8_t maximum_privilege_level,
						  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_channel_authentication_capabilities_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_channel_authentication_capabilities (channel_number,
                                                                       maximum_privilege_level,
                                                                       obj_cmd_rq) < 0));
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
                       
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_channel_authentication_capabilities_v20 (ipmi_ctx_t ctx, 
                                                      uint8_t channel_number,
                                                      uint8_t maximum_privilege_level,
                                                      uint8_t get_ipmi_v20_extended_data,
                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
                      && IPMI_GET_IPMI_DATA_VALID(get_ipmi_v20_extended_data)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_channel_authentication_capabilities_v20_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_v20_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_channel_authentication_capabilities_v20 (channel_number,
                                                                           maximum_privilege_level,
                                                                           get_ipmi_v20_extended_data,
                                                                           obj_cmd_rq) < 0));
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
                       
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_session_challenge (ipmi_ctx_t ctx, 
                                uint8_t authentication_type,
                                char *user_name,
                                uint32_t user_name_len,
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
                      && !(user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_session_challenge_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_session_challenge_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_session_challenge (authentication_type, 
						     user_name,
                                                     user_name_len,
						     obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_activate_session (ipmi_ctx_t ctx, 
                           uint8_t authentication_type,
                           uint8_t maximum_privilege_level,
                           uint8_t *challenge_string,
                           uint32_t challenge_string_len,
                           uint32_t initial_outbound_sequence_number,
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
                      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
                      && challenge_string
                      && !(challenge_string_len > IPMI_CHALLENGE_STRING_LENGTH)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_activate_session_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_activate_session_rq);

  API_ERR_CLEANUP (!(fill_cmd_activate_session (authentication_type,
                                                maximum_privilege_level,
                                                challenge_string,
                                                challenge_string_len,
                                                initial_outbound_sequence_number,
						obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_session_privilege_level (ipmi_ctx_t ctx, 
                                      uint8_t privilege_level,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_PRIVILEGE_LEVEL_VALID(privilege_level)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_session_privilege_level_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_session_privilege_level_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_session_privilege_level (privilege_level, 
							   obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs); 
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_close_session (ipmi_ctx_t ctx, 
                        uint32_t close_session_id,
			fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_close_session_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_close_session_rq);

  API_ERR_CLEANUP (!(fill_cmd_close_session (close_session_id, obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx,
			    IPMI_BMC_IPMB_LUN_BMC,
			    IPMI_NET_FN_APP_RQ,
			    obj_cmd_rq,
			    obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_channel_access (ipmi_ctx_t ctx, 
			     uint8_t channel_number, 
			     uint8_t ipmi_messaging_access_mode, 
			     uint8_t user_level_authentication, 
			     uint8_t per_message_authentication, 
			     uint8_t pef_alerting, 
			     uint8_t channel_access_set, 
			     uint8_t channel_privilege_level_limit, 
			     uint8_t channel_privilege_level_limit_set, 
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_MESSAGING_ACCESS_MODE_VALID(ipmi_messaging_access_mode)
                      && IPMI_USER_LEVEL_AUTHENTICATION_VALID(user_level_authentication)
                      && IPMI_PER_MESSAGE_AUTHENTICATION_VALID(per_message_authentication)
                      && IPMI_PEF_ALERTING_VALID(pef_alerting)
                      && IPMI_CHANNEL_ACCESS_VALID(channel_access_set)
                      && IPMI_PRIVILEGE_LEVEL_LIMIT_VALID(channel_privilege_level_limit)
                      && IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VALID(channel_privilege_level_limit_set)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_channel_access_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_channel_access_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_channel_access (channel_number, 
						  ipmi_messaging_access_mode, 
						  user_level_authentication, 
						  per_message_authentication, 
						  pef_alerting, 
						  channel_access_set, 
						  channel_privilege_level_limit, 
						  channel_privilege_level_limit_set,
						  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_channel_access (ipmi_ctx_t ctx, 
			     uint8_t channel_number,
			     uint8_t channel_access_get,
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_CHANNEL_ACCESS_GET_VALID(channel_access_get)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_channel_access_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_channel_access_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_channel_access (channel_number, 
						  channel_access_get,
						  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_channel_info (ipmi_ctx_t ctx, 
			   uint8_t channel_number,
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_channel_info_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_channel_info_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_channel_info (channel_number, obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx,
			    IPMI_BMC_IPMB_LUN_BMC,
			    IPMI_NET_FN_APP_RQ,
			    obj_cmd_rq,
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_cmd_set_channel_security_keys (ipmi_ctx_t ctx,
                                    uint8_t channel_number,
                                    uint8_t operation,
                                    uint8_t key_id,
                                    uint8_t *key_value,
                                    uint32_t key_value_len,
                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_CHANNEL_SECURITY_KEYS_OPERATION_VALID(operation)
                      && IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_VALID(key_id)
                      && !((key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R
                            && key_value)
                           && key_value_len > IPMI_MAX_K_R_LENGTH)
                      && !((key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G
                            && key_value)
                           && key_value_len > IPMI_MAX_K_G_LENGTH)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_channel_security_keys_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_channel_security_keys_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_channel_security_keys (channel_number,
							 operation,
							 key_id,
							 key_value,
							 key_value_len,
							 obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_user_access (ipmi_ctx_t ctx, 
			  uint8_t channel_number,
			  uint8_t user_ipmi_messaging,
			  uint8_t user_link_authentication,
			  uint8_t user_restricted_to_callback,
			  uint8_t user_id,
			  uint8_t user_privilege_level_limit,
			  uint8_t user_session_number_limit, 
			  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_USER_IPMI_MESSAGING_VALID(user_ipmi_messaging)
                      && IPMI_USER_LINK_AUTHENTICATION_VALID(user_link_authentication)
                      && IPMI_USER_RESTRICTED_TO_CALLBACK_VALID(user_restricted_to_callback)
                      && IPMI_PRIVILEGE_LEVEL_LIMIT_VALID(user_privilege_level_limit)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_user_access_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_user_access_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_user_access (channel_number,
					       user_ipmi_messaging,
					       user_link_authentication,
					       user_restricted_to_callback,
					       user_id,
					       user_privilege_level_limit,
					       user_session_number_limit,
					       obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_user_access (ipmi_ctx_t ctx, 
			  uint8_t channel_number,
			  uint8_t user_id,
			  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_user_access_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_user_access_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_user_access (channel_number, user_id, obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_user_name (ipmi_ctx_t ctx, 
			uint8_t user_id, 
			char *user_name, 
			unsigned int user_name_len,
			fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (!(user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_user_name_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_user_name_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_user_name (user_id, 
					     user_name, 
					     user_name_len,
					     obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_user_name (ipmi_ctx_t ctx, 
			uint8_t user_id, 
			fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_user_name_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_user_name_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_user_name (user_id, obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_user_password (ipmi_ctx_t ctx, 
			    uint8_t user_id, 
			    uint8_t operation, 
			    char *password,
			    unsigned int password_len,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_PASSWORD_OPERATION_VALID(operation)
                      && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_user_password_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_user_password_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_user_password (user_id, 
						 operation, 
						 password, 
						 password_len,
						 obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_user_password_v20 (ipmi_ctx_t ctx, 
                                uint8_t user_id, 
                                uint8_t password_size,
                                uint8_t operation, 
                                char *password,
                                unsigned int password_len,
                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_PASSWORD_OPERATION_VALID(operation)
                      && IPMI_PASSWORD_SIZE_VALID(password_size)
                      && !(password
                           && password_size == IPMI_PASSWORD_SIZE_16_BYTES
                           && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
                      && !(password
                           && password_size == IPMI_PASSWORD_SIZE_20_BYTES
                           && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_user_password_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_user_password_v20_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_user_password_v20 (user_id, 
						     password_size,
						     operation, 
						     password, 
						     password_len,
						     obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_APP_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_get_channel_number (ipmi_ctx_t ctx, uint8_t channel_medium_type)
{
  fiid_obj_t obj_data_rs = NULL;
  uint64_t manufacturer_id, product_id;
  int8_t rv = -1;
  uint64_t val;
  int i;
    
  /* XXX channel medium type check? */
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
    {
      API_FIID_OBJ_CREATE_CLEANUP(obj_data_rs, tmpl_cmd_get_device_id_rs);
      
      if (ipmi_cmd_get_device_id (ctx, obj_data_rs) < 0)
	goto cleanup;
      
      API_FIID_OBJ_GET_CLEANUP (obj_data_rs, "manufacturer_id.id", &manufacturer_id);

      API_FIID_OBJ_GET_CLEANUP (obj_data_rs, "product_id", &product_id);
      
      switch (manufacturer_id)
	{
	case IPMI_MANUFACTURER_ID_INTEL:
	case 0xB000157: // Intel 
	  switch (product_id)
	    {
	    case IPMI_PRODUCT_ID_SE7501WV2:
	      rv = 7;
	      goto cleanup;
	    }
	}

      API_FIID_OBJ_DESTROY(obj_data_rs);
    }
  
  API_FIID_OBJ_CREATE_CLEANUP(obj_data_rs, tmpl_cmd_get_channel_info_rs);
  
  /* Channel numbers range from 0 - 7 */
  for (i = 0; i < 8; i++)
    {
      if (ipmi_cmd_get_channel_info (ctx, i, obj_data_rs) != 0)
	continue;
	
      API_FIID_OBJ_GET_CLEANUP (obj_data_rs, "channel_medium_type", &val);
      
      if ((uint8_t) val == channel_medium_type)
	{
	  API_FIID_OBJ_GET_CLEANUP (obj_data_rs, "actual_channel_number", &val);
	  
	  rv = (int8_t) val;
	  break;
	}
    }

 cleanup:
  API_FIID_OBJ_DESTROY(obj_data_rs);
  return (rv);
}

