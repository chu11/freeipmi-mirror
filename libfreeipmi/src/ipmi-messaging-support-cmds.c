/* 
   ipmi-messaging-support-cmds.c - IPMI Message Support Commands

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
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

#include "freeipmi/ipmi-messaging-support-cmds.h"
#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-channel-spec.h"
#include "freeipmi/ipmi-cmd-spec.h"
#include "freeipmi/ipmi-lan.h"
#include "freeipmi/ipmi-privilege-level-spec.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rs = 
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.straight_password_key", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.oem_prop", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "authentication_type.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.anonymous_login", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.non_null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "authentication_status.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "oem_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rs =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "temp_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "initial_outbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "initial_inbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_privilege_level_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_privilege_level_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };


fiid_template_t tmpl_set_channel_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "channel_access_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "channel_privilege_level_limit_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_channel_access_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_channel_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "channel_access_get", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_channel_access_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_channel_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_channel_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "actual_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "actual_channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "channel_medium_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "channel_medium_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5,  "channel_protocol_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3,  "channel_protocol_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,  "active_session_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,  "session_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24, "vendor_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "auxiliary_channel_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_user_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "change_bits_in_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "user_session_number_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_user_access_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_user_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_user_access_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "max_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "current_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "current_channel_fixed_names", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_user_name_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_user_name_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_user_name_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_user_name_rs =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_user_password_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "operation.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {128, "password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_user_password_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };
    
int8_t 
fill_cmd_get_channel_authentication_capabilities (uint8_t channel_number,
                                                  uint8_t maximum_privilege_level, 
                                                  fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"cmd", IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_number", channel_number); 
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"maximum_privilege_level", maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved2", 0);

  return (0);
}

int8_t 
fill_cmd_get_session_challenge (uint8_t authentication_type, 
				char *user_name, 
				uint32_t user_name_len, 
				fiid_obj_t obj_cmd_rq)
{
  char buf[IPMI_MAX_USER_NAME_LENGTH];

  /* achu: user_name can be IPMI_MAX_USER_NAME_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  ERR_EINVAL (IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
	      && !(user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_session_challenge_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"cmd", IPMI_CMD_GET_SESSION_CHALLENGE);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"authentication_type", authentication_type);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved", 0);

  /* achu: user_name must be zero extended */
  memset(buf, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (user_name)
    strncpy(buf, user_name, IPMI_MAX_USER_NAME_LENGTH);
  
  FIID_OBJ_SET_DATA (obj_cmd_rq, 
		     (uint8_t *)"user_name", 
		     (uint8_t *)buf,
		     IPMI_MAX_USER_NAME_LENGTH);
  
  return (0);
}

int8_t 
fill_cmd_activate_session (uint8_t authentication_type, 
			   uint8_t maximum_privilege_level, 
			   uint8_t *challenge_string, 
			   uint32_t challenge_string_len, 
			   uint32_t initial_outbound_sequence_number, 
			   fiid_obj_t obj_cmd_rq)
{
  char buf[IPMI_CHALLENGE_STRING_LENGTH];

  ERR_EINVAL (IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
	      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
	      && challenge_string
	      && !(challenge_string_len > IPMI_CHALLENGE_STRING_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_activate_session_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"cmd", IPMI_CMD_ACTIVATE_SESSION);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"authentication_type", authentication_type);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"maximum_privilege_level", maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved2", 0);
  
  /* achu: challenge string must be zero extended */
  memset(buf, '\0', IPMI_CHALLENGE_STRING_LENGTH);
  memcpy(buf, challenge_string, challenge_string_len);
  
  FIID_OBJ_SET_DATA (obj_cmd_rq,
		     (uint8_t *)"challenge_string",
		     (uint8_t *)buf,
		     IPMI_CHALLENGE_STRING_LENGTH);

  FIID_OBJ_SET (obj_cmd_rq, 
                (uint8_t *)"initial_outbound_sequence_number", 
                initial_outbound_sequence_number);

  return (0);
}

int8_t 
fill_cmd_set_session_privilege_level (uint8_t privilege_level, 
                                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_PRIVILEGE_LEVEL_VALID(privilege_level)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_session_privilege_level_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"cmd", IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"privilege_level", privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved1", 0);
  return (0);
}  

int8_t 
fill_cmd_close_session (uint32_t close_session_id, 
			fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_close_session_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"cmd", IPMI_CMD_CLOSE_SESSION);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"session_id", close_session_id);
  return (0);
}  

int8_t 
fill_cmd_set_channel_access (uint8_t channel_number, 
			     uint8_t ipmi_messaging_access_mode, 
			     uint8_t user_level_authentication, 
			     uint8_t per_message_authentication, 
			     uint8_t pef_alerting, 
			     uint8_t channel_access_set, 
			     uint8_t channel_privilege_level_limit, 
			     uint8_t channel_privilege_level_limit_set,
                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_MESSAGING_ACCESS_MODE_VALID(ipmi_messaging_access_mode)
	      && IPMI_USER_LEVEL_AUTHENTICATION_VALID(user_level_authentication)
	      && IPMI_PER_MESSAGE_AUTHENTICATION_VALID(per_message_authentication)
	      && IPMI_PEF_ALERTING_VALID(pef_alerting)
	      && IPMI_CHANNEL_ACCESS_VALID(channel_access_set)
	      && IPMI_PRIVILEGE_LEVEL_VALID(channel_privilege_level_limit)
	      && IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VALID(channel_privilege_level_limit_set)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_set_channel_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_CHANNEL_ACCESS);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"ipmi_messaging_access_mode", 
		ipmi_messaging_access_mode);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"user_level_authentication", 
		user_level_authentication);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"per_message_authentication", 
		per_message_authentication);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"pef_alerting", pef_alerting);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_access_set", channel_access_set);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"channel_privilege_level_limit", 
		channel_privilege_level_limit);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"channel_privilege_level_limit_set", 
		channel_privilege_level_limit_set);
  return 0;
}

int8_t
fill_cmd_get_channel_access (uint8_t channel_number,
			     uint8_t channel_access_get,
                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_CHANNEL_ACCESS_GET_VALID(channel_access_get)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_get_channel_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_CHANNEL_ACCESS);  
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_access_get", channel_access_get);

  return 0;
}

int8_t 
fill_cmd_get_channel_info (uint8_t channel_number, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq)
	      && IPMI_CHANNEL_NUMBER_VALID(channel_number));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_get_channel_info_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_CHANNEL_INFO_CMD);  
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved", 0);
  
  return 0;
}

int8_t 
fill_cmd_set_user_access (uint8_t channel_number,
			  uint8_t user_ipmi_messaging,
			  uint8_t user_link_authentication,
			  uint8_t user_restricted_to_callback,
			  uint8_t user_id,
			  uint8_t user_privilege_level_limit,
			  uint8_t user_session_number_limit,
                          fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_USER_IPMI_MESSAGING_VALID(user_ipmi_messaging)
	      && IPMI_USER_LINK_AUTHENTICATION_VALID(user_link_authentication)
	      && IPMI_USER_RESTRICTED_TO_CALLBACK_VALID(user_restricted_to_callback)
	      && IPMI_PRIVILEGE_LEVEL_LIMIT_VALID(user_privilege_level_limit)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_set_user_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_ACCESS_CMD);  
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"user_ipmi_messaging", 
		user_ipmi_messaging);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"user_link_authentication", 
		user_link_authentication);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"user_restricted_to_callback", 
                user_restricted_to_callback);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"change_bits_in_byte", 1);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq,
		(uint8_t *)"user_privilege_level_limit",
		user_privilege_level_limit);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq,
		(uint8_t *)"user_session_number_limit",
		user_session_number_limit);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved3", 0);

  return 0;
}

int8_t
fill_cmd_get_user_access (uint8_t channel_number,
			  uint8_t user_id,
                          fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_get_user_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_USER_ACCESS_CMD);  
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"reserved2", 0);

  return 0;
}

int8_t 
fill_cmd_set_user_name (uint8_t user_id, 
			char *user_name,
                        unsigned int user_name_len,
                        fiid_obj_t obj_cmd_rq)
{
  char buf[IPMI_MAX_USER_NAME_LENGTH];

  /* achu: user_name can be IPMI_MAX_USER_NAME_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  ERR_EINVAL (!(user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_set_user_name_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_NAME);  
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id.reserved", 0);
  
  /* achu: user_name must be zero extended */
  memset(buf, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (user_name)
    strncpy(buf, user_name, IPMI_MAX_USER_NAME_LENGTH);
      
  FIID_OBJ_SET_DATA (obj_cmd_rq, 
		     (uint8_t *)"user_name", 
		     (uint8_t *)buf,
		     IPMI_MAX_USER_NAME_LENGTH);
  
  return 0;
}

int8_t 
fill_cmd_get_user_name (uint8_t user_id, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_get_user_name_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_USER_NAME_CMD);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id.reserved", 0);

  return 0;
}

int8_t 
fill_cmd_set_user_password (uint8_t user_id, 
			    uint8_t operation, 
			    char *password,
                            unsigned int password_len,
                            fiid_obj_t obj_cmd_rq)
{
  char buf[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];

  /* achu: password can be IPMI_MAX_AUTHENTICATION_CODE_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  ERR_EINVAL (IPMI_PASSWORD_OPERATION_VALID(operation)
	      && !(password && password_len > IPMI_MAX_AUTHENTICATION_CODE_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_set_user_password_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_PASSWORD_CMD);  
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"user_id.reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"operation", operation);
  FIID_OBJ_SET (obj_cmd_rq, (uint8_t *)"operation.reserved", 0);
  
  /* achu: password must be zero extended */
  memset(buf, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
  if (password)
    strncpy(buf, password, IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
      
  FIID_OBJ_SET_DATA (obj_cmd_rq, 
		     (uint8_t *)"password", 
		     (uint8_t *)buf,
		     IPMI_MAX_AUTHENTICATION_CODE_LENGTH);

  return 0;
}

