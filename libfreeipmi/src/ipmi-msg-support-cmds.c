/* 
   ipmi-msg-support-cmds.c - IPMI Message Support Commands

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

#include "freeipmi.h"

fiid_template_t tmpl_cmd_get_channel_auth_caps_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "max_priv_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_auth_caps_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "channel_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_type.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_type.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_type.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_type.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_type.straight_passwd_key", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_type.oem_prop", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "auth_type.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_status.anonymous_login", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_status.null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_status.non_null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_status.user_level_auth", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "auth_status.per_message_auth", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "auth_status.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "oem_aux", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "auth_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "tmp_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {128, "challenge_str", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "auth_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "max_priv_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "challenge_str", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "initial_outbound_seq_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "auth_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "initial_inbound_seq_num", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "max_priv_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_priv_level_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "priv_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_priv_level_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "new_priv_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_channel_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "channel_access_set_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

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
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

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
    {2, "channel_access_set_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "channel_privilege_level_limit_set_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0, "", 0}
  };

fiid_template_t tmpl_set_channel_access_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_user_name_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0, "", 0}
  };

fiid_template_t tmpl_set_user_name_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
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
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_set_user_password_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "operation.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {128, "password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0, "", 0}
  };

fiid_template_t tmpl_set_user_password_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}
  };

fiid_template_t tmpl_set_user_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_flags.enable_ipmi_msgs", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_flags.enable_link_auth", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_flags.restrict_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "modify_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "user_session_number_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_user_access_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_user_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_user_access_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {6, "max_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {6, "current_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {6, "current_channel_fixed_user_names", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_flags.enable_ipmi_msgs", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_flags.enable_link_auth", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_flags.restrict_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

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
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {4, "actual_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "actual_channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {7, "channel_medium_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "channel_medium_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {5, "channel_protocol_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "channel_protocol_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {6, "active_session_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "session_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {24, "vendor_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {16, "auxiliary_channel_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0, "", 0}
  };


int8_t 
fill_cmd_get_channel_auth_caps (uint8_t channel_num,
                                uint8_t max_priv_level, 
				fiid_obj_t obj_cmd)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_cmd)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_num)
      || !IPMI_PRIV_LEVEL_VALID(max_priv_level))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_get_channel_auth_caps_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_GET_CHANNEL_AUTH_CAPS);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"channel_num", channel_num); 
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"max_priv_level", max_priv_level);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved2", 0);
  return (0);
}

int8_t 
fill_cmd_get_session_challenge (uint8_t auth_type, 
				char *username, 
				uint32_t username_len, 
				fiid_obj_t obj_cmd)
{
  int8_t rv;
  char buf[IPMI_SESSION_MAX_USERNAME_LEN];

  /* achu: username can be IPMI_SESSION_MAX_USERNAME_LEN length.  Null
   * termination in IPMI packet not required
   */
  if (!fiid_obj_valid(obj_cmd)
      || !IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
      || (username && username_len > IPMI_SESSION_MAX_USERNAME_LEN))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_get_session_challenge_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_GET_SESSION_CHALLENGE);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"auth_type", auth_type);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved1", 0);

  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  ERR (!(fiid_obj_clear_field(obj_cmd, (uint8_t *)"username") < 0));

  /* achu: username must be zero extended */
  memset(buf, '\0', IPMI_SESSION_MAX_USERNAME_LEN);
  if (username)
    strncpy(buf, username, IPMI_SESSION_MAX_USERNAME_LEN);
  
  ERR (!(fiid_obj_set_data (obj_cmd, 
                            (uint8_t *)"username", 
                            (uint8_t *)buf,
                            IPMI_SESSION_MAX_USERNAME_LEN) < 0));
  
  return (0);
}

int8_t 
fill_cmd_activate_session (uint8_t auth_type, 
			   uint8_t max_priv_level, 
			   uint8_t *challenge_str, 
			   uint32_t challenge_str_len, 
			   uint32_t initial_outbound_seq_num, 
			   fiid_obj_t obj_cmd)
{
  int8_t rv;
  char buf[IPMI_SESSION_CHALLENGE_STR_LEN];

  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
      || !IPMI_PRIV_LEVEL_VALID(max_priv_level)
      || !challenge_str
      || challenge_str_len > IPMI_SESSION_CHALLENGE_STR_LEN
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_activate_session_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_ACTIVATE_SESSION);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"auth_type", auth_type);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"max_priv_level", max_priv_level);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved2", 0);
  ERR (!(fiid_obj_clear_field (obj_cmd, (uint8_t *)"challenge_str") < 0));
  
  /* achu: challenge string must be zero extended */
  memset(buf, '\0', IPMI_SESSION_CHALLENGE_STR_LEN);
  memcpy(buf, challenge_str, challenge_str_len);
  
  ERR (!(fiid_obj_set_data (obj_cmd,
                            (uint8_t *)"challenge_str",
                            (uint8_t *)buf,
                            IPMI_SESSION_CHALLENGE_STR_LEN) < 0));

  FIID_OBJ_SET (obj_cmd, 
                (uint8_t *)"initial_outbound_seq_num", 
                initial_outbound_seq_num);

  return (0);
}

int8_t 
fill_cmd_set_session_priv_level (uint8_t priv_level, 
				 fiid_obj_t obj_cmd)
{
  int8_t rv;

  if (!IPMI_PRIV_LEVEL_VALID(priv_level)
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_set_session_priv_level_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_SET_SESSION_PRIV_LEVEL);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"priv_level", priv_level);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved1", 0);
  return (0);
}  

int8_t 
fill_cmd_close_session (uint32_t close_session_id, 
			fiid_obj_t obj_cmd)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_close_session_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_CLOSE_SESSION);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"session_id", close_session_id);
  return (0);
}  

int8_t 
fill_kcs_set_channel_access (uint8_t channel_number, 
			     uint8_t ipmi_messaging_access_mode, 
			     uint8_t user_level_authentication, 
			     uint8_t per_message_authentication, 
			     uint8_t pef_alerting, 
			     uint8_t channel_access_set_flag, 
			     uint8_t channel_privilege_level_limit, 
			     uint8_t channel_privilege_level_limit_set_flag,
                             fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_channel_access_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_CHANNEL_ACCESS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved1",
		0);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"ipmi_messaging_access_mode", 
		ipmi_messaging_access_mode);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_level_authentication", 
		user_level_authentication);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"per_message_authentication", 
		per_message_authentication);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"pef_alerting", 
		pef_alerting);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_access_set_flag", 
		channel_access_set_flag);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_privilege_level_limit", 
		channel_privilege_level_limit);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved2",
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_privilege_level_limit_set_flag", 
		channel_privilege_level_limit_set_flag);
  
  return 0;
}

int8_t 
fill_kcs_set_user_name (uint8_t user_id, 
			char *user_name,
                        unsigned int user_name_len,
                        fiid_obj_t obj_data_rq)
{
  int8_t rv;
  char buf[IPMI_SESSION_MAX_USERNAME_LEN];

  /* achu: username can be IPMI_USER_NAME_MAX_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  if ((user_name && user_name_len > IPMI_USER_NAME_MAX_LENGTH)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_user_name_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_NAME);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id", 
		user_id);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id.reserved", 
                0);
  
  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  ERR (!(fiid_obj_clear_field(obj_data_rq, 
			      (uint8_t *)"user_name") < 0));

  /* achu: username must be zero extended */
  memset(buf, '\0', IPMI_SESSION_MAX_USERNAME_LEN);
  if (user_name)
    strncpy(buf, user_name, IPMI_SESSION_MAX_USERNAME_LEN);
      
  ERR (!(fiid_obj_set_data (obj_data_rq, 
                            (uint8_t *)"user_name", 
                            (uint8_t *)buf,
                            IPMI_SESSION_MAX_USERNAME_LEN) < 0));
  
  return 0;
}

int8_t 
fill_kcs_get_user_name (uint8_t user_id, fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_user_name_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_USER_NAME_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id.reserved", 
                0);

  return 0;
}

int8_t 
fill_kcs_set_user_password (uint8_t user_id, 
			    uint8_t operation, 
			    char *user_password,
                            unsigned int user_password_len,
                            fiid_obj_t obj_data_rq)
{
  int8_t rv;
  char buf[IPMI_USER_PASSWORD_MAX_LENGTH];

  /* achu: password can be IPMI_USER_PASSWORD_MAX_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  if ((user_password && user_password_len > IPMI_USER_PASSWORD_MAX_LENGTH)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_user_password_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_PASSWORD_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id.reserved", 
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"operation", 
		operation);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"operation.reserved", 
		0);
  
  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  ERR (!(fiid_obj_clear_field(obj_data_rq, 
			      (uint8_t *)"password") < 0));

  /* achu: password must be zero extended */
  memset(buf, '\0', IPMI_USER_PASSWORD_MAX_LENGTH);
  if (user_password)
    strncpy(buf, user_password, IPMI_USER_PASSWORD_MAX_LENGTH);
      
  ERR (!(fiid_obj_set_data (obj_data_rq, 
                            (uint8_t *)"password", 
                            (uint8_t *)buf,
                            IPMI_USER_PASSWORD_MAX_LENGTH) < 0));

  return 0;
}

int8_t 
fill_kcs_set_user_access (uint8_t channel_number,
			  uint8_t user_id,
			  uint8_t restrict_to_callback,
			  uint8_t enable_link_auth,
			  uint8_t enable_ipmi_msgs,
			  uint8_t user_privilege_level_limit,
			  uint8_t user_session_number_limit,
                          fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_user_access_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_ACCESS_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_flags.enable_ipmi_msgs", 
		enable_ipmi_msgs);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_flags.enable_link_auth", 
		enable_link_auth);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_flags.restrict_to_callback", 
		restrict_to_callback);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"modify_flag", 
		1);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"user_privilege_level_limit",
		user_privilege_level_limit);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);
 
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"user_session_number_limit",
		user_session_number_limit);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved3",
                0);

  return 0;
}

int8_t
fill_kcs_get_user_access (uint8_t channel_number,
			  uint8_t user_id,
                          fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_user_access_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_USER_ACCESS_CMD);
  
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"channel_number",
		channel_number);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  return 0;
}

int8_t
fill_kcs_get_channel_access (uint8_t channel_number,
			     uint8_t channel_access_set_flag,
                             fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_channel_access_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_CHANNEL_ACCESS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"channel_access_set_flag",
		channel_access_set_flag);

  return 0;
}

int8_t 
fill_kcs_get_channel_info (uint8_t channel_number, fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_channel_info_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_CHANNEL_INFO_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved",
		0);
  
  return 0;
}

int8_t
ipmi_check_cmd(fiid_obj_t obj_cmd, uint8_t cmd)
{
  uint64_t cmd_recv;
  int8_t rv;

  if (!obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_field_lookup (obj_cmd, (uint8_t *)"cmd")) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, (uint8_t *)"cmd", &cmd_recv);

  return ((((uint8_t)cmd_recv) == cmd) ? 1 : 0);
}

int8_t
ipmi_check_comp_code(fiid_obj_t obj_cmd, uint8_t comp_code)
{
  uint64_t comp_code_recv;
  int8_t rv;

  if (!obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_field_lookup (obj_cmd, (uint8_t *)"comp_code")) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, (uint8_t *)"comp_code", &comp_code_recv);

  return ((((uint8_t)comp_code_recv) == comp_code) ? 1 : 0);
}

