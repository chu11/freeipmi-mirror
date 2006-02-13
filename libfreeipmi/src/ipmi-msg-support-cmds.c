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
    {8, "cmd"},
    {4, "channel_num"},
    {4, "reserved1"},
    {4, "max_priv_level"},
    {4, "reserved2"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_channel_auth_caps_rs = 
  {
    {8, "cmd"},
    {8, "comp_code"},
    {8, "channel_num"},
    {1, "auth_type.none"},
    {1, "auth_type.md2"},
    {1, "auth_type.md5"},
    {1, "auth_type.reserved1"},
    {1, "auth_type.straight_passwd_key"},
    {1, "auth_type.oem_prop"},
    {2, "auth_type.reserved2"},
    {1, "auth_status.anonymous_login"},
    {1, "auth_status.null_username"},
    {1, "auth_status.non_null_username"},
    {1, "auth_status.user_level_auth"},
    {1, "auth_status.per_message_auth"},
    {3, "auth_status.reserved"},
    {8, "reserved1"},
    {24, "oem_id"},
    {8, "oem_aux"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_channel_auth_caps_v20_rq =
  {
    {8, "cmd"},
    {4, "channel_num"},
    {3, "reserved1"},
    {1, "get_ipmi_v2.0_extended_data"},
    {4, "max_priv_level"},
    {4, "reserved2"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_channel_auth_caps_v20_rs = 
  {
    {8, "cmd"},
    {8, "comp_code"},
    {8, "channel_num"},
    {1, "auth_type.none"},
    {1, "auth_type.md2"},
    {1, "auth_type.md5"},
    {1, "auth_type.reserved1"},
    {1, "auth_type.straight_passwd_key"},
    {1, "auth_type.oem_prop"},
    {1, "auth_type.ipmi_v2.0_extended_capabilities_available"},
    {1, "auth_type.reserved2"},
    {1, "auth_status.anonymous_login"},
    {1, "auth_status.null_username"},
    {1, "auth_status.non_null_username"},
    {1, "auth_status.user_level_auth"},
    {1, "auth_status.per_message_auth"},
    {3, "auth_status.reserved"},
    {1, "channel_supports_ipmi_v1.5_connections"},
    {1, "channel_supports_ipmi_v2.0_connections"},
    {6, "reserved1"},
    {24, "oem_id"},
    {8, "oem_aux"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rq =
  {
    {8, "cmd"},
    {4, "auth_type"},
    {4, "reserved1"},
    {128, "username"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {32, "tmp_session_id"}, /* LS byte first */
    {128, "challenge_str"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_activate_session_rq =
  {
    {8, "cmd"},
    {4, "auth_type"},
    {4, "reserved1"},
    {4, "max_priv_level"},
    {4, "reserved2"},
    {128, "challenge_str"},
    {32, "initial_outbound_seq_num"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_activate_session_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {4, "auth_type"},
    {4, "reserved1"},
    {32, "session_id"},
    {32, "initial_inbound_seq_num"},
    {4, "max_priv_level"},
    {4, "reserved2"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_set_session_priv_level_rq =
  {
    {8, "cmd"},
    {4, "priv_level"},
    {4, "reserved1"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_set_session_priv_level_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {4, "new_priv_level"},
    {4, "reserved1"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_close_session_rq =
  {
    {8, "cmd"},
    {32, "session_id"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_close_session_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {0, ""}
  };

fiid_template_t tmpl_get_channel_access_rq =
  {
    {8, "cmd"},

    {4, "channel_number"},
    {4, "reserved"},

    {6, "reserved2"},
    {2, "channel_access_set_flag"},

    {0, ""}
  };

fiid_template_t tmpl_get_channel_access_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {3, "ipmi_messaging_access_mode"}, 
    {1, "user_level_authentication"}, 
    {1, "per_message_authentication"}, 
    {1, "pef_alerting"}, 
    {2, "reserved"}, 

    {4, "channel_privilege_level_limit"},
    {4, "reserved2"},

    {0, ""}
  };

fiid_template_t tmpl_set_channel_access_rq =
  {
    {8, "cmd"}, 
    
    {4, "channel_number"}, 
    {4, "reserved1"}, 
    
    {3, "ipmi_messaging_access_mode"}, 
    {1, "user_level_authentication"}, 
    {1, "per_message_authentication"}, 
    {1, "pef_alerting"}, 
    {2, "channel_access_set_flag"}, 
    
    {4, "channel_privilege_level_limit"}, 
    {2, "reserved2"}, 
    {2, "channel_privilege_level_limit_set_flag"}, 
    
    {0, ""}
  };

fiid_template_t tmpl_set_channel_access_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_user_name_rq =
  {
    {8, "cmd"}, 
    
    {6, "user_id"}, 
    {2, "user_id.reserved"}, 
    
    {128, "user_name"}, 
    
    {0, ""}
  };

fiid_template_t tmpl_set_user_name_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_user_name_rq =
  {
    {8, "cmd"}, 
    
    {6, "user_id"}, 
    {2, "user_id.reserved"}, 
    
    {0, ""}
  };

fiid_template_t tmpl_get_user_name_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    
    {128, "user_name"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_set_user_password_rq =
  {
    {8, "cmd"}, 
    
    {6, "user_id"}, 
    {2, "user_id.reserved"}, 
    
    {2, "operation"}, 
    {6, "operation.reserved"}, 
    
    {128, "password"}, 
    
    {0, ""}
  };

fiid_template_t tmpl_set_user_password_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_user_access_rq =
  {
    {8, "cmd"},

    {4, "channel_number"},
    {1, "user_flags.enable_ipmi_msgs"},
    {1, "user_flags.enable_link_auth"},
    {1, "user_flags.restrict_to_callback"},
    {1, "modify_flag"},

    {6, "user_id"},
    {2, "reserved"},

    {4, "user_privilege_level_limit"},
    {4, "reserved2"},

    {4, "user_session_number_limit"},
    {4, "reserved3"},

    {0, ""}
  };

fiid_template_t tmpl_set_user_access_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_user_access_rq =
  {
    {8, "cmd"},

    {4, "channel_number"},
    {4, "reserved"},

    {6, "user_id"},
    {2, "reserved2"},

    {0, ""}
  };

fiid_template_t tmpl_get_user_access_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {6, "max_channel_user_ids"},
    {2, "reserved"},

    {6, "current_channel_user_ids"},
    {2, "reserved2"},

    {6, "current_channel_fixed_user_names"},
    {2, "reserved3"},

    {4, "user_privilege_level_limit"},
    {1, "user_flags.enable_ipmi_msgs"},
    {1, "user_flags.enable_link_auth"},
    {1, "user_flags.restrict_to_callback"},
    {1, "reserved4"},

    {0, ""}
  };
    
fiid_template_t tmpl_get_channel_info_rq =
  {
    {8, "cmd"},
    
    {4, "channel_number"},
    {4, "reserved"},
    
    {0, ""}
  };

fiid_template_t tmpl_get_channel_info_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    
    {4, "actual_channel_number"},
    {4, "actual_channel_number.reserved"},
    
    {7, "channel_medium_type"}, 
    {1, "channel_medium_type.reserved"},
    
    {5, "channel_protocol_type"}, 
    {3, "channel_protocol_type.reserved"},
    
    {6, "active_session_count"}, 
    {2, "session_support"},
    
    {24, "vendor_id"}, 
    
    {16, "auxiliary_channel_info"}, 
    
    {0, ""}
  };


int8_t 
fill_cmd_get_channel_auth_caps (uint8_t channel_num,
                                uint8_t max_priv_level, 
				fiid_obj_t obj_cmd)
{
  if (!obj_cmd 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_num)
      || !IPMI_1_5_PRIV_LEVEL_VALID(max_priv_level))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_rq, (uint8_t *)"cmd", 
		IPMI_CMD_GET_CHANNEL_AUTH_CAPS);

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_rq,
		(uint8_t *)"channel_num", channel_num); 

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_rq, (uint8_t *)"max_priv_level", 
		max_priv_level);
  return (0);
}

int8_t 
fill_cmd_get_channel_auth_caps_v20 (uint8_t channel_num,
                                    uint8_t max_priv_level, 
                                    uint8_t get_ipmi_v20_extended_data,
                                    fiid_obj_t obj_cmd)
{
  if (!obj_cmd 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_num)
      || !IPMI_PRIV_LEVEL_VALID(max_priv_level))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_v20_rq, "cmd", 
		IPMI_CMD_GET_CHANNEL_AUTH_CAPS);
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_v20_rq,
		"channel_num", channel_num); 
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_v20_rq, "max_priv_level", 
		max_priv_level);

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_v20_rq, 
                "get_ipmi_v2.0_extended_data", 
		get_ipmi_v20_extended_data);
  
  return (0);
}

int8_t 
ipmi_lan_get_channel_auth_caps (int sockfd, 
				struct sockaddr *hostaddr, 
				size_t hostaddr_len, 
				uint8_t rq_seq, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq;

  if (!(hostaddr && sockfd && hostaddr_len && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  obj_cmd_rq = alloca (fiid_obj_len_bytes (tmpl_cmd_get_channel_auth_caps_rq));
  memset (obj_cmd_rq, 0, fiid_obj_len_bytes (tmpl_cmd_get_channel_auth_caps_rq));
  ERR (obj_cmd_rq);

  ERR (fill_cmd_get_channel_auth_caps (IPMI_CHANNEL_CURRENT_CHANNEL, 
                                       IPMI_PRIV_LEVEL_USER, 
                                       obj_cmd_rq) != -1);

  ERR (ipmi_lan_cmd (sockfd, hostaddr, hostaddr_len, IPMI_SESSION_AUTH_TYPE_NONE,
		     0, 0, NULL, 0, IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, rq_seq,
		     obj_cmd_rq, tmpl_cmd_get_channel_auth_caps_rq,
		     obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs) != -1);
  
  /* INFO: you can also do auth type check here */
  return (0);
}

int8_t 
fill_cmd_get_session_challenge (uint8_t auth_type, 
				char *username, 
				uint32_t username_len, 
				fiid_obj_t obj_cmd)
{
  /* achu: username can be IPMI_SESSION_MAX_USERNAME_LEN length.  Null
   * termination in IPMI packet not required
   */
  if (!obj_cmd 
      || !IPMI_1_5_SESSION_AUTH_TYPE_VALID(auth_type)
      || (username && username_len > IPMI_SESSION_MAX_USERNAME_LEN))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_session_challenge_rq, (uint8_t *)"cmd", 
		IPMI_CMD_GET_SESSION_CHALLENGE);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_session_challenge_rq, (uint8_t *)"auth_type",
		auth_type);

  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  ERR_EXIT (fiid_obj_memset_field(obj_cmd, '\0',
                                  tmpl_cmd_get_session_challenge_rq,
                                  (uint8_t *)"username") == 0);

  if (username)
    ERR_EXIT (fiid_obj_set_data (obj_cmd, 
                                 tmpl_cmd_get_session_challenge_rq, 
                                 (uint8_t *)"username", 
                                 (uint8_t *)username, 
                                 username_len) == 0);

  return (0);
}

int8_t 
ipmi_lan_get_session_challenge (int sockfd, 
				struct sockaddr *hostaddr, 
				size_t hostaddr_len, 
				uint8_t auth_type, 
				char *username, 
				uint8_t rq_seq, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq;

  if (!(hostaddr && sockfd && hostaddr_len && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  obj_cmd_rq = alloca (fiid_obj_len_bytes (tmpl_cmd_get_session_challenge_rq));
  ERR (obj_cmd_rq);
  memset (obj_cmd_rq, 0, fiid_obj_len_bytes (tmpl_cmd_get_session_challenge_rq));

  ERR (fill_cmd_get_session_challenge (auth_type, username, 
                                       (username) ? strlen(username) : 0, obj_cmd_rq) != -1);

  ERR (ipmi_lan_cmd (sockfd, hostaddr, hostaddr_len, IPMI_SESSION_AUTH_TYPE_NONE,
		     0, 0, NULL, 0, IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, rq_seq,
		     obj_cmd_rq, tmpl_cmd_get_session_challenge_rq,
		     obj_cmd_rs, tmpl_cmd_get_session_challenge_rs) != -1);
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
  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
      || !IPMI_1_5_PRIV_LEVEL_VALID(max_priv_level)
      || (challenge_str && challenge_str_len > IPMI_SESSION_CHALLENGE_STR_LEN)
      || !obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, (uint8_t *)"cmd", 
		IPMI_CMD_ACTIVATE_SESSION);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, (uint8_t *)"auth_type", 
		auth_type);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, (uint8_t *)"max_priv_level", 
		max_priv_level);
  ERR_EXIT (fiid_obj_memset_field (obj_cmd, '\0', 
                                   tmpl_cmd_activate_session_rq,
                                   (uint8_t *)"challenge_str") == 0);
  if (challenge_str)
    fiid_obj_set_data (obj_cmd, tmpl_cmd_activate_session_rq, 
                       (uint8_t *)"challenge_str", challenge_str, 
                       challenge_str_len);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, (uint8_t *)"initial_outbound_seq_num", 
		initial_outbound_seq_num);
  return (0);
}

int8_t 
ipmi_lan_activate_session (int sockfd, 
			   struct sockaddr *hostaddr, 
			   size_t hostaddr_len, 
			   uint8_t auth_type, 
			   uint32_t tmp_session_id, 
			   uint8_t *auth_code_data, 
			   uint32_t auth_code_data_len, 
			   uint8_t max_priv_level, 
			   uint8_t *challenge_str, 
			   uint32_t challenge_str_len, 
			   uint32_t initial_outbound_seq_num, 
			   uint8_t rq_seq, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq;
  
  if (!(hostaddr && sockfd && hostaddr_len && tmp_session_id &&
	challenge_str && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_activate_session_rq);

  ERR (fill_cmd_activate_session (auth_type, max_priv_level, challenge_str, 
				  challenge_str_len, initial_outbound_seq_num,
				  obj_cmd_rq) != -1);

  ERR (ipmi_lan_cmd (sockfd, hostaddr, hostaddr_len, auth_type, 
		     0, tmp_session_id, auth_code_data, auth_code_data_len,
		     IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, rq_seq, 
		     obj_cmd_rq, tmpl_cmd_activate_session_rq,
		     obj_cmd_rs, tmpl_cmd_activate_session_rs) != -1);
  return (0);
}

int8_t 
fill_cmd_set_session_priv_level (uint8_t priv_level, 
				 fiid_obj_t obj_cmd)
{
  if (!IPMI_1_5_PRIV_LEVEL_VALID(priv_level)
      || obj_cmd == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_session_priv_level_rq, (uint8_t *)"cmd",
		IPMI_CMD_SET_SESSION_PRIV_LEVEL);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_session_priv_level_rq, (uint8_t *)"priv_level",
		priv_level);
  return (0);
}  

int8_t 
ipmi_lan_set_session_priv_level (int sockfd, 
				 struct sockaddr *hostaddr, 
				 size_t hostaddr_len, 
				 uint8_t auth_type, 
				 uint32_t session_seq_num, 
				 uint32_t session_id, 
				 uint8_t *auth_code_data, 
				 uint32_t auth_code_data_len, 
				 uint8_t priv_level, 
				 uint8_t rq_seq, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq;
  
  if (!(hostaddr && sockfd && hostaddr_len && session_id && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  obj_cmd_rq = alloca (fiid_obj_len_bytes (tmpl_cmd_set_session_priv_level_rq));
  memset (obj_cmd_rq, 0, fiid_obj_len_bytes (tmpl_cmd_set_session_priv_level_rq));
  ERR (obj_cmd_rq);

  ERR (fill_cmd_set_session_priv_level (priv_level, obj_cmd_rq) != -1);

  ERR (ipmi_lan_cmd (sockfd, hostaddr, hostaddr_len, auth_type, session_seq_num, 
		     session_id, auth_code_data, auth_code_data_len,
		     IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, rq_seq,
		     obj_cmd_rq, tmpl_cmd_set_session_priv_level_rq,
		     obj_cmd_rs, tmpl_cmd_set_session_priv_level_rs) != -1);
  return (0);
}

int8_t 
ipmi_lan_open_session (int sockfd, 
		       struct sockaddr *hostaddr, 
		       size_t hostaddr_len, 
		       uint8_t auth_type, 
		       char *username, 
                       uint8_t *auth_code_data,
                       uint32_t auth_code_data_len,
		       uint32_t initial_outbound_seq_num, 
		       uint8_t priv_level, 
		       uint32_t *session_seq_num, 
		       uint32_t *session_id, 
		       uint8_t *rq_seq)
{
  fiid_obj_t obj_cmd_rs;
  uint64_t temp_session_id, temp_session_seq_num;
  uint8_t challenge_str[IPMI_SESSION_CHALLENGE_STR_LEN];

  if (!session_seq_num
      || !session_id
      || !rq_seq)
    {
      errno = EINVAL;
      return (-1);
    }

  *rq_seq = 0;
  obj_cmd_rs = fiid_obj_calloc (tmpl_cmd_get_channel_auth_caps_rs);
  if (ipmi_lan_get_channel_auth_caps (sockfd, hostaddr, hostaddr_len, *rq_seq, obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    {
      errno = EINVAL;
      goto error;
    }
  ipmi_xfree (obj_cmd_rs);
  IPMI_LAN_RQ_SEQ_INC (*rq_seq);

  obj_cmd_rs = fiid_obj_calloc (tmpl_cmd_get_session_challenge_rs);
  if (ipmi_lan_get_session_challenge (sockfd, hostaddr, hostaddr_len, 
				      auth_type, username, *rq_seq, obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    {
      errno = EINVAL;
      goto error;
    }

  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs, 
		(uint8_t *)"tmp_session_id", &temp_session_id);
  *session_id = temp_session_id;
  fiid_obj_get_data (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs, (uint8_t *)"challenge_str", challenge_str, IPMI_SESSION_CHALLENGE_STR_LEN);
  ipmi_xfree (obj_cmd_rs);
  IPMI_LAN_RQ_SEQ_INC (*rq_seq);

  obj_cmd_rs = fiid_obj_calloc (tmpl_cmd_activate_session_rs);
  if (ipmi_lan_activate_session (sockfd, hostaddr, hostaddr_len, 
				 auth_type, *session_id, auth_code_data, auth_code_data_len,
				 priv_level, challenge_str, IPMI_SESSION_CHALLENGE_STR_LEN,
				 initial_outbound_seq_num, *rq_seq, obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    {
      errno = EINVAL;
      goto error;
    }

  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		(uint8_t *)"session_id", &temp_session_id);
  *session_id = temp_session_id;
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		(uint8_t *)"initial_inbound_seq_num", &temp_session_seq_num);
  *session_seq_num = temp_session_seq_num;
  ipmi_xfree (obj_cmd_rs);
  IPMI_LAN_RQ_SEQ_INC (*rq_seq);

  obj_cmd_rs = fiid_obj_calloc (tmpl_cmd_set_session_priv_level_rs);
  if (ipmi_lan_set_session_priv_level (sockfd, hostaddr, hostaddr_len, 
				       auth_type, *session_seq_num, *session_id, 
				       auth_code_data, auth_code_data_len, priv_level,
				       *rq_seq,  obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    {
      errno = EINVAL;
      goto error;
    }

  ipmi_xfree (obj_cmd_rs);
  IPMI_LAN_RQ_SEQ_INC (*rq_seq);

  return (0);

 error:
  ipmi_xfree (obj_cmd_rs);
  return (-1);
}

int8_t 
fill_cmd_close_session (uint32_t close_session_id, 
			fiid_obj_t obj_cmd)
{
  if (obj_cmd == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_close_session_rq, (uint8_t *)"cmd",
		IPMI_CMD_CLOSE_SESSION);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_close_session_rq, (uint8_t *)"session_id",
		close_session_id);
  return (0);
}  

int8_t 
ipmi_lan_close_session (int sockfd, 
			struct sockaddr *hostaddr, 
			size_t hostaddr_len, 
			uint8_t auth_type, 
			uint32_t session_seq_num, 
			uint32_t session_id, 
			uint8_t *auth_code_data, 
			uint32_t auth_code_data_len, 
			uint8_t rq_seq, 
			uint32_t close_session_id, 
			fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq;
  
  if (!(hostaddr && sockfd && hostaddr_len && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  obj_cmd_rq = alloca (fiid_obj_len_bytes (tmpl_cmd_close_session_rq));
  memset (obj_cmd_rq, 0, fiid_obj_len_bytes (tmpl_cmd_close_session_rq));
  ERR (obj_cmd_rq);

  ERR (fill_cmd_close_session (close_session_id, obj_cmd_rq) != -1);
  ERR (ipmi_lan_cmd (sockfd, hostaddr, hostaddr_len, auth_type,
		     session_seq_num, session_id, auth_code_data, auth_code_data_len,
		     IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, rq_seq,
		     obj_cmd_rq, tmpl_cmd_close_session_rq,
		     obj_cmd_rs, tmpl_cmd_close_session_rs) != -1);
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
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_CHANNEL_ACCESS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"ipmi_messaging_access_mode", 
		ipmi_messaging_access_mode);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"user_level_authentication", 
		user_level_authentication);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"per_message_authentication", 
		per_message_authentication);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"pef_alerting", 
		pef_alerting);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"channel_access_set_flag", 
		channel_access_set_flag);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		(uint8_t *)"channel_privilege_level_limit", 
		channel_privilege_level_limit);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
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
  /* achu: username can be IPMI_USER_NAME_MAX_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  if (!obj_data_rq
      || (user_name && user_name_len > IPMI_USER_NAME_MAX_LENGTH))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_name_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_NAME);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_name_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  ERR_EXIT (fiid_obj_memset_field(obj_data_rq, '\0', 
                                  tmpl_set_user_name_rq,
                                  (uint8_t *)"user_name") == 0);

  if (user_name)
    ERR_EXIT (fiid_obj_set_data (obj_data_rq, 
                                 tmpl_set_user_name_rq, 
                                 (uint8_t *)"user_name", 
                                 (uint8_t *)user_name,
                                 user_name_len) == 0);
  
  return 0;
}

int8_t 
fill_kcs_get_user_name (uint8_t user_id, fiid_obj_t obj_data_rq)
{
  if (!obj_data_rq)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_name_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_USER_NAME_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_name_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  return 0;
}

int8_t 
fill_kcs_set_user_password (uint8_t user_id, 
			    uint8_t operation, 
			    char *user_password,
                            unsigned int user_password_len,
                            fiid_obj_t obj_data_rq)
{
  /* achu: password can be IPMI_USER_PASSWORD_MAX_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  if (!obj_data_rq
      || (user_password 
          && user_password_len > IPMI_USER_PASSWORD_MAX_LENGTH))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_password_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_PASSWORD_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_password_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_password_rq, 
		(uint8_t *)"operation", 
		operation);
  
  /* achu: The BMC may ignore any '\0' characters that indicate the
   * end of the string.  So we need to guarantee the buffer is
   * completely cleared before setting anything.
   */
  ERR_EXIT (fiid_obj_memset_field(obj_data_rq, '\0', 
                                  tmpl_set_user_password_rq,
                                  (uint8_t *)"password") == 0);

  if (user_password)
    ERR_EXIT (fiid_obj_set_data (obj_data_rq,
                                 tmpl_set_user_password_rq,
                                 (uint8_t *)"password",
                                 (uint8_t *)user_password,
                                 user_password_len) == 0);

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
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_USER_ACCESS_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		(uint8_t *)"modify_flag", 
		1);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		(uint8_t *)"user_flags.restrict_to_callback", 
		restrict_to_callback);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		(uint8_t *)"user_flags.enable_link_auth", 
		enable_link_auth);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		(uint8_t *)"user_flags.enable_ipmi_msgs", 
		enable_ipmi_msgs);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_user_access_rq,
		(uint8_t *)"user_privilege_level_limit",
		user_privilege_level_limit);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_user_access_rq,
		(uint8_t *)"user_session_number_limit",
		user_session_number_limit);
  
  return 0;
}

int8_t
fill_kcs_get_user_access (uint8_t channel_number,
			  uint8_t user_id,
                          fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_access_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_USER_ACCESS_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_access_rq, 
		(uint8_t *)"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_user_access_rq,
		(uint8_t *)"channel_number",
		channel_number);
  
  return 0;
}

int8_t
fill_kcs_get_channel_access (uint8_t channel_number,
			     uint8_t channel_access_set_flag,
                             fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_access_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_CHANNEL_ACCESS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_access_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_channel_access_rq,
		(uint8_t *)"channel_access_set_flag",
		channel_access_set_flag);
  
  return 0;
}

int8_t 
fill_kcs_get_channel_info (uint8_t channel_number, fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_info_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_CHANNEL_INFO_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_info_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  return 0;
}

int8_t
ipmi_check_cmd(fiid_template_t tmpl_cmd, fiid_obj_t obj_cmd, uint8_t cmd)
{
  uint64_t cmd_recv;

  if (!(obj_cmd && tmpl_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_cmd, (uint8_t *)"cmd"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, tmpl_cmd, (uint8_t *)"cmd", &cmd_recv);

  return ((((uint8_t)cmd_recv) == cmd) ? 1 : 0);
}

int8_t
ipmi_check_comp_code(fiid_template_t tmpl_cmd, fiid_obj_t obj_cmd, uint8_t comp_code)
{
  uint64_t comp_code_recv;

  if (!(obj_cmd && tmpl_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_cmd, (uint8_t *)"comp_code"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, tmpl_cmd, (uint8_t *)"comp_code", &comp_code_recv);

  return ((((uint8_t)comp_code_recv) == comp_code) ? 1 : 0);
}

int8_t 
ipmi_get_channel_number2 (ipmi_device_t *dev, uint8_t channel_medium_type)
{
  if (!dev)
    {
      errno = EINVAL;
      return -1;
    }

  if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
    {
      fiid_obj_t obj_data_rs;
      uint64_t manf_id, prod_id;
      
      FIID_OBJ_ALLOCA (obj_data_rs, tmpl_cmd_get_dev_id_rs);
      
      ERR (ipmi_cmd_get_dev_id (dev, obj_data_rs) == 0);
      
      FIID_OBJ_GET (obj_data_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"manf_id.id", &manf_id);
      FIID_OBJ_GET (obj_data_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"prod_id", &prod_id);
      
      switch (manf_id)
	{
	case IPMI_MANF_ID_INTEL:
	case 0xB000157: // Intel 
	  switch (prod_id)
	    {
	    case IPMI_PROD_ID_SE7501WV2:
	      return 7;
	    }
	}
    }
  
  {
    fiid_obj_t data_rs;
    uint64_t val;
    int i;
    
    FIID_OBJ_ALLOCA (data_rs, tmpl_get_channel_info_rs);
    
    /* Channel numbers range from 0 - 7 */
    for (i = 0; i < 8; i++)
      {
	if (ipmi_cmd_get_channel_info2 (dev, i, data_rs) != 0)
	  continue;
	
	FIID_OBJ_GET (data_rs, 
		      tmpl_get_channel_info_rs, 
		      (uint8_t *)"channel_medium_type", 
		      &val);
	if ((uint8_t) val == channel_medium_type)
	  {
	    FIID_OBJ_GET (data_rs, 
			  tmpl_get_channel_info_rs, 
			  (uint8_t *)"actual_channel_number", 
			  &val);
	    return (int8_t) val;
	  }
      }
  }
  
  return (-1);
}

int8_t 
ipmi_cmd_get_channel_auth_caps2 (ipmi_device_t *dev, 
				 fiid_obj_t obj_cmd_rs)
{
  ipmi_device_t local_dev;
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  local_dev = *dev;
  local_dev.io.outofband.auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
  local_dev.io.outofband.rq.tmpl_hdr_session_ptr = 
    local_dev.io.outofband.rs.tmpl_hdr_session_ptr = &tmpl_hdr_session;
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rq.obj_hdr_session,
		   *(local_dev.io.outofband.rq.tmpl_hdr_session_ptr));
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rs.obj_hdr_session,
		   *(local_dev.io.outofband.rs.tmpl_hdr_session_ptr));
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_get_channel_auth_caps_rq);
  ERR (fill_cmd_get_channel_auth_caps (IPMI_CHANNEL_CURRENT_CHANNEL,
                                       IPMI_PRIV_LEVEL_USER, 
                                       obj_cmd_rq) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR (ipmi_cmd (&local_dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_get_channel_auth_caps_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_get_channel_auth_caps_rs) == 0);
  
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_session_challenge2 (ipmi_device_t *dev, 
				 fiid_obj_t obj_cmd_rs)
{
  ipmi_device_t local_dev;
  fiid_obj_t obj_cmd_rq = NULL;

  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  local_dev = *dev;
  local_dev.io.outofband.auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
  local_dev.io.outofband.rq.tmpl_hdr_session_ptr = 
    local_dev.io.outofband.rs.tmpl_hdr_session_ptr = &tmpl_hdr_session;
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rq.obj_hdr_session,
		   *(local_dev.io.outofband.rq.tmpl_hdr_session_ptr));
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rs.obj_hdr_session,
		   *(local_dev.io.outofband.rs.tmpl_hdr_session_ptr));
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_get_session_challenge_rq);
  ERR (fill_cmd_get_session_challenge (dev->io.outofband.auth_type, 
				       (char *)dev->io.outofband.username,
				       IPMI_SESSION_MAX_USERNAME_LEN,
				       obj_cmd_rq) == 0);

  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR (ipmi_cmd (&local_dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_get_session_challenge_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_get_session_challenge_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_activate_session2 (ipmi_device_t *dev, 
			    fiid_obj_t obj_cmd_rs)
{
  uint32_t initial_outbound_seq_num = 0;
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  { 
    /* Random number generation */
    unsigned int seedp;
    seedp = (unsigned int) clock () + (unsigned int) time (NULL);
    srand (seedp);
    initial_outbound_seq_num = rand_r (&seedp);
  }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_activate_session_rq);
  ERR (fill_cmd_activate_session (dev->io.outofband.auth_type, 
				  dev->io.outofband.priv_level, 
				  dev->io.outofband.challenge_string, 
				  IPMI_SESSION_CHALLENGE_STR_LEN, 
				  initial_outbound_seq_num, 
				  obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_activate_session_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_activate_session_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_session_priv_level2 (ipmi_device_t *dev, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_set_session_priv_level_rq);
  ERR (fill_cmd_set_session_priv_level (dev->io.outofband.priv_level, 
					obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_set_session_priv_level_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_set_session_priv_level_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_lan_open_session2 (ipmi_device_t *dev)
{
  fiid_obj_t obj_cmd_rs = NULL;
  
  uint64_t supported_auth_type = 0;
  uint64_t temp_session_id = 0;
  uint8_t challenge_str[IPMI_SESSION_CHALLENGE_STR_LEN];
  uint64_t temp_session_seq_num = 0;
  
  if (!dev)
    {
      errno = EINVAL;
      return (-1);
    }

  dev->io.outofband.rq_seq = 0;
  
  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs);
  ERR (ipmi_cmd_get_channel_auth_caps2 (dev, obj_cmd_rs) == 0);
  switch (dev->io.outofband.auth_type)
    {
    case IPMI_SESSION_AUTH_TYPE_NONE:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.none", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_MD2:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.md2", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_MD5:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.md5", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.straight_passwd_key", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_OEM_PROP:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.oem_prop", &supported_auth_type);
      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  if (supported_auth_type == 0)
    {
      errno = ENOTSUP;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs);
  ERR (ipmi_cmd_get_session_challenge2 (dev, obj_cmd_rs) == 0);
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs, 
		(uint8_t *)"tmp_session_id", &temp_session_id);
  dev->io.outofband.session_id = temp_session_id;
  ERR (fiid_obj_get_data (obj_cmd_rs, 
			  tmpl_cmd_get_session_challenge_rs, 
			  (uint8_t *)"challenge_str", 
			  challenge_str,
                          IPMI_SESSION_CHALLENGE_STR_LEN) == 0);
  memcpy (dev->io.outofband.challenge_string, 
	  challenge_str, 
	  IPMI_SESSION_CHALLENGE_STR_LEN);

  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_activate_session_rs);
  ERR (ipmi_cmd_activate_session2 (dev, obj_cmd_rs) == 0);
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		(uint8_t *)"session_id", &temp_session_id);
  dev->io.outofband.session_id = temp_session_id;
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		(uint8_t *)"initial_inbound_seq_num", &temp_session_seq_num);
  dev->io.outofband.session_seq_num = temp_session_seq_num;
  
  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_set_session_priv_level_rs);
  ERR (ipmi_cmd_set_session_priv_level2 (dev, obj_cmd_rs) == 0);
  
  return (0);
}

int8_t 
ipmi_lan_close_session2 (ipmi_device_t *dev, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_close_session_rq);
  ERR (fill_cmd_close_session (dev->io.outofband.session_id, 
			       obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_close_session_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_close_session_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_channel_access2 (ipmi_device_t *dev, 
			      uint8_t channel_number, 
			      uint8_t ipmi_messaging_access_mode, 
			      uint8_t user_level_authentication, 
			      uint8_t per_message_authentication, 
			      uint8_t pef_alerting, 
			      uint8_t channel_access_set_flag, 
			      uint8_t channel_privilege_level_limit, 
			      uint8_t channel_privilege_level_limit_set_flag, 
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_channel_access_rq);
  ERR (fill_kcs_set_channel_access (channel_number, 
				    ipmi_messaging_access_mode, 
				    user_level_authentication, 
				    per_message_authentication, 
				    pef_alerting, 
				    channel_access_set_flag, 
				    channel_privilege_level_limit, 
				    channel_privilege_level_limit_set_flag,
                                    obj_cmd_rq) == 0); 
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_channel_access_rq, 
		 obj_cmd_rs, 
		 tmpl_set_channel_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_user_name2 (ipmi_device_t *dev, 
			 uint8_t user_id, 
			 char *user_name, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_user_name_rq);
  ERR (fill_kcs_set_user_name (user_id, 
			       user_name, 
			       ((user_name) ? strlen (user_name) : 0),
                               obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_user_name_rq, 
		 obj_cmd_rs, 
		 tmpl_set_user_name_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_user_name2 (ipmi_device_t *dev, 
			 uint8_t user_id, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_user_name_rq);
  ERR (fill_kcs_get_user_name (user_id, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_user_name_rq, 
		 obj_cmd_rs, 
		 tmpl_get_user_name_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_user_password2 (ipmi_device_t *dev, 
			     uint8_t user_id, 
			     uint8_t operation, 
			     char *user_password,
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_user_password_rq);
  ERR (fill_kcs_set_user_password (user_id, 
				   operation, 
				   user_password, 
				   ((user_password) ? strlen(user_password) : 0),
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_user_password_rq, 
		 obj_cmd_rs, 
		 tmpl_set_user_password_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_user_access2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
			   uint8_t user_id,
			   uint8_t restrict_to_callback,
			   uint8_t enable_link_auth,
			   uint8_t enable_ipmi_msgs,
			   uint8_t user_privilege_level_limit,
			   uint8_t user_session_number_limit, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_user_access_rq);
  ERR (fill_kcs_set_user_access (channel_number,
				 user_id,
				 restrict_to_callback,
				 enable_link_auth,
				 enable_ipmi_msgs,
				 user_privilege_level_limit,
				 user_session_number_limit,
                                 obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_user_access_rq, 
		 obj_cmd_rs, 
		 tmpl_set_user_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_user_access2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
			   uint8_t user_id,
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_user_access_rq);
  ERR (fill_kcs_get_user_access (channel_number, user_id, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_user_access_rq, 
		 obj_cmd_rs, 
		 tmpl_get_user_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_channel_access2 (ipmi_device_t *dev, 
			      uint8_t channel_number,
			      uint8_t channel_access_set_flag,
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_channel_access_rq);
  ERR (fill_kcs_get_channel_access (channel_number, 
				    channel_access_set_flag,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_channel_access_rq, 
		 obj_cmd_rs, 
		 tmpl_get_channel_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_channel_info2 (ipmi_device_t *dev, 
			    uint8_t channel_number,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_channel_info_rq);
  ERR (fill_kcs_get_channel_info (channel_number, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_channel_info_rq, 
		 obj_cmd_rs, 
		 tmpl_get_channel_info_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

