/* 
   ipmi-msg-support-cmds.c - IPMI Message Support Commands

   Copyright (C) 2003, 2004 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#include <errno.h>
#ifdef __FreeBSD__
#include <sys/types.h>
#endif
#include <netinet/in.h>

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

fiid_template_t tmpl_cmd_get_session_challenge_rq =
  {
    {8, "cmd"},
    {4, "auth_type"},
    {4, "reserved1"},
    {128, "username"},
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
fill_cmd_get_channel_auth_caps (u_int8_t max_priv_level, fiid_obj_t obj_cmd)
{
  if (!obj_cmd || !IPMI_PRIV_LEVEL_VALID(max_priv_level))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_rq, "cmd", 
		IPMI_CMD_GET_CHANNEL_AUTH_CAPS);

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_rq,
		"channel_num", 0x0E); 
  /* retrieve information for channel this request was
     issued on. */

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_channel_auth_caps_rq, "max_priv_level", 
		max_priv_level);
  return (0);
}

int8_t
ipmi_lan_get_channel_auth_caps (int sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, fiid_obj_t obj_cmd_rs)
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

  ERR (fill_cmd_get_channel_auth_caps (IPMI_PRIV_LEVEL_USER, obj_cmd_rq) != -1);

  ERR (ipmi_lan_cmd (sockfd, hostaddr, hostaddr_len, IPMI_SESSION_AUTH_TYPE_NONE,
		     0, 0, NULL, 0, IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, 0,
		     obj_cmd_rq, tmpl_cmd_get_channel_auth_caps_rq,
		     obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs) != -1);

  /* INFO: you can also do auth type check here */
  return (0);
}

int8_t
fill_cmd_get_session_challenge (u_int8_t auth_type, char *username, u_int32_t username_len, fiid_obj_t obj_cmd)
{
  if (!obj_cmd || !IPMI_SESSION_AUTH_TYPE_VALID(auth_type))
    {
      errno = EINVAL;
      return (-1);
    }

  /* achu: username can be IPMI_SESSION_MAX_USERNAME_LEN length.  Null
   * termination in IPMI packet not required
   */
  if (username)
    {
      if ((strlen (username) > IPMI_SESSION_MAX_USERNAME_LEN) ||
	  (username_len > IPMI_SESSION_MAX_USERNAME_LEN))
	{
	  errno = EINVAL;
	  return (-1);
	}

      fiid_obj_set_data (obj_cmd, tmpl_cmd_get_session_challenge_rq, "username", username);
    }
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_session_challenge_rq, "cmd", 
		IPMI_CMD_GET_SESSION_CHALLENGE);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_session_challenge_rq, "auth_type",
		auth_type);
  return (0);
}

int8_t
ipmi_lan_get_session_challenge (int sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, u_int8_t auth_type, char *username, fiid_obj_t obj_cmd_rs)
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
		     0, 0, NULL, 0, IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, 0,
		     obj_cmd_rq, tmpl_cmd_get_session_challenge_rq,
		     obj_cmd_rs, tmpl_cmd_get_session_challenge_rs) != -1);
  return (0);
}

int8_t
fill_cmd_activate_session (u_int8_t auth_type, u_int8_t max_priv_level, u_int8_t *challenge_str, u_int32_t challenge_str_len, u_int32_t initial_outbound_seq_num, fiid_obj_t obj_cmd)
{
  if (!IPMI_SESSION_AUTH_TYPE_VALID(auth_type)
      || !IPMI_PRIV_LEVEL_VALID(max_priv_level)
      || challenge_str_len > IPMI_SESSION_CHALLENGE_STR_LEN
      || !obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, "cmd", 
		IPMI_CMD_ACTIVATE_SESSION);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, "auth_type", 
		auth_type);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, "max_priv_level", 
		max_priv_level);
  if (challenge_str)
    fiid_obj_set_data (obj_cmd, tmpl_cmd_activate_session_rq, "challenge_str", challenge_str);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_activate_session_rq, "initial_outbound_seq_num", 
		initial_outbound_seq_num);
  return (0);
}

int8_t
ipmi_lan_activate_session (int sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, u_int8_t auth_type, u_int32_t tmp_session_id, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, u_int8_t max_priv_level, u_int8_t *challenge_str, u_int32_t challenge_str_len, u_int32_t initial_outbound_seq_num, fiid_obj_t obj_cmd_rs)
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
		     IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, 0, 
		     obj_cmd_rq, tmpl_cmd_activate_session_rq,
		     obj_cmd_rs, tmpl_cmd_activate_session_rs) != -1);
  return (0);
}

int8_t
fill_cmd_set_session_priv_level (u_int8_t priv_level, fiid_obj_t obj_cmd)
{
  if (!IPMI_PRIV_LEVEL_VALID(priv_level)
      || obj_cmd == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_session_priv_level_rq, "cmd",
		IPMI_CMD_SET_SESSION_PRIV_LEVEL);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_session_priv_level_rq, "priv_level",
		priv_level);
  return (0);
}  

int8_t
ipmi_lan_set_session_priv_level (int sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, u_int8_t auth_type, u_int32_t session_seq_num, u_int32_t session_id, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, u_int8_t priv_level, u_int8_t rq_seq, fiid_obj_t obj_cmd_rs)
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
ipmi_lan_open_session (int sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, u_int8_t auth_type, char *username, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, u_int32_t initial_outbound_seq_num, u_int8_t priv_level, u_int32_t *session_seq_num, u_int32_t *session_id)
{
  fiid_obj_t obj_cmd_rs;
  u_int64_t temp_session_id, temp_session_seq_num;
  u_int8_t challenge_str[IPMI_CHALLENGE_STR_MAX];
  
  obj_cmd_rs = fiid_obj_alloc (tmpl_cmd_get_channel_auth_caps_rs);
  if (ipmi_lan_get_channel_auth_caps (sockfd, hostaddr, hostaddr_len, obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    goto error;
  ipmi_xfree (obj_cmd_rs);

  obj_cmd_rs = fiid_obj_alloc (tmpl_cmd_get_session_challenge_rs);
  if (ipmi_lan_get_session_challenge (sockfd, hostaddr, hostaddr_len, 
				      auth_type, username, obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    goto error;
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs, 
		"tmp_session_id", &temp_session_id);
  *session_id = temp_session_id;

  fiid_obj_get_data (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs, "challenge_str", challenge_str);

  ipmi_xfree (obj_cmd_rs);

  obj_cmd_rs = fiid_obj_alloc (tmpl_cmd_activate_session_rs);

  if (ipmi_lan_activate_session (sockfd, hostaddr, hostaddr_len, 
				 auth_type, *session_id, auth_code_data, auth_code_data_len, priv_level, 
				 challenge_str, IPMI_CHALLENGE_STR_MAX,
				 initial_outbound_seq_num, obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    goto error;
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		"session_id", &temp_session_id);
  *session_id = temp_session_id;
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		"initial_inbound_seq_num", &temp_session_seq_num);
  *session_seq_num = temp_session_seq_num;
  ipmi_xfree (obj_cmd_rs);

  obj_cmd_rs = fiid_obj_alloc (tmpl_cmd_set_session_priv_level_rs);

  if (ipmi_lan_set_session_priv_level (sockfd, hostaddr, hostaddr_len, 
				       auth_type, *session_seq_num, *session_id, 
				       auth_code_data, auth_code_data_len, priv_level, 1, obj_cmd_rs) == -1)
    goto error;
  if (!ipmi_comp_test (obj_cmd_rs))
    goto error;
  ipmi_xfree (obj_cmd_rs);

  return (0);

 error:
  ipmi_xfree (obj_cmd_rs);
  return (-1);
}

int8_t
fill_cmd_close_session (u_int32_t close_session_id, fiid_obj_t obj_cmd)
{
  if (obj_cmd == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_close_session_rq, "cmd",
		IPMI_CMD_CLOSE_SESSION);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_close_session_rq, "session_id",
		close_session_id);
  return (0);
}  

int8_t
ipmi_lan_close_session (int sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, u_int8_t auth_type, u_int32_t session_seq_num, u_int32_t session_id, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, u_int8_t rq_seq, u_int32_t close_session_id, fiid_obj_t obj_cmd_rs)
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
fill_kcs_set_channel_access (fiid_obj_t obj_data_rq, 
			     u_int8_t channel_number, 
			     u_int8_t ipmi_messaging_access_mode, 
			     u_int8_t user_level_authentication, 
			     u_int8_t per_message_authentication, 
			     u_int8_t pef_alerting, 
			     u_int8_t channel_access_set_flag, 
			     u_int8_t channel_privilege_level_limit, 
			     u_int8_t channel_privilege_level_limit_set_flag)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"cmd", 
		IPMI_CMD_SET_CHANNEL_ACCESS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"ipmi_messaging_access_mode", 
		ipmi_messaging_access_mode);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"user_level_authentication", 
		user_level_authentication);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"per_message_authentication", 
		per_message_authentication);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"pef_alerting", 
		pef_alerting);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"channel_access_set_flag", 
		channel_access_set_flag);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"channel_privilege_level_limit", 
		channel_privilege_level_limit);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_channel_access_rq, 
		"channel_privilege_level_limit_set_flag", 
		channel_privilege_level_limit_set_flag);
  
  return 0;
}

int8_t 
ipmi_kcs_set_channel_access (u_int8_t channel_number, 
			     u_int8_t ipmi_messaging_access_mode, 
			     u_int8_t user_level_authentication, 
			     u_int8_t per_message_authentication, 
			     u_int8_t pef_alerting, 
			     u_int8_t channel_access_set_flag, 
			     u_int8_t channel_privilege_level_limit, 
			     u_int8_t channel_privilege_level_limit_set_flag, 
			     fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_channel_access_rq);
  fill_kcs_set_channel_access (obj_data_rq, 
			       channel_number, 
			       ipmi_messaging_access_mode, 
			       user_level_authentication, 
			       per_message_authentication, 
			       pef_alerting, 
			       channel_access_set_flag, 
			       channel_privilege_level_limit, 
			       channel_privilege_level_limit_set_flag);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_set_channel_access_rq, 
			 obj_data_rs, tmpl_set_channel_access_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_set_user_name (fiid_obj_t obj_data_rq, 
			u_int8_t user_id, 
			char *user_name)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_name_rq, 
		"cmd", 
		IPMI_CMD_SET_USER_NAME);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_name_rq, 
		"user_id", 
		user_id);
  
  if (user_name)
    fiid_obj_set_data (obj_data_rq, 
                       tmpl_set_user_name_rq, 
                       "user_name", 
                       user_name);
  else
    {
      int32_t start_offset = fiid_obj_field_start_bytes (tmpl_set_user_name_rq,
                                                         "user_name");
      int32_t len = fiid_obj_field_len_bytes (tmpl_set_user_name_rq, 
                                              "user_name");
      memset (obj_data_rq + start_offset, '\0', len);
    }
  
  return 0;
}

int8_t 
ipmi_kcs_set_user_name (u_int8_t user_id, 
			char *user_name, 
			fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_user_name_rq);
  fill_kcs_set_user_name (obj_data_rq, user_id, user_name);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_set_user_name_rq, 
			 obj_data_rs, tmpl_set_user_name_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_user_name (fiid_obj_t obj_data_rq, u_int8_t user_id) 
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_name_rq, 
		"cmd", 
		IPMI_CMD_GET_USER_NAME_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_name_rq, 
		"user_id", 
		user_id);
  
  return 0;
}

int8_t 
ipmi_kcs_get_user_name (u_int8_t user_id, 
			fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_user_name_rq);
  fill_kcs_get_user_name (obj_data_rq, user_id);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_get_user_name_rq, 
			 obj_data_rs, tmpl_get_user_name_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_set_user_password (fiid_obj_t obj_data_rq, 
			    u_int8_t user_id, 
			    u_int8_t operation, 
			    char *user_password)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_password_rq, 
		"cmd", 
		IPMI_CMD_SET_USER_PASSWORD_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_password_rq, 
		"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_password_rq, 
		"operation", 
		operation);
  
  if (user_password)
    fiid_obj_set_data (obj_data_rq,
                       tmpl_set_user_password_rq,
                       "password",
                       user_password);
  else
    {
      int32_t start_offset = fiid_obj_field_start_bytes (tmpl_set_user_password_rq,
                                                         "password");
      int32_t len = fiid_obj_field_len_bytes (tmpl_set_user_password_rq,
                                              "password");
      memset (obj_data_rq + start_offset, '\0', len);
    }
  
  return 0;
}

int8_t 
ipmi_kcs_set_user_password (u_int8_t user_id, 
			    u_int8_t operation, 
			    char *user_password, 
			    fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_user_password_rq);
  fill_kcs_set_user_password (obj_data_rq, user_id, operation, user_password);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_set_user_password_rq, 
			 obj_data_rs, tmpl_set_user_password_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_set_user_access (fiid_obj_t obj_data_rq, 
			  u_int8_t channel_number,
			  u_int8_t user_id,
			  u_int8_t restrict_to_callback,
			  u_int8_t enable_link_auth,
			  u_int8_t enable_ipmi_msgs,
			  u_int8_t user_privilege_level_limit,
			  u_int8_t user_session_number_limit)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		"cmd", 
		IPMI_CMD_SET_USER_ACCESS_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		"modify_flag", 
		1);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		"user_flags.restrict_to_callback", 
		restrict_to_callback);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		"user_flags.enable_link_auth", 
		enable_link_auth);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		"user_flags.enable_ipmi_msgs", 
		enable_ipmi_msgs);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_user_access_rq, 
		"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_user_access_rq,
		"user_privilege_level_limit",
		user_privilege_level_limit);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_user_access_rq,
		"user_session_number_limit",
		user_session_number_limit);
  
  return 0;
}

int8_t
ipmi_kcs_set_user_access (u_int8_t channel_number,
			  u_int8_t user_id,
			  u_int8_t restrict_to_callback,
			  u_int8_t enable_link_auth,
			  u_int8_t enable_ipmi_msgs,
			  u_int8_t user_privilege_level_limit,
			  u_int8_t user_session_number_limit, 
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_user_access_rq);
  fill_kcs_set_user_access (obj_data_rq, 
			    channel_number,
			    user_id,
			    restrict_to_callback,
			    enable_link_auth,
			    enable_ipmi_msgs,
			    user_privilege_level_limit,
			    user_session_number_limit);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_set_user_access_rq, 
			 obj_data_rs, tmpl_set_user_access_rs);
  free (obj_data_rq);
  return status;
}

int8_t
fill_kcs_get_user_access (fiid_obj_t obj_data_rq, 
			  u_int8_t channel_number,
			  u_int8_t user_id)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_access_rq, 
		"cmd", 
		IPMI_CMD_GET_USER_ACCESS_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_user_access_rq, 
		"user_id", 
		user_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_user_access_rq,
		"channel_number",
		channel_number);
  
  return 0;
}

int8_t
ipmi_kcs_get_user_access (u_int8_t channel_number,
			  u_int8_t user_id,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_user_access_rq);
  fill_kcs_get_user_access (obj_data_rq, channel_number, user_id);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_get_user_access_rq, 
			 obj_data_rs, tmpl_get_user_access_rs);
  free (obj_data_rq);
  return status;
}


int8_t
fill_kcs_get_channel_access (fiid_obj_t obj_data_rq, 
			     u_int8_t channel_number,
			     u_int8_t channel_access_set_flag)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_access_rq, 
		"cmd", 
		IPMI_CMD_GET_CHANNEL_ACCESS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_access_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_channel_access_rq,
		"channel_access_set_flag",
		channel_access_set_flag);
  
  return 0;
}

int8_t
ipmi_kcs_get_channel_access (u_int8_t channel_number,
			     u_int8_t channel_access_set_flag,
			     fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_channel_access_rq);
  fill_kcs_get_channel_access (obj_data_rq, channel_number, channel_access_set_flag);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_get_channel_access_rq, 
			 obj_data_rs, tmpl_get_channel_access_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_channel_info (fiid_obj_t obj_data_rq, u_int8_t channel_number)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_info_rq, 
		"cmd", 
		IPMI_CMD_GET_CHANNEL_INFO_CMD);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_channel_info_rq, 
		"channel_number", 
		channel_number);
  
  return 0;
}

int8_t 
ipmi_kcs_get_channel_info (u_int8_t channel_number,
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_channel_info_rq);
  fill_kcs_get_channel_info (obj_data_rq, channel_number);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_APP_RQ, 
			 obj_data_rq, tmpl_get_channel_info_rq, 
			 obj_data_rs, tmpl_get_channel_info_rs);
  free (obj_data_rq);
  return status;
}

int8_t
ipmi_check_cmd(fiid_template_t tmpl_cmd, fiid_obj_t obj_cmd, u_int8_t cmd)
{
  u_int64_t cmd_recv;

  if (!(obj_cmd && tmpl_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_cmd, "cmd"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, tmpl_cmd, "cmd", &cmd_recv);

  return ((((int8_t)cmd_recv) == cmd) ? 1 : 0);
}

int8_t
ipmi_check_comp_code(fiid_template_t tmpl_cmd, fiid_obj_t obj_cmd, u_int8_t comp_code)
{
  u_int64_t comp_code_recv;

  if (!(obj_cmd && tmpl_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (!fiid_obj_field_lookup (tmpl_cmd, "comp_code"))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_GET(obj_cmd, tmpl_cmd, "comp_code", &comp_code_recv);

  return ((((int8_t)comp_code_recv) == comp_code) ? 1 : 0);
}
