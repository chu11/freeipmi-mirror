/*
   ipmi-msg-support-cmds.h - IPMI Message Support Commands

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _IPMI_MSG_SUPPORT_CMDS_H
#define	_IPMI_MSG_SUPPORT_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_USER_NAME_MAX_LENGTH        16
#define IPMI_USER_PASSWORD_MAX_LENGTH    16

#define IPMI_OEM_ID_LEN                0x03

#define IPMI_SLAVE_ADDR_BMC            0x20 /* 12.4 */
#define IPMI_SLAVE_ADDR_SWID           0x81 /* 5.5 */

#define IPMI_MESSAGING_ACCESS_MODE_DISABLED            0x0
#define IPMI_MESSAGING_ACCESS_MODE_PRE_BOOT_ONLY       0x1
#define IPMI_MESSAGING_ACCESS_MODE_ALWAYS_AVAILABLE    0x2
#define IPMI_MESSAGING_ACCESS_MODE_SHARED              0x3

#define IPMI_AUTH_STATUS_ANONYMOUS_LOGIN_ENABLED       1
#define IPMI_AUTH_STATUS_ANONYMOUS_LOGIN_DISABLED      0

#define IPMI_AUTH_STATUS_NULL_USERNAMES_ENABLED        1
#define IPMI_AUTH_STATUS_NULL_USERNAMES_DISABLED       0

#define IPMI_AUTH_STATUS_NON_NULL_USERNAMES_ENABLED    1
#define IPMI_AUTH_STATUS_NON_NULL_USERNAMES_DISABLED   0

#define IPMI_USER_LEVEL_AUTHENTICATION_ENABLE     0x0
#define IPMI_USER_LEVEL_AUTHENTICATION_DISABLE    0x1

#define IPMI_PER_MESSAGE_AUTHENTICATION_ENABLE     0x0
#define IPMI_PER_MESSAGE_AUTHENTICATION_DISABLE    0x1

#define IPMI_PEF_ALERTING_ENABLE     0x0
#define IPMI_PEF_ALERTING_DISABLE    0x1

#define IPMI_CHANNEL_ACCESS_NO_CHANGE           0x0
#define IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE    0x1
#define IPMI_CHANNEL_ACCESS_SET_VOLATILE        0x2
#define IPMI_CHANNEL_ACCESS_RESERVED            0x3

#define IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE    0x1
#define IPMI_CHANNEL_ACCESS_GET_VOLATILE        0x2

#define IPMI_PRIV_LEVEL_LIMIT_NO_CHANGE           0x0
#define IPMI_PRIV_LEVEL_LIMIT_SET_NON_VOLATILE    0x1
#define IPMI_PRIV_LEVEL_LIMIT_SET_VOLATILE        0x2
#define IPMI_PRIV_LEVEL_LIMIT_RESERVED            0x3

/* channel medium type */
#define IPMI_CHANNEL_MEDIUM_TYPE_RESERVED       0x0 /* 0x0D to 0x5F */
#define IPMI_CHANNEL_MEDIUM_TYPE_IPMB           0x1
#define IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10        0x2
#define IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09        0x3
#define IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3      0x4
#define IPMI_CHANNEL_MEDIUM_TYPE_RS232          0x5
#define IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN      0x6
#define IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS      0x7
#define IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11    0x8
#define IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20       0x9
#define IPMI_CHANNEL_MEDIUM_TYPE_USB_1X         0xA
#define IPMI_CHANNEL_MEDIUM_TYPE_USB_2X         0xB
#define IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE      0xC
#define IPMI_CHANNEL_MEDIUM_TYPE_OEM            0x7F /* 0x60 to 0x7F */

/* channel protocol type */
#define IPMI_CHANNEL_PROTOCOL_TYPE_RESERVED       0x0 /* 0x03, 0x0A to 0x1B */
#define IPMI_CHANNEL_PROTOCOL_TYPE_IPMB           0x1
#define IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10        0x2
#define IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X    0x4
#define IPMI_CHANNEL_PROTOCOL_TYPE_KCS            0x5
#define IPMI_CHANNEL_PROTOCOL_TYPE_SMIC           0x6
#define IPMI_CHANNEL_PROTOCOL_TYPE_BT_10          0x7
#define IPMI_CHANNEL_PROTOCOL_TYPE_BT_15          0x8
#define IPMI_CHANNEL_PROTOCOL_TYPE_TMODE          0x9
#define IPMI_CHANNEL_PROTOCOL_TYPE_OEM            0x1F /* 0x1C to 0x1F */

/* channel session support */
#define IPMI_CHANNEL_SESSION_LESS      0x0
#define IPMI_CHANNEL_SINGLE_SESSION    0x1
#define IPMI_CHANNEL_MULTI_SESSION     0x2
#define IPMI_CHANNEL_SESSION_BASED     0x3

/* To avoid gcc warnings, added +1 and -1 in comparison */
#define IPMI_CHANNEL_NUMBER_VALID(channel_number) \
        (((channel_number+1) >= 0x1 \
          && (channel_number-1) <= 0xE) ? 1 : 0)

#define IPMI_PASSWORD_OPERATION_DISABLE_USER     0x0
#define IPMI_PASSWORD_OPERATION_ENABLE_USER      0x1
#define IPMI_PASSWORD_OPERATION_SET_PASSWORD     0x2
#define IPMI_PASSWORD_OPERATION_TEST_PASSWORD    0x3

#define IPMI_PASSWORD_OPERATION_TEST_FAILED    0x80

#define IPMI_GET_IPMI_V20_EXTENDED_DATA          0x01

#if 0
#pragma pack(1)
typedef struct ipmi_cmd_get_channel_auth_caps_rq
{
  u_int8_t cmd;
  struct {
    u_int8_t channel_num;
    u_int8_t max_priv_level:4;
    u_int8_t reserved:4;
  } data;
} ipmi_cmd_get_channel_auth_caps_rq_t;

typedef struct ipmi_cmd_get_channel_auth_caps_rs
{
  u_int8_t cmd;
  u_int8_t comp_code;
  struct {
    u_int8_t channel_num;
    struct {
      u_int8_t none:1;
      u_int8_t md2:1;
      u_int8_t md5:1;
      u_int8_t reserved1:1;
      u_int8_t straight_passwd_key:1;
      u_int8_t oem_prop:1;
      u_int8_t reserved2:2;
    } auth_type;
    struct {
      u_int8_t anonymous_login:1;
      u_int8_t null_username:1;
      u_int8_t non_null_username:1;
      u_int8_t user_level_auth:1;
      u_int8_t per_message_auth:1;
      u_int8_t reserved:3;
    } auth_status;
    u_int8_t reserved;
    u_int8_t oem_id[IPMI_OEM_ID_LEN];
    u_int8_t oem_aux;
  } data;
} ipmi_cmd_get_channel_auth_caps_rs_t;

typedef struct ipmi_cmd_get_session_challenge_rq
{
  u_int8_t cmd;
  struct {
    u_int8_t auth_type:4;
    u_int8_t reserved:4;
    u_int8_t username[IPMI_SESSION_MAX_USERNAME_LEN];
  } data;
} ipmi_cmd_get_session_challenge_rq_t;

typedef struct ipmi_cmd_get_session_challenge_rs
{
  u_int8_t cmd;
  u_int8_t comp_code;
  struct {
    u_int32_t tmp_session_id; /* LS byte first */
    u_int8_t challenge_str[IPMI_SESSION_CHALLENGE_STR_LEN];
  } data;
} ipmi_cmd_get_session_challenge_rs_t;

typedef struct ipmi_cmd_activate_session_rq
{
  u_int8_t cmd;
  struct {
    u_int8_t auth_type:4;
    u_int8_t reserved1:4;
    u_int8_t max_priv_level:4;
    u_int8_t reserved2:4;
    u_int8_t challenge_str[IPMI_SESSION_CHALLENGE_STR_LEN];
    u_int32_t initial_outbound_seq_num;
  } data;
} ipmi_cmd_activate_session_rq_t;

typedef struct ipmi_cmd_activate_session_rs
{
  u_int8_t cmd;
  u_int8_t comp_code;
  struct {
    u_int8_t auth_type:4;
    u_int8_t reserved1:4;
    u_int32_t session_id; 
    u_int32_t initial_inbound_seq_num;
    u_int8_t max_priv_level:4;
    u_int8_t reserved2:4;
  } data;
} ipmi_cmd_activate_session_rs_t;

typedef struct ipmi_cmd_set_session_priv_level_rq
{
  u_int8_t cmd;
  struct {
    u_int8_t priv_level:4;
    u_int8_t reserved:4;
  } data;
} ipmi_cmd_set_session_priv_level_rq_t;

typedef struct ipmi_cmd_set_session_priv_level_rs
{
  u_int8_t cmd;
  u_int8_t comp_code;
  struct {
    u_int8_t new_priv_level:4;
    u_int8_t reserved:4;
  } data;
} ipmi_cmd_set_session_priv_level_rs_t;

typedef struct ipmi_cmd_close_session_rq
{
  u_int8_t cmd;
  struct {
    u_int32_t session_id;
  } data;
} ipmi_cmd_close_session_rq_t;

typedef struct ipmi_cmd_close_session_rs
{
  u_int8_t cmd;
  u_int8_t comp_code;
  struct {
  } data;
} ipmi_cmd_close_session_rs_t;
#pragma pack(0)
#endif

extern fiid_template_t tmpl_cmd_get_channel_auth_caps_rq;
extern fiid_template_t tmpl_cmd_get_channel_auth_caps_rs;
extern fiid_template_t tmpl_cmd_get_channel_auth_caps_v20_rq;
extern fiid_template_t tmpl_cmd_get_channel_auth_caps_v20_rs;
extern fiid_template_t tmpl_cmd_get_session_challenge_rq;
extern fiid_template_t tmpl_cmd_get_session_challenge_rs;
extern fiid_template_t tmpl_cmd_activate_session_rq;
extern fiid_template_t tmpl_cmd_activate_session_rs;
extern fiid_template_t tmpl_cmd_set_session_priv_level_rq;
extern fiid_template_t tmpl_cmd_set_session_priv_level_rs;
extern fiid_template_t tmpl_cmd_close_session_rq;
extern fiid_template_t tmpl_cmd_close_session_rs;
extern fiid_template_t tmpl_get_channel_access_rq;
extern fiid_template_t tmpl_get_channel_access_rs;

int8_t fill_cmd_get_channel_auth_caps (u_int8_t max_priv_level, 
				       fiid_obj_t obj_cmd);

int8_t fill_cmd_get_channel_auth_caps_v20 (u_int8_t max_priv_level, 
                                           u_int8_t get_ipmi_v20_extended_data,
                                           fiid_obj_t obj_cmd);

int8_t ipmi_lan_get_channel_auth_caps (int sockfd, 
				       struct sockaddr *hostaddr, 
				       size_t hostaddr_len, 
				       u_int8_t rq_seq, 
				       fiid_obj_t obj_cmd_rs);

int8_t fill_cmd_get_session_challenge (u_int8_t auth_type, 
				       char *username, 
				       u_int32_t username_len, 
				       fiid_obj_t obj_cmd);
int8_t ipmi_lan_get_session_challenge (int sockfd, 
				       struct sockaddr *hostaddr, 
				       size_t hostaddr_len, 
				       u_int8_t auth_type, 
				       char *username, 
				       u_int8_t rq_seq, 
				       fiid_obj_t obj_cmd_rs);

int8_t fill_cmd_activate_session (u_int8_t auth_type, 
				  u_int8_t max_priv_level, 
				  u_int8_t *challenge_str, 
				  u_int32_t challenge_str_len, 
				  u_int32_t initial_outbound_seq_num, 
				  fiid_obj_t obj_cmd);
int8_t ipmi_lan_activate_session (int sockfd, 
				  struct sockaddr *hostaddr, 
				  size_t hostaddr_len, 
				  u_int8_t auth_type, 
				  u_int32_t tmp_session_id, 
				  u_int8_t *auth_code_data, 
				  u_int32_t auth_code_data_len, 
				  u_int8_t max_priv_level, 
				  u_int8_t *challenge_str, 
				  u_int32_t challenge_str_len, 
				  u_int32_t initial_outbound_seq_num, 
				  u_int8_t rq_seq, 
				  fiid_obj_t obj_cmd_rs);

int8_t fill_cmd_set_session_priv_level (u_int8_t priv_level, 
					fiid_obj_t obj_cmd);
int8_t ipmi_lan_set_session_priv_level (int sockfd, 
					struct sockaddr *hostaddr, 
					size_t hostaddr_len, 
					u_int8_t auth_type, 
					u_int32_t session_seq_num, 
					u_int32_t session_id, 
					u_int8_t *auth_code_data, 
					u_int32_t auth_code_data_len, 
					u_int8_t priv_level, 
					u_int8_t rq_seq, 
					fiid_obj_t obj_cmd_rs);


int8_t ipmi_lan_open_session (int sockfd, 
			      struct sockaddr *hostaddr, 
			      size_t hostaddr_len, 
			      u_int8_t auth_type, 
			      char *username, 
			      char *password, 
			      u_int32_t initial_outbound_seq_num, 
			      u_int8_t priv_level, 
			      u_int32_t *session_seq_num, 
			      u_int32_t *session_id, 
			      u_int8_t *rq_seq);

int8_t fill_cmd_close_session (u_int32_t close_session_id, 
			       fiid_obj_t obj_cmd);
int8_t ipmi_lan_close_session (int sockfd, 
			       struct sockaddr *hostaddr, 
			       size_t hostaddr_len, 
			       u_int8_t auth_type, 
			       u_int32_t session_seq_num, 
			       u_int32_t session_id, 
			       u_int8_t *auth_code_data, 
			       u_int32_t auth_code_data_len, 
			       u_int8_t rq_seq, 
			       u_int32_t close_session_id, 
			       fiid_obj_t obj_cmd_rs);


extern fiid_template_t tmpl_set_channel_access_rq;
extern fiid_template_t tmpl_set_channel_access_rs;

extern fiid_template_t tmpl_set_user_access_rq;
extern fiid_template_t tmpl_set_user_access_rs;

extern fiid_template_t tmpl_set_user_name_rq;
extern fiid_template_t tmpl_set_user_name_rs;

extern fiid_template_t tmpl_get_user_name_rq;
extern fiid_template_t tmpl_get_user_name_rs;

extern fiid_template_t tmpl_set_user_password_rq;
extern fiid_template_t tmpl_set_user_password_rs;

extern fiid_template_t tmpl_get_user_access_rq;
extern fiid_template_t tmpl_get_user_access_rs;

extern fiid_template_t tmpl_get_channel_info_rq;
extern fiid_template_t tmpl_get_channel_info_rs;

int8_t ipmi_kcs_set_channel_access (u_int8_t channel_number, 
				    u_int8_t ipmi_messaging_access_mode, 
				    u_int8_t user_level_authentication, 
				    u_int8_t per_message_authentication, 
				    u_int8_t pef_alerting, 
				    u_int8_t channel_access, 
				    u_int8_t channel_privilege_level, 
				    u_int8_t channel_privilege_level_limit, 
				    fiid_obj_t obj_data_rs);

int8_t ipmi_kcs_set_user_name (u_int8_t user_id, 
			       char *user_name, 
			       fiid_obj_t obj_data_rs);

int8_t ipmi_kcs_get_user_name (u_int8_t user_id, 
			       fiid_obj_t obj_data_rs);

int8_t ipmi_kcs_set_user_password (u_int8_t user_id, 
				   u_int8_t operation, 
				   char *user_password, 
				   fiid_obj_t obj_data_rs);

int8_t ipmi_kcs_set_user_access (u_int8_t channel_number,
				 u_int8_t user_id,
				 u_int8_t restrict_to_callback,
				 u_int8_t enable_link_auth,
				 u_int8_t enable_ipmi_msgs,
				 u_int8_t user_privilege_level_limit,
				 u_int8_t user_session_number_limit, 
				 fiid_obj_t obj_data_rs);

int8_t ipmi_kcs_get_user_access (u_int8_t channel_number,
				 u_int8_t user_id,
				 fiid_obj_t obj_data_rs);

int8_t ipmi_kcs_get_channel_access (u_int8_t channel_number,
				    u_int8_t channel_access,
				    fiid_obj_t obj_data_rs);

int8_t ipmi_kcs_get_channel_info (u_int8_t channel_number,
				  fiid_obj_t obj_data_rs);

int8_t ipmi_check_cmd(fiid_template_t tmpl_cmd, fiid_obj_t obj_cmd, u_int8_t cmd);

int8_t ipmi_check_comp_code(fiid_template_t tmpl_cmd, fiid_obj_t obj_cmd, u_int8_t comp_code);

int8_t ipmi_get_channel_number (u_int8_t channel_medium_type);


int8_t ipmi_cmd_get_channel_auth_caps2 (ipmi_device_t *dev, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_session_challenge2 (ipmi_device_t *dev, 
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_activate_session2 (ipmi_device_t *dev, 
				   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_session_priv_level2 (ipmi_device_t *dev, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_lan_open_session2 (ipmi_device_t *dev);
int8_t ipmi_lan_close_session2 (ipmi_device_t *dev, 
				fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_channel_access2 (ipmi_device_t *dev, 
				     u_int8_t channel_number, 
				     u_int8_t ipmi_messaging_access_mode, 
				     u_int8_t user_level_authentication, 
				     u_int8_t per_message_authentication, 
				     u_int8_t pef_alerting, 
				     u_int8_t channel_access_set_flag, 
				     u_int8_t channel_privilege_level_limit, 
				     u_int8_t channel_privilege_level_limit_set_flag, 
				     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_user_name2 (ipmi_device_t *dev, 
				u_int8_t user_id, 
				char *user_name, 
				fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_user_name2 (ipmi_device_t *dev, 
				u_int8_t user_id, 
				fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_user_password2 (ipmi_device_t *dev, 
				    u_int8_t user_id, 
				    u_int8_t operation, 
				    char *user_password,
				    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_set_user_access2 (ipmi_device_t *dev, 
				  u_int8_t channel_number,
				  u_int8_t user_id,
				  u_int8_t restrict_to_callback,
				  u_int8_t enable_link_auth,
				  u_int8_t enable_ipmi_msgs,
				  u_int8_t user_privilege_level_limit,
				  u_int8_t user_session_number_limit, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_user_access2 (ipmi_device_t *dev, 
				  u_int8_t channel_number,
				  u_int8_t user_id,
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_channel_access2 (ipmi_device_t *dev, 
				     u_int8_t channel_number,
				     u_int8_t channel_access_set_flag,
				     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_channel_info2 (ipmi_device_t *dev, 
				   u_int8_t channel_number,
				   fiid_obj_t obj_cmd_rs);



#ifdef __cplusplus
}
#endif

#endif /* ipmi-msg-support-cmds.h */


