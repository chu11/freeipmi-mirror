/* 
   ipmi-interface.h: IPMI Unified Driver Model (API interface for all IPMI drivers)

   Copyright (C) 2005 FreeIPMI Core Team

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

#ifndef _IPMI_INTERFACE_H
#define _IPMI_INTERFACE_H

#define IPMI_SESSION_MAX_USERNAME_LEN     16
#define IPMI_SESSION_CHALLENGE_STR_LEN    16
#define IPMI_SESSION_MAX_AUTH_CODE_LEN    16

#define IPMI_SESSION_AUTH_TYPE_NONE                0x00
#define IPMI_SESSION_AUTH_TYPE_MD2                 0x01
#define IPMI_SESSION_AUTH_TYPE_MD5                 0x02
#define IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY 0x04
#define IPMI_SESSION_AUTH_TYPE_OEM_PROP            0x05

#define IPMI_SESSION_AUTH_TYPE_VALID(auth_type) \
        (((auth_type) == IPMI_SESSION_AUTH_TYPE_NONE \
          || (auth_type) == IPMI_SESSION_AUTH_TYPE_MD2 \
          || (auth_type) == IPMI_SESSION_AUTH_TYPE_MD5 \
          || (auth_type) == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY \
          || (auth_type) == IPMI_SESSION_AUTH_TYPE_OEM_PROP) ? 1 : 0) 

#define IPMI_PRIV_LEVEL_RESERVED     0x00
#define IPMI_PRIV_LEVEL_CALLBACK     0x01
#define IPMI_PRIV_LEVEL_USER         0x02
#define IPMI_PRIV_LEVEL_OPERATOR     0x03
#define IPMI_PRIV_LEVEL_ADMIN        0x04
#define IPMI_PRIV_LEVEL_OEM          0x05
#define IPMI_PRIV_LEVEL_NO_ACCESS    0x0F

#define IPMI_PRIV_LEVEL_VALID(priv_level) \
        (((priv_level) == IPMI_PRIV_LEVEL_RESERVED \
          || (priv_level) == IPMI_PRIV_LEVEL_CALLBACK \
          || (priv_level) == IPMI_PRIV_LEVEL_USER \
          || (priv_level) == IPMI_PRIV_LEVEL_OPERATOR \
          || (priv_level) == IPMI_PRIV_LEVEL_ADMIN \
          || (priv_level) == IPMI_PRIV_LEVEL_OEM) ? 1 : 0)

#define IPMI_MAX_DRIVERS  5
#define IPMI_MAX_RETRIES  3
#define IPMI_POLL_INTERVAL_USECS  0x01

enum ipmi_mode
  {
    IPMI_MODE_DEFAULT = 0,
    IPMI_MODE_NONBLOCK = 1
  };
typedef enum ipmi_mode ipmi_mode_t;

enum ipmi_driver_type
  {
    IPMI_DEVICE_UNKNOWN = 0,
    IPMI_DEVICE_LAN = 1,
    IPMI_DEVICE_KCS = 2,
    IPMI_DEVICE_SMIC = 3,
    IPMI_DEVICE_BT = 4,
    IPMI_DEVICE_SSIF = 5
  };
typedef enum ipmi_driver_type ipmi_driver_type_t;

struct ipmi_device 
{
  ipmi_driver_type_t type;
  ipmi_mode_t        mode;
  u_int8_t           net_fn;
  u_int8_t           lun;
  union 
  {
    struct 
    {
      unsigned long      poll_interval_usecs;
      u_int8_t           retry_count:4;
      ipmi_locate_info_t locate_info;
      char               *dev_name;
      int                dev_fd; /* Used by FreeBSD /dev/io, SSIF /dev/i2c-0 */ 
      int                mutex_semid;
      
      struct 
      {
	fiid_template_t *tmpl_hdr_ptr;
	fiid_obj_t      obj_hdr;
      } rq;
      
      struct 
      {
	fiid_template_t *tmpl_hdr_ptr;
	fiid_obj_t      obj_hdr;
      } rs;
    } inband;
    
    struct 
    {
      int                local_sockfd;
      struct sockaddr    remote_host;
      unsigned int       remote_host_len;
      
      u_int8_t           auth_type;
      u_int8_t           challenge_string[IPMI_SESSION_CHALLENGE_STR_LEN];
      u_int32_t          session_id;
      u_int32_t          session_seq_num;
      u_int8_t           rq_seq;
      
      u_int8_t           username[IPMI_SESSION_MAX_USERNAME_LEN];
      u_int8_t           password[IPMI_SESSION_MAX_AUTH_CODE_LEN];
      u_int8_t           priv_level;
      
      struct 
      {
	fiid_template_t *tmpl_hdr_rmcp_ptr;
	fiid_obj_t      obj_hdr_rmcp;
	fiid_template_t *tmpl_hdr_session_ptr;
	fiid_obj_t      obj_hdr_session;
	fiid_template_t *tmpl_msg_hdr_ptr;
	fiid_obj_t      obj_msg_hdr;
	fiid_template_t *tmpl_msg_trlr_ptr;
	fiid_obj_t      obj_msg_trlr;
      } rq;
      
      struct 
      {
	fiid_template_t *tmpl_hdr_rmcp_ptr;
	fiid_obj_t      obj_hdr_rmcp;
	fiid_template_t *tmpl_hdr_session_ptr;
	fiid_obj_t      obj_hdr_session;
	fiid_template_t *tmpl_msg_hdr_ptr;
	fiid_obj_t      obj_msg_hdr;
	fiid_template_t *tmpl_msg_trlr_ptr;
	fiid_obj_t      obj_msg_trlr;
      } rs;
    } outofband;
  } io;
};
typedef struct ipmi_device ipmi_device_t;

int ipmi_open_inband (ipmi_device_t *dev, 
		      ipmi_driver_type_t driver_type, 
		      ipmi_mode_t mode);
int ipmi_open_outofband (ipmi_device_t *dev, 
			 ipmi_driver_type_t driver_type, 
			 ipmi_mode_t mode, 
			 struct sockaddr *remote_host, 
			 size_t remote_host_len, 
			 u_int8_t auth_type, 
			 char *username, 
			 char *password, 
			 u_int8_t priv_level);
int ipmi_close (ipmi_device_t *dev);
int ipmi_cmd (ipmi_device_t *dev, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_template_t tmpl_cmd_rq, 
	      fiid_obj_t obj_cmd_rs, 
	      fiid_template_t tmpl_cmd_rs);
int ipmi_cmd_raw (ipmi_device_t *dev, 
		  u_int8_t *in, 
		  size_t in_len, 
		  u_int8_t *out, 
		  size_t *out_len);

#endif /* _IPMI_INTERFACE_H */
