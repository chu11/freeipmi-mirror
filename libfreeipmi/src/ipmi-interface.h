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

/* XXX: need to remove */
#define IPMI_INTERFACE_MAX_USERNAME_LEN     16
#define IPMI_INTERFACE_CHALLENGE_STR_LEN    16
#define IPMI_INTERFACE_MAX_AUTH_CODE_LEN    16

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
  uint8_t            lun;
  uint8_t            net_fn;
  uint8_t            cmd;
  uint8_t            comp_code;
  char               errmsg[IPMI_ERR_STR_MAX_LEN];
  union 
  {
    struct 
    {
      unsigned long      poll_interval_usecs; /* obsolete entry */
      int                disable_auto_probe;
      uint16_t           driver_address; /* also known as sms_io_base/ipmb_addr */
      char               *driver_device; /* also known as dev_name */
      uint8_t            retry_count:4;
      ipmi_locate_info_t locate_info;
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
      
      uint8_t           auth_type;
      uint8_t           challenge_string[IPMI_INTERFACE_CHALLENGE_STR_LEN];
      uint32_t          session_id;
      uint32_t          session_seq_num;
      uint8_t           rq_seq;
      
      uint8_t           username[IPMI_INTERFACE_MAX_USERNAME_LEN];
      uint8_t           password[IPMI_INTERFACE_MAX_AUTH_CODE_LEN];
      uint8_t           priv_level;
      
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
		      int disable_auto_probe, 
		      ipmi_driver_type_t driver_type, 
		      uint16_t driver_address, 
		      uint8_t reg_space,
		      char *driver_device, 
		      ipmi_mode_t mode);
int ipmi_open_outofband (ipmi_device_t *dev, 
			 ipmi_driver_type_t driver_type, 
			 ipmi_mode_t mode, 
			 struct sockaddr *remote_host, 
			 size_t remote_host_len, 
			 uint8_t auth_type, 
			 char *username, 
			 char *password, 
			 uint8_t priv_level);
int ipmi_close (ipmi_device_t *dev);
int ipmi_cmd (ipmi_device_t *dev, 
	      uint8_t lun, 
	      uint8_t net_fn, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_template_t tmpl_cmd_rq, 
	      fiid_obj_t obj_cmd_rs, 
	      fiid_template_t tmpl_cmd_rs);
int ipmi_cmd_raw (ipmi_device_t *dev, 
		  uint8_t *in, 
		  size_t in_len, 
		  uint8_t *out, 
		  size_t *out_len);

#endif /* _IPMI_INTERFACE_H */
