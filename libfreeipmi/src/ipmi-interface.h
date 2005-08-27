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
  unsigned long      poll_interval_usecs;
  u_int8_t           retry_count:4;
  
  union 
  {
    struct 
    {
      struct 
      {
	fiid_template_t *tmpl_hdr_ptr;
	fiid_obj_t      obj_hdr;
	/* Better pass them as arguments to ipmi_INTERFACE_cmd */
	/*  functions -- Anand Babu */
	/* 	fiid_template_t *tmpl_cmd_ptr; */
	/* 	fiid_obj_t obj_cmd; */
      } rq;
      struct 
      {
	fiid_template_t *tmpl_hdr_ptr;
	fiid_obj_t      obj_hdr;
	/* Better pass them as arguments to ipmi_INTERFACE_cmd */
	/*  functions -- Anand Babu */
	/* 	fiid_template_t *tmpl_cmd_ptr; */
	/* 	fiid_obj_t obj_cmd; */
      } rs;
    } inband;
    struct 
    {
      struct 
      {
	fiid_template_t *tmpl_hdr_rmcp_ptr;
	fiid_obj_t      obj_hdr_rmcp;
	fiid_template_t *tmpl_hdr_session_ptr;
	fiid_obj_t      obj_hdr_session;
	fiid_template_t *tmpl_msg_hdr_ptr;
	fiid_obj_t      obj_msg_hdr;
	/* Better pass them as arguments to ipmi_INTERFACE_cmd */
	/*  functions -- Anand Babu */
	/* 	fiid_template_t *tmpl_cmd_ptr; */
	/* 	fiid_obj_t obj_cmd; */
      } rq;
      struct 
      {
	fiid_template_t *tmpl_hdr_rmcp_ptr;
	fiid_obj_t      obj_hdr_rmcp;
	fiid_template_t *tmpl_hdr_session_ptr;
	fiid_obj_t      obj_hdr_session;
	fiid_template_t *tmpl_msg_hdr_ptr;
	fiid_obj_t      obj_msg_hdr;
	/* Better pass them as arguments to ipmi_INTERFACE_cmd */
	/*  functions -- Anand Babu */
	/* 	fiid_template_t *tmpl_cmd_ptr; */
	/* 	fiid_obj_t obj_cmd; */
      } rs;
    } outofband;
  } io;
  
  struct 
  {
    ipmi_locate_info_t locate_info;
    char               *dev_name;
    int                dev_fd; /* Used by FreeBSD /dev/io, 
				  SSIF /dev/i2c-0 and LAN sockfd. */ 
    int                local_sockfd;
    struct sockaddr    *remote_host;
    unsigned int       remote_host_len;
    
    u_int8_t           auth_type;
    u_int32_t          initial_outbound_seq_num;
    u_int32_t          session_id;
    u_int32_t          session_seq_num;
    u_int8_t           rq_seq;
    
    char               *username;
    char               *password;
    u_int32_t          password_len;
    u_int8_t           priv_level;
    
    u_int8_t           net_fn;
    u_int8_t           lun;
    
    int                mutex_semid;
  } private;
};
typedef struct ipmi_device ipmi_device_t;

int ipmi_open_inband (ipmi_device_t *dev, 
		      ipmi_driver_type_t driver_type, 
		      ipmi_mode_t mode, 
		      u_int8_t lun, 
		      u_int8_t fn);
int ipmi_open_outofband (ipmi_device_t *dev, 
			 ipmi_driver_type_t driver_type, 
			 ipmi_mode_t mode, 
			 struct sockaddr *remote_host, 
			 size_t remote_host_len, 
			 u_int8_t auth_type, 
			 char *username, 
			 char *password, 
			 u_int8_t priv_level, 
			 u_int8_t net_fn, 
			 u_int8_t lun);
int ipmi_close (ipmi_device_t *dev);
int ipmi_cmd (ipmi_device_t *dev, 
	      fiid_template_t tmpl_cmd_rq, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_template_t tmpl_cmd_rs, 
	      fiid_obj_t obj_cmd_rs);
int ipmi_cmd_raw (ipmi_device_t *dev, 
		  char *in, 
		  size_t in_len, 
		  char *out, 
		  size_t *out_len);

#endif /* _IPMI_INTERFACE_H */
