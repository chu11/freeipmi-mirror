/* 
   ipmi-udm.h: IPMI Unified Driver Model (API interface for all IPMI drivers)

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifndef _IPMI_UDM_H
#define _IPMI_UDM_H

#if defined (FREEIPMI_BUILD)
#include "ipmi-messaging-support-cmds.h"
#else
#include <freeipmi/ipmi-messaging-support-cmds.h>
#endif

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
      ipmi_locate_info_t locate_info;
      ipmi_kcs_ctx_t     kcs_ctx;
      ipmi_ssif_ctx_t    ssif_ctx;

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
      
      uint8_t           authentication_type;
      uint8_t           challenge_string[IPMI_CHALLENGE_STRING_LENGTH];
      uint32_t          session_id;
      uint32_t          session_sequence_number;
      uint8_t           rq_seq;
      
      uint8_t           username[IPMI_MAX_USER_NAME_LENGTH];
      uint8_t           password[IPMI_MAX_AUTHENTICATION_CODE_LENGTH];
      uint8_t           privilege_level;
      
      struct 
      {
	fiid_template_t *tmpl_rmcp_hdr_ptr;
	fiid_obj_t      obj_rmcp_hdr;
	fiid_template_t *tmpl_lan_session_hdr_ptr;
	fiid_obj_t      obj_lan_session_hdr;
	fiid_template_t *tmpl_lan_msg_hdr_ptr;
	fiid_obj_t      obj_lan_msg_hdr;
	fiid_template_t *tmpl_lan_msg_trlr_ptr;
	fiid_obj_t      obj_lan_msg_trlr;
      } rq;
      
      struct 
      {
	fiid_template_t *tmpl_rmcp_hdr_ptr;
	fiid_obj_t      obj_rmcp_hdr;
	fiid_template_t *tmpl_lan_session_hdr_ptr;
	fiid_obj_t      obj_lan_session_hdr;
	fiid_template_t *tmpl_lan_msg_hdr_ptr;
	fiid_obj_t      obj_lan_msg_hdr;
	fiid_template_t *tmpl_lan_msg_trlr_ptr;
	fiid_obj_t      obj_lan_msg_trlr;
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
			 uint8_t authentication_type, 
			 char *username, 
			 char *password, 
			 uint8_t privilege_level);
int ipmi_close (ipmi_device_t *dev);
int ipmi_cmd (ipmi_device_t *dev, 
	      uint8_t lun, 
	      uint8_t net_fn, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_obj_t obj_cmd_rs);
int ipmi_cmd_raw (ipmi_device_t *dev, 
		  uint8_t *in, 
		  size_t in_len, 
		  uint8_t *out, 
		  size_t *out_len);

#endif /* _IPMI_UDM_H */
