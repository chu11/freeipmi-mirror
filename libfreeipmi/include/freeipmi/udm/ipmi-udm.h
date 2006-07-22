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

#include <sys/socket.h>

#include <freeipmi/ipmi-error.h>
#include <freeipmi/ipmi-kcs-api.h>
#include <freeipmi/ipmi-locate.h>
#include <freeipmi/ipmi-messaging-support-cmds.h>
#include <freeipmi/ipmi-ssif-api.h>

#define IPMI_MAX_DRIVERS  5
#define IPMI_MAX_RETRIES  3
#define IPMI_POLL_INTERVAL_USECS 60

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

typedef struct ipmi_device *ipmi_device_t;
 
ipmi_device_t ipmi_open_inband (int disable_auto_probe, 
                                ipmi_driver_type_t driver_type, 
                                uint16_t driver_address, 
                                uint8_t reg_space,
                                char *driver_device, 
                                ipmi_mode_t mode);

ipmi_device_t ipmi_open_outofband (ipmi_driver_type_t driver_type, 
                                   ipmi_mode_t mode, 
                                   unsigned int session_timeout,
                                   unsigned int retry_timeout, 
                                   struct sockaddr *remote_host, 
                                   size_t remote_host_len, 
                                   uint8_t authentication_type, 
                                   char *username, 
                                   char *password, 
                                   uint8_t privilege_level);

int ipmi_cmd (ipmi_device_t dev, 
	      uint8_t lun, 
	      uint8_t net_fn, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_obj_t obj_cmd_rs);

int ipmi_cmd_raw (ipmi_device_t dev, 
                  uint8_t lun, 
                  uint8_t net_fn, 
		  uint8_t *in, 
		  size_t in_len, 
		  uint8_t *out, 
		  size_t out_len);

void ipmi_close_device (ipmi_device_t dev);

#endif /* _IPMI_UDM_H */
