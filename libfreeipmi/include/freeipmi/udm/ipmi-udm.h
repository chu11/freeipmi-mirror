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

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <sys/socket.h>
#include <freeipmi/ipmi-error.h>
#include <freeipmi/ipmi-kcs-api.h>
#include <freeipmi/ipmi-locate.h>
#include <freeipmi/ipmi-messaging-support-cmds.h>
#include <freeipmi/ipmi-ssif-api.h>

enum ipmi_driver_type
  {
    IPMI_DEVICE_UNKNOWN = 0,
    IPMI_DEVICE_LAN = 1,
    IPMI_DEVICE_KCS = 2,
    IPMI_DEVICE_SMIC = 3,
    IPMI_DEVICE_BT = 4,
    IPMI_DEVICE_SSIF = 5,
    IPMI_DEVICE_OPENIPMI = 6,
  };
typedef enum ipmi_driver_type ipmi_driver_type_t;

#define IPMI_FLAGS_DEFAULT        0x00000000
#define IPMI_FLAGS_NONBLOCKING    0x00000001
#define IPMI_FLAGS_DEBUG_DUMP     0x00000010

typedef struct ipmi_device *ipmi_device_t;
 
ipmi_device_t ipmi_open_inband (ipmi_driver_type_t driver_type, 
				int disable_auto_probe, 
                                uint16_t driver_address, 
                                uint8_t reg_space,
                                char *driver_device, 
                                uint32_t flags);

ipmi_device_t ipmi_open_outofband (ipmi_driver_type_t driver_type, 
				   char *hostname,
                                   char *username, 
                                   char *password, 
                                   uint8_t authentication_type, 
                                   uint8_t privilege_level,
                                   unsigned int session_timeout,
                                   unsigned int retry_timeout, 
                                   uint32_t flags);

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

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_UDM_H */
