/* 
   ipmi-msg-interface-desc.h - IPMI Network Function Specification

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

#ifndef _IPMI_MSG_INTERFACE_DESC_H
#define	_IPMI_MSG_INTERFACE_DESC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_NET_FN_LUN(net_fn)  (net_fn & 0x03)
#define IPMI_NET_FN_FN(net_fn)   (net_fn >> 2)

/* Notes:
   Refer to IPMIv1_5_rev1_1.pdf Table 5-1, Network Function Codes
   for complete description
*/
#define IPMI_NET_FN_CHASSIS_RQ         0x00
#define IPMI_NET_FN_CHASSIS_RS	       0x01
#define IPMI_NET_FN_BRIDGE_RQ	       0x02
#define IPMI_NET_FN_BRIDGE_RS	       0x03
#define IPMI_NET_FN_SENSOR_EVENT_RQ    0x04
#define IPMI_NET_FN_SENSOR_EVENT_RS    0x05
#define IPMI_NET_FN_APP_RQ	       0x06
#define IPMI_NET_FN_APP_RS	       0x07
#define IPMI_NET_FN_FIRMWARE_RQ	       0x08
#define IPMI_NET_FN_FIRMWARE_RS	       0x09
#define IPMI_NET_FN_STORAGE_RQ	       0x0A
#define IPMI_NET_FN_STORAGE_RS	       0x0B
#define IPMI_NET_FN_TRANSPORT_RQ       0x0C
#define IPMI_NET_FN_TRANSPORT_RS       0x0D
/*
   0x0E to 0x2B RESERVED
   0x2C to 0x2D IPMI_NET_FN_GRP
   0x2E to 0x2F IPMI_NET_FN_OEM_GRP
   0x30 to 0x3F IPMI_NET_FN_CNTRLR_OEM_GRP
*/

/* FIXME: Use "Get Channel Info" IPMI command to probe channel info
   dynamically, instead of using the following macros.
   -- ab@gnu.org.in */
#define IPMI_CHANNEL_SR870BN4_IPMB         0x00
#define IPMI_CHANNEL_SR870BN4_EMP          0x01
#define IPMI_CHANNEL_SR870BN4_PCI_SMBUS    0x03
#define IPMI_CHANNEL_SR870BN4_SMM          0x04
#define IPMI_CHANNEL_SR870BN4_LAN1         0x07
#define IPMI_CHANNEL_SR870BN4_SMS          0x0F


enum system_software_type
  {
    IPMI_BIOS, 
    IPMI_SMI_HANDLER, 
    IPMI_SYSTEM_MANAGEMENT_SOFTWARE, 
    IPMI_OEM, 
    IPMI_REMOTE_CONSOLE_SOFTWARE, 
    IPMI_TERMINAL_MODE_REMOTE_CONSOLE_SOFTWARE, 
    IPMI_SYS_SOFT_ID_RESERVED
  };

extern const char *ipmi_system_software_type_desc[];

/* #pragma pack(1) */
/* typedef struct net_fn */
/* { */
/*   uint8_t lun:2; */
/*   uint8_t fn:6; */
/* } net_fn_t; */
/* #pragma pack(0) */

uint8_t ipmi_netfn2byte (net_fn_t net_fn);

int ipmi_get_system_software_type (uint8_t system_software_id);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-msg-interface-desc.h */
