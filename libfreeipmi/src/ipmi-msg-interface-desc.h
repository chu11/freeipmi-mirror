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

int ipmi_get_system_software_type (uint8_t system_software_id);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-msg-interface-desc.h */
