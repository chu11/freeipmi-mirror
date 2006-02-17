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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _IPMI_MSG_INTERFACE_DESC_H
#define	_IPMI_MSG_INTERFACE_DESC_H

#ifdef __cplusplus
extern "C" {
#endif

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
