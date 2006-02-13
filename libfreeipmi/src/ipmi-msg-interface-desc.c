/* 
   ipmi-msg-interface-desc.c - IPMI Network Function Specification

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

#include "freeipmi.h"

const char *ipmi_system_software_type_desc[] = 
  {
    "BIOS", 
    "SMI Handler", 
    "System Management Software", 
    "OEM", 
    "Remote Console software", 
    "Terminal Mode Remote Console software", 
    "Reserved", 
    NULL 
  };

int 
ipmi_get_system_software_type (uint8_t system_software_id)
{
  /* To avoid "warning: comparison is always true due to limited range of data type" */
  if ((system_software_id + 1) >= 1 && system_software_id <= 0x0F)
    return IPMI_BIOS;
  if (system_software_id >= 0x10 && system_software_id <= 0x1F)
    return IPMI_SMI_HANDLER;
  if (system_software_id >= 0x20 && system_software_id <= 0x2F)
    return IPMI_SYSTEM_MANAGEMENT_SOFTWARE;
  if (system_software_id >= 0x30 && system_software_id <= 0x3F)
    return IPMI_OEM;
  if (system_software_id >= 0x40 && system_software_id <= 0x46)
    return IPMI_REMOTE_CONSOLE_SOFTWARE;
  if (system_software_id == 0x47)
    return IPMI_TERMINAL_MODE_REMOTE_CONSOLE_SOFTWARE;
  
  return IPMI_SYS_SOFT_ID_RESERVED;
}
