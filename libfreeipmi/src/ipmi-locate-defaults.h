/* 
   ipmi-locate-defaults.h - Return default locate info for IPMI interfaces.

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

#ifndef _IPMI_LOCATE_DEFAULTS_H
#define _IPMI_LOCATE_DEFAULTS_H 1

ipmi_locate_info_t* ipmi_locate_defaults_get_dev_info (ipmi_interface_type_t type, ipmi_locate_info_t* pinfo);

#endif
