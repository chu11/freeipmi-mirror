/*
   ipmi-device-global-cmds-udm.h - IPMI UDM Device Global Commands

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

#ifndef _IPMI_DEVICE_GLOBAL_CMDS_UDM_H
#define	_IPMI_DEVICE_GLOBAL_CMDS_UDM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/fiid.h>
#include <freeipmi/udm/ipmi-udm.h>

int8_t ipmi_cmd_get_device_id (ipmi_device_t dev, fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_DEVICE_GLOBAL_CMDS_UDM_H */
