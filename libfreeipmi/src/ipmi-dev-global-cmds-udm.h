/*
   ipmi-dev-global-cmds-udm.h - IPMI UDM Device Global Commands

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

#ifndef _IPMI_DEV_GLOBAL_CMDS_UDM_H
#define	_IPMI_DEV_GLOBAL_CMDS_UDM_H

#ifdef __cplusplus
extern "C" {
#endif

int8_t ipmi_cmd_get_dev_id (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-dev-global-cmds.h */
