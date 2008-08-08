/* 
   Copyright (C) 2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/


#ifndef _IPMI_CHASSIS_CONFIG_FRONT_PANEL_BUTTONS_H_
#define _IPMI_CHASSIS_CONFIG_FRONT_PANEL_BUTTONS_H_

#include "ipmi-chassis-config.h"
#include "ipmi-chassis-config-sections.h"

struct config_section * ipmi_chassis_config_front_panel_buttons_get (ipmi_chassis_config_state_data_t *state_data);

#endif /* _IPMI_CHASSIS_CONFIG_FRONT_PANEL_BUTTONS_H_ */
