/*
 * Copyright (C) 2007-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef _IPMI_PEF_CONFIG_COMMUNITY_STRING_H_
#define _IPMI_PEF_CONFIG_COMMUNITY_STRING_H_

#include "ipmi-pef-config.h"
#include "ipmi-pef-config-sections.h"

struct config_section * ipmi_pef_config_community_string_section_get (ipmi_pef_config_state_data_t *state_data,
								      unsigned int config_flags,
								      int channel_index);

#endif /* _IPMI_PEF_CONFIG_COMMUNITY_STRING_H_ */
