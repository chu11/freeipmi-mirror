/*
 * Copyright (C) 2007-2013 FreeIPMI Core Team
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

#ifndef IPMI_PEF_CONFIG_UTILS_H
#define IPMI_PEF_CONFIG_UTILS_H

#include <stdint.h>

#include "ipmi-pef-config.h"

config_err_t load_lan_channel_numbers (ipmi_pef_config_state_data_t *state_data);

config_err_t get_lan_channel_number (struct ipmi_pef_config_state_data *state_data,
				     const char *section_name,
                                     uint8_t *channel_number);

#endif /* IPMI_PEF_CONFIG_UTILS_H */
