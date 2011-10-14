/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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

#ifndef _BMC_CONFIG_UTILS_H
#define _BMC_CONFIG_UTILS_H

#include "bmc-config.h"

config_err_t load_lan_channel_numbers (bmc_config_state_data_t *state_data);

config_err_t load_serial_channel_numbers (bmc_config_state_data_t *state_data);

config_err_t load_sol_channel_numbers (bmc_config_state_data_t *state_data);

config_err_t get_lan_channel_number (bmc_config_state_data_t *state_data,
				     const char *section_name,
				     uint8_t *channel_number);

config_err_t get_serial_channel_number (bmc_config_state_data_t *state_data,
					const char *section_name,
					uint8_t *channel_number);

config_err_t get_sol_channel_number (bmc_config_state_data_t *state_data,
				     const char *section_name,
				     uint8_t *channel_number);

#endif
