/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifndef _BMC_CONFIG_UTILS_H
#define _BMC_CONFIG_UTILS_H

#include "bmc-config.h"

config_err_t get_lan_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_serial_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_sol_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_number_of_users (bmc_config_state_data_t *state_data, uint8_t *number_of_users);

#endif
