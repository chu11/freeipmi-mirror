/*
 * Copyright (C) 2008-2013 FreeIPMI Core Team
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

#ifndef IPMI_SENSORS_CONFIG_UTILS_H
#define IPMI_SENSORS_CONFIG_UTILS_H

#include "ipmi-sensors-config.h"

config_err_t convert_id_string (ipmi_sensors_config_state_data_t *state_data,
                                char *id_string);

config_err_t convert_event_string (ipmi_sensors_config_state_data_t *state_data,
                                   char *event_string);

config_err_t create_section_name (ipmi_sensors_config_state_data_t *state_data,
                                  char *section_name,
                                  unsigned int section_name_len);

config_err_t seek_to_sdr_record (ipmi_sensors_config_state_data_t *state_data,
				 const char *section_name);

#endif /* IPMI_SENSORS_CONFIG_UTILS_H */
