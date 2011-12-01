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

#ifndef _IPMI_PEF_CONFIG_MAP_H
#define _IPMI_PEF_CONFIG_MAP_H

int alert_destination_type_number (const char *source);

char *alert_destination_type_string (uint8_t source);

int alert_gateway_number (const char *source);

char *alert_gateway_string (uint8_t source);

int policy_type_number (const char *source);

char *policy_type_string (uint8_t source);

int filter_type_number (const char *source);

char *filter_type_string (uint8_t source);

int event_severity_number (const char *source);

char *event_severity_string (uint8_t source);

int sensor_type_number (const char *source);

char *sensor_type_string (uint8_t source);

#endif
