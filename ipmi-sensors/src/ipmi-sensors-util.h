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

#ifndef _IPMI_SENSORS_UTIL_H
#define _IPMI_SENSORS_UTIL_H

#include "ipmi-sensors.h"

int get_msg_message_list (struct ipmi_sensors_state_data *state_data,
                          char ***event_message_list,
                          unsigned int *event_message_list_len,
                          char *msg);

int get_threshold_message_list (struct ipmi_sensors_state_data *state_data,
                                char ***event_message_list,
                                unsigned int *event_message_list_len,
                                uint8_t sensor_event_bitmask,
                                char *no_event_msg);

int get_generic_event_message_list (struct ipmi_sensors_state_data *state_data,
                                    char ***event_message_list,
                                    unsigned int *event_message_list_len,
                                    uint8_t event_reading_type_code, 
                                    uint16_t sensor_event_bitmask,
                                    char *no_event_msg);

int get_sensor_specific_event_message_list (struct ipmi_sensors_state_data *state_data,
                                            char ***event_message_list,
                                            unsigned int *event_message_list_len,
                                            uint8_t sensor_type, 
                                            uint16_t sensor_event_bitmask,
                                            char *no_event_msg);

void str_replace_char (char *str, char chr, char with);

#endif
