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


#ifndef _IPMI_SENSORS_CONFIG_SENSOR_EVENT_ENABLE_COMMON_H_
#define _IPMI_SENSORS_CONFIG_SENSOR_EVENT_ENABLE_COMMON_H_

#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-sections.h"

config_err_t sensor_event_enable_enable_all_event_messages_checkout (const char *section_name,
                                                                     struct config_keyvalue *kv,
                                                                     void *arg);
  
config_err_t sensor_event_enable_enable_all_event_messages_commit (const char *section_name,
                                                                   const struct config_keyvalue *kv,
                                                                   void *arg);

config_err_t sensor_event_enable_enable_scanning_on_this_sensor_checkout (const char *section_name,
                                                                          struct config_keyvalue *kv,
                                                                          void *arg);

config_err_t sensor_event_enable_enable_scanning_on_this_sensor_commit (const char *section_name,
                                                                        const struct config_keyvalue *kv,
                                                                        void *arg);

int setup_sensor_event_enable_fields (ipmi_sensors_config_state_data_t *state_data,
                                      uint8_t *sdr_record,
                                      unsigned int sdr_record_len,
                                      struct config_section *section);

#endif /* _IPMI_SENSORS_CONFIG_SENSOR_EVENT_ENABLE_COMMON_H_ */
