/* 
   Copyright (C) 2005 FreeIPMI Core Team

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

#ifndef _IPMI_SENSOR_API_H
#define _IPMI_SENSOR_API_H

#include <stdint.h>

#include "freeipmi/udm/ipmi-udm.h"

#include "ipmi-sdr-cache.h"

struct sensor_reading
{
  double current_reading;
  uint8_t reading_state;
  uint8_t sensor_scanning;
  uint8_t event_messages_flag;
  char **event_message_list;
};
typedef struct sensor_reading sensor_reading_t;

enum ipmi_sensor_class
  {
    IPMI_SENSOR_CLASS_NOT_AVAILABLE, 
    IPMI_SENSOR_CLASS_THRESHOLD, 
    IPMI_SENSOR_CLASS_GENERIC_DISCRETE, 
    IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE, 
    IPMI_SENSOR_CLASS_OEM
  };

int ipmi_sensor_classify (uint8_t event_reading_type_code);

const char *ipmi_get_sensor_group (int sensor_type);

int8_t get_sensor_reading (ipmi_ctx_t ctx, 
                           int debug,
                           sdr_record_t *sdr_record, 
                           sensor_reading_t *sensor_reading);

void sensor_reading_cleanup(sensor_reading_t *sensor_reading);

#endif
