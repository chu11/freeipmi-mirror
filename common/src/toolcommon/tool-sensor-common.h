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

#ifndef _IPMI_SENSOR_COMMON_H
#define _IPMI_SENSOR_COMMON_H

#include <stdint.h>

#define SENSOR_CLASS_NOT_AVAILABLE            0x01
#define SENSOR_CLASS_THRESHOLD                0x02
#define SENSOR_CLASS_GENERIC_DISCRETE         0x03
#define SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE 0x04
#define SENSOR_CLASS_OEM                      0x05

int sensor_classify (uint8_t event_reading_type_code);

const char *sensor_group (int sensor_type);

#endif
