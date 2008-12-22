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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "freeipmi/spec/ipmi-sensor-types-spec.h"

#include "freeipmi-portability.h"
#include "tool-sensor-common.h"

const char *
sensor_group (int sensor_type)
{
  if (IPMI_SENSOR_TYPE_VALID(sensor_type))
    return (ipmi_sensor_types[sensor_type]);
  
  if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
    return ipmi_oem_sensor_type;

  return NULL;
}

