/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _PEF_CONFIG_MAP_H
#define _PEF_CONFIG_MAP_H

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
