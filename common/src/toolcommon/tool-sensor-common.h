/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifndef _TOOL_SENSOR_COMMON_H
#define _TOOL_SENSOR_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include <freeipmi/freeipmi.h>

#include "pstdout.h"

#define UNRECOGNIZED_SENSOR_GROUP "Unrecognized"

const char * get_sensor_group_output_string (unsigned int sensor_type);

void get_sensor_group_cmdline_string (char *sensor_group);

int display_sensor_group_cmdline (pstdout_state_t pstate, 
                                  unsigned int sensor_type);

int display_string_cmdline (pstdout_state_t pstate, 
                            const char *str);

#endif
