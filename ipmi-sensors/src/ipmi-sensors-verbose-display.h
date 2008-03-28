/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifndef _IPMI_SENSORS_VERBOSE_DISPLAY_H
#define _IPMI_SENSORS_VERBOSE_DISPLAY_H

#include "ipmi-sensors.h"

int ipmi_sensors_display_verbose (ipmi_sensors_state_data_t *state_data,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len,
                                  double *reading,
                                  char **event_message_list,
                                  unsigned int event_message_list_len);

#endif
