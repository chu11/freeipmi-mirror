/*
  Copyright (C) 2006 FreeIPMI Core Team

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

#include <stdio.h>

#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"

#include "ipmi-sensors.h"

#include "pstdout.h"

int
ipmi_sensors_output_event_message_list (ipmi_sensors_state_data_t *state_data,
                                        char **event_message_list,
                                        unsigned int event_message_list_len)
{
  assert(state_data);
  
  if (event_message_list)
    {
      int i;
      
      for (i = 0; i < event_message_list_len; i++)
        pstdout_printf (state_data->pstate,
                        "[%s]",
                        event_message_list[i]);
      pstdout_printf (state_data->pstate,
                      "\n");
    }
  else 
    pstdout_printf (state_data->pstate,
                    "[%s]\n", 
                    "OK");
  
  return 0;
}
