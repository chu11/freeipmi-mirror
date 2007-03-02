/*
sensors-simple-display.c: basic mode display functions.
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
#include "ipmi-sensor-api.h"
#include "freeipmi/ipmi-sensor-units-spec.h"
#include "freeipmi/ipmi-sdr-record-types.h"

#include "ipmi-sensors.h"
#include "ipmi-sensors-utils.h"
#include "pstdout.h"

static int 
sensors_display_simple_full_record (ipmi_sensors_state_data_t *state_data,
                                    int record_id, 
				    sdr_full_record_t *record, 
				    sensor_reading_t *sensor_reading)
{
  pstdout_printf (state_data->pstate,
                  "%d: %s (%s): ", 
                  record_id, 
                  record->sensor_name, 
                  ipmi_get_sensor_group (record->sensor_type));
  
  switch (ipmi_sensor_classify (record->event_reading_type_code))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      if (sensor_reading == NULL)
	{
	  pstdout_printf (state_data->pstate,
                          "NA");
	}
      else 
	{
	  pstdout_printf (state_data->pstate,
                          "%.2f %s ", 
                          round_double2 (sensor_reading->current_reading), 
                          ipmi_sensor_units_abbreviated[record->sensor_unit]);
	}
      if (record->readable_threshold_lower_critical_threshold)
	{
	  pstdout_printf (state_data->pstate,
                          "(%.2f/", 
                          round_double2 (record->lower_critical_threshold));
	}
      else 
	{
	  pstdout_printf (state_data->pstate,
                          "(NA/");
	}
      if (record->readable_threshold_upper_critical_threshold)
	{
	  pstdout_printf (state_data->pstate,
                          "%.2f): ", 
                          round_double2 (record->upper_critical_threshold));
	}
      else 
	{
	  pstdout_printf (state_data->pstate,
                          "NA): ");
	}
    default:
      if (sensor_reading == NULL)
	{
	  pstdout_printf (state_data->pstate,
                          "[%s]\n", 
                          "Unknown");
	}
      else 
	{
	  if (sensor_reading->event_message_list == NULL)
	    {
	      pstdout_printf (state_data->pstate,
                              "[%s]\n", 
                              "OK");
	    }
	  else 
	    {
	      int i;
	      
	      for (i = 0; 
		   sensor_reading->event_message_list[i]; 
		   i++)
		{
		  pstdout_printf (state_data->pstate,
                                  "[%s]",
                                  sensor_reading->event_message_list[i]);
		}
	      pstdout_printf (state_data->pstate,
                              "\n");
	    }
	}
    }
  
  return 0;
}

static int 
sensors_display_simple_compact_record (ipmi_sensors_state_data_t *state_data,
                                       uint16_t record_id, 
				       sdr_compact_record_t *record, 
				       sensor_reading_t *sensor_reading)
{
  pstdout_printf (state_data->pstate,
                  "%d: %s (%s): ", 
                  record_id, 
                  record->sensor_name, 
                  ipmi_get_sensor_group (record->sensor_type));
  if (sensor_reading == NULL)
    {
      pstdout_printf (state_data->pstate,
                      "[%s]\n", 
                      "Unknown");
    }
  else 
    {
      if (sensor_reading->event_message_list == NULL)
	{
	  pstdout_printf (state_data->pstate,
                          "[%s]\n", 
                          "OK");
	}
      else 
	{
	  int i;
	  
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      pstdout_printf (state_data->pstate,
                              "[%s]", 
                              sensor_reading->event_message_list[i]);
	    }
	  pstdout_printf (state_data->pstate,
                          "\n");
	}
    }
  
  return 0;
}

int 
sensors_display_simple (ipmi_sensors_state_data_t *state_data,
                        sdr_record_t *sdr_record, 
                        sensor_reading_t *sensor_reading)
{
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return sensors_display_simple_full_record (state_data,
                                                 sdr_record->record_id, 
						 &(sdr_record->record.sdr_full_record), 
						 sensor_reading);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_simple_compact_record (state_data,
                                                    sdr_record->record_id, 
						    &(sdr_record->record.sdr_compact_record), 
						    sensor_reading);
    }
  
  return (0);
}
