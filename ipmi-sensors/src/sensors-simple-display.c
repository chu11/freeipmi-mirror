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

static int 
sensors_display_simple_full_record (int record_id, 
				    sdr_full_record_t *record, 
				    sensor_reading_t *sensor_reading)
{
  printf ("%d: %s (%s):", 
	  record_id, 
	  record->sensor_name, 
	  ipmi_get_sensor_group (record->sensor_type));
  
  switch (ipmi_sensor_classify (record->event_reading_type_code))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      if (sensor_reading == NULL)
	{
	  printf (" NA");
	}
      else 
	{
	  printf ("%f %s", 
		  sensor_reading->current_reading, 
		  ipmi_sensor_units_abbreviated[record->sensor_unit]);
	}
      if (record->readable_threshold_lower_critical_threshold)
	{
	  printf ("(%f/", record->lower_critical_threshold);
	}
      else 
	{
	  printf ("(NA/");
	}
      if (record->readable_threshold_upper_critical_threshold)
	{
	  printf ("%f):", record->upper_critical_threshold);
	}
      else 
	{
	  printf ("NA):");
	}
    default:
      if (sensor_reading == NULL)
	{
	  printf ("[%s]\n", "Unknown");
	}
      else 
	{
	  if (sensor_reading->event_message_list == NULL)
	    {
	      printf ("[%s]\n", "OK");
	    }
	  else 
	    {
	      int i;
	      
	      for (i = 0; 
		   sensor_reading->event_message_list[i]; 
		   i++)
		{
		  printf ("[%s]", sensor_reading->event_message_list[i]);
		}
	      printf ("\n");
	    }
	}
    }
  
  return 0;
}

static int 
sensors_display_simple_compact_record (uint16_t record_id, 
				       sdr_compact_record_t *record, 
				       sensor_reading_t *sensor_reading)
{
  printf ("%d: %s (%s):", 
	  record_id, 
	  record->sensor_name, 
	  ipmi_get_sensor_group (record->sensor_type));
  if (sensor_reading == NULL)
    {
      printf ("[%s]\n", "Unknown");
    }
  else 
    {
      if (sensor_reading->event_message_list == NULL)
	{
	  printf ("[%s]\n", "OK");
	}
      else 
	{
	  int i;
	  
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      printf ("[%s]", sensor_reading->event_message_list[i]);
	    }
	  printf ("\n");
	}
    }
  
  return 0;
}

static int 
sensors_display_simple_event_only_record (uint16_t record_id, 
					  sdr_event_only_record_t *record, 
					  sensor_reading_t *sensor_reading)
{
  printf ("%d: %s (%s):", 
	  record_id, 
	  record->sensor_name, 
	  ipmi_get_sensor_group (record->sensor_type));
  if (sensor_reading == NULL)
    {
      printf ("[%s]\n", "Unknown");
    }
  else 
    {
      if (sensor_reading->event_message_list == NULL)
	{
	  printf ("[%s]\n", "OK");
	}
      else 
	{
	  int i;
	  
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      printf ("[%s]", sensor_reading->event_message_list[i]);
	    }
	  printf ("\n");
	}
    }
  
  return 0;
}

int 
sensors_display_simple (sdr_record_t *sdr_record, sensor_reading_t *sensor_reading)
{
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return sensors_display_simple_full_record (sdr_record->record_id, 
						 &(sdr_record->record.sdr_full_record), 
						 sensor_reading);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_simple_compact_record (sdr_record->record_id, 
						    &(sdr_record->record.sdr_compact_record), 
						    sensor_reading);
    }
  
  return (-1);
}
