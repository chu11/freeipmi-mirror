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

#include "ipmi-sensor-common.h"
#include "pstdout.h"

static int 
sensors_display_verbose_full_record (ipmi_sensors_state_data_t *state_data,
                                     int record_id, 
				     sdr_full_record_t *record, 
				     sensor_reading_t *sensor_reading)
{
  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  record->sensor_name);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n",
                  ipmi_get_sensor_group (record->sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  record->sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  record->event_reading_type_code);
  if (record->readable_threshold_lower_critical_threshold)
    {
      pstdout_printf (state_data->pstate,
                      "Lower Critical Threshold: %f %s\n", 
                      record->lower_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_upper_critical_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Critical Threshold: %f %s\n", 
                      record->upper_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_lower_non_critical_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Critical Threshold: %f %s\n", 
                      record->lower_non_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_upper_non_critical_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Critical Threshold: %f %s\n", 
                      record->upper_non_critical_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Critical Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_lower_non_recoverable_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Recoverable Threshold: %f %s\n", 
                      record->lower_non_recoverable_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Lower Non-Recoverable Threshold: %s\n", 
                      "NA");
    }
  if (record->readable_threshold_upper_non_recoverable_threshold)
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Recoverable Threshold: %f %s\n", 
                      record->upper_non_recoverable_threshold, 
                      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      pstdout_printf (state_data->pstate, 
                      "Upper Non-Recoverable Threshold: %s\n", 
                      "NA");
    }
  pstdout_printf (state_data->pstate, 
                  "Sensor Min. Reading: %f %s\n", 
                  record->sensor_minimum_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Sensor Max. Reading: %f %s\n", 
                  record->sensor_maximum_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Normal Min.: %f %s\n", 
                  record->normal_minimum, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Normal Max.: %f %s\n", 
                  record->normal_maximum, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Nominal reading: %f %s\n", 
                  record->nominal_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  if (sensor_reading == NULL)
    {
      pstdout_printf (state_data->pstate, 
                      "Sensor Reading: %s\n", 
                      "NA");
      pstdout_printf (state_data->pstate, 
                      "Sensor Status: [%s]\n\n", 
                      "Unknown");
    }
  else 
    {
      pstdout_printf (state_data->pstate, 
                      "Sensor Reading: %f %s\n", 
                      sensor_reading->current_reading, 
                      ipmi_sensor_units[record->sensor_unit]);
      if (sensor_reading->event_message_list == NULL)
	{
	  pstdout_printf (state_data->pstate, 
                          "Sensor Status: [%s]\n\n", 
                          "OK");
	}
      else 
	{
	  int i;
	  
	  pstdout_printf (state_data->pstate, 
                          "Sensor Status: ");
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      pstdout_printf (state_data->pstate, 
                              "[%s]",
                              sensor_reading->event_message_list[i]);
	    }
	  pstdout_printf (state_data->pstate, 
                          "\n\n");
	}
    }
  
  return 0;
}

static int 
sensors_display_verbose_compact_record (ipmi_sensors_state_data_t *state_data,
                                        int record_id, 
					sdr_compact_record_t *record, 
					sensor_reading_t *sensor_reading)
{
  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  record->sensor_name);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n", 
                  ipmi_get_sensor_group (record->sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  record->sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  record->event_reading_type_code);
  if (sensor_reading == NULL)
    {
      pstdout_printf (state_data->pstate, 
                      "Sensor Status: [%s]\n\n", 
                      "Unknown");
    }
  else 
    {
      if (sensor_reading->event_message_list == NULL)
	{
	  pstdout_printf (state_data->pstate, 
                          "Sensor Status: [%s]\n\n", 
                          "OK");
	}
      else 
	{
	  int i;
	  
	  pstdout_printf (state_data->pstate, 
                          "Sensor Status: ");
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      pstdout_printf (state_data->pstate, 
                              "[%s]", 
                              sensor_reading->event_message_list[i]);
	    }
	  pstdout_printf (state_data->pstate, 
                          "\n\n");
	}
    }
  
  return 0;
}

static int 
sensors_display_verbose_event_only_record (ipmi_sensors_state_data_t *state_data,
                                           int record_id, 
					   sdr_event_only_record_t *record, 
					   sensor_reading_t *sensor_reading)
{
  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  record->sensor_name);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n", 
                  ipmi_get_sensor_group (record->sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  record->sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  record->event_reading_type_code);
  if (sensor_reading == NULL)
    {
      pstdout_printf (state_data->pstate, 
                      "Sensor Status: [%s]\n\n", 
                      "Unknown");
    }
  else 
    {
      if (sensor_reading->event_message_list == NULL)
	{
	  pstdout_printf (state_data->pstate, 
                          "Sensor Status: [%s]\n\n", 
                          "OK");
	}
      else 
	{
	  int i;
	  
	  pstdout_printf (state_data->pstate, 
                          "Sensor Status: ");
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      pstdout_printf (state_data->pstate, 
                              "[%s]", 
                              sensor_reading->event_message_list[i]);
	    }
	  pstdout_printf (state_data->pstate, 
                          "\n\n");
	}
    }
  
  return 0;
}

int 
sensors_display_verbose (ipmi_sensors_state_data_t *state_data,
                         sdr_record_t *sdr_record, 
                         sensor_reading_t *sensor_reading)
{
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return sensors_display_verbose_full_record (state_data,
                                                  sdr_record->record_id, 
						  &(sdr_record->record.sdr_full_record), 
						  sensor_reading);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_verbose_compact_record (state_data,
                                                     sdr_record->record_id, 
						     &(sdr_record->record.sdr_compact_record), 
						     sensor_reading);
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return sensors_display_verbose_event_only_record (state_data,
                                                        sdr_record->record_id, 
							&(sdr_record->record.sdr_event_only_record), 
							sensor_reading);
    }
  
  return (0);
}
