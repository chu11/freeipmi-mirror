/*
sensors-very-verbose-display.c: very verbose mode display functions.
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
sensors_display_very_verbose_full_record (int record_id, 
					  int record_type, 
					  sdr_full_record_t *record, 
					  sensor_reading_t *sensor_reading)
{
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Sensor Name: %s\n", record->sensor_name);
  printf ("Group Name: %s\n", ipmi_get_sensor_group (record->sensor_type));
  printf ("Sensor Number: %d\n", record->sensor_number);
  printf ("Event/Reading Type Code: %Xh\n", record->event_reading_type_code);
  printf ("Slave Address/System Software ID: %Xh\n", 
	  record->sensor_owner_id);
  printf ("B: %d\n", record->b);
  printf ("M: %d\n", record->m);
  printf ("R Exponent: %d\n", record->r_exponent);
  printf ("B Exponent: %d\n", record->b_exponent);
  printf ("Linear: %d\n", record->linear);
  printf ("Analog Data Format: %d\n", record->analog_data_format);
  if (record->readable_threshold_lower_critical_threshold)
    {
      printf ("Lower Critical Threshold: %f %s\n", 
	      record->lower_critical_threshold, 
	      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      printf ("Lower Critical Threshold: %s\n", "NA");
    }
  if (record->readable_threshold_upper_critical_threshold)
    {
      printf ("Upper Critical Threshold: %f %s\n", 
	      record->upper_critical_threshold, 
	      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      printf ("Upper Critical Threshold: %s\n", "NA");
    }
  if (record->readable_threshold_lower_non_critical_threshold)
    {
      printf ("Lower Non-Critical Threshold: %f %s\n", 
	      record->lower_non_critical_threshold, 
	      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      printf ("Lower Non-Critical Threshold: %s\n", "NA");
    }
  if (record->readable_threshold_upper_non_critical_threshold)
    {
      printf ("Upper Non-Critical Threshold: %f %s\n", 
	      record->upper_non_critical_threshold, 
	      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      printf ("Upper Non-Critical Threshold: %s\n", "NA");
    }
  if (record->readable_threshold_lower_non_recoverable_threshold)
    {
      printf ("Lower Non-Recoverable Threshold: %f %s\n", 
	      record->lower_non_recoverable_threshold, 
	      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      printf ("Lower Non-Recoverable Threshold: %s\n", "NA");
    }
  if (record->readable_threshold_upper_non_recoverable_threshold)
    {
      printf ("Upper Non-Recoverable Threshold: %f %s\n", 
	      record->upper_non_recoverable_threshold, 
	      ipmi_sensor_units[record->sensor_unit]);
    }
  else
    {
      printf ("Upper Non-Recoverable Threshold: %s\n", "NA");
    }
  printf ("Sensor Min. Reading: %f %s\n", 
	  record->sensor_minimum_reading, 
	  ipmi_sensor_units[record->sensor_unit]);
  printf ("Sensor Max. Reading: %f %s\n", 
	  record->sensor_maximum_reading, 
	  ipmi_sensor_units[record->sensor_unit]);
  printf ("Normal Min.: %f %s\n", 
	  record->normal_minimum, 
	  ipmi_sensor_units[record->sensor_unit]);
  printf ("Normal Max.: %f %s\n", 
	  record->normal_maximum, 
	  ipmi_sensor_units[record->sensor_unit]);
  printf ("Nominal reading: %f %s\n", 
	  record->nominal_reading, 
	  ipmi_sensor_units[record->sensor_unit]);
  printf ("Negative Hysteresis: %d\n", record->negative_going_threshold_hysteresis);
  printf ("Positive Hysteresis: %d\n", record->positive_going_threshold_hysteresis);
  if (sensor_reading == NULL)
    {
      printf ("Sensor Reading: %s\n", "NA");
      printf ("Sensor Status: [%s]\n\n", "Unknown");
    }
  else 
    {
      printf ("Sensor Reading: %f %s\n", 
	      sensor_reading->current_reading, 
	      ipmi_sensor_units[record->sensor_unit]);
      if (sensor_reading->event_message_list == NULL)
	{
	  printf ("Sensor Status: [%s]\n\n", "OK");
	}
      else 
	{
	  int i;
	  
	  printf ("Sensor Status: ");
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      printf ("[%s]", sensor_reading->event_message_list[i]);
	    }
	  printf ("\n\n");
	}
    }
  
  return 0;
}

static int 
sensors_display_very_verbose_compact_record (int record_id, 
					     int record_type, 
					     sdr_compact_record_t *record, 
					     sensor_reading_t *sensor_reading)
{
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Sensor Name: %s\n", record->sensor_name);
  printf ("Group Name: %s\n", ipmi_get_sensor_group (record->sensor_type));
  printf ("Sensor Number: %d\n", record->sensor_number);
  printf ("Event/Reading Type Code: %Xh\n", record->event_reading_type_code);
  printf ("Slave Address/System Software ID: %Xh\n", 
	  record->sensor_owner_id);
  printf ("Negative Hysteresis: %d\n", record->negative_going_threshold_hysteresis);
  printf ("Positive Hysteresis: %d\n", record->positive_going_threshold_hysteresis);
  if (sensor_reading == NULL)
    {
      printf ("Sensor Status: [%s]\n\n", "Unknown");
    }
  else 
    {
      if (sensor_reading->event_message_list == NULL)
	{
	  printf ("Sensor Status: [%s]\n\n", "OK");
	}
      else 
	{
	  int i;
	  
	  printf ("Sensor Status: ");
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      printf ("[%s]", sensor_reading->event_message_list[i]);
	    }
	  printf ("\n\n");
	}
    }
  
  return 0;
}

static int 
sensors_display_very_verbose_event_only_record (int record_id, 
						int record_type, 
						sdr_event_only_record_t *record, 
						sensor_reading_t *sensor_reading)
{
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Sensor Name: %s\n", record->sensor_name);
  printf ("Group Name: %s\n", ipmi_get_sensor_group (record->sensor_type));
  printf ("Sensor Number: %d\n", record->sensor_number);
  printf ("Event/Reading Type Code: %Xh\n", record->event_reading_type_code);
  printf ("Slave Address/System Software ID: %Xh\n", 
	  record->sensor_owner_id);
  if (sensor_reading == NULL)
    {
      printf ("Sensor Status: [%s]\n\n", "Unknown");
    }
  else 
    {
      if (sensor_reading->event_message_list == NULL)
	{
	  printf ("Sensor Status: [%s]\n\n", "OK");
	}
      else 
	{
	  int i;
	  
	  printf ("Sensor Status: ");
	  for (i = 0; 
	       sensor_reading->event_message_list[i]; 
	       i++)
	    {
	      printf ("[%s]", sensor_reading->event_message_list[i]);
	    }
	  printf ("\n\n");
	}
    }
  
  return 0;
}

static int 
sensors_display_very_verbose_entity_asso_record (int record_id, 
						 int record_type, 
						 sdr_entity_association_record_t *record, 
						 sensor_reading_t *sensor_reading)
{
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Sensor Name: %s\n", "NONE");
  printf ("Container Entity ID: %Xh\n", 
	  record->container_entity_id);
  printf ("Container Entity Instance: %Xh\n\n", 
	  record->container_entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_gen_dev_locator_record (int record_id, 
						     int record_type, 
						     sdr_generic_device_locator_record_t *record, 
						     sensor_reading_t *sensor_reading)
{
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Device Name: %s\n", record->device_name);
  printf ("Direct Access Address: %Xh\n", 
	  record->direct_access_address);
  printf ("Channel Number: %Xh\n", 
	  record->channel_number);
  printf ("Direct Slave Address: %Xh\n", 
	  record->device_slave_address);
  printf ("Access LUN/Bus ID: %Xh\n", 
	  record->lun_for_master_write_read_command);
  printf ("Address Span: %Xh\n", 
	  record->address_span);
  printf ("Device Type: %Xh\n", 
	  record->device_type);
  printf ("Device Type Modifier: %Xh\n", 
	  record->device_type_modifier);
  printf ("Entity ID: %Xh\n", 
	  record->entity_id);
  printf ("Entity Instance: %Xh\n\n", 
	  record->entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_fru_dev_locator_record (int record_id, 
						     int record_type, 
						     sdr_logical_fru_device_locator_record_t *record, 
						     sensor_reading_t *sensor_reading)
{
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Device Name: %s\n", record->device_name);
  printf ("Device Type: %Xh\n", 
	  record->device_type);
  printf ("Device Type Modifier: %Xh\n", 
	  record->device_type_modifier);
  printf ("FRU Entity ID: %Xh\n", 
	  record->fru_entity_id);
  printf ("FRU Entity Instance: %Xh\n\n", 
	  record->fru_entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_mgmt_cntrlr_dev_locator_record (int record_id, 
							     int record_type, 
							     sdr_management_controller_device_locator_record_t *record, 
							     sensor_reading_t *sensor_reading)
{
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Device Name: %s\n", record->device_name);
  printf ("Entity ID: %Xh\n", 
	  record->entity_id);
  printf ("Entity Instance: %Xh\n\n", 
	  record->entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_oem_record (int record_id, 
					 int record_type, 
					 sdr_oem_record_t *record, 
					 sensor_reading_t *sensor_reading)
{
  int i;
  
  printf ("Record ID: %d\n", record_id);
  printf ("Record Type: %Xh\n", record_type);
  printf ("Sensor Name: %s\n", "NONE");
  printf ("Manufacturer ID: %Xh\n", 
	  record->manufacturer_id);
  printf ("OEM Data: ");
  for (i = 0; i < 55; i++)
    {
      printf ("%02X ", record->oem_data[i]);
    }
  printf ("\n\n");
  
  return 0;
}

int 
sensors_display_very_verbose (sdr_record_t *sdr_record, sensor_reading_t *sensor_reading)
{
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return sensors_display_very_verbose_full_record (sdr_record->record_id, 
						       sdr_record->record_type, 
						       &(sdr_record->record.sdr_full_record), 
						       sensor_reading);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_very_verbose_compact_record (sdr_record->record_id, 
							  sdr_record->record_type, 
							  &(sdr_record->record.sdr_compact_record), 
							  sensor_reading);
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return sensors_display_very_verbose_event_only_record (sdr_record->record_id, 
							     sdr_record->record_type, 
							     &(sdr_record->record.sdr_event_only_record), 
							     sensor_reading);
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      return sensors_display_very_verbose_entity_asso_record (sdr_record->record_id, 
							      sdr_record->record_type, 
							      &(sdr_record->record.sdr_entity_association_record), 
							      sensor_reading);
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_gen_dev_locator_record (sdr_record->record_id, 
								  sdr_record->record_type, 
								  &(sdr_record->record.sdr_generic_device_locator_record), 
								  sensor_reading);
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_fru_dev_locator_record (sdr_record->record_id, 
								  sdr_record->record_type, 
								  &(sdr_record->record.sdr_logical_fru_device_locator_record), 
								  sensor_reading);
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_mgmt_cntrlr_dev_locator_record (sdr_record->record_id, 
									  sdr_record->record_type, 
									  &(sdr_record->record.sdr_management_controller_device_locator_record), 
									  sensor_reading);
    case IPMI_SDR_FORMAT_OEM_RECORD:
      return sensors_display_very_verbose_oem_record (sdr_record->record_id, 
						      sdr_record->record_type, 
						      &(sdr_record->record.sdr_oem_record), 
						      sensor_reading);
    }
  
  return (-1);
}
