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
#include "ipmi-sensors-display-common.h"

#include "pstdout.h"
#include "tool-sensor-common.h"

static int 
sensors_display_very_verbose_full_record (ipmi_sensors_state_data_t *state_data,
                                          uint8_t *sdr_record,
                                          unsigned int sdr_record_len,
                                          uint8_t record_type,
                                          uint16_t record_id,
                                          double *reading,
                                          char **event_message_list,
                                          unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n", 
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  record->sensor_name);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n", 
                  sensor_group (record->sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  record->sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  record->event_reading_type_code);
  pstdout_printf (state_data->pstate, 
                  "Slave Address/System Software ID: %Xh\n", 
                  record->sensor_owner_id);
  pstdout_printf (state_data->pstate, 
                  "B: %d\n", 
                  record->b);
  pstdout_printf (state_data->pstate, 
                  "M: %d\n", 
                  record->m);
  pstdout_printf (state_data->pstate, 
                  "R Exponent: %d\n", 
                  record->r_exponent);
  pstdout_printf (state_data->pstate, 
                  "B Exponent: %d\n", 
                  record->b_exponent);
  pstdout_printf (state_data->pstate, 
                  "Linear: %d\n", 
                  record->linear);
  pstdout_printf (state_data->pstate, 
                  "Analog Data Format: %d\n", 
                  record->analog_data_format);
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
  pstdout_printf (state_data->pstate, "Sensor Min. Reading: %f %s\n", 
                  record->sensor_minimum_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, "Sensor Max. Reading: %f %s\n", 
                  record->sensor_maximum_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, "Normal Min.: %f %s\n", 
                  record->normal_minimum, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, "Normal Max.: %f %s\n", 
                  record->normal_maximum, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, "Nominal reading: %f %s\n", 
                  record->nominal_reading, 
                  ipmi_sensor_units[record->sensor_unit]);
  pstdout_printf (state_data->pstate, 
                  "Negative Hysteresis: %d\n", 
                  record->negative_going_threshold_hysteresis);
  pstdout_printf (state_data->pstate, 
                  "Positive Hysteresis: %d\n", 
                  record->positive_going_threshold_hysteresis);

  if (reading)
    pstdout_printf (state_data->pstate, 
                    "Sensor Reading: %f %s\n", 
                    *reading, 
                    ipmi_sensor_units[sensor_unit]);
  else
    pstdout_printf (state_data->pstate,
                    "Sensor Reading: %s\n",
                    "NA");
  
  if (ipmi_sensors_verbose_output_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    goto cleanup;
  
  return 0;
}

static int 
sensors_display_very_verbose_compact_record (ipmi_sensors_state_data_t *state_data,
                                             uint8_t *sdr_record,
                                             unsigned int sdr_record_len,
                                             uint8_t record_type,
                                             uint16_t record_id,
                                             double *reading,
                                             char **event_message_list,
                                             unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n", 
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  record->sensor_name);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n", 
                  sensor_group (record->sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  record->sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  record->event_reading_type_code);
  pstdout_printf (state_data->pstate, 
                  "Slave Address/System Software ID: %Xh\n", 
                  record->sensor_owner_id);
  pstdout_printf (state_data->pstate, 
                  "Negative Hysteresis: %d\n", 
                  record->negative_going_threshold_hysteresis);
  pstdout_printf (state_data->pstate, 
                  "Positive Hysteresis: %d\n", 
                  record->positive_going_threshold_hysteresis);

  if (ipmi_sensors_verbose_output_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    goto cleanup;

  return 0;
}

static int 
sensors_display_very_verbose_event_only_record (ipmi_sensors_state_data_t *state_data,
                                                uint8_t *sdr_record,
                                                unsigned int sdr_record_len,
                                                uint8_t record_type,
                                                uint16_t record_id,
                                                double *reading,
                                                char **event_message_list,
                                                unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n", 
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  record->sensor_name);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n", 
                  sensor_group (record->sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  record->sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  record->event_reading_type_code);
  pstdout_printf (state_data->pstate, 
                  "Slave Address/System Software ID: %Xh\n", 
                  record->sensor_owner_id);

  if (ipmi_sensors_verbose_output_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    goto cleanup;
  
  return 0;
}

static int 
sensors_display_very_verbose_entity_association_record (ipmi_sensors_state_data_t *state_data,
                                                        uint8_t *sdr_record,
                                                        unsigned int sdr_record_len,
                                                        uint8_t record_type,
                                                        uint16_t record_id,
                                                        double *reading,
                                                        char **event_message_list,
                                                        unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n",
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n", 
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  "NONE");
  pstdout_printf (state_data->pstate, 
                  "Container Entity ID: %Xh\n", 
                  record->container_entity_id);
  pstdout_printf (state_data->pstate, 
                  "Container Entity Instance: %Xh\n\n", 
                  record->container_entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_general_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                            uint8_t *sdr_record,
                                                            unsigned int sdr_record_len,
                                                            uint8_t record_type,
                                                            uint16_t record_id,
                                                            double *reading,
                                                            char **event_message_list,
                                                            unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n",
                  record_id);
  pstdout_printf (state_data->pstate,
                  "Record Type: %Xh\n",
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Device Name: %s\n", 
                  record->device_name);
  pstdout_printf (state_data->pstate, 
                  "Direct Access Address: %Xh\n", 
                  record->direct_access_address);
  pstdout_printf (state_data->pstate, 
                  "Channel Number: %Xh\n", 
                  record->channel_number);
  pstdout_printf (state_data->pstate, 
                  "Direct Slave Address: %Xh\n", 
                  record->device_slave_address);
  pstdout_printf (state_data->pstate, 
                  "Access LUN/Bus ID: %Xh\n", 
                  record->lun_for_master_write_read_command);
  pstdout_printf (state_data->pstate, 
                  "Address Span: %Xh\n", 
                  record->address_span);
  pstdout_printf (state_data->pstate, 
                  "Device Type: %Xh\n", 
                  record->device_type);
  pstdout_printf (state_data->pstate, 
                  "Device Type Modifier: %Xh\n", 
                  record->device_type_modifier);
  pstdout_printf (state_data->pstate, 
                  "Entity ID: %Xh\n", 
                  record->entity_id);
  pstdout_printf (state_data->pstate, 
                  "Entity Instance: %Xh\n\n", 
                  record->entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_fru_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                        uint8_t *sdr_record,
                                                        unsigned int sdr_record_len,
                                                        uint8_t record_type,
                                                        uint16_t record_id,
                                                        double *reading,
                                                        char **event_message_list,
                                                        unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n",
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n",
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Device Name: %s\n",
                  record->device_name);
  pstdout_printf (state_data->pstate,
                  "Device Type: %Xh\n", 
                  record->device_type);
  pstdout_printf (state_data->pstate, 
                  "Device Type Modifier: %Xh\n", 
                  record->device_type_modifier);
  pstdout_printf (state_data->pstate, 
                  "FRU Entity ID: %Xh\n", 
                  record->fru_entity_id);
  pstdout_printf (state_data->pstate, 
                  "FRU Entity Instance: %Xh\n\n", 
                  record->fru_entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_management_controller_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                                          uint8_t *sdr_record,
                                                                          unsigned int sdr_record_len,
                                                                          uint8_t record_type,
                                                                          uint16_t record_id,
                                                                          double *reading,
                                                                          char **event_message_list,
                                                                          unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n",
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Device Name: %s\n", 
                  record->device_name);
  pstdout_printf (state_data->pstate, 
                  "Entity ID: %Xh\n", 
                  record->entity_id);
  pstdout_printf (state_data->pstate, 
                  "Entity Instance: %Xh\n\n", 
                  record->entity_instance);
  
  return 0;
}

static int 
sensors_display_very_verbose_oem_record (ipmi_sensors_state_data_t *state_data,
                                         uint8_t *sdr_record,
                                         unsigned int sdr_record_len,
                                         uint8_t record_type,
                                         uint16_t record_id,
                                         double *reading,
                                         char **event_message_list,
                                         unsigned int event_message_list_len)
{
  int i;
  
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n", 
                  record_type);
  pstdout_printf (state_data->pstate, 
                  "Sensor Name: %s\n", 
                  "NONE");
  pstdout_printf (state_data->pstate,
                  "Manufacturer ID: %Xh\n", 
                  record->manufacturer_id);
  pstdout_printf (state_data->pstate, 
                  "OEM Data: ");
  for (i = 0; i < record->oem_data_length; i++)
    {
      pstdout_printf (state_data->pstate, 
                      "%02X ", 
                      record->oem_data[i]);
    }
  pstdout_printf (state_data->pstate, 
                  "\n\n");
  
  return 0;
}

int 
ipmi_sensors_display_very_verbose (ipmi_sensors_state_data_t *state_data,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   double *reading,
                                   char **event_message_list,
                                   unsigned int event_message_list_len);

{
  uint16_t record_id;
  uint8_t record_type;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_record_id_and_type(state_data->pstate,
                                       sdr_record,
                                       sdr_record_len,
                                       &record_id,
                                       &record_type) < 0)
    return -1;

  switch (record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return sensors_display_very_verbose_full_record (state_data,
                                                       sdr_record,
                                                       sdr_record_len,
                                                       record_type,
                                                       record_id,
                                                       reading,
                                                       event_message_list,
                                                       event_message_list_len);
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return sensors_display_very_verbose_compact_record (state_data,
                                                          sdr_record,
                                                          sdr_record_len,
                                                          record_type,
                                                          record_id,
                                                          reading,
                                                          event_message_list,
                                                          event_message_list_len);
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return sensors_display_very_verbose_event_only_record (state_data,
                                                             sdr_record,
                                                             sdr_record_len,
                                                             record_type,
                                                             record_id,
                                                             reading,
                                                             event_message_list,
                                                             event_message_list_len);
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      return sensors_display_very_verbose_entity_association_record (state_data,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     record_type,
                                                                     record_id,
                                                                     reading,
                                                                     event_message_list,
                                                                     event_message_list_len);
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_general_device_locator_record (state_data,
                                                                         sdr_record,
                                                                         sdr_record_len,
                                                                         record_type,
                                                                         record_id,
                                                                         reading,
                                                                         event_message_list,
                                                                         event_message_list_len);
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_fru_device_locator_record (state_data,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     record_type,
                                                                     record_id,
                                                                     reading,
                                                                     event_message_list,
                                                                     event_message_list_len);
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_management_controller_device_locator_record (state_data,
                                                                                       sdr_record,
                                                                                       sdr_record_len,
                                                                                       record_type,
                                                                                       record_id,
                                                                                       reading,
                                                                                       event_message_list,
                                                                                       event_message_list_len);
    case IPMI_SDR_FORMAT_OEM_RECORD:
      return sensors_display_very_verbose_oem_record (state_data,
                                                      sdr_record,
                                                      sdr_record_len,
                                                      record_type,
                                                      record_id,
                                                      reading,
                                                      event_message_list,
                                                      event_message_list_len);
    default:
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "Unknown Record Type: %X\n",
                      sdr_record->record_type);
      break;
    }
  
  return (0);
}
