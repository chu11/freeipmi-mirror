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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"

#include "ipmi-sensors.h"
#include "ipmi-sensors-display-common.h"

#include "pstdout.h"
#include "tool-sensor-common.h"

#define IPMI_SENSORS_OEM_DATA_LEN 1024

static int
_output_very_verbose_record_type_and_id (ipmi_sensors_state_data_t *state_data,
                                         uint8_t record_type,
                                         uint16_t record_id)
{
  assert(state_data);

  pstdout_printf (state_data->pstate, 
                  "Record ID: %d\n", 
                  record_id);
  pstdout_printf (state_data->pstate, 
                  "Record Type: %Xh\n", 
                  record_type);

  return 0;
}

static int
_output_very_verbose_header (ipmi_sensors_state_data_t *state_data,
                             uint8_t *sdr_record,
                             unsigned int sdr_record_len,
                             uint8_t record_type,
                             uint16_t record_id)
{
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  uint8_t sensor_owner_id_type;
  uint8_t sensor_owner_id;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_sensor_number (state_data->pstate,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_number) < 0)
    return -1;

  if (sdr_cache_get_sensor_type (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_type) < 0)
    return -1;

  if (sdr_cache_get_event_reading_type_code (state_data->pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    return -1;
  
  memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);

  if (sdr_cache_get_id_string (state_data->pstate,
                               sdr_record,
                               sdr_record_len,
                               id_string,
                               IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
    return -1;

  if (sdr_cache_get_sensor_owner_id (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &sensor_owner_id_type,
                                     &sensor_owner_id) < 0)
    return -1;

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;
  
  pstdout_printf (state_data->pstate, 
                  "Sensor ID String: %s\n", 
                  id_string);
  pstdout_printf (state_data->pstate, 
                  "Group Name: %s\n",
                  sensor_group (sensor_type));
  pstdout_printf (state_data->pstate, 
                  "Sensor Number: %d\n", 
                  sensor_number);
  pstdout_printf (state_data->pstate, 
                  "Event/Reading Type Code: %Xh\n", 
                  event_reading_type_code);
  pstdout_printf (state_data->pstate, 
                  "Slave Address/System Software ID Type: %d\n", 
                  sensor_owner_id_type);
  pstdout_printf (state_data->pstate, 
                  "Slave Address/System Software ID: %Xh\n", 
                  sensor_owner_id);

  return 0;
}

static int
_output_very_verbose_hysteresis (ipmi_sensors_state_data_t *state_data,
                                 uint8_t *sdr_record,
                                 unsigned int sdr_record_len,
                                 uint8_t record_type)
{
  double *positive_going_threshold_hysteresis_real = NULL;
  double *negative_going_threshold_hysteresis_real = NULL;
  uint8_t sensor_unit;
  int rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_sensor_unit (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &sensor_unit) < 0)
    goto cleanup;

  /* achu: Well, compact records don't have the values to compute a
   * hysteresis value.  Perhaps that's a typo in the spec?  We just
   * output the integer values?  That's as good as guess as any I
   * could make.
   */
  
  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      if (sdr_cache_get_hysteresis_real (state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         &positive_going_threshold_hysteresis_real,
                                         &negative_going_threshold_hysteresis_real) < 0)
        goto cleanup;
      
      if (negative_going_threshold_hysteresis_real)
        pstdout_printf (state_data->pstate, 
                        "Negative Hysteresis: %f %s\n", 
                        *negative_going_threshold_hysteresis_real,
                        ipmi_sensor_units[sensor_unit]);
      else
        pstdout_printf (state_data->pstate,
                        "Negative Hysteresis: %s\n", 
                        "NA");
      
      if (positive_going_threshold_hysteresis_real)
        pstdout_printf (state_data->pstate, 
                        "Positive Hysteresis: %f %s\n", 
                        *positive_going_threshold_hysteresis_real,
                        ipmi_sensor_units[sensor_unit]);
      else
        pstdout_printf (state_data->pstate,
                        "Positive Hysteresis: %s\n", 
                        "NA");
    }
  else
    {
      uint8_t positive_going_threshold_hysteresis_integer;
      uint8_t negative_going_threshold_hysteresis_integer;

      if (sdr_cache_get_hysteresis_integer (state_data->pstate,
                                            sdr_record,
                                            sdr_record_len,
                                            &positive_going_threshold_hysteresis_integer,
                                            &negative_going_threshold_hysteresis_integer) < 0)
        goto cleanup;
      
      pstdout_printf (state_data->pstate, 
                      "Negative Hysteresis: %d %s\n", 
                      negative_going_threshold_hysteresis_integer,
                      ipmi_sensor_units[sensor_unit]);
      
      pstdout_printf (state_data->pstate, 
                      "Positive Hysteresis: %d %s\n", 
                      positive_going_threshold_hysteresis_integer,
                      ipmi_sensor_units[sensor_unit]);
    }

  rv = 0;
 cleanup:
  if (positive_going_threshold_hysteresis_real)
    free(positive_going_threshold_hysteresis_real);
  if (negative_going_threshold_hysteresis_real)
    free(negative_going_threshold_hysteresis_real);
  return rv;
}

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
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header (state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   record_type,
                                   record_id) < 0)
    return -1;

  if (sdr_cache_get_sensor_decoding_data(state_data->pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         &r_exponent,
                                         &b_exponent,
                                         &m,
                                         &b,
                                         &linearization,
                                         &analog_data_format) < 0)
    return -1;
  
  pstdout_printf (state_data->pstate, 
                  "B: %d\n", 
                  b);
  pstdout_printf (state_data->pstate,
                  "M: %d\n", 
                  m);
  pstdout_printf (state_data->pstate, 
                  "R Exponent: %d\n", 
                  r_exponent);
  pstdout_printf (state_data->pstate, 
                  "B Exponent: %d\n", 
                  b_exponent);
  pstdout_printf (state_data->pstate, 
                  "Linearization: %d\n", 
                  linearization);
  pstdout_printf (state_data->pstate, 
                  "Analog Data Format: %d\n", 
                  analog_data_format);

  if (ipmi_sensors_output_verbose_thresholds (state_data,
                                              sdr_record,
                                              sdr_record_len) < 0)
    return -1;
 
  if (ipmi_sensors_output_verbose_sensor_reading_ranges (state_data,
                                                         sdr_record,
                                                         sdr_record_len) < 0)
    return -1;

  if (_output_very_verbose_hysteresis (state_data,
                                       sdr_record,
                                       sdr_record_len,
                                       record_type) < 0)
    return -1;

  if (ipmi_sensors_output_verbose_sensor_reading (state_data,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  reading) < 0)
    return -1;
  
  if (ipmi_sensors_output_verbose_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

static int 
sensors_display_very_verbose_compact_record (ipmi_sensors_state_data_t *state_data,
                                             uint8_t *sdr_record,
                                             unsigned int sdr_record_len,
                                             uint8_t record_type,
                                             uint16_t record_id,
                                             char **event_message_list,
                                             unsigned int event_message_list_len)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header (state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   record_type,
                                   record_id) < 0)
    return -1;

  if (_output_very_verbose_hysteresis (state_data,
                                       sdr_record,
                                       sdr_record_len,
                                       record_type) < 0)
    return -1;

  if (ipmi_sensors_output_verbose_event_message_list (state_data,
                                                      event_message_list,
                                                      event_message_list_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_event_only_record (ipmi_sensors_state_data_t *state_data,
                                                uint8_t *sdr_record,
                                                unsigned int sdr_record_len,
                                                uint8_t record_type,
                                                uint16_t record_id)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header (state_data,
                                   sdr_record,
                                   sdr_record_len,
                                   record_type,
                                   record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_entity_association_record (ipmi_sensors_state_data_t *state_data,
                                                        uint8_t *sdr_record,
                                                        unsigned int sdr_record_len,
                                                        uint8_t record_type,
                                                        uint16_t record_id)
{
  uint8_t container_entity_id;
  uint8_t container_entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_container_entity (state_data->pstate,
                                      sdr_record,
                                      sdr_record_len,
                                      &container_entity_id,
                                      &container_entity_instance) < 0)
    return -1;
  
  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Container Entity ID: %Xh\n", 
                  container_entity_id);
  pstdout_printf (state_data->pstate, 
                  "Container Entity Instance: %Xh\n", 
                  container_entity_instance);

  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

static int
_output_very_verbose_header2 (ipmi_sensors_state_data_t *state_data,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len,
                              uint8_t record_type,
                              uint16_t record_id)
{
  char device_id_string[IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING + 1];

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  memset(device_id_string, '\0', IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING + 1);

  if (sdr_cache_get_device_id_string (state_data->pstate,
                                      sdr_record,
                                      sdr_record_len,
                                      device_id_string,
                                      IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING) < 0)
    return -1;

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Device ID String: %s\n", 
                  device_id_string);
  
  return 0;
}

static int
_output_device_type_and_modifier (ipmi_sensors_state_data_t *state_data,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len)
{
  uint8_t device_type;
  uint8_t device_type_modifier;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_device_type (state_data->pstate,
                                 sdr_record,
                                 sdr_record_len,
                                 &device_type,
                                 &device_type_modifier) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Device Type: %Xh\n", 
                  device_type);

  pstdout_printf (state_data->pstate, 
                  "Device Type Modifier: %Xh\n", 
                  device_type_modifier);

  return 0;
}

static int
_output_entity_id_and_instance (ipmi_sensors_state_data_t *state_data,
                                uint8_t *sdr_record,
                                unsigned int sdr_record_len)
{
  uint8_t entity_id;
  uint8_t entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (sdr_cache_get_entity_id (state_data->pstate,
                               sdr_record,
                               sdr_record_len,
                               &entity_id,
                               &entity_instance) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Entity ID: %Xh\n", 
                  entity_id);
  
  pstdout_printf (state_data->pstate, 
                  "Entity Instance: %Xh\n", 
                  entity_instance);

  return 0;
}

static int 
sensors_display_very_verbose_general_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                            uint8_t *sdr_record,
                                                            unsigned int sdr_record_len,
                                                            uint8_t record_type,
                                                            uint16_t record_id)
{
  uint8_t direct_access_address;
  uint8_t channel_number;
  uint8_t device_slave_address;
  uint8_t lun_for_master_write_read_command;
  uint8_t address_span;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header2 (state_data,
                                    sdr_record,
                                    sdr_record_len,
                                    record_type,
                                    record_id) < 0)
    return -1;

  if (sdr_cache_get_general_device_parameters (state_data->pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               &direct_access_address,
                                               &channel_number,
                                               &device_slave_address,
                                               &lun_for_master_write_read_command,
                                               &address_span) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "Direct Access Address: %Xh\n", 
                  direct_access_address);
  pstdout_printf (state_data->pstate, 
                  "Channel Number: %Xh\n", 
                  channel_number);
  pstdout_printf (state_data->pstate, 
                  "Direct Slave Address: %Xh\n", 
                  device_slave_address);
  pstdout_printf (state_data->pstate, 
                  "Access LUN/Bus ID: %Xh\n", 
                  lun_for_master_write_read_command);
  pstdout_printf (state_data->pstate, 
                  "Address Span: %Xh\n", 
                  address_span);

  if (_output_device_type_and_modifier (state_data,
                                        sdr_record,
                                        sdr_record_len) < 0)
    return -1;

  if (_output_entity_id_and_instance (state_data,
                                      sdr_record,
                                      sdr_record_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_fru_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                        uint8_t *sdr_record,
                                                        unsigned int sdr_record_len,
                                                        uint8_t record_type,
                                                        uint16_t record_id)
{
  uint8_t entity_id;
  uint8_t entity_instance;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header2 (state_data,
                                    sdr_record,
                                    sdr_record_len,
                                    record_type,
                                    record_id) < 0)
    return -1;

  if (_output_device_type_and_modifier (state_data,
                                        sdr_record,
                                        sdr_record_len) < 0)
    return -1;

  if (sdr_cache_get_entity_id (state_data->pstate,
                               sdr_record,
                               sdr_record_len,
                               &entity_id,
                               &entity_instance) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "FRU Entity ID: %Xh\n", 
                  entity_id);
  
  pstdout_printf (state_data->pstate, 
                  "FRU Entity Instance: %Xh\n", 
                  entity_instance);

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_management_controller_device_locator_record (ipmi_sensors_state_data_t *state_data,
                                                                          uint8_t *sdr_record,
                                                                          unsigned int sdr_record_len,
                                                                          uint8_t record_type,
                                                                          uint16_t record_id)
{
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_header2 (state_data,
                                    sdr_record,
                                    sdr_record_len,
                                    record_type,
                                    record_id) < 0)
    return -1;

  if (_output_entity_id_and_instance (state_data,
                                      sdr_record,
                                      sdr_record_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, "\n");

  return 0;
}

static int 
sensors_display_very_verbose_oem_record (ipmi_sensors_state_data_t *state_data,
                                         uint8_t *sdr_record,
                                         unsigned int sdr_record_len,
                                         uint8_t record_type,
                                         uint16_t record_id)
{
  uint32_t manufacturer_id;
  uint8_t oem_data[IPMI_SENSORS_OEM_DATA_LEN];
  unsigned int oem_data_len = IPMI_SENSORS_OEM_DATA_LEN;
  int i;
  
  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (_output_very_verbose_record_type_and_id (state_data,
                                               record_type,
                                               record_id) < 0)
    return -1;

  if (sdr_cache_get_manufacturer_id (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &manufacturer_id) < 0)
    return -1;

  pstdout_printf (state_data->pstate,
                  "Manufacturer ID: %Xh\n", 
                  manufacturer_id);

  if (sdr_cache_get_oem_data (state_data->pstate,
                              sdr_record,
                              sdr_record_len,
                              oem_data,
                              &oem_data_len) < 0)
    return -1;

  pstdout_printf (state_data->pstate, 
                  "OEM Data: ");
  
  for (i = 0; i < oem_data_len; i++)
    pstdout_printf (state_data->pstate, 
                    "%02X ", 
                    oem_data[i]);
  pstdout_printf (state_data->pstate, "\n");

  pstdout_printf (state_data->pstate, "\n");
  
  return 0;
}

int 
ipmi_sensors_display_very_verbose (ipmi_sensors_state_data_t *state_data,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   double *reading,
                                   char **event_message_list,
                                   unsigned int event_message_list_len)
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
                                                          event_message_list,
                                                          event_message_list_len);
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return sensors_display_very_verbose_event_only_record (state_data,
                                                             sdr_record,
                                                             sdr_record_len,
                                                             record_type,
                                                             record_id);
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      return sensors_display_very_verbose_entity_association_record (state_data,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     record_type,
                                                                     record_id);

#if 0
      /* XXX support later */
    case IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD:
      return sensors_display_very_verbose_entity_association_record (state_data,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     record_type,
                                                                     record_id);
#endif
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_general_device_locator_record (state_data,
                                                                         sdr_record,
                                                                         sdr_record_len,
                                                                         record_type,
                                                                         record_id);
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_fru_device_locator_record (state_data,
                                                                     sdr_record,
                                                                     sdr_record_len,
                                                                     record_type,
                                                                     record_id);
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      return sensors_display_very_verbose_management_controller_device_locator_record (state_data,
                                                                                       sdr_record,
                                                                                       sdr_record_len,
                                                                                       record_type,
                                                                                       record_id);
#if 0
      /* XXX support later */
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD:
      break;
      /* XXX support later */
    case IPMI_SDR_FORMAT_BMC_MESAAGE_CHANNEL_INFO_RECORD:
      break;
#endif
    case IPMI_SDR_FORMAT_OEM_RECORD:
      return sensors_display_very_verbose_oem_record (state_data,
                                                      sdr_record,
                                                      sdr_record_len,
                                                      record_type,
                                                      record_id);
    default:
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "Unknown Record Type: %X\n",
                      record_type);
      break;
    }
  
  return (0);
}
