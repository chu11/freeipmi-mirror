/* 
   ipmi-sensor-api.h - IPMI sensor commands API

   Copyright (C) 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifndef _IPMI_SENSOR_API_H
#define _IPMI_SENSOR_API_H

struct sdr_full_record
{
  /* requires to decode values */
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  uint8_t linear;
  uint8_t analog_data_format;
  /* ************************* */
  uint8_t sensor_owner_id;
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  uint8_t sensor_unit;
  double nominal_reading;
  double normal_minimum;
  double normal_maximum;
  double sensor_minimum_reading;
  double sensor_maximum_reading;
  uint8_t negative_going_threshold_hysteresis;
  uint8_t positive_going_threshold_hysteresis;
  double lower_non_recoverable_threshold;
  double upper_non_recoverable_threshold;
  double lower_critical_threshold;
  double upper_critical_threshold;
  double lower_non_critical_threshold;
  double upper_non_critical_threshold;
  char sensor_name[17];
  /*      threshold masks      */
  uint8_t readable_threshold_lower_critical_threshold;
  uint8_t readable_threshold_upper_critical_threshold;
  uint8_t readable_threshold_lower_non_critical_threshold;
  uint8_t readable_threshold_upper_non_critical_threshold;
  uint8_t readable_threshold_lower_non_recoverable_threshold;
  uint8_t readable_threshold_upper_non_recoverable_threshold;
  /* ************************* */
};
typedef struct sdr_full_record sdr_full_record_t;

struct sdr_compact_record
{
  uint8_t sensor_owner_id;
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  uint8_t sensor_unit;
  uint8_t negative_going_threshold_hysteresis;
  uint8_t positive_going_threshold_hysteresis;
  char sensor_name[17];
};
typedef struct sdr_compact_record sdr_compact_record_t;

struct sdr_event_only_record
{
  uint8_t sensor_owner_id;
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  char sensor_name[17];
};
typedef struct sdr_event_only_record sdr_event_only_record_t;

struct sdr_entity_association_record
{
  uint8_t container_entity_id;
  uint8_t container_entity_instance;
};
typedef struct sdr_entity_association_record sdr_entity_association_record_t;

struct sdr_generic_device_locator_record
{
  uint8_t direct_access_address;
  uint8_t channel_number;
  uint8_t device_slave_address;
  uint8_t private_bus_id;
  uint8_t lun_for_master_write_read_command;
  uint8_t address_span;
  uint8_t device_type;
  uint8_t device_type_modifier;
  uint8_t entity_id;
  uint8_t entity_instance;
  char device_name[17];
};
typedef struct sdr_generic_device_locator_record sdr_generic_device_locator_record_t;

struct sdr_logical_fru_device_locator_record
{
  uint8_t device_type;
  uint8_t device_type_modifier;
  uint8_t fru_entity_id;
  uint8_t fru_entity_instance;
  char device_name[17];
};
typedef struct sdr_logical_fru_device_locator_record sdr_logical_fru_device_locator_record_t;

struct sdr_management_controller_device_locator_record
{
  uint8_t entity_id;
  uint8_t entity_instance;
  char device_name[17];
};
typedef struct sdr_management_controller_device_locator_record sdr_management_controller_device_locator_record_t;

struct sdr_oem_record
{
  uint32_t manufacturer_id;
  uint8_t oem_data_length;
  uint8_t oem_data[55];
};
typedef struct sdr_oem_record sdr_oem_record_t;

struct sdr_record 
{
  uint16_t record_id;
  uint8_t record_type;
  union 
  {
    sdr_full_record_t sdr_full_record;
    sdr_compact_record_t sdr_compact_record;
    sdr_event_only_record_t sdr_event_only_record;
    sdr_entity_association_record_t sdr_entity_association_record;
    sdr_generic_device_locator_record_t sdr_generic_device_locator_record;
    sdr_logical_fru_device_locator_record_t sdr_logical_fru_device_locator_record;
    sdr_management_controller_device_locator_record_t sdr_management_controller_device_locator_record;
    sdr_oem_record_t sdr_oem_record;
  } record;
};
typedef struct sdr_record sdr_record_t;

struct sensor_reading
{
  double current_reading;
  uint8_t reading_availability_flag;
  uint8_t sensor_scanning;
  uint8_t event_messages_flag;
  char **event_message_list;
};
typedef struct sensor_reading sensor_reading_t;

void get_sdr_full_record (uint8_t *sdr_record_data, 
			  uint32_t sdr_record_data_len,
			  sdr_full_record_t *sdr_full_record);
void get_sdr_compact_record (uint8_t *sdr_record_data, 
			     uint32_t sdr_record_data_len,
			     sdr_compact_record_t *sdr_compact_record);
void get_sdr_event_only_record (uint8_t *sdr_record_data, 
				uint32_t sdr_record_data_len,
				sdr_event_only_record_t *sdr_event_only_record);
void get_sdr_entity_association_record (uint8_t *sdr_record_data, 
					uint32_t sdr_record_data_len,
					sdr_entity_association_record_t *sdr_entity_association_record);
void get_sdr_generic_device_locator_record (uint8_t *sdr_record_data, 
					    uint32_t sdr_record_data_len,
					    sdr_generic_device_locator_record_t *sdr_generic_device_locator_record);
void get_sdr_logical_fru_device_locator_record (uint8_t *sdr_record_data, 
						uint32_t sdr_record_data_len,
						sdr_logical_fru_device_locator_record_t *sdr_logical_fru_device_locator_record);
void get_sdr_management_controller_device_locator_record (uint8_t *sdr_record_data, 
							  uint32_t sdr_record_data_len,
							  sdr_management_controller_device_locator_record_t *sdr_management_controller_device_locator_record);
void get_sdr_oem_record (uint8_t *sdr_record_data, 
			 uint32_t sdr_record_data_len,
			 sdr_oem_record_t *sdr_oem_record);

int8_t get_sdr_record (ipmi_device_t *dev, 
			 uint16_t record_id, 
			 uint16_t *next_record_id, 
			 sdr_record_t *sdr_record);
int8_t get_sensor_reading (ipmi_device_t *dev, 
			     sdr_record_t *sdr_record, 
			     sensor_reading_t *sensor_reading);
#endif
