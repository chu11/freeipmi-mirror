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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

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
  u_int8_t linear;
  u_int8_t analog_data_format;
  /* ************************* */
  u_int8_t slave_system_software_id;
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type;
  u_int8_t sensor_unit;
  double nominal_reading;
  double normal_min;
  double normal_max;
  double sensor_min_reading;
  double sensor_max_reading;
  u_int8_t negative_hysteresis;
  u_int8_t positive_hysteresis;
  double lower_non_recoverable_threshold;
  double upper_non_recoverable_threshold;
  double lower_critical_threshold;
  double upper_critical_threshold;
  double lower_non_critical_threshold;
  double upper_non_critical_threshold;
  char sensor_name[17];
  /*      threshold masks      */
  u_int8_t readable_lower_critical_threshold;
  u_int8_t readable_upper_critical_threshold;
  u_int8_t readable_lower_non_critical_threshold;
  u_int8_t readable_upper_non_critical_threshold;
  u_int8_t readable_lower_non_recoverable_threshold;
  u_int8_t readable_upper_non_recoverable_threshold;
  /* ************************* */
};

struct sdr_compact_record
{
  u_int8_t slave_system_software_id;
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type;
  u_int8_t sensor_unit;
  u_int8_t negative_hysteresis;
  u_int8_t positive_hysteresis;
  char sensor_name[17];
};

struct sdr_event_only_record
{
  u_int8_t slave_system_software_id;
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  u_int8_t event_reading_type;
  char sensor_name[17];
};

struct sdr_entity_association_record
{
  u_int8_t container_entity_id;
  u_int8_t container_entity_instance;
};

struct sdr_generic_device_locator_record
{
  u_int8_t direct_access_address;
  u_int8_t channel_number;
  u_int8_t device_slave_address;
  u_int8_t private_bus_id;
  u_int8_t lun_master_write_read_command;
  u_int8_t address_span;
  u_int8_t device_type;
  u_int8_t device_type_modifier;
  u_int8_t entity_id;
  u_int8_t entity_instance;
  char device_name[17];
};

struct sdr_logical_fru_device_locator_record
{
  u_int8_t device_type;
  u_int8_t device_type_modifier;
  u_int8_t fru_entity_id;
  u_int8_t fru_entity_instance;
  char device_name[17];
};

struct sdr_management_controller_device_locator_record
{
  u_int8_t entity_id;
  u_int8_t entity_instance;
  char device_name[17];
};

struct sdr_oem_record
{
  u_int32_t manufacturer_id;
  u_int8_t oem_data_length;
  u_int8_t oem_data[55];
};

struct sdr_record 
{
  u_int16_t record_id;
  u_int8_t record_type;
  union 
  {
    struct sdr_full_record sdr_full_record;
    struct sdr_compact_record sdr_compact_record;
    struct sdr_event_only_record sdr_event_only_record;
    struct sdr_entity_association_record sdr_entity_association_record;
    struct sdr_generic_device_locator_record sdr_generic_device_locator_record;
    struct sdr_logical_fru_device_locator_record sdr_logical_fru_device_locator_record;
    struct sdr_management_controller_device_locator_record sdr_management_controller_device_locator_record;
    struct sdr_oem_record sdr_oem_record;
  } record;
};

struct sensor_reading
{
  double current_reading;
  u_int8_t reading_availability_flag;
  u_int8_t sensor_scanning_flag;
  u_int8_t event_messages_flag;
  char **event_message_list;
};

void get_sdr_full_record (u_int8_t *sdr_record_data, 
			  struct sdr_full_record *sdr_full_record);
void get_sdr_compact_record (u_int8_t *sdr_record_data, 
			     struct sdr_compact_record *sdr_compact_record);
void get_sdr_event_only_record (u_int8_t *sdr_record_data, 
				struct sdr_event_only_record *sdr_event_only_record);
void get_sdr_entity_association_record (u_int8_t *sdr_record_data, 
					struct sdr_entity_association_record *sdr_entity_association_record);
void get_sdr_generic_device_locator_record (u_int8_t *sdr_record_data, 
					    struct sdr_generic_device_locator_record *sdr_generic_device_locator_record);
void get_sdr_logical_fru_device_locator_record (u_int8_t *sdr_record_data, 
						struct sdr_logical_fru_device_locator_record *sdr_logical_fru_device_locator_record);
void get_sdr_management_controller_device_locator_record (u_int8_t *sdr_record_data, 
							  struct sdr_management_controller_device_locator_record *sdr_management_controller_device_locator_record);
void get_sdr_oem_record (u_int8_t *sdr_record_data, 
			 struct sdr_oem_record *sdr_oem_record);

u_int8_t get_sdr_record (u_int16_t record_id, u_int16_t *next_record_id, 
			 struct sdr_record *sdr_record);
u_int8_t get_sensor_reading (struct sdr_record *sdr_record, 
			     struct sensor_reading *sensor_reading);
#endif
