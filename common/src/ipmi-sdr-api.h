/*
ipmi-sdr-api.h: SDR cache creation and management apis.
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

#ifndef _IPMI_SDR_API_H
#define _IPMI_SDR_API_H

#include <stdio.h>
#include <stdint.h>

#define IPMI_SENSOR_NAME_MAX  17
#define IPMI_DEVICE_NAME_MAX  IPMI_SENSOR_NAME_MAX
#define IPMI_OEM_DATA_MAX     55

struct sdr_repository_info
{
  int sdr_version_major;
  int sdr_version_minor;
  int record_count;
  int free_space;
  int most_recent_addition_timestamp;
  int most_recent_erase_timestamp;
  int get_sdr_repository_allocation_info_command_supported;
  int reserve_sdr_repository_command_supported;
  int partial_add_sdr_command_supported;
  int delete_sdr_command_supported;
  int modal_non_modal_sdr_repository_update_operation_supported;
  int overflow_flag;
};
typedef struct sdr_repository_info sdr_repository_info_t;

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
  char sensor_name[IPMI_SENSOR_NAME_MAX];
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
  char sensor_name[IPMI_SENSOR_NAME_MAX];
};
typedef struct sdr_compact_record sdr_compact_record_t;

struct sdr_event_only_record
{
  uint8_t sensor_owner_id;
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  char sensor_name[IPMI_SENSOR_NAME_MAX];
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
  char device_name[IPMI_DEVICE_NAME_MAX];
};
typedef struct sdr_generic_device_locator_record sdr_generic_device_locator_record_t;

struct sdr_logical_fru_device_locator_record
{
  uint8_t device_type;
  uint8_t device_type_modifier;
  uint8_t fru_entity_id;
  uint8_t fru_entity_instance;
  char device_name[IPMI_DEVICE_NAME_MAX];
};
typedef struct sdr_logical_fru_device_locator_record sdr_logical_fru_device_locator_record_t;

struct sdr_management_controller_device_locator_record
{
  uint8_t entity_id;
  uint8_t entity_instance;
  char device_name[IPMI_DEVICE_NAME_MAX];
};
typedef struct sdr_management_controller_device_locator_record sdr_management_controller_device_locator_record_t;

struct sdr_oem_record
{
  uint32_t manufacturer_id;
  uint8_t oem_data_length;
  uint8_t oem_data[IPMI_OEM_DATA_MAX];
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

int get_sdr_repository_info (ipmi_device_t dev, sdr_repository_info_t *sdr_info);

char *get_sdr_cache_filename (char *host, char *user_cache_dir);

int setup_sdr_cache_directory ();

int flush_sdr_cache_file (char *host, char *user_cache_dir);

int create_sdr_cache (ipmi_device_t dev, FILE *fp, int verbose, int debug);

int load_sdr_cache (FILE *fp, sdr_repository_info_t *sdr_info, 
		    sdr_record_t **sdr_record_list, int *count);


#endif
