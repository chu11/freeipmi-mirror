/*
ipmi-sdr-cache.h: SDR cache creation and management apis.
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

#ifndef _IPMI_SDR_CACHE_H
#define _IPMI_SDR_CACHE_H

#include <stdio.h>
#include <stdint.h>

#define SDR_CACHE_CTX_ERR_SUCCESS                        0
#define SDR_CACHE_CTX_ERR_NULL                           1
#define SDR_CACHE_CTX_ERR_INVALID                        2
#define SDR_CACHE_CTX_ERR_PARAMETERS                     3
#define SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY            4
#define SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_PERMISSION 5
#define SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_CREATION   6
#define SDR_CACHE_CTX_ERR_HOSTNAME_INVALID               7
#define SDR_CACHE_CTX_ERR_CACHE_DOES_NOT_EXIST           8
#define SDR_CACHE_CTX_ERR_CACHE_EXISTS                   9
#define SDR_CACHE_CTX_ERR_CACHE_INVALID                 10
#define SDR_CACHE_CTX_ERR_CACHE_EMPTY                   11
#define SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE             12
#define SDR_CACHE_CTX_ERR_CACHE_PERMISSION              13
#define SDR_CACHE_CTX_ERR_OUTMEM                        14
#define SDR_CACHE_CTX_ERR_IPMI_COMMUNICATION            15
#define SDR_CACHE_CTX_ERR_INTERNAL                      16
#define SDR_CACHE_CTX_ERR_ERRNUMRANGE                   17

/* Max length 16 according to IPMI spec */
#define IPMI_SENSOR_NAME_MAX  16
#define IPMI_DEVICE_NAME_MAX  IPMI_SENSOR_NAME_MAX
/* IPMI spec indicates oem records are 64 bytes total including the
 * header.  The header is 8 bytes, thus the max oem data is 56
 * bytes 
 */
#define IPMI_OEM_DATA_MAX     56

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
  char sensor_name[IPMI_SENSOR_NAME_MAX+1];
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
  char sensor_name[IPMI_SENSOR_NAME_MAX+1];
};
typedef struct sdr_compact_record sdr_compact_record_t;

struct sdr_event_only_record
{
  uint8_t sensor_owner_id;
  uint8_t sensor_number;
  uint8_t sensor_type;
  uint8_t event_reading_type_code;
  char sensor_name[IPMI_SENSOR_NAME_MAX+1];
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
  char device_name[IPMI_DEVICE_NAME_MAX+1];
};
typedef struct sdr_generic_device_locator_record sdr_generic_device_locator_record_t;

struct sdr_fru_device_locator_record
{
  uint8_t logical_fru_device_device_slave_address;
  uint8_t logical_physical_fru_device;
  uint8_t device_type;
  uint8_t device_type_modifier;
  uint8_t fru_entity_id;
  uint8_t fru_entity_instance;
  char device_name[IPMI_DEVICE_NAME_MAX+1];
};
typedef struct sdr_fru_device_locator_record sdr_fru_device_locator_record_t;

struct sdr_management_controller_device_locator_record
{
  uint8_t entity_id;
  uint8_t entity_instance;
  char device_name[IPMI_DEVICE_NAME_MAX+1];
};
typedef struct sdr_management_controller_device_locator_record sdr_management_controller_device_locator_record_t;

struct sdr_oem_record
{
  uint32_t manufacturer_id;
  uint8_t oem_data_length;
  uint8_t oem_data[IPMI_OEM_DATA_MAX+1];
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
    sdr_fru_device_locator_record_t sdr_fru_device_locator_record;
    sdr_management_controller_device_locator_record_t sdr_management_controller_device_locator_record;
    sdr_oem_record_t sdr_oem_record;
  } record;
};
typedef struct sdr_record sdr_record_t;

typedef struct sdr_cache_ctx *sdr_cache_ctx_t;

#define IPMI_SDR_CACHE_ERRMSGLEN 1024

sdr_cache_ctx_t sdr_cache_ctx_create(void);

void sdr_cache_ctx_destroy(sdr_cache_ctx_t ctx);

char *sdr_cache_ctx_strerror(int32_t errnum);

int sdr_cache_ctx_errnum(sdr_cache_ctx_t ctx);

int sdr_cache_create (sdr_cache_ctx_t ctx,
                      ipmi_ctx_t ipmi_ctx, 
                      char *host,
                      char *user_cache_dir,
                      int verbose, 
                      int debug);

int sdr_cache_flush (sdr_cache_ctx_t ctx,
                     char *host, 
                     char *user_cache_dir);

int sdr_cache_load (sdr_cache_ctx_t ctx,
                    ipmi_ctx_t ipmi_ctx, 
                    char *host,
                    char *user_cache_dir,
		    sdr_record_t **sdr_record_list, 
                    unsigned int *sdr_record_count);

int sdr_cache_create_and_load (sdr_cache_ctx_t ctx,
                               ipmi_ctx_t ipmi_ctx,
                               char *host,
                               char *user_cache_dir,
                               int verbose,
                               int debug,
                               sdr_record_t **sdr_record_list,
                               unsigned int *sdr_record_count,
                               char *errmsg,
                               unsigned int errmsglen);

#endif
