/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifndef _TOOL_SDR_CACHE_COMMON_H
#define _TOOL_SDR_CACHE_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"

#include "pstdout.h"

#define IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH 1024
#define IPMI_SDR_CACHE_MAX_ID_STRING         16
#define IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING  16

/* For sdr_cache_get_cache_directory: pstate can be NULL if we aren't
 * yet threaded 
 */
int sdr_cache_get_cache_directory(pstdout_state_t pstate,
                                  const char *cache_dir,
                                  char *buf,
                                  unsigned int buflen);

int sdr_cache_get_cache_filename (pstdout_state_t pstate,
                                  const char *hostname,
                                  const char *cache_dir,
                                  char *buf,
                                  unsigned int buflen);

int sdr_cache_create_directory (pstdout_state_t pstate,
                                const char *cache_dir);

int sdr_cache_create (ipmi_sdr_cache_ctx_t ctx,
                      pstdout_state_t pstate,
                      ipmi_ctx_t ipmi_ctx,
                      int quiet_cache,
                      int sdr_cache_recreate,
                      const char *hostname,
                      const char *cache_dir);

int sdr_cache_create_and_load (ipmi_sdr_cache_ctx_t ctx,
                               pstdout_state_t pstate,
                               ipmi_ctx_t ipmi_ctx,
                               int quiet_cache,
                               int sdr_cache_recreate,
                               const char *hostname,
                               const char *cache_dir);

int sdr_cache_flush_cache (ipmi_sdr_cache_ctx_t ctx,
                           pstdout_state_t pstate,
                           int quiet_cache,
                           const char *hostname,
                           const char *cache_dir);

int sdr_cache_get_record_id_and_type (pstdout_state_t pstate,
                                      uint8_t *sdr_record,
                                      unsigned int sdr_record_len,
                                      uint16_t *record_id,
                                      uint8_t *record_type);

int sdr_cache_get_sensor_owner_id (pstdout_state_t pstate,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint8_t *sensor_owner_id_type,
                                   uint8_t *sensor_owner_id);

int sdr_cache_get_sensor_owner_lun (pstdout_state_t pstate,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint8_t *sensor_owner_lun,
                                    uint8_t *channel_number);

int sdr_cache_get_entity_id_instance_type (pstdout_state_t pstate,
                                           uint8_t *sdr_record,
                                           unsigned int sdr_record_len,
                                           uint8_t *entity_id,
                                           uint8_t *entity_instance,
                                           uint8_t *entity_instance_type);

int sdr_cache_get_sensor_number (pstdout_state_t pstate,
                                 uint8_t *sdr_record,
                                 unsigned int sdr_record_len,
                                 uint8_t *sensor_number);

int sdr_cache_get_sensor_type (pstdout_state_t pstate,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               uint8_t *sensor_type);

int sdr_cache_get_event_reading_type_code (pstdout_state_t pstate,
                                           uint8_t *sdr_record,
                                           unsigned int sdr_record_len,
                                           uint8_t *event_reading_type_code);

int sdr_cache_get_sensor_unit (pstdout_state_t pstate,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               uint8_t *sensor_unit);

int sdr_cache_get_id_string (pstdout_state_t pstate,
                             uint8_t *sdr_record,
                             unsigned int sdr_record_len,
                             char *id_string,
                             unsigned int id_string_len);

int sdr_cache_get_device_id_string (pstdout_state_t pstate,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    char *device_id_string,
                                    unsigned int device_id_string_len);

int sdr_cache_get_sensor_capabilities (pstdout_state_t pstate,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint8_t *event_message_control_support,
                                       uint8_t *threshold_access_support,
                                       uint8_t *hysteresis_support,
                                       uint8_t *auto_re_arm_support,
                                       uint8_t *entity_ignore_support);
                                       
int sdr_cache_get_sensor_decoding_data (pstdout_state_t pstate,
                                        uint8_t *sdr_record,
                                        unsigned int sdr_record_len,
                                        int8_t *r_exponent,
                                        int8_t *b_exponent,
                                        int16_t *m,
                                        int16_t *b,
                                        uint8_t *linearization,
                                        uint8_t *analog_data_format);

int sdr_cache_get_sensor_reading_ranges (pstdout_state_t pstate,
                                         uint8_t *sdr_record,
                                         unsigned int sdr_record_len,
                                         double **nominal_reading,
                                         double **normal_maximum,
                                         double **normal_minimum,
                                         double **sensor_maximum_reading,
                                         double **sensor_minimum_reading);

int sdr_cache_get_hysteresis (pstdout_state_t pstate,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len,
                              uint8_t *positive_going_threshold_hysteresis,
                              uint8_t *negative_going_threshold_hysteresis);

int sdr_cache_get_container_entity (pstdout_state_t pstate,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint8_t *container_entity_id,
                                    uint8_t *container_entity_instance);

int sdr_cache_get_general_device_locator_parameters (pstdout_state_t pstate,
                                                     uint8_t *sdr_record,
                                                     unsigned int sdr_record_len,
                                                     uint8_t *direct_access_address,
                                                     uint8_t *channel_number,
                                                     uint8_t *device_slave_address,
                                                     uint8_t *private_bus_id,
                                                     uint8_t *lun_for_master_write_read_command,
                                                     uint8_t *address_span);

int sdr_cache_get_fru_device_locator_parameters (pstdout_state_t pstate,
                                                 uint8_t *sdr_record,
                                                 unsigned int sdr_record_len,
                                                 uint8_t *direct_access_address,
                                                 uint8_t *logical_fru_device_device_slave_address,
                                                 uint8_t *private_bus_id,
                                                 uint8_t *lun_for_master_write_read_fru_command,
                                                 uint8_t *logical_physical_fru_device,
                                                 uint8_t *channel_number);

int sdr_cache_get_management_controller_device_locator_parameters (pstdout_state_t pstate,
                                                                   uint8_t *sdr_record,
                                                                   unsigned int sdr_record_len,
                                                                   uint8_t *device_slave_address,
                                                                   uint8_t *channel_number);

int sdr_cache_get_device_type (pstdout_state_t pstate,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               uint8_t *device_type,
                               uint8_t *device_type_modifier);

int sdr_cache_get_entity_id_and_instance (pstdout_state_t pstate,
                                          uint8_t *sdr_record,
                                          unsigned int sdr_record_len,
                                          uint8_t *entity_id,
                                          uint8_t *entity_instance);

int sdr_cache_get_fru_entity_id_and_instance (pstdout_state_t pstate,
                                              uint8_t *sdr_record,
                                              unsigned int sdr_record_len,
                                              uint8_t *fru_entity_id,
                                              uint8_t *fru_entity_instance);

int sdr_cache_get_manufacturer_id (pstdout_state_t pstate,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint32_t *manufacturer_id);

int sdr_cache_get_product_id (pstdout_state_t pstate,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len,
                              uint16_t *product_id);

int sdr_cache_get_oem_data (pstdout_state_t pstate,
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            uint8_t *oem_data,
                            unsigned int *oem_data_len);

int sdr_cache_get_assertion_supported (pstdout_state_t pstate,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint8_t *event_state_0,
                                       uint8_t *event_state_1,
                                       uint8_t *event_state_2,
                                       uint8_t *event_state_3,
                                       uint8_t *event_state_4,
                                       uint8_t *event_state_5,
                                       uint8_t *event_state_6,
                                       uint8_t *event_state_7,
                                       uint8_t *event_state_8,
                                       uint8_t *event_state_9,
                                       uint8_t *event_state_10,
                                       uint8_t *event_state_11,
                                       uint8_t *event_state_12,
                                       uint8_t *event_state_13,
                                       uint8_t *event_state_14);

int sdr_cache_get_deassertion_supported (pstdout_state_t pstate,
                                         uint8_t *sdr_record,
                                         unsigned int sdr_record_len,
                                         uint8_t *event_state_0,
                                         uint8_t *event_state_1,
                                         uint8_t *event_state_2,
                                         uint8_t *event_state_3,
                                         uint8_t *event_state_4,
                                         uint8_t *event_state_5,
                                         uint8_t *event_state_6,
                                         uint8_t *event_state_7,
                                         uint8_t *event_state_8,
                                         uint8_t *event_state_9,
                                         uint8_t *event_state_10,
                                         uint8_t *event_state_11,
                                         uint8_t *event_state_12,
                                         uint8_t *event_state_13,
                                         uint8_t *event_state_14);

int sdr_cache_get_threshold_assertion_supported (pstdout_state_t pstate,
                                                 uint8_t *sdr_record,
                                                 unsigned int sdr_record_len,
                                                 uint8_t *lower_non_critical_going_low,
                                                 uint8_t *lower_non_critical_going_high,
                                                 uint8_t *lower_critical_going_low,
                                                 uint8_t *lower_critical_going_high,
                                                 uint8_t *lower_non_recoverable_going_low,
                                                 uint8_t *lower_non_recoverable_going_high,
                                                 uint8_t *upper_non_critical_going_low,
                                                 uint8_t *upper_non_critical_going_high,
                                                 uint8_t *upper_critical_going_low,
                                                 uint8_t *upper_critical_going_high,
                                                 uint8_t *upper_non_recoverable_going_low,
                                                 uint8_t *upper_non_recoverable_going_high);

int sdr_cache_get_threshold_deassertion_supported (pstdout_state_t pstate,
                                                   uint8_t *sdr_record,
                                                   unsigned int sdr_record_len,
                                                   uint8_t *lower_non_critical_going_low,
                                                   uint8_t *lower_non_critical_going_high,
                                                   uint8_t *lower_critical_going_low,
                                                   uint8_t *lower_critical_going_high,
                                                   uint8_t *lower_non_recoverable_going_low,
                                                   uint8_t *lower_non_recoverable_going_high,
                                                   uint8_t *upper_non_critical_going_low,
                                                   uint8_t *upper_non_critical_going_high,
                                                   uint8_t *upper_critical_going_low,
                                                   uint8_t *upper_critical_going_high,
                                                   uint8_t *upper_non_recoverable_going_low,
                                                   uint8_t *upper_non_recoverable_going_high);

int sdr_cache_get_threshold_readable (pstdout_state_t pstate,
                                      uint8_t *sdr_record,
                                      unsigned int sdr_record_len,
                                      uint8_t *lower_non_critical_threshold,
                                      uint8_t *lower_critical_threshold,
                                      uint8_t *lower_non_recoverable_threshold,
                                      uint8_t *upper_non_critical_threshold,
                                      uint8_t *upper_critical_threshold,
                                      uint8_t *upper_non_recoverable_threshold);

int sdr_cache_get_threshold_settable (pstdout_state_t pstate,
                                      uint8_t *sdr_record,
                                      unsigned int sdr_record_len,
                                      uint8_t *lower_non_critical_threshold,
                                      uint8_t *lower_critical_threshold,
                                      uint8_t *lower_non_recoverable_threshold,
                                      uint8_t *upper_non_critical_threshold,
                                      uint8_t *upper_critical_threshold,
                                      uint8_t *upper_non_recoverable_threshold);

int sdr_cache_get_thresholds_raw (pstdout_state_t pstate,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len,
                                  uint8_t *lower_non_critical_threshold,
                                  uint8_t *lower_critical_threshold,
                                  uint8_t *lower_non_recoverable_threshold,
                                  uint8_t *upper_non_critical_threshold,
                                  uint8_t *upper_critical_threshold,
                                  uint8_t *upper_non_recoverable_threshold);

#endif
