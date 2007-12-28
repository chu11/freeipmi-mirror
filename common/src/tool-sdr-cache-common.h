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

#ifndef _TOOL_SDR_CACHE_COMMON_H
#define _TOOL_SDR_CACHE_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"

#include "pstdout.h"

#define IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH 1024
#define IPMI_SDR_CACHE_MAX_ID_STRING         16

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

int sdr_cache_create (ipmi_sdr_cache_ctx_t ctx,
                      pstdout_state_t pstate,
                      ipmi_ctx_t ipmi_ctx,
                      int quiet_cache,
                      const char *hostname,
                      const char *cache_dir);

int sdr_cache_create_and_load (ipmi_sdr_cache_ctx_t ctx,
                               pstdout_state_t pstate,
                               ipmi_ctx_t ipmi_ctx,
                               int quiet_cache,
                               const char *hostname,
                               const char *cache_dir);

int sdr_cache_flush_cache (ipmi_sdr_cache_ctx_t ctx,
                           pstdout_state_t pstate,
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
                              double **positive_going_threshold_hysteresis,
                              double **negative_going_threshold_hysteresis);

#endif
