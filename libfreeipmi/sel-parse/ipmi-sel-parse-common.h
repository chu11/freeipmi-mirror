/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef IPMI_SEL_PARSE_COMMON_H
#define IPMI_SEL_PARSE_COMMON_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "ipmi-sel-parse-defs.h"

/* convenience struct */
struct ipmi_sel_system_event_record_data
{
  uint32_t timestamp;
  uint8_t generator_id;
  uint8_t ipmb_device_lun;
  uint8_t channel_number;
  uint8_t event_message_format_version;
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_type_code;
  uint8_t event_direction;
  uint8_t offset_from_event_reading_type_code;
  uint8_t event_data2_flag;
  uint8_t event_data3_flag;
  uint8_t event_data2;
  uint8_t event_data3;
};

int sel_parse_get_reservation_id (ipmi_sel_parse_ctx_t ctx,
                                  uint16_t *reservation_id,
                                  unsigned int *is_insufficient_privilege_level);

int sel_parse_get_record_header_info (ipmi_sel_parse_ctx_t ctx,
                                      struct ipmi_sel_parse_entry *sel_parse_entry,
                                      uint16_t *record_id,
                                      uint8_t *record_type);

int sel_parse_get_timestamp (ipmi_sel_parse_ctx_t ctx,
                             struct ipmi_sel_parse_entry *sel_parse_entry,
                             uint32_t *timestamp);

int sel_parse_get_manufacturer_id (ipmi_sel_parse_ctx_t ctx,
                                   struct ipmi_sel_parse_entry *sel_parse_entry,
                                   uint32_t *manufacturer_id);

int sel_parse_get_oem (ipmi_sel_parse_ctx_t ctx,
                       struct ipmi_sel_parse_entry *sel_parse_entry,
                       uint8_t *buf,
                       unsigned int buflen);

int sel_parse_get_system_event_record (ipmi_sel_parse_ctx_t ctx,
                                       struct ipmi_sel_parse_entry *sel_parse_entry,
                                       struct ipmi_sel_system_event_record_data *system_event_record_data);

int sel_parse_get_previous_state_or_severity (ipmi_sel_parse_ctx_t ctx,
                                              struct ipmi_sel_parse_entry *sel_parse_entry,
                                              uint8_t *previous_offset_from_event_reading_type_code,
                                              uint8_t *offset_from_severity_event_reading_type_code);

#endif /* IPMI_SEL_PARSE_COMMON_H */
