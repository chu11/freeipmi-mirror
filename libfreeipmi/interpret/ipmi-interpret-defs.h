/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifndef IPMI_INTERPRET_DEFS_H
#define IPMI_INTERPRET_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>

#include "freeipmi/interpret/ipmi-interpret.h"
#include "freeipmi/sel/ipmi-sel.h"

#include "hash.h"

#define IPMI_INTERPRET_CTX_MAGIC 0xACFF3289

#define IPMI_INTERPRET_FLAGS_MASK \
  (IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA \
   | IPMI_INTERPRET_FLAGS_SEL_ASSUME_SYSTEM_EVENT_RECORDS \
   | IPMI_INTERPRET_FLAGS_IGNORE_UNRECOGNIZED_EVENTS)

#define IPMI_INTERPRET_MAX_BITMASKS 16

#define IPMI_INTERPRET_SEL_HASH_SIZE 32

#define IPMI_INTERPRET_SENSOR_HASH_SIZE 32

#define IPMI_OEM_STATE_TYPE_BITMASK 0
#define IPMI_OEM_STATE_TYPE_VALUE   1

/* manufacturing_id:product_id:event_type_code:sensor_type
 *
 * Based on value limits, can't be more than 22 bytes
 */
#define IPMI_OEM_HASH_KEY_BUFLEN                32

#define IPMI_SEL_OEM_DATA_MAX                   13

#define IPMI_SEL_OEM_DATA_TIMESTAMPED_BYTES     6

#define IPMI_SEL_OEM_DATA_NON_TIMESTAMPED_BYTES 13

#define IPMI_SEL_OEM_SENSOR_MAX                 16

#define IPMI_SEL_OEM_RECORD_MAX                 16

#define IPMI_SEL_OEM_DATA_HEX_BYTE_ANY          "ANY"

/* achu:
 *
 * Storing each interpretation rule for every
 * manufacturer_id:product_id:event_reading_type_code:sensor_type
 * combination in the hash is memory costly.  The trade off is that it
 * gives users the ability to adjust the configuration file
 * specifically for their needs and only for a particular motherboard
 * they care about.
 */

struct ipmi_interpret_sensor_config {
  char *option_str;
  int state;
};

struct ipmi_interpret_sel_config {
  char *option_str;
  int assertion_state;
  int deassertion_state;
};

struct ipmi_interpret_sel_oem_sensor_data {
  unsigned int event_direction_any_flag;
  uint8_t event_direction;
  unsigned int event_data1_any_flag;
  uint8_t event_data1;
  unsigned int event_data2_any_flag;
  uint8_t event_data2;
  unsigned int event_data3_any_flag;
  uint8_t event_data3;
  unsigned int sel_state;
};

struct ipmi_interpret_sel_oem_sensor_config {
  char key[IPMI_OEM_HASH_KEY_BUFLEN + 1];
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  struct ipmi_interpret_sel_oem_sensor_data oem_sensor_data[IPMI_SEL_OEM_SENSOR_MAX];
  unsigned int oem_sensor_data_count;
};

struct ipmi_interpret_sel_oem_data_byte {
  unsigned int any_flag;
  uint8_t oem_data_byte;
};

struct ipmi_interpret_sel_oem_record {
  struct ipmi_interpret_sel_oem_data_byte oem_bytes[IPMI_SEL_OEM_DATA_MAX];
  unsigned int oem_bytes_count;
  unsigned int sel_state;
};

struct ipmi_interpret_sel_oem_record_config {
  char key[IPMI_OEM_HASH_KEY_BUFLEN + 1];
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t record_type;
  struct ipmi_interpret_sel_oem_record oem_record[IPMI_SEL_OEM_RECORD_MAX];
  unsigned int oem_record_count;
};

struct ipmi_interpret_sel {
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_threshold_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_temperature_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_temperature_limit_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_temperature_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_voltage_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_voltage_limit_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_voltage_performance_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_voltage_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_current_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_fan_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_fan_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_fan_device_present_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_fan_transition_availability_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_fan_redundancy_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_physical_security_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_platform_security_violation_attempt_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_processor_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_processor_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_power_supply_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_power_supply_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_power_supply_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_power_supply_redundancy_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_power_unit_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_power_unit_device_present_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_power_unit_redundancy_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_cooling_device_redundancy_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_memory_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_memory_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_memory_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_memory_redundancy_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_drive_slot_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_drive_slot_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_drive_slot_predictive_failure_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_drive_slot_device_present_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_post_memory_resize_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_system_firmware_progress_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_system_firmware_progress_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_event_logging_disabled_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_system_event_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_system_event_transition_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_system_event_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_critical_interrupt_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_button_switch_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_button_switch_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_button_switch_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_module_board_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_module_board_device_present_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_chassis_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_chip_set_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_chip_set_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_cable_interconnect_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_cable_interconnect_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_system_boot_initiated_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_boot_error_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_boot_error_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_boot_error_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_os_boot_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_os_critical_stop_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_os_critical_stop_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_slot_connector_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_slot_connector_transition_severity_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_system_acpi_power_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_watchdog2_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_platform_alert_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_platform_alert_state_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_entity_presence_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_entity_presence_device_present_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_lan_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_management_subsystem_health_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_battery_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_session_audit_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_version_change_config;
  struct ipmi_interpret_sel_config **ipmi_interpret_sel_fru_state_config;

  hash_t sel_oem_sensor_config;
  hash_t sel_oem_record_config;
};

struct ipmi_interpret_sensor_oem_state {
  uint16_t sensor_event_bitmask;
  unsigned int sensor_state;
  int oem_state_type;
};

struct ipmi_interpret_sensor_oem_config {
  char key[IPMI_OEM_HASH_KEY_BUFLEN + 1];
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  struct ipmi_interpret_sensor_oem_state oem_state[IPMI_INTERPRET_MAX_BITMASKS];
  unsigned int oem_state_count;
};

struct ipmi_interpret_sensor {
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_threshold_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_temperature_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_temperature_limit_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_temperature_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_voltage_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_voltage_limit_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_voltage_performance_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_voltage_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_current_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_fan_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_fan_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_fan_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_fan_transition_availability_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_fan_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_physical_security_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_platform_security_violation_attempt_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_processor_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_processor_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_power_supply_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_power_supply_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_power_supply_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_power_supply_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_power_unit_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_power_unit_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_power_unit_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_cooling_device_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_memory_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_memory_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_memory_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_memory_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_drive_slot_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_drive_slot_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_drive_slot_predictive_failure_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_drive_slot_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_post_memory_resize_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_system_firmware_progress_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_system_firmware_progress_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_event_logging_disabled_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_system_event_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_system_event_transition_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_system_event_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_critical_interrupt_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_button_switch_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_button_switch_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_button_switch_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_module_board_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_module_board_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_chassis_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_chip_set_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_cable_interconnect_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_cable_interconnect_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_boot_error_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_boot_error_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_boot_error_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_os_boot_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_os_critical_stop_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_slot_connector_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_slot_connector_transition_severity_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_system_acpi_power_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_watchdog2_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_platform_alert_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_entity_presence_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_entity_presence_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_management_subsystem_health_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_battery_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_session_audit_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_version_change_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_sensor_fru_state_config;

  hash_t sensor_oem_config;
};

struct ipmi_interpret_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;
  uint32_t manufacturer_id;
  uint16_t product_id;

  ipmi_sel_ctx_t sel_ctx;

  struct ipmi_interpret_sel interpret_sel;
  struct ipmi_interpret_sensor interpret_sensor;
};

#endif /* IPMI_INTERPRET_DEFS_H */
