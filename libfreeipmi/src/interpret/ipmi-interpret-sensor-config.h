/*
  Copyright (C) 2003-2010 FreeIPMI Core Team

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
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.

*/

#ifndef _IPMI_INTERPRET_SENSOR_CONFIG_H
#define _IPMI_INTERPRET_SENSOR_CONFIG_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"

#include "ipmi-interpret-sensor-config.h"
#include "ipmi-interpret-defs.h"

struct ipmi_interpret_sensor_config {
  char *option_str;
  int sensor_state;
};

struct ipmi_interpret_sensors {
  struct ipmi_interpret_sensor_config **ipmi_interpret_threshold_sensor_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_voltage_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_voltage_performance_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_fan_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_fan_transition_availability_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_fan_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_physical_security_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_platform_security_violation_attempt_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_processor_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_processor_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_power_supply_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_power_supply_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_power_supply_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_power_unit_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_power_unit_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_power_unit_redundancy_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_memory_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_drive_slot_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_drive_slot_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_drive_slot_predictive_failure_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_drive_slot_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_system_firmware_progress_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_event_logging_disabled_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_system_event_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_critical_interrupt_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_button_switch_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_button_switch_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_module_board_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_module_board_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_cable_interconnect_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_boot_error_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_slot_connector_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_system_acpi_power_state_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_watchdog2_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_entity_presence_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_entity_presence_device_present_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_management_subsystem_health_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_battery_config;
  struct ipmi_interpret_sensor_config **ipmi_interpret_fru_state_config;
};

extern struct ipmi_interpret_sensor_config ipmi_interpret_threshold_sensor_config[];
extern unsigned int ipmi_interpret_threshold_sensor_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_voltage_state_config[];
extern unsigned int ipmi_interpret_voltage_state_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_voltage_performance_config[];
extern unsigned int ipmi_interpret_voltage_performance_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_fan_device_present_config[];
extern unsigned int ipmi_interpret_fan_device_present_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_fan_transition_availability_config[];
extern unsigned int ipmi_interpret_fan_transition_availability_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_fan_redundancy_config[];
extern unsigned int ipmi_interpret_fan_redundancy_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_physical_security_config[];
extern unsigned int ipmi_interpret_physical_security_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_platform_security_violation_attempt_config[];
extern unsigned int ipmi_interpret_platform_security_violation_attempt_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_processor_config[];
extern unsigned int ipmi_interpret_processor_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_processor_state_config[];
extern unsigned int ipmi_interpret_processor_state_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_power_supply_config[];
extern unsigned int ipmi_interpret_power_supply_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_power_supply_state_config[];
extern unsigned int ipmi_interpret_power_supply_state_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_power_supply_redundancy_config[];
extern unsigned int ipmi_interpret_power_supply_redundancy_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_power_unit_config[];
extern unsigned int ipmi_interpret_power_unit_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_power_unit_device_present_config[];
extern unsigned int ipmi_interpret_power_unit_device_present_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_power_unit_redundancy_config[];
extern unsigned int ipmi_interpret_power_unit_redundancy_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_memory_config[];
extern unsigned int ipmi_interpret_memory_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_config[];
extern unsigned int ipmi_interpret_drive_slot_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_state_config[];
extern unsigned int ipmi_interpret_drive_slot_state_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_predictive_failure_config[];
extern unsigned int ipmi_interpret_drive_slot_predictive_failure_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_device_present_config[];
extern unsigned int ipmi_interpret_drive_slot_device_present_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_system_firmware_progress_config[];
extern unsigned int ipmi_interpret_system_firmware_progress_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_event_logging_disabled_config[];
extern unsigned int ipmi_interpret_event_logging_disabled_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_system_event_config[];
extern unsigned int ipmi_interpret_system_event_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_critical_interrupt_config[];
extern unsigned int ipmi_interpret_critical_interrupt_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_button_switch_config[];
extern unsigned int ipmi_interpret_button_switch_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_button_switch_state_config[];
extern unsigned int ipmi_interpret_button_switch_state_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_module_board_state_config[];
extern unsigned int ipmi_interpret_module_board_state_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_module_board_device_present_config[];
extern unsigned int ipmi_interpret_module_board_device_present_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_cable_interconnect_config[];
extern unsigned int ipmi_interpret_cable_interconnect_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_boot_error_config[];
extern unsigned int ipmi_interpret_boot_error_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_slot_connector_config[];
extern unsigned int ipmi_interpret_slot_connector_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_system_acpi_power_state_config[];
extern unsigned int ipmi_interpret_system_acpi_power_state_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_watchdog2_config[];
extern unsigned int ipmi_interpret_watchdog2_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_entity_presence_config[];
extern unsigned int ipmi_interpret_entity_presence_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_entity_presence_device_present_config[];
extern unsigned int ipmi_interpret_entity_presence_device_present_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_management_subsystem_health_config[];
extern unsigned int ipmi_interpret_management_subsystem_health_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_battery_config[];
extern unsigned int ipmi_interpret_battery_config_len;

extern struct ipmi_interpret_sensor_config ipmi_interpret_fru_state_config[];
extern unsigned int ipmi_interpret_fru_state_config_len;

#endif /* ipmi-interpret-sensor-config.h */

