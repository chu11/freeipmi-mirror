/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <freeipmi/freeipmi.h>

#include "pef-config.h"
#include "pef-config-map.h"

#include "freeipmi-portability.h"

int
alert_destination_type_number (const char *source)
{
  if (same (source, "pet_trap"))
    return IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION;
  if (same (source, "oem1"))
    return IPMI_DESTINATION_TYPE_OEM1;
  if (same (source, "oem2"))
    return IPMI_DESTINATION_TYPE_OEM2;
  return -1;
}

char *
alert_destination_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION:
      return "PET_Trap";
    case IPMI_DESTINATION_TYPE_OEM1:
      return "OEM1";
    case IPMI_DESTINATION_TYPE_OEM2:
      return "OEM2";
    }
  return "";
}

int
alert_gateway_number (const char *source)
{
  if (same (source, "default"))
    return IPMI_GATEWAY_SELECTOR_DEFAULT;
  if (same (source, "backup"))
    return IPMI_GATEWAY_SELECTOR_BACKUP;
  return -1;
}

char *
alert_gateway_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_GATEWAY_SELECTOR_DEFAULT:
      return "Default";
    case IPMI_GATEWAY_SELECTOR_BACKUP:
      return "Backup";
    }
  return "";
}

int
policy_type_number (const char *source)
{
  if (same (source, "always_send_to_this_destination"))
    return IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION;
  if (same (source, "proceed_to_next_entry"))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY;
  if (same (source, "do_not_proceed_any_more_entries"))
    return IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES;
  if (same (source, "proceed_to_next_entry_different_channel"))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL;
  if (same (source, "proceed_to_next_entry_different_destination_type"))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE;
  return -1;
}

char *
policy_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION:
      return "Always_Send_To_This_Destination";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY:
      return "Proceed_To_Next_Entry";
      break;
    case IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES:
      return "Do_Not_Proceed_Any_More_Entries";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL:
      return "Proceed_To_Next_Entry_Different_Channel";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE:
      return "Proceed_To_Next_Entry_Different_Destination_Type";
      break;
    }

  return "";
}

int 
filter_type_number (const char *source)
{
  if (same (source, "manufacturer_pre_configured"))
    return IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER;
  if (same (source, "software_configurable"))
    return IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER;
  if (same (source, "reserved1"))
    return 0x1;
  if (same (source, "reserved3"))
    return 0x3;

  return -1;
}

char *
filter_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER:
      return "Manufacturer_Pre_Configured";
    case IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER:
      return "Software_Configurable";
    case 0x1:
      return "Reserved1";
    case 0x3:
      return "Reserved3";
    }
  return "";
}

int 
event_severity_number (const char *source)
{
  if (same (source, "unspecified"))
    return IPMI_EVENT_SEVERITY_UNSPECIFIED;
  if (same (source, "monitor"))
    return IPMI_EVENT_SEVERITY_MONITOR;
  if (same (source, "information"))
    return IPMI_EVENT_SEVERITY_INFORMATION;
  if (same (source, "ok"))
    return IPMI_EVENT_SEVERITY_OK;
  if (same (source, "non_critical"))
    return IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION;
  if (same (source, "critical"))
    return IPMI_EVENT_SEVERITY_CRITICAL_CONDITION;
  if (same (source, "non_recoverable"))
    return IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION;
  
  return -1;
}

char *
event_severity_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_EVENT_SEVERITY_UNSPECIFIED:
      return "Unspecified";
    case IPMI_EVENT_SEVERITY_MONITOR:
      return "Monitor";
    case IPMI_EVENT_SEVERITY_INFORMATION:
      return "Information";
    case IPMI_EVENT_SEVERITY_OK:
      return "OK";
    case IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION:
      return "Non_Critical";
    case IPMI_EVENT_SEVERITY_CRITICAL_CONDITION:
      return "Critical";
    case IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION:
      return "Non_Recoverable";
    }
  return "";
}

int 
sensor_type_number (const char *source)
{
  if (same (source, "reserved"))
    return IPMI_EVENT_SENSOR_TYPE_RESERVED;
  if (same (source, "temperature"))
    return IPMI_EVENT_SENSOR_TYPE_TEMPERATURE;
  if (same (source, "voltage"))
    return IPMI_EVENT_SENSOR_TYPE_VOLTAGE;
  if (same (source, "current"))
    return IPMI_EVENT_SENSOR_TYPE_CURRENT;
  if (same (source, "fan"))
    return IPMI_EVENT_SENSOR_TYPE_FAN;
  if (same (source, "physical_security"))
    return IPMI_EVENT_SENSOR_TYPE_PHYSICAL_SECURITY;
  if (same (source, "platform_security_violation_attempt"))
    return IPMI_EVENT_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT;
  if (same (source, "processor"))
    return IPMI_EVENT_SENSOR_TYPE_PROCESSOR;
  if (same (source, "power_supply"))
    return IPMI_EVENT_SENSOR_TYPE_POWER_SUPPLY;
  if (same (source, "power_unit"))
    return IPMI_EVENT_SENSOR_TYPE_POWER_UNIT;
  if (same (source, "cooling_device"))
    return IPMI_EVENT_SENSOR_TYPE_COOLING_DEVICE;
  if (same (source, "other_units_based_sensor"))
    return IPMI_EVENT_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR;
  if (same (source, "memory"))
    return IPMI_EVENT_SENSOR_TYPE_MEMORY;
  if (same (source, "drive_slot"))
    return IPMI_EVENT_SENSOR_TYPE_DRIVE_SLOT;
  if (same (source, "post_memory_resize"))
    return IPMI_EVENT_SENSOR_TYPE_POST_MEMORY_RESIZE;
  if (same (source, "system_firmware_progress"))
    return IPMI_EVENT_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS;
  if (same (source, "event_logging_disabled"))
    return IPMI_EVENT_SENSOR_TYPE_EVENT_LOGGING_DISABLED;
  if (same (source, "watchdog1"))
    return IPMI_EVENT_SENSOR_TYPE_WATCHDOG1;
  if (same (source, "system_event"))
    return IPMI_EVENT_SENSOR_TYPE_SYSTEM_EVENT;
  if (same (source, "critical_interrupt"))
    return IPMI_EVENT_SENSOR_TYPE_CRITICAL_INTERRUPT;
  if (same (source, "button_switch"))
    return IPMI_EVENT_SENSOR_TYPE_BUTTON_SWITCH;
  if (same (source, "module_board"))
    return IPMI_EVENT_SENSOR_TYPE_MODULE_BOARD;
  if (same (source, "microcontroller_coprocessor"))
    return IPMI_EVENT_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR;
  if (same (source, "add_in_card"))
    return IPMI_EVENT_SENSOR_TYPE_ADD_IN_CARD;
  if (same (source, "chassis"))
    return IPMI_EVENT_SENSOR_TYPE_CHASSIS;
  if (same (source, "chip_set"))
    return IPMI_EVENT_SENSOR_TYPE_CHIP_SET;
  if (same (source, "other_fru"))
    return IPMI_EVENT_SENSOR_TYPE_OTHER_FRU;
  if (same (source, "cable_interconnect"))
    return IPMI_EVENT_SENSOR_TYPE_CABLE_INTERCONNECT;
  if (same (source, "terminator"))
    return IPMI_EVENT_SENSOR_TYPE_TERMINATOR;
  if (same (source, "system_boot_initiated"))
    return IPMI_EVENT_SENSOR_TYPE_SYSTEM_BOOT_INITIATED;
  if (same (source, "boot_error"))
    return IPMI_EVENT_SENSOR_TYPE_BOOT_ERROR;
  if (same (source, "os_boot"))
    return IPMI_EVENT_SENSOR_TYPE_OS_BOOT;
  if (same (source, "os_critical_stop"))
    return IPMI_EVENT_SENSOR_TYPE_OS_CRITICAL_STOP;
  if (same (source, "slot_connector"))
    return IPMI_EVENT_SENSOR_TYPE_SLOT_CONNECTOR;
  if (same (source, "system_acpi_power_state"))
    return IPMI_EVENT_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE;
  if (same (source, "watchdog2"))
    return IPMI_EVENT_SENSOR_TYPE_WATCHDOG2;
  if (same (source, "platform_alert"))
    return IPMI_EVENT_SENSOR_TYPE_PLATFORM_ALERT;
  if (same (source, "entity_presence"))
    return IPMI_EVENT_SENSOR_TYPE_ENTITY_PRESENCE;
  if (same (source, "monitor_asic_ic"))
    return IPMI_EVENT_SENSOR_TYPE_MONITOR_ASIC_IC;
  if (same (source, "lan"))
    return IPMI_EVENT_SENSOR_TYPE_LAN;
  if (same (source, "management_subsystem_health"))
    return IPMI_EVENT_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH;
  if (same (source, "battery"))
    return IPMI_EVENT_SENSOR_TYPE_BATTERY;
  if (same (source, "session_audit"))
    return IPMI_EVENT_SENSOR_TYPE_SESSION_AUDIT;
  if (same (source, "version_change"))
    return IPMI_EVENT_SENSOR_TYPE_VERSION_CHANGE;
  if (same (source, "fru_state"))
    return IPMI_EVENT_SENSOR_TYPE_FRU_STATE;
  if (same (source, "any"))
    return IPMI_EVENT_SENSOR_TYPE_ANY;
  
  return -1;
}

char *
sensor_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_EVENT_SENSOR_TYPE_RESERVED:
      return "Reserved";
    case IPMI_EVENT_SENSOR_TYPE_TEMPERATURE:
      return "Temperature";
    case IPMI_EVENT_SENSOR_TYPE_VOLTAGE:
      return "Voltage";
    case IPMI_EVENT_SENSOR_TYPE_CURRENT:
      return "Current";
    case IPMI_EVENT_SENSOR_TYPE_FAN:
      return "Fan";
    case IPMI_EVENT_SENSOR_TYPE_PHYSICAL_SECURITY:
      return "Physical_Security";
    case IPMI_EVENT_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return "Platform_Security_Violation_Attempt";
    case IPMI_EVENT_SENSOR_TYPE_PROCESSOR:
      return "Processor";
    case IPMI_EVENT_SENSOR_TYPE_POWER_SUPPLY:
      return "Power_Supply";
    case IPMI_EVENT_SENSOR_TYPE_POWER_UNIT:
      return "Power_Unit";
    case IPMI_EVENT_SENSOR_TYPE_COOLING_DEVICE:
      return "Cooling_Device";
    case IPMI_EVENT_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR:
      return "Other_Units_Based_Sensor";
    case IPMI_EVENT_SENSOR_TYPE_MEMORY:
      return "Memory";
    case IPMI_EVENT_SENSOR_TYPE_DRIVE_SLOT:
      return "Drive_Slot";
    case IPMI_EVENT_SENSOR_TYPE_POST_MEMORY_RESIZE:
      return "Post_Memory_Resize";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return "System_Firmware_Progress";
    case IPMI_EVENT_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return "Event_Logging_Disabled";
    case IPMI_EVENT_SENSOR_TYPE_WATCHDOG1:
      return "Watchdog1";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_EVENT:
      return "System_Event";
    case IPMI_EVENT_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return "Critical_interrupt";
    case IPMI_EVENT_SENSOR_TYPE_BUTTON_SWITCH:
      return "Button_Switch";
    case IPMI_EVENT_SENSOR_TYPE_MODULE_BOARD:
      return "Module_Board";
    case IPMI_EVENT_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR:
      return "Microcontroller_Coprocessor";
    case IPMI_EVENT_SENSOR_TYPE_ADD_IN_CARD:
      return "Add_In_Card";
    case IPMI_EVENT_SENSOR_TYPE_CHASSIS:
      return "Chassis";
    case IPMI_EVENT_SENSOR_TYPE_CHIP_SET:
      return "Chip_Set";
    case IPMI_EVENT_SENSOR_TYPE_OTHER_FRU:
      return "Other_FRU";
    case IPMI_EVENT_SENSOR_TYPE_CABLE_INTERCONNECT:
      return "Cable_Interconnect";
    case IPMI_EVENT_SENSOR_TYPE_TERMINATOR:
      return "Terminator";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return "System_Boot_Initiated";
    case IPMI_EVENT_SENSOR_TYPE_BOOT_ERROR:
      return "Boot_Error";
    case IPMI_EVENT_SENSOR_TYPE_OS_BOOT:
      return "OS_Boot";
    case IPMI_EVENT_SENSOR_TYPE_OS_CRITICAL_STOP:
      return "OS_Critical_Stop";
    case IPMI_EVENT_SENSOR_TYPE_SLOT_CONNECTOR:
      return "Slot_Connector";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE:
      return "System_ACPI_Power_State";
    case IPMI_EVENT_SENSOR_TYPE_WATCHDOG2:
      return "Watchdog2";
    case IPMI_EVENT_SENSOR_TYPE_PLATFORM_ALERT:
      return "Platform_Alert";
    case IPMI_EVENT_SENSOR_TYPE_ENTITY_PRESENCE:
      return "Entity_Presence";
    case IPMI_EVENT_SENSOR_TYPE_MONITOR_ASIC_IC:
      return "Monitor_ASIC_IC";
    case IPMI_EVENT_SENSOR_TYPE_LAN:
      return "LAN";
    case IPMI_EVENT_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return "Management_Subsystem_Health";
    case IPMI_EVENT_SENSOR_TYPE_BATTERY:
      return "Battery";
    case IPMI_EVENT_SENSOR_TYPE_SESSION_AUDIT:
      return "Session_Audit";
    case IPMI_EVENT_SENSOR_TYPE_VERSION_CHANGE:
      return "Version_Change";
    case IPMI_EVENT_SENSOR_TYPE_FRU_STATE:
      return "FRU_State";
    case IPMI_EVENT_SENSOR_TYPE_ANY:
      return "Any";

    }
  return "";
}

