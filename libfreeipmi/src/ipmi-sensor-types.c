/* 
   ipmi-sensor-types.c - IPMI Sensor Types

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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

#include "freeipmi.h"

const char *const ipmi_sensor_types[] = 
  {
    "Unknown", 
    "Temperature",
    "Voltage",
    "Current",
    "Fan",
    "Platform Chassis Intrusion",
    "Platform Security Violation",
    "Processor",
    "Power Supply",
    "Power Unit",
    "Cooling Device",
    "FRU Sensor",
    "Memory",
    "Drive Slot",
    "Post Memory Resize",
    "System Firmware",
    "Event Logging Disabled",
    "Watchdog1",
    "System Event",
    "Critical Interrupt",
    "Button",
    "Board",
    "Microcontroller",
    "Add In Card",
    "Chassis",
    "Chip Set",
    "Other Fru",
    "Cable Interconnect",
    "Terminator",
    "System Boot Initiated",
    "Boot Error",
    "OS Boot",
    "OS Critical Stop",
    "Slot Connector",
    "ACPI Power State",
    "Watchdog 2",
    "Platform Alert",
    "Entity Presence",
    "Monitor Asic",
    "LAN",
    "Management Subsystem Health",
    "Battery",
    "Session Audit",
    "Version Change",
    "FRU State",
    NULL
  };

const char *const ipmi_oem_sensor_type = "OEM Reserved";

const char *const ipmi_sensor_units[] = 
  {
    "Unspecified", 
    "Degrees C", 
    "Degrees F", 
    "Degrees K", 
    "Volts", 
    "Amps", 
    "Watts", 
    "Joules", 
    "Coulombs", 
    "VA", 
    "Nits", 
    "Lumen", 
    "Lux", 
    "Candela", 
    "KPA", 
    "PSI", 
    "Newton", 
    "CFM", 
    "RPM", 
    "Hz", 
    "uSec", 
    "mSec", 
    "Sec", 
    "Min", 
    "Hour", 
    "Day", 
    "Week", 
    "Mil", 
    "Inches", 
    "FEET", 
    "Cu In", 
    "Cu Feet", 
    "mm", 
    "cm", 
    "m", 
    "Cu cm", 
    "Cu m", 
    "Liters", 
    "Fluid Ounce", 
    "Radians", 
    "Steradians", 
    "Revolutions", 
    "Cycles", 
    "Gravities", 
    "Ounce", 
    "Pound", 
    "Ft LB", 
    "Oz In", 
    "Gauss", 
    "Gilberts", 
    "Henry", 
    "Millihenry", 
    "Farad", 
    "uFarad", 
    "Ohms", 
    "Siemens", 
    "Mole", 
    "Becquerel", 
    "PPM", 
    "Reserved", 
    "Decibels", 
    "DBA", 
    "DBC", 
    "Gray", 
    "Sievert", 
    "Color Temp Deg K", 
    "Bit", 
    "Kb", 
    "Mb", 
    "Gb", 
    "B", 
    "KB", 
    "MB", 
    "GB", 
    "Word", 
    "Dword", 
    "Line", 
    "Hit", 
    "Miss", 
    "Retry", 
    "Reset", 
    "Overrun Overflow", 
    "Underrun", 
    "Collision", 
    "Packets", 
    "Messages", 
    "Characters", 
    "Error", 
    "Correctable Error", 
    "Uncorrectable Error",
    NULL
  };

const char *const ipmi_sensor_units_short[] = 
  {
    "Unspecified",
    "C",
    "F",
    "K",
    "V",
    "A",
    "W",
    "J",
    "Coulombs",
    "VA",
    "Nits",
    "Lumen",
    "Lux",
    "Candela",
    "KPA",
    "PSI",
    "Newton",
    "CFM",
    "RPM",
    "Hz",
    "us",
    "ms",
    "s",
    "Min",
    "Hour",
    "Day",
    "Week",
    "Mil",
    "Inches",
    "FEET",
    "Cu In",
    "Cu Feet",
    "mm",
    "cm",
    "m",
    "cm3",
    "m3",
    "L",
    "Fluid Ounce",
    "Radians",
    "Steradians",
    "Revolutions",
    "Cycles",
    "Gravities",
    "Ounce",
    "Pound",
    "Ft LB",
    "Oz In",
    "Gauss",
    "Gilberts",
    "Henry",
    "Millihenry",
    "Farad",
    "uFarad",
    "Ohms",
    "Siemens",
    "Mole",
    "Becquerel",
    "PPM",
    "Reserved",
    "Decibels",
    "DBA",
    "DBC",
    "Gray",
    "Sievert",
    "Color Temp Deg K",
    "Bit",
    "Kb",
    "Mb",
    "Gb",
    "B",
    "KB",
    "MB",
    "GB",
    "Word",
    "Dword",
    "Line",
    "Hit",
    "Miss",
    "Retry",
    "Reset",
    "Overrun Overflow",
    "Underrun",
    "Collision",
    "Pkts",
    "Msgs",
    "Chars",
    "Err",
    "Correctable Err",
    "Uncorrectable Err",
    NULL
  };

int 
ipmi_sensor_classify (uint8_t event_reading_type_code)
{
  if (event_reading_type_code == 0x01)
    return IPMI_SENSOR_CLASS_THRESHOLD;
  
  if (event_reading_type_code >= 0x02 && event_reading_type_code <= 0x0C)
    return IPMI_SENSOR_CLASS_GENERIC_DISCRETE;
  
  if (event_reading_type_code == 0x6F)
    return IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE;
  
  if (event_reading_type_code >= 0x70 && event_reading_type_code <= 0x7F)
    return IPMI_SENSOR_CLASS_OEM;
  
  return IPMI_SENSOR_CLASS_NOT_AVAILABLE;
}

int 
ipmi_is_oem_reserved_sensor_type (int sensor_type)
{
  if ((sensor_type >= 0xC0) && (sensor_type <= 0xFF))
    return (1);
  else 
    return (0);
}

const char *
ipmi_get_sensor_group (int sensor_type)
{
  if (sensor_type <= IPMI_SENSOR_TYPE_FRU_STATE)
    return (ipmi_sensor_types[sensor_type]);
  
  if (ipmi_is_oem_reserved_sensor_type (sensor_type))
    return ipmi_oem_sensor_type;

  return ipmi_sensor_types[0];
}

