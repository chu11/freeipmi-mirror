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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include "freeipmi/spec/ipmi-sensor-types-spec.h"

#include "freeipmi-portability.h"

const char *const ipmi_sensor_types[] =
  {
    "Reserved",
    "Temperature",
    "Voltage",
    "Current",
    "Fan",
    "Physical Security",
    "Platform Security Violation Attempt",
    "Processor",
    "Power Supply",
    "Power Unit",
    "Cooling Device",
    "Other Units Based Sensor",
    "Memory",
    "Drive Slot",
    "POST Memory Resize",
    "System Firmware Progress",
    "Event Logging Disabled",
    "Watchdog 1",
    "System Event",
    "Critical Interrupt",
    "Button/Switch",
    "Module/Board",
    "Microcontroller/Coprocessor",
    "Add In Card",
    "Chassis",
    "Chip Set",
    "Other Fru",
    "Cable/Interconnect",
    "Terminator",
    "System Boot Initiated",
    "Boot Error",
    "OS Boot",
    "OS Critical Stop",
    "Slot/Connector",
    "System ACPI Power State",
    "Watchdog 2",
    "Platform Alert",
    "Entity Presence",
    "Monitor ASIC/IC",
    "LAN",
    "Management Subsystem Health",
    "Battery",
    "Session Audit",
    "Version Change",
    "FRU State",
    NULL
  };

const char *const ipmi_oem_sensor_type = "OEM Reserved";

