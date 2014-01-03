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

#include "freeipmi/spec/ipmi-entity-ids-spec.h"

#include "freeipmi-portability.h"

/* achu: case preserved from spec.  space before and after slashes removed */
const char *const ipmi_entity_ids[] =
  {
    "unspecified",
    "other",
    "unknown",
    "processor",
    "disk or disk bay",
    "peripheral bay",
    "system management module",
    "system board",
    "memory module",
    "processor module",
    "power supply",
    "add-in card",
    "front panel board",
    "back panel board",
    "power system board",
    "drive backplane",
    "system internal expansion board",
    "other system board",
    "processor board",
    "power unit/power domain",
    "power module/DC-to-DC converter",
    "power management/power distribution board",
    "chassis back panel board",
    "system chassis",
    "sub-chassis",
    "Other chassis board",
    "Disk Drive Bay",
    "Peripheral Bay",
    "Device Bay",
    "fan/cooling device",
    "cooling unit/cooling domain",
    "cable/interconnect",
    "memory device",
    "System Management Software",
    "System Firmware",
    "Operating System",
    "system bus",
    "Group",
    "Remote Management Communication Device",
    "External Environment",
    "battery",
    "Processing blade",
    "Connectivity switch",
    "Processor/memory module",
    "I/O module",
    "Processor/IO module",    /* no slash between IO in spec */
    "Management Controller Firmware",
    "IPMI Channel",
    "PCI Bus",
    "PCI Express Bus",
    "SCSI Bus",
    "SATA/SAS bus",
    "Processor/front-side bus",
    "Real Time Clock",
    "reserved",
    "reserved",                 /* listed as air-inlet, assume is typo */
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "air inlet",
    "processor/CPU",
    "baseboard/main system board",
    NULL
  };

const char *const ipmi_entity_ids_pretty[] =
  {
    "Unspecified",
    "Other",
    "Unknown",
    "Processor",
    "Disk",
    "Peripheral Bay",
    "System Management Module",
    "System Board",
    "Memory Module",
    "Processor Module",
    "Power Supply",
    "Add-in Card",
    "Front Panel Board",
    "Back Panel Board",
    "Power System Board",
    "Drive Backplane",
    "System Internal Expansion Board",
    "Other System Board",
    "Processor Board",
    "Power Unit",
    "Power Module",
    "Power Management",
    "Chassis Back Panel Board",
    "System Chassis",
    "Sub-chassis",
    "Other Chassis Board",
    "Disk Drive Bay",
    "Peripheral Bay",
    "Device Bay",
    "Fan",
    "Cooling Unit",
    "Cable/Interconnect",       /* keep '/', this is like sensor group naming */
    "Memory Device",
    "System Management Software",
    "System Firmware",
    "Operating System",
    "System Bus",
    "Group",
    "Remote Management Communication Device",
    "External Environment",
    "Battery",
    "Processing Blade",
    "Connectivity Switch",
    "Processor/Memory Module",  /* keep '/', this is Processor + Memory Module unit */
    "I/O Module",
    "Processor/IO module",      /* keep '/' this is a Processor + IO Module unit, no slash between IO in spec */
    "Management Controller Firmware",
    "IPMI Channel",
    "PCI Bus",
    "PCI Express Bus",
    "SCSI Bus",
    "SATA/SAS bus",
    "Processor/front-side bus",
    "Real Time Clock",
    "reserved",
    "reserved",                 /* listed as air-inlet, assume is typo */
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Air Inlet",
    "Processor",                /* equivalent to 0x03, see errata */
    "System Board",             /* equivalent to 0x07, see errata */
    NULL
  };

const char *const ipmi_entity_id_chassis_specific = "Chassis Specific";
const char *const ipmi_entity_id_board_set_specific = "Board-Set Specific";
const char *const ipmi_entity_id_oem_system_integrator = "OEM System Integrator";
