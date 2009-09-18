/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

/************************************************
 * Generic Event Reading Strings (FULL STRINGS) *
 ************************************************/

static char * ipmi_generic_event_reading_type_code_01_desc[] =
  {
    "Lower Non-critical - going low",
    "Lower Non-critical - going high",
    "Lower Critical - going low",
    "Lower Critical - going high",
    "Lower Non-recoverable - going low",
    "Lower Non-recoverable - going high",
    "Upper Non-critical - going low",
    "Upper Non-critical - going high",
    "Upper Critical - going low",
    "Upper Critical - going high",
    "Upper Non-recoverable - going low",
    "Upper Non-recoverable - going high",
    NULL
  };
static int ipmi_generic_event_reading_type_code_01_desc_max = 0x0B;

static char * ipmi_generic_event_reading_type_code_02_desc[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
static int ipmi_generic_event_reading_type_code_02_desc_max = 0x02;

static char * ipmi_generic_event_reading_type_code_03_desc[] =  {
  "State Deasserted",
  "State Asserted",
  NULL
};
static int ipmi_generic_event_reading_type_code_03_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_04_desc[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
static int ipmi_generic_event_reading_type_code_04_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_05_desc[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
static int ipmi_generic_event_reading_type_code_05_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_06_desc[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
static int ipmi_generic_event_reading_type_code_06_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_07_desc[] =
  {
    "transition to OK",
    "transition to Non-Critical from OK",
    "transition to Critical from less severe",
    "transition to Non-recoverable from less severe",
    "transition to Non-Critical from more severe",
    "transition to Critical from Non-recoverable",
    "transition to Non-recoverable",
    "Monitor",
    "Informational",
    NULL
  };
static int ipmi_generic_event_reading_type_code_07_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_08_desc[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
static int ipmi_generic_event_reading_type_code_08_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_09_desc[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
static int ipmi_generic_event_reading_type_code_09_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_0A_desc[] =
  {
    "transition to Running",
    "transition to In Test",
    "transition to Power Off",
    "transition to On Line",
    "transition to Off Line",
    "transition to Off Duty",
    "transition to Degraded",
    "transition to Power Save",
    "Install Error",
    NULL
  };
static int ipmi_generic_event_reading_type_code_0A_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_0B_desc[] =
  {
    "Fully Redundant (formerly \"Redundancy Regained\")",
    "Redundancy Lost",
    "Redundancy Degraded",
    "Non-redundant:Sufficient Resources from Redundant",
    "Non-redundant:Sufficient Resources from Insufficient Resources",
    "Non-redundant:Insufficient Resources",
    "Redundancy Degraded from Fully Redundant",
    "Redundancy Degraded from Non-redundant",
    NULL
  };
static int ipmi_generic_event_reading_type_code_0B_desc_max = 0x07;

static char * ipmi_generic_event_reading_type_code_0C_desc[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
static int ipmi_generic_event_reading_type_code_0C_desc_max = 0x03;

/************************************** 
 * Sensor Type Strings (FULL STRINGS) *
 **************************************/

static char * ipmi_sensor_type_code_01_desc[] =
  {
    "Temperature",
    NULL
  };
static int ipmi_sensor_type_code_01_desc_max = 0x00;

static char * ipmi_sensor_type_code_02_desc[] =
  {
    "Voltage",
    NULL
  };
static int ipmi_sensor_type_code_02_desc_max = 0x00;

static char * ipmi_sensor_type_code_03_desc[] =
  {
    "Current",
    NULL
  };
static int ipmi_sensor_type_code_03_desc_max = 0x00;

static char * ipmi_sensor_type_code_04_desc[] =
  {
    "Fan",
    NULL
  };
static int ipmi_sensor_type_code_04_desc_max = 0x00;

/* achu: 'undock' removed as noted in errata */
static char * ipmi_sensor_type_code_05_desc[] =
  {
    "General Chassis Intrusion",
    "Drive Bay intrusion",
    "I/O Card area intrusion",
    "Processor area intrusion",
    "LAN Leash Lost (system is unplugged from LAN)",
    "Unauthorized dock",
    "FAN area intrusion (supports detection of hot plug fan tampering)",
    NULL
  };
static int ipmi_sensor_type_code_05_desc_max = 0x06;

static char * ipmi_sensor_type_code_06_desc[] =
  {
    "Secure Mode (Front Panel Lockout) Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation attempt - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
static int ipmi_sensor_type_code_06_desc_max = 0x05;

static char * ipmi_sensor_type_code_07_desc[] =
  {
    "IERR",
    "Thermal Trip",
    "FRB1/BIST failure",
    "FRB2/Hang in POST failure (used hang is believed to be due or related to a processor failure. Use System Firmware Progress sensor for other BIOS hangs.)",
    "FRB3/Processor Startup/Initialization failure (CPU didn't start)",
    "Configuration Error",
    "SM BIOS `Uncorrectable CPU-complex Error'",
    "Processor Presence detected",
    "Processor disabled",
    "Terminator Presence Detected",
    "Processor Automatically Throttled (processor throttling triggered by a hardware-based mechanism operating independent from system software, such as automatic thermal throttling or throttling to limit power consumption.)",
    NULL
  };
static int ipmi_sensor_type_code_07_desc_max = 0x0A;

static char * ipmi_sensor_type_code_08_desc[] =
  {
    "Presence detected",
    "Power Supply Failure detected",
    "Predictive Failure",
    "Power Supply input lost (AC/DC)",
    "Power Supply input lost or out-of-range",
    "Power Supply input out-of-range, but present",
    "Configuration error",
    NULL
  };
static int ipmi_sensor_type_code_08_desc_max = 0x06;

static char * ipmi_sensor_type_code_09_desc[] =
  {
    "Power Off/Power Down",
    "Power Cycle",
    "240VA Power Down",
    "Interlock Power Down",
    "AC lost",
    "Soft Power Control Failure (unit did not respond to request to turn on)",
    "Power Unit Failure detected",
    "Predictive Failure",
    NULL
  };
static int ipmi_sensor_type_code_09_desc_max = 0x07;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_0C_desc[] =
  {
    "Correctable ECC/other correctable memory error",
    "Uncorrectable ECC/other uncorrectable memory error",
    "Parity",
    "Memory Scrub Failed (stuck bit)",
    "Memory Device Disabled",
    "Correctable ECC/other correctable memory error logging limit reached",
    "Presence detected",
    "Configuration error",
    "Spare",
    "Memory Automatically Throttled",
    "Critical Overtemperature.  Memory device has entered a critical overtemperature state, exceeding specified operating conditions.  Memory devices in this state may produce errors or become inaccessible",
    NULL
  };
static int ipmi_sensor_type_code_0C_desc_max = 0x0A;

/* achu: defined in errata */
static char * ipmi_sensor_type_code_0D_desc[] =
  {
    "Drive Presence",
    "Drive Fault",
    "Predictive Failure",
    "Hot Spare",
    "Consistency Check / Parity Check in progress",
    "In Critical Array",
    "In Failed Array",
    "Rebuild/Remap in progress",
    "Rebuild/Remap Aborted (was not completed normally)",
    NULL
  };
static int ipmi_sensor_type_code_0D_desc_max = 0x08;

static char * ipmi_sensor_type_code_0F_desc[] =
  {
    "System Firmware Error (POST Error)",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
static int ipmi_sensor_type_code_0F_desc_max = 0x02;

static char * ipmi_sensor_type_code_10_desc[] =
  {
    "Correctable Memory Error Logging Disabled",
    "Event `Type' Logging Disabled",
    "Log Area Reset/Cleared",
    "All Event Logging Disabled",
    "SEL Full",
    "SEL Almost Full",
    NULL
  };
static int ipmi_sensor_type_code_10_desc_max = 0x05;

static char * ipmi_sensor_type_code_11_desc[] =
  {
    "BIOS Watchdog Reset",
    "OS Watchdog Reset",
    "OS Watchdog Shut Down",
    "OS Watchdog Power Down",
    "OS Watchdog Power Cycle",
    "OS Watchdog NMI/Diagnostic Interrupt",
    "OS Watchdog Expired, status only",
    "OS Watchdog pre-timeout Interrupt, non-NMI",
    NULL
  };
static int ipmi_sensor_type_code_11_desc_max = 0x07;

static char * ipmi_sensor_type_code_12_desc[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
static int ipmi_sensor_type_code_12_desc_max = 0x05;

static char * ipmi_sensor_type_code_13_desc[] =
  {
    "Front Panel NMI/Diagnostic Interrupt",
    "Bus Timeout",
    "I/O channel check NMI",
    "Software NMI",
    "PCI PERR",
    "PCI SERR",
    "EISA Fail Safe Timeout",
    "Bus Correctable Error",
    "Bus Uncorrectable Error",
    "Fatal NMI (port 61h, bit 7)",
    "Bus Fatal Error",
    "Bus Degraded (bus operating in a degraded performance state)",
    NULL
  };
static int ipmi_sensor_type_code_13_desc_max = 0x0B;

static char * ipmi_sensor_type_code_14_desc[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open (Switch indicating FRU latch is in `unlatched' position and FRU is mechanically removable)",
    "FRU service request button (pressed, service, e.g. removal/replacement, requested)",
    NULL
  };
static int ipmi_sensor_type_code_14_desc_max = 0x04;

static char * ipmi_sensor_type_code_19_desc[] =
  {
    "Soft Power Control Failure (chipset did not respond to BMC request to change system power state)", 
    NULL
  };
static int ipmi_sensor_type_code_19_desc_max = 0x00;

static char * ipmi_sensor_type_code_1B_desc[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected / Incorrect interconnection",
    NULL
  };
static int ipmi_sensor_type_code_1B_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_1D_desc[] =
  {
    "Initiated by power up",
    "Initiated by hard reset",
    "Initiated by warm reset",
    "User requested PXE boot",
    "Automatic boot to diagnostic",
    "OS / run-time software initiated hard reset",
    "OS / run-time software initiated warm reset",
    "System Restart",
    NULL
  };
static int ipmi_sensor_type_code_1D_desc_max = 0x07;

static char * ipmi_sensor_type_code_1E_desc[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
static int ipmi_sensor_type_code_1E_desc_max = 0x04;

static char * ipmi_sensor_type_code_1F_desc[] =
  {
    "A: boot completed",
    "C: boot completed",
    "PXE boot completed",
    "Diagnostic boot completed",
    "CD-ROM boot completed",
    "ROM boot completed",
    "boot completed - boot device not specified",
    NULL
  };
static int ipmi_sensor_type_code_1F_desc_max = 0x06;

/* achu: modified per errata */
static char * ipmi_sensor_type_code_20_desc[] =
  {
    "Critical stop during OS load / initialization.  Unexpected error during system startup.  Stopped waiting for input or power cycle/reset.",
    "Run-time Critical Stop (a.k.a. 'core dump', 'blue screen')",
    "OS Graceful Stop (system powered up, but normal OS operation has shut down and system is awaiting reset pushbutton, powercycle or other external input)",
    "OS Graceful Shutdown (system graceful power down by OS)",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding.  Graceful shutdown request to agent via BMC did not occur due to missing or malfunctioning local agent.",
    NULL
  };
static int ipmi_sensor_type_code_20_desc_max = 0x05;

static char * ipmi_sensor_type_code_21_desc[] =
  {
    "Fault Status asserted",
    "Identify Status asserted",
    "Slot/Connector Device installed/attached",
    "Slot/Connector Ready for Device Installation",
    "Slot/Connector Ready for Device Removal",
    "Slot Power is Off",
    "Slot/Connector Device Removal Request",
    "Interlock asserted",
    "Slot is Disabled",
    "Slot holds spare device",
    NULL
  };
static int ipmi_sensor_type_code_21_desc_max = 0x09;

static char * ipmi_sensor_type_code_22_desc[] =
  {
    "S0/G0 \"working\"",
    "S1 \"sleeping with system h/w & processor context maintained\"",
    "S2 \"sleeping, processor context lost\"",
    "S3 \"sleeping, processor & h/w context lost, memory retained.\"",
    "S4 \"non-volatile sleep/suspend-to disk\"",
    "S5/G2 \"soft-off\"",
    "S4/S5 soft-off, particular S4/S5 state cannot be determined",
    "G3/Mechanical Off",
    "Sleeping in an S1, S2, or S3 states (used when particular S1, S2, S3 state cannot be determined)",
    "G1 sleeping (S1-S4 state cannot be determined)",
    "S5 entered by override",
    "Legacy ON state",
    "Legacy OFF state",
    "Unspecified",
    "Unknown",
    NULL
  };
static int ipmi_sensor_type_code_22_desc_max = 0x0E;

static char * ipmi_sensor_type_code_23_desc[] =
  {
    "Timer expired, status only (no action, no interrupt)",
    "Hard Reset",
    "Power Down",
    "Power Cycle",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Timer interrupt",
    NULL
  };
static int ipmi_sensor_type_code_23_desc_max = 0x08;

static char * ipmi_sensor_type_code_24_desc[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated, formatted per IPMI PET specification",
    "platform generated SNMP trap, OEM format",
    NULL
  };
static int ipmi_sensor_type_code_24_desc_max = 0x03;

static char * ipmi_sensor_type_code_25_desc[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
static int ipmi_sensor_type_code_25_desc_max = 0x02;

static char * ipmi_sensor_type_code_27_desc[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
static int ipmi_sensor_type_code_27_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_28_desc[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
static int ipmi_sensor_type_code_28_desc_max = 0x05;

static char * ipmi_sensor_type_code_29_desc[] =
  {
    "battery low (predictive failure)",
    "battery failed",
    "battery presence detected",
    NULL
  };
static int ipmi_sensor_type_code_29_desc_max = 0x02;

static char * ipmi_sensor_type_code_2A_desc[] =
  {
    "Session Activated",
    "Session Deactivated",
    NULL
  };
static int ipmi_sensor_type_code_2A_desc_max = 0x01;

static char * ipmi_sensor_type_code_2B_desc[] =
  {
    "Hardware change detected with associated Entity",
    "Firmware or software change detected with associated Entity",
    "Hardware incompatibility detected with associated Entity",
    "Firmware or software incompatibility detected with associated Entity",
    "Entity is of an invalid or unsupported hardware version",
    "Entity contains an invalid or unsupported firmware or software version",
    "Hardware Change detected with associated Entity was successful",
    "Software or F/W Change detected with associated Entity was successful",
    NULL
  };
static int ipmi_sensor_type_code_2B_desc_max = 0x07;

static char * ipmi_sensor_type_code_2C_desc[] =
  {
    "FRU Not Installed",
    "FRU Inactive (in standby or `hot spare' state)",
    "FRU Activation Requested",
    "FRU Activation In Progress",
    "FRU Active",
    "FRU Deactivation Requested",
    "FRU Deactivation In Progress",
    "FRU Communication Lost",
    NULL
  };
static int ipmi_sensor_type_code_2C_desc_max = 0x07;

/*************************************************
 * Generic Event Reading Strings (SHORT STRINGS) *
 *************************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

static char * ipmi_generic_event_reading_type_code_01_short_desc[] =
  {
    "Lower Non-critical - going low",
    "Lower Non-critical - going high",
    "Lower Critical - going low",
    "Lower Critical - going high",
    "Lower Non-recoverable - going low",
    "Lower Non-recoverable - going high",
    "Upper Non-critical - going low",
    "Upper Non-critical - going high",
    "Upper Critical - going low",
    "Upper Critical - going high",
    "Upper Non-recoverable - going low",
    "Upper Non-recoverable - going high",
    NULL
  };
static int ipmi_generic_event_reading_type_code_01_short_desc_max = 0x0B;

static char * ipmi_generic_event_reading_type_code_02_short_desc[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
static int ipmi_generic_event_reading_type_code_02_short_desc_max = 0x02;

static char * ipmi_generic_event_reading_type_code_03_short_desc[] =  {
  "State Deasserted",
  "State Asserted",
  NULL
};
static int ipmi_generic_event_reading_type_code_03_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_04_short_desc[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
static int ipmi_generic_event_reading_type_code_04_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_05_short_desc[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
static int ipmi_generic_event_reading_type_code_05_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_06_short_desc[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
static int ipmi_generic_event_reading_type_code_06_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_07_short_desc[] =
  {
    "transition to OK",
    "transition to Non-Critical from OK",
    "transition to Critical from less severe",
    "transition to Non-recoverable from less severe",
    "transition to Non-Critical from more severe",
    "transition to Critical from Non-recoverable",
    "transition to Non-recoverable",
    "Monitor",
    "Informational",
    NULL
  };
static int ipmi_generic_event_reading_type_code_07_short_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_08_short_desc[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
static int ipmi_generic_event_reading_type_code_08_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_09_short_desc[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
static int ipmi_generic_event_reading_type_code_09_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_0A_short_desc[] =
  {
    "transition to Running",
    "transition to In Test",
    "transition to Power Off",
    "transition to On Line",
    "transition to Off Line",
    "transition to Off Duty",
    "transition to Degraded",
    "transition to Power Save",
    "Install Error",
    NULL
  };
static int ipmi_generic_event_reading_type_code_0A_short_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_0B_short_desc[] =
  {
    "Fully Redundant",
    "Redundancy Lost",
    "Redundancy Degraded",
    "Non-redundant:Sufficient Resources from Redundant",
    "Non-redundant:Sufficient Resources from Insufficient Resources",
    "Non-redundant:Insufficient Resources",
    "Redundancy Degraded from Fully Redundant",
    "Redundancy Degraded from Non-redundant",
    NULL
  };
static int ipmi_generic_event_reading_type_code_0B_short_desc_max = 0x07;

static char * ipmi_generic_event_reading_type_code_0C_short_desc[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
static int ipmi_generic_event_reading_type_code_0C_short_desc_max = 0x03;

/*************************************** 
 * Sensor Type Strings (SHORT STRINGS) *
 ***************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

static char * ipmi_sensor_type_code_01_short_desc[] =
  {
    "Temperature",
    NULL
  };
static int ipmi_sensor_type_code_01_short_desc_max = 0x00;

static char * ipmi_sensor_type_code_02_short_desc[] =
  {
    "Voltage",
    NULL
  };
static int ipmi_sensor_type_code_02_short_desc_max = 0x00;

static char * ipmi_sensor_type_code_03_short_desc[] =
  {
    "Current",
    NULL
  };
static int ipmi_sensor_type_code_03_short_desc_max = 0x00;

static char * ipmi_sensor_type_code_04_short_desc[] =
  {
    "Fan",
    NULL
  };
static int ipmi_sensor_type_code_04_short_desc_max = 0x00;

/* achu: 'undock' removed as noted in errata */
static char * ipmi_sensor_type_code_05_short_desc[] =
  {
    "General Chassis Intrusion",
    "Drive Bay intrusion",
    "I/O Card area intrusion",
    "Processor area intrusion",
    "LAN Leash Lost",
    "Unauthorized dock",
    "FAN area intrusion",
    NULL
  };
static int ipmi_sensor_type_code_05_short_desc_max = 0x06;

static char * ipmi_sensor_type_code_06_short_desc[] =
  {
    "Secure Mode Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
static int ipmi_sensor_type_code_06_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_07_short_desc[] =
  {
    "IERR",
    "Thermal Trip",
    "FRB1/BIST failure",
    "FRB2/Hang in POST failure",
    "FRB3/Processor Startup/Initialization failure",
    "Configuration Error",
    "SM BIOS `Uncorrectable CPU-complex Error'",
    "Processor Presence detected",
    "Processor disabled",
    "Terminator Presence Detected",
    "Processor Automatically Throttled",
    NULL
  };
static int ipmi_sensor_type_code_07_short_desc_max = 0x0A;

static char * ipmi_sensor_type_code_08_short_desc[] =
  {
    "Presence detected",
    "Power Supply Failure detected",
    "Predictive Failure",
    "Power Supply input lost (AC/DC)",
    "Power Supply input lost or out-of-range",
    "Power Supply input out-of-range, but present",
    "Configuration error",
    NULL
  };
static int ipmi_sensor_type_code_08_short_desc_max = 0x06;

static char * ipmi_sensor_type_code_09_short_desc[] =
  {
    "Power Off/Power Down",
    "Power Cycle",
    "240VA Power Down",
    "Interlock Power Down",
    "AC lost",
    "Soft Power Control Failure",
    "Power Unit Failure detected",
    "Predictive Failure",
    NULL
  };
static int ipmi_sensor_type_code_09_short_desc_max = 0x07;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_0C_short_desc[] =
  {
    "Correctable memory error",
    "Uncorrectable memory error",
    "Parity",
    "Memory Scrub Failed",
    "Memory Device Disabled",
    "Correctable memory error logging limit reached",
    "Presence detected",
    "Configuration error",
    "Spare",
    "Memory Automatically Throttled",
    "Critical Overtemperature",
    NULL
  };
static int ipmi_sensor_type_code_0C_short_desc_max = 0x0A;

/* achu: defined in errata */
static char * ipmi_sensor_type_code_0D_short_desc[] =
  {
    "Drive Presence",
    "Drive Fault",
    "Predictive Failure",
    "Hot Spare",
    "Consistency Check / Parity Check in progress",
    "In Critical Array",
    "In Failed Array",
    "Rebuild/Remap in progress",
    "Rebuild/Remap Aborted",
    NULL
  };
static int ipmi_sensor_type_code_0D_short_desc_max = 0x08;

static char * ipmi_sensor_type_code_0F_short_desc[] =
  {
    "System Firmware Error",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
static int ipmi_sensor_type_code_0F_short_desc_max = 0x02;

static char * ipmi_sensor_type_code_10_short_desc[] =
  {
    "Correctable Memory Error Logging Disabled",
    "Event Type Logging Disabled",
    "Log Area Reset/Cleared",
    "All Event Logging Disabled",
    "SEL Full",
    "SEL Almost Full",
    NULL
  };
static int ipmi_sensor_type_code_10_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_11_short_desc[] =
  {
    "BIOS Watchdog Reset",
    "OS Watchdog Reset",
    "OS Watchdog Shut Down",
    "OS Watchdog Power Down",
    "OS Watchdog Power Cycle",
    "OS Watchdog NMI/Diagnostic Interrupt",
    "OS Watchdog Expired, status only",
    "OS Watchdog pre-timeout Interrupt, non-NMI",
    NULL
  };
static int ipmi_sensor_type_code_11_short_desc_max = 0x07;

static char * ipmi_sensor_type_code_12_short_desc[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
static int ipmi_sensor_type_code_12_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_13_short_desc[] =
  {
    "Front Panel NMI/Diagnostic Interrupt",
    "Bus Timeout",
    "I/O channel check NMI",
    "Software NMI",
    "PCI PERR",
    "PCI SERR",
    "EISA Fail Safe Timeout",
    "Bus Correctable Error",
    "Bus Uncorrectable Error",
    "Fatal NMI",
    "Bus Fatal Error",
    "Bus Degraded",
    NULL
  };
static int ipmi_sensor_type_code_13_short_desc_max = 0x0B;

static char * ipmi_sensor_type_code_14_short_desc[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open",
    "FRU service request button",
    NULL
  };
static int ipmi_sensor_type_code_14_short_desc_max = 0x04;

static char * ipmi_sensor_type_code_19_short_desc[] =
  {
    "Soft Power Control Failure", 
    NULL
  };
static int ipmi_sensor_type_code_19_short_desc_max = 0x00;

static char * ipmi_sensor_type_code_1B_short_desc[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected",
    NULL
  };
static int ipmi_sensor_type_code_1B_short_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_1D_short_desc[] =
  {
    "Initiated by power up",
    "Initiated by hard reset",
    "Initiated by warm reset",
    "User requested PXE boot",
    "Automatic boot to diagnostic",
    "OS / run-time software initiated hard reset",
    "OS / run-time software initiated warm reset",
    "System Restart",
    NULL
  };
static int ipmi_sensor_type_code_1D_short_desc_max = 0x07;

static char * ipmi_sensor_type_code_1E_short_desc[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
static int ipmi_sensor_type_code_1E_short_desc_max = 0x04;

static char * ipmi_sensor_type_code_1F_short_desc[] =
  {
    "A: boot completed",
    "C: boot completed",
    "PXE boot completed",
    "Diagnostic boot completed",
    "CD-ROM boot completed",
    "ROM boot completed",
    "boot completed - boot device not specified",
    NULL
  };
static int ipmi_sensor_type_code_1F_short_desc_max = 0x06;

/* achu: modified per errata */
static char * ipmi_sensor_type_code_20_short_desc[] =
  {
    "Critical stop during OS load",
    "Run-time Critical Stop",
    "OS Graceful Stop",
    "OS Graceful Shutdown",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding",
    NULL
  };
static int ipmi_sensor_type_code_20_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_21_short_desc[] =
  {
    "Fault Status asserted",
    "Identify Status asserted",
    "Slot/Connector Device installed/attached",
    "Slot/Connector Ready for Device Installation",
    "Slot/Connector Ready for Device Removal",
    "Slot Power is Off",
    "Slot/Connector Device Removal Request",
    "Interlock asserted",
    "Slot is Disabled",
    "Slot holds spare device",
    NULL
  };
static int ipmi_sensor_type_code_21_short_desc_max = 0x09;

static char * ipmi_sensor_type_code_22_short_desc[] =
  {
    "S0/G0",
    "S1",
    "S2",
    "S3",
    "S4",
    "S5/G2",
    "S4/S5 soft-off",
    "G3/Mechanical Off",
    "Sleeping in an S1, S2, or S3 states",
    "G1 sleeping",
    "S5 entered by override",
    "Legacy ON state",
    "Legacy OFF state",
    "Unspecified",
    "Unknown",
    NULL
  };
static int ipmi_sensor_type_code_22_short_desc_max = 0x0E;

static char * ipmi_sensor_type_code_23_short_desc[] =
  {
    "Timer expired, status only",
    "Hard Reset",
    "Power Down",
    "Power Cycle",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Timer interrupt",
    NULL
  };
static int ipmi_sensor_type_code_23_short_desc_max = 0x08;

static char * ipmi_sensor_type_code_24_short_desc[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated",
    "platform generated SNMP trap, OEM format",
    NULL
  };
static int ipmi_sensor_type_code_24_short_desc_max = 0x03;

static char * ipmi_sensor_type_code_25_short_desc[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
static int ipmi_sensor_type_code_25_short_desc_max = 0x02;

static char * ipmi_sensor_type_code_27_short_desc[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
static int ipmi_sensor_type_code_27_short_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_28_short_desc[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
static int ipmi_sensor_type_code_28_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_29_short_desc[] =
  {
    "battery low",
    "battery failed",
    "battery presence detected",
    NULL
  };
static int ipmi_sensor_type_code_29_short_desc_max = 0x02;

static char * ipmi_sensor_type_code_2A_short_desc[] =
  {
    "Session Activated",
    "Session Deactivated",
    NULL
  };
static int ipmi_sensor_type_code_2A_short_desc_max = 0x01;

static char * ipmi_sensor_type_code_2B_short_desc[] =
  {
    "Hardware change detected with associated Entity",
    "Firmware or software change detected with associated Entity",
    "Hardware incompatibility detected with associated Entity",
    "Firmware or software incompatibility detected with associated Entity",
    "Entity is of an invalid or unsupported hardware version",
    "Entity contains an invalid or unsupported firmware or software version",
    "Hardware Change detected with associated Entity was successful",
    "Software or F/W Change detected with associated Entity was successful",
    NULL
  };
static int ipmi_sensor_type_code_2B_short_desc_max = 0x07;

static char * ipmi_sensor_type_code_2C_short_desc[] =
  {
    "FRU Not Installed",
    "FRU Inactive",
    "FRU Activation Requested",
    "FRU Activation In Progress",
    "FRU Active",
    "FRU Deactivation Requested",
    "FRU Deactivation In Progress",
    "FRU Communication Lost",
    NULL
  };
static int ipmi_sensor_type_code_2C_short_desc_max = 0x07;

/*******************************************************
 * Sensor Type Strings for Event Data 2 (FULL STRINGS) *
 *******************************************************/

static char * ipmi_sensor_type_code_0F_event_data2_offset_00_desc[] =
  {
    "Unspecified",
    "No system memory is physically installed in the system",
    "No usable system memory, all installed memory has experienced an unrecoverable failure",
    "Unrecoverable hard-disk/ATAPI/IDE device failure",
    "Unrecoverable system-board failure",
    "Unrecoverable diskette subsystem failure",
    "Unrecoverable hard-disk controller failure",
    "Unrecoverable PS/2 or USB keyboard failure",
    "Removable boot media not found",
    "Unrecoverable video controller failure",
    "No video device detected",
    "Firmware (BIOS) ROM corruption detected",
    "CPU voltage mismatch (processors that share same supply have mismatched voltage requirements)",
    "CPU speed matching failure",
    NULL
  };
static int ipmi_sensor_type_code_0F_event_data2_offset_00_desc_max = 0x0D;

static char * ipmi_sensor_type_code_0F_event_data2_offset_01_desc[] =
  {
    "Unspecified",
    "Memory initialization",
    "Hard-disk initialization",
    "Secondary processor(s) initialization",
    "User authentication",
    "User-initiated system setup",
    "USB resource configuration",
    "PCI resource configuration",
    "Option ROM initialization",
    "Video initialization",
    "Cache initialization",
    "SM Bus initialization",
    "Keyboard controller initialization",
    "Embedded controller/management controller initialization",
    "Docking station attachment",
    "Enabling docking station",
    "Docking station ejection",
    "Disabling docking station",
    "Calling operating system wake-up vector",
    "Starting operating system boot process, e.g. calling Int 19h",
    "Baseboard or motherboard initialization",
    "reserved",
    "Floppy initialization",
    "Keyboard test",
    "Pointing device test",
    "Primary processor initialization",
    NULL
  };
static int ipmi_sensor_type_code_0F_event_data2_offset_01_desc_max = 0x19;

static char * ipmi_sensor_type_code_0F_event_data2_offset_02_desc[] =
  {
    "Unspecified",
    "Memory initialization",
    "Hard-disk initialization",
    "Secondary processor(s) initialization",
    "User authentication",
    "User-initiated system setup",
    "USB resource configuration",
    "PCI resource configuration",
    "Option ROM initialization",
    "Video initialization",
    "Cache initialization",
    "SM Bus initialization",
    "Keyboard controller initialization",
    "Embedded controller/management controller initialization",
    "Docking station attachment",
    "Enabling docking station",
    "Docking station ejection",
    "Disabling docking station",
    "Calling operating system wake-up vector",
    "Starting operating system boot process, e.g. calling Int 19h",
    "Baseboard or motherboard initialization",
    "reserved",
    "Floppy initialization",
    "Keyboard test",
    "Pointing device test",
    "Primary processor initialization",
    NULL
  };
static int ipmi_sensor_type_code_0F_event_data2_offset_02_desc_max = 0x19;

static char * ipmi_sensor_type_code_12_event_data2_offset_03_log_entry_action_desc[] =
  {
    "Log entry action = entry added",
    "Log entry action = entry added because event did not be map to standard IPMI event",
    "Log entry action = entry added along with one or more corresponding SEL entries",
    "Log entry action = log cleared",
    "Log entry action = log disabled",
    "Log entry action = log enabled",
    NULL
  };
static int ipmi_sensor_type_code_12_event_data2_offset_03_log_entry_action_desc_max = 0x05;

static char * ipmi_sensor_type_code_12_event_data2_offset_03_log_type_desc[] =
  {
    "Log Type = MCA log",
    "Log Type = OEM1",
    "Log Type = OEM2",
    NULL
  };
static int ipmi_sensor_type_code_12_event_data2_offset_03_log_type_desc_max = 0x02;

static char * ipmi_sensor_type_code_12_event_data2_offset_04_pef_action_desc[] =
  {
    "Alert",
    "power off",
    "reset",
    "power cycle",
    "OEM action",
    "Diagnostic Interrupt (NMI)",
    NULL,
  };
#if 0
/* Not used */
static int ipmi_sensor_type_code_12_event_data2_offset_04_pef_action_desc_max = 0x05;
#endif

static char * ipmi_sensor_type_code_12_event_data2_offset_05_first_second_desc[] =
  {
    "event is first of pair",
    "event is second of pair",
    NULL,
  };
static int ipmi_sensor_type_code_12_event_data2_offset_05_first_second_desc_max = 0x01;

static char * ipmi_sensor_type_code_12_event_data2_offset_05_timestamp_clock_type_desc[] =
  {
    "SEL Timestamp Clock updated (Also used when both SEL and SDR Timestamp clocks are linked together)",
    "SDR Timestamp Clock updated",
    NULL,
  };
static int ipmi_sensor_type_code_12_event_data2_offset_05_timestamp_clock_type_desc_max = 0x01;

static char * ipmi_sensor_type_code_19_event_data2_offset_00_desc[] =
  {
    "Requested power state = S0/G0 \"working\"",
    "Requested power state = S1 \"sleeping with system h/w & processor context maintained\"",
    "Requested power state = S2 \"sleeping, processor context lost\"",
    "Requested power state = S3 \"sleeping, processor & h/w context lost, memory retained.\"",
    "Requested power state = S4 \"non-volatile sleep/suspend-to disk\"",
    "Requested power state = S5/G2 \"soft-off\"",
    "Requested power state = S4/S5 soft-off, particular S4/S5 state cannot be determined",
    "Requested power state = G3/Mechanical Off",
    "Requested power state = Sleeping in an S1, S2, or S3 states (used when particular S1, S2, S3 state cannot be determined)",
    "Requested power state = G1 sleeping (S1-S4 state cannot be determined)",
    "Requested power state = S5 entered by override",
    "Requested power state = Legacy ON state",
    "Requested power state = Legacy OFF state",
    NULL,
  };
static int ipmi_sensor_type_code_19_event_data2_offset_00_desc_max = 0x0C;

static char * ipmi_sensor_type_code_1D_event_data2_offset_07_restart_cause_desc[] =
  {
    "unknown",
    "Chassis Control command",
    "reset via pushbutton",
    "power-up via power pushbutton",
    "Watchdog expiration",
    "OEM",
    "automatic power-up on AC being applied due to 'always restore' power restore policy",
    "automatic power-up on AC being applied due to 'restore previous power state' power restore policy",
    "reset via PEF",
    "power-cycle via PEF",
    "soft reset (e.g. CTRL-ALT-DEL)",
    "power-up via RTC (system real time clock) wakeup",
    NULL
  };
static int ipmi_sensor_type_code_1D_event_data2_offset_07_restart_cause_desc_max = 0x0B;

static char * ipmi_sensor_type_code_21_event_data2_offset_09_slot_connector_type_desc[] =
  {
    "Slot/Connector Type = PCI",
    "Slot/Connector Type = Drive Array",
    "Slot/Connector Type = External Peripheral Connector",
    "Slot/Connector Type = Docking",
    "Slot/Connector Type = other standard internal expansion slot",
    "Slot/Connector Type = slot associated with entity specified by Entity ID for sensor",
    "Slot/Connector Type = AdvancedTCA",
    "Slot/Connector Type = DIMM/memory device",
    "Slot/Connector Type = FAN",
    NULL
  };
static int ipmi_sensor_type_code_21_event_data2_offset_09_slot_connector_type_desc_max = 0x08;

static char * ipmi_sensor_type_code_23_event_data2_offset_08_interrupt_type_desc[] =
  {
    "Interrupt type = none",
    "Interrupt type = SMI",
    "Interrupt type = NMI",
    "Interrupt type = Messaging Interrupt",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Interrupt type = unspecified",
    NULL,
  };
static int ipmi_sensor_type_code_23_event_data2_offset_08_interrupt_type_desc_max = 0x0F;

static char * ipmi_sensor_type_code_23_event_data2_offset_08_timer_use_at_expiration_desc[] =
  {
    "reserved",
    "Timer use at expiration = BIOS FRB2",
    "Timer use at expiration = BIOS/POST",
    "Timer use at expiration = OS Load",
    "Timer use at expiration = SMS/OS",
    "Timer use at expiration = OEM",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Timer use at expiration = unspecified",
    NULL
  };
static int ipmi_sensor_type_code_23_event_data2_offset_08_timer_use_at_expiration_desc_max = 0x0F;

static char * ipmi_sensor_type_code_28_event_data2_offset_05_logical_fru_device_desc[] = 
  {
    "device is not a logical FRU Device",
    "device is logical FRU Device (accessed via FRU commands to mgmt. controller",
    NULL
  };
static int ipmi_sensor_type_code_28_event_data2_offset_05_logical_fru_device_desc_max = 0x01;

static char * ipmi_sensor_type_code_2B_event_data2_offset_07_version_change_type_desc[] =
  {
    "Version change type = unspecified",
    "Version change type = management controller device ID (change in one or more fields from `Get Device ID')",
    "Version change type = management controller firmware revision",
    "Version change type = management controller device revision",
    "Version change type = management controller manufacturer ID",
    "Version change type = management controller IPMI version",
    "Version change type = management controller auxiliary firmware ID",
    "Version change type = management controller firmware boot block",
    "Version change type = other management controller firmware",
    "Version change type = system firmware (EFI/BIOS) change",
    "Version change type = SMBIOS change",
    "Version change type = operating system change",
    "Version change type = operating system loader change",
    "Version change type = service or diagnostic partition change",
    "Version change type = management software agent change",
    "Version change type = management software application change",
    "Version change type = management software middleware change",
    "Version change type = programmable hardware change (e.g. FPGA)",
    "Version change type = board/FRU module change (change of a module plugged into associated entity)",
    "Version change type = board/FRU component change (addition or removal of a replaceable component on the board/FRU that is not tracked as a FRU)",
    "Version change type = board/FRU replaced with equivalent version",
    "Version change type = board/FRU replaced with newer version",
    "Version change type = board/FRU replaced with older version",
    "Version change type = board/FRU hardware configuration change (e.g. strap, jumper, cable change, etc.)",
    NULL
  };
static int ipmi_sensor_type_code_2B_event_data2_offset_07_version_change_type_desc_max = 0x17;

static char * ipmi_sensor_type_code_2C_event_data2_offset_07_cause_of_state_change_desc[] =
  {
    "Cause of state change = Normal State Change",
    "Cause of state change = Change Commanded by software external to FRU",
    "Cause of state change = State Change due to operator changing a Handle latch",
    "Cause of state change = State Change due to operator pressing the hotswap push button",
    "Cause of state change = State Change due to FRU programmatic action",
    "Cause of state change = Communication Lost",
    "Cause of state change = Communication Lost due to local failure",
    "Cause of state change = State Change due to unexpected extraction",
    "Cause of state change = State Change due to operator intervention/update",
    "Cause of state change = Unable to compute IPMB address",
    "Cause of state change = Unexpected Deactivation",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Cause of state change = State Change, Cause Unknown",
    NULL
  };
static int ipmi_sensor_type_code_2C_event_data2_offset_07_cause_of_state_change_desc_max = 0x0F;

/*******************************************************
 * Sensor Type Strings for Event Data 3 (FULL STRINGS) *
 *******************************************************/

static char * ipmi_sensor_type_code_08_event_data3_offset_06_error_type_desc[] = 
  {
    "Vendor mismatch",
    "Revision mismatch",
    "Processor missing or unexpected/unsupported condition",
    NULL
  };
static int ipmi_sensor_type_code_08_event_data3_offset_06_error_type_desc_max = 0x02;

static char * ipmi_sensor_type_code_10_event_data3_offset_01_assertion_event_desc[] =
  {
    "deassertion event",
    "assertion event",
    NULL
  };
static int ipmi_sensor_type_code_10_event_data3_offset_01_assertion_event_desc_max = 0x01;

static char * ipmi_sensor_type_code_10_event_data3_offset_01_logging_disabled_all_events_desc[] =
  {
    "",
    "logging has been disabled for all events of given type",
    NULL
  };
static int ipmi_sensor_type_code_10_event_data3_offset_01_logging_disabled_all_events_desc_max = 0x01;

static char * ipmi_sensor_type_code_19_event_data3_offset_00_desc[] =
  {
    "Power state at time of request = S0/G0 \"working\"",
    "Power state at time of request = S1 \"sleeping with system h/w & processor context maintained\"",
    "Power state at time of request = S2 \"sleeping, processor context lost\"",
    "Power state at time of request = S3 \"sleeping, processor & h/w context lost, memory retained.\"",
    "Power state at time of request = S4 \"non-volatile sleep/suspend-to disk\"",
    "Power state at time of request = S5/G2 \"soft-off\"",
    "Power state at time of request = S4/S5 soft-off, particular S4/S5 state cannot be determined",
    "Power state at time of request = G3/Mechanical Off",
    "Power state at time of request = Sleeping in an S1, S2, or S3 states (used when particular S1, S2, S3 state cannot be determined)",
    "Power state at time of request = G1 sleeping (S1-S4 state cannot be determined)",
    "Power state at time of request = S5 entered by override",
    "Power state at time of request = Legacy ON state",
    "Power state at time of request = Legacy OFF state",
    "Power state at time of request = unknown",
    NULL
  };
static int ipmi_sensor_type_code_19_event_data3_offset_00_desc_max = 0x0D;

static char * ipmi_sensor_type_code_2A_event_data3_offset_01_deactivation_cause_desc[] =
  {
    "Session deactivatation cause unspecified. This value is also used for Session Activated events",
    "Session deactivated by Close Session command",
    "Session deactivated by timeout",
    "Session deactivated by configuration change",
    NULL
  };
static int ipmi_sensor_type_code_2A_event_data3_offset_01_deactivation_cause_desc_max = 0x03;

static int
_snprintf(char *buf, unsigned int buflen, char *fmt, ...)
{
  int rv;
  va_list ap;

  assert(buf && buflen && fmt);

  va_start(ap, fmt);
  rv = vsnprintf (buf, buflen, fmt, ap);
  va_end(ap);

  /* -1 to account for '\0' */
  ERR_ENOSPC(!(rv >= (buflen - 1)));
  return (0);
}

static int
get_05_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x04)
    return _snprintf(buf, buflen, "Network controller #%d", event_data2);
  
  ERR_EINVAL (0);
}

static int
get_0F_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x00 && event_data2 <= ipmi_sensor_type_code_0F_event_data2_offset_00_desc_max)
    return _snprintf (buf, buflen, ipmi_sensor_type_code_0F_event_data2_offset_00_desc[event_data2]);
  if (offset == 0x01 && event_data2 <= ipmi_sensor_type_code_0F_event_data2_offset_01_desc_max)
    return _snprintf (buf, buflen, ipmi_sensor_type_code_0F_event_data2_offset_01_desc[event_data2]);
  if (offset == 0x02 && event_data2 <= ipmi_sensor_type_code_0F_event_data2_offset_02_desc_max)
    return _snprintf (buf, buflen, ipmi_sensor_type_code_0F_event_data2_offset_02_desc[event_data2]);
  
  ERR_EINVAL (0);
}

static int
get_10_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x00)
    return _snprintf(buf, buflen, "Memory module/device #%d", event_data2);
  if (offset == 0x01)
    return _snprintf(buf, buflen, "Event/Reading Type Code #%d", event_data2);
  
  ERR_EINVAL (0);
}

static int
_get_12_event_data2_message_offset_03(int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 = 
    {
      {4, "log_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {4, "log_entry_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {0, "", 0}
    };
  uint64_t val;
  uint8_t log_type;
  uint8_t log_entry_action;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
  
  FIID_OBJ_GET_CLEANUP (obj, "log_type", &val);
  log_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "log_entry_action", &val);
  log_entry_action = val;
          
  if (log_type <= ipmi_sensor_type_code_12_event_data2_offset_03_log_entry_action_desc_max)
    str1 = ipmi_sensor_type_code_12_event_data2_offset_03_log_entry_action_desc[log_type];
  
  if (log_entry_action <= ipmi_sensor_type_code_12_event_data2_offset_03_log_type_desc_max)
    str2 = ipmi_sensor_type_code_12_event_data2_offset_03_log_type_desc[log_entry_action];
  
  if (str1 || str2)
    rv = _snprintf (buf, buflen, "%s%s%s", 
		    (str1 ? str1 : ""), 
		    ((str1 && str2) ? "; " : ""), 
		    (str2 ? str2 : ""));
	
 cleanup:
  FIID_OBJ_DESTROY(obj);
  return rv;
}

static int
_strcat12(char *buf, unsigned int buflen, uint8_t flag, int str_len, int index)
{
  if (flag)
    {
      str_len += strlen(ipmi_sensor_type_code_12_event_data2_offset_04_pef_action_desc[index]);
      ERR_ENOSPC (!(str_len < buflen));

      if (str_len)
	strcat(buf, ipmi_sensor_type_code_12_event_data2_offset_04_pef_action_desc[index]);
      else
	{
	  strcat(buf, "; ");
	  strcat(buf, "%s");
	}
      return str_len;
    }
  return str_len;
}
static int
_get_12_event_data2_message_offset_04(int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 = 
    {
      {1, "alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "power_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "diagonstic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {0, "", 0}
    };
  uint64_t val;
  uint8_t alert, power_off, reset, power_cycle, oem_action, diagnostic_interrupt;
  fiid_obj_t obj = NULL;
  int str_len = 0;
  int rv = -1; 

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
  
  FIID_OBJ_GET_CLEANUP (obj, "alert", &val);
  alert = val;
	
  FIID_OBJ_GET_CLEANUP (obj, "power_off", &val);
  power_off = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "reset", &val);
  reset = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "power_cycle", &val);
  power_cycle = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "oem_action", &val);
  oem_action = val;

  FIID_OBJ_GET_CLEANUP (obj, "diagnostic_interrupt", &val);
  diagnostic_interrupt = val;

  memset(buf, '\0', buflen);

  ERR_CLEANUP (!((str_len = _strcat12(buf, buflen, alert, str_len, 0)) < 0));

  ERR_CLEANUP (!((str_len = _strcat12(buf, buflen, power_off, str_len, 1)) < 0));

  ERR_CLEANUP (!((str_len = _strcat12(buf, buflen, reset, str_len, 2)) < 0));
  
  ERR_CLEANUP (!((str_len = _strcat12(buf, buflen, power_cycle, str_len, 3)) < 0));
  
  ERR_CLEANUP (!((str_len = _strcat12(buf, buflen, oem_action, str_len, 4)) < 0));
  
  ERR_CLEANUP (!((str_len = _strcat12(buf, buflen, diagnostic_interrupt, str_len, 5)) < 0));
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY(obj);
  return rv;
}

static int
_get_12_event_data2_message_offset_05(int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 = 
    {
      {4, "timestamp_clock_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "first_second", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {0, "", 0}
    };
  uint64_t val;
  uint8_t timestamp_clock_type;
  uint8_t first_second;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
  
  FIID_OBJ_GET_CLEANUP (obj, "timestamp_clock_type", &val);
  timestamp_clock_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "first_second", &val);
  first_second = val;
  
  if (timestamp_clock_type <= ipmi_sensor_type_code_12_event_data2_offset_05_timestamp_clock_type_desc_max)
    str1 = ipmi_sensor_type_code_12_event_data2_offset_05_timestamp_clock_type_desc[timestamp_clock_type];

  if (first_second <= ipmi_sensor_type_code_12_event_data2_offset_05_first_second_desc_max)
    str2 = ipmi_sensor_type_code_12_event_data2_offset_05_first_second_desc[first_second];

  rv = _snprintf(buf, buflen, "%s; %s", 
                 str1 ? str1 : "",
                 str2 ? str2 : "");

 cleanup:
  FIID_OBJ_DESTROY(obj);
  return rv;
}

static int
get_12_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x03)
    return _get_12_event_data2_message_offset_03(offset, event_data2, buf, buflen);
  if (offset == 0x04)
    return _get_12_event_data2_message_offset_04(offset, event_data2, buf, buflen);
  if (offset == 0x05)
    return _get_12_event_data2_message_offset_05(offset, event_data2, buf, buflen);

  ERR_EINVAL (0);
}

static int
get_19_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x00 && event_data2 <= ipmi_sensor_type_code_19_event_data2_offset_00_desc_max)
    return _snprintf (buf, buflen, ipmi_sensor_type_code_19_event_data2_offset_00_desc[event_data2]);
  
  ERR_EINVAL (0);
}

static int
get_1D_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x07)
    {
      fiid_template_t tmpl_event_data2 = 
	{
          {4, "restart_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {0, "", 0}
	};
      uint64_t val;
      fiid_obj_t obj = NULL;
      int rv = -1;

      FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
      
      FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));

      FIID_OBJ_GET_CLEANUP (obj, "restart_cause", &val);

      if (val <= ipmi_sensor_type_code_1D_event_data2_offset_07_restart_cause_desc_max)
        rv = _snprintf (buf, buflen, ipmi_sensor_type_code_1D_event_data2_offset_07_restart_cause_desc[val]);

    cleanup:
      FIID_OBJ_DESTROY(obj);
      return rv;
    }
  
  ERR_EINVAL (0);
}

static int
get_21_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 = 
    {
      {7, "slot_connector_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {0, "", 0}
    };
  uint64_t val;
  fiid_obj_t obj = NULL;
  int rv = -1;
  
  assert(buf && buflen);
  
  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
  
  FIID_OBJ_GET_CLEANUP (obj, "slot_connector_type", &val);
  
  if (val <= ipmi_sensor_type_code_21_event_data2_offset_09_slot_connector_type_desc_max)
    rv = _snprintf (buf, buflen, ipmi_sensor_type_code_21_event_data2_offset_09_slot_connector_type_desc[val]);
  
 cleanup:
  FIID_OBJ_DESTROY(obj);
  return rv;
}

static int
get_23_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 = 
    {
      {4, "timer_at_expiration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {4, "interrupt_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {0, "", 0}
    };
  uint64_t val;
  uint8_t timer_at_expiration;
  uint8_t interrupt_type;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;
  
  assert(buf && buflen);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
  
  FIID_OBJ_GET_CLEANUP (obj, "timer_at_expiration", &val);
  timer_at_expiration = val;

  FIID_OBJ_GET_CLEANUP (obj, "interrupt_type", &val);
  interrupt_type = val;

  if (timer_at_expiration <= ipmi_sensor_type_code_23_event_data2_offset_08_timer_use_at_expiration_desc_max)
    str1 = ipmi_sensor_type_code_23_event_data2_offset_08_timer_use_at_expiration_desc[timer_at_expiration];
  
  if (interrupt_type <= ipmi_sensor_type_code_23_event_data2_offset_08_interrupt_type_desc_max)
    str2 = ipmi_sensor_type_code_23_event_data2_offset_08_interrupt_type_desc[interrupt_type];
  
  if (str1 || str2)
    rv = _snprintf (buf, buflen, "%s%s%s", 
                    (str1 ? str1 : ""), 
                    ((str1 && str2) ? "; " : ""), 
                    (str2 ? str2 : ""));
  
 cleanup:
  FIID_OBJ_DESTROY(obj);
  return rv;
}

static int
get_28_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  int rv = -1;

  assert(buf && buflen);

  if (offset == 0x00 || offset == 0x04)
    rv = _snprintf (buf, buflen, "Sensor Number #%d", event_data2);
  else if (offset == 0x05)
    {
      fiid_template_t tmpl_event_data2 = 
	{
          {3, "private_bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          {1, "fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {0, "", 0}
	};
      uint64_t val;
      uint8_t private_bus_id, lun, fru_device;
      fiid_obj_t obj = NULL;
      char *str = NULL;
      int rv = -1;

      FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
      
      FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
      
      FIID_OBJ_GET_CLEANUP (obj, "private_bus_id", &val);
      private_bus_id = val;

      FIID_OBJ_GET_CLEANUP (obj, "lun", &val);
      lun = val;

      FIID_OBJ_GET_CLEANUP (obj, "fru_device", &val);
      fru_device = val;
  
      if (fru_device <= ipmi_sensor_type_code_28_event_data2_offset_05_logical_fru_device_desc_max)
	str = ipmi_sensor_type_code_28_event_data2_offset_05_logical_fru_device_desc[fru_device];

      
      rv = _snprintf(buf, buflen, "%s; LUN for Master Write-Read command or FRU Command #%d; Private bus ID #%d", 
                     str ? str : "", 
                     lun, private_bus_id);

    cleanup:
      FIID_OBJ_DESTROY(obj);
      return rv;
    }

  ERR_EINVAL (0);
}

static int
get_2A_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x01)
    {
      fiid_template_t tmpl_event_data2 = 
	{
	  {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {0, "", 0}
	};
      uint64_t val;
      fiid_obj_t obj = NULL;
      int rv = -1;

      FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
      
      FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
      
      FIID_OBJ_GET_CLEANUP (obj, "user_id", &val);
             
      if (val == 0x0)
	rv = _snprintf(buf, buflen, "User ID for user that activated session = Unspecified");
      else
	rv = _snprintf(buf, buflen, "User ID for user that activated session #%d", (int) val);

    cleanup:
      FIID_OBJ_DESTROY(obj);
      return rv;
    }
  
  ERR_EINVAL (0);
}

static int
get_2B_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (event_data2 <= ipmi_sensor_type_code_2B_event_data2_offset_07_version_change_type_desc_max)
    return _snprintf (buf, buflen, ipmi_sensor_type_code_2B_event_data2_offset_07_version_change_type_desc[event_data2]);
  
  ERR_EINVAL (0);
}

static int
get_2C_event_data2_message (int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 = 
    {
      {4, "previous_state_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {4, "cause_of_state_change", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {0, "", 0}
    };
  uint64_t val;
  uint8_t previous_state_offset;
  uint8_t cause_of_state_change;
  char *str = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;
  
  assert(buf && buflen);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data2);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data2, sizeof(uint8_t));
  
  FIID_OBJ_GET_CLEANUP (obj, "previous_state_offset", &val);
  previous_state_offset = val;

  FIID_OBJ_GET_CLEANUP (obj, "cause_of_state_change", &val);
  cause_of_state_change = val;
  
  if (cause_of_state_change <= ipmi_sensor_type_code_2C_event_data2_offset_07_cause_of_state_change_desc_max)
    str = ipmi_sensor_type_code_2C_event_data2_offset_07_cause_of_state_change_desc[cause_of_state_change];
  
  rv = _snprintf (buf, buflen, "Previous state offset value = %d; %s", previous_state_offset, str ? str : "");
  
 cleanup:
  FIID_OBJ_DESTROY(obj);
  return rv;
}

static int
get_08_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x06)
    {
      fiid_template_t tmpl_event_data3 = 
	{
	  {4, "event_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {0, "", 0}
	};
      uint64_t val;      
      fiid_obj_t obj = NULL;
      int rv = -1;

      FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data3);

      FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data3, sizeof(uint8_t));
      
      FIID_OBJ_GET_CLEANUP (obj, "event_type", &val);
      
      if (val <= ipmi_sensor_type_code_08_event_data3_offset_06_error_type_desc_max)
        rv = _snprintf (buf, buflen, ipmi_sensor_type_code_08_event_data3_offset_06_error_type_desc[val]);

    cleanup:
      FIID_OBJ_DESTROY(obj);
      return rv;
    }
  
  ERR_EINVAL (0);
}

static int
get_0C_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x08)
    return _snprintf (buf, buflen, "Memory module/device #%d", event_data3);
  
  ERR_EINVAL (0);
}

static int
get_10_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  switch (offset)
    {
    case 0x01:
      {
	fiid_template_t tmpl_event_data3 = 
	  {
	    {4, "event_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	    {1, "assertion_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	    {1, "logging_disabled_all_events", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	    {0, "", 0}
	  };
	uint64_t val;
	uint8_t event_offset;
	uint8_t assertion_deassertion_event;
	uint8_t logging_disabled_all_events;
        fiid_obj_t obj = NULL;
	char *str1 = NULL;
	char *str2 = NULL;
	int rv = -1;
        
	FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data3);
        
        FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data3, sizeof(uint8_t));
      
        FIID_OBJ_GET_CLEANUP (obj, "event_offset", &val);
	event_offset = val;

        FIID_OBJ_GET_CLEANUP (obj, "assertion_deassertion_e", &val);
	assertion_deassertion_event = val;

        FIID_OBJ_GET_CLEANUP (obj, "logging_disabled_all_ev", &val);
	logging_disabled_all_events = val;

        if (assertion_deassertion_event <= ipmi_sensor_type_code_10_event_data3_offset_01_assertion_event_desc_max)
	  str1 = ipmi_sensor_type_code_10_event_data3_offset_01_assertion_event_desc[assertion_deassertion_event];

	if (logging_disabled_all_events <= ipmi_sensor_type_code_10_event_data3_offset_01_logging_disabled_all_events_desc_max)
	  str2 = ipmi_sensor_type_code_10_event_data3_offset_01_logging_disabled_all_events_desc[logging_disabled_all_events];

	rv = _snprintf (buf, buflen, "Event Offset #%d; %s%s%s", 
			event_offset, (str1 ? str1 : ""), ((str1 && str2 && strlen(str2)) ? "; " : ""), (str2 ? str2 : ""));

      cleanup:
	FIID_OBJ_DESTROY(obj);
	return rv;
      }
    case 0x05:
      return _snprintf (buf, buflen, "%d% full", event_data3);
    }
  
  ERR_EINVAL (0);
}

static int
get_19_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x00 && event_data3 <= ipmi_sensor_type_code_19_event_data3_offset_00_desc_max)
    return _snprintf (buf, buflen, ipmi_sensor_type_code_19_event_data3_offset_00_desc[event_data3]);
  
  ERR_EINVAL (0);
}

static int
get_1D_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x07)
    return _snprintf (buf, buflen, "Channel Number used to deliver command that generated restart #%d", event_data3);
  
  ERR_EINVAL (0);
}

static int
get_21_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  return _snprintf (buf, buflen, "Slot/Connector #%d", event_data3);
}

static int
get_28_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x05)
    return _snprintf (buf, buflen, "FRU Device ID/Slave Address #%d", event_data3);
  
  ERR_EINVAL (0);
}

static int
get_2A_event_data3_message (int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert(buf && buflen);

  if (offset == 0x01 || offset == 0x02)
    {
      fiid_template_t tmpl_event_data3 = 
	{
	  {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {2, "deactivation_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  
	  {0, "", 0}
	};
      uint64_t val;
      uint8_t channel_number;
      uint8_t deactivation_cause;
      fiid_obj_t obj = NULL;
      char *str = NULL;
      int rv = -1;

      FIID_OBJ_CREATE_CLEANUP(obj, tmpl_event_data3);

      FIID_OBJ_SET_ALL_CLEANUP(obj, &event_data3, sizeof(uint8_t));
      
      FIID_OBJ_GET_CLEANUP (obj, "channel_number", &val);
      channel_number = val;
      
      FIID_OBJ_GET_CLEANUP (obj, "deactivation_cause", &val);
      deactivation_cause = val;
      
      /* output deactivation case only if deactivation offset occurred */
      if (offset == 0x02)
        {
          if (deactivation_cause <= ipmi_sensor_type_code_2A_event_data3_offset_01_deactivation_cause_desc_max)
            str = ipmi_sensor_type_code_2A_event_data3_offset_01_deactivation_cause_desc[deactivation_cause];
        }

      rv = _snprintf (buf, buflen, "Channel number that session was activated/deactivated = %d%s%s", 
		      channel_number, (str) ? "; " : "", str ? str : ""); 

    cleanup:
      FIID_OBJ_DESTROY(obj);
      return rv;
    }
  
  ERR_EINVAL (0);
}

/***************************************************/

static int
_get_event_message(uint16_t offset, 
		   char *buf, 
		   unsigned int buflen,
		   uint16_t offset_max,
		   char *string_array[])
{
  int rv;

  assert(buf && buflen);

  ERR_EINVAL (!(offset > offset_max));

  rv = snprintf(buf, buflen, string_array[offset]);

  /* -1 to account for '\0' */
  ERR_ENOSPC(!(rv >= (buflen - 1)));

  return (0);
}

int
ipmi_get_generic_event_message (uint8_t event_reading_type_code, 
				uint16_t offset,
				char *buf,
				unsigned int buflen)
{
  ERR_EINVAL (buf && buflen);

  switch (event_reading_type_code)
    {
    case 0x01: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_01_desc_max, ipmi_generic_event_reading_type_code_01_desc);
    case 0x02: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_02_desc_max, ipmi_generic_event_reading_type_code_02_desc);
    case 0x03: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_03_desc_max, ipmi_generic_event_reading_type_code_03_desc);
    case 0x04: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_04_desc_max, ipmi_generic_event_reading_type_code_04_desc);
    case 0x05: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_05_desc_max, ipmi_generic_event_reading_type_code_05_desc);
    case 0x06: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_06_desc_max, ipmi_generic_event_reading_type_code_06_desc);
    case 0x07: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_07_desc_max, ipmi_generic_event_reading_type_code_07_desc);
    case 0x08: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_08_desc_max, ipmi_generic_event_reading_type_code_08_desc);
    case 0x09: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_09_desc_max, ipmi_generic_event_reading_type_code_09_desc);
    case 0x0A: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_0A_desc_max, ipmi_generic_event_reading_type_code_0A_desc);
    case 0x0B: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_0B_desc_max, ipmi_generic_event_reading_type_code_0B_desc);
    case 0x0C: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_0C_desc_max, ipmi_generic_event_reading_type_code_0C_desc);
    }
  
  return -1;
}

int
ipmi_get_sensor_type_code_message (int sensor_type_code, 
				   int offset, 
				   char *buf, 
				   unsigned int buflen)
{
  ERR_EINVAL (buf && buflen);

  switch (sensor_type_code)
    {
    case 0x01: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_01_desc_max, ipmi_sensor_type_code_01_desc);
    case 0x02: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_02_desc_max, ipmi_sensor_type_code_02_desc);
    case 0x03: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_03_desc_max, ipmi_sensor_type_code_03_desc);
    case 0x04: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_04_desc_max, ipmi_sensor_type_code_04_desc);
    case 0x05: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_05_desc_max, ipmi_sensor_type_code_05_desc);
    case 0x06: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_06_desc_max, ipmi_sensor_type_code_06_desc);
    case 0x07: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_07_desc_max, ipmi_sensor_type_code_07_desc);
    case 0x08: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_08_desc_max, ipmi_sensor_type_code_08_desc);
    case 0x09: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_09_desc_max, ipmi_sensor_type_code_09_desc);
    case 0x0C: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_0C_desc_max, ipmi_sensor_type_code_0C_desc);
    case 0x0D: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_0D_desc_max, ipmi_sensor_type_code_0D_desc);
    case 0x0F: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_0F_desc_max, ipmi_sensor_type_code_0F_desc);
    case 0x10: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_10_desc_max, ipmi_sensor_type_code_10_desc);
    case 0x11: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_11_desc_max, ipmi_sensor_type_code_11_desc);
    case 0x12: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_12_desc_max, ipmi_sensor_type_code_12_desc);
    case 0x13: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_13_desc_max, ipmi_sensor_type_code_13_desc);
    case 0x14: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_14_desc_max, ipmi_sensor_type_code_14_desc);
    case 0x19: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_19_desc_max, ipmi_sensor_type_code_19_desc);
    case 0x1B: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1B_desc_max, ipmi_sensor_type_code_1B_desc);
    case 0x1D: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1D_desc_max, ipmi_sensor_type_code_1D_desc);
    case 0x1E: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1E_desc_max, ipmi_sensor_type_code_1E_desc);
    case 0x1F: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1F_desc_max, ipmi_sensor_type_code_1F_desc);
    case 0x20: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_20_desc_max, ipmi_sensor_type_code_20_desc);
    case 0x21: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_21_desc_max, ipmi_sensor_type_code_21_desc);
    case 0x22: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_22_desc_max, ipmi_sensor_type_code_22_desc);
    case 0x23: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_23_desc_max, ipmi_sensor_type_code_23_desc);
    case 0x24: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_24_desc_max, ipmi_sensor_type_code_24_desc);
    case 0x25: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_25_desc_max, ipmi_sensor_type_code_25_desc);
    case 0x27: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_27_desc_max, ipmi_sensor_type_code_27_desc);
    case 0x28: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_28_desc_max, ipmi_sensor_type_code_28_desc);
    case 0x29: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_29_desc_max, ipmi_sensor_type_code_29_desc);
    case 0x2A: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_2A_desc_max, ipmi_sensor_type_code_2A_desc);
    case 0x2B: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_2B_desc_max, ipmi_sensor_type_code_2B_desc);
    case 0x2C: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_2C_desc_max, ipmi_sensor_type_code_2C_desc);
    }
  
  ERR_EINVAL(0);
}

int
ipmi_get_generic_event_message_short (uint8_t event_reading_type_code, 
                                      uint16_t offset,
                                      char *buf,
                                      unsigned int buflen)
{
  ERR_EINVAL (buf && buflen);

  switch (event_reading_type_code)
    {
    case 0x01: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_01_short_desc_max, ipmi_generic_event_reading_type_code_01_short_desc);
    case 0x02: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_02_short_desc_max, ipmi_generic_event_reading_type_code_02_short_desc);
    case 0x03: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_03_short_desc_max, ipmi_generic_event_reading_type_code_03_short_desc);
    case 0x04: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_04_short_desc_max, ipmi_generic_event_reading_type_code_04_short_desc);
    case 0x05: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_05_short_desc_max, ipmi_generic_event_reading_type_code_05_short_desc);
    case 0x06: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_06_short_desc_max, ipmi_generic_event_reading_type_code_06_short_desc);
    case 0x07: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_07_short_desc_max, ipmi_generic_event_reading_type_code_07_short_desc);
    case 0x08: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_08_short_desc_max, ipmi_generic_event_reading_type_code_08_short_desc);
    case 0x09: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_09_short_desc_max, ipmi_generic_event_reading_type_code_09_short_desc);
    case 0x0A: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_0A_short_desc_max, ipmi_generic_event_reading_type_code_0A_short_desc);
    case 0x0B: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_0B_short_desc_max, ipmi_generic_event_reading_type_code_0B_short_desc);
    case 0x0C: return _get_event_message(offset, buf, buflen, ipmi_generic_event_reading_type_code_0C_short_desc_max, ipmi_generic_event_reading_type_code_0C_short_desc);
    }
  
  return -1;
}

int
ipmi_get_sensor_type_code_message_short (int sensor_type_code, 
                                         int offset, 
                                         char *buf, 
                                         unsigned int buflen)
{
  ERR_EINVAL (buf && buflen);

  switch (sensor_type_code)
    {
    case 0x01: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_01_short_desc_max, ipmi_sensor_type_code_01_short_desc);
    case 0x02: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_02_short_desc_max, ipmi_sensor_type_code_02_short_desc);
    case 0x03: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_03_short_desc_max, ipmi_sensor_type_code_03_short_desc);
    case 0x04: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_04_short_desc_max, ipmi_sensor_type_code_04_short_desc);
    case 0x05: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_05_short_desc_max, ipmi_sensor_type_code_05_short_desc);
    case 0x06: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_06_short_desc_max, ipmi_sensor_type_code_06_short_desc);
    case 0x07: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_07_short_desc_max, ipmi_sensor_type_code_07_short_desc);
    case 0x08: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_08_short_desc_max, ipmi_sensor_type_code_08_short_desc);
    case 0x09: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_09_short_desc_max, ipmi_sensor_type_code_09_short_desc);
    case 0x0C: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_0C_short_desc_max, ipmi_sensor_type_code_0C_short_desc);
    case 0x0D: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_0D_short_desc_max, ipmi_sensor_type_code_0D_short_desc);
    case 0x0F: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_0F_short_desc_max, ipmi_sensor_type_code_0F_short_desc);
    case 0x10: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_10_short_desc_max, ipmi_sensor_type_code_10_short_desc);
    case 0x11: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_11_short_desc_max, ipmi_sensor_type_code_11_short_desc);
    case 0x12: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_12_short_desc_max, ipmi_sensor_type_code_12_short_desc);
    case 0x13: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_13_short_desc_max, ipmi_sensor_type_code_13_short_desc);
    case 0x14: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_14_short_desc_max, ipmi_sensor_type_code_14_short_desc);
    case 0x19: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_19_short_desc_max, ipmi_sensor_type_code_19_short_desc);
    case 0x1B: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1B_short_desc_max, ipmi_sensor_type_code_1B_short_desc);
    case 0x1D: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1D_short_desc_max, ipmi_sensor_type_code_1D_short_desc);
    case 0x1E: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1E_short_desc_max, ipmi_sensor_type_code_1E_short_desc);
    case 0x1F: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_1F_short_desc_max, ipmi_sensor_type_code_1F_short_desc);
    case 0x20: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_20_short_desc_max, ipmi_sensor_type_code_20_short_desc);
    case 0x21: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_21_short_desc_max, ipmi_sensor_type_code_21_short_desc);
    case 0x22: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_22_short_desc_max, ipmi_sensor_type_code_22_short_desc);
    case 0x23: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_23_short_desc_max, ipmi_sensor_type_code_23_short_desc);
    case 0x24: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_24_short_desc_max, ipmi_sensor_type_code_24_short_desc);
    case 0x25: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_25_short_desc_max, ipmi_sensor_type_code_25_short_desc);
    case 0x27: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_27_short_desc_max, ipmi_sensor_type_code_27_short_desc);
    case 0x28: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_28_short_desc_max, ipmi_sensor_type_code_28_short_desc);
    case 0x29: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_29_short_desc_max, ipmi_sensor_type_code_29_short_desc);
    case 0x2A: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_2A_short_desc_max, ipmi_sensor_type_code_2A_short_desc);
    case 0x2B: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_2B_short_desc_max, ipmi_sensor_type_code_2B_short_desc);
    case 0x2C: return _get_event_message(offset, buf, buflen, ipmi_sensor_type_code_2C_short_desc_max, ipmi_sensor_type_code_2C_short_desc);
    }
  
  ERR_EINVAL(0);
}

int
ipmi_get_event_data2_message (int sensor_type_code, 
			      int offset, 
			      uint8_t event_data2, 
			      char *buf, 
			      unsigned int buflen)
{
  ERR_EINVAL (buf && buflen);

  switch (sensor_type_code)
    {
    case 0x05: return get_05_event_data2_message (offset, event_data2, buf, buflen);
    case 0x0F: return get_0F_event_data2_message (offset, event_data2, buf, buflen);
    case 0x10: return get_10_event_data2_message (offset, event_data2, buf, buflen);
    case 0x12: return get_12_event_data2_message (offset, event_data2, buf, buflen);
    case 0x19: return get_19_event_data2_message (offset, event_data2, buf, buflen);
    case 0x1D: return get_1D_event_data2_message (offset, event_data2, buf, buflen);
    case 0x21: return get_21_event_data2_message (offset, event_data2, buf, buflen);
    case 0x23: return get_23_event_data2_message (offset, event_data2, buf, buflen);
    case 0x28: return get_28_event_data2_message (offset, event_data2, buf, buflen);
    case 0x2A: return get_2A_event_data2_message (offset, event_data2, buf, buflen);
    case 0x2B: return get_2B_event_data2_message (offset, event_data2, buf, buflen);
    case 0x2C: return get_2C_event_data2_message (offset, event_data2, buf, buflen);
    }

  ERR_EINVAL(0);
}

int
ipmi_get_event_data3_message (int sensor_type_code, 
			      int offset, 
			      uint8_t event_data2, 
			      uint8_t event_data3, 
			      char *buf, 
			      unsigned int buflen)
{
  ERR_EINVAL (buf && buflen);

  switch (sensor_type_code)
    {
    case 0x08: return get_08_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    case 0x0C: return get_0C_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    case 0x10: return get_10_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    case 0x19: return get_19_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    case 0x1D: return get_1D_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    case 0x21: return get_21_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    case 0x28: return get_28_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    case 0x2A: return get_2A_event_data3_message (offset, event_data2, event_data3, buf, buflen);
    }
  
  ERR_EINVAL(0);
}

