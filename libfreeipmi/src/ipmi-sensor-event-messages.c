/* 
   ipmi-sensor-event-messages.c - IPMI Sensor Event Messages

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#include "freeipmi.h"

/* 
 * Generic Event Reading Strings
 */
const char *const ipmi_generic_event_reading_type_code_01_desc[] =
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

const char *const ipmi_generic_event_reading_type_code_02_desc[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_03_desc[] =
  {
    "State Deasserted",
    "State Asserted",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_04_desc[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_05_desc[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_06_desc[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_07_desc[] =
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

const char *const ipmi_generic_event_reading_type_code_08_desc[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_09_desc[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_0A_desc[] =
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

const char *const ipmi_generic_event_reading_type_code_0B_desc[] =
  {
    "Fully Redundant (formerly \"Redundancy Regained\")",
    "Redundancy Lost",
    "Redundancy Degraded",
    "Entered from Redundancy Degraded or Fully Redundant",
    "Entered from Non-redundant:Insufficient Resources",
    "Non-redundant:Insufficient Resources",
    "Redundancy Degraded from Fully Redundant",
    "Redundancy Degraded from Non-redundant",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_0C_desc[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };

const char *const ipmi_generic_event_reading_type_code_0D_desc[] =
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

/* 
 * Sensor Type Strings
 */

const char *const ipmi_sensor_type_code_01_desc[] =
  {
    "Temperature",
    NULL
  };

const char *const ipmi_sensor_type_code_02_desc[] =
  {
    "Voltage",
    NULL
  };

const char *const ipmi_sensor_type_code_03_desc[] =
  {
    "Current",
    NULL
  };

const char *const ipmi_sensor_type_code_04_desc[] =
  {
    "Fan",
    NULL
  };

/* achu: 'undock' removed as noted in errata */
const char *const ipmi_sensor_type_code_05_desc[] =
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

const char *const ipmi_sensor_type_code_06_desc[] =
  {
    "Secure Mode (Front Panel Lockout) Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation attempt - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };

const char *const ipmi_sensor_type_code_07_desc[] =
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

const char *const ipmi_sensor_type_code_08_desc[] =
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

const char *const ipmi_sensor_type_code_09_desc[] =
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

/* achu: new additions as stated in errata */
const char *const ipmi_sensor_type_code_0C_desc[] =
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
    NULL
  };

const char *const ipmi_sensor_type_code_0F_desc[] =
  {
    "System Firmware Error (POST Error)",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };

const char *const ipmi_sensor_type_code_10_desc[] =
  {
    "Correctable Memory Error Logging Disabled",
    "Event `Type' Logging Disabled",
    "Log Area Reset/Cleared",
    "All Event Logging Disabled",
    "SEL Full",
    "SEL Almost Full",
    NULL
  };

const char *const ipmi_sensor_type_code_11_desc[] =
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

const char *const ipmi_sensor_type_code_12_desc[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };

const char *const ipmi_sensor_type_code_13_desc[] =
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
    NULL
  };

const char *const ipmi_sensor_type_code_14_desc[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open (Switch indicating FRU latch is in `unlatched' position and FRU is mechanically removable)",
    "FRU service request button (pressed, service, e.g. removal/replacement, requested)",
    NULL
  };

const char *const ipmi_sensor_type_code_19_desc[] =
  {
    "Soft Power Control Failure (chipset did not respond to BMC request to change system power state)", 
    NULL
  };

const char *const ipmi_sensor_type_code_1B_desc[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected / Incorrect inerconnection",
    NULL
  };

/* achu: new additions as stated in errata */
const char *const ipmi_sensor_type_code_1D_desc[] =
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

const char *const ipmi_sensor_type_code_1E_desc[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };

const char *const ipmi_sensor_type_code_1F_desc[] =
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

/* achu: modified per errata */
const char *const ipmi_sensor_type_code_20_desc[] =
  {
    "Critical stop during OS load / initialization.  Unexpected error during system startup.  Stopped waiting for input or power cycle/reset.",
    "Run-time Critical Stop (a.k.a. 'core dump', 'blue screen')",
    "OS Graceful Stop (system powered up, but normal OS operation has shut down and system is awaiting reset pushbutton, powercycle or other external input)",
    "OS Graceful Shutdown (system graceful power down by OS)",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding.  Graceful shutdown request to agent via BMC did not occur due to missing or malfunctioning local agent.",
    NULL
  };

const char *const ipmi_sensor_type_code_21_desc[] =
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

const char *const ipmi_sensor_type_code_22_desc[] =
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

const char *const ipmi_sensor_type_code_23_desc[] =
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

const char *const ipmi_sensor_type_code_24_desc[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated, formatted per IPMI PET specification",
    "platform generated SNMP trap, OEM format",
    NULL
  };

const char *const ipmi_sensor_type_code_25_desc[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };

const char *const ipmi_sensor_type_code_27_desc[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };

/* achu: new additions as stated in errata */
const char *const ipmi_sensor_type_code_28_desc[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };

const char *const ipmi_sensor_type_code_29_desc[] =
  {
    "battery low (predictive failure)",
    "battery failed",
    "battery presence detected",
    NULL
  };

const char *const ipmi_sensor_type_code_2A_desc[] =
  {
    "Session Activated",
    "Session Deactivated",
    NULL
  };

const char *const ipmi_sensor_type_code_2B_desc[] =
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

const char *const ipmi_sensor_type_code_2C_desc[] =
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

/* 
 * Sensor Type Strings for Event Data 2
 */

const char *const ipmi_sensor_type_code_0F_event_data_2_offset_00_desc[] =
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

const char *const ipmi_sensor_type_code_0F_event_data_2_offset_01_desc[] =
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
    "Floppy initialization",
    "Keyboard test",
    "Pointing device test",
    "Primary processor initialization",
    NULL
  };

const char *const ipmi_sensor_type_code_0F_event_data_2_offset_02_desc[] =
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
    "Floppy initialization",
    "Keyboard test",
    "Pointing device test",
    "Primary processor initialization",
    NULL
  };

const char *const ipmi_sensor_type_code_12_event_data_2_offset_03_log_entry_action_desc[] =
  {
   "Log entry action = entry added",
   "Log entry action = entry added because event did not be map to standard IPMI event",
   "Log entry action = entry added along with one or more corresponding SEL entries",
   "Log entry action = log cleared",
   "Log entry action = log disabled",
   "Log entry action = log enabled",
   NULL
  };

const char *const ipmi_sensor_type_code_12_event_data_2_offset_03_log_type_desc[] =
  {
    "Log Type = MCA log",
    "Log Type = OEM1",
    "Log Type = OEM2",
    NULL
  };

const char *const ipmi_sensor_type_code_12_event_data_2_offset_04_pef_action_desc[] =
  {
    "Alert",
    "power off",
    "reset",
    "power cycle",
    "OEM action",
    "Diagnostic Interrupt (NMI)",
    NULL,
  };

const char *const ipmi_sensor_type_code_19_event_data_2_offset_00_desc[] =
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

const char *const ipmi_sensor_type_code_1D_event_data_2_offset_07_restart_cause_desc[] =
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

const char *const ipmi_sensor_type_code_21_event_data_2_offset_09_slot_connector_type_desc[] =
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

const char *const ipmi_sensor_type_code_23_event_data_2_offset_08_interrupt_type_desc[] =
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

const char *const ipmi_sensor_type_code_23_event_data_2_offset_08_timer_use_at_expiration_desc[] =
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

const char *const ipmi_sensor_type_code_2B_event_data_2_offset_07_version_change_type_desc[] =
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

const char *const ipmi_sensor_type_code_2C_event_data_2_offset_07_cause_of_state_change_desc[] =
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

/* 
 * Sensor Type Strings for Event Data 3
 */

const char *const ipmi_sensor_type_code_08_event_data_3_offset_06_error_type_desc[] = 
  {
    "Vendor mismatch",
    "Revision mismatch",
    "Processor missing or unexpected/unsupported condition",
    NULL
  };

const char *const ipmi_sensor_type_code_19_event_data_3_offset_00_desc[] =
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

const char *const ipmi_sensor_type_code_2A_event_data_3_offset_01_deactivation_cause_desc[] =
  {
    "Session deactivatation cause unspecified. This value is also used for Session Activated events",
    "Session deactivated by Close Session command",
    "Session deactivated by timeout",
    "Session deactivated by configuration change",
    NULL
  };

/**********************************************************/
/***********      event message functions   ***************/
/**********************************************************/
static char *
get_01_generic_event_message (uint16_t offset)
{
  if (offset <= 0x0B)
    return strdup(ipmi_generic_event_reading_type_code_01_desc[offset]);
  
  return NULL;
}

static char *
get_02_generic_event_message (uint16_t offset)
{
  if (offset <= 0x02)
    return strdup(ipmi_generic_event_reading_type_code_02_desc[offset]);
  
  return NULL;
}

static char *
get_03_generic_event_message (uint16_t offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_generic_event_reading_type_code_03_desc[offset]);
  return NULL;
}

static char *
get_04_generic_event_message (uint16_t offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_generic_event_reading_type_code_04_desc[offset]);
  return NULL;
}

static char *
get_05_generic_event_message (uint16_t offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_generic_event_reading_type_code_05_desc[offset]);
  return NULL;
}

static char *
get_06_generic_event_message (uint16_t offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_generic_event_reading_type_code_06_desc[offset]);
  return NULL;
}

static char *
get_07_generic_event_message (uint16_t offset)
{
  if (offset <= 0x08)
    return strdup(ipmi_generic_event_reading_type_code_07_desc[offset]);
  return NULL;
}

static char *
get_08_generic_event_message (uint16_t offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_generic_event_reading_type_code_08_desc[offset]);
  return NULL;
}

static char *
get_09_generic_event_message (uint16_t offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_generic_event_reading_type_code_09_desc[offset]);
  return NULL;
}

static char *
get_0A_generic_event_message (uint16_t offset)
{
  if (offset <= 0x0A)
    return strdup(ipmi_generic_event_reading_type_code_0A_desc[offset]);
  return NULL;
}

static char *
get_0B_generic_event_message (uint16_t offset)
{
  if (offset <= 0x07)
    return strdup(ipmi_generic_event_reading_type_code_0B_desc[offset]);
  return NULL;
}

static char *
get_0C_generic_event_message (uint16_t offset)
{
  if (offset <= 0x03)
    return strdup(ipmi_generic_event_reading_type_code_0C_desc[offset]);
  return NULL;
}

/**********************************************************/
/***********      event message functions   ***************/
/**********************************************************/
static char *
get_01_event_message (int offset)
{
  if (offset <= 0x00)
    return strdup(ipmi_sensor_type_code_01_desc[offset]);
  return NULL;
}

static char *
get_02_event_message (int offset)
{
  if (offset <= 0x00)
    return strdup(ipmi_sensor_type_code_02_desc[offset]);
  return NULL;
}

static char *
get_03_event_message (int offset)
{
  if (offset <= 0x00)
    return strdup(ipmi_sensor_type_code_03_desc[offset]);
  return NULL;
}

static char *
get_04_event_message (int offset)
{
  if (offset <= 0x00)
    return strdup(ipmi_sensor_type_code_04_desc[offset]);
  return NULL;
}

static char *
get_05_event_message (int offset)
{
  if (offset <= 0x06)
    return strdup(ipmi_sensor_type_code_05_desc[offset]);
  return NULL;
}

static char *
get_06_event_message (int offset)
{
  if (offset <= 0x05)
    return strdup(ipmi_sensor_type_code_06_desc[offset]);
  return NULL;
}

static char *
get_07_event_message (int offset)
{
  if (offset <= 0x0A)
    return strdup(ipmi_sensor_type_code_07_desc[offset]);
  return NULL;
}

static char *
get_08_event_message (int offset)
{
  if (offset <= 0x06)
    return strdup(ipmi_sensor_type_code_08_desc[offset]);
  return NULL;
}

static char *
get_09_event_message (int offset)
{
  if (offset <= 0x07)
    return strdup(ipmi_sensor_type_code_09_desc[offset]);
  return NULL;
}

static char *
get_0C_event_message (int offset)
{
  if (offset <= 0x09)
    return strdup(ipmi_sensor_type_code_0C_desc[offset]);
  return NULL;
}

static char *
get_0D_event_message (int offset)
{
  if (offset <= 0x08)
    return strdup(ipmi_generic_event_reading_type_code_0D_desc[offset]);
  return NULL;
}

static char *
get_0F_event_message (int offset)
{
  if (offset <= 0x02)
    return strdup(ipmi_sensor_type_code_0F_desc[offset]);
  return NULL;
}

static char *
get_10_event_message (int offset)
{
  if (offset <= 0x05)
    return strdup(ipmi_sensor_type_code_10_desc[offset]);
  return NULL;
}

static char *
get_11_event_message (int offset)
{
  if (offset <= 0x07)
    return strdup(ipmi_sensor_type_code_11_desc[offset]);
  return NULL;
}

static char *
get_12_event_message (int offset)
{
  if (offset <= 0x05)
    return strdup(ipmi_sensor_type_code_12_desc[offset]);
  return NULL;
}

static char *
get_13_event_message (int offset)
{
  if (offset <= 0x09)
    return strdup(ipmi_sensor_type_code_13_desc[offset]);
  return NULL;
}

static char *
get_14_event_message (int offset)
{
  if (offset <= 0x04)
    return strdup(ipmi_sensor_type_code_14_desc[offset]);
  return NULL;
}

static char *
get_19_event_message (int offset)
{
  if (offset <= 0x00)
    return strdup(ipmi_sensor_type_code_19_desc[offset]);
  return NULL;
}

static char *
get_1B_event_message (int offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_sensor_type_code_1B_desc[offset]);
  return NULL;
}

static char *
get_1D_event_message (int offset)
{
  if (offset <= 0x07)
    return strdup(ipmi_sensor_type_code_1D_desc[offset]);
  return NULL;
}

static char *
get_1E_event_message (int offset)
{
  if (offset <= 0x04)
    return strdup(ipmi_sensor_type_code_1E_desc[offset]);
  return NULL;
}

static char *
get_1F_event_message (int offset)
{
  if (offset <= 0x06)
    return strdup(ipmi_sensor_type_code_1F_desc[offset]);
  return NULL;
}

static char *
get_20_event_message (int offset)
{
  if (offset <= 0x05)
    return strdup(ipmi_sensor_type_code_20_desc[offset]);
  return NULL;
}

static char *
get_21_event_message (int offset)
{
  if (offset <= 0x09)
    return strdup(ipmi_sensor_type_code_21_desc[offset]);
  return NULL;
}

static char *
get_22_event_message (int offset)
{
  if (offset <= 0x0E)
    return strdup(ipmi_sensor_type_code_22_desc[offset]);
  return NULL;
}

static char *
get_23_event_message (int offset)
{
  if (offset <= 0x08)
    return strdup(ipmi_sensor_type_code_23_desc[offset]);
  return NULL;
}

static char *
get_24_event_message (int offset)
{
  if (offset <= 0x03)
    return strdup(ipmi_sensor_type_code_24_desc[offset]);
  return NULL;
}

static char *
get_25_event_message (int offset)
{
  if (offset <= 0x02)
    return strdup(ipmi_sensor_type_code_25_desc[offset]);
  return NULL;
}

static char *
get_27_event_message (int offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_sensor_type_code_27_desc[offset]);
  return NULL;
}

static char *
get_28_event_message (int offset)
{
  if (offset <= 0x05)
    return strdup(ipmi_sensor_type_code_28_desc[offset]);
  return NULL;
}

static char *
get_29_event_message (int offset)
{
  if (offset <= 0x02)
    return strdup(ipmi_sensor_type_code_29_desc[offset]);
  return NULL;
}

static char *
get_2A_event_message (int offset)
{
  if (offset <= 0x01)
    return strdup(ipmi_sensor_type_code_2A_desc[offset]);
  return NULL;
}

static char *
get_2B_event_message (int offset)
{
  if (offset <= 0x07)
    return strdup(ipmi_sensor_type_code_2B_desc[offset]);
  return NULL;
}

static char *
get_2C_event_message (int offset)
{
  if (offset <= 0x07)
    return strdup(ipmi_sensor_type_code_2C_desc[offset]);
  return NULL;
}

/**********************************************************/
/***********  event_data2 message functions  **************/
/**********************************************************/
static char *
get_05_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x04)
    {
      char *str = NULL;
      asprintf (&str, "Network controller #%d", event_data);
      return str;
    }
  
  return NULL;
}

static char *
get_0F_event_data2_message (int offset, uint8_t event_data)
{
  switch (offset)
    {
    case 0x00:
      if (event_data <= 0x0D)
        return strdup(ipmi_sensor_type_code_0F_event_data_2_offset_00_desc[event_data]);
      return NULL;
    case 0x01:
    case 0x02:
      if (event_data <= 0x19)
        return strdup(ipmi_sensor_type_code_0F_event_data_2_offset_01_desc[event_data]);
      return NULL;
    }
  
  return NULL;
}

static char *
get_10_event_data2_message (int offset, uint8_t event_data)
{
  switch (offset)
    {
    case 0x00:
      {
	char *str = NULL;
	asprintf (&str, "Memory module/device #%d", event_data);
	return str;
      }
    case 0x01:
      {
	char *str = NULL;
	asprintf (&str, "Event/Reading Type Code #%d", event_data);
	return str;
      }
    }
  
  return NULL;
}

static char *
get_12_event_data2_message (int offset, uint8_t event_data)
{
  switch (offset)
    {
    case 0x03:
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
	char *str = NULL;
	char *str1 = NULL;
	char *str2 = NULL;
	fiid_obj_t obj = NULL;

        if (!(obj = fiid_obj_create(tmpl_event_data2)))
          return NULL;

        if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        
        if (fiid_obj_get(obj,
                         (uint8_t *)"log_type",
                         &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
	log_type = val;

        if (fiid_obj_get(obj,
                         (uint8_t *)"log_entry_action",
                         &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
	log_entry_action = val;
          
	fiid_obj_destroy(obj);

        if (log_type <= 0x02)
          str1 = strdupa(ipmi_sensor_type_code_12_event_data_2_offset_03_log_entry_action_desc[log_type]);

        if (log_entry_action <= 0x05)
          str2 = strdupa(ipmi_sensor_type_code_12_event_data_2_offset_03_log_type_desc[log_entry_action]);

	if (str1 || str2)
	  {
	    asprintf (&str, "%s%s%s", (str1 ? str1 : ""), 
		      ((str1 && str2) ? "; " : ""), (str2 ? str2 : ""));
	  }
	
	return str;
      }
    case 0x04:
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
	uint8_t alert;
	uint8_t power_off;
	uint8_t reset;
	uint8_t power_cycle;
	uint8_t oem_action;
	uint8_t diagnostic_interrupt;
	char *str = NULL;
	char *tmp_str = NULL;
        fiid_obj_t obj = NULL;
	const char *msg = NULL;
     
        if (!(obj = fiid_obj_create(tmpl_event_data2)))
          return NULL;
        
        if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        
        if (fiid_obj_get (obj,
                          (uint8_t *)"alert", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        alert = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"power_off", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        power_off = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"reset", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        reset = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"power_cycle", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        power_cycle = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"oem_action", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        oem_action = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"diagnostic_interrupt", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        diagnostic_interrupt = val;

	fiid_obj_destroy(obj);

	if (alert)
	  {
	    tmp_str = str;
            msg = ipmi_sensor_type_code_12_event_data_2_offset_04_pef_action_desc[0];
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; %s", tmp_str, msg);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "%s", msg);
	  }
	
	if (power_off)
	  {
	    tmp_str = str;
            msg = ipmi_sensor_type_code_12_event_data_2_offset_04_pef_action_desc[1];
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; %s", tmp_str, msg);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "%s", msg);
	  }
	
	if (reset)
	  {
	    tmp_str = str;
            msg = ipmi_sensor_type_code_12_event_data_2_offset_04_pef_action_desc[2];
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; %s", tmp_str, msg);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "%s", msg);
	  }
	
	if (power_cycle)
	  {
	    tmp_str = str;
            msg = ipmi_sensor_type_code_12_event_data_2_offset_04_pef_action_desc[3];
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; %s", tmp_str, msg);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "%s", msg);
	  }
	
	if (oem_action)
	  {
	    tmp_str = str;
            msg = ipmi_sensor_type_code_12_event_data_2_offset_04_pef_action_desc[4];
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; %s", tmp_str, msg);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "%s", msg);
	  }
	
	if (diagnostic_interrupt)
	  {
	    tmp_str = str;
            msg = ipmi_sensor_type_code_12_event_data_2_offset_04_pef_action_desc[5];
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; %s", tmp_str, msg);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "%s", msg);
	  }
	
	return str;
      }
    case 0x05:
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
	char *str = NULL;
	
        fiid_obj_t obj = NULL;
        
        if (!(obj = fiid_obj_create(tmpl_event_data2)))
          return NULL;
        
        if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        
        if (fiid_obj_get (obj,
                          (uint8_t *)"timestamp_clock_type", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
	timestamp_clock_type = val;
        
        if (fiid_obj_get (obj,
                          (uint8_t *)"first_second", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
	first_second = val;

        fiid_obj_destroy(obj);
	
	asprintf (&str, "%s; %s", 
		  (timestamp_clock_type ? "SDR Timestamp Clock updated" : 
		   "SEL Timestamp Clock updated (Also used when both SEL and SDR Timestamp clocks are linked together)"), 
		  (first_second ? "event is second of pair" : "event is first of pair"));
	
	return str;
      }
    }
  
  return NULL;
}

static char *
get_19_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x00)
    {
      if (event_data <= 0x0C)
        return strdup (ipmi_sensor_type_code_19_event_data_2_offset_00_desc[event_data]);
    }
  
  return NULL;
}

static char *
get_1D_event_data2_message (int offset, uint8_t event_data)
{
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
     
      if (!(obj = fiid_obj_create(tmpl_event_data2)))
        return NULL;
      
      if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }

      if (fiid_obj_get (obj,
                        (uint8_t *)"restart_cause", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }

      fiid_obj_destroy(obj);

      if (val <= 0x08)
        return strdup(ipmi_sensor_type_code_1D_event_data_2_offset_07_restart_cause_desc[val]);
    }
  
  return NULL;
}

static char *
get_21_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x09)
    {
      fiid_template_t tmpl_event_data2 = 
	{
	  {7, "slot_connector_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {0, "", 0}
	};
      uint64_t val;
      
      fiid_obj_t obj = NULL;
     
      if (!(obj = fiid_obj_create(tmpl_event_data2)))
        return NULL;
      
      if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      
      if (fiid_obj_get (obj,
                        (uint8_t *)"slot_connector_type", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }

      fiid_obj_destroy(obj);
      
      if (val <= 0x08)
        return strdup(ipmi_sensor_type_code_21_event_data_2_offset_09_slot_connector_type_desc[val]);
    }
  
  return NULL;
}

static char *
get_23_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x08)
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
      char *str = NULL;
      char *str1 = NULL;
      char *str2 = NULL;
      
      fiid_obj_t obj = NULL;
     
      if (!(obj = fiid_obj_create(tmpl_event_data2)))
        return NULL;
      
      if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      
      if (fiid_obj_get (obj,
                        (uint8_t *)"timer_at_expiration", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      timer_at_expiration = val;

      if (fiid_obj_get (obj,
                        (uint8_t *)"interrupt_type", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      interrupt_type = val;

      fiid_obj_destroy(obj);

      if (timer_at_expiration <= 0x0F)
        str1 = strdupa (ipmi_sensor_type_code_23_event_data_2_offset_08_timer_use_at_expiration_desc[timer_at_expiration]);

      if (interrupt_type <= 0x0F)
        str2 = strdupa (ipmi_sensor_type_code_23_event_data_2_offset_08_interrupt_type_desc[interrupt_type]);

      if (str1 || str2)
	{
	  asprintf (&str, "%s%s%s", (str1 ? str1 : ""), 
		    ((str1 && str2) ? "; " : ""), 
		    (str1 ? str1 : ""));
	}
      
      return str;
    }
  
  return NULL;
}

static char *
get_28_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x00 || offset == 0x04)
    {
      char *str = NULL;
      asprintf (&str, "Sensor Number #%d", event_data);
      return str;
    }
  else if (offset == 0x05)
    {
      fiid_template_t tmpl_event_data2 = 
	{
          {3, "private_bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          {1, "logical_fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
	  {0, "", 0}
	};
      uint64_t val;
      char *str = NULL;
      uint8_t private_bus_id, lun, logical_fru_device;
      
      fiid_obj_t obj = NULL;
     
      if (!(obj = fiid_obj_create(tmpl_event_data2)))
        return NULL;
      
      if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      
      if (fiid_obj_get (obj,
                        (uint8_t *)"private_bus_id", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      private_bus_id = val;

      if (fiid_obj_get (obj,
                        (uint8_t *)"lun", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      lun = val;

      if (fiid_obj_get (obj,
                        (uint8_t *)"logical_fru_device", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      logical_fru_device = val;

      fiid_obj_destroy(obj);
      
      asprintf (&str, "%s; LUN for Master Write-Read command or FRU Command #%d; Private bus ID #%d", (logical_fru_device) ? "device is logical FRU Device (accessed via FRU commands to mgmt. controller" : "device is not a logical FRU Device", lun, private_bus_id);
      return str;
      
    }

  return NULL;
}

static char *
get_2A_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x01)
    {
      fiid_template_t tmpl_event_data2 = 
	{
	  {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
	  {0, "", 0}
	};
      uint64_t val;
      char *str = NULL;
      fiid_obj_t obj = NULL;
     
      if (!(obj = fiid_obj_create(tmpl_event_data2)))
        return NULL;
      
      if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      
      if (fiid_obj_get (obj,
                        (uint8_t *)"user_id", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
        
      fiid_obj_destroy(obj);
      
      if (val == 0x0)
	return strdup ("User ID for user that activated session = Unspecified");
      asprintf (&str, "User ID for user that activated session = %d", (int) val);
      return str;
    }
  
  return NULL;
}

static char *
get_2B_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x07)
    {
      if (event_data <= 0x17)
        return strdup(ipmi_sensor_type_code_2B_event_data_2_offset_07_version_change_type_desc[event_data]);
    }
  
  return NULL;
}

static char *
get_2C_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x07)
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
     
      if (!(obj = fiid_obj_create(tmpl_event_data2)))
        return NULL;
      
      if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      
      if (fiid_obj_get (obj,
                        (uint8_t *)"previous_state_offset", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
        
      previous_state_offset = val;

      if (fiid_obj_get (obj,
                        (uint8_t *)"cause_os_state_change", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }

      cause_of_state_change = val;
      fiid_obj_destroy(obj);
      
      if (cause_of_state_change <= 0x0F)
        {
          asprintf (&str, "Previous state offset value = %d; %s", 
                    previous_state_offset, 
                    ipmi_sensor_type_code_2C_event_data_2_offset_07_cause_of_state_change_desc[cause_of_state_change]);
          return str;
        }
    }
  
  return NULL;
}

/**********************************************************/
/***********  event_data3 message functions  **************/
/**********************************************************/
static char *
get_08_event_data3_message (int offset, uint8_t event_data)
{
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
     
      if (!(obj = fiid_obj_create(tmpl_event_data3)))
        return NULL;
      
      if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      
      if (fiid_obj_get (obj,
                        (uint8_t *)"event_type", 
                        &val) < 0)
        {
          fiid_obj_destroy(obj);
          return NULL;
        }
      
      fiid_obj_destroy(obj);
      if (val <= 0x2)
        return strdup(ipmi_sensor_type_code_08_event_data_3_offset_06_error_type_desc[val]);
    }
  
  return NULL;
}

static char *
get_0C_event_data3_message (int offset, uint8_t event_data)
{
  if (offset == 0x08)
    {
      char *str = NULL;
      asprintf (&str, "Memory module/device #%d", event_data);
      return str;
    }
  
  return NULL;
}

static char *
get_10_event_data3_message (int offset, uint8_t event_data)
{
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
	char *str = NULL;
        fiid_obj_t obj = NULL;
        
        if (!(obj = fiid_obj_create(tmpl_event_data3)))
          return NULL;
        
        if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
      
        if (fiid_obj_get (obj,
                          (uint8_t *)"event_offset", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
	event_offset = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"assertion_deassertion_e", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
	assertion_deassertion_event = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"logging_disabled_all_ev", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
	logging_disabled_all_events = val;
        
        fiid_obj_destroy(obj);
	
	asprintf (&str, "Event Offset %d; %s%s", event_offset, 
		  (assertion_deassertion_event ? "assertion event" : "deassertion event"), 
		  (logging_disabled_all_events ? 
		   "; logging has been disabled for all events of given type" : ""));
	
	return str;
      }
    case 0x05:
      {
	char *str = NULL;
	asprintf (&str, "%d%% full", event_data);
	return str;
      }
    }
  
  return NULL;
}

static char *
get_19_event_data3_message (int offset, uint8_t event_data)
{
  if (offset == 0x00)
    {
      if (event_data <= 0x0D)
        return strdup(ipmi_sensor_type_code_19_event_data_3_offset_00_desc[event_data]);
    }
  
  return NULL;
}

static char *
get_1D_event_data3_message (int offset, uint8_t event_data)
{
  if (offset == 0x07)
    {
      char *str = NULL;
      asprintf (&str, "Channel Number used to deliver command that generated restart #%d", event_data);
      return str;
    }
  
  return NULL;
}

static char *
get_21_event_data3_message (int offset, uint8_t event_data)
{
  if (offset == 0x09)
    {
      char *str = NULL;
      asprintf (&str, "Slot/Connector# %d", event_data);
      return str;
    }
  
  return NULL;
}

static char *
get_28_event_data3_message (int offset, uint8_t event_data)
{
  if (offset == 0x05)
    {
      /* XXX: achu: this code logic cannot support this function, 
       * since it requires info from the event data 2
       */ 
    }
  
  return NULL;
}

static char *
get_2A_event_data3_message (int offset, uint8_t event_data)
{
  if (offset == 0x01)
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
      char *str = NULL;
      
        fiid_obj_t obj = NULL;
        
        if (!(obj = fiid_obj_create(tmpl_event_data3)))
          return NULL;
        
        if (fiid_obj_set_all(obj, &event_data, sizeof(uint8_t)) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
      
        if (fiid_obj_get (obj,
                          (uint8_t *)"channel_number", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        channel_number = val;

        if (fiid_obj_get (obj,
                          (uint8_t *)"deactivation_cause", 
                          &val) < 0)
          {
            fiid_obj_destroy(obj);
            return NULL;
          }
        deactivation_cause = val;
        
        fiid_obj_destroy(obj);

        if (deactivation_cause <= 0x03)
          {
            asprintf (&str, "Channel number that session was activated/deactivated = %d; %s", channel_number, ipmi_sensor_type_code_2A_event_data_3_offset_01_deactivation_cause_desc[deactivation_cause]);
            return str;
          }
    }
  
  return NULL;
}

/***************************************************/
char *
ipmi_get_generic_event_message (uint8_t event_reading_type, uint16_t offset)
{
  switch (event_reading_type)
    {
    case 0x01: return get_01_generic_event_message (offset);
    case 0x02: return get_02_generic_event_message (offset);
    case 0x03: return get_03_generic_event_message (offset);
    case 0x04: return get_04_generic_event_message (offset);
    case 0x05: return get_05_generic_event_message (offset);
    case 0x06: return get_06_generic_event_message (offset);
    case 0x07: return get_07_generic_event_message (offset);
    case 0x08: return get_08_generic_event_message (offset);
    case 0x09: return get_09_generic_event_message (offset);
    case 0x0A: return get_0A_generic_event_message (offset);
    case 0x0B: return get_0B_generic_event_message (offset);
    case 0x0C: return get_0C_generic_event_message (offset);
    }
  
  return NULL;
}

char *
ipmi_get_event_message (int sensor_type_code, int offset)
{
  switch (sensor_type_code)
    {
    case 0x01: return get_01_event_message (offset);
    case 0x02: return get_02_event_message (offset);
    case 0x03: return get_03_event_message (offset);
    case 0x04: return get_04_event_message (offset);
    case 0x05: return get_05_event_message (offset);
    case 0x06: return get_06_event_message (offset);
    case 0x07: return get_07_event_message (offset);
    case 0x08: return get_08_event_message (offset);
    case 0x09: return get_09_event_message (offset);
    case 0x0C: return get_0C_event_message (offset);
    case 0x0D: return get_0D_event_message (offset);
    case 0x0F: return get_0F_event_message (offset);
    case 0x10: return get_10_event_message (offset);
    case 0x11: return get_11_event_message (offset);
    case 0x12: return get_12_event_message (offset);
    case 0x13: return get_13_event_message (offset);
    case 0x14: return get_14_event_message (offset);
    case 0x19: return get_19_event_message (offset);
    case 0x1B: return get_1B_event_message (offset);
    case 0x1D: return get_1D_event_message (offset);
    case 0x1E: return get_1E_event_message (offset);
    case 0x1F: return get_1F_event_message (offset);
    case 0x20: return get_20_event_message (offset);
    case 0x21: return get_21_event_message (offset);
    case 0x22: return get_22_event_message (offset);
    case 0x23: return get_23_event_message (offset);
    case 0x24: return get_24_event_message (offset);
    case 0x25: return get_25_event_message (offset);
    case 0x27: return get_27_event_message (offset);
    case 0x28: return get_28_event_message (offset);
    case 0x29: return get_29_event_message (offset);
    case 0x2A: return get_2A_event_message (offset);
    case 0x2B: return get_2B_event_message (offset);
    case 0x2C: return get_2C_event_message (offset);
    }
  
  return NULL;
}

char *
ipmi_get_event_data2_message (int sensor_type_code, int offset, uint8_t event_data)
{
  switch (sensor_type_code)
    {
    case 0x05: return get_05_event_data2_message (offset, event_data);
    case 0x0F: return get_0F_event_data2_message (offset, event_data);
    case 0x10: return get_10_event_data2_message (offset, event_data);
    case 0x12: return get_12_event_data2_message (offset, event_data);
    case 0x19: return get_19_event_data2_message (offset, event_data);
    case 0x1D: return get_1D_event_data2_message (offset, event_data);
    case 0x21: return get_21_event_data2_message (offset, event_data);
    case 0x23: return get_23_event_data2_message (offset, event_data);
    case 0x28: return get_28_event_data2_message (offset, event_data);
    case 0x2A: return get_2A_event_data2_message (offset, event_data);
    case 0x2B: return get_2B_event_data2_message (offset, event_data);
    case 0x2C: return get_2C_event_data2_message (offset, event_data);
    }
  
  return NULL;
}

char *
ipmi_get_event_data3_message (int sensor_type_code, int offset, uint8_t event_data)
{
  switch (sensor_type_code)
    {
    case 0x08: return get_08_event_data3_message (offset, event_data);
    case 0x0C: return get_0C_event_data3_message (offset, event_data);
    case 0x10: return get_10_event_data3_message (offset, event_data);
    case 0x19: return get_19_event_data3_message (offset, event_data);
    case 0x1D: return get_1D_event_data3_message (offset, event_data);
    case 0x21: return get_21_event_data3_message (offset, event_data);
    case 0x28: return get_28_event_data3_message (offset, event_data);
    case 0x2A: return get_2A_event_data3_message (offset, event_data);
    }
  
  return NULL;
}

char **
ipmi_get_generic_event_message_list (uint8_t event_reading_type, uint16_t sensor_state)
{
  char **event_message_list = NULL;
  char *message_list[16];
  int i = 0;
  uint16_t offset;
  uint16_t bit; 
  
  for (offset = 0; offset < 16; offset++)
    {
      bit = pow (2, offset);
      if (sensor_state & bit)
	{
	  message_list[i] = ipmi_get_generic_event_message (event_reading_type, offset);
	  if (message_list[i])
	    i++;
	}
    }
  
  if (i != 0)
    {
      event_message_list = (char **) malloc (sizeof (char *) * (i + 1));
      for (offset = 0; offset < i; offset++)
	event_message_list[offset] = message_list[offset];
      event_message_list[i] = NULL;
    }
  
  return event_message_list;
}

char **
ipmi_get_event_message_list (int sensor_type_code, uint16_t sensor_state)
{
  char **event_message_list = NULL;
  char *message_list[16];
  int i = 0;
  uint16_t offset;
  uint16_t bit; 
  
  for (offset = 0; offset < 16; offset++)
    {
      bit = pow (2, offset);
      if (sensor_state & bit)
	{
	  message_list[i] = ipmi_get_event_message (sensor_type_code, offset);
	  if (message_list[i])
	    i++;
	}
    }
  
  if (i != 0)
    {
      event_message_list = (char **) malloc (sizeof (char *) * (i + 1));
      for (offset = 0; offset < i; offset++)
	event_message_list[offset] = message_list[offset];
      event_message_list[i] = NULL;
    }
  
  return event_message_list;
}
