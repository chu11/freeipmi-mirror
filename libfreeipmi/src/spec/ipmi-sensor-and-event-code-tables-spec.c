/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

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
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/************************************************
 * Generic Event Reading Strings (FULL STRINGS) *
 ************************************************/

const char * const ipmi_generic_event_reading_type_code_threshold_strings[] =
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
int ipmi_generic_event_reading_type_code_threshold_strings_max_index = 0x0B;

const char * const ipmi_generic_event_reading_type_code_transition_state_strings[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
int ipmi_generic_event_reading_type_code_transition_state_strings_max_index = 0x02;

const char * const ipmi_generic_event_reading_type_code_state_strings[] =  {
  "State Deasserted",
  "State Asserted",
  NULL
};
int ipmi_generic_event_reading_type_code_state_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_predictive_failure_strings[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
int ipmi_generic_event_reading_type_code_predictive_failure_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_limit_strings[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
int ipmi_generic_event_reading_type_code_limit_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_performance_strings[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
int ipmi_generic_event_reading_type_code_performance_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_severity_strings[] =
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
int ipmi_generic_event_reading_type_code_transition_severity_strings_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_device_present_strings[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
int ipmi_generic_event_reading_type_code_device_present_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_device_enabled_strings[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
int ipmi_generic_event_reading_type_code_device_enabled_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_availability_strings[] =
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
int ipmi_generic_event_reading_type_code_transition_availability_strings_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_redundancy_strings[] =
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
int ipmi_generic_event_reading_type_code_redundancy_strings_max_index = 0x07;

const char * const ipmi_generic_event_reading_type_code_acpi_power_state_strings[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
int ipmi_generic_event_reading_type_code_acpi_power_state_strings_max_index = 0x03;

/*************************************************
 * Generic Event Reading Strings (SHORT STRINGS) *
 *************************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

const char * const ipmi_generic_event_reading_type_code_threshold_short_strings[] =
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
int ipmi_generic_event_reading_type_code_threshold_short_strings_max_index = 0x0B;

const char * const ipmi_generic_event_reading_type_code_transition_state_short_strings[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
int ipmi_generic_event_reading_type_code_transition_state_short_strings_max_index = 0x02;

const char * const ipmi_generic_event_reading_type_code_state_short_strings[] =  {
  "State Deasserted",
  "State Asserted",
  NULL
};
int ipmi_generic_event_reading_type_code_state_short_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_predictive_failure_short_strings[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
int ipmi_generic_event_reading_type_code_predictive_failure_short_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_limit_short_strings[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
int ipmi_generic_event_reading_type_code_limit_short_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_performance_short_strings[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
int ipmi_generic_event_reading_type_code_performance_short_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_severity_short_strings[] =
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
int ipmi_generic_event_reading_type_code_transition_severity_short_strings_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_device_present_short_strings[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
int ipmi_generic_event_reading_type_code_device_present_short_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_device_enabled_short_strings[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
int ipmi_generic_event_reading_type_code_device_enabled_short_strings_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_availability_short_strings[] =
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
int ipmi_generic_event_reading_type_code_transition_availability_short_strings_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_redundancy_short_strings[] =
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
int ipmi_generic_event_reading_type_code_redundancy_short_strings_max_index = 0x07;

const char * const ipmi_generic_event_reading_type_code_acpi_power_state_short_strings[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
int ipmi_generic_event_reading_type_code_acpi_power_state_short_strings_max_index = 0x03;

/**************************************
 * Sensor Type Strings (FULL STRINGS) *
 **************************************/

/* achu: 'undock' removed as noted in errata */
const char * const ipmi_sensor_type_physical_security_strings[] =
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
int ipmi_sensor_type_physical_security_strings_max_index = 0x06;

const char * const ipmi_sensor_type_platform_security_violation_attempt_strings[] =
  {
    "Secure Mode (Front Panel Lockout) Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation attempt - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
int ipmi_sensor_type_platform_security_violation_attempt_strings_max_index = 0x05;

const char * const ipmi_sensor_type_processor_strings[] =
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
    "Machine Check Exception (Uncorrectable)",
    "Correctable Machine Check Error",
    NULL
  };
int ipmi_sensor_type_processor_strings_max_index = 0x0C;

const char * const ipmi_sensor_type_power_supply_strings[] =
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
int ipmi_sensor_type_power_supply_strings_max_index = 0x06;

const char * const ipmi_sensor_type_power_unit_strings[] =
  {
    "Power Off/Power Down",
    "Power Cycle",
    "240VA Power Down",
    "Interlock Power Down",
    "AC lost/Power input lost (The power source for the power unit was lost)",
    "Soft Power Control Failure (unit did not respond to request to turn on)",
    "Power Unit Failure detected",
    "Predictive Failure",
    NULL
  };
int ipmi_sensor_type_power_unit_strings_max_index = 0x07;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_memory_strings[] =
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
int ipmi_sensor_type_memory_strings_max_index = 0x0A;

/* achu: defined in errata */
const char * const ipmi_sensor_type_drive_slot_strings[] =
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
int ipmi_sensor_type_drive_slot_strings_max_index = 0x08;

const char * const ipmi_sensor_type_system_firmware_progress_strings[] =
  {
    "System Firmware Error (POST Error)",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
int ipmi_sensor_type_system_firmware_progress_strings_max_index = 0x02;

const char * const ipmi_sensor_type_event_logging_disabled_strings[] =
  {
    "Correctable Memory Error Logging Disabled",
    "Event `Type' Logging Disabled",
    "Log Area Reset/Cleared",
    "All Event Logging Disabled",
    "SEL Full",
    "SEL Almost Full",
    "Correctable Machine Check Error Logging Disabled",
    NULL
  };
int ipmi_sensor_type_event_logging_disabled_strings_max_index = 0x06;

const char * const ipmi_sensor_type_watchdog1_strings[] =
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
int ipmi_sensor_type_watchdog1_strings_max_index = 0x07;

const char * const ipmi_sensor_type_system_event_strings[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
int ipmi_sensor_type_system_event_strings_max_index = 0x05;

const char * const ipmi_sensor_type_critical_interrupt_strings[] =
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
int ipmi_sensor_type_critical_interrupt_strings_max_index = 0x0B;

const char * const ipmi_sensor_type_button_switch_strings[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open (Switch indicating FRU latch is in `unlatched' position and FRU is mechanically removable)",
    "FRU service request button (pressed, service, e.g. removal/replacement, requested)",
    NULL
  };
int ipmi_sensor_type_button_switch_strings_max_index = 0x04;

const char * const ipmi_sensor_type_chip_set_strings[] =
  {
    "Soft Power Control Failure (chipset did not respond to BMC request to change system power state)",
    "Thermal Trip",
    NULL
  };
int ipmi_sensor_type_chip_set_strings_max_index = 0x01;

const char * const ipmi_sensor_type_cable_interconnect_strings[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected / Incorrect interconnection",
    NULL
  };
int ipmi_sensor_type_cable_interconnect_strings_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_system_boot_initiated_strings[] =
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
int ipmi_sensor_type_system_boot_initiated_strings_max_index = 0x07;

const char * const ipmi_sensor_type_boot_error_strings[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
int ipmi_sensor_type_boot_error_strings_max_index = 0x04;

const char * const ipmi_sensor_type_os_boot_strings[] =
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
int ipmi_sensor_type_os_boot_strings_max_index = 0x06;

/* achu: modified per errata */
const char * const ipmi_sensor_type_os_critical_stop_strings[] =
  {
    "Critical stop during OS load / initialization.  Unexpected error during system startup.  Stopped waiting for input or power cycle/reset.",
    "Run-time Critical Stop (a.k.a. 'core dump', 'blue screen')",
    "OS Graceful Stop (system powered up, but normal OS operation has shut down and system is awaiting reset pushbutton, powercycle or other external input)",
    "OS Graceful Shutdown (system graceful power down by OS)",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding.  Graceful shutdown request to agent via BMC did not occur due to missing or malfunctioning local agent.",
    NULL
  };
int ipmi_sensor_type_os_critical_stop_strings_max_index = 0x05;

const char * const ipmi_sensor_type_slot_connector_strings[] =
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
int ipmi_sensor_type_slot_connector_strings_max_index = 0x09;

const char * const ipmi_sensor_type_acpi_power_state_strings[] =
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
int ipmi_sensor_type_acpi_power_state_strings_max_index = 0x0E;

const char * const ipmi_sensor_type_watchdog2_strings[] =
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
int ipmi_sensor_type_watchdog2_strings_max_index = 0x08;

const char * const ipmi_sensor_type_platform_alert_strings[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated, formatted per IPMI PET specification",
    "platform generated SNMP trap, OEM format",
    NULL
  };
int ipmi_sensor_type_platform_alert_strings_max_index = 0x03;

const char * const ipmi_sensor_type_entity_presence_strings[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
int ipmi_sensor_type_entity_presence_strings_max_index = 0x02;

const char * const ipmi_sensor_type_lan_strings[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
int ipmi_sensor_type_lan_strings_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_management_subsystem_health_strings[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
int ipmi_sensor_type_management_subsystem_health_strings_max_index = 0x05;

const char * const ipmi_sensor_type_battery_strings[] =
  {
    "battery low (predictive failure)",
    "battery failed",
    "battery presence detected",
    NULL
  };
int ipmi_sensor_type_battery_strings_max_index = 0x02;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_session_audit_strings[] =
  {
    "Session Activated",
    "Session Deactivated",
    "Invalid Username or Password",
    "Invalid Password Disable",
    NULL
  };
int ipmi_sensor_type_session_audit_strings_max_index = 0x03;

const char * const ipmi_sensor_type_version_change_strings[] =
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
int ipmi_sensor_type_version_change_strings_max_index = 0x07;

const char * const ipmi_sensor_type_fru_state_strings[] =
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
int ipmi_sensor_type_fru_state_strings_max_index = 0x07;

/***************************************
 * Sensor Type Strings (SHORT STRINGS) *
 ***************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

/* achu: 'undock' removed as noted in errata */
const char * const ipmi_sensor_type_physical_security_short_strings[] =
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
int ipmi_sensor_type_physical_security_short_strings_max_index = 0x06;

const char * const ipmi_sensor_type_platform_security_violation_attempt_short_strings[] =
  {
    "Secure Mode Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
int ipmi_sensor_type_platform_security_violation_attempt_short_strings_max_index = 0x05;

const char * const ipmi_sensor_type_processor_short_strings[] =
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
    "Machine Check Exception",
    "Correctable Machine Check Error",
    NULL
  };
int ipmi_sensor_type_processor_short_strings_max_index = 0x0C;

const char * const ipmi_sensor_type_power_supply_short_strings[] =
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
int ipmi_sensor_type_power_supply_short_strings_max_index = 0x06;

const char * const ipmi_sensor_type_power_unit_short_strings[] =
  {
    "Power Off/Power Down",
    "Power Cycle",
    "240VA Power Down",
    "Interlock Power Down",
    "AC lost/Power input lost",
    "Soft Power Control Failure",
    "Power Unit Failure detected",
    "Predictive Failure",
    NULL
  };
int ipmi_sensor_type_power_unit_short_strings_max_index = 0x07;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_memory_short_strings[] =
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
int ipmi_sensor_type_memory_short_strings_max_index = 0x0A;

/* achu: defined in errata */
const char * const ipmi_sensor_type_drive_slot_short_strings[] =
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
int ipmi_sensor_type_drive_slot_short_strings_max_index = 0x08;

const char * const ipmi_sensor_type_system_firmware_progress_short_strings[] =
  {
    "System Firmware Error",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
int ipmi_sensor_type_system_firmware_progress_short_strings_max_index = 0x02;

const char * const ipmi_sensor_type_event_logging_disabled_short_strings[] =
  {
    "Correctable Memory Error Logging Disabled",
    "Event Type Logging Disabled",
    "Log Area Reset/Cleared",
    "All Event Logging Disabled",
    "SEL Full",
    "SEL Almost Full",
    "Correctable Machine Check Error Logging Disabled",
    NULL
  };
int ipmi_sensor_type_event_logging_disabled_short_strings_max_index = 0x06;

const char * const ipmi_sensor_type_watchdog1_short_strings[] =
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
int ipmi_sensor_type_watchdog1_short_strings_max_index = 0x07;

const char * const ipmi_sensor_type_system_event_short_strings[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
int ipmi_sensor_type_system_event_short_strings_max_index = 0x05;

const char * const ipmi_sensor_type_critical_interrupt_short_strings[] =
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
int ipmi_sensor_type_critical_interrupt_short_strings_max_index = 0x0B;

const char * const ipmi_sensor_type_button_switch_short_strings[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open",
    "FRU service request button",
    NULL
  };
int ipmi_sensor_type_button_switch_short_strings_max_index = 0x04;

const char * const ipmi_sensor_type_chip_set_short_strings[] =
  {
    "Soft Power Control Failure",
    "Thermal Trip",
    NULL
  };
int ipmi_sensor_type_chip_set_short_strings_max_index = 0x00;

const char * const ipmi_sensor_type_cable_interconnect_short_strings[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected",
    NULL
  };
int ipmi_sensor_type_cable_interconnect_short_strings_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_system_boot_initiated_short_strings[] =
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
int ipmi_sensor_type_system_boot_initiated_short_strings_max_index = 0x07;

const char * const ipmi_sensor_type_boot_error_short_strings[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
int ipmi_sensor_type_boot_error_short_strings_max_index = 0x04;

const char * const ipmi_sensor_type_os_boot_short_strings[] =
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
int ipmi_sensor_type_os_boot_short_strings_max_index = 0x06;

/* achu: modified per errata */
const char * const ipmi_sensor_type_os_critical_stop_short_strings[] =
  {
    "Critical stop during OS load",
    "Run-time Critical Stop",
    "OS Graceful Stop",
    "OS Graceful Shutdown",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding",
    NULL
  };
int ipmi_sensor_type_os_critical_stop_short_strings_max_index = 0x05;

const char * const ipmi_sensor_type_slot_connector_short_strings[] =
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
int ipmi_sensor_type_slot_connector_short_strings_max_index = 0x09;

const char * const ipmi_sensor_type_acpi_power_state_short_strings[] =
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
int ipmi_sensor_type_acpi_power_state_short_strings_max_index = 0x0E;

const char * const ipmi_sensor_type_watchdog2_short_strings[] =
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
int ipmi_sensor_type_watchdog2_short_strings_max_index = 0x08;

const char * const ipmi_sensor_type_platform_alert_short_strings[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated",
    "platform generated SNMP trap, OEM format",
    NULL
  };
int ipmi_sensor_type_platform_alert_short_strings_max_index = 0x03;

const char * const ipmi_sensor_type_entity_presence_short_strings[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
int ipmi_sensor_type_entity_presence_short_strings_max_index = 0x02;

const char * const ipmi_sensor_type_lan_short_strings[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
int ipmi_sensor_type_lan_short_strings_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_management_subsystem_health_short_strings[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
int ipmi_sensor_type_management_subsystem_health_short_strings_max_index = 0x05;

const char * const ipmi_sensor_type_battery_short_strings[] =
  {
    "battery low",
    "battery failed",
    "battery presence detected",
    NULL
  };
int ipmi_sensor_type_battery_short_strings_max_index = 0x02;

const char * const ipmi_sensor_type_session_audit_short_strings[] =
  {
    "Session Activated",
    "Session Deactivated",
    "Invalid Username of Password",
    "Invalid Password Disable",
    NULL
  };
int ipmi_sensor_type_session_audit_short_strings_max_index = 0x03;

const char * const ipmi_sensor_type_version_change_short_strings[] =
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
int ipmi_sensor_type_version_change_short_strings_max_index = 0x07;

const char * const ipmi_sensor_type_fru_state_short_strings[] =
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
int ipmi_sensor_type_fru_state_short_strings_max_index = 0x07;

/*******************************************************
 * Sensor Type Strings for Event Data 2 (FULL STRINGS) *
 *******************************************************/

const char * const ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_error_strings[] =
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
int ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_error_strings_max_index = 0x0D;

const char * const ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_hang_strings[] =
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
int ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_hang_strings_max_index = 0x19;

const char * const ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_progress_strings[] =
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
int ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_progress_strings_max_index = 0x19;

const char * const ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_entry_action_strings[] =
  {
    "Log entry action = entry added",
    "Log entry action = entry added because event did not be map to standard IPMI event",
    "Log entry action = entry added along with one or more corresponding SEL entries",
    "Log entry action = log cleared",
    "Log entry action = log disabled",
    "Log entry action = log enabled",
    NULL
  };
int ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_entry_action_strings_max_index = 0x05;

const char * const ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_type_strings[] =
  {
    "Log Type = MCA log",
    "Log Type = OEM1",
    "Log Type = OEM2",
    NULL
  };
int ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_type_strings_max_index = 0x02;

const char * const ipmi_sensor_type_system_event_event_data2_offset_pef_action_strings[] =
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
int ipmi_sensor_type_system_event_event_data2_offset_pef_action_strings_max_index = 0x05;
#endif

const char * const ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_first_second_strings[] =
  {
    "event is first of pair",
    "event is second of pair",
    NULL,
  };
int ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_first_second_strings_max_index = 0x01;

const char * const ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_timestamp_clock_type_strings[] =
  {
    "SEL Timestamp Clock updated (Also used when both SEL and SDR Timestamp clocks are linked together)",
    "SDR Timestamp Clock updated",
    NULL,
  };
int ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_timestamp_clock_type_strings_max_index = 0x01;

const char * const ipmi_sensor_type_chip_set_event_data2_offset_soft_power_control_failure_strings[] =
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
int ipmi_sensor_type_chip_set_event_data2_offset_soft_power_control_failure_strings_max_index = 0x0C;

const char * const ipmi_sensor_type_system_boot_initiated_event_data2_offset_system_restart_restart_cause_strings[] =
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
int ipmi_sensor_type_system_boot_initiated_event_data2_offset_system_restart_restart_cause_strings_max_index = 0x0B;

const char * const ipmi_sensor_type_slot_connector_event_data2_offset_slot_holds_spare_device_slot_connector_type_strings[] =
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
    "Slot/Connector Type = PCI Express",
    "Slot/Connector Type = SCSI (parallel)",
    "Slot/Connector Type = SATA / SAS",
    "Slot/Connector Type = USB",
    NULL
  };
int ipmi_sensor_type_slot_connector_event_data2_offset_slot_holds_spare_device_slot_connector_type_strings_max_index = 0x0C;

const char * const ipmi_sensor_type_watchdog2_event_data2_interrupt_type_strings[] =
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
int ipmi_sensor_type_watchdog2_event_data2_interrupt_type_strings_max_index = 0x0F;

const char * const ipmi_sensor_type_watchdog2_event_data2_timer_use_at_expiration_strings[] =
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
int ipmi_sensor_type_watchdog2_event_data2_timer_use_at_expiration_strings_max_index = 0x0F;

const char * const ipmi_sensor_type_management_subsystem_health_event_data2_offset_fru_failure_logical_fru_device_strings[] =
  {
    "device is not a logical FRU Device",
    "device is logical FRU Device (accessed via FRU commands to mgmt. controller",
    NULL
  };
int ipmi_sensor_type_management_subsystem_health_event_data2_offset_fru_failure_logical_fru_device_strings_max_index = 0x01;

const char * const ipmi_sensor_type_version_change_event_data2_offset_software_or_fw_change_detected_with_associated_entity_was_successful_version_change_type_strings[] =
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
int ipmi_sensor_type_version_change_event_data2_offset_software_or_fw_change_detected_with_associated_entity_was_successful_version_change_type_strings_max_index = 0x17;

const char * const ipmi_sensor_type_fru_state_event_data2_offset_communication_lost_cause_of_state_change_strings[] =
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
int ipmi_sensor_type_fru_state_event_data2_offset_communication_lost_cause_of_state_change_strings_max_index = 0x0F;

/*******************************************************
 * Sensor Type Strings for Event Data 3 (FULL STRINGS) *
 *******************************************************/

const char * const ipmi_sensor_type_power_supply_event_data3_offset_configuration_error_error_type_strings[] =
  {
    "Vendor mismatch",
    "Revision mismatch",
    "Processor missing or unexpected/unsupported condition",
    "Power Supply rating mismatch",
    "Voltage rating mismatch",
    NULL
  };
int ipmi_sensor_type_power_supply_event_data3_offset_configuration_error_error_type_strings_max_index = 0x04;

const char * const ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_assertion_event_strings[] =
  {
    "deassertion event",
    "assertion event",
    NULL
  };
int ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_assertion_event_strings_max_index = 0x01;

const char * const ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_logging_disabled_all_events_strings[] =
  {
    "",
    "logging has been disabled for all events of given type",
    NULL
  };
int ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_logging_disabled_all_events_strings_max_index = 0x01;

const char * const ipmi_sensor_type_chip_set_event_data3_offset_soft_power_control_failure_strings[] =
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
int ipmi_sensor_type_chip_set_event_data3_offset_soft_power_control_failure_strings_max_index = 0x0D;

const char * const ipmi_sensor_type_session_audit_event_data3_offset_session_deactivated_deactivation_cause_strings[] =
  {
    "Session deactivatation cause unspecified. This value is also used for Session Activated events",
    "Session deactivated by Close Session command",
    "Session deactivated by timeout",
    "Session deactivated by configuration change",
    NULL
  };
int ipmi_sensor_type_session_audit_event_data3_offset_session_deactivated_deactivation_cause_strings_max_index = 0x03;
