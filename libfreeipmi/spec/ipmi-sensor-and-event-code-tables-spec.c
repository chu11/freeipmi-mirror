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

const char * const ipmi_generic_event_reading_type_code_threshold[] =
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
unsigned int ipmi_generic_event_reading_type_code_threshold_max_index = 0x0B;

const char * const ipmi_generic_event_reading_type_code_transition_state[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_transition_state_max_index = 0x02;

const char * const ipmi_generic_event_reading_type_code_state[] =  {
  "State Deasserted",
  "State Asserted",
  NULL
};
unsigned int ipmi_generic_event_reading_type_code_state_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_predictive_failure[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_predictive_failure_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_limit[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_limit_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_performance[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_performance_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_severity[] =
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
unsigned int ipmi_generic_event_reading_type_code_transition_severity_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_device_present[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_device_present_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_device_enabled[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_device_enabled_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_availability[] =
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
unsigned int ipmi_generic_event_reading_type_code_transition_availability_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_redundancy[] =
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
unsigned int ipmi_generic_event_reading_type_code_redundancy_max_index = 0x07;

const char * const ipmi_generic_event_reading_type_code_acpi_power_state[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_acpi_power_state_max_index = 0x03;

/*************************************************
 * Generic Event Reading Strings (SHORT STRINGS) *
 *************************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

const char * const ipmi_generic_event_reading_type_code_threshold_short[] =
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
unsigned int ipmi_generic_event_reading_type_code_threshold_short_max_index = 0x0B;

const char * const ipmi_generic_event_reading_type_code_transition_state_short[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_transition_state_short_max_index = 0x02;

const char * const ipmi_generic_event_reading_type_code_state_short[] = 
  {
    "State Deasserted",
    "State Asserted",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_state_short_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_predictive_failure_short[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_predictive_failure_short_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_limit_short[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_limit_short_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_performance_short[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_performance_short_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_severity_short[] =
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
unsigned int ipmi_generic_event_reading_type_code_transition_severity_short_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_device_present_short[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_device_present_short_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_device_enabled_short[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_device_enabled_short_max_index = 0x01;

const char * const ipmi_generic_event_reading_type_code_transition_availability_short[] =
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
unsigned int ipmi_generic_event_reading_type_code_transition_availability_short_max_index = 0x08;

const char * const ipmi_generic_event_reading_type_code_redundancy_short[] =
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
unsigned int ipmi_generic_event_reading_type_code_redundancy_short_max_index = 0x07;

const char * const ipmi_generic_event_reading_type_code_acpi_power_state_short[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_acpi_power_state_short_max_index = 0x03;

/**************************************
 * Sensor Type Strings (FULL STRINGS) *
 **************************************/

/* achu: 'undock' removed as noted in errata */
const char * const ipmi_sensor_type_physical_security[] =
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
unsigned int ipmi_sensor_type_physical_security_max_index = 0x06;

const char * const ipmi_sensor_type_platform_security_violation_attempt[] =
  {
    "Secure Mode (Front Panel Lockout) Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation attempt - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
unsigned int ipmi_sensor_type_platform_security_violation_attempt_max_index = 0x05;

const char * const ipmi_sensor_type_processor[] =
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
unsigned int ipmi_sensor_type_processor_max_index = 0x0C;

const char * const ipmi_sensor_type_power_supply[] =
  {
    "Presence detected",
    "Power Supply Failure detected",
    "Predictive Failure",
    "Power Supply input lost (AC/DC)",
    "Power Supply input lost or out-of-range",
    "Power Supply input out-of-range, but present",
    "Configuration error",
    "Power Supply Inactive (in standby state)",
    NULL
  };
unsigned int ipmi_sensor_type_power_supply_max_index = 0x07;

const char * const ipmi_sensor_type_power_unit[] =
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
unsigned int ipmi_sensor_type_power_unit_max_index = 0x07;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_memory[] =
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
unsigned int ipmi_sensor_type_memory_max_index = 0x0A;

/* achu: defined in errata */
const char * const ipmi_sensor_type_drive_slot[] =
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
unsigned int ipmi_sensor_type_drive_slot_max_index = 0x08;

const char * const ipmi_sensor_type_system_firmware_progress[] =
  {
    "System Firmware Error (POST Error)",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
unsigned int ipmi_sensor_type_system_firmware_progress_max_index = 0x02;

const char * const ipmi_sensor_type_event_logging_disabled[] =
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
unsigned int ipmi_sensor_type_event_logging_disabled_max_index = 0x06;

const char * const ipmi_sensor_type_watchdog1[] =
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
unsigned int ipmi_sensor_type_watchdog1_max_index = 0x07;

const char * const ipmi_sensor_type_system_event[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
unsigned int ipmi_sensor_type_system_event_max_index = 0x05;

const char * const ipmi_sensor_type_critical_interrupt[] =
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
unsigned int ipmi_sensor_type_critical_interrupt_max_index = 0x0B;

const char * const ipmi_sensor_type_button_switch[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open (Switch indicating FRU latch is in `unlatched' position and FRU is mechanically removable)",
    "FRU service request button (pressed, service, e.g. removal/replacement, requested)",
    NULL
  };
unsigned int ipmi_sensor_type_button_switch_max_index = 0x04;

const char * const ipmi_sensor_type_chip_set[] =
  {
    "Soft Power Control Failure (chipset did not respond to BMC request to change system power state)",
    "Thermal Trip",
    NULL
  };
unsigned int ipmi_sensor_type_chip_set_max_index = 0x01;

const char * const ipmi_sensor_type_cable_interconnect[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected / Incorrect interconnection",
    NULL
  };
unsigned int ipmi_sensor_type_cable_interconnect_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_system_boot_initiated[] =
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
unsigned int ipmi_sensor_type_system_boot_initiated_max_index = 0x07;

const char * const ipmi_sensor_type_boot_error[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
unsigned int ipmi_sensor_type_boot_error_max_index = 0x04;

const char * const ipmi_sensor_type_os_boot[] =
  {
    "A: boot completed",
    "C: boot completed",
    "PXE boot completed",
    "Diagnostic boot completed",
    "CD-ROM boot completed",
    "ROM boot completed",
    "boot completed - boot device not specified",
    "Base OS/Hypervisor Installation started",
    "Base OS/Hypervisor Installation completed",
    "Base OS/Hypervisor Installation aborted",
    "Base OS/Hypervisor Installation failed",
    NULL
  };
unsigned int ipmi_sensor_type_os_boot_max_index = 0x0A;

/* achu: modified per errata */
const char * const ipmi_sensor_type_os_critical_stop[] =
  {
    "Critical stop during OS load / initialization.  Unexpected error during system startup.  Stopped waiting for input or power cycle/reset.",
    "Run-time Critical Stop (a.k.a. 'core dump', 'blue screen')",
    "OS Graceful Stop (system powered up, but normal OS operation has shut down and system is awaiting reset pushbutton, powercycle or other external input)",
    "OS Graceful Shutdown (system graceful power down by OS)",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding.  Graceful shutdown request to agent via BMC did not occur due to missing or malfunctioning local agent.",
    NULL
  };
unsigned int ipmi_sensor_type_os_critical_stop_max_index = 0x05;

const char * const ipmi_sensor_type_slot_connector[] =
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
unsigned int ipmi_sensor_type_slot_connector_max_index = 0x09;

const char * const ipmi_sensor_type_acpi_power_state[] =
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
unsigned int ipmi_sensor_type_acpi_power_state_max_index = 0x0E;

const char * const ipmi_sensor_type_watchdog2[] =
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
unsigned int ipmi_sensor_type_watchdog2_max_index = 0x08;

const char * const ipmi_sensor_type_platform_alert[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated, formatted per IPMI PET specification",
    "platform generated SNMP trap, OEM format",
    NULL
  };
unsigned int ipmi_sensor_type_platform_alert_max_index = 0x03;

const char * const ipmi_sensor_type_entity_presence[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
unsigned int ipmi_sensor_type_entity_presence_max_index = 0x02;

const char * const ipmi_sensor_type_lan[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
unsigned int ipmi_sensor_type_lan_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_management_subsystem_health[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
unsigned int ipmi_sensor_type_management_subsystem_health_max_index = 0x05;

const char * const ipmi_sensor_type_battery[] =
  {
    "battery low (predictive failure)",
    "battery failed",
    "battery presence detected",
    NULL
  };
unsigned int ipmi_sensor_type_battery_max_index = 0x02;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_session_audit[] =
  {
    "Session Activated",
    "Session Deactivated",
    "Invalid Username or Password",
    "Invalid Password Disable",
    NULL
  };
unsigned int ipmi_sensor_type_session_audit_max_index = 0x03;

const char * const ipmi_sensor_type_version_change[] =
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
unsigned int ipmi_sensor_type_version_change_max_index = 0x07;

const char * const ipmi_sensor_type_fru_state[] =
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
unsigned int ipmi_sensor_type_fru_state_max_index = 0x07;

/***************************************
 * Sensor Type Strings (SHORT STRINGS) *
 ***************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

/* achu: 'undock' removed as noted in errata */
const char * const ipmi_sensor_type_physical_security_short[] =
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
unsigned int ipmi_sensor_type_physical_security_short_max_index = 0x06;

const char * const ipmi_sensor_type_platform_security_violation_attempt_short[] =
  {
    "Secure Mode Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
unsigned int ipmi_sensor_type_platform_security_violation_attempt_short_max_index = 0x05;

const char * const ipmi_sensor_type_processor_short[] =
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
unsigned int ipmi_sensor_type_processor_short_max_index = 0x0C;

const char * const ipmi_sensor_type_power_supply_short[] =
  {
    "Presence detected",
    "Power Supply Failure detected",
    "Predictive Failure",
    "Power Supply input lost (AC/DC)",
    "Power Supply input lost or out-of-range",
    "Power Supply input out-of-range, but present",
    "Configuration error",
    "Power Supply Inactive",
    NULL
  };
unsigned int ipmi_sensor_type_power_supply_short_max_index = 0x07;

const char * const ipmi_sensor_type_power_unit_short[] =
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
unsigned int ipmi_sensor_type_power_unit_short_max_index = 0x07;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_memory_short[] =
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
unsigned int ipmi_sensor_type_memory_short_max_index = 0x0A;

/* achu: defined in errata */
const char * const ipmi_sensor_type_drive_slot_short[] =
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
unsigned int ipmi_sensor_type_drive_slot_short_max_index = 0x08;

const char * const ipmi_sensor_type_system_firmware_progress_short[] =
  {
    "System Firmware Error",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
unsigned int ipmi_sensor_type_system_firmware_progress_short_max_index = 0x02;

const char * const ipmi_sensor_type_event_logging_disabled_short[] =
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
unsigned int ipmi_sensor_type_event_logging_disabled_short_max_index = 0x06;

const char * const ipmi_sensor_type_watchdog1_short[] =
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
unsigned int ipmi_sensor_type_watchdog1_short_max_index = 0x07;

const char * const ipmi_sensor_type_system_event_short[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
unsigned int ipmi_sensor_type_system_event_short_max_index = 0x05;

const char * const ipmi_sensor_type_critical_interrupt_short[] =
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
unsigned int ipmi_sensor_type_critical_interrupt_short_max_index = 0x0B;

const char * const ipmi_sensor_type_button_switch_short[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open",
    "FRU service request button",
    NULL
  };
unsigned int ipmi_sensor_type_button_switch_short_max_index = 0x04;

const char * const ipmi_sensor_type_chip_set_short[] =
  {
    "Soft Power Control Failure",
    "Thermal Trip",
    NULL
  };
unsigned int ipmi_sensor_type_chip_set_short_max_index = 0x00;

const char * const ipmi_sensor_type_cable_interconnect_short[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected",
    NULL
  };
unsigned int ipmi_sensor_type_cable_interconnect_short_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_system_boot_initiated_short[] =
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
unsigned int ipmi_sensor_type_system_boot_initiated_short_max_index = 0x07;

const char * const ipmi_sensor_type_boot_error_short[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
unsigned int ipmi_sensor_type_boot_error_short_max_index = 0x04;

const char * const ipmi_sensor_type_os_boot_short[] =
  {
    "A: boot completed",
    "C: boot completed",
    "PXE boot completed",
    "Diagnostic boot completed",
    "CD-ROM boot completed",
    "ROM boot completed",
    "boot completed - boot device not specified",
    "Base OS/Hypervisor Installation started",
    "Base OS/Hypervisor Installation completed",
    "Base OS/Hypervisor Installation aborted",
    "Base OS/Hypervisor Installation failed",
    NULL
  };
unsigned int ipmi_sensor_type_os_boot_short_max_index = 0x0A;

/* achu: modified per errata */
const char * const ipmi_sensor_type_os_critical_stop_short[] =
  {
    "Critical stop during OS load",
    "Run-time Critical Stop",
    "OS Graceful Stop",
    "OS Graceful Shutdown",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding",
    NULL
  };
unsigned int ipmi_sensor_type_os_critical_stop_short_max_index = 0x05;

const char * const ipmi_sensor_type_slot_connector_short[] =
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
unsigned int ipmi_sensor_type_slot_connector_short_max_index = 0x09;

const char * const ipmi_sensor_type_acpi_power_state_short[] =
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
unsigned int ipmi_sensor_type_acpi_power_state_short_max_index = 0x0E;

const char * const ipmi_sensor_type_watchdog2_short[] =
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
unsigned int ipmi_sensor_type_watchdog2_short_max_index = 0x08;

const char * const ipmi_sensor_type_platform_alert_short[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated",
    "platform generated SNMP trap, OEM format",
    NULL
  };
unsigned int ipmi_sensor_type_platform_alert_short_max_index = 0x03;

const char * const ipmi_sensor_type_entity_presence_short[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
unsigned int ipmi_sensor_type_entity_presence_short_max_index = 0x02;

const char * const ipmi_sensor_type_lan_short[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
unsigned int ipmi_sensor_type_lan_short_max_index = 0x01;

/* achu: new additions as stated in errata */
const char * const ipmi_sensor_type_management_subsystem_health_short[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
unsigned int ipmi_sensor_type_management_subsystem_health_short_max_index = 0x05;

const char * const ipmi_sensor_type_battery_short[] =
  {
    "battery low",
    "battery failed",
    "battery presence detected",
    NULL
  };
unsigned int ipmi_sensor_type_battery_short_max_index = 0x02;

const char * const ipmi_sensor_type_session_audit_short[] =
  {
    "Session Activated",
    "Session Deactivated",
    "Invalid Username or Password",
    "Invalid Password Disable",
    NULL
  };
unsigned int ipmi_sensor_type_session_audit_short_max_index = 0x03;

const char * const ipmi_sensor_type_version_change_short[] =
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
unsigned int ipmi_sensor_type_version_change_short_max_index = 0x07;

const char * const ipmi_sensor_type_fru_state_short[] =
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
unsigned int ipmi_sensor_type_fru_state_short_max_index = 0x07;

/*******************************************************
 * Sensor Type Strings for Event Data 2 (FULL STRINGS) *
 *******************************************************/

const char * const ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_error[] =
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
unsigned int ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_error_max_index = 0x0D;

const char * const ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_hang[] =
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
    "Starting operating system boot process, e.g. calling Unsigned Int 19h",
    "Baseboard or motherboard initialization",
    "reserved",
    "Floppy initialization",
    "Keyboard test",
    "Pointing device test",
    "Primary processor initialization",
    NULL
  };
unsigned int ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_hang_max_index = 0x19;

const char * const ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_progress[] =
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
    "Starting operating system boot process, e.g. calling Unsigned Int 19h",
    "Baseboard or motherboard initialization",
    "reserved",
    "Floppy initialization",
    "Keyboard test",
    "Pointing device test",
    "Primary processor initialization",
    NULL
  };
unsigned int ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_progress_max_index = 0x19;

const char * const ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_entry_action[] =
  {
    "Log entry action = entry added",
    "Log entry action = entry added because event did not be map to standard IPMI event",
    "Log entry action = entry added along with one or more corresponding SEL entries",
    "Log entry action = log cleared",
    "Log entry action = log disabled",
    "Log entry action = log enabled",
    NULL
  };
unsigned int ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_entry_action_max_index = 0x05;

const char * const ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_type[] =
  {
    "Log Type = MCA log",
    "Log Type = OEM1",
    "Log Type = OEM2",
    NULL
  };
unsigned int ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_type_max_index = 0x02;

const char * const ipmi_sensor_type_system_event_event_data2_offset_pef_action[] =
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
unsigned int ipmi_sensor_type_system_event_event_data2_offset_pef_action_max_index = 0x05;
#endif

const char * const ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_first_second[] =
  {
    "event is first of pair",
    "event is second of pair",
    NULL,
  };
unsigned int ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_first_second_max_index = 0x01;

const char * const ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_timestamp_clock_type[] =
  {
    "SEL Timestamp Clock updated (Also used when both SEL and SDR Timestamp clocks are linked together)",
    "SDR Timestamp Clock updated",
    NULL,
  };
unsigned int ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_timestamp_clock_type_max_index = 0x01;

const char * const ipmi_sensor_type_chip_set_event_data2_offset_soft_power_control_failure[] =
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
unsigned int ipmi_sensor_type_chip_set_event_data2_offset_soft_power_control_failure_max_index = 0x0C;

const char * const ipmi_sensor_type_system_boot_initiated_event_data2_offset_system_restart_restart_cause[] =
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
unsigned int ipmi_sensor_type_system_boot_initiated_event_data2_offset_system_restart_restart_cause_max_index = 0x0B;

const char * const ipmi_sensor_type_slot_connector_event_data2_offset_slot_holds_spare_device_slot_connector_type[] =
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
unsigned int ipmi_sensor_type_slot_connector_event_data2_offset_slot_holds_spare_device_slot_connector_type_max_index = 0x0C;

const char * const ipmi_sensor_type_watchdog2_event_data2_interrupt_type[] =
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
unsigned int ipmi_sensor_type_watchdog2_event_data2_interrupt_type_max_index = 0x0F;

const char * const ipmi_sensor_type_watchdog2_event_data2_timer_use_at_expiration[] =
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
unsigned int ipmi_sensor_type_watchdog2_event_data2_timer_use_at_expiration_max_index = 0x0F;

const char * const ipmi_sensor_type_management_subsystem_health_event_data2_offset_fru_failure_logical_fru_device[] =
  {
    "device is not a logical FRU Device",
    "device is logical FRU Device (accessed via FRU commands to mgmt. controller",
    NULL
  };
unsigned int ipmi_sensor_type_management_subsystem_health_event_data2_offset_fru_failure_logical_fru_device_max_index = 0x01;

const char * const ipmi_sensor_type_version_change_event_data2_offset_software_or_fw_change_detected_with_associated_entity_was_successful_version_change_type[] =
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
unsigned int ipmi_sensor_type_version_change_event_data2_offset_software_or_fw_change_detected_with_associated_entity_was_successful_version_change_type_max_index = 0x17;

const char * const ipmi_sensor_type_fru_state_event_data2_offset_communication_lost_cause_of_state_change[] =
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
unsigned int ipmi_sensor_type_fru_state_event_data2_offset_communication_lost_cause_of_state_change_max_index = 0x0F;

/*******************************************************
 * Sensor Type Strings for Event Data 3 (FULL STRINGS) *
 *******************************************************/

const char * const ipmi_sensor_type_power_supply_event_data3_offset_configuration_error_error_type[] =
  {
    "Vendor mismatch",
    "Revision mismatch",
    "Processor missing or unexpected/unsupported condition",
    "Power Supply rating mismatch",
    "Voltage rating mismatch",
    NULL
  };
unsigned int ipmi_sensor_type_power_supply_event_data3_offset_configuration_error_error_type_max_index = 0x04;

const char * const ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_assertion_event[] =
  {
    "deassertion event",
    "assertion event",
    NULL
  };
unsigned int ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_assertion_event_max_index = 0x01;

const char * const ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_logging_disabled_all_events[] =
  {
    "",
    "logging has been disabled for all events of given type",
    NULL
  };
unsigned int ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_logging_disabled_all_events_max_index = 0x01;

const char * const ipmi_sensor_type_chip_set_event_data3_offset_soft_power_control_failure[] =
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
unsigned int ipmi_sensor_type_chip_set_event_data3_offset_soft_power_control_failure_max_index = 0x0D;

const char * const ipmi_sensor_type_session_audit_event_data3_offset_session_deactivated_deactivation_cause[] =
  {
    "Session deactivatation cause unspecified. This value is also used for Session Activated events",
    "Session deactivated by Close Session command",
    "Session deactivated by timeout",
    "Session deactivated by configuration change",
    NULL
  };
unsigned int ipmi_sensor_type_session_audit_event_data3_offset_session_deactivated_deactivation_cause_max_index = 0x03;
