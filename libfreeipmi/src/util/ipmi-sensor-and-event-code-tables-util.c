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
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

int
ipmi_event_reading_type_code_class (uint8_t event_reading_type_code)
{
  if (IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD);

  if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE);

  if (IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE);

  if (IPMI_EVENT_READING_TYPE_CODE_IS_OEM (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM);

  return (IPMI_EVENT_READING_TYPE_CODE_CLASS_UNKNOWN);
}

/************************************************
 * Generic Event Reading Strings (FULL STRINGS) *
 ************************************************/

static char * ipmi_generic_event_reading_type_code_threshold_desc[] =
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
static int ipmi_generic_event_reading_type_code_threshold_desc_max = 0x0B;

static char * ipmi_generic_event_reading_type_code_transition_state_desc[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
static int ipmi_generic_event_reading_type_code_transition_state_desc_max = 0x02;

static char * ipmi_generic_event_reading_type_code_state_desc[] =  {
  "State Deasserted",
  "State Asserted",
  NULL
};
static int ipmi_generic_event_reading_type_code_state_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_predictive_failure_desc[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
static int ipmi_generic_event_reading_type_code_predictive_failure_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_limit_desc[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
static int ipmi_generic_event_reading_type_code_limit_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_performance_desc[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
static int ipmi_generic_event_reading_type_code_performance_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_transition_severity_desc[] =
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
static int ipmi_generic_event_reading_type_code_transition_severity_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_device_present_desc[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
static int ipmi_generic_event_reading_type_code_device_present_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_device_enabled_desc[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
static int ipmi_generic_event_reading_type_code_device_enabled_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_transition_availability_desc[] =
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
static int ipmi_generic_event_reading_type_code_transition_availability_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_redundancy_desc[] =
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
static int ipmi_generic_event_reading_type_code_redundancy_desc_max = 0x07;

static char * ipmi_generic_event_reading_type_code_acpi_power_state_desc[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
static int ipmi_generic_event_reading_type_code_acpi_power_state_desc_max = 0x03;

/**************************************
 * Sensor Type Strings (FULL STRINGS) *
 **************************************/

/* achu: 'undock' removed as noted in errata */
static char * ipmi_sensor_type_code_physical_security_desc[] =
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
static int ipmi_sensor_type_code_physical_security_desc_max = 0x06;

static char * ipmi_sensor_type_code_platform_security_violation_attempt_desc[] =
  {
    "Secure Mode (Front Panel Lockout) Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation attempt - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
static int ipmi_sensor_type_code_platform_security_violation_attempt_desc_max = 0x05;

static char * ipmi_sensor_type_code_processor_desc[] =
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
static int ipmi_sensor_type_code_processor_desc_max = 0x0C;

static char * ipmi_sensor_type_code_power_supply_desc[] =
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
static int ipmi_sensor_type_code_power_supply_desc_max = 0x06;

static char * ipmi_sensor_type_code_power_unit_desc[] =
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
static int ipmi_sensor_type_code_power_unit_desc_max = 0x07;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_memory_desc[] =
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
static int ipmi_sensor_type_code_memory_desc_max = 0x0A;

/* achu: defined in errata */
static char * ipmi_sensor_type_code_drive_slot_desc[] =
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
static int ipmi_sensor_type_code_drive_slot_desc_max = 0x08;

static char * ipmi_sensor_type_code_system_firmware_progress_desc[] =
  {
    "System Firmware Error (POST Error)",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
static int ipmi_sensor_type_code_system_firmware_progress_desc_max = 0x02;

static char * ipmi_sensor_type_code_event_logging_disabled_desc[] =
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
static int ipmi_sensor_type_code_event_logging_disabled_desc_max = 0x06;

static char * ipmi_sensor_type_code_watchdog1_desc[] =
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
static int ipmi_sensor_type_code_watchdog1_desc_max = 0x07;

static char * ipmi_sensor_type_code_system_event_desc[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
static int ipmi_sensor_type_code_system_event_desc_max = 0x05;

static char * ipmi_sensor_type_code_critical_interrupt_desc[] =
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
static int ipmi_sensor_type_code_critical_interrupt_desc_max = 0x0B;

static char * ipmi_sensor_type_code_button_switch_desc[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open (Switch indicating FRU latch is in `unlatched' position and FRU is mechanically removable)",
    "FRU service request button (pressed, service, e.g. removal/replacement, requested)",
    NULL
  };
static int ipmi_sensor_type_code_button_switch_desc_max = 0x04;

static char * ipmi_sensor_type_code_chip_set_desc[] =
  {
    "Soft Power Control Failure (chipset did not respond to BMC request to change system power state)",
    "Thermal Trip",
    NULL
  };
static int ipmi_sensor_type_code_chip_set_desc_max = 0x01;

static char * ipmi_sensor_type_code_cable_interconnect_desc[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected / Incorrect interconnection",
    NULL
  };
static int ipmi_sensor_type_code_cable_interconnect_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_system_boot_initiated_desc[] =
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
static int ipmi_sensor_type_code_system_boot_initiated_desc_max = 0x07;

static char * ipmi_sensor_type_code_boot_error_desc[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
static int ipmi_sensor_type_code_boot_error_desc_max = 0x04;

static char * ipmi_sensor_type_code_os_boot_desc[] =
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
static int ipmi_sensor_type_code_os_boot_desc_max = 0x06;

/* achu: modified per errata */
static char * ipmi_sensor_type_code_os_critical_stop_desc[] =
  {
    "Critical stop during OS load / initialization.  Unexpected error during system startup.  Stopped waiting for input or power cycle/reset.",
    "Run-time Critical Stop (a.k.a. 'core dump', 'blue screen')",
    "OS Graceful Stop (system powered up, but normal OS operation has shut down and system is awaiting reset pushbutton, powercycle or other external input)",
    "OS Graceful Shutdown (system graceful power down by OS)",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding.  Graceful shutdown request to agent via BMC did not occur due to missing or malfunctioning local agent.",
    NULL
  };
static int ipmi_sensor_type_code_os_critical_stop_desc_max = 0x05;

static char * ipmi_sensor_type_code_slot_connector_desc[] =
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
static int ipmi_sensor_type_code_slot_connector_desc_max = 0x09;

static char * ipmi_sensor_type_code_acpi_power_state_desc[] =
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
static int ipmi_sensor_type_code_acpi_power_state_desc_max = 0x0E;

static char * ipmi_sensor_type_code_watchdog2_desc[] =
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
static int ipmi_sensor_type_code_watchdog2_desc_max = 0x08;

static char * ipmi_sensor_type_code_platform_alert_desc[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated, formatted per IPMI PET specification",
    "platform generated SNMP trap, OEM format",
    NULL
  };
static int ipmi_sensor_type_code_platform_alert_desc_max = 0x03;

static char * ipmi_sensor_type_code_entity_presence_desc[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
static int ipmi_sensor_type_code_entity_presence_desc_max = 0x02;

static char * ipmi_sensor_type_code_lan_desc[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
static int ipmi_sensor_type_code_lan_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_management_subsystem_health_desc[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
static int ipmi_sensor_type_code_management_subsystem_health_desc_max = 0x05;

static char * ipmi_sensor_type_code_battery_desc[] =
  {
    "battery low (predictive failure)",
    "battery failed",
    "battery presence detected",
    NULL
  };
static int ipmi_sensor_type_code_battery_desc_max = 0x02;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_session_audit_desc[] =
  {
    "Session Activated",
    "Session Deactivated",
    "Invalid Username of Password",
    NULL
  };
static int ipmi_sensor_type_code_session_audit_desc_max = 0x02;

static char * ipmi_sensor_type_code_version_change_desc[] =
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
static int ipmi_sensor_type_code_version_change_desc_max = 0x07;

static char * ipmi_sensor_type_code_fru_state_desc[] =
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
static int ipmi_sensor_type_code_fru_state_desc_max = 0x07;

/*************************************************
 * Generic Event Reading Strings (SHORT STRINGS) *
 *************************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

static char * ipmi_generic_event_reading_type_code_threshold_short_desc[] =
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
static int ipmi_generic_event_reading_type_code_threshold_short_desc_max = 0x0B;

static char * ipmi_generic_event_reading_type_code_transition_state_short_desc[] =
  {
    "Transition to Idle",
    "Transition to Active",
    "Transition to Busy",
    NULL
  };
static int ipmi_generic_event_reading_type_code_transition_state_short_desc_max = 0x02;

static char * ipmi_generic_event_reading_type_code_state_short_desc[] =  {
  "State Deasserted",
  "State Asserted",
  NULL
};
static int ipmi_generic_event_reading_type_code_state_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_predictive_failure_short_desc[] =
  {
    "Predictive Failure deasserted",
    "Predictive Failure asserted",
    NULL
  };
static int ipmi_generic_event_reading_type_code_predictive_failure_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_limit_short_desc[] =
  {
    "Limit Not Exceeded",
    "Limit Exceeded",
    NULL
  };
static int ipmi_generic_event_reading_type_code_limit_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_performance_short_desc[] =
  {
    "Performance Met",
    "Performance Lags",
    NULL
  };
static int ipmi_generic_event_reading_type_code_performance_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_transition_severity_short_desc[] =
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
static int ipmi_generic_event_reading_type_code_transition_severity_short_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_device_present_short_desc[] =
  {
    "Device Removed/Device Absent",
    "Device Inserted/Device Present",
    NULL
  };
static int ipmi_generic_event_reading_type_code_device_present_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_device_enabled_short_desc[] =
  {
    "Device Disabled",
    "Device Enabled",
    NULL
  };
static int ipmi_generic_event_reading_type_code_device_enabled_short_desc_max = 0x01;

static char * ipmi_generic_event_reading_type_code_transition_availability_short_desc[] =
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
static int ipmi_generic_event_reading_type_code_transition_availability_short_desc_max = 0x08;

static char * ipmi_generic_event_reading_type_code_redundancy_short_desc[] =
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
static int ipmi_generic_event_reading_type_code_redundancy_short_desc_max = 0x07;

static char * ipmi_generic_event_reading_type_code_acpi_power_state_short_desc[] =
  {
    "D0 Power State",
    "D1 Power State",
    "D2 Power State",
    "D3 Power State",
    NULL
  };
static int ipmi_generic_event_reading_type_code_acpi_power_state_short_desc_max = 0x03;

/***************************************
 * Sensor Type Strings (SHORT STRINGS) *
 ***************************************/

/* achu: these are identical to the above but cleaned up for
 * situations where "short strings" are better for output.  I may have
 * slightly modified the strings statements too.
 */

/* achu: 'undock' removed as noted in errata */
static char * ipmi_sensor_type_code_physical_security_short_desc[] =
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
static int ipmi_sensor_type_code_physical_security_short_desc_max = 0x06;

static char * ipmi_sensor_type_code_platform_security_violation_attempt_short_desc[] =
  {
    "Secure Mode Violation attempt",
    "Pre-boot Password Violation - user password",
    "Pre-boot Password Violation - setup password",
    "Pre-boot Password Violation - network boot password",
    "Other pre-boot Password Violation",
    "Out-of-band Access Password Violation",
    NULL
  };
static int ipmi_sensor_type_code_platform_security_violation_attempt_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_processor_short_desc[] =
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
static int ipmi_sensor_type_code_processor_short_desc_max = 0x0C;

static char * ipmi_sensor_type_code_power_supply_short_desc[] =
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
static int ipmi_sensor_type_code_power_supply_short_desc_max = 0x06;

static char * ipmi_sensor_type_code_power_unit_short_desc[] =
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
static int ipmi_sensor_type_code_power_unit_short_desc_max = 0x07;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_memory_short_desc[] =
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
static int ipmi_sensor_type_code_memory_short_desc_max = 0x0A;

/* achu: defined in errata */
static char * ipmi_sensor_type_code_drive_slot_short_desc[] =
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
static int ipmi_sensor_type_code_drive_slot_short_desc_max = 0x08;

static char * ipmi_sensor_type_code_system_firmware_progress_short_desc[] =
  {
    "System Firmware Error",
    "System Firmware Hang",
    "System Firmware Progress",
    NULL
  };
static int ipmi_sensor_type_code_system_firmware_progress_short_desc_max = 0x02;

static char * ipmi_sensor_type_code_event_logging_disabled_short_desc[] =
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
static int ipmi_sensor_type_code_event_logging_disabled_short_desc_max = 0x06;

static char * ipmi_sensor_type_code_watchdog1_short_desc[] =
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
static int ipmi_sensor_type_code_watchdog1_short_desc_max = 0x07;

static char * ipmi_sensor_type_code_system_event_short_desc[] =
  {
    "System Reconfigured",
    "OEM System Boot Event",
    "Undetermined system hardware failure",
    "Entry added to Auxiliary Log",
    "PEF Action",
    "Timestamp Clock Synch",
    NULL
  };
static int ipmi_sensor_type_code_system_event_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_critical_interrupt_short_desc[] =
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
static int ipmi_sensor_type_code_critical_interrupt_short_desc_max = 0x0B;

static char * ipmi_sensor_type_code_button_switch_short_desc[] =
  {
    "Power Button pressed",
    "Sleep Button pressed",
    "Reset Button pressed",
    "FRU latch open",
    "FRU service request button",
    NULL
  };
static int ipmi_sensor_type_code_button_switch_short_desc_max = 0x04;

static char * ipmi_sensor_type_code_chip_set_short_desc[] =
  {
    "Soft Power Control Failure",
    "Thermal Trip",
    NULL
  };
static int ipmi_sensor_type_code_chip_set_short_desc_max = 0x00;

static char * ipmi_sensor_type_code_cable_interconnect_short_desc[] =
  {
    "Cable/Interconnect is connected",
    "Configuration Error - Incorrect cable connected",
    NULL
  };
static int ipmi_sensor_type_code_cable_interconnect_short_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_system_boot_initiated_short_desc[] =
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
static int ipmi_sensor_type_code_system_boot_initiated_short_desc_max = 0x07;

static char * ipmi_sensor_type_code_boot_error_short_desc[] =
  {
    "No bootable media",
    "Non-bootable diskette left in drive",
    "PXE Server not found",
    "Invalid boot sector",
    "Timeout waiting for user selection of boot source",
    NULL
  };
static int ipmi_sensor_type_code_boot_error_short_desc_max = 0x04;

static char * ipmi_sensor_type_code_os_boot_short_desc[] =
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
static int ipmi_sensor_type_code_os_boot_short_desc_max = 0x06;

/* achu: modified per errata */
static char * ipmi_sensor_type_code_os_critical_stop_short_desc[] =
  {
    "Critical stop during OS load",
    "Run-time Critical Stop",
    "OS Graceful Stop",
    "OS Graceful Shutdown",
    "Soft Shutdown initiated by PEF",
    "Agent Not Responding",
    NULL
  };
static int ipmi_sensor_type_code_os_critical_stop_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_slot_connector_short_desc[] =
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
static int ipmi_sensor_type_code_slot_connector_short_desc_max = 0x09;

static char * ipmi_sensor_type_code_acpi_power_state_short_desc[] =
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
static int ipmi_sensor_type_code_acpi_power_state_short_desc_max = 0x0E;

static char * ipmi_sensor_type_code_watchdog2_short_desc[] =
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
static int ipmi_sensor_type_code_watchdog2_short_desc_max = 0x08;

static char * ipmi_sensor_type_code_platform_alert_short_desc[] =
  {
    "platform generated page",
    "platform generated LAN alert",
    "Platform Event Trap generated",
    "platform generated SNMP trap, OEM format",
    NULL
  };
static int ipmi_sensor_type_code_platform_alert_short_desc_max = 0x03;

static char * ipmi_sensor_type_code_entity_presence_short_desc[] =
  {
    "Entity Present",
    "Entity Absent",
    "Entity Disabled",
    NULL
  };
static int ipmi_sensor_type_code_entity_presence_short_desc_max = 0x02;

static char * ipmi_sensor_type_code_lan_short_desc[] =
  {
    "LAN Heartbeat Lost",
    "LAN Heartbeat",
    NULL
  };
static int ipmi_sensor_type_code_lan_short_desc_max = 0x01;

/* achu: new additions as stated in errata */
static char * ipmi_sensor_type_code_management_subsystem_health_short_desc[] =
  {
    "sensor access degraded or unavailable",
    "controller access degraded or unavailable",
    "management controller off-line",
    "management controller unavailable",
    "sensor failure",
    "FRU failure",
    NULL
  };
static int ipmi_sensor_type_code_management_subsystem_health_short_desc_max = 0x05;

static char * ipmi_sensor_type_code_battery_short_desc[] =
  {
    "battery low",
    "battery failed",
    "battery presence detected",
    NULL
  };
static int ipmi_sensor_type_code_battery_short_desc_max = 0x02;

static char * ipmi_sensor_type_code_session_audit_short_desc[] =
  {
    "Session Activated",
    "Session Deactivated",
    "Invalid Username of Password",
    NULL
  };
static int ipmi_sensor_type_code_session_audit_short_desc_max = 0x02;

static char * ipmi_sensor_type_code_version_change_short_desc[] =
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
static int ipmi_sensor_type_code_version_change_short_desc_max = 0x07;

static char * ipmi_sensor_type_code_fru_state_short_desc[] =
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
static int ipmi_sensor_type_code_fru_state_short_desc_max = 0x07;

/*******************************************************
 * Sensor Type Strings for Event Data 2 (FULL STRINGS) *
 *******************************************************/

static char * ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_00_desc[] =
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
static int ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_00_desc_max = 0x0D;

static char * ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_01_desc[] =
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
static int ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_01_desc_max = 0x18;

static char * ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_02_desc[] =
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
static int ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_02_desc_max = 0x18;

static char * ipmi_sensor_type_code_system_event_event_data2_offset_03_log_entry_action_desc[] =
  {
    "Log entry action = entry added",
    "Log entry action = entry added because event did not be map to standard IPMI event",
    "Log entry action = entry added along with one or more corresponding SEL entries",
    "Log entry action = log cleared",
    "Log entry action = log disabled",
    "Log entry action = log enabled",
    NULL
  };
static int ipmi_sensor_type_code_system_event_event_data2_offset_03_log_entry_action_desc_max = 0x05;

static char * ipmi_sensor_type_code_system_event_event_data2_offset_03_log_type_desc[] =
  {
    "Log Type = MCA log",
    "Log Type = OEM1",
    "Log Type = OEM2",
    NULL
  };
static int ipmi_sensor_type_code_system_event_event_data2_offset_03_log_type_desc_max = 0x02;

static char * ipmi_sensor_type_code_system_event_event_data2_offset_04_pef_action_desc[] =
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
static int ipmi_sensor_type_code_system_event_event_data2_offset_04_pef_action_desc_max = 0x05;
#endif

static char * ipmi_sensor_type_code_system_event_event_data2_offset_05_first_second_desc[] =
  {
    "event is first of pair",
    "event is second of pair",
    NULL,
  };
static int ipmi_sensor_type_code_system_event_event_data2_offset_05_first_second_desc_max = 0x01;

static char * ipmi_sensor_type_code_system_event_event_data2_offset_05_timestamp_clock_type_desc[] =
  {
    "SEL Timestamp Clock updated (Also used when both SEL and SDR Timestamp clocks are linked together)",
    "SDR Timestamp Clock updated",
    NULL,
  };
static int ipmi_sensor_type_code_system_event_event_data2_offset_05_timestamp_clock_type_desc_max = 0x01;

static char * ipmi_sensor_type_code_chip_set_event_data2_offset_00_desc[] =
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
static int ipmi_sensor_type_code_chip_set_event_data2_offset_00_desc_max = 0x0C;

static char * ipmi_sensor_type_code_system_boot_initiated_event_data2_offset_07_restart_cause_desc[] =
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
static int ipmi_sensor_type_code_system_boot_initiated_event_data2_offset_07_restart_cause_desc_max = 0x0B;

static char * ipmi_sensor_type_code_slot_connector_event_data2_offset_09_slot_connector_type_desc[] =
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
static int ipmi_sensor_type_code_slot_connector_event_data2_offset_09_slot_connector_type_desc_max = 0x0C;

static char * ipmi_sensor_type_code_watchdog2_event_data2_offset_08_interrupt_type_desc[] =
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
static int ipmi_sensor_type_code_watchdog2_event_data2_offset_08_interrupt_type_desc_max = 0x0F;

static char * ipmi_sensor_type_code_watchdog2_event_data2_offset_08_timer_use_at_expiration_desc[] =
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
static int ipmi_sensor_type_code_watchdog2_event_data2_offset_08_timer_use_at_expiration_desc_max = 0x0F;

static char * ipmi_sensor_type_code_management_subsystem_health_event_data2_offset_05_logical_fru_device_desc[] =
  {
    "device is not a logical FRU Device",
    "device is logical FRU Device (accessed via FRU commands to mgmt. controller",
    NULL
  };
static int ipmi_sensor_type_code_management_subsystem_health_event_data2_offset_05_logical_fru_device_desc_max = 0x01;

static char * ipmi_sensor_type_code_version_change_event_data2_offset_07_version_change_type_desc[] =
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
static int ipmi_sensor_type_code_version_change_event_data2_offset_07_version_change_type_desc_max = 0x17;

static char * ipmi_sensor_type_code_fru_state_event_data2_offset_07_cause_of_state_change_desc[] =
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
static int ipmi_sensor_type_code_fru_state_event_data2_offset_07_cause_of_state_change_desc_max = 0x0F;

/*******************************************************
 * Sensor Type Strings for Event Data 3 (FULL STRINGS) *
 *******************************************************/

static char * ipmi_sensor_type_code_power_supply_event_data3_offset_06_error_type_desc[] =
  {
    "Vendor mismatch",
    "Revision mismatch",
    "Processor missing or unexpected/unsupported condition",
    "Power Supply rating mismatch",
    "Voltage rating mismatch",
    NULL
  };
static int ipmi_sensor_type_code_power_supply_event_data3_offset_06_error_type_desc_max = 0x04;

static char * ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_assertion_event_desc[] =
  {
    "deassertion event",
    "assertion event",
    NULL
  };
static int ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_assertion_event_desc_max = 0x01;

static char * ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_logging_disabled_all_events_desc[] =
  {
    "",
    "logging has been disabled for all events of given type",
    NULL
  };
static int ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_logging_disabled_all_events_desc_max = 0x01;

static char * ipmi_sensor_type_code_chip_set_event_data3_offset_00_desc[] =
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
static int ipmi_sensor_type_code_chip_set_event_data3_offset_00_desc_max = 0x0D;

static char * ipmi_sensor_type_code_session_audit_event_data3_offset_01_deactivation_cause_desc[] =
  {
    "Session deactivatation cause unspecified. This value is also used for Session Activated events",
    "Session deactivated by Close Session command",
    "Session deactivated by timeout",
    "Session deactivated by configuration change",
    NULL
  };
static int ipmi_sensor_type_code_session_audit_event_data3_offset_01_deactivation_cause_desc_max = 0x03;

/***************************************
 * Generic Event Reading Strings (OEM) *
 ***************************************/

/*
 * Dell
 */

static char * ipmi_generic_event_reading_type_code_oem_dell_status_desc[] =
  {
    "Absent",
    "Standby",
    "IPMI Function ready",
    "Fully ready",
    "Offline",
    "Failed",
    "Active",
    "Booting",
    "Write protected",
    NULL
  };
static int ipmi_generic_event_reading_type_code_oem_dell_status_desc_max = 0x08;

/*****************************
 * Sensor Type Strings (OEM) *
 *****************************/

/*
 * Dell
 */

/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 */
/* achu:
 *
 * I have a feeling "good" is the random string they choose in some
 * code, it's not the real string.  But that's all I got to go on.
 *
 */

static char * ipmi_sensor_type_code_oem_dell_system_performance_degradation_status_desc[] =
  {
    "Good",
    "Degraded, other",
    "Degraded, thermal protection",
    "Degraded, cooling capacity change",
    "Degraded, power capacity change",
    "Degraded, user defined power capacity",
    "Halted, system power exceeds capacity",
    "Degraded, system power exceeds capacity",
    NULL
  };
static int ipmi_sensor_type_code_oem_dell_system_performance_degradation_status_desc_max = 0x07;

static char * ipmi_sensor_type_code_oem_dell_link_tuning_desc[] =
  {
    "Good",
    "Failed to program virtual MAC address",
    "Device option ROM failed to support link tuning or flex address",
    "Failed to get link tuning or flex address data",
    NULL
  };
static int ipmi_sensor_type_code_oem_dell_link_tuning_desc_max = 0x03;

static char * ipmi_sensor_type_code_oem_dell_non_fatal_error_desc[] =
  {
    "PCIe error",
    NULL
  };
static int ipmi_sensor_type_code_oem_dell_non_fatal_error_desc_max = 0x00;

static char * ipmi_sensor_type_code_oem_dell_fatal_io_error_desc[] =
  {
    "Successful",
    "Fatal IO error",
    NULL
  };
static int ipmi_sensor_type_code_oem_dell_fatal_io_error_desc_max = 0x01;

static char * ipmi_sensor_type_code_oem_dell_upgrade_desc[] =
  {
    "Successful",
    "Failed",
    NULL
  };
static int ipmi_sensor_type_code_oem_dell_upgrade_desc_max = 0x01;

/*****************************
 * FLAGS                     *
 *****************************/

#define IPMI_SENSOR_TYPE_CODE_EVENT_LOGGING_DISABLED_EVENT_DATA_OFFSET3_ENTITY_INSTANCE_NUMBER           0x0
#define IPMI_SENSOR_TYPE_CODE_EVENT_LOGGING_DISABLED_EVENT_DATA_OFFSET3_VENDOR_SPECIFIC_PROCESSOR_NUMBER 0x1

static int
_snprintf (char *buf, unsigned int buflen, char *fmt, ...)
{
  int rv;
  va_list ap;

  assert (buf && buflen && fmt);

  va_start (ap, fmt);
  rv = vsnprintf (buf, buflen, fmt, ap);
  va_end (ap);

  return (rv);
}

static int
get_physical_security_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x04)
    return (_snprintf (buf, buflen, "Network controller #%d", event_data2));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_system_firmware_progress_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x00 && event_data2 <= ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_00_desc_max)
    return (_snprintf (buf, buflen, ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_00_desc[event_data2]));
  if (offset == 0x01 && event_data2 <= ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_01_desc_max)
    return (_snprintf (buf, buflen, ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_01_desc[event_data2]));
  if (offset == 0x02 && event_data2 <= ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_02_desc_max)
    return (_snprintf (buf, buflen, ipmi_sensor_type_code_system_firmware_progress_event_data2_offset_02_desc[event_data2]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_event_logging_disabled_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x00)
    return (_snprintf (buf, buflen, "Memory module/device #%d", event_data2));
  if (offset == 0x01)
    return (_snprintf (buf, buflen, "Event/Reading Type Code #%d", event_data2));
  if (offset == 0x06)
    return (_snprintf (buf, buflen, "Instance ID #%d", event_data2));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
_get_system_event_event_data2_message_offset_03 (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "log_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 4, "log_entry_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t log_type;
  uint8_t log_entry_action;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "log_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  log_type = val;

  if (FIID_OBJ_GET (obj, "log_entry_action", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  log_entry_action = val;

  if (log_type <= ipmi_sensor_type_code_system_event_event_data2_offset_03_log_entry_action_desc_max)
    str1 = ipmi_sensor_type_code_system_event_event_data2_offset_03_log_entry_action_desc[log_type];

  if (log_entry_action <= ipmi_sensor_type_code_system_event_event_data2_offset_03_log_type_desc_max)
    str2 = ipmi_sensor_type_code_system_event_event_data2_offset_03_log_type_desc[log_entry_action];

  if (str1 || str2)
    rv = _snprintf (buf, buflen, "%s%s%s",
                    (str1 ? str1 : ""),
                    ((str1 && str2) ? "; " : ""),
                    (str2 ? str2 : ""));

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
_strcat12 (char *buf, unsigned int buflen, uint8_t flag, int str_len, int index)
{
  if (flag)
    {
      str_len += strlen (ipmi_sensor_type_code_system_event_event_data2_offset_04_pef_action_desc[index]);
      if (str_len < buflen)
        {
          SET_ERRNO (ENOSPC);
          return (-1);
        }

      if (str_len)
        strcat (buf, ipmi_sensor_type_code_system_event_event_data2_offset_04_pef_action_desc[index]);
      else
        {
          strcat (buf, "; ");
          strcat (buf, "%s");
        }
      return (str_len);
    }
  return (str_len);
}
static int
_get_system_event_event_data2_message_offset_04 (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 1, "alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "power_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "diagonstic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t alert, power_off, reset, power_cycle, oem_action, diagnostic_interrupt;
  fiid_obj_t obj = NULL;
  int str_len = 0;
  int rv = -1;

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "alert", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  alert = val;

  if (FIID_OBJ_GET (obj, "power_off", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  power_off = val;

  if (FIID_OBJ_GET (obj, "reset", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  reset = val;

  if (FIID_OBJ_GET (obj, "power_cycle", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  power_cycle = val;

  if (FIID_OBJ_GET (obj, "oem_action", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  oem_action = val;

  if (FIID_OBJ_GET (obj, "diagnostic_interrupt", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  diagnostic_interrupt = val;

  memset (buf, '\0', buflen);

  if ((str_len = _strcat12 (buf, buflen, alert, str_len, 0)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat12 (buf, buflen, power_off, str_len, 1)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat12 (buf, buflen, reset, str_len, 2)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat12 (buf, buflen, power_cycle, str_len, 3)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat12 (buf, buflen, oem_action, str_len, 4)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat12 (buf, buflen, diagnostic_interrupt, str_len, 5)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
_get_system_event_event_data2_message_offset_05 (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "timestamp_clock_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "first_second", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t timestamp_clock_type;
  uint8_t first_second;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "timestamp_clock_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  timestamp_clock_type = val;

  if (FIID_OBJ_GET (obj, "first_second", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  first_second = val;

  if (timestamp_clock_type <= ipmi_sensor_type_code_system_event_event_data2_offset_05_timestamp_clock_type_desc_max)
    str1 = ipmi_sensor_type_code_system_event_event_data2_offset_05_timestamp_clock_type_desc[timestamp_clock_type];

  if (first_second <= ipmi_sensor_type_code_system_event_event_data2_offset_05_first_second_desc_max)
    str2 = ipmi_sensor_type_code_system_event_event_data2_offset_05_first_second_desc[first_second];

  rv = _snprintf (buf, buflen, "%s; %s",
                  str1 ? str1 : "",
                  str2 ? str2 : "");

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_system_event_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x03)
    return (_get_system_event_event_data2_message_offset_03 (offset, event_data2, buf, buflen));
  if (offset == 0x04)
    return (_get_system_event_event_data2_message_offset_04 (offset, event_data2, buf, buflen));
  if (offset == 0x05)
    return (_get_system_event_event_data2_message_offset_05 (offset, event_data2, buf, buflen));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_chip_set_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x00 && event_data2 <= ipmi_sensor_type_code_chip_set_event_data2_offset_00_desc_max)
    return (_snprintf (buf, buflen, ipmi_sensor_type_code_chip_set_event_data2_offset_00_desc[event_data2]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_system_boot_initiated_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x07)
    {
      fiid_template_t tmpl_event_data2 =
        {
          { 4, "restart_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      fiid_obj_t obj = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create ( tmpl_event_data2)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "restart_cause", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (val <= ipmi_sensor_type_code_system_boot_initiated_event_data2_offset_07_restart_cause_desc_max)
        rv = _snprintf (buf, buflen, ipmi_sensor_type_code_system_boot_initiated_event_data2_offset_07_restart_cause_desc[val]);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_slot_connector_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 7, "slot_connector_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  fiid_obj_t obj = NULL;
  int rv = -1;

  assert (buf && buflen);

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "slot_connector_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (val <= ipmi_sensor_type_code_slot_connector_event_data2_offset_09_slot_connector_type_desc_max)
    rv = _snprintf (buf, buflen, ipmi_sensor_type_code_slot_connector_event_data2_offset_09_slot_connector_type_desc[val]);

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_watchdog2_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "timer_at_expiration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 4, "interrupt_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t timer_at_expiration;
  uint8_t interrupt_type;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  assert (buf && buflen);

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "timer_at_expiration", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  timer_at_expiration = val;

  if (FIID_OBJ_GET (obj, "interrupt_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  interrupt_type = val;

  if (timer_at_expiration <= ipmi_sensor_type_code_watchdog2_event_data2_offset_08_timer_use_at_expiration_desc_max)
    str1 = ipmi_sensor_type_code_watchdog2_event_data2_offset_08_timer_use_at_expiration_desc[timer_at_expiration];

  if (interrupt_type <= ipmi_sensor_type_code_watchdog2_event_data2_offset_08_interrupt_type_desc_max)
    str2 = ipmi_sensor_type_code_watchdog2_event_data2_offset_08_interrupt_type_desc[interrupt_type];

  if (str1 || str2)
    rv = _snprintf (buf, buflen, "%s%s%s",
                    (str1 ? str1 : ""),
                    ((str1 && str2) ? "; " : ""),
                    (str2 ? str2 : ""));

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_management_subsystem_health_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  int rv = -1;

  assert (buf && buflen);

  if (offset == 0x00 || offset == 0x04)
    rv = _snprintf (buf, buflen, "Sensor Number #%d", event_data2);
  else if (offset == 0x05)
    {
      fiid_template_t tmpl_event_data2 =
        {
          { 3, "private_bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 1, "fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      uint8_t private_bus_id, lun, fru_device;
      fiid_obj_t obj = NULL;
      char *str = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create (tmpl_event_data2)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "private_bus_id", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      private_bus_id = val;

      if (FIID_OBJ_GET (obj, "lun", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      lun = val;

      if (FIID_OBJ_GET (obj, "fru_device", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      fru_device = val;

      if (fru_device <= ipmi_sensor_type_code_management_subsystem_health_event_data2_offset_05_logical_fru_device_desc_max)
        str = ipmi_sensor_type_code_management_subsystem_health_event_data2_offset_05_logical_fru_device_desc[fru_device];


      rv = _snprintf (buf, buflen, "%s; LUN for Master Write-Read command or FRU Command #%d; Private bus ID #%d",
                      str ? str : "",
                      lun, private_bus_id);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_session_audit_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x01)
    {
      fiid_template_t tmpl_event_data2 =
        {
          { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      uint8_t user_id;
      fiid_obj_t obj = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create (tmpl_event_data2)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "user_id", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      user_id = val;

      if (user_id == 0)
        rv = _snprintf (buf, buflen, "User ID for user that activated session = Unspecified");
      else
        rv = _snprintf (buf, buflen, "User ID for user that activated session #%u", user_id);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_version_change_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (event_data2 <= ipmi_sensor_type_code_version_change_event_data2_offset_07_version_change_type_desc_max)
    return (_snprintf (buf, buflen, ipmi_sensor_type_code_version_change_event_data2_offset_07_version_change_type_desc[event_data2]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_fru_state_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "previous_state_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 4, "cause_of_state_change", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t previous_state_offset;
  uint8_t cause_of_state_change;
  char *str = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  assert (buf && buflen);

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "previous_state_offset", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  previous_state_offset = val;

  if (FIID_OBJ_GET (obj, "cause_of_state_change", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  cause_of_state_change = val;

  if (cause_of_state_change <= ipmi_sensor_type_code_fru_state_event_data2_offset_07_cause_of_state_change_desc_max)
    str = ipmi_sensor_type_code_fru_state_event_data2_offset_07_cause_of_state_change_desc[cause_of_state_change];

  rv = _snprintf (buf, buflen, "Previous state offset value = %d; %s", previous_state_offset, str ? str : "");

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_power_supply_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x06)
    {
      fiid_template_t tmpl_event_data3 =
        {
          { 4, "event_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      fiid_obj_t obj = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create (tmpl_event_data3)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "event_type", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (val <= ipmi_sensor_type_code_power_supply_event_data3_offset_06_error_type_desc_max)
        rv = _snprintf (buf, buflen, ipmi_sensor_type_code_power_supply_event_data3_offset_06_error_type_desc[val]);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_memory_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x08)
    return (_snprintf (buf, buflen, "Memory module/device #%d", event_data3));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_event_logging_disabled_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  switch (offset)
    {
    case 0x01:
      {
        fiid_template_t tmpl_event_data3 =
          {
            { 4, "event_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 1, "assertion_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 1, "logging_disabled_all_events", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 0, "", 0}
          };
        uint64_t val;
        uint8_t event_offset;
        uint8_t assertion_deassertion_event;
        uint8_t logging_disabled_all_events;
        fiid_obj_t obj = NULL;
        char *str1 = NULL;
        char *str2 = NULL;
        int rv = -1;

        if (!(obj = fiid_obj_create (tmpl_event_data3)))
          {
            ERRNO_TRACE (errno);
            goto cleanup1;
          }

        if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }

        if (FIID_OBJ_GET (obj, "event_offset", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }
        event_offset = val;

        if (FIID_OBJ_GET (obj, "assertion_deassertion_e", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }
        assertion_deassertion_event = val;

        if (FIID_OBJ_GET (obj, "logging_disabled_all_ev", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }
        logging_disabled_all_events = val;

        if (assertion_deassertion_event <= ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_assertion_event_desc_max)
          str1 = ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_assertion_event_desc[assertion_deassertion_event];

        if (logging_disabled_all_events <= ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_logging_disabled_all_events_desc_max)
          str2 = ipmi_sensor_type_code_event_logging_disabled_event_data3_offset_01_logging_disabled_all_events_desc[logging_disabled_all_events];

        rv = _snprintf (buf, buflen, "Event Offset #%d; %s%s%s",
                        event_offset, (str1 ? str1 : ""), ((str1 && str2 && strlen (str2)) ? "; " : ""), (str2 ? str2 : ""));

      cleanup1:
        fiid_obj_destroy (obj);
        return (rv);
      }
    case 0x05:
      return (_snprintf (buf, buflen, "%d% full", event_data3));
    case 0x06:
      {
        fiid_template_t tmpl_event_data3 =
          {
            { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 1, "number_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 0, "", 0}
          };
        uint64_t val;
        uint8_t number_type;
        char *str = NULL;
        fiid_obj_t obj = NULL;
        int rv = -1;
        
        if (!(obj = fiid_obj_create (tmpl_event_data3)))
          {
            ERRNO_TRACE (errno);
            goto cleanup2;
          }
        
        if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup2;
          }
        
        if (FIID_OBJ_GET (obj, "number_type", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup2;
          }
        number_type = val;
        
        if (number_type == IPMI_SENSOR_TYPE_CODE_EVENT_LOGGING_DISABLED_EVENT_DATA_OFFSET3_ENTITY_INSTANCE_NUMBER)
          str = "Entity Instance Number";
        else 
          str = "Vendor-specific Processor Number";

        rv = _snprintf (buf,
                        buflen,
                        "%s = #%d \n",
                        str,
                        event_data2);
        
      cleanup2:
        fiid_obj_destroy (obj);
        return (rv);
      }
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_chip_set_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x00 && event_data3 <= ipmi_sensor_type_code_chip_set_event_data3_offset_00_desc_max)
    return (_snprintf (buf, buflen, ipmi_sensor_type_code_chip_set_event_data3_offset_00_desc[event_data3]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_system_boot_initiated_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x07)
    return (_snprintf (buf, buflen, "Channel Number used to deliver command that generated restart #%d", event_data3));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_slot_connector_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  return (_snprintf (buf, buflen, "Slot/Connector #%d", event_data3));
}

static int
get_management_subsystem_health_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x05)
    return (_snprintf (buf, buflen, "FRU Device ID/Slave Address #%d", event_data3));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_session_audit_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == 0x01 || offset == 0x02)
    {
      fiid_template_t tmpl_event_data3 =
        {
          { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "deactivation_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      uint8_t channel_number;
      uint8_t deactivation_cause;
      fiid_obj_t obj = NULL;
      char *str = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create (tmpl_event_data3)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "channel_number", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      channel_number = val;

      if (FIID_OBJ_GET (obj, "deactivation_cause", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      deactivation_cause = val;

      /* output deactivation case only if deactivation offset occurred */
      if (offset == 0x02)
        {
          if (deactivation_cause <= ipmi_sensor_type_code_session_audit_event_data3_offset_01_deactivation_cause_desc_max)
            str = ipmi_sensor_type_code_session_audit_event_data3_offset_01_deactivation_cause_desc[deactivation_cause];
        }

      rv = _snprintf (buf, buflen, "Channel number that session was activated/deactivated = %d%s%s",
                      channel_number, (str) ? "; " : "", str ? str : "");

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

/***************************************************/

static int
_get_event_message (unsigned int offset,
                    char *buf,
                    unsigned int buflen,
                    unsigned int offset_max,
                    char *string_array[])
{
  assert (buf && buflen);

  if (offset > offset_max)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (snprintf (buf, buflen, string_array[offset]));
}

int
ipmi_get_generic_event_message (uint8_t event_reading_type_code,
                                unsigned int offset,
                                char *buf,
                                unsigned int buflen)
{
  if (!buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (event_reading_type_code)
    {
    case IPMI_EVENT_READING_TYPE_CODE_THRESHOLD:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_threshold_desc_max,
                                  ipmi_generic_event_reading_type_code_threshold_desc));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_state_desc_max,
                                  ipmi_generic_event_reading_type_code_transition_state_desc));
    case IPMI_EVENT_READING_TYPE_CODE_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_state_desc_max,
                                  ipmi_generic_event_reading_type_code_state_desc));
    case IPMI_EVENT_READING_TYPE_CODE_PREDICTIVE_FAILURE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_predictive_failure_desc_max,
                                  ipmi_generic_event_reading_type_code_predictive_failure_desc));
    case IPMI_EVENT_READING_TYPE_CODE_LIMIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_limit_desc_max,
                                  ipmi_generic_event_reading_type_code_limit_desc));
    case IPMI_EVENT_READING_TYPE_CODE_PERFORMANCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_performance_desc_max,
                                  ipmi_generic_event_reading_type_code_performance_desc));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_severity_desc_max,
                                  ipmi_generic_event_reading_type_code_transition_severity_desc));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_present_desc_max,
                                  ipmi_generic_event_reading_type_code_device_present_desc));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_enabled_desc_max,
                                  ipmi_generic_event_reading_type_code_device_enabled_desc));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_AVAILABILITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_availability_desc_max,
                                  ipmi_generic_event_reading_type_code_transition_availability_desc));
    case IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_redundancy_desc_max,
                                  ipmi_generic_event_reading_type_code_redundancy_desc));
    case IPMI_EVENT_READING_TYPE_CODE_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_acpi_power_state_desc_max,
                                  ipmi_generic_event_reading_type_code_acpi_power_state_desc));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_sensor_type_code_message (uint8_t sensor_type_code,
                                   unsigned int offset,
                                   char *buf,
                                   unsigned int buflen)
{
  if (!buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type_code)
    {
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_physical_security_desc_max,
                                  ipmi_sensor_type_code_physical_security_desc));
    case IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_platform_security_violation_attempt_desc_max,
                                  ipmi_sensor_type_code_platform_security_violation_attempt_desc));
    case IPMI_SENSOR_TYPE_PROCESSOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_processor_desc_max,
                                  ipmi_sensor_type_code_processor_desc));
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_power_supply_desc_max,
                                  ipmi_sensor_type_code_power_supply_desc));
    case IPMI_SENSOR_TYPE_POWER_UNIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_power_unit_desc_max,
                                  ipmi_sensor_type_code_power_unit_desc));
    case IPMI_SENSOR_TYPE_MEMORY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_memory_desc_max,
                                  ipmi_sensor_type_code_memory_desc));
    case IPMI_SENSOR_TYPE_DRIVE_SLOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_drive_slot_desc_max,
                                  ipmi_sensor_type_code_drive_slot_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_system_firmware_progress_desc_max,
                                  ipmi_sensor_type_code_system_firmware_progress_desc));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_event_logging_disabled_desc_max,
                                  ipmi_sensor_type_code_event_logging_disabled_desc));
    case IPMI_SENSOR_TYPE_WATCHDOG1:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_watchdog1_desc_max,
                                  ipmi_sensor_type_code_watchdog1_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_system_event_desc_max,
                                  ipmi_sensor_type_code_system_event_desc));
    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_critical_interrupt_desc_max,
                                  ipmi_sensor_type_code_critical_interrupt_desc));
    case IPMI_SENSOR_TYPE_BUTTON_SWITCH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_button_switch_desc_max,
                                  ipmi_sensor_type_code_button_switch_desc));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_chip_set_desc_max,
                                  ipmi_sensor_type_code_chip_set_desc));
    case IPMI_SENSOR_TYPE_CABLE_INTERCONNECT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_cable_interconnect_desc_max,
                                  ipmi_sensor_type_code_cable_interconnect_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_system_boot_initiated_desc_max,
                                  ipmi_sensor_type_code_system_boot_initiated_desc));
    case IPMI_SENSOR_TYPE_BOOT_ERROR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_boot_error_desc_max,
                                  ipmi_sensor_type_code_boot_error_desc));
    case IPMI_SENSOR_TYPE_OS_BOOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_os_boot_desc_max,
                                  ipmi_sensor_type_code_os_boot_desc));
    case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_os_critical_stop_desc_max,
                                  ipmi_sensor_type_code_os_critical_stop_desc));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_slot_connector_desc_max,
                                  ipmi_sensor_type_code_slot_connector_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_acpi_power_state_desc_max,
                                  ipmi_sensor_type_code_acpi_power_state_desc));
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_watchdog2_desc_max,
                                  ipmi_sensor_type_code_watchdog2_desc));
    case IPMI_SENSOR_TYPE_PLATFORM_ALERT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_platform_alert_desc_max,
                                  ipmi_sensor_type_code_platform_alert_desc));
    case IPMI_SENSOR_TYPE_ENTITY_PRESENCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_entity_presence_desc_max,
                                  ipmi_sensor_type_code_entity_presence_desc));
    case IPMI_SENSOR_TYPE_LAN:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_lan_desc_max,
                                  ipmi_sensor_type_code_lan_desc));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_management_subsystem_health_desc_max,
                                  ipmi_sensor_type_code_management_subsystem_health_desc));
    case IPMI_SENSOR_TYPE_BATTERY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_battery_desc_max,
                                  ipmi_sensor_type_code_battery_desc));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_session_audit_desc_max,
                                  ipmi_sensor_type_code_session_audit_desc));
    case IPMI_SENSOR_TYPE_VERSION_CHANGE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_version_change_desc_max,
                                  ipmi_sensor_type_code_version_change_desc));
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_fru_state_desc_max,
                                  ipmi_sensor_type_code_fru_state_desc));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_generic_event_message_short (uint8_t event_reading_type_code,
                                      unsigned int offset,
                                      char *buf,
                                      unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (event_reading_type_code)
    {
    case IPMI_EVENT_READING_TYPE_CODE_THRESHOLD:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_threshold_short_desc_max,
                                  ipmi_generic_event_reading_type_code_threshold_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_state_short_desc_max,
                                  ipmi_generic_event_reading_type_code_transition_state_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_state_short_desc_max,
                                  ipmi_generic_event_reading_type_code_state_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_PREDICTIVE_FAILURE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_predictive_failure_short_desc_max,
                                  ipmi_generic_event_reading_type_code_predictive_failure_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_LIMIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_limit_short_desc_max,
                                  ipmi_generic_event_reading_type_code_limit_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_PERFORMANCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_performance_short_desc_max,
                                  ipmi_generic_event_reading_type_code_performance_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_severity_short_desc_max,
                                  ipmi_generic_event_reading_type_code_transition_severity_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_present_short_desc_max,
                                  ipmi_generic_event_reading_type_code_device_present_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_enabled_short_desc_max,
                                  ipmi_generic_event_reading_type_code_device_enabled_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_AVAILABILITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_availability_short_desc_max,
                                  ipmi_generic_event_reading_type_code_transition_availability_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_redundancy_short_desc_max,
                                  ipmi_generic_event_reading_type_code_redundancy_short_desc));
    case IPMI_EVENT_READING_TYPE_CODE_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_acpi_power_state_short_desc_max,
                                  ipmi_generic_event_reading_type_code_acpi_power_state_short_desc));
    }

  return (-1);
}

int
ipmi_get_sensor_type_code_message_short (uint8_t sensor_type_code,
                                         unsigned int offset,
                                         char *buf,
                                         unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type_code)
    {
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_physical_security_short_desc_max,
                                  ipmi_sensor_type_code_physical_security_short_desc));
    case IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_platform_security_violation_attempt_short_desc_max,
                                  ipmi_sensor_type_code_platform_security_violation_attempt_short_desc));
    case IPMI_SENSOR_TYPE_PROCESSOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_processor_short_desc_max,
                                  ipmi_sensor_type_code_processor_short_desc));
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_power_supply_short_desc_max,
                                  ipmi_sensor_type_code_power_supply_short_desc));
    case IPMI_SENSOR_TYPE_POWER_UNIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_power_unit_short_desc_max,
                                  ipmi_sensor_type_code_power_unit_short_desc));
    case IPMI_SENSOR_TYPE_MEMORY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_memory_short_desc_max,
                                  ipmi_sensor_type_code_memory_short_desc));
    case IPMI_SENSOR_TYPE_DRIVE_SLOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_drive_slot_short_desc_max,
                                  ipmi_sensor_type_code_drive_slot_short_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_system_firmware_progress_short_desc_max,
                                  ipmi_sensor_type_code_system_firmware_progress_short_desc));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_event_logging_disabled_short_desc_max,
                                  ipmi_sensor_type_code_event_logging_disabled_short_desc));
    case IPMI_SENSOR_TYPE_WATCHDOG1:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_watchdog1_short_desc_max,
                                  ipmi_sensor_type_code_watchdog1_short_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_system_event_short_desc_max,
                                  ipmi_sensor_type_code_system_event_short_desc));
    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_critical_interrupt_short_desc_max,
                                  ipmi_sensor_type_code_critical_interrupt_short_desc));
    case IPMI_SENSOR_TYPE_BUTTON_SWITCH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_button_switch_short_desc_max,
                                  ipmi_sensor_type_code_button_switch_short_desc));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_chip_set_short_desc_max,
                                  ipmi_sensor_type_code_chip_set_short_desc));
    case IPMI_SENSOR_TYPE_CABLE_INTERCONNECT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_cable_interconnect_short_desc_max,
                                  ipmi_sensor_type_code_cable_interconnect_short_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_system_boot_initiated_short_desc_max,
                                  ipmi_sensor_type_code_system_boot_initiated_short_desc));
    case IPMI_SENSOR_TYPE_BOOT_ERROR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_boot_error_short_desc_max,
                                  ipmi_sensor_type_code_boot_error_short_desc));
    case IPMI_SENSOR_TYPE_OS_BOOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_os_boot_short_desc_max,
                                  ipmi_sensor_type_code_os_boot_short_desc));
    case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_os_critical_stop_short_desc_max,
                                  ipmi_sensor_type_code_os_critical_stop_short_desc));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_slot_connector_short_desc_max,
                                  ipmi_sensor_type_code_slot_connector_short_desc));
    case IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_acpi_power_state_short_desc_max,
                                  ipmi_sensor_type_code_acpi_power_state_short_desc));
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_watchdog2_short_desc_max,
                                  ipmi_sensor_type_code_watchdog2_short_desc));
    case IPMI_SENSOR_TYPE_PLATFORM_ALERT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_platform_alert_short_desc_max,
                                  ipmi_sensor_type_code_platform_alert_short_desc));
    case IPMI_SENSOR_TYPE_ENTITY_PRESENCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_entity_presence_short_desc_max,
                                  ipmi_sensor_type_code_entity_presence_short_desc));
    case IPMI_SENSOR_TYPE_LAN:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_lan_short_desc_max,
                                  ipmi_sensor_type_code_lan_short_desc));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_management_subsystem_health_short_desc_max,
                                  ipmi_sensor_type_code_management_subsystem_health_short_desc));
    case IPMI_SENSOR_TYPE_BATTERY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_battery_short_desc_max,
                                  ipmi_sensor_type_code_battery_short_desc));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_session_audit_short_desc_max,
                                  ipmi_sensor_type_code_session_audit_short_desc));
    case IPMI_SENSOR_TYPE_VERSION_CHANGE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_version_change_short_desc_max,
                                  ipmi_sensor_type_code_version_change_short_desc));
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_code_fru_state_short_desc_max,
                                  ipmi_sensor_type_code_fru_state_short_desc));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_event_data2_message (uint8_t sensor_type_code,
                              unsigned int offset,
                              uint8_t event_data2,
                              char *buf,
                              unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type_code)
    {
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (get_physical_security_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (get_system_firmware_progress_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (get_event_logging_disabled_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (get_system_event_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (get_chip_set_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (get_system_boot_initiated_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (get_slot_connector_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (get_watchdog2_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (get_management_subsystem_health_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (get_session_audit_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_VERSION_CHANGE:
      return (get_version_change_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (get_fru_state_event_data2_message (offset, event_data2, buf, buflen));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_event_data3_message (uint8_t sensor_type_code,
                              unsigned int offset,
                              uint8_t event_data2,
                              uint8_t event_data3,
                              char *buf,
                              unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type_code)
    {
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (get_power_supply_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_MEMORY:
      return (get_memory_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (get_event_logging_disabled_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (get_chip_set_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (get_system_boot_initiated_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (get_slot_connector_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (get_management_subsystem_health_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (get_session_audit_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_oem_generic_event_message (uint32_t manufacturer_id,
                                    uint16_t product_id,
                                    uint8_t event_reading_type_code,
                                    unsigned int offset,
                                    char *buf,
                                    unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710))
    {
      switch (event_reading_type_code)
        {
        case IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_generic_event_reading_type_code_oem_dell_status_desc_max,
                                      ipmi_generic_event_reading_type_code_oem_dell_status_desc));
        }
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_oem_sensor_type_code_message (uint32_t manufacturer_id,
                                       uint16_t product_id,
                                       uint8_t sensor_type_code,
                                       unsigned int offset,
                                       char *buf,
                                       unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710))
    {
      switch (sensor_type_code)
        {
        case IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_code_oem_dell_system_performance_degradation_status_desc_max,
                                      ipmi_sensor_type_code_oem_dell_system_performance_degradation_status_desc));
        case IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_code_oem_dell_link_tuning_desc_max,
                                      ipmi_sensor_type_code_oem_dell_link_tuning_desc));
        case IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_code_oem_dell_non_fatal_error_desc_max,
                                      ipmi_sensor_type_code_oem_dell_non_fatal_error_desc));
        case IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_code_oem_dell_fatal_io_error_desc_max,
                                      ipmi_sensor_type_code_oem_dell_fatal_io_error_desc));
        case IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_code_oem_dell_upgrade_desc_max,
                                      ipmi_sensor_type_code_oem_dell_upgrade_desc));
        }
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_oem_sensor_event_bitmask_message (uint32_t manufacturer_id,
					   uint16_t product_id,
					   uint8_t event_reading_type_code,
					   uint8_t sensor_type_code,
					   uint16_t sensor_event_bitmask,
					   char *buf,
					   unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* OEM Interpretation
   *
   * Supermicro X8DTH
   *
   * Event Reading Type Code = 0x70 (OEM)
   * Sensor Type = 0xC0 (OEM)
   * - 0 = Low
   * - 1 = Medium
   * - 2 = High
   * - 4 = Overheat
   * - 7 = Not Installed
   */
  if ((manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
       || manufacturer_id ==  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
      && product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTH)
    {
      switch (event_reading_type_code)
	{
	case 0x70:		/* OEM */
	  {
	    switch (sensor_type_code)
	      {
	      case 0xC0:	/* OEM */
		{
		  switch (sensor_event_bitmask)
		    {
		    case 0:
		      return (snprintf (buf, buflen, "Low"));
		    case 1:
		      return (snprintf (buf, buflen, "Medium"));
		    case 2:
		      return (snprintf (buf, buflen, "High"));
		    case 4:
		      return (snprintf (buf, buflen, "Overheat"));
		    case 7:
		      return (snprintf (buf, buflen, "Not Installed"));
		    }
		}
		break;
	      }
	  }
	  break;
	  /* end case 0x70: */
	}
    }

  SET_ERRNO (EINVAL);
  return (-1);
}
