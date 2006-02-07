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

const char *const ipmi_sensor_type_threshold_desc[] = 
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

const struct ipmi_discrete_desc ipmi_sensor_type_dummy_desc[] = 
  {
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_physical_security_desc[] =
  {
    {"General Chassis Intrusion", false}, 
    {"Drive Bay intrusion", false}, 
    {"I/O Card area intrusion", false}, 
    {"Processor area intrusion", false}, 
    {"LAN Leash Lost (system is unplugged from LAN)", false}, 
    {"Unauthorized dock/undock", false}, 
    {"FAN area intrusion", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_platform_security_violation_attempt[] =
  {
    {"Secure Mode (Front Panel Lockout) Violation attempt", false}, 
    {"Pre-boot Password Violation - user password", false}, 
    {"Pre-boot Password Violation attempt - setup password", false}, 
    {"Pre-boot Password Violation - network boot password", false}, 
    {"Other pre-boot Password Violation", false}, 
    {"Out-of-band Access Password Violation", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_processor[] =
  {
    {"IERR", false}, 
    {"Thermal Trip", false}, 
    {"FRB1/BIST failure", false}, 
    {"FRB2/Hang in POST failure", false}, 
    {"FRB3/Processor Startup/Initialization failure", false}, 
    {"Configuration Error", false}, 
    {"SM BIOS `Uncorrectable CPU-complex Error'", false}, 
    {"Processor Presence detected", true}, 
    {"Processor disabled", false}, 
    {"Terminator Presence Detected", true}, 
    {"Processor Automatically Throttled", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_power_supply[] =
  {
    {"Presence detected", true}, 
    {"Power Supply Failure detected", false}, 
    {"Predictive Failure", false}, 
    {"Power Supply AC input lost", false}, 
    {"Power Supply input AC lost or out-of-range", false}, 
    {"Power Supply input AC out-of-range, but present", false}, 
    {"Configuration error", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_power_unit[] =
  {
    {"Power Off/Power Down", false}, 
    {"Power Cycle", false}, 
    {"240VA Power Down", false}, 
    {"Interlock Power Down", false}, 
    {"AC lost", false}, 
    {"Soft Power Control Failure", false}, 
    {"Power Unit Failure detected", false}, 
    {"Predictive Failure", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_memory[] =
  {
    {"Correctable ECC/other correctable memory error", false}, 
    {"Uncorrectable ECC/other uncorrectable memory error", false}, 
    {"Parity", false}, 
    {"Memory Scrub Failed (stuck bit)", false}, 
    {"Memory Device Disabled", false}, 
    {"Correctable ECC/other correctable memory error logging limit reached", false}, 
    {"Presence detected", true}, 
    {"Configuration error", false}, 
    {"Spare", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_system_firmware_progress[] =
  {
    {"System Firmware Error (POST Error)", false}, 
    {"System Firmware Hang", false}, 
    {"System Firmware Progress", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_event_logging_disabled[] =
  {
    {"Correctable Memory Error Logging Disabled", false}, 
    {"Event `Type' Logging Disabled", false}, 
    {"Log Area Reset/Cleared", false}, 
    {"All Event Logging Disabled", false}, 
    {"SEL Full", false}, 
    {"SEL Almost Full", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_watchdog_1[] =
  {
    {"BIOS Watchdog Reset", false}, 
    {"OS Watchdog Reset", false}, 
    {"OS Watchdog Shut Down", false}, 
    {"OS Watchdog Power Down", false}, 
    {"OS Watchdog Power Cycle", false}, 
    {"OS Watchdog NMI/Diagnostic Interrupt", false}, 
    {"OS Watchdog Expired, status only", false}, 
    {"OS Watchdog pre-timeout Interrupt, non-NMI", false}, 
    {NULL, 0}
  };


const struct ipmi_discrete_desc ipmi_sensor_type_system_event[] =
  {
    {"System Reconfigured", false}, 
    {"OEM System Boot Event", false}, 
    {"Undetermined system hardware failure", false}, 
    {"Entry added to Auxiliary Log", false}, 
    {"PEF Action", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_critical_interrupt[] =
  {
    {"Front Panel NMI/Diagnostic Interrupt", false}, 
    {"Bus Timeout", false}, 
    {"I/O channel check NMI", false}, 
    {"Software NMI", false}, 
    {"PCI PERR", false}, 
    {"PCI SERR", false}, 
    {"EISA Fail Safe Timeout", false}, 
    {"Bus Correctable Error", false}, 
    {"Bus Uncorrectable Error", false}, 
    {"Fatal NMI (port 61h, bit 7)", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_button_switch[] =
  {
    {"Power Button pressed", false}, 
    {"Sleep Button pressed", false}, 
    {"Reset Button pressed", false}, 
    {"FRU latch open", false}, 
    {"FRU service request button ", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_chip_set[] =
  {
    {"Soft Power Control Failure", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_system_boot_initiated[] =
  {
    {"Initiated by power up", false}, 
    {"Initiated by hard reset", false}, 
    {"Initiated by warm reset", false}, 
    {"User requested PXE boot", false}, 
    {"Automatic boot to diagnostic", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_boot_error[] =
  {
    {"No bootable media", false}, 
    {"Non-bootable diskette left in drive", false}, 
    {"PXE Server not found", false}, 
    {"Invalid boot sector", false}, 
    {"Timeout waiting for user selection of boot source", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_os_boot[] =
  {
    {"A: boot completed", false}, 
    {"C: boot completed", false}, 
    {"PXE boot completed", false}, 
    {"Diagnostic boot completed", false}, 
    {"CD-ROM boot completed", false}, 
    {"ROM boot completed", false}, 
    {"boot completed - boot device not specified", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_os_critical_stop[] =
  {
    {"Stop during OS load/initialization", false}, 
    {"Run-time Stop", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_slot_connector[] =
  {
    {"Fault Status asserted", false}, 
    {"Identify Status asserted", false}, 
    {"Slot/Connector Device installed/attached", true}, 
    {"Slot/Connector Ready for Device Installation", true}, 
    {"Slot/Connector Ready for Device Removal", true}, 
    {"Slot Power is Off", false}, 
    {"Slot/Connector Device Removal Request", false}, 
    {"Interlock asserted", false}, 
    {"Slot is Disabled", false}, 
    {"Slot holds spare device", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_system_acpi_power_state[] =
  {
    {"S0/G0 working", true}, 
    {"S1 sleeping with system h/w & processor context maintained", false}, 
    {"S2 sleeping, processor context lost", false}, 
    {"S3 sleeping, processor & h/w context lost, memory retained", false}, 
    {"S4 non-volatile sleep/suspend-to disk", false}, 
    {"S5/G2 soft-off", false}, 
    {"S4/S5 soft-off, particular S4/S5 state cannot be determined", false}, 
    {"G3/Mechanical Off", false}, 
    {"Sleeping in an S1, S2, or S3 states", false}, 
    {"G1 sleeping", false}, 
    {"S5 entered by override", false}, 
    {"Legacy ON state", false}, 
    {"Legacy OFF state", false}, 
    {"Unknown", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_watchdog_2[] =
  {
    {"Timer expired, status only (no action, no interrupt)", false}, 
    {"Hard Reset", false}, 
    {"Power Down", false}, 
    {"Power Cycle", false}, 
    {"reserved", false}, 
    {"reserved", false}, 
    {"reserved", false}, 
    {"reserved", false}, 
    {"Timer interrupt", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_platform_alert[] =
  {
    {"platform generated page", false}, 
    {"platform generated LAN alert", false}, 
    {"Platform Event Trap generated, formatted per IPMI PET", false}, 
    {"platform generated SNMP trap, OEM format", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_entity_presence[] =
  {
    {"Entity Present", true}, 
    {"Entity Absent", false}, 
    {"Entity Disabled", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_lan[] =
  {
    {"LAN Heartbeat Lost", false}, 
    {"LAN Heartbeat", true}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_management_subsystem_health[] =
  {
    {"sensor access degraded or unavailable", false}, 
    {"controller access degraded or unavailable", false}, 
    {"management controller off-line", false}, 
    {"management controller unavailable", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_battery[] =
  {
    {"battery low (predictive failure)", false}, 
    {"battery failed", false}, 
    {"battery presence detected", false}, 
    {NULL, 0}
  };


const struct ipmi_discrete_desc ipmi_sensor_type_session_audit[] =
  {
    {"Session Activated", false}, 
    {"Session Deactivated", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_version_change[] =
  {
    {"Hardware change detected with associated Entity", false}, 
    {"Firmware or software change detected with associated Entity", false}, 
    {"Hardware incompatibility detected with associated Entity", false}, 
    {"Firmware or software incompatibility detected with associated Entity", false}, 
    {"Entity is of an invalid or unsupported hardware version", false}, 
    {"Entity contains an invalid or unsupported firmware or software version", false}, 
    {"Hardware Change detected with associated Entity was successful", false}, 
    {"Software or F/W Change detected with associated Entity was successful", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc ipmi_sensor_type_fru_state[] =
  {
    {"FRU Not Installed", false}, 
    {"FRU Inactive", false}, 
    {"FRU Activation Requested", false}, 
    {"FRU Activation In Progress", false}, 
    {"FRU Active", true}, 
    {"FRU Deactivation Requested", false}, 
    {"FRU Deactivation In Progress", false}, 
    {"FRU Communication Lost", false}, 
    {NULL, 0}
  };

const struct ipmi_discrete_desc *const ipmi_sensor_type_desc_ptr[] = 
  {
    ipmi_sensor_type_dummy_desc, 
    ipmi_sensor_type_dummy_desc, 
    ipmi_sensor_type_dummy_desc, 
    ipmi_sensor_type_dummy_desc, 
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_physical_security_desc, 
    ipmi_sensor_type_platform_security_violation_attempt, 
    ipmi_sensor_type_processor,
    ipmi_sensor_type_power_supply,
    ipmi_sensor_type_power_unit,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_memory,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_system_firmware_progress,
    ipmi_sensor_type_event_logging_disabled,
    ipmi_sensor_type_watchdog_1,
    ipmi_sensor_type_system_event,
    ipmi_sensor_type_critical_interrupt,
    ipmi_sensor_type_button_switch,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_chip_set,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_system_boot_initiated,
    ipmi_sensor_type_boot_error,
    ipmi_sensor_type_os_boot,
    ipmi_sensor_type_os_critical_stop,
    ipmi_sensor_type_slot_connector,
    ipmi_sensor_type_system_acpi_power_state,
    ipmi_sensor_type_watchdog_2,
    ipmi_sensor_type_platform_alert,
    ipmi_sensor_type_entity_presence,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_lan, 
    ipmi_sensor_type_management_subsystem_health,
    ipmi_sensor_type_battery,
    ipmi_sensor_type_session_audit,
    ipmi_sensor_type_version_change,
    ipmi_sensor_type_fru_state,
    ipmi_sensor_type_dummy_desc,
    ipmi_sensor_type_dummy_desc,
    NULL 
  };

const char *const ipmi_event_reading_type_code_dummy_desc[] = 
  {
    NULL
  };

const char *const ipmi_event_reading_type_code_2_desc[] = 
  {
    "Transition to Idle", 
    "Transition to Active", 
    "Transition to Busy", 
    NULL
  };

const char *const ipmi_event_reading_type_code_3_desc[] = 
  {
    "State Deasserted", 
    "State Asserted", 
    NULL
  };

const char *const ipmi_event_reading_type_code_4_desc[] = 
  {
    "Predictive Failure deasserted", 
    "Predictive Failure asserted", 
    NULL
  }; 

const char *const ipmi_event_reading_type_code_5_desc[] = 
  {
    "Limit Not Exceeded", 
    "Limit Exceeded", 
    NULL
  }; 

const char *const ipmi_event_reading_type_code_6_desc[] = 
  {
    "Performance Met", 
    "Performance Lags", 
    NULL
  };

const char *const ipmi_event_reading_type_code_7_desc[] = 
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

const char *const ipmi_event_reading_type_code_8_desc[] = 
  {
    "Device Removed/Device Absent", 
    "Device Inserted/Device Present", 
    NULL
  };

const char *const ipmi_event_reading_type_code_9_desc[] = 
  {
    "Device Disabled", 
    "Device Enabled", 
    NULL
  };

const char *const ipmi_event_reading_type_code_10_desc[] = 
  {
    "transition to Running", 
    "transition to In Test", 
    "transition to Power Off", 
    "transition to On Line", 
    "transition to Off Line", 
    "transition to Off Duty", 
    "transition to Degraded", 
    "transition to Power Save", 
    NULL
  };

const char *const ipmi_event_reading_type_code_11_desc[] = 
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

const char *const ipmi_event_reading_type_code_12_desc[] = 
  {
    "D0 Power State", 
    "D1 Power State", 
    "D2 Power State", 
    "D3 Power State", 
    NULL
  };

const char *const *const ipmi_event_reading_type_code_desc_ptr[] = 
  {
    ipmi_event_reading_type_code_dummy_desc, 
    ipmi_event_reading_type_code_dummy_desc, 
    ipmi_event_reading_type_code_2_desc, 
    ipmi_event_reading_type_code_3_desc, 
    ipmi_event_reading_type_code_4_desc, 
    ipmi_event_reading_type_code_5_desc, 
    ipmi_event_reading_type_code_6_desc, 
    ipmi_event_reading_type_code_7_desc, 
    ipmi_event_reading_type_code_8_desc, 
    ipmi_event_reading_type_code_9_desc, 
    ipmi_event_reading_type_code_10_desc, 
    ipmi_event_reading_type_code_11_desc, 
    ipmi_event_reading_type_code_12_desc, 
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

int 
ipmi_sensor_threshold_health_check (double sensor_reading, 
				    double normal_min, 
				    double normal_max, 
				    fiid_obj_t data_rs)
{
  uint64_t val;
  
  if (!data_rs)
    {
      errno = EINVAL;
      return (-1);
    }

  fiid_obj_get (data_rs, 
		tmpl_get_sensor_threshold_reading_rs, 
		(uint8_t *)"status_comparison_lower_non_critical_threshold", 
		&val);
  if (val == 1)
    return 0;
      
  fiid_obj_get (data_rs, 
		tmpl_get_sensor_threshold_reading_rs, 
		(uint8_t *)"status_comparison_lower_critical_threshold", 
		&val);
  if (val == 1)
    return 0;
      
  fiid_obj_get (data_rs, 
		tmpl_get_sensor_threshold_reading_rs, 
		(uint8_t *)"status_comparison_lower_non_recoverable_threshold", 
		&val);
  if (val == 1)
    return 0;
      
  fiid_obj_get (data_rs, 
		tmpl_get_sensor_threshold_reading_rs, 
		(uint8_t *)"status_comparison_upper_non_critical_threshold", 
		&val);
  if (val == 1)
    return 0;
      
  fiid_obj_get (data_rs, 
		tmpl_get_sensor_threshold_reading_rs, 
		(uint8_t *)"status_comparison_upper_critical_threshold", 
		&val);
  if (val == 1)
    return 0;
  
  fiid_obj_get (data_rs, 
		tmpl_get_sensor_threshold_reading_rs, 
		(uint8_t *)"status_comparison_upper_non_recoverable_threshold", 
		&val);
  if (val == 1)
    return 0;
  
  if (normal_min != 0)
    {
      if (sensor_reading < normal_min)
	return 0;
      else 
	{
	  if (normal_max != 0)
	    {
	      if (sensor_reading > normal_max)
		return 0;
	    }
	}
    }
  
/*   if (sensor_reading < normal_min || sensor_reading > normal_max) */
/*     return 0; */
  return 1;
}

int 
ipmi_sensor_discrete_health_check (int sensor_type, fiid_obj_t data_rs)
{
  uint64_t val;
  char key[65];
  int i;
  struct ipmi_discrete_desc *discrete_sensor_desc;

  if (!data_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  discrete_sensor_desc = (struct ipmi_discrete_desc *) 
    ipmi_sensor_type_desc_ptr[sensor_type];
  
  switch (sensor_type)
    {
    case IPMI_SENSOR_TYPE_PROCESSOR:
      {
	uint8_t processor_present_flag = 0;
	uint8_t terminator_present_flag = 0;
	
	for (i = 0; discrete_sensor_desc[i].message != NULL; i++)
	  {
	    if (strcasecmp (discrete_sensor_desc[i].message, "reserved") == 0)
	      continue;
	    
	    if (i == 7)
	      {
		fiid_obj_get (data_rs, 
			      tmpl_get_sensor_discrete_reading_rs, 
			      (uint8_t *)"state_7_asserted", 
			      &val);
		processor_present_flag = val;
		continue;
	      }
	    
	    if (i == 9)
	      {
		fiid_obj_get (data_rs, 
			      tmpl_get_sensor_discrete_reading_rs, 
			      (uint8_t *)"state_9_asserted", 
			      &val);
		terminator_present_flag = val;
		continue;
	      }
	    
	    snprintf (key, 64, "state_%d_asserted", i);
	    fiid_obj_get (data_rs, 
			  tmpl_get_sensor_discrete_reading_rs, 
			  (uint8_t *)key, 
			  &val);
	    
	    if (discrete_sensor_desc[i].normal_code != val)
	      return 0;
	  }
	
	if (processor_present_flag == 1 && terminator_present_flag == 0)
	  return 1;
	return 0;
      }
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      {
	uint8_t device_installed_flag = 0;
	uint8_t power_off_flag = 0;
	
	for (i = 0; discrete_sensor_desc[i].message != NULL; i++)
	  {
	    if (strcasecmp (discrete_sensor_desc[i].message, "reserved") == 0)
	      continue;
	    
	    if (i == 2)
	      {
		fiid_obj_get (data_rs, 
			      tmpl_get_sensor_discrete_reading_rs, 
			      (uint8_t *)"state_2_asserted", 
			      &val);
		device_installed_flag = val;
		continue;
	      }
	    
	    if (i == 5)
	      {
		fiid_obj_get (data_rs, 
			      tmpl_get_sensor_discrete_reading_rs, 
			      (uint8_t *)"state_5_asserted", 
			      &val);
		power_off_flag = val;
		continue;
	      }
	    
	    snprintf (key, 64, "state_%d_asserted", i);
	    fiid_obj_get (data_rs, 
			  tmpl_get_sensor_discrete_reading_rs, 
			  (uint8_t *)key, 
			  &val);
	    
	    if (discrete_sensor_desc[i].normal_code != val)
	      return 0;
	  }
	
	if ((device_installed_flag == 0 && power_off_flag == 0) || 
	    (device_installed_flag == 1 && power_off_flag == 1))
	  return 0;
	return 1;
      }
    default:
      for (i = 0; discrete_sensor_desc[i].message != NULL; i++)
	{
	  if (strcasecmp (discrete_sensor_desc[i].message, "reserved") == 0)
	    continue;
	  
	  snprintf (key, 64, "state_%d_asserted", i);
	  fiid_obj_get (data_rs, 
			tmpl_get_sensor_discrete_reading_rs, 
			(uint8_t *)key, 
			&val);
	  
	  if (discrete_sensor_desc[i].normal_code != val)
	    return 0;
	}
      return 1;
    }
  return 1;
}

