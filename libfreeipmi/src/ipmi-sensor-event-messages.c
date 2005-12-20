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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include "freeipmi.h"

/**********************************************************/
/***********      event message functions   ***************/
/**********************************************************/
static char *
get_01_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Lower Non-critical - going low");
    case 0x01:
      return strdup ("Lower Non-critical - going high");
    case 0x02:
      return strdup ("Lower Critical - going low");
    case 0x03:
      return strdup ("Lower Critical - going high");
    case 0x04:
      return strdup ("Lower Non-recoverable - going low");
    case 0x05:
      return strdup ("Lower Non-recoverable - going high");
    case 0x06:
      return strdup ("Upper Non-critical - going low");
    case 0x07:
      return strdup ("Upper Non-critical - going high");
    case 0x08:
      return strdup ("Upper Critical - going low");
    case 0x09:
      return strdup ("Upper Critical - going high");
    case 0x0A:
      return strdup ("Upper Non-recoverable - going low");
    case 0x0B:
      return strdup ("Upper Non-recoverable - going high");
    }
  
  return NULL;
}

static char *
get_02_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Transition to Idle");
    case 0x01:
      return strdup ("Transition to Active");
    case 0x02:
      return strdup ("Transition to Busy");
    }
  
  return NULL;
}

static char *
get_03_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("State Deasserted");
    case 0x01:
      return strdup ("State Asserted");
    }
  
  return NULL;
}

static char *
get_04_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Predictive Failure deasserted");
    case 0x01:
      return strdup ("Predictive Failure asserted");
    }
  
  return NULL;
}

static char *
get_05_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Limit Not Exceeded");
    case 0x01:
      return strdup ("Limit Exceeded");
    }
  
  return NULL;
}

static char *
get_06_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Performance Met");
    case 0x01:
      return strdup ("Performance Lags");
    }
  
  return NULL;
}

static char *
get_07_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("transition to OK");
    case 0x01:
      return strdup ("transition to Non-Critical from OK");
    case 0x02:
      return strdup ("transition to Critical from less severe");
    case 0x03:
      return strdup ("transition to Non-recoverable from less severe");
    case 0x04:
      return strdup ("transition to Non-Critical from more severe");
    case 0x05:
      return strdup ("transition to Critical from Non-recoverable");
    case 0x06:
      return strdup ("transition to Non-recoverable");
    case 0x07:
	return strdup ("Monitor");
    case 0x08:
      return strdup ("Informational");
    }
  
  return NULL;
}

static char *
get_08_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Device Removed/Device Absent");
    case 0x01:
	return strdup ("Device Inserted/Device Present");
    }
  
  return NULL;
}

static char *
get_09_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Device Disabled");
    case 0x01:
      return strdup ("Device Enabled");
    }
  
  return NULL;
}

static char *
get_0A_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("transition to Running");
    case 0x01:
      return strdup ("transition to In Test");
    case 0x02:
      return strdup ("transition to Power Off");
    case 0x03:
      return strdup ("transition to On Line");
    case 0x04:
      return strdup ("transition to Off Line");
    case 0x05:
      return strdup ("transition to Off Duty");
    case 0x06:
      return strdup ("transition to Degraded");
    case 0x07:
      return strdup ("transition to Power Save");
    case 0x08:
      return strdup ("Install Error");
    }
  
  return NULL;
}

static char *
get_0B_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Fully Redundant (formerly \"Redundancy Regained\")");
    case 0x01:
      return strdup ("Redundancy Lost");
    case 0x02:
      return strdup ("Redundancy Degraded");
    case 0x03:
      return strdup ("Entered from Redundancy Degraded or Fully Redundant");
    case 0x04:
      return strdup ("Entered from Non-redundant:Insufficient Resources");
    case 0x05:
      return strdup ("Non-redundant:Insufficient Resources");
    case 0x06:
      return strdup ("Redundancy Degraded from Fully Redundant");
    case 0x07:
      return strdup ("Redundancy Degraded from Non-redundant");
    }
  
  return NULL;
}

static char *
get_0C_generic_event_message (uint16_t offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("D0 Power State");
    case 0x01:
      return strdup ("D1 Power State");
    case 0x02:
      return strdup ("D2 Power State");
    case 0x03:
      return strdup ("D3 Power State");
    }
  
  return NULL;
}

/**********************************************************/
/***********      event message functions   ***************/
/**********************************************************/
static char *
get_01_event_message (int offset)
{
  return strdup ("Temperature");
}

static char *
get_02_event_message (int offset)
{
  return strdup ("Voltage");
}

static char *
get_03_event_message (int offset)
{
  return strdup ("Current");
}

static char *
get_04_event_message (int offset)
{
  return strdup ("Fan");
}

static char *
get_05_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("General Chassis Intrusion");
    case 0x01:
      return strdup ("Drive Bay intrusion");
    case 0x02:
      return strdup ("I/O Card area intrusion");
    case 0x03:
      return strdup ("Processor area intrusion");
    case 0x04:
      return strdup ("LAN Leash Lost (system is unplugged from LAN)");
    case 0x05:
      return strdup ("Unauthorized dock/undock");
    case 0x06:
      return strdup (" FAN area intrusion (supports detection of hot plug fan tampering)");
    }
  
  return NULL;
}

static char *
get_06_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Secure Mode (Front Panel Lockout) Violation attempt");
    case 0x01:
      return strdup ("Pre-boot Password Violation - user password");
    case 0x02:
      return strdup ("Pre-boot Password Violation attempt - setup password");
    case 0x03:
      return strdup ("Pre-boot Password Violation - network boot password");
    case 0x04:
      return strdup ("Other pre-boot Password Violation");
    case 0x05:
      return strdup ("Out-of-band Access Password Violation");
    }
  
  return NULL;
}

static char *
get_07_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("IERR");
    case 0x01:
      return strdup ("Thermal Trip");
    case 0x02:
      return strdup ("FRB1/BIST failure");
    case 0x03:
      return strdup ("FRB2/Hang in POST failure (used hang is believed to be due or related to a processor failure. Use System Firmware Progress sensor for other BIOS hangs.)");
    case 0x04:
      return strdup ("FRB3/Processor Startup/Initialization failure (CPU didn't start)");
    case 0x05:
      return strdup ("Configuration Error");
    case 0x06:
      return strdup ("SM BIOS `Uncorrectable CPU-complex Error'");
    case 0x07:
      return strdup ("Processor Presence detected");
    case 0x08:
      return strdup ("Processor disabled");
    case 0x09:
      return strdup ("Terminator Presence Detected");
    case 0x0A:
      return strdup ("Processor Automatically Throttled (processor throttling triggered by a hardware-based mechanism operating independent from system software, such as automatic thermal throttling or throttling to limit power consumption.)");
    }
  
  return NULL;
}

static char *
get_08_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Presence detected");
    case 0x01:
      return strdup ("Power Supply Failure detected");
    case 0x02:
      return strdup ("Predictive Failure");
    case 0x03:
      return strdup ("Power Supply input lost (AC/DC)");
    case 0x04:
      return strdup ("Power Supply input lost or out-of-range");
    case 0x05:
      return strdup ("Power Supply input out-of-range, but present");
    case 0x06:
      return strdup ("Configuration error");
    }
  
  return NULL;
}

static char *
get_09_event_message (int offset)
{
  switch (offset)
    {
    case 0x00: 
      return strdup ("Power Off/Power Down");
    case 0x01: 
      return strdup ("Power Cycle");
    case 0x02: 
      return strdup ("240VA Power Down");
    case 0x03: 
      return strdup ("Interlock Power Down");
    case 0x04: 
      return strdup ("AC lost");
    case 0x05: 
      return strdup ("Soft Power Control Failure (unit did not respond to request to turn on)");
    case 0x06: 
      return strdup ("Power Unit Failure detected");
    case 0x07: 
      return strdup ("Predictive Failure");
    }
  
  return NULL;
}

static char *
get_0C_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Correctable ECC/other correctable memory error");
    case 0x01:
      return strdup ("Uncorrectable ECC/other uncorrectable memory error");
    case 0x02:
      return strdup ("Parity");
    case 0x03:
      return strdup ("Memory Scrub Failed (stuck bit)");
    case 0x04:
      return strdup ("Memory Device Disabled");
    case 0x05:
      return strdup ("Correctable ECC/other correctable memory error logging limit reached");
    case 0x06:
      return strdup ("Presence detected");
    case 0x07:
      return strdup ("Configuration error");
    case 0x08:
      return strdup ("Spare");
    }
  
  return NULL;
}

static char *
get_0F_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("System Firmware Error (POST Error)");
    case 0x01:
      return strdup ("System Firmware Hang");
    case 0x02:
      return strdup ("System Firmware Progress");
    }
  
  return NULL;
}

static char *
get_10_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Correctable Memory Error Logging Disabled");
    case 0x01:
      return strdup ("Event `Type' Logging Disabled");
    case 0x02:
      return strdup ("Log Area Reset/Cleared");
    case 0x03:
      return strdup ("All Event Logging Disabled");
    case 0x04:
      return strdup ("SEL Full");
    case 0x05:
      return strdup ("SEL Almost Full");
    }
  
  return NULL;
}

static char *
get_11_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("BIOS Watchdog Reset");
    case 0x01:
      return strdup ("OS Watchdog Reset");
    case 0x02:
      return strdup ("OS Watchdog Shut Down");
    case 0x03:
      return strdup ("OS Watchdog Power Down");
    case 0x04:
      return strdup ("OS Watchdog Power Cycle");
    case 0x05:
      return strdup ("OS Watchdog NMI/Diagnostic Interrupt");
    case 0x06:
      return strdup ("OS Watchdog Expired, status only");
    case 0x07:
      return strdup ("OS Watchdog pre-timeout Interrupt, non-NMI");
    }
  
  return NULL;
}

static char *
get_12_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("System Reconfigured");
    case 0x01:
      return strdup ("OEM System Boot Event");
    case 0x02:
      return strdup ("Undetermined system hardware failure");
    case 0x03:
      return strdup ("Entry added to Auxiliary Log");
    case 0x04:
      return strdup ("PEF Action");
    case 0x05:
      return strdup ("Timestamp Clock Synch");
    }
  
  return NULL;
}

static char *
get_13_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Front Panel NMI/Diagnostic Interrupt");
    case 0x01:
      return strdup ("Bus Timeout");
    case 0x02:
      return strdup ("I/O channel check NMI");
    case 0x03:
      return strdup ("Software NMI");
    case 0x04:
      return strdup ("PCI PERR");
    case 0x05:
      return strdup ("PCI SERR");
    case 0x06:
      return strdup ("EISA Fail Safe Timeout");
    case 0x07:
      return strdup ("Bus Correctable Error");
    case 0x08:
      return strdup ("Bus Uncorrectable Error");
    case 0x09:
      return strdup ("Fatal NMI (port 61h, bit 7)");
    }
  
  return NULL;
}

static char *
get_14_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Power Button pressed");
    case 0x01:
      return strdup ("Sleep Button pressed");
    case 0x02:
      return strdup ("Reset Button pressed");
    case 0x03:
      return strdup ("FRU latch open (Switch indicating FRU latch is in `unlatched' position and FRU is mechanically removable)");
    case 0x04:
      return strdup ("FRU service request button (pressed, service, e.g. removal/replacement, requested)");
    }
  
  return NULL;
}

static char *
get_19_event_message (int offset)
{
  switch (offset)
    {
    case 0x00: 
      return strdup ("Soft Power Control Failure (chipset did not respond to BMC request to change system power state)"); 
    }
  
  return NULL;
}


static char *
get_1D_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Initiated by power up");
    case 0x01:
      return strdup ("Initiated by hard reset");
    case 0x02:
      return strdup ("Initiated by warm reset");
    case 0x03:
      return strdup ("User requested PXE boot");
    case 0x04:
      return strdup ("Automatic boot to diagnostic");
    }
  
  return NULL;
}

static char *
get_1E_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("No bootable media");
    case 0x01:
      return strdup ("Non-bootable diskette left in drive");
    case 0x02:
      return strdup ("PXE Server not found");
    case 0x03:
      return strdup ("Invalid boot sector");
    case 0x04:
      return strdup ("Timeout waiting for user selection of boot source");
    }
  
  return NULL;
}

static char *
get_1F_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("A: boot completed");
    case 0x01:
      return strdup ("C: boot completed");
    case 0x02:
      return strdup ("PXE boot completed");
    case 0x03:
      return strdup ("Diagnostic boot completed");
    case 0x04:
      return strdup ("CD-ROM boot completed");
    case 0x05:
      return strdup ("ROM boot completed");
    case 0x06:
      return strdup ("boot completed - boot device not specified");
    }
  
  return NULL;
}

static char *
get_20_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Stop during OS load/initialization");
    case 0x01:
      return strdup ("Run-time Stop");
    }
  
  return NULL;
}

static char *
get_21_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Fault Status asserted");
    case 0x01:
      return strdup ("Identify Status asserted");
    case 0x02:
      return strdup ("Slot/Connector Device installed/attached");
    case 0x03:
      return strdup ("Slot/Connector Ready for Device Installation");
    case 0x04:
      return strdup ("Slot/Connector Ready for Device Removal");
    case 0x05:
      return strdup ("Slot Power is Off");
    case 0x06:
      return strdup ("Slot/Connector Device Removal Request");
    case 0x07:
      return strdup ("Interlock asserted");
    case 0x08:
      return strdup ("Slot is Disabled");
    case 0x09:
      return strdup ("Slot holds spare device");
    }
  
  return NULL;
}

static char *
get_22_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("S0/G0 \"working\"");
    case 0x01:
      return strdup ("S1 \"sleeping with system h/w & processor context maintained\"");
    case 0x02:
      return strdup ("S2 \"sleeping, processor context lost\"");
    case 0x03:
      return strdup ("S3 \"sleeping, processor & h/w context lost, memory retained.\"");
    case 0x04:
      return strdup ("S4 \"non-volatile sleep/suspend-to disk\"");
    case 0x05:
      return strdup ("S5/G2 \"soft-off\"");
    case 0x06:
      return strdup ("S4/S5 soft-off, particular S4/S5 state cannot be determined");
    case 0x07:
      return strdup ("G3/Mechanical Off");
    case 0x08:
      return strdup ("Sleeping in an S1, S2, or S3 states (used when particular S1, S2, S3 state cannot be determined)");
    case 0x09:
      return strdup ("G1 sleeping (S1-S4 state cannot be determined)");
    case 0x0A:
      return strdup ("S5 entered by override");
    case 0x0B:
      return strdup ("Legacy ON state");
    case 0x0C:
      return strdup ("Legacy OFF state");
    case 0x0E:
      return strdup ("Unknown");
    }
  
  return NULL;
}

static char *
get_23_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Timer expired, status only (no action, no interrupt)");
    case 0x01:
      return strdup ("Hard Reset");
    case 0x02:
      return strdup ("Power Down");
    case 0x03:
      return strdup ("Power Cycle");
    case 0x08:
      return strdup ("Timer interrupt");
    }
  
  return NULL;
}

static char *
get_24_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("platform generated page");
    case 0x01:
      return strdup ("platform generated LAN alert");
    case 0x02:
      return strdup ("Platform Event Trap generated, formatted per IPMI PET specification");
    case 0x03:
      return strdup ("platform generated SNMP trap, OEM format");
    }
  
  return NULL;
}

static char *
get_25_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Entity Present");
    case 0x01:
      return strdup ("Entity Absent");
    case 0x02:
      return strdup ("Entity Disabled");
    }
  
  return NULL;
}

static char *
get_27_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("LAN Heartbeat Lost");
    case 0x01:
      return strdup ("LAN Heartbeat");
    }
  
  return NULL;
}

static char *
get_28_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("sensor access degraded or unavailable");
    case 0x01:
      return strdup ("controller access degraded or unavailable");
    case 0x02:
      return strdup ("management controller off-line");
    case 0x03:
      return strdup ("management controller unavailable");
    }
  
  return NULL;
}

static char *
get_29_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("battery low (predictive failure)");
    case 0x01:
      return strdup ("battery failed");
    case 0x02:
      return strdup ("battery presence detected");
    }
  
  return NULL;
}

static char *
get_2A_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Session Activated");
    case 0x01:
      return strdup ("Session Deactivated");
    }
  
  return NULL;
}

static char *
get_2B_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("Hardware change detected with associated Entity");
    case 0x01:
      return strdup ("Firmware or software change detected with associated Entity");
    case 0x02:
      return strdup ("Hardware incompatibility detected with associated Entity");
    case 0x03:
      return strdup ("Firmware or software incompatibility detected with associated Entity");
    case 0x04:
      return strdup ("Entity is of an invalid or unsupported hardware version");
    case 0x05:
      return strdup ("Entity contains an invalid or unsupported firmware or software version");
    case 0x06:
      return strdup ("Hardware Change detected with associated Entity was successful");
    case 0x07:
      return strdup ("Software or F/W Change detected with associated Entity was successful");
    }
  
  return NULL;
}

static char *
get_2C_event_message (int offset)
{
  switch (offset)
    {
    case 0x00:
      return strdup ("FRU Not Installed");
    case 0x01:
      return strdup ("FRU Inactive (in standby or `hot spare' state)");
    case 0x02:
      return strdup ("FRU Activation Requested");
    case 0x03:
      return strdup ("FRU Activation In Progress");
    case 0x04:
      return strdup ("FRU Active");
    case 0x05:
      return strdup ("FRU Deactivation Requested");
    case 0x06:
      return strdup ("FRU Deactivation In Progress");
    case 0x07:
      return strdup ("FRU Communication Lost");
    }
  
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
      switch (event_data)
	{
	case 0x00:
	  return strdup ("Unspecified");
	case 0x01:
	  return strdup ("No system memory is physically installed in the system");
	case 0x02:
	  return strdup ("No usable system memory, all installed memory has experienced an unrecoverable failure");
	case 0x03:
	  return strdup ("Unrecoverable hard-disk/ATAPI/IDE device failure");
	case 0x04:
	  return strdup ("Unrecoverable system-board failure");
	case 0x05:
	  return strdup ("Unrecoverable diskette subsystem failure");
	case 0x06: 
	  return strdup ("Unrecoverable hard-disk controller failure");
	case 0x07: 
	  return strdup ("Unrecoverable PS/2 or USB keyboard failure");
	case 0x08: 
	  return strdup ("Removable boot media not found");
	case 0x09: 
	  return strdup ("Unrecoverable video controller failure");
	case 0x0A: 
	  return strdup ("No video device detected");
	case 0x0B: 
	  return strdup ("Firmware (BIOS) ROM corruption detected");
	case 0x0C: 
	  return strdup ("CPU voltage mismatch (processors that share same supply have mismatched voltage requirements)");
	case 0x0D: 
	  return strdup ("CPU speed matching failure");
	default:
	  return NULL;
	}
    case 0x01:
    case 0x02:
      switch (event_data)
	{
	case 0x00:
	  return strdup ("Unspecified");
	case 0x01:
	  return strdup ("Memory initialization");
	case 0x02:
	  return strdup ("Hard-disk initialization");
	case 0x03:
	  return strdup ("Secondary processor(s) initialization");
	case 0x04:
	  return strdup ("User authentication");
	case 0x05:
	  return strdup ("User-initiated system setup");
	case 0x06:
	  return strdup ("USB resource configuration");
	case 0x07:
	  return strdup ("PCI resource configuration");
	case 0x08:
	  return strdup ("Option ROM initialization");
	case 0x09:
	  return strdup ("Video initialization");
	case 0x0A:
	  return strdup ("Cache initialization");
	case 0x0B:
	  return strdup ("SM Bus initialization");
	case 0x0C:
	  return strdup ("Keyboard controller initialization");
	case 0x0D:
	  return strdup ("Embedded controller/management controller initialization");
	case 0x0E:
	  return strdup ("Docking station attachment");
	case 0x0F:
	  return strdup ("Enabling docking station");
	case 0x10:
	  return strdup ("Docking station ejection");
	case 0x11:
	  return strdup ("Disabling docking station");
	case 0x12:
	  return strdup ("Calling operating system wake-up vector");
	case 0x13:
	  return strdup ("Starting operating system boot process, e.g. calling Int 19h");
	case 0x14:
	  return strdup ("Baseboard or motherboard initialization");
	case 0x16:
	  return strdup ("Floppy initialization");
	case 0x17:
	  return strdup ("Keyboard test");
	case 0x18:
	  return strdup ("Pointing device test");
	case 0x19:
	  return strdup ("Primary processor initialization");
	default:
	  return NULL;
	}
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
	    {4, "log_type"}, 
	    {4, "log_entry_action"}, 
	    {0, ""}
	  };
	uint64_t val;
	uint8_t log_type;
	uint8_t log_entry_action;
	char *str = NULL;
	char *str1 = NULL;
	char *str2 = NULL;
	
	fiid_obj_get (&event_data, tmpl_event_data2, "log_type", &val);
	log_type = val;
	fiid_obj_get (&event_data, tmpl_event_data2, "log_entry_action", &val);
	log_entry_action = val;
	
	switch (log_type)
	  {
	  case 0x00:
	    str1 = strdupa ("Log Type = MCA log");
	    break;
	  case 0x01:
	    str1 = strdupa ("Log Type = OEM1");
	    break;
	  case 0x02:
	    str1 = strdupa ("Log Type = OEM2");
	    break;
	  }
	
	switch (log_entry_action)
	  {
	  case 0x00:
	    str2 = strdupa ("Log entry action = entry added");
	    break;
	  case 0x01:
	    str2 = strdupa ("Log entry action = entry added because event did not be map to standard IPMI event");
	    break;
	  case 0x02:
	    str2 = strdupa ("Log entry action = entry added along with one or more corresponding SEL entries");
	    break;
	  case 0x03:
	    str2 = strdupa ("Log entry action = log cleared");
	    break;
	  case 0x04:
	    str2 = strdupa ("Log entry action = log disabled");
	    break;
	  case 0x05:
	    str2 = strdupa ("Log entry action = log enabled");
	    break;
	  }
	
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
	    {1, "alert"}, 
	    {1, "power_off"}, 
	    {1, "reset"}, 
	    {1, "power_cycle"}, 
	    {1, "oem_action"}, 
	    {1, "diagonstic_interrupt"}, 
	    {2, "reserved"}, 
	    {0, ""}
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
	
	fiid_obj_get (&event_data, tmpl_event_data2, "alert", &val);
	alert = val;
	fiid_obj_get (&event_data, tmpl_event_data2, "power_off", &val);
	power_off = val;
	fiid_obj_get (&event_data, tmpl_event_data2, "reset", &val);
	reset = val;
	fiid_obj_get (&event_data, tmpl_event_data2, "power_cycle", &val);
	power_cycle = val;
	fiid_obj_get (&event_data, tmpl_event_data2, "oem_action", &val);
	oem_action = val;
	fiid_obj_get (&event_data, tmpl_event_data2, "diagonstic_interrupt", &val);
	diagnostic_interrupt = val;
	
	if (alert)
	  {
	    tmp_str = str;
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; Alert", tmp_str);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "Alert");
	  }
	
	if (power_off)
	  {
	    tmp_str = str;
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; Power off", tmp_str);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "Power off");
	  }
	
	if (reset)
	  {
	    tmp_str = str;
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; Reset", tmp_str);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "Reset");
	  }
	
	if (power_cycle)
	  {
	    tmp_str = str;
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; Power cycle", tmp_str);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "Power cycle");
	  }
	
	if (oem_action)
	  {
	    tmp_str = str;
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; OEM action", tmp_str);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "OEM action");
	  }
	
	if (diagnostic_interrupt)
	  {
	    tmp_str = str;
	    if (str)
	      {
		str = NULL;
		asprintf (&str, "%s; Diagnostic interrupt(NMI)", tmp_str);
		free (tmp_str);
	      }
	    else
	      asprintf (&str, "Diagnostic interrupt");
	  }
	
	return str;
      }
    case 0x05:
      {
	fiid_template_t tmpl_event_data2 = 
	  {
	    {4, "timestamp_clock_type"}, 
	    {3, "reserved"}, 
	    {1, "first_second"}, 
	    {0, ""}
	  };
	uint64_t val;
	uint8_t timestamp_clock_type;
	uint8_t first_second;
	char *str = NULL;
	
	fiid_obj_get (&event_data, tmpl_event_data2, "timestamp_clock_type", &val);
	timestamp_clock_type = val;
	fiid_obj_get (&event_data, tmpl_event_data2, "first_second", &val);
	first_second = val;
	
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
      switch (event_data)
	{
	case 0x00:
	  return strdup ("Requested power state = S0/G0 \"working\"");
	case 0x01:
	  return strdup ("Requested power state = S1 \"sleeping with system h/w & processor context maintained\"");
	case 0x02:
	  return strdup ("Requested power state = S2 \"sleeping, processor context lost\"");
	case 0x03:
	  return strdup ("Requested power state = S3 \"sleeping, processor & h/w context lost, memory retained.\"");
	case 0x04:
	  return strdup ("Requested power state = S4 \"non-volatile sleep/suspend-to disk\"");
	case 0x05:
	  return strdup ("Requested power state = S5/G2 \"soft-off\"");
	case 0x06:
	  return strdup ("Requested power state = S4/S5 soft-off, particular S4/S5 state cannot be determined");
	case 0x07:
	  return strdup ("Requested power state = G3/Mechanical Off");
	case 0x08:
	  return strdup ("Requested power state = Sleeping in an S1, S2, or S3 states (used when particular S1, S2, S3 state cannot be determined)");
	case 0x09:
	  return strdup ("Requested power state = G1 sleeping (S1-S4 state cannot be determined)");
	case 0x0A:
	  return strdup ("Requested power state = S5 entered by override");
	case 0x0B:
	  return strdup ("Requested power state = Legacy ON state");
	case 0x0C:
	  return strdup ("Requested power state = Legacy OFF state");
	}
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
	  {6, "slot_connector_type"}, 
	  {1, "reserved"}, 
	  {0, ""}
	};
      uint64_t val;
      
      fiid_obj_get (&event_data, tmpl_event_data2, "slot_connector_type", &val);
      
      switch (val)
	{
	case 0x00:
	  return strdup ("Slot/Connector Type = PCI");
	case 0x01:
	  return strdup ("Slot/Connector Type = Drive Array");
	case 0x02:
	  return strdup ("Slot/Connector Type = External Peripheral Connector");
	case 0x03:
	  return strdup ("Slot/Connector Type = Docking");
	case 0x04:
	  return strdup ("Slot/Connector Type = other standard internal expansion slot");
	case 0x05:
	  return strdup ("Slot/Connector Type = slot associated with entity specified by Entity ID for sensor");
	case 0x06:
	  return strdup ("Slot/Connector Type = AdvancedTCA");
	case 0x07:
	  return strdup ("Slot/Connector Type = DIMM/memory device");
	case 0x08:
	  return strdup ("Slot/Connector Type = FAN");
	}
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
	  {4, "timer_at_expiration"}, 
	  {4, "interrupt_type"}, 
	  {0, ""}
	};
      uint64_t val;
      uint8_t timer_at_expiration;
      uint8_t interrupt_type;
      char *str = NULL;
      char *str1 = NULL;
      char *str2 = NULL;
      
      fiid_obj_get (&event_data, tmpl_event_data2, "timer_at_expiration", &val);
      timer_at_expiration = val;
      fiid_obj_get (&event_data, tmpl_event_data2, "interrupt_type", &val);
      interrupt_type = val;
      switch (timer_at_expiration)
	{
	case 0x01:
	  str1 = strdupa ("Timer use at expiration = BIOS FRB2");
	  break;
	case 0x02:
	  str1 = strdupa ("Timer use at expiration = BIOS/POST");
	  break;
	case 0x03:
	  str1 = strdupa ("Timer use at expiration = OS Load");
	  break;
	case 0x04:
	  str1 = strdupa ("Timer use at expiration = SMS/OS");
	  break;
	case 0x05:
	  str1 = strdupa ("Timer use at expiration = OEM");
	  break;
	case 0x0F:
	  str1 = strdupa ("Timer use at expiration = unspecified");
	  break;
	}
      switch (interrupt_type)
	{
	case 0x00:
	  str2 = strdupa ("Interrupt type = none");
	  break;
	case 0x01:
	  str2 = strdupa ("Interrupt type = SMI");
	  break;
	case 0x02:
	  str2 = strdupa ("Interrupt type = NMI");
	  break;
	case 0x03:
	  str2 = strdupa ("Interrupt type = Messaging Interrupt");
	  break;
	case 0x0F:
	  str2 = strdupa ("Interrupt type = unspecified");
	  break;
	}
      
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
get_2A_event_data2_message (int offset, uint8_t event_data)
{
  if (offset == 0x01)
    {
      fiid_template_t tmpl_event_data2 = 
	{
	  {6, "user_id"}, 
	  {2, "reserved"}, 
	  {0, ""}
	};
      uint64_t val;
      char *str = NULL;
      
      fiid_obj_get (&event_data, tmpl_event_data2, "timer_at_expiration", &val);
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
      switch (event_data)
	{
	case 0x00:
	  return strdup ("Version change type = unspecified");
	case 0x01:
	  return strdup ("Version change type = management controller device ID (change in one or more fields from `Get Device ID')");
	case 0x02:
	  return strdup ("Version change type = management controller firmware revision");
	case 0x03:
	  return strdup ("Version change type = management controller device revision");
	case 0x04:
	  return strdup ("Version change type = management controller manufacturer ID");
	case 0x05:
	  return strdup ("Version change type = management controller IPMI version");
	case 0x06:
	  return strdup ("Version change type = management controller auxiliary firmware ID");
	case 0x07:
	  return strdup ("Version change type = management controller firmware boot block");
	case 0x08:
	  return strdup ("Version change type = other management controller firmware");
	case 0x09:
	  return strdup ("Version change type = system firmware (EFI/BIOS) change");
	case 0x0A:
	  return strdup ("Version change type = SMBIOS change");
	case 0x0B:
	  return strdup ("Version change type = operating system change");
	case 0x0C:
	  return strdup ("Version change type = operating system loader change");
	case 0x0D:
	  return strdup ("Version change type = service or diagnostic partition change");
	case 0x0E:
	  return strdup ("Version change type = management software agent change");
	case 0x0F:
	  return strdup ("Version change type = management software application change");
	case 0x10:
	  return strdup ("Version change type = management software middleware change");
	case 0x11:
	  return strdup ("Version change type = programmable hardware change (e.g. FPGA)");
	case 0x12:
	  return strdup ("Version change type = board/FRU module change (change of a module plugged into associated entity)");
	case 0x13:
	  return strdup ("Version change type = board/FRU component change (addition or removal of a replaceable component on the board/FRU that is not tracked as a FRU)");
	case 0x14:
	  return strdup ("Version change type = board/FRU replaced with equivalent version");
	case 0x15:
	  return strdup ("Version change type = board/FRU replaced with newer version");
	case 0x16:
	  return strdup ("Version change type = board/FRU replaced with older version");
	case 0x17:
	  return strdup ("Version change type = board/FRU hardware configuration change (e.g. strap, jumper, cable change, etc.)");
	}
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
	  {4, "previous_state_offset"}, 
	  {4, "cause_of_state_change"}, 
	  {0, ""}
	};
      uint64_t val;
      uint8_t previous_state_offset;
      uint8_t cause_of_state_change;
      char *str = NULL;
      
      fiid_obj_get (&event_data, tmpl_event_data2, "previous_state_offset", &val);
      previous_state_offset = val;
      fiid_obj_get (&event_data, tmpl_event_data2, "cause_os_state_change", &val);
      cause_of_state_change = val;
      switch (cause_of_state_change)
	{
	case 0x00: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = Normal State Change", previous_state_offset);
	  return str;
	case 0x01: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = Change Commanded by software external to FRU", previous_state_offset);
	  return str;
	case 0x02: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = State Change due to operator changing a Handle latch", previous_state_offset);
	  return str;
	case 0x03: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = State Change due to operator pressing the hotswap push button", previous_state_offset);
	  return str;
	case 0x04: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = State Change due to FRU programmatic action", previous_state_offset);
	  return str;
	case 0x05: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = Communication Lost", previous_state_offset);
	  return str;
	case 0x06: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = Communication Lost due to local failure", previous_state_offset);
	  return str;
	case 0x07: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = State Change due to unexpected extraction", previous_state_offset);
	  return str;
	case 0x08: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = State Change due to operator intervention/update", previous_state_offset);
	  return str;
	case 0x09: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = Unable to compute IPMB address", previous_state_offset);
	  return str;
	case 0x0A: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = Unexpected Deactivation", previous_state_offset);
	  return str;
	case 0x0F: 
	  asprintf (&str, "Previous state offset value = %d; Cause of state change = State Change, Cause Unknown", previous_state_offset);
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
	  {4, "event_type"}, 
	  {4, "reserved"}, 
	  {0, ""}
	};
      uint64_t val;
      
      fiid_obj_get (&event_data, tmpl_event_data3, "event_type", &val);
      switch (val)
	{
	case 0x0:
	  return strdup ("Vendor mismatch");
	case 0x1:
	  return strdup ("Revision mismatch");
	case 0x2:
	  return strdup ("Processor missing or unexpected/unsupported condition");
	}
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
	    {4, "event_offset"}, 
	    {1, "assertion_deassertion_event"}, 
	    {1, "logging_disabled_all_events"}, 
	    {2, "reserved"}, 
	    {0, ""}
	  };
	uint64_t val;
	uint8_t event_offset;
	uint8_t assertion_deassertion_event;
	uint8_t logging_disabled_all_events;
	char *str = NULL;
	
	fiid_obj_get (&event_data, tmpl_event_data3, "event_offset", &val);
	event_offset = val;
	fiid_obj_get (&event_data, tmpl_event_data3, "assertion_deassertion_event", &val);
	assertion_deassertion_event = val;
	fiid_obj_get (&event_data, tmpl_event_data3, "logging_disabled_all_events", &val);
	logging_disabled_all_events = val;
	
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
      switch (event_data)
	{
	case 0x00:
	  return strdup ("Power state at time of request = S0/G0 \"working\"");
	case 0x01:
	  return strdup ("Power state at time of request = S1 \"sleeping with system h/w & processor context maintained\"");
	case 0x02:
	  return strdup ("Power state at time of request = S2 \"sleeping, processor context lost\"");
	case 0x03:
	  return strdup ("Power state at time of request = S3 \"sleeping, processor & h/w context lost, memory retained.\"");
	case 0x04:
	  return strdup ("Power state at time of request = S4 \"non-volatile sleep/suspend-to disk\"");
	case 0x05:
	  return strdup ("Power state at time of request = S5/G2 \"soft-off\"");
	case 0x06:
	  return strdup ("Power state at time of request = S4/S5 soft-off, particular S4/S5 state cannot be determined");
	case 0x07:
	  return strdup ("Power state at time of request = G3/Mechanical Off");
	case 0x08:
	  return strdup ("Power state at time of request = Sleeping in an S1, S2, or S3 states (used when particular S1, S2, S3 state cannot be determined)");
	case 0x09:
	  return strdup ("Power state at time of request = G1 sleeping (S1-S4 state cannot be determined)");
	case 0x0A:
	  return strdup ("Power state at time of request = S5 entered by override");
	case 0x0B:
	  return strdup ("Power state at time of request = Legacy ON state");
	case 0x0C:
	  return strdup ("Power state at time of request = Legacy OFF state");
	case 0x0D:
	  return strdup ("Power state at time of request = unknown");
	}
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
get_2A_event_data3_message (int offset, uint8_t event_data)
{
  if (offset == 0x01)
    {
      fiid_template_t tmpl_event_data3 = 
	{
	  {4, "channel_number"}, 
	  {2, "deactivation_cause"}, 
	  {2, "reserved"}, 
	  {0, ""}
	};
      uint64_t val;
      uint8_t channel_number;
      uint8_t deactivation_cause;
      char *str = NULL;
      
      fiid_obj_get (&event_data, tmpl_event_data3, "channel_number", &val);
      channel_number = val;
      fiid_obj_get (&event_data, tmpl_event_data3, "deactivation_cause", &val);
      deactivation_cause = val;
      
      switch (deactivation_cause)
	{
	case 0x00:
	  asprintf (&str, "Channel number that session was activated/deactivated = %d; Session deactivatation cause unspecified. This value is also used for Session Activated events", channel_number);
	  return str;
	case 0x01:
	  asprintf (&str, "Channel number that session was activated/deactivated = %d; Session deactivated by Close Session command", channel_number);
	  return str;
	case 0x02:
	  asprintf (&str, "Channel number that session was activated/deactivated = %d; Session deactivated by timeout", channel_number);
	  return str;
	case 0x03:
	  asprintf (&str, "Channel number that session was activated/deactivated = %d; Session deactivated by configuration change", channel_number);
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
    case 0x0F: return get_0F_event_message (offset);
    case 0x10: return get_10_event_message (offset);
    case 0x11: return get_11_event_message (offset);
    case 0x12: return get_12_event_message (offset);
    case 0x13: return get_13_event_message (offset);
    case 0x14: return get_14_event_message (offset);
    case 0x19: return get_19_event_message (offset);
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
    case 0x21: return get_21_event_data2_message (offset, event_data);
    case 0x23: return get_23_event_data2_message (offset, event_data);
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
    case 0x21: return get_21_event_data3_message (offset, event_data);
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
