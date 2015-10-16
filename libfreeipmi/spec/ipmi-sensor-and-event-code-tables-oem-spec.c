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

/***************************************
 * Generic Event Reading Strings (OEM) *
 ***************************************/

/*
 * Dell
 */

/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 * Dell Poweredge R720
 */

const char * const ipmi_generic_event_reading_type_code_oem_dell_status[] =
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
unsigned int ipmi_generic_event_reading_type_code_oem_dell_status_max_index = 0x08;

/*
 * Dell Poweredge R720
 */

const char * const ipmi_generic_event_reading_type_code_oem_dell_failure[] =
  {
    "undocumented",		/* not known yet */
    "undocumented",		/* not known yet */
    "undocumented",		/* not known yet */
    "Memory failed to transition to Online",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_oem_dell_failure_max_index = 0x03;

/*****************************
 * Sensor Type Strings (OEM) *
 *****************************/

/*
 * Dell
 */

/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 * Dell Poweredge R720
 */
/* achu:
 *
 * I have a feeling "good" is the random string they choose in some
 * code, it's not the real string.  But that's all I got to go on.
 *
 */

const char * const ipmi_sensor_type_oem_dell_system_performance_degradation_status[] =
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
unsigned int ipmi_sensor_type_oem_dell_system_performance_degradation_status_max_index = 0x07;

const char * const ipmi_sensor_type_oem_dell_link_tuning[] =
  {
    "Good",
    "Failed to program virtual MAC address",
    "Device option ROM failed to support link tuning or flex address",
    "Failed to get link tuning or flex address data",
    NULL
  };
unsigned int ipmi_sensor_type_oem_dell_link_tuning_max_index = 0x03;

const char * const ipmi_sensor_type_oem_dell_non_fatal_error[] =
  {
    "PCIe error",
    "undocumented",		/* not known yet */
    "QPI Link Degrade",
  };
unsigned int ipmi_sensor_type_oem_dell_non_fatal_error_max_index = 0x02;

const char * const ipmi_sensor_type_oem_dell_fatal_io_error[] =
  {
    "Successful",
    "Fatal IO error",
    NULL
  };
unsigned int ipmi_sensor_type_oem_dell_fatal_io_error_max_index = 0x01;

const char * const ipmi_sensor_type_oem_dell_upgrade[] =
  {
    "Successful",
    "Failed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_dell_upgrade_max_index = 0x01;

/*
 * Fuijitsu
 */

/*
 * iRMC S1 / iRMC S2
 */

/* 0xC0 / IPMI_SENSOR_TYPE_OEM_FUJITSU_I2C_BUS */
const char * const ipmi_sensor_type_oem_fujitsu_i2c_bus[] =
  {
    /* EN 0x00 */	"I2C Bus Error",
    /* EN 0x01 */	"I2C Bus OK",
    /* EN 0x02 */	"I2C Bus Disabled",
    /* EN 0x03 */	"I2C Bus Failed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_i2c_bus_max_index = 0x03;

/* 0xDD / IPMI_SENSOR_TYPE_OEM_FUJITSU_SYSTEM_POWER_CONSUMPTION */
const char * const ipmi_sensor_type_oem_fujitsu_system_power_consumption[] =
  {
    /* EN 0x00 */	"System Power Consumption within Limit",
    /* EN 0x01 */	"System Power Consumption above Warning Level",
    /* EN 0x02 */	"System Power Consumption above Critical Level",
    /* EN 0x03 */	"System Power Consumption limiting disabled",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_system_power_consumption_max_index = 0x03;


/* 0xDE / IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_STATUS */
const char * const ipmi_sensor_type_oem_fujitsu_memory_status[] =
  {
    /* EN 0x00 */	"Empty slot",
    /* EN 0x01 */	"OK",
    /* EN 0x02 */	"Reserved",
    /* EN 0x03 */	"Error",
    /* EN 0x04 */	"Fail",
    /* EN 0x05 */	"Prefailure",
    /* EN 0x06 */	"Reserved",
    /* EN 0x07 */	"Unknown",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_memory_status_max_index = 0x07;

/* 0xDF / IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_CONFIG */
const char * const ipmi_sensor_type_oem_fujitsu_memory_config[] =
  {
    /* EN 0x00 */	"Normal",
    /* EN 0x01 */	"Disabled",
    /* EN 0x02 */	"Spare module",
    /* EN 0x03 */	"Mirrored module",
    /* EN 0x04 */	"RAID module",
    /* EN 0x05 */	"Not Usable",
    /* EN 0x06 */	"Unspecified state(6)",
    /* EN 0x07 */	"Unspecified state(7)",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_memory_config_max_index = 0x07;

/* 0xE1 / IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY */
const char * const ipmi_sensor_type_oem_fujitsu_memory[] =
  {
    /* EN 0x00 */	"Non Fujitsu memory module detected",
    /* EN 0x01 */	"Memory module replaced",
    /* EN 0x02 */	"Fatal general memory error",
    /* EN 0x03 */	"Recoverable general memory error",
    /* EN 0x04 */	"Recoverable ECC memory error",
    /* EN 0x05 */	"Recoverable CRC memory error",
    /* EN 0x06 */	"Fatal CRC memory error",
    /* EN 0x07 */	"Recoverable thermal memory event",
    /* EN 0x08 */	"Fatal thermal memory error",
    /* EN 0x09 */	"Too many correctable memory errors",
    /* EN 0x0A */	"Uncorrectable Parity memory error",
    /* EN 0x0B */	"Memory Modules swapped",
    /* EN 0x0C */	"Memory Module moved",
    /* EN 0x0D */	"Memory removed",
    /* EN 0x0E */	"Memory Re-inserted",
    /* EN 0x0F */	"Memory module(s) changed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_memory_max_index = 0x0F;

/* 0xE3 / IPMI_SENSOR_TYPE_OEM_FUJITSU_HW_ERROR */
const char * const ipmi_sensor_type_oem_fujitsu_hw_error[] =
  {
    /* EN 0x00 */	"TPM Error",
    /* EN 0x01 */	"Reserved",
    /* EN 0x02 */	"No usable CPU",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_hw_error_max_index = 0x02;

/* 0xE4 / IPMI_SENSOR_TYPE_OEM_FUJITSU_SYS_ERROR */
const char * const ipmi_sensor_type_oem_fujitsu_sys_error[] =
  {
    /* EN 0x00 */	"System configuration Data error",
    /* EN 0x01 */	"Resource Conflict",                  /* Slot in EventData3 */
    /* EN 0x02 */	"IRQ not configured",                 /* Slot in EventData3 */
    /* EN 0x03 */	"Device node allocation error",       /* Device in EventData3 */
    /* EN 0x04 */	"Expansion ROM Slot not initialized", /* Slot in EventData3 */
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_sys_error_max_index = 0x04;

/* 0xE6 / IPMI_SENSOR_TYPE_OEM_FUJITSU_FAN_STATUS */
const char * const ipmi_sensor_type_oem_fujitsu_fan_status[] =
  {
    /* EN 0x00 */	"FAN on, running",
    /* EN 0x01 */	"FAN failed",
    /* EN 0x02 */	"FAN prefailure",
    /* EN 0x03 */	"Redundant FAN failed",
    /* EN 0x04 */	"FAN not manageable",
    /* EN 0x05 */	"FAN not installed",
    /* EN 0x06 */	"FAN unspecified state(6)",
    /* EN 0x07 */	"FAN in init phase",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_fan_status_max_index = 0x07;

/* 0xE8 / IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_STATUS */
const char * const ipmi_sensor_type_oem_fujitsu_psu_status[] =
  {
    /* EN 0x00 */	"Power supply - Not present",
    /* EN 0x01 */	"Power supply - OK",
    /* EN 0x02 */	"Power supply - Failed",
    /* EN 0x03 */	"Redundant power supply - AC failed",
    /* EN 0x04 */	"Redundant power supply - DC failed",
    /* EN 0x05 */	"Power supply - Critical Temperature",
    /* EN 0x06 */	"Power supply - Not manageable",
    /* EN 0x07 */	"Power supply - Fan failure predicted",
    /* EN 0x08 */	"Power supply - Fan failed",
    /* EN 0x09 */	"Power supply - Power Save Mode",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_psu_status_max_index = 0x09;

/* 0xE9 / IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_REDUNDANCY */
const char * const ipmi_sensor_type_oem_fujitsu_psu_redundancy[] =
  {
    /* EN 0x00 */	"Power Supply - redundancy present",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_psu_redundancy_max_index = 0x00;

/* 0xEC / IPMI_SENSOR_TYPE_OEM_FUJITSU_FLASH */
const char * const ipmi_sensor_type_oem_fujitsu_flash[] =
  {
    /* EN 0x00 */	"Online firmware flash",
    /* EN 0x01 */	"Online firmware flash: reboot",
    /* EN 0x02 */	"BIOS TFTP Flash: OK",
    /* EN 0x03 */	"BIOS TFTP Flash: failed",
    /* EN 0x04 */	"iRMC TFTP Flash: OK",
    /* EN 0x05 */	"iRMC TFTP Flash: failed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_flash_max_index = 0x05;

/* 0xEF / IPMI_SENSOR_TYPE_OEM_FUJITSU_CONFIG_BACKUP */
const char * const ipmi_sensor_type_oem_fujitsu_config_backup[] =
  {
    /* EN 0x00 */	"Chassis IDPROM: Motherboard Exchange detected",
    /* EN 0x01 */	"Chassis IDPROM: Read or Write error",
    /* EN 0x02 */	"Chassis IDPROM: Restore successful",
    /* EN 0x03 */	"Chassis IDPROM: Restore failed",
    /* EN 0x04 */	"Chassis IDPROM: Backup successful",
    /* EN 0x05 */	"Chassis IDPROM: Backup failed",
    /* EN 0x06 */	"Chassis IDPROM: Feature disabled",
    /* EN 0x07 */	"Chassis IDPROM: Function Not Available",
    /* EN 0x08 */	"Reserved",
    /* EN 0x09 */	"Reserved",
    /* EN 0x0A */	"Reserved",
    /* EN 0x0B */	"Reserved",
    /* EN 0x0C */	"Reserved",
    /* EN 0x0D */	"Reserved",
    /* EN 0x0E */	"Reserved",
    /* EN 0x0F */	"NVRAM defaults loaded",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_config_backup_max_index = 0x0F;

/*****************************
 * OEM Specific              *
 *****************************/

/*******************************************
 * HP                                      *
 *******************************************/

/*
 * HP Proliant DL160 G8
 */

const char * const ipmi_oem_hp_uid_light[] =
  {
    "on",
    "off",
    "blinking",
  };
unsigned int ipmi_oem_hp_uid_light_max_index = 0x02;

const char * const ipmi_oem_hp_health_led[] =
  {
    "green",
    "amber",
    "red",
  };
unsigned int ipmi_oem_hp_health_led_max_index = 0x02;

/*
 * Intel
 */

/*
 * Intel S5500WB/Penguin Computing Relion 700
 */

const char * const ipmi_oem_intel_specific_pci_fatal_sensor[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down",
    "Unexpected Completion",
    "Received Unsupported request condition on inbound address decode with exception of SAD",
    "Poisoned TLP Error",
    "Flow Control Protocol Error",
    "Completion Timeout Error",
    "Completer Abort Error",
    "Receiver Buffer Overflow Error",
    "ACS Violation Error",
    "Malformed TLP Error",
    "Received ERR_FATAL Message From Downstream Error",
    "Unexpected Completion",    /* not a typo, identical to above */
    "Received ERR_NONFATAL Message Error",
    NULL
  };
unsigned int ipmi_oem_intel_specific_pci_fatal_sensor_max_index = 0x0D;

const char * const ipmi_oem_intel_specific_pci_correctable_sensor[] =
  {
    "Receiver Error",
    "Bad DLLP Error",
    "Bad TLLP Error",
    "REPLAY_NUM Rollover Error",
    "REPLAY Timer Timeout Error",
    "Advisory Non-fatal Error (Received ERR_COR message)",
    "Link Bandwidth Changed (ECN) Error",
    NULL
  };
unsigned int ipmi_oem_intel_specific_pci_correctable_sensor_max_index = 0x06;

/*
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard maintains Intel manufacturer ID)          
 */

/* achu: Similar to above, but some events text changed, so new arrays to differentiate */

const char * const ipmi_oem_intel_quanta_qssc_s4r_specific_pci_fatal_sensor[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down",
    "Unexpected Completer",
    "Received Unsupported request condition on inbound address decode with exception of SAD",
    "Poisoned TLP Error",
    "Flow Control Protocol Error",
    "Completion Timeout Error",
    "Completer Abort Error",
    "Receiver Buffer Overflow Error",
    "ACS Violation Error",
    "Malformed TLP Error",
    "Received ERR_FATAL Message From Downstream Error",
    "Unexpected Completion Error",
    "Received ERR_NONFATAL Message Error",
    NULL
  };
unsigned int ipmi_oem_intel_quanta_qssc_s4r_specific_pci_fatal_sensor_max_index = 0x0D;

const char * const ipmi_oem_intel_quanta_qssc_s4r_specific_pci_correctable_sensor[] =
  {
    "Receiver Error",
    "Bad DLLP Error",
    "Bad TLLP Error",
    "REPLAY_NUM Rollover Error",
    "REPLAY Timer Timeout Error",
    "Advisory Non-fatal Error (Received ERR_COR message)",
    "Link Bandwidth Changed (ECN) Error",
    NULL
  };
unsigned int ipmi_oem_intel_quanta_qssc_s4r_specific_pci_correctable_sensor_max_index = 0x06;

/*
 * Intel S2600JF/Appro 512X
 */

/* achu: Similar to above, but some events text changed and new ones,
 * so new arrays to differentiate */

const char * const ipmi_oem_intel_s2600jf_specific_pci_fatal_error[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down Error",
    "Completer Abort",
    "Unsupported Request",
    "Poisoned TLP",
    "Flow Control Protocol",
    "Completion Timeout",
    "Receiver Buffer Overflow",
    "ACS Violation Error",
    "Malformed TLP Error",
    "ECRC Error",
    "Received Fatal Message From Downstream",
    "Unexpected Completion",
    "Received ERR_NONFATAL Message",
    "Uncorrectable Internal",
    "MC Blocked TLP",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_pci_fatal_error_max_index = 0x0F;

const char * const ipmi_oem_intel_s2600jf_specific_pci_fatal_error_2[] =
  {
    "Atomic Egress Blocked",
    "TLP Prefix Blocked",
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
    "reserved",
    "reserved",
    "Unspecified Non-AER Fatal Error",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_pci_fatal_error_2_max_index = 0x0F;

const char * const ipmi_oem_intel_s2600jf_specific_pci_correctable_error[] =
  {
    "Receiver Error",
    "Bad DLLP",
    "Bad TLP",
    "Replay Num Rollover",
    "Replay Timer timeout",
    "Advisory Non-fatal",
    "Link BW Changed",
    "Correctable Internal",
    "Header Log Overflow",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_pci_correctable_error_max_index = 0x08;

const char * const ipmi_oem_intel_s2600jf_specific_opi_fatal_error[] =
  {
    "Link Layer Uncorrectable ECC Error",
    "Protocol Layer Poisoned Packet Reception Error",
    "LINK/PHY Init Failure with resultant degradation in link width",
    "CSI PHY Layer detected drift buffer alarm",
    "CSI PHY detected latency buffer rollover",
    "CSI PHY Init Failure",
    "CSI Link Layer generic control error (buffer overflow/underflow, credit underflow and so on.)",
    "Parity error in link or PHY layer",
    "Protocol layer timeout detected",
    "Protocol layer failed response",
    "Protocol layer illegal packet field, target Node ID and so on.",
    "Protocol Layer Queue/table overflow/underflow",
    "Viral Error",
    "Protocol Layer parity error",
    "Routing Table Error",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_opi_fatal_error_max_index = 0x0E;

#if 0
/* achu: Intel informed me there was an error in their documentation and the following was not correct.
 * I'll leave this here for legacy documentation
 */ 
const char * const ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2[] =
  {
    "Illegal inbound request",
    "PCH Write Cache Uncorrectable Data ECC Error",
    "PCH Write Cache Uncorrectable Data ECC Error", /* same not typo, typo in spec? */
    "PCH Write Cache Uncorrectable Data ECC Error", /* same not typo, typo in spec? */
    "PCH Received XPF physical/logical redirect interrupt inbound",
    "PCH Illegal SAD or Illegal or non-existent address or memory",
    "PCH Write Cache Coherency Violation",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2_max_index = 0x06;
#else  /* !0 */
const char * const ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2[] =
  {
    "Illegal inbound request",
    "IIO Write Cache Uncorrectable Data ECC Error",
    "IIO CSR crossing 32-bit boundary Error",
    "IIO Received XPF physical/logical redirect interrupt inbound",
    "IIO Illegal SAD or Illegal or non-existent address or memory",
    "IIO Write Cache Coherency Violation",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2_max_index = 0x05;
#endif	/* !0 */

const char * const ipmi_oem_intel_s2600jf_specific_qpi_link_width_reduced[] =
  {
    "reserved",
    "Reduced to 1/2 width",
    "Reduced to 1/4 width",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_qpi_link_width_reduced_max_index = 0x02;

/*
 * Intel S2600KP                                                                                                                                                                                   
 * Intel S2600WT2                                                                                                                                                                                  
 * Intel S2600WTT  
 */

const char * const ipmi_oem_intel_e52600v3_specific_qpi_fatal_error[] =
  {
    "Link Layer Uncorrectable ECC Error",
    "Protocol Layer Poisoned Packet Reception Error",
    "LINK/PHY Init Failure with resultant degradation in link width",
    "PHY Layer detected drift buffer alarm",
    "PHY detected latency buffer rollover",
    "PHY Init Failure",
    "Link Layer generic control error (buffer overflow/underflow, credit underflow, and so on.)",
    "Parity error in link or PHY layer",
    "Protocol layer timeout detected",
    "Protocol layer failed response",
    "Protocol layer illegal packet field, target Node ID, and so on.",
    "Protocol Layer Queue/table overflow/underflow",
    "Viral Error",
    "Protocol Layer parity error",
    "Routing Table Error",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_max_index = 0x0E;

const char * const ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_2[] =
  {
    "Illegal inbound request",
    "IIO Write Cache Uncorrectable Data ECC Error",
    "IIO CSR crossing 32-bit boundary Error",
    "IIO Received XPF physical/logical redirect interrupt inbound",
    "IIO Illegal SAD or Illegal or non-existent address or memory",
    "IIO Write Cache Coherency Violation",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_2_max_index = 0x05;

const char * const ipmi_oem_intel_e52600v3_specific_qpi_link_width_reduced[] =
  {
    "reserved",
    "Reduced to 1/2 width",
    "Reduced to 1/4 width",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_qpi_link_width_reduced_max_index = 0x02;

const char * const ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down Error",
    "Completer Abort",
    "Unsupported Request",
    "Poisoned TLP",
    "Flow Control Protocol",
    "Completion Timeout",
    "Receiver Buffer Overflow",
    "ACS Violation",
    "Malformed TLP",
    "ECRC Error",
    "Received Fatal Message From Downstream",
    "Unexpected Completion",
    "Received ERR_NONFATAL Message",
    "Uncorrectable Internal",
    "MC Blocked TLP",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_max_index = 0x0F;

const char * const ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_2[] =
  {
    "Atomic Egress Blocked",
    "TLP Prefix Blocked",
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
    "reserved",
    "reserved",
    "Unspecified Non-AER Fatal Error",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_2_max_index = 0x0F;

const char * const ipmi_oem_intel_e52600v3_specific_pci_express_correctable_errors[] =
  {
    "Receiver Error",
    "Bad DLLP",
    "Bad TLP",
    "Replay Num Rollover",
    "Replay Timer timeout",
    "Advisory Non-fatal",
    "Link BW Changed",
    "Correctable Internal",
    "Header Log Overflow",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_pci_express_correctable_errors_max_index = 0x08;


const char * const ipmi_oem_intel_e52600v3_specific_firmware_update_status_sensor[] =
  {
    "Update started",
    "Update completed successfully",
    "Update failure",
    NULL
  };

unsigned int ipmi_oem_intel_e52600v3_specific_firmware_update_status_sensor_max_index = 0x02;

const char * const ipmi_oem_intel_e52600v3_specific_bios_recovery_start[] =
  {
    "reserved",
    "BIOS Recovery Start",
    NULL
  };

unsigned int ipmi_oem_intel_e52600v3_specific_bios_recovery_start_max_index = 0x01;

const char * const ipmi_oem_intel_e52600v3_specific_bios_recovery_finish[] =
  {
    "reserved",
    "BIOS Recovery Finish",
    NULL
  };

unsigned int ipmi_oem_intel_e52600v3_specific_bios_recovery_finish_max_index = 0x01;

const char * const ipmi_oem_intel_e52600v3_specific_ierr_recovery_dump_info[] =
  {
    "reserved",
    "Dump failed",
    NULL,
  };
unsigned int ipmi_oem_intel_e52600v3_specific_ierr_recovery_dump_info_max_index = 0x01;

/******************************************* 
 * Wistron                                 *
 *******************************************/

/*
 * Wistron / Dell Poweredge C6220
 */

const char * const ipmi_sensor_type_oem_wistron_ioh_core_error[] =
  {
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "core",
    "non-fatal",
    "reserved",
    "fatal",
    NULL
  };
unsigned int ipmi_sensor_type_oem_wistron_ioh_core_error_max_index = 0x0A;

