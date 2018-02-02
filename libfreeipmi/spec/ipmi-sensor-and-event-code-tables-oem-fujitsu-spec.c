/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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

#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-fujitsu-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/*
 * Fuijitsu
 */

/*
 * iRMC S1 / iRMC S2
 */

/* 0xC0 / IPMI_SENSOR_TYPE_OEM_FUJITSU_I2C_BUS */
const char * const ipmi_sensor_type_oem_fujitsu_i2c_bus[] =
  {
    /* EN 0x00 */       "I2C Bus Error",
    /* EN 0x01 */       "I2C Bus OK",
    /* EN 0x02 */       "I2C Bus Disabled",
    /* EN 0x03 */       "I2C Bus Failed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_i2c_bus_max_index = 0x03;

/* 0xDD / IPMI_SENSOR_TYPE_OEM_FUJITSU_SYSTEM_POWER_CONSUMPTION */
const char * const ipmi_sensor_type_oem_fujitsu_system_power_consumption[] =
  {
    /* EN 0x00 */       "System Power Consumption within Limit",
    /* EN 0x01 */       "System Power Consumption above Warning Level",
    /* EN 0x02 */       "System Power Consumption above Critical Level",
    /* EN 0x03 */       "System Power Consumption limiting disabled",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_system_power_consumption_max_index = 0x03;


/* 0xDE / IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_STATUS */
const char * const ipmi_sensor_type_oem_fujitsu_memory_status[] =
  {
    /* EN 0x00 */       "Empty slot",
    /* EN 0x01 */       "OK",
    /* EN 0x02 */       "Reserved",
    /* EN 0x03 */       "Error",
    /* EN 0x04 */       "Fail",
    /* EN 0x05 */       "Prefailure",
    /* EN 0x06 */       "Reserved",
    /* EN 0x07 */       "Unknown",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_memory_status_max_index = 0x07;

/* 0xDF / IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_CONFIG */
const char * const ipmi_sensor_type_oem_fujitsu_memory_config[] =
  {
    /* EN 0x00 */       "Normal",
    /* EN 0x01 */       "Disabled",
    /* EN 0x02 */       "Spare module",
    /* EN 0x03 */       "Mirrored module",
    /* EN 0x04 */       "RAID module",
    /* EN 0x05 */       "Not Usable",
    /* EN 0x06 */       "Unspecified state(6)",
    /* EN 0x07 */       "Unspecified state(7)",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_memory_config_max_index = 0x07;

/* 0xE1 / IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY */
const char * const ipmi_sensor_type_oem_fujitsu_memory[] =
  {
    /* EN 0x00 */       "Non Fujitsu memory module detected",
    /* EN 0x01 */       "Memory module replaced",
    /* EN 0x02 */       "Fatal general memory error",
    /* EN 0x03 */       "Recoverable general memory error",
    /* EN 0x04 */       "Recoverable ECC memory error",
    /* EN 0x05 */       "Recoverable CRC memory error",
    /* EN 0x06 */       "Fatal CRC memory error",
    /* EN 0x07 */       "Recoverable thermal memory event",
    /* EN 0x08 */       "Fatal thermal memory error",
    /* EN 0x09 */       "Too many correctable memory errors",
    /* EN 0x0A */       "Uncorrectable Parity memory error",
    /* EN 0x0B */       "Memory Modules swapped",
    /* EN 0x0C */       "Memory Module moved",
    /* EN 0x0D */       "Memory removed",
    /* EN 0x0E */       "Memory Re-inserted",
    /* EN 0x0F */       "Memory module(s) changed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_memory_max_index = 0x0F;

/* 0xE3 / IPMI_SENSOR_TYPE_OEM_FUJITSU_HW_ERROR */
const char * const ipmi_sensor_type_oem_fujitsu_hw_error[] =
  {
    /* EN 0x00 */       "TPM Error",
    /* EN 0x01 */       "Reserved",
    /* EN 0x02 */       "No usable CPU",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_hw_error_max_index = 0x02;

/* 0xE4 / IPMI_SENSOR_TYPE_OEM_FUJITSU_SYS_ERROR */
const char * const ipmi_sensor_type_oem_fujitsu_sys_error[] =
  {
    /* EN 0x00 */       "System configuration Data error",
    /* EN 0x01 */       "Resource Conflict",                  /* Slot in EventData3 */
    /* EN 0x02 */       "IRQ not configured",                 /* Slot in EventData3 */
    /* EN 0x03 */       "Device node allocation error",       /* Device in EventData3 */
    /* EN 0x04 */       "Expansion ROM Slot not initialized", /* Slot in EventData3 */
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_sys_error_max_index = 0x04;

/* 0xE6 / IPMI_SENSOR_TYPE_OEM_FUJITSU_FAN_STATUS */
const char * const ipmi_sensor_type_oem_fujitsu_fan_status[] =
  {
    /* EN 0x00 */       "FAN on, running",
    /* EN 0x01 */       "FAN failed",
    /* EN 0x02 */       "FAN prefailure",
    /* EN 0x03 */       "Redundant FAN failed",
    /* EN 0x04 */       "FAN not manageable",
    /* EN 0x05 */       "FAN not installed",
    /* EN 0x06 */       "FAN unspecified state(6)",
    /* EN 0x07 */       "FAN in init phase",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_fan_status_max_index = 0x07;

/* 0xE8 / IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_STATUS */
const char * const ipmi_sensor_type_oem_fujitsu_psu_status[] =
  {
    /* EN 0x00 */       "Power supply - Not present",
    /* EN 0x01 */       "Power supply - OK",
    /* EN 0x02 */       "Power supply - Failed",
    /* EN 0x03 */       "Redundant power supply - AC failed",
    /* EN 0x04 */       "Redundant power supply - DC failed",
    /* EN 0x05 */       "Power supply - Critical Temperature",
    /* EN 0x06 */       "Power supply - Not manageable",
    /* EN 0x07 */       "Power supply - Fan failure predicted",
    /* EN 0x08 */       "Power supply - Fan failed",
    /* EN 0x09 */       "Power supply - Power Save Mode",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_psu_status_max_index = 0x09;

/* 0xE9 / IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_REDUNDANCY */
const char * const ipmi_sensor_type_oem_fujitsu_psu_redundancy[] =
  {
    /* EN 0x00 */       "Power Supply - redundancy present",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_psu_redundancy_max_index = 0x00;

/* 0xEC / IPMI_SENSOR_TYPE_OEM_FUJITSU_FLASH */
const char * const ipmi_sensor_type_oem_fujitsu_flash[] =
  {
    /* EN 0x00 */       "Online firmware flash",
    /* EN 0x01 */       "Online firmware flash: reboot",
    /* EN 0x02 */       "BIOS TFTP Flash: OK",
    /* EN 0x03 */       "BIOS TFTP Flash: failed",
    /* EN 0x04 */       "iRMC TFTP Flash: OK",
    /* EN 0x05 */       "iRMC TFTP Flash: failed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_flash_max_index = 0x05;

/* 0xEF / IPMI_SENSOR_TYPE_OEM_FUJITSU_CONFIG_BACKUP */
const char * const ipmi_sensor_type_oem_fujitsu_config_backup[] =
  {
    /* EN 0x00 */       "Chassis IDPROM: Motherboard Exchange detected",
    /* EN 0x01 */       "Chassis IDPROM: Read or Write error",
    /* EN 0x02 */       "Chassis IDPROM: Restore successful",
    /* EN 0x03 */       "Chassis IDPROM: Restore failed",
    /* EN 0x04 */       "Chassis IDPROM: Backup successful",
    /* EN 0x05 */       "Chassis IDPROM: Backup failed",
    /* EN 0x06 */       "Chassis IDPROM: Feature disabled",
    /* EN 0x07 */       "Chassis IDPROM: Function Not Available",
    /* EN 0x08 */       "Reserved",
    /* EN 0x09 */       "Reserved",
    /* EN 0x0A */       "Reserved",
    /* EN 0x0B */       "Reserved",
    /* EN 0x0C */       "Reserved",
    /* EN 0x0D */       "Reserved",
    /* EN 0x0E */       "Reserved",
    /* EN 0x0F */       "NVRAM defaults loaded",
    NULL
  };
unsigned int ipmi_sensor_type_oem_fujitsu_config_backup_max_index = 0x0F;
