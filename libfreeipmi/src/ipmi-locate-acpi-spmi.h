/* 
   ipmi-locate-acpi-spmi.h - ACPI tables driver to locate IPMI interfaces
   using SPMI table.

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

#ifndef _IPMI_LOCATE_ACPI_SPMI_H
#define _IPMI_LOCATE_ACPI_SPMI_H

#define IPMI_ACPI_ADDRESS_SPACE_ID_SYSTEM_MEMORY IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY
#define IPMI_ACPI_ADDRESS_SPACE_ID_SYSTEM_IO     IPMI_ADDRESS_SPACE_ID_SYSTEM_IO
#define IPMI_ACPI_ADDRESS_SPACE_ID_SMBUS         IPMI_ADDRESS_SPACE_ID_SMBUS

/* Certain ACPI table field widths are architecture specific */
#define IPMI_ACPI_MACHINE_WIDTH         (sizeof (void *) * 8)

/* Constants used in searching for the RSDP (Root System Description
   Pointer) in low memory */
#define IPMI_ACPI_LO_RSDP_WINDOW_BASE        0           /* Physical Address */
#define IPMI_ACPI_HI_RSDP_WINDOW_BASE        0xE0000     /* Physical Address */
#define IPMI_ACPI_LO_RSDP_WINDOW_SIZE        0x400
#define IPMI_ACPI_HI_RSDP_WINDOW_SIZE        0x20000
#define IPMI_ACPI_RSDP_SCAN_STEP             16

/*
 *  Values for description table header signatures
 */
#define IPMI_ACPI_RSDP_NAME   "RSDP"
#define IPMI_ACPI_RSDP_SIG    "RSD PTR "  /* RSDT Pointer signature */
#define IPMI_ACPI_APIC_SIG    "APIC"      /* Multiple APIC Description Table */
#define IPMI_ACPI_DSDT_SIG    "DSDT"      /* Differentiated System Description Table */
#define IPMI_ACPI_FADT_SIG    "FACP"      /* Fixed ACPI Description Table */
#define IPMI_ACPI_FACS_SIG    "FACS"      /* Firmware ACPI Control Structure */
#define IPMI_ACPI_PSDT_SIG    "PSDT"      /* Persistent System Description Table */
#define IPMI_ACPI_RSDT_SIG    "RSDT"      /* Root System Description Table */
#define IPMI_ACPI_XSDT_SIG    "XSDT"      /* Extended  System Description Table */
#define IPMI_ACPI_SSDT_SIG    "SSDT"      /* Secondary System Description Table */
#define IPMI_ACPI_SBST_SIG    "SBST"      /* Smart Battery Specification Table */
#define IPMI_ACPI_SPIC_SIG    "SPIC"      /* IOSAPIC table */
#define IPMI_ACPI_BOOT_SIG    "BOOT"      /* Boot table */
#define IPMI_ACPI_SPMI_SIG    "SPMI"      /* Service Processor Management Interface */

/* RSDP checksums */
#define IPMI_ACPI_RSDP_CHECKSUM_LENGTH       20
#define IPMI_ACPI_RSDP_XCHECKSUM_LENGTH      36

#endif
