/* 
   acpi-spmi-locate.h - ACPI tables driver to locate IPMI interfaces
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

#ifndef _ACPI_SPMI_LOCATE_H
#define _ACPI_SPMI_LOCATE_H

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

/*******************************************************************************
 *
 * FUNCTION:
 *   ipmi_acpi_table_checksum
 *
 * PARAMETERS:  
 *   buffer  - Buffer to checksum
 *   length  - Size of the buffer
 *
 * RETURNS:
 *   8 bit checksum of buffer. NON-ZERO = checksum failed.
 *
 * DESCRIPTION:
 *   Computes an 8 bit checksum of the buffer(length) and returns it.
 *
 ******************************************************************************/
uint8_t ipmi_acpi_table_chksum (uint8_t *buffer, size_t len);


/*******************************************************************************
 *
 * FUNCTION:
 *   ipmi_acpi_get_rsdp
 *
 * PARAMETERS:  
 *   rsdp_window_base_addr  - Starting pointer for search
 *   rsdp_window_wize       - Maximum length to search
 *   obj_acpi_rsdp_descriptor  - Initialized rsdp descriptor object
 *
 * RETURN:
 *   A return value of 0 means success. RSDP descriptor is returned
 *   through obj_acpi_rsdp_descriptor parameter.
 *
 * DESCRIPTION: 
 *   Search a block of memory for the RSDP signature.
 *
 * NOTE: 
 *   The RSDP must be either in the first 1_k of the Extended BIOS
 *   Data Area or between E0000 and FFFFF (ACPI 1.0 section 5.2.2;
 *   assertion #421).
 *
 ******************************************************************************/
int ipmi_acpi_get_rsdp (uint64_t rsdp_window_base_addr, size_t rsdp_window_size, 
			fiid_obj_t obj_acpi_rsdp_descriptor);


/*******************************************************************************
 *
 * FUNCTION:
 *   ipmi_acpi_get_table
 *
 * PARAMETERS:  
 *   table_address     - ACPI table physical address
 *   signature         - signature of the table
 *   acpi_table        - ACPI table in malloc'ed memory
 *   acpi_table_length - ACPI table length
 *
 * RETURN:
 *   A return value of 0 means success. ACPI table (including header) is
 *   returned through acpi_table parameter.
 *
 * DESCRIPTION:
 *   Retrieve any ACPI table (including header) pointed by table address.
 *
 ******************************************************************************/
int ipmi_acpi_get_table (uint64_t table_address, char *signature, 
			 uint8_t **acpi_table, uint32_t *acpi_table_length);


/*******************************************************************************
 *
 * FUNCTION:
 *   ipmi_acpi_get_firmware_table
 *
 * PARAMETERS:  
 *   signature               - ACPI signature for firmware table header
 *   table_instance          - Which instance of the firmware table
 *   obj_acpi_table_hdr      - Initialized ACPI table header
 *   sign_table_data         - Initialized with malloc'ed ACPI firmware table data 
 *   sign_table_data_length  - ACPI table DATA length
 *
 * RETURN:      
 *   Return 0 for success. ACPI table header and firmware table DATA are
 *   returned through obj_acpi_table_hdr and signed_table_data
 *   parameters.
 *
 * DESCRIPTION:
 *   Top level call for any ACPI firmware table by table signature string.
 *   It gets table header and data from RSDT/XSDT.
 *
 ******************************************************************************/
int ipmi_acpi_get_firmware_table (char *signature, int table_instance, 
				  fiid_obj_t obj_acpi_table_hdr,
				  uint8_t **sign_table_data, 
				  uint32_t *sign_table_data_length);


/*******************************************************************************
 *
 * FUNCTION:
 *   ipmi_acpi_get_spmi_table
 *
 * PARAMETERS:  
 *   interface_type           - Type of interface to look for (KCS, SSIF, SMIC, BT)
 *   obj_acpi_table_hdr       - Initialized ACPI table header
 *   acpi_table_firmware      - Initialized ACPI firmware table
 *
 * RETURN:      
 *   Return 0 for success. ACPI table header and SPMI table is
 *   returned through obj_acpi_table_hdr and obj_acpi_spmi_table_descriptor
 *   parameters.
 *
 * DESCRIPTION: 
 *   Get SPMI table for the given interface type.
 *
 ******************************************************************************/
int ipmi_acpi_get_spmi_table (uint8_t interface_type,
			      fiid_obj_t obj_acpi_table_hdr,
			      fiid_obj_t obj_acpi_spmi_table_descriptor);


/*******************************************************************************
 *
 * FUNCTION:
 * acpi_spmi_get_dev_info
 *
 * PARAMETERS:  
 *   type    = which interface (KCS, SMIC, BT)
 *   pinfo   = pointer to information structure filled in by this function
 *
 * RETURNS:
 *   pinfo if successful, NULL otherwise 
 ******************************************************************************/
ipmi_locate_info_t *acpi_spmi_get_dev_info (ipmi_interface_type_t interface_type, 
					    ipmi_locate_info_t *pinfo);

#endif
