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
#include <sys/stat.h>
#include <sys/mman.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"

#include "ipmi-locate-defs.h"
#include "ipmi-locate-trace.h"
#include "ipmi-locate-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

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

fiid_template_t tmpl_acpi_rsdp_descriptor =  /* Root System Descriptor Pointer */
  {
    /* ACPI signature, contains "RSD PTR " */
    { 64, "signature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* To make sum of struct == 0 */
    { 8, "checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM identification */
    { 48, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Must be 0 for 1.0, 2 for 2.0 */
    { 8, "revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 32-bit physical address of RSDT */
    { 32, "rsdt_physical_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* XSDT Length in bytes including hdr */
    { 32, "length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 64-bit physical address of XSDT */
    { 64, "xsdt_physical_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Checksum of entire table */
    { 8, "extended_checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Reserved field must be 0 */
    { 24, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_acpi_table_hdr =
  {
    /* ACPI signature (4 ASCII characters) */
    { 32, "signature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Length of table, in bytes, including header */
    { 32, "length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* ACPI Specification minor version # */
    { 8, "revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* To make sum of entire table == 0 */
    { 8, "checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM identification */
    { 48, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM table identification */
    { 64, "oem_table_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM revision number */
    { 32, "oem_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* ASL compiler vendor ID */
    { 32, "asl_compiler_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* ASL compiler revision number */
    { 32, "asl_compiler_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_acpi_spmi_table_descriptor =
  {
    /* `SPMI'. Signature for the Service Processor Management
       Interface Table. */
    { 32, "signature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Length, in bytes, of the entire Service Processor Management
       Interface Table. */
    { 32, "length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 5 */
    { 8, "revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Entire table must sum to zero. */
    { 8, "checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM ID. Per ACPI specification. An OEM-supplied string that
       identifies the OEM. */
    { 48, "oemid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For the Service Processor Management Interface Table,
       the table ID is the manufacturer model ID
       (assigned by the OEM identified by "OEM ID"). */
    { 64, "oem_table_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM revision of Service Processor Management Interface
       Table for supplied the given OEM Table ID. Per ACPI, this is
       "An OEM-supplied revision number. Larger numbers are
       assumed to be newer revisions." */
    { 32, "oem_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Vendor ID of utility that created the table. For the tables
       containing Definition Blocks, this is the ID for the ASL
       Compiler. */
    { 32, "creator_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Revision of utility that created the table. For the tables
       containing Definition Blocks, this is the revision
       for the ASL Compiler. */
    { 32, "creator_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Indicates the type of IPMI interface:
       0 Reserved
       1 Keyboard Controller Style (KCS)
       2 Server Management Interface Chip (SMIC)
       3 Block Transfer (BT)
       4 SMBus System Interface (SSIF)
       5-255 Reserved */
    { 8, "interface_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* This field must always be 01h to be compatible with any
       software that implements previous
       versions of this spec. */
    { 8, "ipmi_legacy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Identifies the IPMI specification revision,
       in BCD format, to which the interface
       was designed. The first byte holds the
       most significant digits, while second byte holds
       the least significant digits of the revision,
       e.g. a value of 0x0150 indicates the interface is
       compatible with IPMI version v1.5. */
    /*     {16, "specification_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  */
    { 8, "specification_revision.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_revision.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Interrupt type(s) used by
       the interface:
       [0] - SCI triggered through GPE
       (use 0b for SSIF)
       0 = not supported
       1 = supported */
    { 1, "interrupt_type.sci_triggered_thru_gpe", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [1] - I/O APIC/SAPIC interrupt
       (Global System Interrupt) */
    { 1, "interrupt_type.io_apic_sapic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7:2] - Reserved (must be 0) */
    { 6, "interrupt_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The bit assignment of the SCI interrupt within the GPEx_STS
       register of a GPE described if the FADT that the interface
       triggers. (Note: This field is valid only if Bit[0] of the Interrupt
       Type field is set. Otherwise set to 00h.) */
    { 8, "gpe", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 00h */
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [0] - PCI Device Flag. For PCI IPMI devices,
       this bit is set. For non-PCI devices, this bit is cleared.
       When this bit is cleared, the PCI Segment Group, Bus,
       Device and Function Number fields combined corresponds
       to the ACPI _UID value of the device whose _HID or _CID
       contains IPI0001 plug and play ID.
       _UID must be an integer. Byte 60 contains the least
       significant byte of the _UID value. Set to 0b for SSIF. */
    { 1, "pci_device_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7:1] - Reserved */
    { 7, "pci_device_flag.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The I/O APIC or I/O SAPIC Global System
       Interrupt used by the interface.
       (Note: This field is valid only if Bit[1] of the
       Interrupt Type field is set.
       Otherwise set to 00h.) */
    { 32, "global_system_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The base address of the interface register
       set described using the Generic Address Structure
       (GAS, See [ACPI 2.0] for the
       definition). The Address_Space_ID field in
       the GAS can only be of the value of 0 (System Memory),
       1 (System IO), and 4 (SMBus).
       All other values are not permitted.
       For SSIF:
       The Address_Space_ID = 4 and the address field of the GAS
       holds the 7-bit slave address of the BMC on the host SMBus
       in the least significant byte. Note that the slave address is
       stored with the 7-bit slave address in the least
       significant 7-
       bits of the byte, and the most significant
       bit of the byte set to
       0b.
       Register_Bit_Width = 0
       Register_Bit_Offset = 0
       Address_Size field = 1 (Byte access)
       Address = 7-bit SMBus address of BMC
       SSIF */
    /*     {96, "base_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  */
    /* Address space where
       struct or register
       exists. */
    { 8, "base_address.address_space_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Size in bits of given
       register */
    { 8, "base_address.register_bit_width", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bit offset within the
       register */
    { 8, "base_address.register_bit_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Must be 0 */
    { 8, "base_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 64-bit address of
       struct or register */
    { 64, "base_address.address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* for IPMI cards not located on PCI */
    { 32, "uid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* This field must always be null (0x00) to be compatible with
       any software that implements previous versions of this spec.
       This field is a deprecated "SPMI ID Field". Implementations
       based on pre-IPMI v2.0 versions of SPMI may contain a null-
       terminated string here. */
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_acpi_spmi_table_descriptor_ssif =
  {
    /* `SPMI'. Signature for the Service Processor Management
       Interface Table. */
    { 32, "signature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Length, in bytes, of the entire Service Processor Management
       Interface Table. */
    { 32, "length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 5 */
    { 8, "revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Entire table must sum to zero. */
    { 8, "checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM ID. Per ACPI specification. An OEM-supplied string that
       identifies the OEM. */
    { 48, "oemid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For the Service Processor Management Interface Table,
       the table ID is the manufacturer model ID
       (assigned by the OEM identified by "OEM ID"). */
    { 64, "oem_table_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM revision of Service Processor Management Interface
       Table for supplied the given OEM Table ID. Per ACPI, this is
       "An OEM-supplied revision number. Larger numbers are
       assumed to be newer revisions." */
    { 32, "oem_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Vendor ID of utility that created the table. For the tables
       containing Definition Blocks, this is the ID for the ASL
       Compiler. */
    { 32, "creator_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Revision of utility that created the table. For the tables
       containing Definition Blocks, this is the revision
       for the ASL Compiler. */
    { 32, "creator_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Indicates the type of IPMI interface:
       0 Reserved
       1 Keyboard Controller Style (KCS)
       2 Server Management Interface Chip (SMIC)
       3 Block Transfer (BT)
       4 SMBus System Interface (SSIF)
       5-255 Reserved */
    { 8, "interface_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* This field must always be 01h to be compatible with any
       software that implements previous
       versions of this spec. */
    { 8, "ipmi_legacy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Identifies the IPMI specification revision,
       in BCD format, to which the interface
       was designed. The first byte holds the
       most significant digits, while second byte holds
       the least significant digits of the revision,
       e.g. a value of 0x0150 indicates the interface is
       compatible with IPMI version v1.5. */
    /* {16, "specification_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  */
    { 8, "specification_revision.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_revision.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Interrupt type(s) used by
       the interface:
       [0] - SCI triggered through GPE
       (use 0b for SSIF)
       0 = not supported
       1 = supported */
    { 1, "interrupt_type.sci_triggered_thru_gpe", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [1] - I/O APIC/SAPIC interrupt
       (Global System Interrupt) */
    { 1, "interrupt_type.io_apic_sapic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7:2] - Reserved (must be 0) */
    { 6, "interrupt_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The bit assignment of the SCI interrupt within the GPEx_STS
       register of a GPE described if the FADT that the interface
       triggers. (Note: This field is valid only if Bit[0] of the Interrupt
       Type field is set. Otherwise set to 00h.) */
    { 8, "gpe", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 00h */
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [0] - PCI Device Flag. For PCI IPMI devices,
       this bit is set. For non-PCI devices, this bit is cleared.
       When this bit is cleared, the PCI Segment Group, Bus,
       Device and Function Number fields combined corresponds
       to the ACPI _UID value of the device whose _HID or _CID
       contains IPI0001 plug and play ID.
       _UID must be an integer. Byte 60 contains the least
       significant byte of the _UID value. Set to 0b for SSIF. */
    { 1, "pci_device_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7:1] - Reserved */
    { 7, "pci_device_flag.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The I/O APIC or I/O SAPIC Global System
       Interrupt used by the interface.
       (Note: This field is valid only if Bit[1] of the
       Interrupt Type field is set.
       Otherwise set to 00h.) */
    { 32, "global_system_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The base address of the interface register
       set described using the Generic Address Structure
       (GAS, See [ACPI 2.0] for the
       definition). The Address_Space_ID field in
       the GAS can only be of the value of 0 (System Memory),
       1 (System IO), and 4 (SMBus).
       All other values are not permitted.
       For SSIF:
       The Address_Space_ID = 4 and the address field of the GAS
       holds the 7-bit slave address of the BMC on the host SMBus
       in the least significant byte. Note that the slave address is
       stored with the 7-bit slave address in the least
       significant 7-
       bits of the byte, and the most significant
       bit of the byte set to
       0b.
       Register_Bit_Width = 0
       Register_Bit_Offset = 0
       Address_Size field = 1 (Byte access)
       Address = 7-bit SMBus address of BMC
       SSIF */
    /*     {96, "base_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  */
    /* Address space where
       struct or register
       exists. */
    { 8, "base_address.address_space_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Size in bits of given
       register */
    { 8, "base_address.register_bit_width", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bit offset within the
       register */
    { 8, "base_address.register_bit_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* field = 1 (byte
       access) */
    { 8, "base_address.address_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* SMBus address of BMC
       SSIF */
    { 7, "base_address.address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Reserved */
    { 57, "base_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* for IPMI cards not located on PCI */
    { 32, "uid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* This field must always be null (0x00) to be compatible with
       any software that implements previous versions of this spec.
       This field is a deprecated "SPMI ID Field". Implementations
       based on pre-IPMI v2.0 versions of SPMI may contain a null-
       terminated string here. */
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_acpi_spmi_table_descriptor_pci_ipmi =
  {
    /* `SPMI'. Signature for the Service Processor Management
       Interface Table. */
    { 32, "signature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Length, in bytes, of the entire Service Processor Management
       Interface Table. */
    { 32, "length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 5 */
    { 8, "revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Entire table must sum to zero. */
    { 8, "checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM ID. Per ACPI specification. An OEM-supplied string that
       identifies the OEM. */
    { 48, "oemid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For the Service Processor Management Interface Table,
       the table ID is the manufacturer model ID
       (assigned by the OEM identified by "OEM ID"). */
    { 64, "oem_table_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* OEM revision of Service Processor Management Interface
       Table for supplied the given OEM Table ID. Per ACPI, this is
       "An OEM-supplied revision number. Larger numbers are
       assumed to be newer revisions." */
    { 32, "oem_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Vendor ID of utility that created the table. For the tables
       containing Definition Blocks, this is the ID for the ASL
       Compiler. */
    { 32, "creator_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Revision of utility that created the table. For the tables
       containing Definition Blocks, this is the revision
       for the ASL Compiler. */
    { 32, "creator_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Indicates the type of IPMI interface:
       0 Reserved
       1 Keyboard Controller Style (KCS)
       2 Server Management Interface Chip (SMIC)
       3 Block Transfer (BT)
       4 SMBus System Interface (SSIF)
       5-255 Reserved */
    { 8, "interface_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* This field must always be 01h to be compatible with any
       software that implements previous
       versions of this spec. */
    { 8, "ipmi_legacy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Identifies the IPMI specification revision,
       in BCD format, to which the interface
       was designed. The first byte holds the
       most significant digits, while second byte holds
       the least significant digits of the revision,
       e.g. a value of 0x0150 indicates the interface is
       compatible with IPMI version v1.5. */
    /* {16, "specification_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, */
    { 8, "specification_revision.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_revision.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Interrupt type(s) used by
       the interface:
       [0] - SCI triggered through GPE
       (use 0b for SSIF)
       0 = not supported
       1 = supported */
    { 1, "interrupt_type.sci_triggered_thru_gpe", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [1] - I/O APIC/SAPIC interrupt
       (Global System Interrupt) */
    { 1, "interrupt_type.io_apic_sapic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7:2] - Reserved (must be 0) */
    { 6, "interrupt_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The bit assignment of the SCI interrupt within the GPEx_STS
       register of a GPE described if the FADT that the interface
       triggers. (Note: This field is valid only if Bit[0] of the Interrupt
       Type field is set. Otherwise set to 00h.) */
    { 8, "gpe", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 00h */
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [0] - PCI Device Flag. For PCI IPMI devices,
       this bit is set. For non-PCI devices, this bit is cleared.
       When this bit is cleared, the PCI Segment Group, Bus,
       Device and Function Number fields combined corresponds
       to the ACPI _UID value of the device whose _HID or _CID
       contains IPI0001 plug and play ID.
       _UID must be an integer. Byte 60 contains the least
       significant byte of the _UID value. Set to 0b for SSIF. */
    { 1, "pci_device_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7:1] - Reserved */
    { 7, "pci_device_flag.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The I/O APIC or I/O SAPIC Global System
       Interrupt used by the interface.
       (Note: This field is valid only if Bit[1] of the
       Interrupt Type field is set.
       Otherwise set to 00h.) */
    { 32, "global_system_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The base address of the interface register
       set described using the Generic Address Structure
       (GAS, See [ACPI 2.0] for the
       definition). The Address_Space_ID field in
       the GAS can only be of the value of 0 (System Memory),
       1 (System IO), and 4 (SMBus).
       All other values are not permitted.
       For SSIF:
       The Address_Space_ID = 4 and the address field of the GAS
       holds the 7-bit slave address of the BMC on the host SMBus
       in the least significant byte. Note that the slave address is
       stored with the 7-bit slave address in the least
       significant 7-
       bits of the byte, and the most significant
       bit of the byte set to
       0b.
       Register_Bit_Width = 0
       Register_Bit_Offset = 0
       Address_Size field = 1 (Byte access)
       Address = 7-bit SMBus address of BMC SSIF */
    /* {96, "base_address"} */
    /* Address space where
       struct or register
       exists. */
    { 8, "base_address.address_space_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Size in bits of given
       register */
    { 8, "base_address.register_bit_width", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bit offset within the
       register */
    { 8, "base_address.register_bit_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Must be 0 */
    { 8, "base_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 64-bit address of
       struct or register */
    { 64, "base_address.address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* PCI Segment Group Number,
       if the IPMI device is a PCI
       device. Otherwise, this field is byte 1 of a UID.
       See description for PCI Device Flag, above. */
    { 8, "pci_segment_group_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* PCI Bus Number, if the IPMI device is a PCI device.
       Otherwise, this field is byte 2 of a UID.
       See description for PCI Device Flag, above. */
    { 8, "pci_bus_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* PCI Device fields or byte 3 of a UID. Per PCI Device Flag,
       above.
       For PCI Device Flag = 1b:
       [4:0] - PCI Device Number: The PCI device number if the
       IPMI device is a PCI device.
       For PCI Device Flag = 0b:
       [7:0] - byte 3 of UID */
    { 4, "pci_device_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7:5] - Reserved */
    { 4, "pci_device_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* PCI Device fields or byte 4 of a UID.
       Per PCI Device Flag, above.
       For PCI Device Flag = 1b:
       [2:0] - PCI Function Number: The PCI function number if
       the IPMI device is a PCI device. */
    { 3, "pci_function_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [5:3] - Reserved */
    { 3, "pci_function_number.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [6] -    Interrupt Flag:
       0b = interrupt not supported
       1b = interrupt supported */
    { 1, "pci_function_number.interrupt_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* [7] -    Reserved
       For PCI Device Flag = 0b:
       [7:0] - byte 4 of UID */
    { 1, "pci_function_number.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* This field must always be null (0x00) to be compatible with
       any software that implements previous versions of this spec.
       This field is a deprecated "SPMI ID Field". Implementations
       based on pre-IPMI v2.0 versions of SPMI may contain a null-
       terminated string here. */
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

static uint8_t _ipmi_acpi_table_checksum (ipmi_locate_ctx_t ctx,
                                          uint8_t *buffer,
                                          size_t len);
static int _ipmi_acpi_get_rsdp (ipmi_locate_ctx_t ctx,
                                uint64_t rsdp_window_base_address,
                                size_t rsdp_window_size,
                                fiid_obj_t obj_acpi_rsdp_descriptor);
static int _ipmi_acpi_get_table (ipmi_locate_ctx_t ctx,
                                 uint64_t table_address,
                                 char *signature,
                                 uint8_t **acpi_table,
                                 uint32_t *acpi_table_length);
static int _ipmi_acpi_get_firmware_table (ipmi_locate_ctx_t ctx,
                                          char *signature,
                                          unsigned int table_instance,
                                          fiid_obj_t obj_acpi_table_hdr,
                                          uint8_t **sign_table_data,
                                          uint32_t *sign_table_data_length);
static int _ipmi_acpi_get_spmi_table (ipmi_locate_ctx_t ctx,
                                      uint8_t interface_type,
                                      fiid_obj_t obj_acpi_table_hdr,
                                      fiid_obj_t obj_acpi_spmi_table_descriptor);

#define IPMI_INTERFACE_COUNT 5

static uint64_t physical_memory_size = 0;

static int
_ipmi_physical_address_valid (ipmi_locate_ctx_t ctx,
                              uint64_t physical_address,
                              size_t length)
{
  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);

  /* achu: Some buggy kernels will crash the system if the physical
   * address is bad.  Yes, I know it's the kernel's fault, but we have
   * to do our best to get around it.  We do so by making sure the
   * physical address is legit.
   */
#if defined(_SC_PAGESIZE) && defined(_SC_PHYS_PAGES)
  if (!physical_memory_size)
    {
      long pagesize, physical_pages;

      if ((pagesize = sysconf (_SC_PAGESIZE)) < 0)
        {
          LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
          return (-1);
        }
      if ((physical_pages = sysconf (_SC_PHYS_PAGES)) < 0)
        {
          LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
          return (-1);
        }

      physical_memory_size = pagesize * physical_pages;
    }

  if (physical_address < physical_memory_size
      && (physical_address + length) > physical_address
      && (physical_address + length) < physical_memory_size)
    return (1);
  else
    return (0);
#else /* !(_SC_PAGESIZE && _SC_PHYS_PAGES) */
  /* achu: For now we return 1.  Later we can maybe read /dev/meminfo
   * or something.
   */
  return (1);
#endif /* !(_SC_PAGESIZE && _SC_PHYS_PAGES) */
}

/*******************************************************************************
 *
 * FUNCTION:
 *   _ipmi_acpi_table_checksum
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
static uint8_t
_ipmi_acpi_table_checksum (ipmi_locate_ctx_t ctx,
                           uint8_t *buffer,
                           size_t len)
{
  size_t i = 0;
  uint8_t sum = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (buffer);

  for (i = 0; i < len; i++)
    sum += buffer[i];

  return (sum);
}

static int
_ipmi_ioremap (ipmi_locate_ctx_t ctx,
               uint64_t physical_address,
               size_t physical_address_len,
               void **virtual_address,
               void **mapped_address,
               size_t *mapped_address_len)
{
  uint64_t startaddress;
  uint32_t pad;
  int mem_fd = -1;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (physical_address_len);
  assert (virtual_address);
  assert (mapped_address);
  assert (mapped_address_len);

  if (!_ipmi_physical_address_valid (ctx,
                                     physical_address,
                                     physical_address_len))
    return (-1);

  if ((mem_fd = open ("/dev/mem",
                      O_RDONLY|O_SYNC)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* XXX: what is the error return for getpagesize??? */
  pad = physical_address % getpagesize ();
  startaddress = physical_address - pad;
  *mapped_address_len = physical_address_len + pad;

  if ((*mapped_address = mmap (NULL,
                               *mapped_address_len,
                               PROT_READ,
                               MAP_PRIVATE,
                               mem_fd,
                               startaddress)) == MAP_FAILED)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  *virtual_address = (*mapped_address) + pad;
  rv = 0;
 cleanup:
  /* ignore potential error, cleanup path */
  close (mem_fd);
  return (rv);
}

static void
_ipmi_iounmap (ipmi_locate_ctx_t ctx,
               void *mapped_address,
               size_t mapped_address_len)
{
  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);

  /* ignore potential error, void return func */
  munmap (mapped_address, mapped_address_len);
}

static int
_ipmi_get_physical_mem_data (ipmi_locate_ctx_t ctx,
                             uint64_t physical_address,
                             size_t length,
                             uint8_t *data)
{
  void *virtual_address = NULL;
  void *mapped_address = NULL;
  size_t mapped_address_len = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (data);

  if (_ipmi_ioremap (ctx,
                     physical_address,
                     length,
                     &virtual_address,
                     &mapped_address,
                     &mapped_address_len) < 0)
    return (-1);

  memcpy (data, virtual_address, length);

  _ipmi_iounmap (ctx, mapped_address, mapped_address_len);

  return (0);
}

/*******************************************************************************
 *
 * FUNCTION:
 *   _ipmi_acpi_get_rsdp
 *
 * PARAMETERS:
 *   rsdp_window_base_address  - Starting pointer for search
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
static int
_ipmi_acpi_get_rsdp (ipmi_locate_ctx_t ctx,
                     uint64_t rsdp_window_base_address,
                     size_t rsdp_window_size,
                     fiid_obj_t obj_acpi_rsdp_descriptor)
{
  uint8_t *memdata = NULL;
  int acpi_rsdp_descriptor_len;
  size_t i;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (fiid_obj_valid (obj_acpi_rsdp_descriptor));
  assert (fiid_obj_template_compare (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor) == 1);

  if (!(memdata = malloc (rsdp_window_size)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (memdata, '\0', rsdp_window_size);

  if ((acpi_rsdp_descriptor_len = fiid_template_len_bytes (tmpl_acpi_rsdp_descriptor)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (_ipmi_get_physical_mem_data (ctx,
                                   rsdp_window_base_address,
                                   rsdp_window_size,
                                   memdata) < 0)
    goto cleanup;

  /* Search from given start address for the requested length  */
  for (i = 0; i < rsdp_window_size; i += IPMI_ACPI_RSDP_SCAN_STEP)
    {
      /* check RSDP signature */
      if (strncmp ((char *)&memdata[i],
                   IPMI_ACPI_RSDP_SIG,
                   strlen (IPMI_ACPI_RSDP_SIG)) != 0)
        continue;

      /* now check the checksum */
      if (!_ipmi_acpi_table_checksum (ctx,
                                      &memdata[i],
                                      IPMI_ACPI_RSDP_CHECKSUM_LENGTH))
        {
          if (fiid_obj_set_all (obj_acpi_rsdp_descriptor,
                                &memdata[i],
                                acpi_rsdp_descriptor_len) < 0)
            {
              LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
              goto cleanup;
            }

          /* check this RSDP has RSDT/XSDT */
          {
            uint64_t val;
            uint8_t revision;
            uint64_t rsdt_xsdt_address;
            char *rsdt_xsdt_signature;
            uint8_t *rsdt_xsdt_table = NULL;
            uint32_t rsdt_xsdt_table_length;

            if (FIID_OBJ_GET (obj_acpi_rsdp_descriptor,
                              "revision",
                              &val) < 0)
              {
                LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
                goto cleanup;
              }

            revision = val;
            if (revision < 2)
              {
                if (FIID_OBJ_GET (obj_acpi_rsdp_descriptor,
                                  "rsdt_physical_address",
                                  &rsdt_xsdt_address) < 0)
                  {
                    LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
                    goto cleanup;
                  }

                rsdt_xsdt_signature = IPMI_ACPI_RSDT_SIG;
              }
            else
              {
                if (FIID_OBJ_GET (obj_acpi_rsdp_descriptor,
                                  "xsdt_physical_address",
                                  &rsdt_xsdt_address) < 0)
                  {
                    LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
                    goto cleanup;
                  }

                rsdt_xsdt_signature = IPMI_ACPI_XSDT_SIG;
              }

            /* achu: logic of code indicates should check for == 0, not < 0 */
            if (_ipmi_acpi_get_table (ctx,
                                      rsdt_xsdt_address,
                                      rsdt_xsdt_signature,
                                      &rsdt_xsdt_table,
                                      &rsdt_xsdt_table_length) == 0)
              {
                /* we found RSDT/XSDT */
                free (rsdt_xsdt_table);
                rv = 0;
                goto cleanup;
              }
            free (rsdt_xsdt_table);

            /* This is special case because of EFI */
            if (FIID_OBJ_GET (obj_acpi_rsdp_descriptor,
                              "rsdt_physical_address",
                              &rsdt_xsdt_address) < 0)
              {
                LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
                goto cleanup;
              }

            free (memdata);
            if (!(memdata = malloc (acpi_rsdp_descriptor_len)))
              {
                LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_OUT_OF_MEMORY);
                goto cleanup;
              }
            memset (memdata, '\0', acpi_rsdp_descriptor_len);
            
            if (_ipmi_get_physical_mem_data (ctx,
                                             rsdt_xsdt_address,
                                             acpi_rsdp_descriptor_len,
                                             memdata) < 0)
              goto cleanup;

            /* check RSDP signature */
            if (strncmp ((char *)memdata,
                         IPMI_ACPI_RSDP_SIG,
                         strlen (IPMI_ACPI_RSDP_SIG)))
              {
                LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
                goto cleanup;
              }

            /* now check the checksum */
            if (_ipmi_acpi_table_checksum (ctx,
                                           memdata,
                                           IPMI_ACPI_RSDP_CHECKSUM_LENGTH) != 0)
              {
                LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
                goto cleanup;
              }

            /* we found another RSDP */
            memcpy (obj_acpi_rsdp_descriptor, memdata, acpi_rsdp_descriptor_len);
          }

          rv = 0;
          goto cleanup;
        }
    }

 cleanup:
  free (memdata);
  return (-1);
}

/*******************************************************************************
 *
 * FUNCTION:
 *   _ipmi_acpi_get_table
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
static int
_ipmi_acpi_get_table (ipmi_locate_ctx_t ctx,
                      uint64_t table_address,
                      char *signature,
                      uint8_t **acpi_table,
                      uint32_t *acpi_table_length)
{
  uint64_t val;

  uint8_t table_signature_length;
  char *table_signature = NULL;

  fiid_obj_t obj_acpi_table_hdr = NULL;
  uint8_t *acpi_table_buf = NULL;
  uint32_t table_length = 0;
  int acpi_table_hdr_length;
  uint8_t *table = NULL;
  int len;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (signature);
  assert (acpi_table);
  assert (acpi_table_length);

  *acpi_table = NULL;

  if ((len = fiid_template_field_len_bytes (tmpl_acpi_table_hdr, "signature")) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  table_signature_length = len + 1;
  if (!(table_signature = malloc (table_signature_length)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (table_signature, '\0', table_signature_length);

  if ((acpi_table_hdr_length = fiid_template_len_bytes (tmpl_acpi_table_hdr)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(obj_acpi_table_hdr = fiid_obj_create (tmpl_acpi_table_hdr)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(acpi_table_buf = malloc (acpi_table_hdr_length)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (acpi_table_buf, '\0', acpi_table_hdr_length);

  if (_ipmi_get_physical_mem_data (ctx,
                                   table_address,
                                   acpi_table_hdr_length,
                                   acpi_table_buf) < 0)
    goto cleanup;

  if (fiid_obj_set_all (obj_acpi_table_hdr,
                        acpi_table_buf,
                        acpi_table_hdr_length) < 0)
    {
      LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_table_hdr);
      goto cleanup;
    }

  if (fiid_obj_get_data (obj_acpi_table_hdr,
                         "signature",
                         table_signature,
                         table_signature_length) < 0)
    {
      LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_table_hdr);
      goto cleanup;
    }

  if (strcmp (table_signature, signature))
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_acpi_table_hdr,
                    "length",
                    &val) < 0)
    {
      LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_table_hdr);
      goto cleanup;
    }
  table_length = val;

  if (!(table = malloc (table_length)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (table, '\0', table_length);

  if (_ipmi_get_physical_mem_data (ctx,
                                   table_address,
                                   table_length,
                                   table) < 0)
    goto cleanup;

  if (_ipmi_acpi_table_checksum (ctx,
                                 table,
                                 table_length) != 0)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  if (!(*acpi_table = malloc (table_length)))
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }
  memcpy (*acpi_table, table, table_length);
  *acpi_table_length = table_length;

  rv = 0;
 cleanup:
  free (table_signature);
  free (acpi_table_buf);
  free (table);
  fiid_obj_destroy (obj_acpi_table_hdr);
  return (rv);
}

/*******************************************************************************
 *
 * FUNCTION:
 *   _ipmi_acpi_get_firmware_table
 *
 * PARAMETERS:
 *   signature               - ACPI signature for firmware table header
 *   table_instance          - Which instance of the firmware table
 *   obj_acpi_table_hdr      - Initialized ACPI table header
 *   sign_table_data         - Initialized with malloc'ed ACPI firmware table data
 *   sign_table_data_length  - ACPI table DATA length
 *
 * RETURN:
 *   return (0) for success. ACPI table header and firmware table DATA are
 *   returned through obj_acpi_table_hdr and signed_table_data
 *   parameters.
 *
 * DESCRIPTION:
 *   Top level call for any ACPI firmware table by table signature string.
 *   It gets table header and data from RSDT/XSDT.
 *
 ******************************************************************************/
static int
_ipmi_acpi_get_firmware_table (ipmi_locate_ctx_t ctx,
                               char *signature,
                               unsigned int table_instance,
                               fiid_obj_t obj_acpi_table_hdr,
                               uint8_t **sign_table_data,
                               uint32_t *sign_table_data_length)
{
  uint64_t val;

  int acpi_table_hdr_length;
  int acpi_rsdp_descriptor_length;

  fiid_obj_t obj_acpi_rsdp_descriptor = NULL;
  uint64_t rsdt_xsdt_address;
  char *rsdt_xsdt_signature;
  uint8_t revision;

  uint8_t *rsdt_xsdt_table = NULL;
  uint32_t rsdt_xsdt_table_length;
  uint8_t *rsdt_xsdt_table_data;
  uint32_t rsdt_xsdt_table_data_length;
  unsigned int acpi_table_count;
  uint8_t *acpi_table = NULL;
  uint32_t acpi_table_length;

  fiid_obj_t obj_table = NULL;
  uint64_t table_address;
  unsigned int signature_table_count;
  unsigned int i;
  int rv = -1;
  fiid_template_t tmpl_table_address_32 =
    {
      { 32, "table_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  fiid_template_t tmpl_table_address_64 =
    {
      { 64, "table_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (signature);
  assert (fiid_obj_valid (obj_acpi_table_hdr));
  assert (sign_table_data);
  assert (sign_table_data_length);
  assert (fiid_obj_template_compare (obj_acpi_table_hdr, tmpl_acpi_table_hdr) == 1);

  *sign_table_data = NULL;

  if ((acpi_table_hdr_length = fiid_template_len_bytes (tmpl_acpi_table_hdr)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(obj_acpi_rsdp_descriptor = fiid_obj_create (tmpl_acpi_rsdp_descriptor)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((acpi_rsdp_descriptor_length = fiid_template_len_bytes (tmpl_acpi_rsdp_descriptor)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (_ipmi_acpi_get_rsdp (ctx,
                           IPMI_ACPI_LO_RSDP_WINDOW_BASE,
                           IPMI_ACPI_LO_RSDP_WINDOW_SIZE,
                           obj_acpi_rsdp_descriptor) < 0)
    {
      if (_ipmi_acpi_get_rsdp (ctx,
                               IPMI_ACPI_HI_RSDP_WINDOW_BASE,
                               IPMI_ACPI_HI_RSDP_WINDOW_SIZE,
                               obj_acpi_rsdp_descriptor) < 0)
        goto cleanup;
    }

  if (FIID_OBJ_GET (obj_acpi_rsdp_descriptor,
                    "revision",
                    &val) < 0)
    {
      LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
      goto cleanup;
    }

  revision = val;
  if (revision < 2)
    {
      if (FIID_OBJ_GET (obj_acpi_rsdp_descriptor,
                        "rsdt_physical_address",
                        &rsdt_xsdt_address) < 0)
        {
          LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
          goto cleanup;
        }
      rsdt_xsdt_signature = IPMI_ACPI_RSDT_SIG;
    }
  else
    {
      if (FIID_OBJ_GET (obj_acpi_rsdp_descriptor,
                        "xsdt_physical_address",
                        &rsdt_xsdt_address) < 0)
        {
          LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_rsdp_descriptor);
          goto cleanup;
        }
      rsdt_xsdt_signature = IPMI_ACPI_XSDT_SIG;
    }

  if (_ipmi_acpi_get_table (ctx,
                            rsdt_xsdt_address,
                            rsdt_xsdt_signature,
                            &rsdt_xsdt_table,
                            &rsdt_xsdt_table_length) < 0)
    goto cleanup;

  rsdt_xsdt_table_data_length = rsdt_xsdt_table_length - acpi_table_hdr_length;
  rsdt_xsdt_table_data = (rsdt_xsdt_table + acpi_table_hdr_length);

  if (revision < 2)
    acpi_table_count = rsdt_xsdt_table_data_length / 4;
  else
    acpi_table_count = rsdt_xsdt_table_data_length / 8;

  acpi_table = NULL;
  acpi_table_length = 0;
  for (i = 0, signature_table_count = 0; i < acpi_table_count; i++)
    {
      fiid_field_t *tmpl_table_address = NULL;
      int len_table;

      if (revision < 2)
        tmpl_table_address = &tmpl_table_address_32[0];
      else
        tmpl_table_address = &tmpl_table_address_64[0];

      if (!(obj_table = fiid_obj_create (tmpl_table_address)))
        {
          LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if ((len_table = fiid_template_len_bytes (tmpl_table_address)) < 0)
        {
          LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj_table,
                            (rsdt_xsdt_table_data + (i * 4)),
                            len_table) < 0)
        {
          LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_table);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_table,
                        "table_address",
                        &table_address) < 0)
        {
          LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_table);
          goto cleanup;
        }

      fiid_obj_destroy (obj_table);

      if (_ipmi_acpi_get_table (ctx,
                                table_address,
                                signature,
                                &acpi_table,
                                &acpi_table_length) < 0)
        continue;

      signature_table_count++;
      if (signature_table_count == table_instance)
        break;

      free (acpi_table);
      acpi_table = NULL;
      acpi_table_length = 0;
    }

  if (!acpi_table)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  memcpy (obj_acpi_table_hdr, acpi_table, acpi_table_hdr_length);
  *sign_table_data_length = acpi_table_length - acpi_table_hdr_length;
  if (!(*sign_table_data = malloc (*sign_table_data_length)))
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }
  memcpy (*sign_table_data,
          (acpi_table + acpi_table_hdr_length),
          *sign_table_data_length);

  rv = 0;
 cleanup:
  free (acpi_table);
  free (rsdt_xsdt_table);
  fiid_obj_destroy (obj_table);
  fiid_obj_destroy (obj_acpi_rsdp_descriptor);
  return (rv);
}

/*******************************************************************************
 *
 * FUNCTION:
 *   _ipmi_acpi_get_spmi_table
 *
 * PARAMETERS:
 *   interface_type           - Type of interface to look for (KCS, SSIF, SMIC, BT)
 *   obj_acpi_table_hdr       - Initialized ACPI table header
 *   acpi_table_firmware      - Initialized ACPI firmware table
 *
 * RETURN:
 *   return (0) for success. ACPI table header and SPMI table is
 *   returned through obj_acpi_table_hdr and obj_acpi_spmi_table_descriptor
 *   parameters.
 *
 * DESCRIPTION:
 *   Get SPMI table for the given interface type.
 *
 ******************************************************************************/
static int
_ipmi_acpi_get_spmi_table (ipmi_locate_ctx_t ctx,
                           uint8_t interface_type,
                           fiid_obj_t obj_acpi_table_hdr,
                           fiid_obj_t obj_acpi_spmi_table_descriptor)
{
  uint64_t val;
  uint8_t table_interface_type;
  uint8_t *table_data = NULL;
  uint32_t table_data_length = 0;
  uint32_t copy_length;
  unsigned int instance;
  int acpi_spmi_table_descriptor_len;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (fiid_obj_valid (obj_acpi_table_hdr));
  assert (fiid_obj_valid (obj_acpi_spmi_table_descriptor));
  assert (fiid_obj_template_compare (obj_acpi_table_hdr, tmpl_acpi_table_hdr) == 1);
  assert (fiid_obj_template_compare (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor) == 1);

  for (instance = 0; instance < IPMI_INTERFACE_COUNT; instance++)
    {
      if (_ipmi_acpi_get_firmware_table (ctx,
                                         IPMI_ACPI_SPMI_SIG,
                                         instance,
                                         obj_acpi_table_hdr,
                                         &table_data,
                                         &table_data_length) < 0)
        continue;

#if 0
      printf ("__DEBUG__ instance = %d, signature = [%s] found\n",
              instance, IPMI_ACPI_SPMI_SIG);
#endif

      if ((acpi_spmi_table_descriptor_len = fiid_template_len_bytes (tmpl_acpi_spmi_table_descriptor)) < 0)
        {
          LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (acpi_spmi_table_descriptor_len < table_data_length)
        copy_length = acpi_spmi_table_descriptor_len;
      else
        copy_length = table_data_length;

#if 0
      if (copy_length != table_data_length)
        printf ("_DEBUG_ table_data_length=%d, template_length=%d,"
                " tmpl_acpi_spmi_table_descriptor length is too short\n",
                table_data_length, acpi_spmi_table_descriptor_len);
#endif

      if (fiid_obj_set_all (obj_acpi_spmi_table_descriptor,
                            table_data,
                            copy_length) < 0)
        {
          LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
          goto cleanup;
        }

      free (table_data);
      table_data = NULL;
      table_data_length = 0;

      if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                        "interface_type",
                        &val) < 0)
        {
          LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
          goto cleanup;
        }

      table_interface_type = val;
      if (table_interface_type == interface_type)
        rv = 0;
    }

 cleanup:
  free (table_data);
  return (rv);
}

int
ipmi_locate_acpi_spmi_get_device_info (ipmi_locate_ctx_t ctx,
                                       ipmi_interface_type_t type,
                                       struct ipmi_locate_info *info)
{
  fiid_obj_t obj_acpi_table_hdr = NULL;
  fiid_obj_t obj_acpi_spmi_table_descriptor = NULL;
  struct ipmi_locate_info linfo;
  uint64_t val;
  int rv = -1;

#if defined(__arm__) || defined(__aarch64__)
  return (-1);
#else
  if (!ctx || ctx->magic != IPMI_LOCATE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_locate_ctx_errormsg (ctx), ipmi_locate_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_INTERFACE_TYPE_VALID (type) || !info)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_PARAMETERS);
      return (-1);
    }

  memset (&linfo, '\0', sizeof (struct ipmi_locate_info));
  linfo.interface_type = type;
  if (type == IPMI_INTERFACE_SSIF)
    {
      strncpy (linfo.driver_device, IPMI_DEFAULT_I2C_DEVICE, IPMI_LOCATE_PATH_MAX);
      linfo.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
    }
  linfo.locate_driver_type = IPMI_LOCATE_DRIVER_ACPI;

  if (!(obj_acpi_table_hdr = fiid_obj_create (tmpl_acpi_table_hdr)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(obj_acpi_spmi_table_descriptor = fiid_obj_create (tmpl_acpi_spmi_table_descriptor)))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (_ipmi_acpi_get_spmi_table (ctx,
                                 type,
                                 obj_acpi_table_hdr,
                                 obj_acpi_spmi_table_descriptor) < 0)
    goto cleanup;

  /* I don't see any reason to perform this check now -- Anand Babu */
  /* This field must always be 01h to be compatible with any software
     that implements previous versions of this spec. */
#if 0
  {
    uint8_t ipmi_legacy;
    
    if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                      tmpl_acpi_spmi_table_descriptor,
                      "ipmi_legacy",
                      &val) < 0)
      {
        LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
        goto cleanup;
      }
    ipmi_legacy = val;
    
    if (ipmi_legacy != 1)
      {
        LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
        goto cleanup;
      }
  }
#endif

  /* IPMI version */
  {
    uint8_t ipmi_version_major, ipmi_version_minor;

    if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                      "specification_revision.major",
                      &val) < 0)
      {
        LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
        goto cleanup;
      }
    ipmi_version_major = val;
    
    if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                      "specification_revision.minor",
                      &val) < 0)
      {
        LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
        goto cleanup;
      }
    ipmi_version_minor = val;

    linfo.ipmi_version_major = ipmi_version_major;
    linfo.ipmi_version_minor = ipmi_version_minor;
  }

  /* Interface type - KCS, SMIC, SSIF, BT */
  {
    uint8_t interface_type;

    if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                      "interface_type",
                      &val) < 0)
      {
        LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
        goto cleanup;
      }
    interface_type = val;

    if (!IPMI_INTERFACE_TYPE_VALID (interface_type))
      {
        LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
        goto cleanup;
      }

    linfo.interface_type = interface_type;
  }

  /* Address space id (memory mapped, IO mapped, SMBus) and IO base address */
  {
    uint8_t address_space_id;
    uint64_t base_address;

    if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                      "base_address.address_space_id",
                      &val) < 0)
      {
        LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
        goto cleanup;
      }
    address_space_id = val;

    if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                      "base_address.address",
                      &val) < 0)
      {
        LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
        goto cleanup;
      }
    base_address = val;

    switch (address_space_id)
      {
      case IPMI_ACPI_ADDRESS_SPACE_ID_SYSTEM_MEMORY:
        {
          linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY;
          linfo.driver_address = base_address;
          break;
        }
      case IPMI_ACPI_ADDRESS_SPACE_ID_SYSTEM_IO:
        {
          linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
          linfo.driver_address = base_address;
          break;
        }
      case IPMI_ACPI_ADDRESS_SPACE_ID_SMBUS:
        {
          linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
          linfo.driver_address = base_address;
          break;
        }
      default:
        LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
        goto cleanup;
      }
  }

  /* Register spacing */
  {
    uint8_t register_bit_width;

    if (FIID_OBJ_GET (obj_acpi_spmi_table_descriptor,
                      "base_address.register_bit_width",
                      &val) < 0)
      {
        LOCATE_FIID_OBJECT_ERROR_TO_LOCATE_ERRNUM (ctx, obj_acpi_spmi_table_descriptor);
        goto cleanup;
      }
    register_bit_width = val;

    linfo.register_spacing = (register_bit_width / 8);
  }

  memcpy (info, &linfo, sizeof (struct ipmi_locate_info));
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_acpi_table_hdr);
  fiid_obj_destroy (obj_acpi_spmi_table_descriptor);
  return (rv);
#endif /* defined(__arm__) || defined(__aarch64__) */
}
