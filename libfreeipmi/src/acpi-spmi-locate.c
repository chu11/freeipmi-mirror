/* 
   acpi-spmi-locate.c - ACPI tables driver to locate IPMI interfaces
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

#include "freeipmi.h"

fiid_template_t tmpl_acpi_rsdp_descriptor =  /* Root System Descriptor Pointer */
  {
    {64, "signature"},              /* ACPI signature, contains "RSD PTR " */
    {8 , "checksum"},               /* To make sum of struct == 0 */
    {48, "oem_id"},                 /* OEM identification */
    {8 , "revision"},               /* Must be 0 for 1.0, 2 for 2.0 */
    {32, "rsdt_physical_address"},  /* 32-bit physical address of RSDT */
    {32, "length"},                 /* XSDT Length in bytes including hdr */
    {64, "xsdt_physical_address"},  /* 64-bit physical address of XSDT */
    {8 , "extended_checksum"},      /* Checksum of entire table */
    {24, "reserved"},               /* Reserved field must be 0 */
    {0, ""}
  };

fiid_template_t tmpl_acpi_table_hdr =
  {
    {32, "signature"},              /* ACPI signature (4 ASCII characters) */
    {32, "length"},                 /* Length of table, in bytes, including header */
    {8,  "revision"},               /* ACPI Specification minor version # */
    {8,  "checksum"},               /* To make sum of entire table == 0 */
    {48, "oem_id"},                 /* OEM identification */
    {64, "oem_table_id"},           /* OEM table identification */
    {32, "oem_revision"},           /* OEM revision number */ 
    {32, "asl_compiler_id"},        /* ASL compiler vendor ID */
    {32, "asl_compiler_revision"},  /* ASL compiler revision number */
    {0,  ""}
  };

fiid_template_t tmpl_acpi_spmi_table_descriptor = 
  {
    {32, "signature"}, /* `SPMI'. Signature for the Service Processor Management 
			  Interface Table. */
    {32, "length"},    /* Length, in bytes, of the entire Service Processor Management 
			  Interface Table. */
    {8,  "revision"},  /* 5 */
    {8,  "checksum"},  /* Entire table must sum to zero. */
    {48, "oemid"},     /* OEM ID. Per ACPI specification. An OEM-supplied string that 
			  identifies the OEM. */
    {64, "oem_table_id"}, /* For the Service Processor Management Interface Table, 
			     the table ID is the manufacturer model ID 
			     (assigned by the OEM identified by "OEM ID"). */
    {32, "oem_revision"}, /* OEM revision of Service Processor Management Interface
			     Table for supplied the given OEM Table ID. Per ACPI, this is
			     "An OEM-supplied revision number. Larger numbers are
			     assumed to be newer revisions." */
    {32, "creator_id"},   /* Vendor ID of utility that created the table. For the tables
			     containing Definition Blocks, this is the ID for the ASL
			     Compiler. */
    {32, "creator_revision"}, /* Revision of utility that created the table. For the tables
				 containing Definition Blocks, this is the revision 
				 for the ASL Compiler. */
    {8, "interface_type"},    /* Indicates the type of IPMI interface:
				 0 Reserved
				 1 Keyboard Controller Style (KCS)
				 2 Server Management Interface Chip (SMIC)
				 3 Block Transfer (BT)
				 4 SMBus System Interface (SSIF)
				 5-255 Reserved */
    {8, "ipmi_legacy"},         /* This field must always be 01h to be compatible with any
				 software that implements previous 
				 versions of this spec. */
/*     {16, "specification_revision"},  *//* Identifies the IPMI specification revision, 
				       in BCD format, to which the interface 
				       was designed. The first byte holds the
				       most significant digits, while second byte holds 
				       the least significant digits of the revision, 
				       e.g. a value of 0x0150 indicates the interface is 
				       compatible with IPMI version v1.5. */
    {8, "specification_revision.major"},
    {8, "specification_revision.minor"},
    {1, "interrupt_type.sci_triggered_thru_gpe"},  /* Interrupt type(s) used by 
						      the interface:
						      [0] - SCI triggered through GPE 
						      (use 0b for SSIF)
						        0 = not supported
						        1 = supported */
    {1, "interrupt_type.io_apic_sapic_interrupt"}, /* [1] - I/O APIC/SAPIC interrupt 
						      (Global System Interrupt) */
    {6, "interrupt_type.reserved"}, /* [7:2] - Reserved (must be 0) */
    
    {8, "gpe"},  /* The bit assignment of the SCI interrupt within the GPEx_STS
		    register of a GPE described if the FADT that the interface
		    triggers. (Note: This field is valid only if Bit[0] of the Interrupt
		    Type field is set. Otherwise set to 00h.) */
    {8, "reserved1"}, /* 00h */
    {1, "pci_device_flag"}, /* [0] - PCI Device Flag. For PCI IPMI devices, 
			       this bit is set. For non-PCI devices, this bit is cleared. 
			       When this bit is cleared, the PCI Segment Group, Bus, 
			       Device and Function Number fields combined corresponds 
			       to the ACPI _UID value of the device whose _HID or _CID 
			       contains IPI0001 plug and play ID. 
			       _UID must be an integer. Byte 60 contains the least
			       significant byte of the _UID value. Set to 0b for SSIF. */
    {7, "pci_device_flag.reserved"}, /* [7:1] - Reserved */
    {32, "global_system_interrupt"}, /* The I/O APIC or I/O SAPIC Global System 
					Interrupt used by the interface. 
					(Note: This field is valid only if Bit[1] of the 
					Interrupt Type field is set. 
					Otherwise set to 00h.) */
/*     {96, "base_address"},  *//* The base address of the interface register 
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
    {8,  "base_address.address_space_id"},    /* Address space where
						 struct or register
						 exists. */  
    {8,  "base_address.register_bit_width"},  /* Size in bits of given
						 register */ 
    {8,  "base_address.register_bit_offset"}, /* Bit offset within the
						 register */ 
    {8,  "base_address.reserved"},            /* Must be 0 */
    {64, "base_address.address"},             /* 64-bit address of
						 struct or register */ 

    {32, "uid"},  /* for IPMI cards not located on PCI */
    {8, "reserved2"}, /* This field must always be null (0x00) to be compatible with
			any software that implements previous versions of this spec.
			This field is a deprecated "SPMI ID Field". Implementations
			based on pre-IPMI v2.0 versions of SPMI may contain a null-
			terminated string here. */
    {0,  ""}
  };

fiid_template_t tmpl_acpi_spmi_table_descriptor_ssif = 
  {
    {32, "signature"}, /* `SPMI'. Signature for the Service Processor Management 
			  Interface Table. */
    {32, "length"},    /* Length, in bytes, of the entire Service Processor Management 
			  Interface Table. */
    {8,  "revision"},  /* 5 */
    {8,  "checksum"},  /* Entire table must sum to zero. */
    {48, "oemid"},     /* OEM ID. Per ACPI specification. An OEM-supplied string that 
			  identifies the OEM. */
    {64, "oem_table_id"}, /* For the Service Processor Management Interface Table, 
			     the table ID is the manufacturer model ID 
			     (assigned by the OEM identified by "OEM ID"). */
    {32, "oem_revision"}, /* OEM revision of Service Processor Management Interface
			     Table for supplied the given OEM Table ID. Per ACPI, this is
			     "An OEM-supplied revision number. Larger numbers are
			     assumed to be newer revisions." */
    {32, "creator_id"},   /* Vendor ID of utility that created the table. For the tables
			     containing Definition Blocks, this is the ID for the ASL
			     Compiler. */
    {32, "creator_revision"}, /* Revision of utility that created the table. For the tables
				 containing Definition Blocks, this is the revision 
				 for the ASL Compiler. */
    {8, "interface_type"},    /* Indicates the type of IPMI interface:
				 0 Reserved
				 1 Keyboard Controller Style (KCS)
				 2 Server Management Interface Chip (SMIC)
				 3 Block Transfer (BT)
				 4 SMBus System Interface (SSIF)
				 5-255 Reserved */
    {8, "ipmi_legacy"},       /* This field must always be 01h to be compatible with any
				 software that implements previous 
				 versions of this spec. */
    /* {16, "specification_revision"},  *//* Identifies the IPMI specification revision, 
				       in BCD format, to which the interface 
				       was designed. The first byte holds the
				       most significant digits, while second byte holds 
				       the least significant digits of the revision, 
				       e.g. a value of 0x0150 indicates the interface is 
				       compatible with IPMI version v1.5. */
    {8, "specification_revision.major"},
    {8, "specification_revision.minor"},
    {1, "interrupt_type.sci_triggered_thru_gpe"},  /* Interrupt type(s) used by 
						      the interface:
						      [0] - SCI triggered through GPE 
						      (use 0b for SSIF)
						        0 = not supported
						        1 = supported */
    {1, "interrupt_type.io_apic_sapic_interrupt"}, /* [1] - I/O APIC/SAPIC interrupt 
						      (Global System Interrupt) */
    {6, "interrupt_type.reserved"}, /* [7:2] - Reserved (must be 0) */
    
    {8, "gpe"},  /* The bit assignment of the SCI interrupt within the GPEx_STS
		    register of a GPE described if the FADT that the interface
		    triggers. (Note: This field is valid only if Bit[0] of the Interrupt
		    Type field is set. Otherwise set to 00h.) */
    {8, "reserved1"}, /* 00h */
    {1, "pci_device_flag"}, /* [0] - PCI Device Flag. For PCI IPMI devices, 
			       this bit is set. For non-PCI devices, this bit is cleared. 
			       When this bit is cleared, the PCI Segment Group, Bus, 
			       Device and Function Number fields combined corresponds 
			       to the ACPI _UID value of the device whose _HID or _CID 
			       contains IPI0001 plug and play ID. 
			       _UID must be an integer. Byte 60 contains the least
			       significant byte of the _UID value. Set to 0b for SSIF. */
    {7, "pci_device_flag.reserved"}, /* [7:1] - Reserved */
    {32, "global_system_interrupt"}, /* The I/O APIC or I/O SAPIC Global System 
					Interrupt used by the interface. 
					(Note: This field is valid only if Bit[1] of the 
					Interrupt Type field is set. 
					Otherwise set to 00h.) */
/*     {96, "base_address"},  *//* The base address of the interface register 
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
    {8,  "base_address.address_space_id"},    /* Address space where
						 struct or register
						 exists. */  
    {8,  "base_address.register_bit_width"},  /* Size in bits of given
						 register */ 
    {8,  "base_address.register_bit_offset"}, /* Bit offset within the
						 register */ 
    {8,  "base_address.address_size"},        /* field = 1 (byte
						 access) */
    {7, "base_address.address"},              /* SMBus address of BMC
						 SSIF */
    {57, "base_address.reserved"},            /* Reserved */
    {32, "uid"},  /* for IPMI cards not located on PCI */
    {8, "reserved2"}, /* This field must always be null (0x00) to be compatible with
			any software that implements previous versions of this spec.
			This field is a deprecated "SPMI ID Field". Implementations
			based on pre-IPMI v2.0 versions of SPMI may contain a null-
			terminated string here. */
    {0,  ""}
  };

fiid_template_t tmpl_acpi_spmi_table_descriptor_pci_ipmi = 
  {
    {32, "signature"}, /* `SPMI'. Signature for the Service Processor Management 
			  Interface Table. */
    {32, "length"},    /* Length, in bytes, of the entire Service Processor Management 
			  Interface Table. */
    {8,  "revision"},  /* 5 */
    {8,  "checksum"},  /* Entire table must sum to zero. */
    {48, "oemid"},     /* OEM ID. Per ACPI specification. An OEM-supplied string that 
			  identifies the OEM. */
    {64, "oem_table_id"}, /* For the Service Processor Management Interface Table, 
			     the table ID is the manufacturer model ID 
			     (assigned by the OEM identified by "OEM ID"). */
    {32, "oem_revision"}, /* OEM revision of Service Processor Management Interface
			     Table for supplied the given OEM Table ID. Per ACPI, this is
			     "An OEM-supplied revision number. Larger numbers are
			     assumed to be newer revisions." */
    {32, "creator_id"},   /* Vendor ID of utility that created the table. For the tables
			     containing Definition Blocks, this is the ID for the ASL
			     Compiler. */
    {32, "creator_revision"}, /* Revision of utility that created the table. For the tables
				 containing Definition Blocks, this is the revision 
				 for the ASL Compiler. */
    {8, "interface_type"},    /* Indicates the type of IPMI interface:
				 0 Reserved
				 1 Keyboard Controller Style (KCS)
				 2 Server Management Interface Chip (SMIC)
				 3 Block Transfer (BT)
				 4 SMBus System Interface (SSIF)
				 5-255 Reserved */
    {8, "ipmi_legacy"},       /* This field must always be 01h to be compatible with any
				 software that implements previous 
				 versions of this spec. */
    /* {16, "specification_revision"}, */ /* Identifies the IPMI specification revision, 
				       in BCD format, to which the interface 
				       was designed. The first byte holds the
				       most significant digits, while second byte holds 
				       the least significant digits of the revision, 
				       e.g. a value of 0x0150 indicates the interface is 
				       compatible with IPMI version v1.5. */
    {8, "specification_revision.major"},
    {8, "specification_revision.minor"},
    {1, "interrupt_type.sci_triggered_thru_gpe"},  /* Interrupt type(s) used by 
						      the interface:
						      [0] - SCI triggered through GPE 
						      (use 0b for SSIF)
						        0 = not supported
						        1 = supported */
    {1, "interrupt_type.io_apic_sapic_interrupt"}, /* [1] - I/O APIC/SAPIC interrupt 
						      (Global System Interrupt) */
    {6, "interrupt_type.reserved"}, /* [7:2] - Reserved (must be 0) */
    
    {8, "gpe"},  /* The bit assignment of the SCI interrupt within the GPEx_STS
		    register of a GPE described if the FADT that the interface
		    triggers. (Note: This field is valid only if Bit[0] of the Interrupt
		    Type field is set. Otherwise set to 00h.) */
    {8, "reserved1"}, /* 00h */
    {1, "pci_device_flag"}, /* [0] - PCI Device Flag. For PCI IPMI devices, 
			       this bit is set. For non-PCI devices, this bit is cleared. 
			       When this bit is cleared, the PCI Segment Group, Bus, 
			       Device and Function Number fields combined corresponds 
			       to the ACPI _UID value of the device whose _HID or _CID 
			       contains IPI0001 plug and play ID. 
			       _UID must be an integer. Byte 60 contains the least
			       significant byte of the _UID value. Set to 0b for SSIF. */
    {7,  "pci_device_flag.reserved"}, /* [7:1] - Reserved */
    {32, "global_system_interrupt"},  /* The I/O APIC or I/O SAPIC Global System 
					 Interrupt used by the interface. 
					 (Note: This field is valid only if Bit[1] of the 
					 Interrupt Type field is set. 
					 Otherwise set to 00h.) */
    /* {96, "base_address"} */ /* The base address of the interface register 
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
    {8,  "base_address.address_space_id"},    /* Address space where
						 struct or register
						 exists. */  
    {8,  "base_address.register_bit_width"},  /* Size in bits of given
						 register */ 
    {8,  "base_address.register_bit_offset"}, /* Bit offset within the
						 register */ 
    {8,  "base_address.reserved"},            /* Must be 0 */
    {64, "base_address.address"},             /* 64-bit address of
						 struct or register */ 
    {8, "pci_segment_group_number"}, /* PCI Segment Group Number, 
					if the IPMI device is a PCI
					device. Otherwise, this field is byte 1 of a UID. 
					See description for PCI Device Flag, above. */
    {8, "pci_bus_number"}, /* PCI Bus Number, if the IPMI device is a PCI device.
			      Otherwise, this field is byte 2 of a UID. 
			      See description for PCI Device Flag, above. */
    {4, "pci_device_number"}, /* PCI Device fields or byte 3 of a UID. Per PCI Device Flag,
				 above.
				 For PCI Device Flag = 1b:
				 [4:0] - PCI Device Number: The PCI device number if the
				 IPMI device is a PCI device.
				 For PCI Device Flag = 0b:
				 [7:0] - byte 3 of UID */
    {4, "pci_device_number.reserved"}, /* [7:5] - Reserved */
    {3, "pci_function_number"}, /* PCI Device fields or byte 4 of a UID. 
				   Per PCI Device Flag, above.
				   For PCI Device Flag = 1b:
				   [2:0] - PCI Function Number: The PCI function number if
				   the IPMI device is a PCI device. */
    {3, "pci_function_number.reserved1"}, /* [5:3] - Reserved */
    {1, "pci_function_number.interrupt_flag"}, /* [6] -    Interrupt Flag:
						  0b = interrupt not supported
						  1b = interrupt supported */
    {1, "pci_function_number.reserved2"}, /* [7] -    Reserved 
					     For PCI Device Flag = 0b:
					     [7:0] - byte 4 of UID */
    {8, "reserved2"}, /* This field must always be null (0x00) to be compatible with
			any software that implements previous versions of this spec.
			This field is a deprecated "SPMI ID Field". Implementations
			based on pre-IPMI v2.0 versions of SPMI may contain a null-
			terminated string here. */
    {0,  ""}
  };

uint8_t 
ipmi_acpi_table_chksum (uint8_t *buffer, size_t len)
{
  int i = 0;
  uint8_t sum = 0;
  
  if (buffer == NULL)
    return 0;
  
  for (i = 0; i < len; i++)
    sum += buffer[i];
  
  return sum;
}

int 
ipmi_acpi_get_rsdp (uint64_t rsdp_window_base_addr, size_t rsdp_window_size, 
		    fiid_obj_t obj_acpi_rsdp_descriptor)
{
  uint8_t *memdata = NULL;
  int acpi_rsdp_descriptor_len;
  int i;
  
  if (obj_acpi_rsdp_descriptor == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  memdata = alloca (rsdp_window_size);
  memset (memdata, 0, rsdp_window_size);
  
  acpi_rsdp_descriptor_len = fiid_obj_len_bytes (tmpl_acpi_rsdp_descriptor);
  
  if (ipmi_get_physical_mem_data (rsdp_window_base_addr, 
				  rsdp_window_size, memdata) != 0)
    return (-1);
  
  /* Search from given start addr for the requested length  */
  for (i = 0; i < rsdp_window_size; i += IPMI_ACPI_RSDP_SCAN_STEP)
    {
      /* check RSDP signature */
      if (strncmp ((char *)&memdata[i], 
		   IPMI_ACPI_RSDP_SIG, 
		   strlen (IPMI_ACPI_RSDP_SIG)) != 0)
	continue;
      
      /* now check the checksum */
      if (ipmi_acpi_table_chksum (&memdata[i], 
				  IPMI_ACPI_RSDP_CHECKSUM_LENGTH) == 0)
	{
	  memcpy (obj_acpi_rsdp_descriptor, &memdata[i], acpi_rsdp_descriptor_len);
	  
	  /* check this RSDP has RSDT/XSDT */
	  {
	    uint64_t val;
	    uint8_t revision;
	    uint64_t rsdt_xsdt_address;
	    char *rsdt_xsdt_signature;
	    uint8_t *rsdt_xsdt_table = NULL;
	    uint32_t rsdt_xsdt_table_length;
	    
	    fiid_obj_get (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor, 
			  (uint8_t *)"revision", &val);
	    revision = val;
	    if (revision < 2)
	      { 
		fiid_obj_get (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor, 
			      (uint8_t *)"rsdt_physical_address", &rsdt_xsdt_address);
		rsdt_xsdt_signature = strdupa (IPMI_ACPI_RSDT_SIG);
	      }
	    else 
	      {
		fiid_obj_get (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor, 
			      (uint8_t *)"xsdt_physical_address", &rsdt_xsdt_address);
		rsdt_xsdt_signature = strdupa (IPMI_ACPI_XSDT_SIG);
	      }
	    
	    if (ipmi_acpi_get_table (rsdt_xsdt_address, rsdt_xsdt_signature, 
				     &rsdt_xsdt_table, 
				     &rsdt_xsdt_table_length) == 0)
	      {
		/* we found RSDT/XSDT */
		free (rsdt_xsdt_table);
		return 0;
	      }
	    free (rsdt_xsdt_table);
	    
	    /* This is special case because of EFI */
	    fiid_obj_get (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor, 
			  (uint8_t *)"rsdt_physical_address", &rsdt_xsdt_address);
	    memdata = alloca (acpi_rsdp_descriptor_len);
	    memset (memdata, 0, acpi_rsdp_descriptor_len);
	    if (ipmi_get_physical_mem_data (rsdt_xsdt_address, 
					    acpi_rsdp_descriptor_len, 
					    memdata) != 0)
	      return (-1);
	    
	    /* check RSDP signature */
	    if (strncmp ((char *)memdata, 
			 IPMI_ACPI_RSDP_SIG, 
			 strlen (IPMI_ACPI_RSDP_SIG)) != 0)
	      return (-1);
	    
	    /* now check the checksum */
	    if (ipmi_acpi_table_chksum (memdata, 
					IPMI_ACPI_RSDP_CHECKSUM_LENGTH) != 0)
	      return (-1);
	    
	    /* we found another RSDP */
	    memcpy (obj_acpi_rsdp_descriptor, memdata, acpi_rsdp_descriptor_len);
	  }
	  
	  return 0;
	}
    }
  
  return (-1);
}

int 
ipmi_acpi_get_table (uint64_t table_address, char *signature, 
		     uint8_t **acpi_table, uint32_t *acpi_table_length)
{
  uint64_t val;
  
  uint8_t table_signature_length;
  char *table_signature;
  
  fiid_obj_t obj_acpi_table_hdr;
  uint32_t acpi_table_hdr_length;
  uint32_t table_length = 0;
  uint8_t *table;
  
  if (signature == NULL || acpi_table == NULL || acpi_table_length == NULL)
    return (-1);
  
  table_signature_length = fiid_obj_field_len_bytes (tmpl_acpi_table_hdr, 
						     (uint8_t *)"signature") + 1;
  table_signature = alloca (table_signature_length);
  memset (table_signature, 0, table_signature_length);
  
  acpi_table_hdr_length = fiid_obj_len_bytes (tmpl_acpi_table_hdr);
  obj_acpi_table_hdr = alloca (acpi_table_hdr_length);
  memset (obj_acpi_table_hdr, 0, acpi_table_hdr_length);
  
  if (ipmi_get_physical_mem_data (table_address, 
				  acpi_table_hdr_length, 
				  obj_acpi_table_hdr) != 0)
    return (-1);
  
  fiid_obj_get_data (obj_acpi_table_hdr, tmpl_acpi_table_hdr, 
		     (uint8_t *)"signature", (uint8_t *)table_signature, table_signature_length);
  if (strcmp (table_signature, signature) != 0)
    return (-1);
  
  fiid_obj_get (obj_acpi_table_hdr, tmpl_acpi_table_hdr, 
		(uint8_t *)"length", &val);
  table_length = val;
  
  table = alloca (table_length);
  memset (table, 0, table_length);
  if (ipmi_get_physical_mem_data (table_address, 
				  table_length, 
				  table) != 0)
    return (-1);
  
  if (ipmi_acpi_table_chksum (table, table_length) != 0)
    return (-1);
  
  *acpi_table = malloc (table_length);
  memcpy (*acpi_table, table, table_length);
  *acpi_table_length = table_length;
  
  return 0;
}

int 
ipmi_acpi_get_firmware_table (char *signature, int table_instance, 
			      fiid_obj_t obj_acpi_table_hdr, 
			      uint8_t **sign_table_data, 
			      uint32_t *sign_table_data_length)
{
  uint64_t val;
  
  uint32_t acpi_table_hdr_length;
  
  fiid_obj_t obj_acpi_rsdp_descriptor;
  uint64_t rsdt_xsdt_address;
  char *rsdt_xsdt_signature;
  uint8_t revision;
  
  uint8_t *rsdt_xsdt_table;
  uint32_t rsdt_xsdt_table_length;
  uint8_t *rsdt_xsdt_table_data;
  uint32_t rsdt_xsdt_table_data_length;
  int acpi_table_count;
  uint8_t *acpi_table;
  uint32_t acpi_table_length;
  
  uint64_t table_address;
  int signature_table_count;
  int i;
  
  if (signature == NULL || obj_acpi_table_hdr == NULL || 
      sign_table_data == NULL || sign_table_data_length == NULL)
    return (-1);
  
  acpi_table_hdr_length = fiid_obj_len_bytes (tmpl_acpi_table_hdr);
  
  obj_acpi_rsdp_descriptor = alloca (fiid_obj_len_bytes (tmpl_acpi_rsdp_descriptor));
  memset (obj_acpi_rsdp_descriptor, 0, fiid_obj_len_bytes (tmpl_acpi_rsdp_descriptor));
  
  if (ipmi_acpi_get_rsdp (IPMI_ACPI_LO_RSDP_WINDOW_BASE,
			  IPMI_ACPI_LO_RSDP_WINDOW_SIZE,
			  obj_acpi_rsdp_descriptor) != 0)
    {
      if (ipmi_acpi_get_rsdp (IPMI_ACPI_HI_RSDP_WINDOW_BASE,
			      IPMI_ACPI_HI_RSDP_WINDOW_SIZE,
			      obj_acpi_rsdp_descriptor) != 0)
	return (-1);
    }
  
  fiid_obj_get (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor, 
		(uint8_t *)"revision", &val);
  revision = val;
  if (revision < 2)
    { 
      fiid_obj_get (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor, 
	 	    (uint8_t *)"rsdt_physical_address", &rsdt_xsdt_address);
      rsdt_xsdt_signature = strdupa (IPMI_ACPI_RSDT_SIG);
    }
  else 
    {
      fiid_obj_get (obj_acpi_rsdp_descriptor, tmpl_acpi_rsdp_descriptor, 
	 	    (uint8_t *)"xsdt_physical_address", &rsdt_xsdt_address);
      rsdt_xsdt_signature = strdupa (IPMI_ACPI_XSDT_SIG);
    }
  
  if (ipmi_acpi_get_table (rsdt_xsdt_address, rsdt_xsdt_signature, 
			   &rsdt_xsdt_table, 
			   &rsdt_xsdt_table_length) != 0)
    return (-1);
  
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
      if (revision < 2)
	{
	  fiid_template_t tmpl_table_address =
	    {
	      {32, "table_address"}, 
	      {0,  ""}
	    };
	  
	  fiid_obj_get ((rsdt_xsdt_table_data + (i * 4)), 
			tmpl_table_address, 
			(uint8_t *)"table_address", 
			&table_address);
	}
      else 
	{
	  fiid_template_t tmpl_table_address =
	    {
	      {64, "table_address"}, 
	      {0,  ""}
	    };
	  
	  fiid_obj_get ((rsdt_xsdt_table_data + (i * 8)), 
			tmpl_table_address, 
			(uint8_t *)"table_address", 
			&table_address);
	}
      
      if (ipmi_acpi_get_table (table_address, signature, 
			       &acpi_table, 
			       &acpi_table_length) != 0)
	continue;
      
      signature_table_count++;
      if (signature_table_count == table_instance)
	break;
      
      free (acpi_table);
      acpi_table = NULL;
      acpi_table_length = 0;
    }
  
  free (rsdt_xsdt_table);
  
  if (acpi_table == NULL)
    return (-1);
  
  memcpy (obj_acpi_table_hdr, acpi_table, acpi_table_hdr_length);
  *sign_table_data_length = acpi_table_length - acpi_table_hdr_length;
  *sign_table_data = malloc (*sign_table_data_length);
  memcpy (*sign_table_data, (acpi_table + acpi_table_hdr_length), *sign_table_data_length);
  
  free (acpi_table);
  
  return 0;
}

int 
ipmi_acpi_get_spmi_table (uint8_t interface_type,
			  fiid_obj_t obj_acpi_table_hdr,
			  fiid_obj_t obj_acpi_spmi_table_descriptor)
{
  uint64_t val;
  uint8_t table_interface_type;
  
  uint8_t *table_data = NULL;
  uint32_t table_data_length = 0;
  
  uint32_t copy_length;
  int instance;
  
  if (!obj_acpi_table_hdr
      || !obj_acpi_spmi_table_descriptor)
    {
      errno = EINVAL;
      return (-1);
    }

  for (instance = 0; instance <= IPMI_INTERFACE_MAX; instance++)
    {
      if (ipmi_acpi_get_firmware_table (IPMI_ACPI_SPMI_SIG, instance, 
					obj_acpi_table_hdr,
					&table_data, 
					&table_data_length) != 0)
	continue;
      
      printf ("__DEBUG__ instance = %d, signature = [%s] found\n", 
	      instance, IPMI_ACPI_SPMI_SIG);
      
      if (fiid_obj_len_bytes (tmpl_acpi_spmi_table_descriptor) < table_data_length)
	copy_length = fiid_obj_len_bytes (tmpl_acpi_spmi_table_descriptor);
      else 
	copy_length = table_data_length;
      
      if (copy_length != table_data_length)
	printf ("_DEBUG_ table_data_length=%d, template_length=%d,"
		" tmpl_acpi_spmi_table_descriptor length is too short\n", 
		table_data_length, fiid_obj_len_bytes (tmpl_acpi_spmi_table_descriptor));
      
      memcpy (obj_acpi_spmi_table_descriptor, table_data, copy_length);
      
      free (table_data);
      table_data = NULL;
      table_data_length = 0;
      
      fiid_obj_get (obj_acpi_spmi_table_descriptor, 
		    tmpl_acpi_spmi_table_descriptor, 
		    (uint8_t *)"interface_type", &val);
      table_interface_type = val;
      if (table_interface_type == interface_type)
	return (0);
    }
  
  return (-1);
}

ipmi_locate_info_t*
acpi_spmi_get_dev_info (ipmi_interface_type_t interface_type, 
			ipmi_locate_info_t* pinfo)
{
  fiid_obj_t obj_acpi_table_hdr = NULL;
  fiid_obj_t obj_acpi_spmi_table_descriptor = NULL;
  extern int errno;
  
  if (pinfo == NULL)
    {
      errno = EINVAL;
      return (NULL);
    }
  
  pinfo->locate_driver_type = IPMI_LOCATE_DRIVER_ACPI;

  if ((obj_acpi_table_hdr = fiid_obj_calloc (tmpl_acpi_table_hdr)) == NULL)
    return (NULL);

  if ((obj_acpi_spmi_table_descriptor = fiid_obj_calloc (tmpl_acpi_spmi_table_descriptor)) == NULL)
    {
      ipmi_xfree (obj_acpi_table_hdr);
      return (NULL);
    }

  if (ipmi_acpi_get_spmi_table (interface_type,
				obj_acpi_table_hdr,
				obj_acpi_spmi_table_descriptor) != 0)
    {
      ipmi_xfree (obj_acpi_table_hdr);
      ipmi_xfree (obj_acpi_spmi_table_descriptor);
/*       errno = ENODEV; */
      return (NULL);
    }
  
    /* I don't see any reason to perform this check now -- Anand Babu */
    /* This field must always be 01h to be compatible with any software
       that implements previous versions of this spec. */
    /*
    {
      uint64_t ipmi_legacy;
      fiid_obj_get (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor, (uint8_t *)"ipmi_legacy", &ipmi_legacy);
      if (ipmi_legacy != 1)
	{
	  errno = ENODEV;
	  return (NULL);
	}
    }
    */

  /* IPMI version */
  {
    uint64_t ipmi_ver_maj, ipmi_ver_min;
    fiid_obj_get (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor, (uint8_t *)"specification_revision.major", &ipmi_ver_maj);
    fiid_obj_get (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor, (uint8_t *)"specification_revision.minor", &ipmi_ver_min);
    pinfo->ipmi_ver_major = ipmi_ver_maj;
    pinfo->ipmi_ver_minor = ipmi_ver_min;
  }  
  /* Interface type - KCS, SMIC, SSIF, BT */
  {
    uint64_t interface_type;
    fiid_obj_get (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor, (uint8_t *)"interface_type", &interface_type);
    switch (interface_type)
      {
      case IPMI_INTERFACE_KCS:
      case IPMI_INTERFACE_SMIC:
      case IPMI_INTERFACE_BT:
      case IPMI_INTERFACE_SSIF:
	{
	  pinfo->interface_type = interface_type;
	  break;
	}
      case IPMI_INTERFACE_RESERVED:
      default:
	{
	  ipmi_xfree (obj_acpi_table_hdr);
	  ipmi_xfree (obj_acpi_spmi_table_descriptor);
	  errno = ENODEV;
	  return (NULL);
	}
      }
  }
  
  /* Address space id (memory mapped, IO mapped, SMBus) and IO base address */
  {
    uint64_t addr_space_id;
    uint64_t base_addr;

    fiid_obj_get (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor, (uint8_t *)"base_address.address_space_id", &addr_space_id);

    fiid_obj_get (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor, (uint8_t *)"base_address.address", &base_addr);

    switch (addr_space_id)
      {
      case IPMI_ACPI_ADDRESS_SPACE_ID_SYSTEM_MEMORY:
	{
	  pinfo->addr_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY;
	  pinfo->base_addr.bmc_iobase_addr = base_addr;
	  break;
	}
      case IPMI_ACPI_ADDRESS_SPACE_ID_SYSTEM_IO:
	{
	  pinfo->addr_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
	  pinfo->base_addr.bmc_membase_addr = base_addr;
	  break;
	}
      case IPMI_ACPI_ADDRESS_SPACE_ID_SMBUS:
	{
	  pinfo->addr_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
	  pinfo->base_addr.bmc_smbus_slave_addr = base_addr;
	  break;
	}
      default:
	{
	  ipmi_xfree (obj_acpi_table_hdr);
	  ipmi_xfree (obj_acpi_spmi_table_descriptor);
	  errno = ENODEV;
	  return (NULL);
	}
      }
  }
  
  /* Register spacing */
  {
    uint64_t reg_bit_width;
    fiid_obj_get (obj_acpi_spmi_table_descriptor, tmpl_acpi_spmi_table_descriptor, (uint8_t *)"base_address.register_bit_width", &reg_bit_width);
    pinfo->reg_space = (reg_bit_width / 8);
  }

  ipmi_xfree (obj_acpi_table_hdr);
  ipmi_xfree (obj_acpi_spmi_table_descriptor);
  return (pinfo);
}

