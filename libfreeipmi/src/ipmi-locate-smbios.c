/* 
   ipmi-locate-smbios.c - SMBIOS driver to locate IPMI interfaces.

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

/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <errno.h>

#include "freeipmi/ipmi-locate.h"
#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-ssif-api.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"
#include "xmalloc.h"

/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

#define IPMI_SMBIOS_ENTRY_CSUM_OFFSET 	0x4
#define IPMI_SMBIOS_ENTRY_LEN_OFFSET 	0x5
#define IPMI_SMBIOS_ENTRY_ANCHOR_OFFSET 	0x10
#define IPMI_SMBIOS_ENTRY_ANCHOR_CSUM_OFFSET 0x15

#define IPMI_SMBIOS_IPMI_DEV_INFO_SIG 		38
#define IPMI_SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET 	0x4

#define IPMI_SMBIOS_AREA_START 		0x000f0000
#define IPMI_SMBIOS_AREA_END 		0x000fffff
#define IPMI_SMBIOS_AREA_LEN 		((IPMI_SMBIOS_AREA_END - IPMI_SMBIOS_AREA_START) + 1)
#define IPMI_SMBIOS_AREA_ALIGN 		16
#define IPMI_SMBIOS_ENTRY_TLEN_OFFSET 	0x16
#define IPMI_SMBIOS_ENTRY_PTR_OFFSET 	0x18
#define IPMI_SMBIOS_DEV_INFO_LEN_OFFSET 	0x1
#define IPMI_SMBIOS_IPMI_DEV_INFO_VER_OFFSET 	0x5
#define IPMI_SMBIOS_IPMI_DEV_INFO_I2C_OFFSET 	0x6
#define IPMI_SMBIOS_IPMI_DEV_INFO_NVSTOR_OFFSET 	0x7
#define IPMI_SMBIOS_IPMI_DEV_INFO_ADDRESS_OFFSET 	0x8
#define IPMI_SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET 	0x10
#define IPMI_SMBIOS_LSB_BIT 				4
#define IPMI_SMBIOS_REGSPACING_SHIFT 		        6
#define IPMI_SMBIOS_REGSPACING_MASK 			0x3
#define IPMI_SMBIOS_INTINFO_PRESENT_BIT 		3
#define IPMI_SMBIOS_INTINFO_POLARITY_BIT 		1
#define IPMI_SMBIOS_INTINFO_TRIGGER_BIT 		0
#define IPMI_SMBIOS_DEV_INFO_INTNUM_OFFSET 		0x11

#define IPMI_SMBIOS_REG_SPACE_1BYTE_BOUND    0x00
#define IPMI_SMBIOS_REG_SPACE_4BYTE_BOUND    0x01
#define IPMI_SMBIOS_REG_SPACE_16BYTE_BOUND   0x02
#define IPMI_SMBIOS_REG_SPACE_RESERVED       0x03

fiid_template_t tmpl_smbios_ipmi_device_info_record =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* IPMI Device Information structure indicator. value = 38
       (Note this number is given in decimal) */
    {8, "type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* Length of the structure, a minimum of 10h (for
       full IPMI address description, this is a minimum
       of 12h) */
    {8, "length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "handle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* Baseboard Management Controller (BMC)
       interface type, see Table C1-2, Interface Type
       field values. */
    {8, "interface_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* Somewhat mis-named. Actually identifies the
       IPMI Specification Version, in BCD format, to
       which the BMC was designed. Bits 7:4 hold the
       most significant digit of the version, 
       while bits 3:0 hold the least significant bits,
       e.g. a value of 15h indicates version 1.5. */
    {4, "ipmi_spec_revision.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "ipmi_spec_revision.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The slave address on the I2C bus of this BMC. */
    {8, "i2c_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* Bus id of the NV storage device. If no storage
       device exists for this BMC, the field is set to
       0FFh. */
    {8, "nv_storage_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "base_address.io_mapped_or_mem_mapped", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* Identifies the base address (either memory-
       mapped or I/O) of the BMC. If the least-
       significant bit of the field is a 1, the address is
       in I/O space; otherwise, the address is
       memory-mapped. Refer to the IPMI Interface
       Specification for usage details. */
    {15, "base_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 0 - Interrupt Trigger Mode.
       1b = level, 0b = edge. */
    {1, "interrupt_trigger_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 1 - Interrupt Polarity.
       1b = active high, 0b = active low. */
    {1, "interrupt_polarity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* bit 2 - reserved. 
       Return as 0b. */
    {1, "base_address_modifier_interrupt_info.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* Interrupt Info
       Identifies the type and polarity of the
       interrupt associated with the IPMI system
       interface, if any.
       bit 3 - 1b = interrupt info specified
       0b = interrupt info not specified */
    {1, "interrupt_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* bit 4 - LS-bit for addresses
       0b = Address bit 0 = 0b
       1b = Address bit 0 = 1b */
    {1, "ls_bit_for_addresses", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 5 - reserved. 
       Return as 0b. */
    {1, "base_address_modifier_interrupt_info.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* bit 7:6 - Register spacing
       00b = interface registers are on
       successive byte boundaries
       01b = interface registers are on 32-
       bit boundaries
       10b = interface registers are on 16-
       byte boundaries
       11b = reserved */
    {2, "register_spacing", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* Interrupt number for IPMI System Interface.
       00h = unspecified / unsupported */
    {8, "interrupt_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

static int
ipmi_smbios_reg_space (uint8_t reg_space_boundary, uint8_t *reg_space)
{
  ERR_EINVAL (reg_space 
	      && (reg_space_boundary == IPMI_SMBIOS_REG_SPACE_1BYTE_BOUND
		  || reg_space_boundary == IPMI_SMBIOS_REG_SPACE_4BYTE_BOUND
		  || reg_space_boundary == IPMI_SMBIOS_REG_SPACE_16BYTE_BOUND
		  || reg_space_boundary == IPMI_SMBIOS_REG_SPACE_RESERVED));

  switch (reg_space_boundary)
    {
    case IPMI_SMBIOS_REG_SPACE_1BYTE_BOUND:
      *reg_space = 0x01;
      return (0);
    case IPMI_SMBIOS_REG_SPACE_4BYTE_BOUND:
      *reg_space = 0x04;
      return (0);
    case IPMI_SMBIOS_REG_SPACE_16BYTE_BOUND:
      *reg_space = 0x10;
      return (0);
    case IPMI_SMBIOS_REG_SPACE_RESERVED:
    default:
      *reg_space = 0;
      ERR_EINVAL(0);
    }
}


/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

/* is_ipmi_entry
   ARGUMENTS:
   sigp = points to start of purported SMBIOS entry structure
   RETURNS:
   0 = not really a SMBIOS entry structure
   1 = yes, a real SMBIOS entry structure */ 
static int
is_ipmi_entry (uint8_t* sigp)
{
  static const char smbios_entry_sig[4] = { '_', 'S', 'M', '_' };
  static const char smbios_entry_anchor[5] = { '_', 'D', 'M', 'I', '_' };
  uint32_t csum_computed;
  uint8_t csum_given;
  uint8_t entry_len;
  uint8_t* bp;

  if (memcmp (sigp, smbios_entry_sig, sizeof (smbios_entry_sig)) != 0)
    return 0;

  entry_len = sigp[IPMI_SMBIOS_ENTRY_LEN_OFFSET];

  csum_given = sigp[IPMI_SMBIOS_ENTRY_CSUM_OFFSET];
  csum_computed = 0;
  for (bp = sigp; bp < sigp + entry_len; bp++)
    csum_computed = (csum_computed + (*bp)) % (1 << CHAR_BIT);

  if (memcmp (sigp + IPMI_SMBIOS_ENTRY_ANCHOR_OFFSET, smbios_entry_anchor,
	      sizeof (smbios_entry_anchor)) != 0)
    return (0);

  csum_given = sigp[IPMI_SMBIOS_ENTRY_ANCHOR_CSUM_OFFSET];
  csum_computed = 0;
  for (bp = sigp + IPMI_SMBIOS_ENTRY_ANCHOR_CSUM_OFFSET; bp < sigp + entry_len; bp++)
    csum_computed = (csum_computed + (*bp)) % (1 << CHAR_BIT);
    
  return (1);
}


/* is_ipmi_dev_info
   ARGUMENTS:
   type = which interface (KCS, SMIC, BT)
   dev_info_p = points to start of purported IPMI device info structure
   RETURNS:
   0 = not a IPMI device info structure for TYPE
   1 = yes, IPMI device info structure for TYPE */
static int
is_ipmi_dev_info (ipmi_interface_type_t type, uint8_t* dev_info_p)
{
  if (*dev_info_p != IPMI_SMBIOS_IPMI_DEV_INFO_SIG)
    return 0;

  if (dev_info_p[IPMI_SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET] != type)
    return 0;

  return 1;
}

/* map_physmem
   ARGUMENTS:
   physaddress = physical address to access
   len = length of area to access
   startp = place to store pointer to unmap (caller responsible for unmapping)
   totallen = length of area to unmap
   RETURNS:
   pointer to area of physical memory at physmem */
static uint8_t*
map_physmem (uint32_t physaddress, size_t len, void** startp, size_t* totallen)
{
  uint32_t startaddress;
  uint32_t pad;
  int mem_fd;

  if (startp == NULL || totallen == NULL)
    return NULL;

  mem_fd = open ("/dev/mem", O_RDONLY|O_SYNC);

  if (mem_fd != -1)
    {
      pad = physaddress % sysconf (_SC_PAGESIZE);
      startaddress = physaddress - pad;
      *totallen = len + pad;
      *startp = mmap (NULL, *totallen, PROT_READ, MAP_PRIVATE, mem_fd, startaddress);
      close (mem_fd);

      if (*startp != MAP_FAILED)
	return ((uint8_t*)(*startp) + pad);
    }
  return NULL;	  
}

/* copy_impi_dev_info
   ARGUMENTS:
   type = which interface (KCS, SMIC, BT)
   RETURNS:
   pointer to the device info structure in heap (caller responsible
   for freeing */
static uint8_t*
copy_impi_dev_info (ipmi_interface_type_t type)
{
  int status;
  uint8_t* result = NULL;
  void* map_entry;
  size_t map_entry_len;
  uint8_t* pmem_entry; 

  status = 1;
  pmem_entry = map_physmem (IPMI_SMBIOS_AREA_START, IPMI_SMBIOS_AREA_LEN, &map_entry, &map_entry_len);
  if (pmem_entry != NULL)
    {
      uint8_t* sigp;
      for (sigp = pmem_entry; sigp - pmem_entry < IPMI_SMBIOS_AREA_LEN; sigp += IPMI_SMBIOS_AREA_ALIGN)
	{
	  if (is_ipmi_entry (sigp))
	    {
	      uint16_t s_table_len;
	      uint8_t* pmem_table;
	      void* map_table;
	      size_t map_table_len;

	      s_table_len = *(uint16_t*)(sigp + IPMI_SMBIOS_ENTRY_TLEN_OFFSET);
	      pmem_table = map_physmem (*(uint32_t*)(sigp + IPMI_SMBIOS_ENTRY_PTR_OFFSET), s_table_len,
					&map_table, &map_table_len);
	      if (pmem_table != NULL)
		{
		  uint8_t* dev_info_p;
		  size_t size;
		  uint8_t* var_info_p;

		  dev_info_p = pmem_table;
		  size = dev_info_p[IPMI_SMBIOS_DEV_INFO_LEN_OFFSET];
		  while (dev_info_p - pmem_table < s_table_len)
		    {
		      if (is_ipmi_dev_info (type, dev_info_p))
			{
			  ERR_NULL_RETURN ((result = xmalloc (size)));
			  if (result != NULL)
			    {
			      status = 0;
			      memcpy (result, dev_info_p, size);
			    }
			  break;
			}
		      var_info_p = dev_info_p + size;
		      while (var_info_p[0] != 0 || var_info_p[1] != 0)
			var_info_p++;
		      dev_info_p = var_info_p + 2;
		      size = dev_info_p[IPMI_SMBIOS_DEV_INFO_LEN_OFFSET];
		    }
		  munmap (map_table, map_table_len);
		}
	    }
	  if (status <= 0)
	    break;
	}
      munmap (map_entry, map_entry_len);
    }
  return (status == 0 ? result : NULL);
}

/* get_smbios_dev_info
   ARGUMENTS:
   type = which interface (KCS, SMIC, BT)
   pinfo = pointer to information structure filled in by this function
   RETURNS:
   pinfo if successful, NULL otherwise */
ipmi_locate_info_t*
ipmi_locate_smbios_get_dev_info (ipmi_interface_type_t type)
{
  uint8_t* bufp = NULL;
  uint8_t version;
  uint64_t address;
  uint64_t strobed;
  ipmi_locate_info_t *pinfo = NULL;

  ERR_EINVAL_NULL_RETURN (IPMI_INTERFACE_TYPE_VALID(type));

  ERR_CLEANUP ((pinfo = (ipmi_locate_info_t *)malloc(sizeof(struct ipmi_locate_info))));
  memset(pinfo, '\0', sizeof(struct ipmi_locate_info));
  pinfo->interface_type = type;
  if (type == IPMI_INTERFACE_SSIF)
    pinfo->bmc_i2c_dev_name = strdup (IPMI_DEFAULT_I2C_DEVICE);

  ERR_CLEANUP ((bufp = copy_impi_dev_info (type)));

  pinfo->locate_driver_type = IPMI_LOCATE_DRIVER_SMBIOS;

  version = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_VER_OFFSET];
  pinfo->ipmi_ver_major = (version >> 4) & 0xf;
  pinfo->ipmi_ver_minor = version & 0xf;

  pinfo->interface_type = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET];
  ERR_ENODEV_CLEANUP (pinfo->interface_type == type);

  strobed = address = *(uint64_t*)(bufp+IPMI_SMBIOS_IPMI_DEV_INFO_ADDRESS_OFFSET);
  if (bufp[IPMI_SMBIOS_DEV_INFO_LEN_OFFSET] >= IPMI_SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET)
    {
      uint8_t modifier;
      uint8_t lsb;
      int reg_space_boundary;

      modifier = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET];
      lsb = (modifier >> IPMI_SMBIOS_LSB_BIT) & 1;
      strobed = (strobed & ~1) | lsb;

      reg_space_boundary = (modifier >> IPMI_SMBIOS_REGSPACING_SHIFT) & IPMI_SMBIOS_REGSPACING_MASK;
      ERR_CLEANUP (!(ipmi_smbios_reg_space (reg_space_boundary, &pinfo->reg_space) == -1));
    }

  if (pinfo->interface_type == IPMI_INTERFACE_SSIF)
    {
      pinfo->address_space_id  = IPMI_ADDRESS_SPACE_ID_SMBUS;
      pinfo->base_address.bmc_smbus_slave_address = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_I2C_OFFSET];
    }
  else
    {
      if ((address & 1) != 0)
	{
	  pinfo->address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
	  pinfo->base_address.bmc_iobase_address = strobed;
	}
      else
	{
	  pinfo->address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY;
	  pinfo->base_address.bmc_membase_address = strobed;
	}
    }

  free (bufp);
  return (pinfo);

 cleanup:
  if (bufp)
    free(bufp);
  ipmi_locate_destroy(pinfo);
  return (NULL);
}
