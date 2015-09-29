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

/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

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
#include <assert.h>
#include <errno.h>

#include "freeipmi/fiid/fiid.h"
#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"

#include "ipmi-locate-defs.h"
#include "ipmi-locate-trace.h"
#include "ipmi-locate-util.h"

#include "freeipmi-portability.h"

/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

#define IPMI_SMBIOS_ENTRY_CSUM_OFFSET   0x4
#define IPMI_SMBIOS_ENTRY_LEN_OFFSET    0x5
#define IPMI_SMBIOS_ENTRY_ANCHOR_OFFSET         0x10
#define IPMI_SMBIOS_ENTRY_ANCHOR_CSUM_OFFSET 0x15

#define IPMI_SMBIOS_IPMI_DEV_INFO_SIG           38
#define IPMI_SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET   0x4

#define IPMI_SMBIOS_AREA_START          0x000f0000
#define IPMI_SMBIOS_AREA_END            0x000fffff
#define IPMI_SMBIOS_AREA_LEN            ((IPMI_SMBIOS_AREA_END - IPMI_SMBIOS_AREA_START) + 1)
#define IPMI_SMBIOS_AREA_ALIGN          16
#define IPMI_SMBIOS_ENTRY_TLEN_OFFSET   0x16
#define IPMI_SMBIOS_ENTRY_PTR_OFFSET    0x18
#define IPMI_SMBIOS_DEV_INFO_LEN_OFFSET         0x1
#define IPMI_SMBIOS_IPMI_DEV_INFO_VER_OFFSET    0x5
#define IPMI_SMBIOS_IPMI_DEV_INFO_I2C_OFFSET    0x6
#define IPMI_SMBIOS_IPMI_DEV_INFO_NVSTOR_OFFSET         0x7
#define IPMI_SMBIOS_IPMI_DEV_INFO_ADDRESS_OFFSET        0x8
#define IPMI_SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET       0x10
#define IPMI_SMBIOS_LSB_BIT                             4
#define IPMI_SMBIOS_REGSPACING_SHIFT                    6
#define IPMI_SMBIOS_REGSPACING_MASK                     0x3
#define IPMI_SMBIOS_INTINFO_PRESENT_BIT                 3
#define IPMI_SMBIOS_INTINFO_POLARITY_BIT                1
#define IPMI_SMBIOS_INTINFO_TRIGGER_BIT                 0
#define IPMI_SMBIOS_DEV_INFO_INTNUM_OFFSET              0x11

#define IPMI_SMBIOS_REGISTER_SPACING_1BYTE_BOUND    0x00
#define IPMI_SMBIOS_REGISTER_SPACING_4BYTE_BOUND    0x01
#define IPMI_SMBIOS_REGISTER_SPACING_16BYTE_BOUND   0x02
#define IPMI_SMBIOS_REGISTER_SPACING_RESERVED       0x03

fiid_template_t tmpl_smbios_ipmi_device_info_record =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    /* IPMI Device Information structure indicator. value = 38
       (Note this number is given in decimal) */
    { 8, "type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Length of the structure, a minimum of 10h (for
       full IPMI address description, this is a minimum
       of 12h) */
    { 8, "length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "handle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Baseboard Management Controller (BMC)
       interface type, see Table C1-2, Interface Type
       field values. */
    { 8, "interface_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Somewhat mis-named. Actually identifies the
       IPMI Specification Version, in BCD format, to
       which the BMC was designed. Bits 7:4 hold the
       most significant digit of the version,
       while bits 3:0 hold the least significant bits,
       e.g. a value of 15h indicates version 1.5. */
    { 4, "ipmi_spec_revision.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "ipmi_spec_revision.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* The slave address on the I2C bus of this BMC. */
    { 8, "i2c_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bus id of the NV storage device. If no storage
       device exists for this BMC, the field is set to
       0FFh. */
    { 8, "nv_storage_device_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "base_address.io_mapped_or_mem_mapped", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Identifies the base address (either memory-
       mapped or I/O) of the BMC. If the least-
       significant bit of the field is a 1, the address is
       in I/O space; otherwise, the address is
       memory-mapped. Refer to the IPMI Interface
       Specification for usage details. */
    { 15, "base_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 0 - Interrupt Trigger Mode.
       1b = level, 0b = edge. */
    { 1, "interrupt_trigger_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 1 - Interrupt Polarity.
       1b = active high, 0b = active low. */
    { 1, "interrupt_polarity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 2 - reserved.
       Return as 0b. */
    { 1, "base_address_modifier_interrupt_info.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Interrupt Info
       Identifies the type and polarity of the
       interrupt associated with the IPMI system
       interface, if any.
       bit 3 - 1b = interrupt info specified
       0b = interrupt info not specified */
    { 1, "interrupt_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 4 - LS-bit for addresses
       0b = Address bit 0 = 0b
       1b = Address bit 0 = 1b */
    { 1, "ls_bit_for_addresses", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 5 - reserved.
       Return as 0b. */
    { 1, "base_address_modifier_interrupt_info.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* bit 7:6 - Register spacing
       00b = interface registers are on
       successive byte boundaries
       01b = interface registers are on 32-
       bit boundaries
       10b = interface registers are on 16-
       byte boundaries
       11b = reserved */
    { 2, "register_spacing", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Interrupt number for IPMI System Interface.
       00h = unspecified / unsupported */
    { 8, "interrupt_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

static int
_ipmi_smbios_register_spacing (uint8_t register_spacing_boundary, uint8_t *register_spacing)
{
  assert (register_spacing);
  assert (register_spacing_boundary == IPMI_SMBIOS_REGISTER_SPACING_1BYTE_BOUND
          || register_spacing_boundary == IPMI_SMBIOS_REGISTER_SPACING_4BYTE_BOUND
          || register_spacing_boundary == IPMI_SMBIOS_REGISTER_SPACING_16BYTE_BOUND
          || register_spacing_boundary == IPMI_SMBIOS_REGISTER_SPACING_RESERVED);

  switch (register_spacing_boundary)
    {
    case IPMI_SMBIOS_REGISTER_SPACING_1BYTE_BOUND:
      *register_spacing = 0x01;
      return (0);
    case IPMI_SMBIOS_REGISTER_SPACING_4BYTE_BOUND:
      *register_spacing = 0x04;
      return (0);
    case IPMI_SMBIOS_REGISTER_SPACING_16BYTE_BOUND:
      *register_spacing = 0x10;
      return (0);
    case IPMI_SMBIOS_REGISTER_SPACING_RESERVED:
    default:
      *register_spacing = 0;
      /* Should not reach */
      SET_ERRNO (EINVAL);
      return (-1);
    }
}

/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

/* _is_ipmi_entry
   ARGUMENTS:
   sigp = points to start of purported SMBIOS entry structure
   RETURNS:
   0 = not really a SMBIOS entry structure
   1 = yes, a real SMBIOS entry structure */
static int
_is_ipmi_entry (ipmi_locate_ctx_t ctx,
                uint8_t* sigp)
{
  static const char smbios_entry_sig[4] = { '_', 'S', 'M', '_' };
  static const char smbios_entry_anchor[5] = { '_', 'D', 'M', 'I', '_' };
  uint32_t csum_computed;
  uint8_t csum_given;
  uint8_t entry_len;
  uint8_t* bp;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (sigp);

  if (memcmp (sigp, smbios_entry_sig, sizeof (smbios_entry_sig)) != 0)
    return (0);

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


/* _is_ipmi_dev_info
   ARGUMENTS:
   type = which interface (KCS, SMIC, BT)
   dev_info_p = points to start of purported IPMI device info structure
   RETURNS:
   0 = not a IPMI device info structure for TYPE
   1 = yes, IPMI device info structure for TYPE */
static int
_is_ipmi_dev_info (ipmi_locate_ctx_t ctx,
                   ipmi_interface_type_t type,
                   uint8_t* dev_info_p)
{
  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (IPMI_INTERFACE_TYPE_VALID (type));
  assert (dev_info_p);

  if (*dev_info_p != IPMI_SMBIOS_IPMI_DEV_INFO_SIG)
    return (0);

  if (dev_info_p[IPMI_SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET] != type)
    return (0);

  return (1);
}

/* _map_physmem
   ARGUMENTS:
   physaddress = physical address to access
   len = length of area to access
   startp = place to store pointer to unmap (caller responsible for unmapping)
   totallen = length of area to unmap
   RETURNS:
   pointer to area of physical memory at physmem */
static uint8_t*
_map_physmem (ipmi_locate_ctx_t ctx,
              uint32_t physaddress,
              size_t len,
              void** startp,
              size_t* totallen)
{
  uint32_t startaddress;
  uint32_t pad;
  int mem_fd = -1;
  uint8_t *rv = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (startp);
  assert (totallen);

  if ((mem_fd = open ("/dev/mem", O_RDONLY|O_SYNC)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  pad = physaddress % sysconf (_SC_PAGESIZE);
  startaddress = physaddress - pad;
  *totallen = len + pad;

  if ((*startp = mmap (NULL,
                       *totallen,
                       PROT_READ,
                       MAP_PRIVATE,
                       mem_fd,
                       startaddress)) == MAP_FAILED)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  rv = (uint8_t*)(*startp) + pad;

 cleanup:
  /* ignore potential error, cleanup path */
  close (mem_fd);
  return (rv);
}

/* _copy_ipmi_dev_info
   ARGUMENTS:
   type = which interface (KCS, SMIC, BT)
   RETURNS:
   pointer to the device info structure in heap (caller responsible
   for freeing */
static uint8_t*
_copy_ipmi_dev_info (ipmi_locate_ctx_t ctx,
                     ipmi_interface_type_t type)
{
  uint8_t* result = NULL;
  void* map_entry = NULL;
  size_t map_entry_len;
  uint8_t* pmem_entry;
  uint8_t* rv = NULL;
  uint8_t* sigp;
  int flag;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (IPMI_INTERFACE_TYPE_VALID (type));

  if (!(pmem_entry = _map_physmem (ctx,
                                   IPMI_SMBIOS_AREA_START,
                                   IPMI_SMBIOS_AREA_LEN,
                                   &map_entry,
                                   &map_entry_len)))
    goto cleanup;

  for (sigp = pmem_entry; sigp - pmem_entry < IPMI_SMBIOS_AREA_LEN; sigp += IPMI_SMBIOS_AREA_ALIGN)
    {
      if ((flag = _is_ipmi_entry (ctx, sigp)) < 0)
        goto cleanup;

      if (flag)
        {
          uint16_t s_table_len;
          uint8_t* pmem_table;
          void* map_table;
          size_t map_table_len;
          uint8_t* dev_info_p;
          size_t size;
          uint8_t* var_info_p;

          s_table_len = *(uint16_t*)(sigp + IPMI_SMBIOS_ENTRY_TLEN_OFFSET);
          if (!(pmem_table = _map_physmem (ctx,
                                           *(uint32_t*)(sigp + IPMI_SMBIOS_ENTRY_PTR_OFFSET),
                                           s_table_len,
                                           &map_table,
                                           &map_table_len)))
            goto cleanup;

          dev_info_p = pmem_table;
          size = dev_info_p[IPMI_SMBIOS_DEV_INFO_LEN_OFFSET];
          while (dev_info_p - pmem_table < s_table_len)
            {
              if ((flag = _is_ipmi_dev_info (ctx,
                                             type,
                                             dev_info_p)) < 0)
                goto cleanup;

              if (flag)
                {
                  if (!(result = malloc (size)))
                    {
                      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_OUT_OF_MEMORY);
                      goto cleanup;
                    }
                  memcpy (result, dev_info_p, size);
                  rv = result;
                  break;
                }
              var_info_p = dev_info_p + size;
              while (var_info_p[0] != 0 || var_info_p[1] != 0)
                var_info_p++;
              dev_info_p = var_info_p + 2;
              size = dev_info_p[IPMI_SMBIOS_DEV_INFO_LEN_OFFSET];
            }
          /* ignore potential error, just return result */
          munmap (map_table, map_table_len);
        }

      if (rv)
        break;
    }

  LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
 cleanup:
  /* ignore potential error, just return result */
  if (map_entry)
    munmap (map_entry, map_entry_len);
  return (rv);
}

/* ipmi_locate_smbios_get_device_info
   ARGUMENTS:
   ctx = ipmi locate context
   type = which interface (KCS, SMIC, BT)
   pinfo = pointer to information structure filled in by this function
   RETURNS:
   0 on success, -1 on error */
int
ipmi_locate_smbios_get_device_info (ipmi_locate_ctx_t ctx,
                                    ipmi_interface_type_t type,
                                    struct ipmi_locate_info *info)
{
  uint8_t* bufp = NULL;
  uint8_t version;
  uint64_t address;
  uint64_t strobed;
  struct ipmi_locate_info linfo;

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

  if (!(bufp = _copy_ipmi_dev_info (ctx, type)))
    goto cleanup;

  linfo.locate_driver_type = IPMI_LOCATE_DRIVER_SMBIOS;

  version = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_VER_OFFSET];
  linfo.ipmi_version_major = (version >> 4) & 0xf;
  linfo.ipmi_version_minor = version & 0xf;

  linfo.interface_type = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET];
  if (linfo.interface_type != type)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  strobed = address = *(uint64_t*)(bufp+IPMI_SMBIOS_IPMI_DEV_INFO_ADDRESS_OFFSET);
  if (bufp[IPMI_SMBIOS_DEV_INFO_LEN_OFFSET] >= IPMI_SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET)
    {
      uint8_t modifier;
      uint8_t lsb;
      int register_spacing_boundary;

      modifier = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET];
      lsb = (modifier >> IPMI_SMBIOS_LSB_BIT) & 1;
      strobed = (strobed & ~1) | lsb;

      register_spacing_boundary = (modifier >> IPMI_SMBIOS_REGSPACING_SHIFT) & IPMI_SMBIOS_REGSPACING_MASK;
      if (_ipmi_smbios_register_spacing (register_spacing_boundary,
                                         &linfo.register_spacing) < 0)
        goto cleanup;
    }

  if (linfo.interface_type == IPMI_INTERFACE_SSIF)
    {
      linfo.address_space_id  = IPMI_ADDRESS_SPACE_ID_SMBUS;
      linfo.driver_address = bufp[IPMI_SMBIOS_IPMI_DEV_INFO_I2C_OFFSET];
    }
  else
    {
      if ((address & 1) != 0)
        {
          linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
          linfo.driver_address = strobed;
        }
      else
        {
          linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY;
          linfo.driver_address = strobed;
        }
    }

  free (bufp);
  memcpy (info, &linfo, sizeof (struct ipmi_locate_info));
  return (0);

 cleanup:
  free (bufp);
  return (-1);
#endif
}
