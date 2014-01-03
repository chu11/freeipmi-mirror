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

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"

#include "ipmi-locate-defs.h"
#include "ipmi-locate-trace.h"
#include "ipmi-locate-util.h"

#include "freeipmi-portability.h"

#ifdef UNTESTED /* __linux */           /* this code uses the /proc filesystem */

#define PCI_CLASS_REVISION      0x08    /* High 24 bits are class, low 8 revision */
#define PCI_REVISION_ID         0x08    /* Revision ID */
#define PCI_CLASS_PROG          0x09    /* Reg. Level Programming Interface */
#define PCI_CLASS_DEVICE        0x0a    /* Device class */

#define  PCI_BASE_ADDRESS_SPACE 0x01    /* 0 = memory, 1 = I/O */
#define  PCI_BASE_ADDRESS_SPACE_IO 0x01
#define  PCI_BASE_ADDRESS_SPACE_MEMORY 0x00
#define  PCI_BASE_ADDRESS_MEM_TYPE_MASK 0x06
#define  PCI_BASE_ADDRESS_MEM_TYPE_32   0x00    /* 32 bit address */
#define  PCI_BASE_ADDRESS_MEM_TYPE_1M   0x02    /* Below 1M [obsolete] */
#define  PCI_BASE_ADDRESS_MEM_TYPE_64   0x04    /* 64 bit address */
#define  PCI_BASE_ADDRESS_MEM_PREFETCH  0x08    /* prefetchable? */
#define  PCI_BASE_ADDRESS_MEM_MASK      (~0x0fUL)
#define  PCI_BASE_ADDRESS_IO_MASK       (~0x03UL)

#define IPMI_CLASS 0xc
#define IPMI_SUBCLASS 0x7

#define PCI_SLOT(devfn)         (((devfn) >> 3) & 0x1f)
#define PCI_FUNC(devfn)         ((devfn) & 0x07)

enum pci_address_space_type
  {
    past_io = PCI_BASE_ADDRESS_SPACE_IO,
    past_memory = PCI_BASE_ADDRESS_SPACE_MEMORY,
  };

typedef enum pci_address_space_type pci_address_space_type_t;

enum pci_address_mem_type
  {
    pamt_32 = PCI_BASE_ADDRESS_MEM_TYPE_32,
    pamt_1M = PCI_BASE_ADDRESS_MEM_TYPE_1M,
    pamt_64 = PCI_BASE_ADDRESS_MEM_TYPE_64,
  };

typedef enum pci_address_mem_type pci_address_mem_type_t;

struct pci_class_regs
{
  uint8_t pci_class;
  uint8_t pci_subclass;
  uint8_t pci_prog_interface;
  uint8_t pci_rev;
};

typedef struct pci_class_regs pci_class_regs_t;

/* pci_get_regs - read the file under /proc that is the image of a device's PCI registers */
/* bus = bus number from devices file */
/* dev = device number from devices file */
/* func = function number from devices file */
/* pregs = pointer to structure where to store the important registers */
/* return : pregs if successful, otherwise NULL */

static int
_pci_get_regs (ipmi_locate_ctx_t ctx,
               uint8_t bus,
               uint8_t dev,
               uint16_t func,
               pci_class_regs_t* pregs)
{
  FILE* fp = NULL;
  size_t n;
  char fname[128];
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_LOCATE_CTX_MAGIC);
  assert (pregs);

  snprintf (fname, sizeof (fname), "/proc/bus/pci/%02x/%02x.%d", bus, dev, func);

  if (!(fp = fopen (fname, "r")))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fseek (fp, PCI_CLASS_REVISION, SEEK_SET) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((n = fread (&(pregs->pci_rev), 1, 1, fp)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (n != 1)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  if ((n = fread (&(pregs->pci_prog_interface), 1, 1, fp)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (n != 1)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  if ((n = fread (&(pregs->pci_subclass), 1, 1, fp)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (n != 1)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  if ((n = fread (&(pregs->pci_class), 1, 1, fp)) < 0)
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (n != 1)
    {
      LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fclose (fp);
  return (rv);
}

#if (__WORDSIZE == 32)
#define FORMAT_X64 "%Lx"
#elif (__WORDSIZE == 64)
#define FORMAT_X64 "%lx"
#endif

/* pci_get_dev_info - probe PCI for IPMI interrupt number and register base */
/* type = which interface (KCS, SMIC, BT) */
/* pinfo = pointer to information structure filled in by this function */

int
ipmi_locate_pci_get_device_info (ipmi_locate_ctx_t ctx,
                                 ipmi_interface_type_t type,
                                 struct ipmi_locate_info *info)
{
  unsigned dfn;
  unsigned vendor;
  unsigned bus;
  unsigned dev;
  unsigned func;
  unsigned irq;
  uint64_t base_address[6];
  char buf[512];
  FILE* fp_devices;
  int items;
  unsigned int i;
  struct ipmi_locate_info linfo;
  int rv = -1;

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

  if (!(fp_devices = fopen ("/proc/bus/pci/devices", "r")))
    {
      LOCATE_ERRNO_TO_LOCATE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  while (fgets (buf, sizeof (buf), fp_devices) != NULL)
    {
      pci_class_regs_t regs;
      
      items = sscanf (buf, "%x %x %x " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64,
                      &dfn, &vendor, &irq,
                      &base_address[0], &base_address[1], &base_address[2], &base_address[3], &base_address[4], &base_address[5]);
      linfo.intr_num = (uint16_t)irq;
      
      if (items != 9)
        {
          LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
          goto cleanup;
        }
      bus = dfn >> 8U;
      dev = PCI_SLOT (dfn & 0xff);
      func = PCI_FUNC (dfn & 0xff);
      
      if (_pci_get_regs (ctx, bus, dev, func, &regs) < 0)
        goto cleanup;
      
      if (regs.pci_class != IPMI_CLASS ||
          regs.pci_subclass != IPMI_SUBCLASS ||
          regs.pci_prog_interface + 1 != type)
        continue;
      
      for (i = 0; i < 6; i++)
        {
          if (base_address[i] == 0 || base_address[i] == ~0)
            continue;
          
          switch (base_address[i] & PCI_BASE_ADDRESS_SPACE)
            {
            case past_io:
              linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY;
              linfo.driver_address = base_address[i] & ~PCI_BASE_ADDRESS_IO_MASK;
              memcpy (info, &linfo, sizeof (struct ipmi_locate_info));
              rv = 0;
              goto cleanup;
              
            case past_memory:
              linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
              linfo.driver_address = base_address[i] & ~PCI_BASE_ADDRESS_MEM_MASK;
              memcpy (info, &linfo, sizeof (struct ipmi_locate_info));
              rv = 0;
              goto cleanup;
            }
      }
  }

 cleanup:
  if (fp_devices)
    fclose (fp_devices);
  return (rv);
}

#else  /* __linux */

int
ipmi_locate_pci_get_device_info (ipmi_locate_ctx_t ctx,
                                 ipmi_interface_type_t type,
                                 struct ipmi_locate_info *info)
{
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

  LOCATE_SET_ERRNUM (ctx, IPMI_LOCATE_ERR_SYSTEM_ERROR);
  return (-1);
}

#endif /* !__linux */
