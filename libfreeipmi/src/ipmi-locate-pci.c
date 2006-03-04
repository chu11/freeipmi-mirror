/* 
   ipmi-locate-pci.c - Locate IPMI interfaces by scanning PCI bus information

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

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-locate.h"
#include "freeipmi/ipmi-ssif-interface.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"

#ifdef UNTESTED /* __linux */           /* this code uses the /proc filesystem */

#define PCI_CLASS_REVISION	0x08	/* High 24 bits are class, low 8 revision */
#define PCI_REVISION_ID         0x08    /* Revision ID */
#define PCI_CLASS_PROG          0x09    /* Reg. Level Programming Interface */
#define PCI_CLASS_DEVICE        0x0a    /* Device class */

#define  PCI_BASE_ADDRESS_SPACE	0x01	/* 0 = memory, 1 = I/O */
#define  PCI_BASE_ADDRESS_SPACE_IO 0x01
#define  PCI_BASE_ADDRESS_SPACE_MEMORY 0x00
#define  PCI_BASE_ADDRESS_MEM_TYPE_MASK 0x06
#define  PCI_BASE_ADDRESS_MEM_TYPE_32	0x00	/* 32 bit address */
#define  PCI_BASE_ADDRESS_MEM_TYPE_1M	0x02	/* Below 1M [obsolete] */
#define  PCI_BASE_ADDRESS_MEM_TYPE_64	0x04	/* 64 bit address */
#define  PCI_BASE_ADDRESS_MEM_PREFETCH	0x08	/* prefetchable? */
#define  PCI_BASE_ADDRESS_MEM_MASK	(~0x0fUL)
#define  PCI_BASE_ADDRESS_IO_MASK	(~0x03UL)

#define IPMI_CLASS 0xc
#define IPMI_SUBCLASS 0x7

#define PCI_SLOT(devfn)		(((devfn) >> 3) & 0x1f)
#define PCI_FUNC(devfn)		((devfn) & 0x07)

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

static pci_class_regs_t*
pci_get_regs (uint8_t bus, uint8_t dev, uint16_t func, pci_class_regs_t* pregs)
{
  FILE* fp;
  char fname[128];

  snprintf (fname, sizeof(fname), "/proc/bus/pci/%02x/%02x.%d", bus, dev, func);
  fp = fopen (fname, "r");
  if (fp == NULL)
    {
      return NULL;
    }
  fseek (fp, PCI_CLASS_REVISION, SEEK_SET);
  fread (&pregs->pci_rev, 1, 1, fp);
  fread (&pregs->pci_prog_interface, 1, 1, fp);
  fread (&pregs->pci_subclass, 1, 1, fp);
  fread (&pregs->pci_class, 1, 1, fp);
  fclose (fp);
  return pregs;
}

#if (__WORDSIZE == 32)
#define FORMAT_X64 "%Lx"
#elif (__WORDSIZE == 64)
#define FORMAT_X64 "%lx"
#endif

/* pci_get_dev_info - probe PCI for IPMI interrupt number and register base */
/* type = which interface (KCS, SMIC, BT) */
/* pinfo = pointer to information structure filled in by this function */

ipmi_locate_info_t*
ipmi_locate_pci_get_dev_info (ipmi_interface_t type)
{
  unsigned dfn;
  unsigned vendor;
  unsigned bus;
  unsigned dev;
  unsigned func;
  unsigned irq;
  uint64_t base_addr[6];
  char buf[512];
  FILE* fp_devices;
  int items;
  int i;
  int status;
  ipmi_locate_info_t *pinfo = NULL;

  ERR_EINVAL_NULL_RETURN (IPMI_INTERFACE_TYPE_VALID(type));

  if (!(pinfo = (ipmi_locate_info_t *)malloc(sizeof(struct ipmi_locate_info))))
    goto cleanup;
  memset(pinfo, '\0', sizeof(struct ipmi_locate_info));
  pinfo->interface_type = type;
  if (type == IPMI_INTERFACE_SSIF)
    pinfo->bmc_i2c_dev_name = strdup (IPMI_DEFAULT_I2C_DEVICE);

  status = 1;
  fp_devices = fopen ("/proc/bus/pci/devices", "r");
  if (fp_devices == NULL) 
    goto cleanup;

  while (fgets (buf, sizeof(buf), fp_devices) != NULL) {
    pci_class_regs_t regs;

    items = sscanf (buf, "%x %x %x " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64 " " FORMAT_X64,
		    &dfn, &vendor, &irq,
		    &base_addr[0], &base_addr[1], &base_addr[2], &base_addr[3], &base_addr[4], &base_addr[5]);
    pinfo->intr_num = (uint16_t)irq;
    if (items == 9)
      {
	bus = dfn >> 8U;
	dev = PCI_SLOT(dfn & 0xff);
	func = PCI_FUNC(dfn & 0xff);
	if (pci_get_regs (bus, dev, func, &regs, &status) != NULL)
	  {
	    if (regs.pci_class != IPMI_CLASS ||
		regs.pci_subclass != IPMI_SUBCLASS ||
		regs.pci_prog_interface + 1 != type)
	      continue;

	    for (i = 0; i < 6; i++)
	      {
		if (base_addr[i] == 0 || base_addr[i] == ~0) continue;
		switch (base_addr[i] & PCI_BASE_ADDRESS_SPACE)
		  {
		  case past_io:
		    pinfo->bmc_io_mapped = 0;
		    pinfo->base.bmc_iobase_addr = base_addr[i] & ~PCI_BASE_ADDRESS_IO_MASK;
		    return pinfo;

		  case past_memory:
		    pinfo->bmc_io_mapped = 1;
		    pinfo->base.bmc_membase_addr = base_addr[i] & ~PCI_BASE_ADDRESS_MEM_MASK;
		    return pinfo;
		  }
	      }
	  }
	else
	  goto cleanup;
      }
    else
      goto cleanup;
  }

 cleanup:
  if (fp_devices != NULL)
    fclose (fp_devices);
  ipmi_locate_destroy(pinfo);
  return NULL;
}

#else  /* __linux */

ipmi_locate_info_t*
ipmi_locate_pci_get_dev_info (ipmi_interface_type_t type)
{
  ERR_EINVAL_NULL_RETURN (IPMI_INTERFACE_TYPE_VALID(type));

  return NULL;
}

#endif
