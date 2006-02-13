/* 
   $Id: ipmi-locate.c,v 1.6.2.1 2006-02-13 17:01:42 chu11 Exp $ 

   ipmi-locate - Probes and displays IPMI devices.

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

#include "freeipmi.h"
#include "ipmi-locate-argp.h"
#include "ipmi-common.h"

void 
display_ipmi_locate_info (ipmi_locate_info_t *info)
{
  printf ("IPMI Version: %d.%d\n", 
	  info->ipmi_ver_major, 
	  info->ipmi_ver_minor);
  
  switch (info->locate_driver_type)
    {
    case IPMI_LOCATE_DRIVER_NONE:
      printf ("IPMI locate driver: NONE\n");
      break;
    case IPMI_LOCATE_DRIVER_DEFAULTS:
      printf ("IPMI locate driver: DEFAULT\n");
      break;
    case IPMI_LOCATE_DRIVER_SMBIOS:
      printf ("IPMI locate driver: SMBIOS\n");
      break;
    case IPMI_LOCATE_DRIVER_ACPI:
      printf ("IPMI locate driver: ACPI\n");
      break;
    case IPMI_LOCATE_DRIVER_PCI:
      printf ("IPMI locate driver: PCI\n");
      break;
    default:
      printf ("IPMI locate driver: UNKNOWN\n");
    };
  
  printf ("IPMI locate driver: %d\n", info->locate_driver);
  
  switch (info->interface_type)
    {
    case IPMI_INTERFACE_RESERVED:
      printf ("IPMI interface: RESERVED\n");
      break;
    case IPMI_INTERFACE_KCS:
      printf ("IPMI interface: KCS\n");
      break;
    case IPMI_INTERFACE_SMIC:
      printf ("IPMI interface: SMIC\n");
      break;
    case IPMI_INTERFACE_BT:
      printf ("IPMI interface: BT\n");
      break;
    case IPMI_INTERFACE_SSIF:
      printf ("IPMI interface: SSIF\n");
      break;
    default:
      printf ("IPMI interface: UNKNOWN\n");
    }
  
  printf ("BMC I2C device: %s\n", info->bmc_i2c_dev_name);
  
  switch (info->addr_space_id)
    {
    case IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY:
      printf ("BMC memory base address: " FI_64 "X\n", info->base_addr.bmc_membase_addr);
      break;
    case IPMI_ADDRESS_SPACE_ID_SYSTEM_IO:
      printf ("BMC I/O base address: " FI_64 "X\n", info->base_addr.bmc_iobase_addr);
      break;
    case IPMI_ADDRESS_SPACE_ID_SMBUS:
      printf ("BMC SMBUS slave address: %lX\n", (unsigned long)info->base_addr.bmc_smbus_slave_addr);
      break;
    default:
      printf ("FATAL: error parsing base address\n");
    }
  
  printf ("Register space: %d\n", info->reg_space);
  
  return;
}

void 
smbios_probe_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo;
  
  pinfo = (ipmi_locate_info_t *) alloca (sizeof (ipmi_locate_info_t));
  
  printf ("Probing KCS device using SMBIOS... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (smbios_get_dev_info (IPMI_INTERFACE_KCS, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SMIC device using SMBIOS... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (smbios_get_dev_info (IPMI_INTERFACE_SMIC, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing BT device using SMBIOS... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (smbios_get_dev_info (IPMI_INTERFACE_BT, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SSIF device using SMBIOS... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (smbios_get_dev_info (IPMI_INTERFACE_SSIF, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  return;
}

void 
acpi_probe_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo;
  
  pinfo = (ipmi_locate_info_t *) alloca (sizeof (ipmi_locate_info_t));
  
  printf ("Probing KCS device using ACPI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (acpi_spmi_get_dev_info (IPMI_INTERFACE_KCS, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SMIC device using ACPI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (acpi_spmi_get_dev_info (IPMI_INTERFACE_SMIC, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing BT device using ACPI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (acpi_spmi_get_dev_info (IPMI_INTERFACE_BT, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SSIF device using ACPI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (acpi_spmi_get_dev_info (IPMI_INTERFACE_SSIF, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  return;
}

void 
pci_probe_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo;
  
  pinfo = (ipmi_locate_info_t *) alloca (sizeof (ipmi_locate_info_t));
  
  printf ("Probing KCS device using PCI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (pci_get_dev_info (IPMI_INTERFACE_KCS, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SMIC device using PCI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (pci_get_dev_info (IPMI_INTERFACE_SMIC, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing BT device using PCI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (pci_get_dev_info (IPMI_INTERFACE_BT, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SSIF device using PCI... ");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (pci_get_dev_info (IPMI_INTERFACE_SSIF, pinfo) != NULL)
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  return;
}

void 
defaults_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo;
  
  pinfo = (ipmi_locate_info_t *) alloca (sizeof (ipmi_locate_info_t));
  
  printf ("KCS device default values:\n");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (defaults_get_dev_info (IPMI_INTERFACE_KCS, pinfo) != NULL)
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  
  printf ("SMIC device default values:\n");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (defaults_get_dev_info (IPMI_INTERFACE_SMIC, pinfo) != NULL)
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  
  printf ("BT device default values:\n");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (defaults_get_dev_info (IPMI_INTERFACE_BT, pinfo) != NULL)
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  
  printf ("SSIF device default values:\n");
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  if (defaults_get_dev_info (IPMI_INTERFACE_SSIF, pinfo) != NULL)
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  
  return;
}

int 
main (int argc, char **argv)
{
  struct rlimit resource_limit;
  
  /* generate core dump on seg-fault */
  if (ipmi_is_root ())
    {
      resource_limit.rlim_cur =
	resource_limit.rlim_max = RLIM_INFINITY;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
	perror ("warning: setrlimit()");
    }
  
  ipmi_locate_argp_parse (argc, argv);
  
  smbios_probe_display ();
  acpi_probe_display ();
  pci_probe_display ();
  defaults_display ();
  
  return (0);
}

