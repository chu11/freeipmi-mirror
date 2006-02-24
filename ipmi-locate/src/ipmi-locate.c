/* 
   $Id: ipmi-locate.c,v 1.11 2006-02-24 05:46:08 chu11 Exp $ 

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

#include "freeipmi-build.h"
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
  ipmi_locate_info_t *pinfo = NULL;
  
  printf ("Probing KCS device using SMBIOS... ");
  if ((pinfo = ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_KCS)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);

  printf ("Probing SMIC device using SMBIOS... ");
  if ((pinfo = ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_SMIC)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing BT device using SMBIOS... ");
  if ((pinfo = ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_BT)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing SSIF device using SMBIOS... ");
  if ((pinfo = ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_SSIF)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  return;
}

void 
acpi_probe_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo = NULL;
  
  printf ("Probing KCS device using ACPI... ");
  if ((pinfo = ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_KCS)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing SMIC device using ACPI... ");
  if ((pinfo = ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_SMIC)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing BT device using ACPI... ");
  if ((pinfo = ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_BT)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing SSIF device using ACPI... ");
  if ((pinfo = ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_SSIF)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  return;
}

void 
pci_probe_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo = NULL;
  
  printf ("Probing KCS device using PCI... ");
  if ((pinfo = ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_KCS)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing SMIC device using PCI... ");
  if ((pinfo = ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_SMIC)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing BT device using PCI... ");
  if ((pinfo = ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_BT)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("Probing SSIF device using PCI... ");
  if ((pinfo = ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_SSIF)))
    {
      printf ("done\n");
      display_ipmi_locate_info (pinfo);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  return;
}

void 
defaults_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo = NULL;
  
  printf ("KCS device default values:\n");
  if ((pinfo = ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_KCS)))
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("SMIC device default values:\n");
  if ((pinfo = ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_SMIC)))
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("BT device default values:\n");
  if ((pinfo = ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_BT)))
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
  
  printf ("SSIF device default values:\n");
  if ((pinfo = ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_SSIF)))
    {
      display_ipmi_locate_info (pinfo);
    }
  printf ("\n");
  ipmi_locate_destroy(pinfo);
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

