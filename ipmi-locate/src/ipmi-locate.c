/* 
   $Id: ipmi-locate.c,v 1.1 2005-12-17 21:28:09 balamurugan Exp $ 

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include "common.h"

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
    case 0x0:
      printf ("BMC memory base address: %lX\n", info->base_addr.bmc_membase_addr);
      break;
    case 0x1:
      printf ("BMC I/O base address: %lX\n", info->base_addr.bmc_iobase_addr);
      break;
    case 0x2:
      printf ("BMC SMBUS slave address: %lX\n", info->base_addr.bmc_smbus_slave_addr);
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
  
  return;
}

void 
acpi_probe_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo;
  
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
  
  return;
}

void 
pci_probe_display ()
{
  extern int errno;
  ipmi_locate_info_t *pinfo;
  
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
  
  return;
}

int 
main (int argc, char **argv)
{
  struct rlimit resource_limit;
  struct arguments *arguments;
  int retval = 0;
  
  textdomain (PACKAGE);
  
  /* generate core dump on seg-fault */
  resource_limit.rlim_cur =
    resource_limit.rlim_max = RLIM_INFINITY;
  if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
    {
      perror ("setrlimit()");
      exit (EXIT_FAILURE);
    }
  
  fi_argp_parse (argc, argv);
  
  arguments = fi_get_arguments ();
  
  smbios_probe_display ();  
  acpi_probe_display ();  
  pci_probe_display ();  
  
  return (0);
}

