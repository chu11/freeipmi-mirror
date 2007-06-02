/* 
   $Id: ipmi-locate.c,v 1.29 2007-06-02 15:41:49 chu11 Exp $ 

   ipmi-locate - Probes and displays IPMI devices.

   Copyright (C) 2003, 2004, 2005, 2006 FreeIPMI Core Team

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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "freeipmi-portability.h"
#include "ipmi-locate-argp.h"
#include "ipmi-common.h"

void 
display_ipmi_locate_info (struct ipmi_locate_info *info)
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
    case IPMI_LOCATE_DRIVER_DMIDECODE:
      printf ("IPMI locate driver: DMIDECODE\n");
      break;
    default:
      printf ("IPMI locate driver: UNKNOWN\n");
    };
  
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
  
  printf ("BMC driver device: %s\n", info->driver_device);
  
  switch (info->address_space_id)
    {
    case IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY:
      printf ("BMC memory base address: " FI_64 "X\n", info->driver_address);
      break;
    case IPMI_ADDRESS_SPACE_ID_SYSTEM_IO:
      printf ("BMC I/O base address: " FI_64 "X\n", info->driver_address);
      break;
    case IPMI_ADDRESS_SPACE_ID_SMBUS:
      printf ("BMC SMBUS slave address: %lX\n", (unsigned long)info->driver_address);
      break;
    default:
      printf ("error: Error parsing base address\n");
    }
  
  printf ("Register space: %d\n", info->reg_space);
  
  return;
}

void 
dmidecode_probe_display ()
{
  struct ipmi_locate_info info;
  
  printf ("Probing KCS device using DMIDECODE... ");
  if (ipmi_locate_dmidecode_get_dev_info (IPMI_INTERFACE_KCS, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");

  printf ("Probing SMIC device using DMIDECODE... ");
  if (ipmi_locate_dmidecode_get_dev_info (IPMI_INTERFACE_SMIC, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing BT device using DMIDECODE... ");
  if (ipmi_locate_dmidecode_get_dev_info (IPMI_INTERFACE_BT, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SSIF device using DMIDECODE... ");
  if (ipmi_locate_dmidecode_get_dev_info (IPMI_INTERFACE_SSIF, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  return;
}

void 
smbios_probe_display ()
{
  struct ipmi_locate_info info;
  
  printf ("Probing KCS device using SMBIOS... ");
  if (ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_KCS, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");

  printf ("Probing SMIC device using SMBIOS... ");
  if (ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_SMIC, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing BT device using SMBIOS... ");
  if (ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_BT, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SSIF device using SMBIOS... ");
  if (ipmi_locate_smbios_get_dev_info (IPMI_INTERFACE_SSIF, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
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
  struct ipmi_locate_info info;
  
  printf ("Probing KCS device using ACPI... ");
  if (ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_KCS, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SMIC device using ACPI... ");
  if (ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_SMIC, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing BT device using ACPI... ");
  if (ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_BT, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SSIF device using ACPI... ");
  if (ipmi_locate_acpi_spmi_get_dev_info (IPMI_INTERFACE_SSIF, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
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
  struct ipmi_locate_info info;
  
  printf ("Probing KCS device using PCI... ");
  if (ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_KCS, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SMIC device using PCI... ");
  if (ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_SMIC, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing BT device using PCI... ");
  if (ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_BT, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
      printf ("FAILED\n");
    }
  printf ("\n");
  
  printf ("Probing SSIF device using PCI... ");
  if (ipmi_locate_pci_get_dev_info (IPMI_INTERFACE_SSIF, &info) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
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
  struct ipmi_locate_info info;
  
  printf ("KCS device default values:\n");
  if (ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_KCS, &info) == 0)
    {
      display_ipmi_locate_info (&info);
    }
  printf ("\n");
  
  printf ("SMIC device default values:\n");
  if (ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_SMIC, &info) == 0)
    {
      display_ipmi_locate_info (&info);
    }
  printf ("\n");
  
  printf ("BT device default values:\n");
  if (ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_BT, &info) == 0)
    {
      display_ipmi_locate_info (&info);
    }
  printf ("\n");
  
  printf ("SSIF device default values:\n");
  if (ipmi_locate_defaults_get_dev_info (IPMI_INTERFACE_SSIF, &info) == 0)
    {
      display_ipmi_locate_info (&info);
    }
  printf ("\n");
  return;
}

int 
main (int argc, char **argv)
{
  if (!ipmi_is_root())
    {
      fprintf(stderr, "%s: Permission Denied\n", argv[0]);
      exit(1);
    }

  ipmi_disable_coredump();

  ipmi_locate_argp_parse (argc, argv);
  
  dmidecode_probe_display ();
  smbios_probe_display ();
  acpi_probe_display ();
  pci_probe_display ();
  defaults_display ();
  
  return (0);
}

