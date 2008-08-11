/* 
   Copyright (C) 2005-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-locate-argp.h"

#include "freeipmi-portability.h"
#include "tool-common.h"

void 
display_ipmi_locate_info (struct ipmi_locate_info *info)
{
  printf ("IPMI Version: %d.%d\n", 
	  info->ipmi_version_major, 
	  info->ipmi_version_minor);
  
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
      printf ("BMC memory base address: 0x" FI_64 "X\n", info->driver_address);
      break;
    case IPMI_ADDRESS_SPACE_ID_SYSTEM_IO:
      printf ("BMC I/O base address: 0x" FI_64 "X\n", info->driver_address);
      break;
    case IPMI_ADDRESS_SPACE_ID_SMBUS:
      printf ("BMC SMBUS slave address: 0x%lX\n", (unsigned long)info->driver_address);
      break;
    default:
      printf ("error: Error parsing base address\n");
    }
  
  printf ("Register spacing: %d\n", info->register_spacing);
  
  return;
}

void 
dmidecode_probe_display (void)
{
  struct ipmi_locate_info info;
  int rv;

  printf ("Probing KCS device using DMIDECODE... ");
  if ((rv = ipmi_locate_dmidecode_get_device_info (IPMI_INTERFACE_KCS, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");

  printf ("Probing SMIC device using DMIDECODE... ");
  if ((rv = ipmi_locate_dmidecode_get_device_info (IPMI_INTERFACE_SMIC, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing BT device using DMIDECODE... ");
  if ((rv = ipmi_locate_dmidecode_get_device_info (IPMI_INTERFACE_BT, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing SSIF device using DMIDECODE... ");
  if ((rv = ipmi_locate_dmidecode_get_device_info (IPMI_INTERFACE_SSIF, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  return;
}

void 
smbios_probe_display (void)
{
  struct ipmi_locate_info info;
  int rv;
  
  printf ("Probing KCS device using SMBIOS... ");
  if ((rv = ipmi_locate_smbios_get_device_info (IPMI_INTERFACE_KCS, 
                                                &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");

  printf ("Probing SMIC device using SMBIOS... ");
  if ((rv = ipmi_locate_smbios_get_device_info (IPMI_INTERFACE_SMIC, 
                                                &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing BT device using SMBIOS... ");
  if ((rv = ipmi_locate_smbios_get_device_info (IPMI_INTERFACE_BT, 
                                                &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing SSIF device using SMBIOS... ");
  if ((rv = ipmi_locate_smbios_get_device_info (IPMI_INTERFACE_SSIF, 
                                                &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  return;
}

void 
acpi_probe_display (void)
{
  struct ipmi_locate_info info;
  int rv;
  
  printf ("Probing KCS device using ACPI... ");
  if ((rv = ipmi_locate_acpi_spmi_get_device_info (IPMI_INTERFACE_KCS, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing SMIC device using ACPI... ");
  if ((rv = ipmi_locate_acpi_spmi_get_device_info (IPMI_INTERFACE_SMIC, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing BT device using ACPI... ");
  if ((rv = ipmi_locate_acpi_spmi_get_device_info (IPMI_INTERFACE_BT, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing SSIF device using ACPI... ");
  if ((rv = ipmi_locate_acpi_spmi_get_device_info (IPMI_INTERFACE_SSIF, 
                                                   &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  return;
}

void 
pci_probe_display (void)
{
  struct ipmi_locate_info info;
  int rv;
  
  printf ("Probing KCS device using PCI... ");
  if ((rv = ipmi_locate_pci_get_device_info (IPMI_INTERFACE_KCS, 
                                             &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing SMIC device using PCI... ");
  if ((rv = ipmi_locate_pci_get_device_info (IPMI_INTERFACE_SMIC, 
                                             &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing BT device using PCI... ");
  if ((rv = ipmi_locate_pci_get_device_info (IPMI_INTERFACE_BT, 
                                             &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("Probing SSIF device using PCI... ");
  if ((rv = ipmi_locate_pci_get_device_info (IPMI_INTERFACE_SSIF, 
                                             &info)) == 0)
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  return;
}

void 
defaults_display (void)
{
  struct ipmi_locate_info info;
  int rv;

  printf ("KCS device default values: ");
  if ((rv = ipmi_locate_defaults_get_device_info (IPMI_INTERFACE_KCS,
                                                  &info)) == 0)
    {
      printf("\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
  printf ("SMIC device default values: ");
  if ((rv = ipmi_locate_defaults_get_device_info (IPMI_INTERFACE_SMIC, 
                                                  &info)) == 0)
    {
      printf("\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  
#if 0

  /* Default values of BT not known, this will always fail */

  printf ("BT device default values: ");
  if ((rv = ipmi_locate_defaults_get_device_info (IPMI_INTERFACE_BT, 
                                                  &info)) == 0)
    {
      printf("\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
#else /* !0 */

  printf ("BT device default values: ");
  printf("\n");

#endif  /* !0 */
  
  printf ("SSIF device default values: ");
  if ((rv = ipmi_locate_defaults_get_device_info (IPMI_INTERFACE_SSIF, 
                                                  &info)) == 0)
    {
      printf("\n");
      display_ipmi_locate_info (&info);
    }
  else 
    {
#ifndef NDEBUG
      printf ("FAILED: %s\n", ipmi_locate_strerror(rv));
#else
      printf ("FAILED\n");
#endif
    }
  printf ("\n");
  return;
}

int 
main (int argc, char **argv)
{
  ipmi_disable_coredump();

  ipmi_locate_argp_parse (argc, argv);

  if (!ipmi_is_root())
    {
      fprintf(stderr, "%s: permission denied\n", argv[0]);
      exit(1);
    }

  dmidecode_probe_display ();
  smbios_probe_display ();
  acpi_probe_display ();
  pci_probe_display ();
  defaults_display ();
  
  return (0);
}

