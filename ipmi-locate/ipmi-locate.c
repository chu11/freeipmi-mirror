/*
 * Copyright (C) 2005-2014 FreeIPMI Core Team
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

#include "ipmi-locate_.h"
#include "ipmi-locate-argp.h"

#include "freeipmi-portability.h"
#include "tool-common.h"
#include "tool-util-common.h"

static void
display_ipmi_locate_info (struct ipmi_locate_info *info)
{
  assert (info);

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

static void
dmidecode_probe_display (ipmi_locate_ctx_t ctx)
{
  struct ipmi_locate_info info;

  assert (ctx);

  printf ("Probing KCS device using DMIDECODE... ");
  if (!ipmi_locate_dmidecode_get_device_info (ctx,
                                              IPMI_INTERFACE_KCS,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SMIC device using DMIDECODE... ");
  if (!ipmi_locate_dmidecode_get_device_info (ctx,
                                              IPMI_INTERFACE_SMIC,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing BT device using DMIDECODE... ");
  if (!ipmi_locate_dmidecode_get_device_info (ctx,
                                              IPMI_INTERFACE_BT,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SSIF device using DMIDECODE... ");
  if (!ipmi_locate_dmidecode_get_device_info (ctx,
                                              IPMI_INTERFACE_SSIF,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  return;
}

static void
smbios_probe_display (ipmi_locate_ctx_t ctx)
{
  struct ipmi_locate_info info;

  assert (ctx);

  printf ("Probing KCS device using SMBIOS... ");
  if (!ipmi_locate_smbios_get_device_info (ctx,
                                           IPMI_INTERFACE_KCS,
                                           &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SMIC device using SMBIOS... ");
  if (!ipmi_locate_smbios_get_device_info (ctx,
                                           IPMI_INTERFACE_SMIC,
                                           &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing BT device using SMBIOS... ");
  if (!ipmi_locate_smbios_get_device_info (ctx,
                                           IPMI_INTERFACE_BT,
                                           &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SSIF device using SMBIOS... ");
  if (!ipmi_locate_smbios_get_device_info (ctx,
                                           IPMI_INTERFACE_SSIF,
                                           &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  return;
}

static void
acpi_probe_display (ipmi_locate_ctx_t ctx)
{
  struct ipmi_locate_info info;

  assert (ctx);

  printf ("Probing KCS device using ACPI... ");
  if (!ipmi_locate_acpi_spmi_get_device_info (ctx,
                                              IPMI_INTERFACE_KCS,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SMIC device using ACPI... ");
  if (!ipmi_locate_acpi_spmi_get_device_info (ctx,
                                              IPMI_INTERFACE_SMIC,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing BT device using ACPI... ");
  if (!ipmi_locate_acpi_spmi_get_device_info (ctx,
                                              IPMI_INTERFACE_BT,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SSIF device using ACPI... ");
  if (!ipmi_locate_acpi_spmi_get_device_info (ctx,
                                              IPMI_INTERFACE_SSIF,
                                              &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  return;
}

static void
pci_probe_display (ipmi_locate_ctx_t ctx)
{
  struct ipmi_locate_info info;

  assert (ctx);

  printf ("Probing KCS device using PCI... ");
  if (!ipmi_locate_pci_get_device_info (ctx,
                                        IPMI_INTERFACE_KCS,
                                        &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SMIC device using PCI... ");
  if (!ipmi_locate_pci_get_device_info (ctx,
                                        IPMI_INTERFACE_SMIC,
                                        &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing BT device using PCI... ");
  if (!ipmi_locate_pci_get_device_info (ctx,
                                        IPMI_INTERFACE_BT,
                                        &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("Probing SSIF device using PCI... ");
  if (!ipmi_locate_pci_get_device_info (ctx,
                                        IPMI_INTERFACE_SSIF,
                                        &info))
    {
      printf ("done\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  return;
}

static void
defaults_display (ipmi_locate_ctx_t ctx)
{
  struct ipmi_locate_info info;

  assert (ctx);

  printf ("KCS device default values: ");
  if (!ipmi_locate_defaults_get_device_info (ctx,
                                             IPMI_INTERFACE_KCS,
                                             &info))
    {
      printf ("\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

  printf ("SMIC device default values: ");
  if (!ipmi_locate_defaults_get_device_info (ctx,
                                             IPMI_INTERFACE_SMIC,
                                             &info))
    {
      printf ("\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");

#if 0

  /* Default values of BT not known, this will always fail */

  printf ("BT device default values: ");
  if (!ipmi_locate_defaults_get_device_info (ctx,
                                             IPMI_INTERFACE_BT,
                                             &info))
    {
      printf ("\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");
#else /* !0 */

  printf ("BT device default values: ");
  printf ("\n");

#endif  /* !0 */

  printf ("SSIF device default values: ");
  if (!ipmi_locate_defaults_get_device_info (ctx,
                                             IPMI_INTERFACE_SSIF,
                                             &info))
    {
      printf ("\n");
      display_ipmi_locate_info (&info);
    }
  else
    {
      if (ipmi_locate_ctx_errnum (ctx) == IPMI_LOCATE_ERR_SYSTEM_ERROR)
	printf ("FAILED\n");
      else
	printf ("ERROR: %s\n", ipmi_locate_ctx_errormsg (ctx));
    }
  printf ("\n");
  return;
}

int
main (int argc, char **argv)
{
  struct ipmi_locate_arguments cmd_args;
  ipmi_locate_ctx_t ctx = NULL;

  ipmi_disable_coredump ();

  ipmi_locate_argp_parse (argc, argv, &cmd_args);

  if (!ipmi_is_root ())
    {
      fprintf (stderr, "%s: permission denied\n", argv[0]);
      exit (EXIT_FAILURE);
    }

  if (!(ctx = ipmi_locate_ctx_create ()))
    {
      fprintf (stderr, "ipmi_locate_ctx_create(): %s", strerror (errno));
      exit (EXIT_FAILURE);
    }

  dmidecode_probe_display (ctx);
  smbios_probe_display (ctx);
  acpi_probe_display (ctx);
  pci_probe_display (ctx);
  if (cmd_args.defaults)
    defaults_display (ctx);

  ipmi_locate_ctx_destroy (ctx);
  return (EXIT_SUCCESS);
}

