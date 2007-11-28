/* 
   ipmi-locate.c - Locate IPMI interfaces by scanning various system
   information

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif 
#include <errno.h>

#include "freeipmi/ipmi-locate.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"
#include "xmalloc.h"

typedef int ((*ipmi_locate_func)(ipmi_interface_type_t, struct ipmi_locate_info *));

int
ipmi_locate (ipmi_interface_type_t type, struct ipmi_locate_info *info)
{
  static ipmi_locate_func things_to_try[] =
    {
      ipmi_locate_dmidecode_get_device_info, 
      ipmi_locate_smbios_get_device_info,
      ipmi_locate_acpi_spmi_get_device_info,
      ipmi_locate_pci_get_device_info,
      ipmi_locate_defaults_get_device_info,
      NULL
    };
  struct ipmi_locate_info linfo;
  int i, rv;

  ERR_EINVAL (IPMI_INTERFACE_TYPE_VALID(type) && info);
  
  for (i = 0; things_to_try[i] != NULL; i++)
    {
      memset (&linfo, 0, sizeof (struct ipmi_locate_info));
      rv = (*things_to_try[i])(type, &linfo);

      if (!rv)
	{
	  memcpy(info, &linfo, sizeof(struct ipmi_locate_info));
	  return 0;
	}
    }

  return (-1);
}
