/* 
   pcilocate.c - Locate IPMI interfaces by scanning PCI bus information

   Copyright (C) 2003, 2004 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "freeipmi.h"

typedef ipmi_locate_info_t* ((*ipmi_locate_func)(ipmi_interface_t, ipmi_locate_info_t*, int*));

ipmi_locate_info_t*
ipmi_locate (ipmi_interface_t type, ipmi_locate_info_t* pinfo, int* statusp)
{
  static ipmi_locate_func things_to_try[] =
    {
      pci_get_dev_info,
      acpi_spmi_get_dev_info,
      smbios_get_dev_info,
      NULL
    };
  int i;
  int status;
  ipmi_locate_info_t* pinfo2;

  memset (pinfo, 0, sizeof (ipmi_locate_info_t));

  status = 1;
  for (i = 0; things_to_try[i] != NULL; i++)
    {
      pinfo2 = (*things_to_try[i])(type, pinfo, &status);
      if (status == 0)
	{
	  if (statusp != NULL) *statusp = 0;
	  return pinfo2;
	}
      else if (status < 0)
	break;
    }
  if (statusp != NULL) *statusp = status;
  return NULL;
}

