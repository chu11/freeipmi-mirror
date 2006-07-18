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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/ipmi-locate.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"
#include "xmalloc.h"

typedef ipmi_locate_info_t* ((*ipmi_locate_func)(ipmi_interface_type_t));

ipmi_locate_info_t*
ipmi_locate (ipmi_interface_type_t type)
{
  static ipmi_locate_func things_to_try[] =
    {
      ipmi_locate_dmidecode_get_dev_info, 
      ipmi_locate_smbios_get_dev_info,
      ipmi_locate_acpi_spmi_get_dev_info,
      ipmi_locate_pci_get_dev_info,
      ipmi_locate_defaults_get_dev_info,
      NULL
    };
  ipmi_locate_info_t* pinfo = NULL;
  int i;

  ERR_EINVAL_NULL_RETURN (IPMI_INTERFACE_TYPE_VALID(type));
  
  for (i = 0; things_to_try[i] != NULL; i++)
    {
      pinfo = (*things_to_try[i])(type);
      
      if (pinfo)
	return (pinfo);
    }

  pinfo->locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
  return (NULL);
}

void
ipmi_locate_destroy (ipmi_locate_info_t* pinfo)
{
  if (pinfo)
    {
      if (pinfo->bmc_i2c_dev_name)
	xfree (pinfo->bmc_i2c_dev_name);
      xfree (pinfo);
    }
}
