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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include "freeipmi.h"

typedef ipmi_locate_info_t* ((*ipmi_locate_func)(ipmi_interface_type_t, ipmi_locate_info_t*));

ipmi_locate_info_t*
ipmi_locate (ipmi_interface_type_t type, ipmi_locate_info_t* pinfo)
{
  extern int errno;
  
  static ipmi_locate_func things_to_try[] =
    {
      smbios_get_dev_info,
      acpi_spmi_get_dev_info,
      pci_get_dev_info,
      defaults_get_dev_info,
      NULL
    };

  int i;
  ipmi_locate_info_t* pinfo2;
  
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));

  switch (type)
    {
    case IPMI_INTERFACE_KCS:
      pinfo->interface_type = type;
      break;
    case IPMI_INTERFACE_SMIC:
      pinfo->interface_type = type;
      break;
    case IPMI_INTERFACE_BT:
      pinfo->interface_type = type;
      break;
    case IPMI_INTERFACE_SSIF:
      pinfo->interface_type = type;
      pinfo->bmc_i2c_dev_name = strdup (IPMI_DEFAULT_I2C_DEVICE);
      break;
    case IPMI_INTERFACE_LAN:
      pinfo->interface_type = type;
      break;
    case IPMI_INTERFACE_RESERVED:
      break;
    }
  
  for (i = 0; things_to_try[i] != NULL; i++)
    {
      pinfo2 = (*things_to_try[i])(type, pinfo);
      
      if (pinfo2 != NULL)
	return (pinfo2);
    }

  pinfo->locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
  return (NULL);
}

void
ipmi_locate_free (ipmi_locate_info_t* pinfo)
{
  ipmi_xfree (pinfo->bmc_i2c_dev_name);
}
