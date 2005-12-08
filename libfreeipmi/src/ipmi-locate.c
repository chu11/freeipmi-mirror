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
      pci_get_dev_info,
      acpi_spmi_get_dev_info,
      smbios_get_dev_info,
      NULL
    };

  int i;
  ipmi_locate_info_t* pinfo2;
  
  memset (pinfo, 0, sizeof (ipmi_locate_info_t));
  
  for (i = 0; things_to_try[i] != NULL; i++)
    {
      pinfo2 = (*things_to_try[i])(type, pinfo);
      
      if (pinfo2 != NULL)
	return (pinfo2);
    }

  switch (type){
  case IPMI_INTERFACE_KCS:
    pinfo->interface_type = IPMI_INTERFACE_KCS;
    pinfo->addr_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
    pinfo->base_addr.bmc_iobase_addr = IPMI_KCS_SMS_IO_BASE_DEFAULT;
    return (pinfo);
  case IPMI_INTERFACE_SMIC:
    pinfo->interface_type = IPMI_INTERFACE_SMIC;
    pinfo->addr_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
    pinfo->base_addr.bmc_iobase_addr = IPMI_SMIC_SMS_IO_BASE_DEFAULT;
    return (pinfo);
  case IPMI_INTERFACE_SSIF:
    pinfo->interface_type = IPMI_INTERFACE_SSIF;
    pinfo->addr_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
    pinfo->base_addr.bmc_smbus_slave_addr = IPMI_SSIF_SMBUS_SLAVE_ADDR;
    return (pinfo);
  case IPMI_INTERFACE_BT:
  default:
    return (NULL);
  }
  return (NULL);
}
