/* 
   ipmi-locate-defaults.c - Return default locate info for IPMI interfaces.

   Copyright (C) 2005 FreeIPMI Core Team

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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-locate.h"
#include "freeipmi/ipmi-kcs-api.h"
#include "freeipmi/ipmi-slave-address-spec.h"
#include "freeipmi/ipmi-smic-api.h"
#include "freeipmi/ipmi-ssif-api.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"

int
ipmi_locate_defaults_get_dev_info (ipmi_interface_type_t type, struct ipmi_locate_info *info)
{
  struct ipmi_locate_info linfo;

  ERR_EINVAL ((type == IPMI_INTERFACE_KCS
	       || type == IPMI_INTERFACE_SMIC
	       || type == IPMI_INTERFACE_SSIF) 
	      && info);

  memset(&linfo, '\0', sizeof(struct ipmi_locate_info));
  linfo.interface_type = type;
  if (type == IPMI_INTERFACE_SSIF)
    {
      strncpy(linfo.bmc_i2c_dev_name, IPMI_DEFAULT_I2C_DEVICE, IPMI_LOCATE_PATH_MAX);
      linfo.bmc_i2c_dev_name[IPMI_LOCATE_PATH_MAX - 1] = '\0';
    }
  linfo.locate_driver_type = IPMI_LOCATE_DRIVER_DEFAULTS;
  switch (type)
    {
    case IPMI_INTERFACE_KCS:
      linfo.ipmi_ver_major = 1;
      linfo.ipmi_ver_minor = 5;
      linfo.interface_type = IPMI_INTERFACE_KCS;
      linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
      linfo.base_address.bmc_iobase_address = IPMI_KCS_SMS_IO_BASE_DEFAULT;
      linfo.reg_space = 1;
      break;
    case IPMI_INTERFACE_SMIC:
      linfo.ipmi_ver_major = 1;
      linfo.ipmi_ver_minor = 5;
      linfo.interface_type = IPMI_INTERFACE_SMIC;
      linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
      linfo.base_address.bmc_iobase_address = IPMI_SMIC_SMS_IO_BASE_DEFAULT;
      linfo.reg_space = 1;
      break;
    case IPMI_INTERFACE_SSIF:
      linfo.ipmi_ver_major = 1;
      linfo.ipmi_ver_minor = 5;
      linfo.interface_type = IPMI_INTERFACE_SSIF;
      linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
      linfo.base_address.bmc_smbus_slave_address = IPMI_SSIF_SMBUS_SLAVE_ADDRESS;
      linfo.reg_space = 1;
      break;
    default:
      ERR_EXIT(0);
    }

  memcpy(info, &linfo, sizeof(struct ipmi_locate_info));
  return 0;
}
