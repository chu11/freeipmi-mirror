/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"

#include "libcommon/ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

/* achu: Used to be in ipmi-smic-api.h, but that is now removed. */
#define IPMI_SMIC_SMS_IO_BASE_DEFAULT    0x0CA9

static int
_ipmi_locate_defaults_get_device_info (int *locate_errnum,
                                       ipmi_interface_type_t type,
                                       struct ipmi_locate_info *info)
{
  struct ipmi_locate_info linfo;

  assert(locate_errnum);

  LOCATE_ERR_PARAMETERS ((type == IPMI_INTERFACE_KCS
                          || type == IPMI_INTERFACE_SMIC
                          || type == IPMI_INTERFACE_SSIF) 
                         && info);
  
  memset(&linfo, '\0', sizeof(struct ipmi_locate_info));
  linfo.interface_type = type;
  if (type == IPMI_INTERFACE_SSIF)
    {
      strncpy(linfo.driver_device, IPMI_DEFAULT_I2C_DEVICE, IPMI_LOCATE_PATH_MAX);
      linfo.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
    }
  linfo.locate_driver_type = IPMI_LOCATE_DRIVER_DEFAULTS;
  switch (type)
    {
    case IPMI_INTERFACE_KCS:
      linfo.ipmi_version_major = 1;
      linfo.ipmi_version_minor = 5;
      linfo.interface_type = IPMI_INTERFACE_KCS;
      linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
      linfo.driver_address = IPMI_KCS_SMS_IO_BASE_DEFAULT;
      linfo.register_spacing = 1;
      break;
    case IPMI_INTERFACE_SMIC:
      linfo.ipmi_version_major = 1;
      linfo.ipmi_version_minor = 5;
      linfo.interface_type = IPMI_INTERFACE_SMIC;
      linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
      linfo.driver_address = IPMI_SMIC_SMS_IO_BASE_DEFAULT;
      linfo.register_spacing = 1;
      break;
    case IPMI_INTERFACE_SSIF:
      linfo.ipmi_version_major = 1;
      linfo.ipmi_version_minor = 5;
      linfo.interface_type = IPMI_INTERFACE_SSIF;
      linfo.address_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
      linfo.driver_address = IPMI_SSIF_SMBUS_SLAVE_ADDRESS;
      linfo.register_spacing = 1;
      break;
    default:
      /* Should not reach */
      ERR_EXIT(0);
    }

  memcpy(info, &linfo, sizeof(struct ipmi_locate_info));
  return 0;
}

int
ipmi_locate_defaults_get_device_info (ipmi_interface_type_t type,
                                      struct ipmi_locate_info *info)
{
  int errnum;
  int *locate_errnum;

  locate_errnum = &errnum;

  if (_ipmi_locate_defaults_get_device_info(&errnum, type, info) < 0)
    {
      if (!errnum)
        LOCATE_ERRNUM_SET(IPMI_LOCATE_ERR_INTERNAL_ERROR);
      return errnum;
    }
  return 0;
}
