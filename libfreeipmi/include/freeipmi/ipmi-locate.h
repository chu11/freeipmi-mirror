/* 
   ipmi-locate.h - Locate IPMI interfaces by any means necessary.

   Copyright (C) 2004, 2005 FreeIPMI Core Team

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

#ifndef _IPMI_LOCATE_H
#define	_IPMI_LOCATE_H	1

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#define IPMI_LOCATE_PATH_MAX                1024

#define IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY 0x00
#define IPMI_ADDRESS_SPACE_ID_SYSTEM_IO     0x01
#define IPMI_ADDRESS_SPACE_ID_SMBUS         0x04

enum ipmi_locate_driver_type
{
  IPMI_LOCATE_DRIVER_NONE = 0,
  IPMI_LOCATE_DRIVER_DEFAULTS = 1,
  IPMI_LOCATE_DRIVER_SMBIOS = 2,
  IPMI_LOCATE_DRIVER_ACPI = 3,
  IPMI_LOCATE_DRIVER_PCI = 4,
  IPMI_LOCATE_DRIVER_DMIDECODE = 5
};
typedef enum ipmi_locate_driver_type ipmi_locate_driver_type_t;

#define IPMI_LOCATE_DRIVER_VALID(__val) \
        (((__val) == IPMI_LOCATE_DRIVER_NONE \
	  || (__val) == IPMI_LOCATE_DRIVER_DEFAULTS \
	  || (__val) == IPMI_LOCATE_DRIVER_SMBIOS \
	  || (__val) == IPMI_LOCATE_DRIVER_ACPI \
	  || (__val) == IPMI_LOCATE_DRIVER_PCI \
	  || (__val) == IPMI_LOCATE_DRIVER_DMIDECODE) ? 1 : 0)

enum ipmi_interface_type
{
  IPMI_INTERFACE_RESERVED = 0,
  IPMI_INTERFACE_KCS = 1,
  IPMI_INTERFACE_SMIC = 2,
  IPMI_INTERFACE_BT = 3,
  IPMI_INTERFACE_SSIF = 4,
};
typedef enum ipmi_interface_type ipmi_interface_type_t;

#define IPMI_INTERFACE_TYPE_VALID(__val) \
        (((__val) == IPMI_INTERFACE_KCS \
	  || (__val) == IPMI_INTERFACE_SMIC \
	  || (__val) == IPMI_INTERFACE_BT \
	  || (__val) == IPMI_INTERFACE_SSIF) ? 1 : 0)

struct ipmi_locate_info
{
  uint8_t ipmi_ver_major;
  uint8_t ipmi_ver_minor;
  ipmi_locate_driver_type_t locate_driver_type;
  ipmi_interface_type_t interface_type; /* KCS, SMIC, BT, SSIF */
  char driver_device[IPMI_LOCATE_PATH_MAX];
  uint8_t address_space_id;  /* Memory mapped, IO mapped, SMBus*/
  uint64_t driver_address;
  uint8_t reg_space; /* Register spacing in bytes */
};

int ipmi_locate (ipmi_interface_type_t type, struct ipmi_locate_info *info);

int ipmi_locate_smbios_get_dev_info (ipmi_interface_type_t type, struct ipmi_locate_info *info);

int ipmi_locate_pci_get_dev_info (ipmi_interface_type_t type, struct ipmi_locate_info *info);

int ipmi_locate_acpi_spmi_get_dev_info (ipmi_interface_type_t interface_type, struct ipmi_locate_info *info);

int ipmi_locate_defaults_get_dev_info (ipmi_interface_type_t type, struct ipmi_locate_info *info);

int ipmi_locate_dmidecode_get_dev_info (ipmi_interface_type_t type, struct ipmi_locate_info *info);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-locate.h */

