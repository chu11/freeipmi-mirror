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

#include <stdint.h>

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
};
typedef enum ipmi_locate_driver_type ipmi_locate_driver_type_t;

#define IPMI_LOCATE_DRIVER_VALID(__val) \
        (((__val) == IPMI_LOCATE_DRIVER_NONE \
	  || (__val) == IPMI_LOCATE_DRIVER_DEFAULTS \
	  || (__val) == IPMI_LOCATE_DRIVER_SMBIOS \
	  || (__val) == IPMI_LOCATE_DRIVER_ACPI \
	  || (__val) == IPMI_LOCATE_DRIVER_PCI) ? 1 : 0)

enum ipmi_interface_type
{
  IPMI_INTERFACE_RESERVED = 0,
  IPMI_INTERFACE_KCS = 1,
  IPMI_INTERFACE_SMIC = 2,
  IPMI_INTERFACE_BT = 3,
  IPMI_INTERFACE_SSIF = 4,
  /* Note: If you add a new interface here, don't forget to update 
     "IPMI_INTERFACE_MAX" macro below. */
};
typedef enum ipmi_interface_type ipmi_interface_type_t;

#define IPMI_INTERFACE_TYPE_VALID(__val) \
        (((__val) == IPMI_INTERFACE_KCS \
	  || (__val) == IPMI_INTERFACE_SMIC \
	  || (__val) == IPMI_INTERFACE_BT \
	  || (__val) == IPMI_INTERFACE_SSIF) ? 1 : 0)

#define IPMI_INTERFACE_MAX  IPMI_INTERFACE_SSIF
#define IPMI_INTERFACE_LAST IPMI_INTERFACE_MAX

struct ipmi_locate_info
{
  uint8_t ipmi_ver_major;
  uint8_t ipmi_ver_minor;
  ipmi_locate_driver_type_t locate_driver_type;
  uint8_t locate_driver;
  ipmi_interface_type_t interface_type; /* KCS, SMIC, BT, SSIF */
  char *bmc_i2c_dev_name;
  uint8_t addr_space_id;  /* Memory mapped, IO mapped, SMBus*/
  union {
    uint64_t bmc_iobase_addr;
    uint64_t bmc_membase_addr;
    uint8_t  bmc_smbus_slave_addr:7; /* ipmb_addr */
  } base_addr;
  uint8_t reg_space; /* Register spacing in bytes */
};
typedef struct ipmi_locate_info ipmi_locate_info_t;

ipmi_locate_info_t* ipmi_locate (ipmi_interface_type_t type);

ipmi_locate_info_t* ipmi_locate_smbios_get_dev_info (ipmi_interface_type_t type);

ipmi_locate_info_t* ipmi_locate_pci_get_dev_info (ipmi_interface_type_t type);

ipmi_locate_info_t *ipmi_locate_acpi_spmi_get_dev_info (ipmi_interface_type_t interface_type);

ipmi_locate_info_t* ipmi_locate_defaults_get_dev_info (ipmi_interface_type_t type);

void ipmi_locate_destroy (ipmi_locate_info_t* pinfo);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-locate.h */

