/* 
   ipmi-locate.h - Locate IPMI interfaces by any means necessary.

   Copyright (C) 2004,05 FreeIPMI Core Team

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

#ifndef _IPMI_LOCATE_H
#define	_IPMI_LOCATE_H	1

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_REG_SPACE_DEFAULT   0x01

#define IPMI_ADDRESS_SPACE_ID_SYSTEM_MEMORY 0x00
#define IPMI_ADDRESS_SPACE_ID_SYSTEM_IO     0x01
#define IPMI_ADDRESS_SPACE_ID_SMBUS         0x04

#define IPMI_INTERFACE_RESERVED   0x00
#define IPMI_INTERFACE_KCS        0x01
#define IPMI_INTERFACE_SMIC       0x02
#define IPMI_INTERFACE_BT         0x03
#define IPMI_INTERFACE_SSIF       0x04
#define IPMI_INTERFACE_MAX        IPMI_INTERFACE_SSIF
/* 5-255 Reserved */

enum ipmi_interface
{
  ipmi_interface_kcs = 1,
  ipmi_interface_smic = 2,
  ipmi_interface_bt = 3,
  ipmi_interface_ssif = 4,
  ipmi_interface_last
};
typedef enum ipmi_interface ipmi_interface_t;

struct ipmi_locate_info
{
  u_int8_t ipmi_ver_major;
  u_int8_t ipmi_ver_minor;
  u_int8_t interface_type; /* KCS, SMIC, BT, SSIF */
  char *bmc_i2c_dev_name;
  u_int8_t addr_space_id;  /* Memory mapped, IO mapped, SMBus*/
  union {
    u_int64_t bmc_iobase_addr;
    u_int64_t bmc_membase_addr;
    u_int8_t  bmc_smbus_slave_addr:7;
  } base_addr;
  u_int8_t reg_space; /* Register spacing in bytes */
};
typedef struct ipmi_locate_info ipmi_locate_info_t;

ipmi_locate_info_t*
ipmi_locate (ipmi_interface_t type, ipmi_locate_info_t* pinfo, int* statusp);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-locate.h */

