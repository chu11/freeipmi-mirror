/* 
   smbios-locate.h - SMBIOS driver to locate IPMI interfaces.

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

/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

#ifndef _SMBIOS_LOCATE_H
#define _SMBIOS_LOCATE_H 1

#define IPMI_SMBIOS_ENTRY_CSUM_OFFSET 	0x4
#define IPMI_SMBIOS_ENTRY_LEN_OFFSET 	0x5
#define IPMI_SMBIOS_ENTRY_ANCHOR_OFFSET 	0x10
#define IPMI_SMBIOS_ENTRY_ANCHOR_CSUM_OFFSET 0x15

#define IPMI_SMBIOS_IPMI_DEV_INFO_SIG 		38
#define IPMI_SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET 	0x4

#define IPMI_SMBIOS_AREA_START 		0x000f0000
#define IPMI_SMBIOS_AREA_END 		0x000fffff
#define IPMI_SMBIOS_AREA_LEN 		((IPMI_SMBIOS_AREA_END - IPMI_SMBIOS_AREA_START) + 1)
#define IPMI_SMBIOS_AREA_ALIGN 		16
#define IPMI_SMBIOS_ENTRY_TLEN_OFFSET 	0x16
#define IPMI_SMBIOS_ENTRY_PTR_OFFSET 	0x18
#define IPMI_SMBIOS_DEV_INFO_LEN_OFFSET 	0x1
#define IPMI_SMBIOS_IPMI_DEV_INFO_VER_OFFSET 	0x5
#define IPMI_SMBIOS_IPMI_DEV_INFO_I2C_OFFSET 	0x6
#define IPMI_SMBIOS_IPMI_DEV_INFO_NVSTOR_OFFSET 	0x7
#define IPMI_SMBIOS_IPMI_DEV_INFO_ADDR_OFFSET 	0x8
#define IPMI_SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET 	0x10
#define IPMI_SMBIOS_LSB_BIT 				4
#define IPMI_SMBIOS_REGSPACING_SHIFT 		        6
#define IPMI_SMBIOS_REGSPACING_MASK 			0x3
#define IPMI_SMBIOS_INTINFO_PRESENT_BIT 		3
#define IPMI_SMBIOS_INTINFO_POLARITY_BIT 		1
#define IPMI_SMBIOS_INTINFO_TRIGGER_BIT 		0
#define IPMI_SMBIOS_DEV_INFO_INTNUM_OFFSET 		0x11

#define IPMI_SMBIOS_REG_SPACE_1BYTE_BOUND    0x00
#define IPMI_SMBIOS_REG_SPACE_4BYTE_BOUND    0x01
#define IPMI_SMBIOS_REG_SPACE_16BYTE_BOUND   0x02
#define IPMI_SMBIOS_REG_SPACE_RESERVED       0x03

int ipmi_smbios_reg_space (u_int8_t reg_space_boundary, u_int8_t *reg_space);
ipmi_locate_info_t* smbios_get_dev_info (ipmi_interface_type_t type, ipmi_locate_info_t* pinfo);

#endif
