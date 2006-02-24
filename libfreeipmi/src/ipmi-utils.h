/* 
   ipmi-utils.h - general utility procedures

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

#ifndef _IPMI_UTILS_H
#define	_IPMI_UTILS_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* XXX fix after reorg */
#include "fiid.h"

int8_t ipmi_checksum (uint8_t *buf, uint64_t len);

int8_t ipmi_completion_code_check (fiid_obj_t obj_cmd);

int8_t ipmi_ipv4_address_string2int(char *src, uint32_t *dest);

int8_t ipmi_mac_address_string2int(char *src, uint64_t *dest);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-utils.h */


