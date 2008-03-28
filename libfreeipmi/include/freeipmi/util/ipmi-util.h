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

#ifndef _IPMI_UTIL_H
#define	_IPMI_UTIL_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

uint8_t ipmi_checksum (uint8_t *buf, uint64_t len);

int8_t ipmi_check_cmd(fiid_obj_t obj_cmd, uint8_t cmd);

int8_t ipmi_check_completion_code(fiid_obj_t obj_cmd, uint8_t completion_code);

int8_t ipmi_check_completion_code_success (fiid_obj_t obj_cmd);

int ipmi_get_random (uint8_t *buf, unsigned int buflen);

int8_t ipmi_is_ipmi_1_5_packet(uint8_t *pkt, uint32_t pkt_len);

int8_t ipmi_is_ipmi_2_0_packet(uint8_t *pkt, uint32_t pkt_len);

const char *ipmi_cmd_str(uint8_t net_fn, uint8_t cmd);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-util.h */


