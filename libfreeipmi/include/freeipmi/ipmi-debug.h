/*
   ipmi-debug.h - IPMI Debugging Functions

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

#ifndef _IPMI_DEBUG_H
#define	_IPMI_DEBUG_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid.h>
 
extern fiid_template_t tmpl_unexpected_data;

int8_t ipmi_dump_setup(int fd, char *prefix, char *hdr, char *prefix_buf, uint32_t prefix_buf_len);
int8_t ipmi_obj_dump_perror (int fd, char *prefix, char *hdr, char *trlr, fiid_obj_t obj);
int8_t ipmi_obj_dump (int fd, fiid_obj_t obj);
int8_t ipmi_dump_lan_packet (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_lan_msg_hdr, fiid_template_t tmpl_cmd);
int8_t ipmi_dump_rmcp_packet (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd);

int32_t ipmi_dump_rmcpplus_packet (int fd, char *prefix, char *hdr, uint8_t authentication_algorithm, uint8_t integrity_algorithm, uint8_t confidentiality_algorithm, uint8_t *integrity_key, uint32_t integrity_key_len, uint8_t *confidentiality_key, uint32_t confidentiality_key_len, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_lan_msg_hdr, fiid_template_t tmpl_cmd);

void ipmi_debug(const char *fmt, ...);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-debug.h */


