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

#define IPMI_DEBUG_DEFAULT_FD   STDERR_FILENO
  
int8_t ipmi_dump_setup(int fd, char *prefix, char *hdr, char *prefix_buf, uint32_t prefix_buf_len);
int8_t ipmi_obj_dump_perror (int fd, char *prefix, char *hdr, char *trlr, fiid_obj_t obj, fiid_template_t tmpl);
int8_t ipmi_obj_dump (int fd, fiid_obj_t obj, fiid_template_t tmpl);
int8_t ipmi_dump_lan_packet (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_session, fiid_template_t tmpl_msg_hdr, fiid_template_t tmpl_cmd);
int8_t ipmi_dump_rmcp_packet (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd);
uint8_t ipmi_kcs_print_state (int fd, uint8_t state);

void ipmi_debug(const char *fmt, ...);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-debug.h */


