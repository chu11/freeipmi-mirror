/*
   ipmi-debug.h - IPMI Debugging Functions

   Copyright (C) 2003 FreeIPMI Core Team

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

#ifndef _IPMI_DEBUG_H
#define	_IPMI_DEBUG_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#define IPMI_DEBUG_DEFAULT_FD   STDERR_FILENO
  
/* IPMI KCS Interface */
int8_t fiid_obj_dump_perror (int fd, char *prefix, char *hdr, char *trlr, fiid_obj_t obj, fiid_template_t tmpl);
int8_t fiid_obj_dump (int fd, fiid_obj_t obj, fiid_template_t tmpl);
int8_t fiid_obj_dump_lan (int fd, char *prefix, char *hdr, u_int8_t *pkt, u_int32_t pkt_len, fiid_template_t tmpl_session, fiid_template_t tmpl_msg_hdr, fiid_template_t tmpl_cmd);
int8_t fiid_obj_dump_rmcp (int fd, char *prefix, char *hdr, u_int8_t *pkt, u_int32_t pkt_len, fiid_template_t tmpl_cmd);
u_int8_t ipmi_kcs_print_state (int fd, u_int8_t state);



#ifdef __cplusplus
}
#endif

#endif /* ipmi-debug.h */


