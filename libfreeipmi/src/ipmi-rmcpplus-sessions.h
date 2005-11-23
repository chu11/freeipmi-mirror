/*
   ipmi-lanplus-sessions.h - IPMI LAN Commands

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _IPMI_RMCPPLUS_SESSIONS_H
#define _IPMI_RMCPPLUS_SESSIONS_H
 
#ifdef __cplusplus
extern "C" {
#endif

int32_t assemble_ipmi_lanplus_pkt (u_int8_t authentication_algorithm, u_int8_t integrity_algorithm, u_int8_t confidentiality_algorithm, u_int8_t *integrity_key, u_int32_t integrity_key_len, u_int8_t *confidentiality_key, u_int32_t confidentiality_key_len, fiid_obj_t obj_hdr_rmcp, fiid_obj_t obj_lanplus_hdr_session, fiid_obj_t obj_msg_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, fiid_obj_t obj_lanplus_trlr_session, fiid_template_t tmpl_trlr_session, u_int8_t *pkt, u_int32_t pkt_len);

#ifdef __cplusplus
}
#endif

#endif
