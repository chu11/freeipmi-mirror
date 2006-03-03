/*
   ipmi-rmcpplus-interface.h - IPMI RMCPPLUS Interface

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

#ifndef _IPMI_RMCPPLUS_INTERFACE_H
#define _IPMI_RMCPPLUS_INTERFACE_H
 
#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid.h>

int32_t assemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm, uint8_t integrity_algorithm, uint8_t confidentiality_algorithm, uint8_t *integrity_key, uint32_t integrity_key_len, uint8_t *confidentiality_key, uint32_t confidentiality_key_len, fiid_obj_t obj_rmcp_hdr, fiid_obj_t obj_rmcpplus_session_hdr, fiid_obj_t obj_msg_hdr, fiid_obj_t obj_cmd, fiid_obj_t obj_rmcpplus_session_trlr, uint8_t *pkt, uint32_t pkt_len);

int32_t unassemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm, uint8_t integrity_algorithm, uint8_t confidentiality_algorithm, uint8_t *integrity_key, uint32_t integrity_key_len, uint8_t *confidentiality_key, uint32_t confidentiality_key_len, uint8_t *pkt, uint32_t pkt_len, fiid_obj_t obj_rmcp_hdr, fiid_obj_t obj_rmcpplus_session_hdr, fiid_obj_t obj_payload, fiid_obj_t obj_msg_hdr, fiid_obj_t obj_cmd, fiid_obj_t obj_lan_msg_trlr, fiid_obj_t obj_rmcpplus_session_trlr);

#ifdef __cplusplus
}
#endif

#endif
