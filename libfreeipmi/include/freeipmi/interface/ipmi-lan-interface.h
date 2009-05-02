/*
   Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifndef _IPMI_LAN_INTERFACE_H
#define _IPMI_LAN_INTERFACE_H   1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#include <sys/types.h>
#include <sys/socket.h>

#define IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX    0x3F /* 111111b */

/* 
 * see freeipmi/templates/ for template definitions 
 */

extern fiid_template_t tmpl_lan_session_hdr;
extern fiid_template_t tmpl_lan_msg_hdr_rq;
extern fiid_template_t tmpl_lan_msg_hdr_rs;
extern fiid_template_t tmpl_lan_msg_trlr;

int fill_lan_session_hdr (uint8_t authentication_type,
                          uint32_t session_sequence_number,
                          uint32_t session_id,
                          fiid_obj_t obj_lan_session_hdr);

int fill_lan_msg_hdr (uint8_t rs_addr,
                      uint8_t net_fn,
                      uint8_t rs_lun,
                      uint8_t rq_seq,
                      fiid_obj_t obj_lan_msg_hdr);

int assemble_ipmi_lan_pkt (fiid_obj_t obj_rmcp_hdr,
                           fiid_obj_t obj_lan_session_hdr,
                           fiid_obj_t obj_lan_msg_hdr,
                           fiid_obj_t obj_cmd,
                           const uint8_t *authentication_code_data,
                           unsigned int authentication_code_data_len,
                           uint8_t *pkt,
                           unsigned int pkt_len);

int unassemble_ipmi_lan_pkt (const uint8_t *pkt,
                             unsigned int pkt_len,
                             fiid_obj_t obj_rmcp_hdr,
                             fiid_obj_t obj_lan_session_hdr,
                             fiid_obj_t obj_lan_msg_hdr,
                             fiid_obj_t obj_cmd,
                             fiid_obj_t obj_lan_msg_trlr);

ssize_t ipmi_lan_sendto (int s,
                         const void *buf,
                         size_t len,
                         int flags,
                         const struct sockaddr *to,
                         socklen_t tolen);

ssize_t ipmi_lan_recvfrom (int s,
                           void *buf,
                           size_t len,
                           int flags,
                           struct sockaddr *from,
                           socklen_t *fromlen);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-lan-interface.h */


