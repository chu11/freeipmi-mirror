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

#ifndef _IPMI_LAN_UTIL_H
#define	_IPMI_LAN_UTIL_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

int8_t ipmi_lan_check_session_sequence_number (fiid_obj_t obj_lan_session_hdr, 
                                               uint32_t session_sequence_number);

int8_t ipmi_lan_check_session_id (fiid_obj_t obj_lan_session_hdr, 
				  uint32_t session_id);

int8_t ipmi_lan_check_session_authentication_code (fiid_obj_t obj_lan_session_hdr_rs,
                                                   fiid_obj_t obj_lan_msg_hdr_rs,
                                                   fiid_obj_t obj_cmd,
                                                   fiid_obj_t obj_lan_msg_trlr_rs,
                                                   uint8_t authentication_type,
                                                   uint8_t *authentication_code_data,
                                                   uint32_t authentication_code_data_len);

int8_t ipmi_lan_check_packet_session_authentication_code (uint8_t *pkt, 
                                                          uint64_t pkt_len, 
                                                          uint8_t authentication_type, 
                                                          uint8_t *authentication_code_data, 
                                                          uint32_t authentication_code_data_len);

int8_t ipmi_lan_check_net_fn (fiid_obj_t obj_lan_msg_hdr, uint8_t net_fn);

int8_t ipmi_lan_check_rq_seq (fiid_obj_t obj_lan_msg_hdr, uint8_t rq_seq);

int8_t ipmi_lan_check_checksum (fiid_obj_t obj_lan_msg_hdr,
                                fiid_obj_t obj_cmd,
                                fiid_obj_t obj_lan_msg_trlr);
 
int8_t ipmi_lan_check_packet_checksum (uint8_t *pkt, uint64_t pkt_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-lan-util.h */


