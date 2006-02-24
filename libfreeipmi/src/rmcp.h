/* 
   rmcp.h - remote management control protocol definitions

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


#ifndef _RMCP_H
#define	_RMCP_H	1

#ifdef __cplusplus
extern "C" {
#endif

/* XXX - FIX after include file reorg */
#include "fiid.h"

#define RMCP_VERSION_1_0              0x06 // RMCP Version 1.0

#define RMCP_AUX_BUS_SHUNT            0x26F
#define RMCP_PRIMARY_RMCP_PORT        RMCP_AUX_BUS_SHUNT

#define RMCP_SECURE_AUX_BUS           0x298
#define RMCP_SECONDARY_RMCP_PORT      RMCP_SECURE_AUX_BUS 

#define RMCP_HDR_SEQ_NUM_NO_RMCP_ACK  0xFF

#define RMCP_HDR_MESSAGE_CLASS_BIT_RMCP_NORMAL   0x0
#define RMCP_HDR_MESSAGE_CLASS_BIT_RMCP_ACK      0x1

#define RMCP_HDR_MESSAGE_CLASS_ASF   0x06
#define RMCP_HDR_MESSAGE_CLASS_IPMI  0x07
#define RMCP_HDR_MESSAGE_CLASS_OEM   0x08

#define RMCP_HDR_MESSAGE_CLASS_VALID(__message_class) \
        (((__message_class) == RMCP_HDR_MESSAGE_CLASS_ASF \
          || (__message_class) == RMCP_HDR_MESSAGE_CLASS_IPMI \
          || (__message_class) == RMCP_HDR_MESSAGE_CLASS_OEM) ? 1 : 0)

#define RMCP_ASF_IANA_ENTERPRISE_NUM    0x11BE /* 4542 */

#define RMCP_ASF_MESSAGE_TYPE_PRESENCE_PING 0x80
#define RMCP_ASF_MESSAGE_TYPE_PRESENCE_PONG 0x40

#define RMCP_ASF_MESSAGE_TAG_MAX 0xFE

extern fiid_template_t tmpl_rmcp_hdr;
extern fiid_template_t tmpl_cmd_asf_presence_ping;
extern fiid_template_t tmpl_cmd_asf_presence_pong;

int8_t fill_rmcp_hdr (uint8_t message_class, fiid_obj_t obj_rmcp_hdr);

int8_t fill_rmcp_hdr_ipmi (fiid_obj_t obj_rmcp_hdr);

int8_t fill_rmcp_hdr_asf (fiid_obj_t obj_rmcp_hdr);

/* MESSAGE_TAG:
   achu: Consecutive ping messages should use different message tags,
   ranging from 0x00 to 0xFE.  This is because the RMCP consumers may
   optionally discard duplicate messages.  */

int8_t fill_cmd_asf_presence_ping(uint8_t message_tag, fiid_obj_t obj_cmd);

int32_t assemble_rmcp_pkt (fiid_obj_t obj_rmcp_hdr, fiid_obj_t obj_cmd, uint8_t *pkt, uint32_t pkt_len);

int32_t unassemble_rmcp_pkt (void *pkt, uint32_t pkt_len, fiid_obj_t obj_rmcp_hdr, fiid_obj_t obj_cmd);

int8_t ipmi_rmcp_check_message_tag (uint8_t message_tag, fiid_obj_t pong);

#ifdef __cplusplus
}
#endif

#endif /* rmcp.h */
