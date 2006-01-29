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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/


#ifndef _RMCP_H
#define	_RMCP_H	1

#ifdef __cplusplus
extern "C" {
#endif

#define IP_HDR_TTL_BMC_DEFAULT   0x40

#define RMCP_VER_1_0             0x06 // RMCP Version 1.0

#define RMCP_AUX_BUS_SHUNT       0x26F
#define RMCP_PRI_RMCP_PORT  RMCP_AUX_BUS_SHUNT

#define RMCP_SECURE_AUX_BUS      0x298
#define RMCP_SEC_RMCP_PORT  RMCP_SECURE_AUX_BUS 

#define RMCP_RESERVED            0x00

#define RMCP_HDR_SEQ_NUM_NO_RMCP_ACK         0xFF

#define RMCP_HDR_MSG_CLASS_BIT_RMCP_NORMAL   0x0
#define RMCP_HDR_MSG_CLASS_BIT_RMCP_ACK      0x1

#define RMCP_HDR_MSG_CLASS_ASF   0x06
#define RMCP_HDR_MSG_CLASS_IPMI  0x07
#define RMCP_HDR_MSG_CLASS_OEM   0x08

#define RMCP_ASF_IANA_ENTERPRISE_NUM    0x11BE /* 4542 */

#define RMCP_ASF_MSG_TYPE_PRESENCE_PING 0x80
#define RMCP_ASF_MSG_TYPE_PRESENCE_PONG 0x40

/* #define RMCP_ASF_PRESENCE_PONG_MESSAGE_TAG_MAX 0xFE 
   - Should be removed before release --ab@gnu.org.in */

extern fiid_template_t tmpl_hdr_rmcp;
extern fiid_template_t tmpl_cmd_asf_presence_ping;
extern fiid_template_t tmpl_cmd_asf_presence_pong;

int8_t fill_hdr_rmcp (uint8_t message_class, fiid_obj_t obj_hdr);
int8_t fill_hdr_rmcp_ipmi (fiid_obj_t obj_hdr);
int8_t fill_hdr_rmcp_asf (fiid_obj_t obj_hdr);
/* MSG_TAG:
   achu: Consecutive ping messages should use different message tags,
   ranging from 0x00 to 0xFE.  This is because the RMCP consumers may
   optionally discard duplicate messages.  */
int8_t fill_cmd_asf_presence_ping(uint8_t msg_tag, fiid_obj_t obj_cmd);
int32_t assemble_rmcp_pkt (fiid_obj_t obj_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, uint8_t *pkt, uint32_t pkt_len);
int32_t unassemble_rmcp_pkt (void *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr, fiid_obj_t obj_cmd);
int8_t ipmi_rmcp_ping (int sockfd, struct sockaddr *hostaddr, unsigned long hostaddr_len, uint32_t msg_tag, fiid_obj_t pong);
int8_t ipmi_rmcp_msg_tag_chk (uint8_t msg_tag, fiid_obj_t pong);

#ifdef __cplusplus
}
#endif

#endif /* rmcp.h */
