/* 
   ipmi-lan-interface.h - IPMI LAN Interface

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

#ifndef _IPMI_LAN_INTERFACE_H
#define	_IPMI_LAN_INTERFACE_H	1

#ifdef __cplusplus
extern "C" {
#endif

/*
  Complete IPMI LAN Request Packet
  +----------------------+
  |  RMCP                |
  |  Session             |
  |  Message             |
  |  Command             |
  |    Data              |
  |  Checksum            |
  +----------------------+
*/

/*
  Complete IPMI LAN Response Packet
  +----------------------+
  |  Session             |
  |  RMCP                |
  |  Message             |
  |  Command             |
  |    Completion Code   |
  |    Data              |
  |  Checksum            |
  +----------------------+
*/

#define IPMI_LAN_PKT_PAD_SIZE   1
#define IPMI_LAN_SEQ_NUM_MAX    0x3F /* 111111b */

#define IPMI_LAN_RQ_SEQ_INC(rq_seq) (rq_seq = ((rq_seq + 1) % (IPMI_LAN_SEQ_NUM_MAX + 1)))

#define IPMI_LAN_PKT_CHKSUM1_BLOCK_INDX(auth_type)                          \
  (fiid_obj_len_bytes (tmpl_hdr_rmcp) +                                     \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_type") +          \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_seq_num") +    \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_id") +         \
   ((auth_type == IPMI_SESSION_AUTH_TYPE_MD2                                \
     || auth_type == IPMI_SESSION_AUTH_TYPE_MD5                             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)                       \
    ? fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_code") : 0) +  \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"ipmi_msg_len"))

#define IPMI_LAN_PKT_RQ_CHKSUM1_BLOCK_LEN                       \
  fiid_obj_field_start_bytes (tmpl_lan_msg_hdr_rq, (uint8_t *)"chksum1") 

#define IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_INDX(auth_type)                       \
  (fiid_obj_len_bytes (tmpl_hdr_rmcp) +                                     \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_type") +          \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_seq_num") +    \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_id") +         \
   ((auth_type == IPMI_SESSION_AUTH_TYPE_MD2                                \
     || auth_type == IPMI_SESSION_AUTH_TYPE_MD5                             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)                       \
    ? fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_code") : 0) +  \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"ipmi_msg_len") +       \
   fiid_obj_field_end_bytes (tmpl_lan_msg_hdr_rq, (uint8_t *)"chksum1"))

#define IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_LEN(tmpl_cmd)            \
  (fiid_obj_len_bytes (tmpl_lan_msg_hdr_rq) -                  \
   fiid_obj_field_end_bytes (tmpl_lan_msg_hdr_rq, (uint8_t *)"chksum1") + \
   fiid_obj_len_bytes (tmpl_cmd)) 
   
#define IPMI_LAN_PKT_RS_CHKSUM1_BLOCK_LEN                      \
  fiid_obj_field_start_bytes (tmpl_lan_msg_hdr_rs, (uint8_t *)"chksum1")

#define IPMI_LAN_PKT_RS_CHKSUM2_BLOCK_INDX(auth_type)                       \
  (fiid_obj_len_bytes (tmpl_hdr_rmcp) +                                     \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_type") +          \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_seq_num") +    \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"session_id") +         \
   ((auth_type == IPMI_SESSION_AUTH_TYPE_MD2                                \
     || auth_type == IPMI_SESSION_AUTH_TYPE_MD5                             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)                       \
    ? fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"auth_code") : 0) +  \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, (uint8_t *)"ipmi_msg_len") +       \
   fiid_obj_field_end_bytes (tmpl_lan_msg_hdr_rs, (uint8_t *)"chksum1"))

#define IPMI_LAN_PKT_RS_CHKSUM2_BLOCK_LEN(tmpl_cmd)             \
  (fiid_obj_len_bytes (tmpl_lan_msg_hdr_rs) -                   \
   fiid_obj_field_end_bytes (tmpl_lan_msg_hdr_rs, (uint8_t *)"chksum1") +  \
   fiid_obj_len_bytes (tmpl_cmd))
   
extern fiid_template_t tmpl_lan_msg_hdr_rq;
extern fiid_template_t tmpl_lan_msg_hdr_rs;
extern fiid_template_t tmpl_lan_msg_trlr;


int8_t fill_lan_msg_hdr (uint8_t net_fn, 
			 uint8_t rs_lun, 
			 uint8_t rq_seq, 
			 fiid_obj_t obj_msg);

int32_t assemble_ipmi_lan_pkt (fiid_obj_t obj_hdr_rmcp, 
			       fiid_obj_t obj_hdr_session, 
			       fiid_template_t tmpl_hdr_session, 
			       fiid_obj_t obj_msg_hdr, 
			       fiid_obj_t obj_cmd, 
			       fiid_template_t tmpl_cmd, 
			       uint8_t *pkt, 
			       uint32_t pkt_len);

int8_t unassemble_ipmi_lan_pkt (uint8_t *pkt, 
				uint32_t pkt_len, 
				fiid_template_t tmpl_hdr_session, 
				fiid_template_t tmpl_cmd, 
				fiid_obj_t obj_hdr_rmcp, 
				fiid_obj_t obj_hdr_session, 
				fiid_obj_t obj_msg_hdr, 
				fiid_obj_t obj_cmd, 
				fiid_obj_t obj_msg_trlr);

ssize_t ipmi_lan_sendto (int sockfd, 
			 const void *buffer, 
			 size_t buffer_size, 
			 int flags, 
			 const struct sockaddr *server_addr, 
			 socklen_t server_addr_len);

ssize_t ipmi_lan_recvfrom (int sockfd, 
			   void *buffer, 
			   size_t buffer_size, 
			   int flags, 
			   struct sockaddr *server_addr, 
			   socklen_t *server_addr_len);

int8_t ipmi_lan_cmd (uint32_t sockfd, 
		     struct sockaddr *hostaddr, 
		     size_t hostaddr_len, 
		     uint8_t auth_type, 
		     uint32_t session_seq_num, 
		     uint32_t session_id, 
		     uint8_t *auth_code_data, 
		     uint32_t auth_code_data_len, 
		     uint8_t net_fn, 
		     uint8_t lun, 
		     uint8_t rq_seq, 
		     fiid_obj_t obj_cmd_rq, 
		     fiid_template_t tmpl_cmd_rq, 
		     fiid_obj_t obj_cmd_rs, 
		     fiid_template_t tmpl_cmd_rs);

int8_t ipmi_lan_cmd2 (ipmi_device_t *dev, 
		      fiid_obj_t obj_cmd_rq, 
		      fiid_template_t tmpl_cmd_rq, 
		      fiid_obj_t obj_cmd_rs, 
		      fiid_template_t tmpl_cmd_rs);
int8_t ipmi_lan_cmd_raw2 (ipmi_device_t *dev, 
			  uint8_t *buf_rq, 
			  size_t buf_rq_len, 
			  uint8_t *buf_rs, 
			  size_t *buf_rs_len);

int8_t ipmi_lan_check_net_fn (fiid_template_t tmpl_msg_hdr, 
			      fiid_obj_t obj_msg_hdr, 
			      uint8_t net_fn);

int8_t ipmi_lan_check_rq_seq (fiid_template_t tmpl_msg_hdr, 
			      fiid_obj_t obj_msg_hdr, 
			      uint8_t rq_seq);

int8_t ipmi_lan_check_chksum (uint8_t *pkt, uint64_t pkt_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-lan-interface.h */


