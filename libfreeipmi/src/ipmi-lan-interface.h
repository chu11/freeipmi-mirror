/* 
   ipmi-lan-interface.h - IPMI LAN Interface

   Copyright (C) 2003, 2004 FreeIPMI Core Team

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

#include <sys/types.h>
#include <sys/socket.h>

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
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "auth_type") +          \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "session_seq_num") +    \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "session_id") +         \
   ((auth_type == IPMI_SESSION_AUTH_TYPE_MD2                                \
     || auth_type == IPMI_SESSION_AUTH_TYPE_MD5                             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)                       \
    ? fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "auth_code") : 0) +  \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "ipmi_msg_len"))

#define IPMI_LAN_PKT_RQ_CHKSUM1_BLOCK_LEN                       \
  fiid_obj_field_start_bytes (tmpl_lan_msg_hdr_rq, "chksum1") 

#define IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_INDX(auth_type)                       \
  (fiid_obj_len_bytes (tmpl_hdr_rmcp) +                                     \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "auth_type") +          \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "session_seq_num") +    \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "session_id") +         \
   ((auth_type == IPMI_SESSION_AUTH_TYPE_MD2                                \
     || auth_type == IPMI_SESSION_AUTH_TYPE_MD5                             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)                       \
    ? fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "auth_code") : 0) +  \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "ipmi_msg_len") +       \
   fiid_obj_field_end_bytes (tmpl_lan_msg_hdr_rq, "chksum1"))

#define IPMI_LAN_PKT_RQ_CHKSUM2_BLOCK_LEN(tmpl_cmd)            \
  (fiid_obj_field_len_bytes (tmpl_lan_msg_hdr_rq, "rq_addr") + \
   fiid_obj_field_len_bytes (tmpl_lan_msg_hdr_rq, "rq_lun") +  \
   fiid_obj_field_len_bytes (tmpl_lan_msg_hdr_rq, "rq_seq") +  \
   fiid_obj_len_bytes (tmpl_cmd)) 
   
#define IPMI_LAN_PKT_RS_CHKSUM1_BLOCK_LEN                      \
  fiid_obj_field_start_bytes (tmpl_lan_msg_hdr_rs, "chksum1")

#define IPMI_LAN_PKT_RS_CHKSUM2_BLOCK_INDX(auth_type)                       \
  (fiid_obj_len_bytes (tmpl_hdr_rmcp) +                                     \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "auth_type") +          \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "session_seq_num") +    \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "session_id") +         \
   ((auth_type == IPMI_SESSION_AUTH_TYPE_MD2                                \
     || auth_type == IPMI_SESSION_AUTH_TYPE_MD5                             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY             \
     || auth_type == IPMI_SESSION_AUTH_TYPE_OEM_PROP)                       \
    ? fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "auth_code") : 0) +  \
   fiid_obj_field_len_bytes (tmpl_hdr_session_auth, "ipmi_msg_len") +       \
   fiid_obj_field_end_bytes (tmpl_lan_msg_hdr_rs, "chksum1"))

#define IPMI_LAN_PKT_RS_CHKSUM2_BLOCK_LEN(tmpl_cmd)             \
  (fiid_obj_field_len_bytes (tmpl_lan_msg_hdr_rs, "rq_addr") +  \
   fiid_obj_field_len_bytes (tmpl_lan_msg_hdr_rs, "rq_lun") +   \
   fiid_obj_field_len_bytes (tmpl_lan_msg_hdr_rs, "rq_seq") +   \
   fiid_obj_len_bytes (tmpl_cmd))
   
#if 0  
#pragma pack(1)
typedef struct ipmi_lan_msg_rq
{
  struct {
    u_int8_t slave_addr;
    net_fn_t net_fn;
  } rs;
  ipmi_chksum_t chksum;
  struct {
    u_int8_t slave_addr;
    u_int8_t lun:2;
    u_int8_t rq_seq:6;
  } rq;
} ipmi_lan_msg_rq_t;

typedef struct ipmi_lan_msg_rs
{
  struct {
    u_int8_t slave_addr;
    net_fn_t net_fn;
  } rq;
  ipmi_chksum_t chksum;
  struct {
    u_int8_t slave_addr;
    u_int8_t lun:2;
    u_int8_t rq_seq:6;
  } rs;
} ipmi_lan_msg_rs_t;
#pragma pack(0)
#endif

extern fiid_template_t tmpl_lan_msg_hdr_rq;
extern fiid_template_t tmpl_lan_msg_hdr_rs;
extern fiid_template_t tmpl_lan_msg_trlr;

int8_t fill_lan_msg_hdr (u_int8_t net_fn, u_int8_t rs_lun, u_int8_t rq_seq, fiid_obj_t obj_msg);
int32_t assemble_ipmi_lan_pkt (fiid_obj_t obj_hdr_rmcp, fiid_obj_t obj_hdr_session, fiid_template_t tmpl_hdr_session, fiid_obj_t obj_msg_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, u_int8_t *pkt, u_int32_t pkt_len);
int8_t unassemble_ipmi_lan_pkt (u_int8_t *pkt, u_int32_t pkt_len, fiid_template_t tmpl_hdr_session, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr_rmcp, fiid_obj_t obj_hdr_session, fiid_obj_t obj_msg_hdr, fiid_obj_t obj_cmd, fiid_obj_t obj_msg_trlr);
ssize_t ipmi_lan_sendto (int sockfd, const void *ipmi_lan_pkt, size_t ipmi_lan_pkt_len, int flags, const struct sockaddr *to, unsigned int tolen);
ssize_t ipmi_lan_recvfrom (int sockfd, void *ipmi_pkt, size_t ipmi_pkt_len, int flags, struct sockaddr *from, unsigned int *fromlen);
int8_t ipmi_lan_cmd (u_int32_t sockfd, struct sockaddr *hostaddr, size_t hostaddr_len, u_int8_t auth_type, u_int32_t session_seq_num, u_int32_t session_id, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, u_int8_t net_fn, u_int8_t lun, u_int8_t rq_seq, fiid_obj_t obj_cmd_rq, fiid_template_t tmpl_cmd_rq, fiid_obj_t obj_cmd_rs, fiid_template_t tmpl_cmd_rs);
int8_t ipmi_lan_check_net_fn(fiid_template_t tmpl_msg_hdr, fiid_obj_t obj_msg_hdr, u_int8_t net_fn);
int8_t ipmi_lan_check_rq_seq(fiid_template_t tmpl_msg_hdr, fiid_obj_t obj_msg_hdr, u_int8_t rq_seq);
int8_t ipmi_lan_check_chksum (u_int8_t *pkt, u_int64_t pkt_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-lan-interface.h */


