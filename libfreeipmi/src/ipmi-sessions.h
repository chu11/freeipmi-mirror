/* 
   ipmi-sessions.h - IPMI Session Handler
   
   Copyright (C) 2003 - 2004 FreeIPMI Core Team

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

#ifndef _IPMI_SESSIONS_H
#define	_IPMI_SESSIONS_H

#ifdef __cplusplus
extern "C" {
#endif

#if 0
#pragma pack(1)
typedef struct ipmi_session
{
  u_int8_t auth_type;
  u_int32_t session_seq_num;
  u_int32_t session_id;
  u_int8_t ipmi_msg_len;
} ipmi_session_t;

typedef struct ipmi_session_auth
{
  u_int8_t auth_type;
  u_int32_t session_seq_num;
  u_int32_t session_id;
  u_int8_t auth_code[IPMI_SESSION_MAX_AUTH_CODE_LEN];
  u_int8_t ipmi_msg_len;
} ipmi_session_auth_t;
#pragma pack(0)
#endif

extern fiid_template_t tmpl_hdr_session;
extern fiid_template_t tmpl_hdr_session_auth;
extern fiid_template_t tmpl_hdr_session_auth_calc;

int8_t fill_hdr_session  (fiid_template_t tmpl_session, u_int8_t auth_type, u_int32_t inbound_seq_num, u_int32_t session_id, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr);
int8_t check_hdr_session_session_seq_num (fiid_template_t tmpl_hdr_session, fiid_obj_t obj_hdr_session, u_int32_t session_seq_num);
int8_t check_hdr_session_session_id (fiid_template_t tmpl_hdr_session, fiid_obj_t obj_hdr_session, u_int32_t session_id);
int8_t check_hdr_session_authcode (u_int8_t *pkt, u_int64_t pkt_len, fiid_template_t tmpl_hdr_session, u_int8_t auth_type, u_int8_t *auth_code_data, u_int32_t auth_code_data_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-sessions.h */


