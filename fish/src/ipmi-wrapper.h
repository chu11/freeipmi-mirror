/* 
   ipmi_wrapper.h: higher level wrapper to libfreeipmi functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#ifndef _IPMI_WRAPPER_H
#define _IPMI_WRAPPER_H


typedef struct channel_info 
{
  u_int8_t channel_number;
  u_int8_t medium_type;
  u_int8_t actual_medium_type;
  u_int8_t protocol_type;
  u_int8_t actual_protocol_type;
} channel_info;

int ipmi_ping (int sockfd, char *hostname, u_int8_t *presence_pong);
int lan_open_session (int sockfd, char *hostname, u_int8_t auth_type, char *username, char *auth_code, u_int8_t priv_level, u_int32_t *session_id, u_int32_t *session_seq_num);
int8_t lan_close_session (int sockfd, char *hostname, u_int8_t auth_type, u_int32_t session_seq_num, u_int32_t session_id, char *auth_code, u_int8_t rq_seq, u_int32_t close_session_id);
int display_get_dev_id (u_int8_t *cmd_rs, u_int32_t cmd_rs_len);

#if 0
int chassis_ctrl (int sockfd, char *hostname, unsigned char auth_type, unsigned char priv_level, char *username, char *passwd, unsigned int session_id, unsigned int inbound_seq_num, unsigned char session_seq_num, unsigned char chassis_ctrl);
#endif

channel_info *get_channel_info_list ();
u_int8_t get_lan_channel_number ();
u_int8_t get_serial_channel_number ();
int display_channel_info ();

#endif
