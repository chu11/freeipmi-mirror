/*
ipmi-lan-test.c: Test Utility and Example implementation for LAN driver.
Copyright (C) 2004 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <freeipmi/freeipmi.h>

#define IPMI_HOST   "192.168.1.111"
#define AUTH_TYPE   IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY
#define USERNAME    "admin"
#define PASSWORD    "system"
#define PRIVILEGE_LEVEL IPMI_PRIV_LEVEL_ADMIN

int
main (void)
{
  u_int32_t sockfd = 0;
  int8_t status;
  u_int8_t auth_type;
  u_int32_t session_seq_num;
  u_int32_t session_id;
  u_int32_t auth_code_data_len;
  u_int8_t net_fn;
  u_int8_t lun;
  u_int8_t rq_seq;
  struct hostent *hostinfo;
  struct sockaddr_in host;
  u_int8_t *auth_code_data = PASSWORD;
  u_int32_t initial_outbound_seq_num;
  
  status = auth_type = session_seq_num = session_id = auth_code_data_len = net_fn = lun = 0; 
  rq_seq = 3;
  initial_outbound_seq_num = 1;
  if (auth_code_data)
    auth_code_data_len = strlen (auth_code_data);

  /* Open client (local) UDP socket */
  if ((sockfd = ipmi_open_free_udp_port ()) == -1)
    return (1);
  
  host.sin_family = AF_INET;
  host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
  hostinfo = gethostbyname (IPMI_HOST);
  if (hostinfo == NULL)
    {
      fprintf (stderr, "Unknown host %s.\n", IPMI_HOST);
      exit (EXIT_FAILURE);
    }
  host.sin_addr = *(struct in_addr *) hostinfo->h_addr;

  /* IPMI open LAN session */
  {
    initial_outbound_seq_num = 1;
    if((status = ipmi_lan_open_session (sockfd, (struct sockaddr *) &host, sizeof (struct sockaddr), AUTH_TYPE, USERNAME, PASSWORD,  auth_code_data_len, initial_outbound_seq_num, PRIVILEGE_LEVEL, &session_seq_num, &session_id)) == -1)
      {
	fprintf (stderr, "Error: Open session failed.\n");
	exit (EXIT_FAILURE);
      }
  }
  
  /* IPMI Get Device ID command */
  {
    fiid_obj_t obj_cmd_rq;
    fiid_obj_t obj_cmd_rs;

    obj_cmd_rq = alloca (fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rq));
    memset (obj_cmd_rq, 0, fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rq));
    
    obj_cmd_rs = alloca (fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rs));
    memset (obj_cmd_rs, 0, fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rs));

    fill_cmd_get_dev_id (obj_cmd_rq);

    {
      rq_seq = 0;
      
      status = ipmi_lan_cmd (sockfd, (struct sockaddr *) &host, sizeof (struct sockaddr), AUTH_TYPE, ++session_seq_num, session_id, PASSWORD, auth_code_data_len, IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, rq_seq, obj_cmd_rq, tmpl_cmd_get_dev_id_rq, obj_cmd_rs, tmpl_cmd_get_dev_id_rs);

      IPMI_LAN_RQ_SEQ_INC (rq_seq);
      fiid_obj_dump (fileno(stdout), obj_cmd_rs, tmpl_cmd_get_dev_id_rs);
    }
  }
  
  /* IPMI close LAN session */
  {
    fiid_obj_t obj_cmd_rq;
    fiid_obj_t obj_cmd_rs;

    obj_cmd_rq = alloca (fiid_obj_len_bytes (tmpl_cmd_close_session_rq));
    memset (obj_cmd_rq, 0, fiid_obj_len_bytes (tmpl_cmd_close_session_rq));
    
    obj_cmd_rs = alloca (fiid_obj_len_bytes (tmpl_cmd_close_session_rs));
    memset (obj_cmd_rs, 0, fiid_obj_len_bytes (tmpl_cmd_close_session_rs));

    fill_cmd_close_session (session_id, obj_cmd_rq);
    status = ipmi_lan_close_session (sockfd, (struct sockaddr *)&host, sizeof (struct sockaddr), AUTH_TYPE, ++session_seq_num, session_id, PASSWORD, auth_code_data_len, ++rq_seq, session_id, obj_cmd_rs);
  }
  return (0);
}
