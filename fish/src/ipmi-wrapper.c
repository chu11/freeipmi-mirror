/* 
   ipmi_wrapper.c: higher level wrapper to libfreeipmi functions
   
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <stdio.h>
#ifdef __FreeBSD__
#include <sys/types.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "freeipmi.h"

#ifdef STDC_HEADERS
#include <string.h>
#endif

#ifdef __FreeBSD__
#include <stdlib.h>
#else
#include <alloca.h>
#endif
#include <netdb.h>

#include "fish.h"
#include "ipmi-wrapper.h"
#include "fi-utils.h"

u_int8_t channel_info_list_initialized = false;
channel_info channel_info_list[8];

static unsigned int rmcp_msg_tag;

static unsigned int 
_get_rmcp_msg_tag (void)
{
  if (rmcp_msg_tag == 0xFE)
    rmcp_msg_tag = 0; /* roll over */

  return (rmcp_msg_tag++);
  /* return (0xFF); */
}

int
ipmi_ping (int sockfd, char *hostname, fiid_obj_t pong)
{
  int status;
  struct sockaddr_in to_addr;
  struct hostent *hostinfo;

  if (hostname == NULL)
      return (1);

  to_addr.sin_family = AF_INET;
  to_addr.sin_port   = htons (RMCP_PRI_RMCP_PORT);
  hostinfo = gethostbyname (hostname);
  if (hostinfo == NULL)
    return (1);
  to_addr.sin_addr   = *(struct in_addr *) hostinfo->h_addr;

  {
    int _msg_tag;
    _msg_tag = _get_rmcp_msg_tag ();
    status = ipmi_rmcp_ping (sockfd, (struct sockaddr *) &to_addr, sizeof (struct sockaddr_in), _msg_tag, pong);

    /*    
    {
      u_int64_t val;
      fiid_obj_get (pong, tmpl_cmd_asf_presence_pong, "msg_tag", &val);
      fprintf (stdout, "msg_tag[Rq:%d, Rs:%qd]n", _msg_tag, val);
    }
    fiid_obj_dump (2, tmpl_cmd_asf_presence_pong, pong);
    */
  }
  return (status);
}

int
lan_open_session (int sockfd, char *hostname, u_int8_t auth_type, char *username, char *auth_code, u_int8_t priv_level, u_int32_t *session_id, u_int32_t *session_seq_num)
{
  struct sockaddr_in to_addr;
  struct hostent *hostinfo;

  if (hostname == NULL)
      return (1);

  to_addr.sin_family = AF_INET;
  to_addr.sin_port   = htons (RMCP_PRI_RMCP_PORT);
  hostinfo = gethostbyname (hostname);
  if (hostinfo == NULL)
    return (1);
  to_addr.sin_addr   = *(struct in_addr *) hostinfo->h_addr;

  if (ipmi_lan_open_session (sockfd, (struct sockaddr *) &to_addr, sizeof (struct sockaddr_in), auth_type, username, auth_code, strlen (auth_code), 0x01, priv_level, session_seq_num, session_id) == -1)
    return (1);
  return (0);
}

int8_t
lan_close_session (int sockfd, char *hostname, u_int8_t auth_type, u_int32_t session_seq_num, u_int32_t session_id, char *auth_code, u_int8_t rq_seq, u_int32_t close_session_id)
{
  struct sockaddr_in to_addr;
  struct hostent *hostinfo;
  fiid_obj_t obj_cmd_rs;

  if (hostname == NULL)
    return (1);

  to_addr.sin_family = AF_INET;
  to_addr.sin_port   = htons (RMCP_PRI_RMCP_PORT);
  hostinfo = gethostbyname (hostname);
  if (hostinfo == NULL)
    return (1);
  to_addr.sin_addr   = *(struct in_addr *) hostinfo->h_addr;

  obj_cmd_rs = fiid_obj_alloc (tmpl_cmd_set_session_priv_level_rs);
  if (!obj_cmd_rs)
    return (1);

  if (ipmi_lan_close_session (sockfd, (struct sockaddr *) &to_addr, sizeof (struct sockaddr_in), auth_type, session_seq_num, session_id, auth_code, strlen (auth_code), rq_seq, close_session_id, obj_cmd_rs) == -1)
    return (-1);
  
  if (IPMI_COMP_CODE(obj_cmd_rs) != IPMI_COMMAND_SUCCESS)
    {
      char errstr[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_cmd_rs, errstr, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "ipmi_close_session_rs: %s\n", errstr);
      fiid_obj_free (obj_cmd_rs);
      return (-1);
    }
  return 0;
}


#if 0
int
chassis_ctrl (int sockfd, char *hostname, unsigned char auth_type, unsigned char priv_level, char *username, char *passwd, unsigned int session_id, unsigned int inbound_seq_num, unsigned char session_seq_num, unsigned char chassis_ctrl)
{
  int status;
  struct sockaddr_in to_addr;
  struct hostent *hostinfo;

  if (hostname == NULL)
      return (1);

  to_addr.sin_family = AF_INET;
  to_addr.sin_port   = htons (RMCP_PRI_RMCP_PORT);
  hostinfo = gethostbyname (hostname);
  if (hostinfo == NULL)
    return (1);
  to_addr.sin_addr   = *(struct in_addr *) hostinfo->h_addr;
  
  /* Chassis Control - Request */
  {
    net_fn_t net_fn;
    ipmi_cmd_chassis_ctrl_rq_t cmd;
    u_int32_t cmd_len;
    void *lan_pkt;
    u_int32_t lan_pkt_len;

    cmd_len = sizeof (ipmi_cmd_set_session_priv_level_rq_t);
    lan_pkt_len = IPMI_LAN_RQ_PKT_SIZE (cmd_len);
    
    lan_pkt = alloca (lan_pkt_len);
    if (lan_pkt == NULL)    /* on error, NULL not returned on all systems */
      return (1);

    net_fn.fn  = IPMI_NET_FN_CHASSIS_RQ;
    net_fn.lun = IPMI_BMC_IPMB_LUN_BMC;

    ipmi_chassis_ctrl_rq (chassis_ctrl, &cmd);

    assemble_ipmi_lan_rq_pkt (IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY, inbound_seq_num, session_id, passwd, IPMI_SESSION_MAX_AUTH_CODE_LEN, net_fn, session_seq_num, &cmd, cmd_len, lan_pkt, lan_pkt_len);
    
    status = ipmi_lan_sendto (sockfd, lan_pkt, lan_pkt_len, 0, (struct sockaddr *)&to_addr, sizeof (struct sockaddr_in));
    if (status == -1) 
      {
	perror ("ipmi_lan_sendto");
	return (1);
      }
  }

  /* Chassis Control - Response */
  {
    struct sockaddr_in from;
    socklen_t fromlen;
    rmcp_hdr_t rmcp_hdr;
    ipmi_session_auth_t session;
    ipmi_lan_msg_rs_t msg;
    ipmi_cmd_chassis_ctrl_rs_t cmd;
    u_int32_t cmd_len;
    ipmi_chksum_t chksum;
    void *lan_pkt;
    u_int32_t lan_pkt_len;

    cmd_len     = sizeof (ipmi_cmd_chassis_ctrl_rs_t);
    lan_pkt_len = IPMI_LAN_RS_PKT_SIZE (cmd_len);
    lan_pkt = alloca (lan_pkt_len);
    if (lan_pkt == NULL)
      return (1);
    
    status = ipmi_lan_recvfrom (sockfd, lan_pkt, lan_pkt_len, 0, (struct sockaddr *)&from, &fromlen);
    if (status == -1)
      {
	perror ("ipmi_lan_recvfrom");
	return (1);
      }

    if (!ipmi_lan_chksum_test (lan_pkt, lan_pkt_len))
      {
	fprintf (stderr, "ipmi_chassis_ctrl_rs: chksum mismatch\n");
	return (-1);
      }

    unassemble_ipmi_lan_rs_pkt (lan_pkt, lan_pkt_len, cmd_len, &rmcp_hdr, &session, &msg, &cmd, &chksum);
    
    if (cmd.comp_code != IPMI_COMMAND_SUCCESS)
      {
	char errstr[IPMI_ERR_STR_MAX_LEN];
	ipmi_strerror_cmd_r (IPMI_CMD_CHASSIS_CTRL, cmd.comp_code, errstr, IPMI_ERR_STR_MAX_LEN);
	fprintf (stderr, "ipmi_chassis_ctrl_rs: %s\n", errstr);
	return (-1);
      }
  }
  return 0;
}
#endif

channel_info *
get_channel_info_list ()
{
  u_int8_t *data_rs = NULL; 
  u_int8_t i;
  u_int8_t ci;
  u_int64_t val;
  
  if (channel_info_list_initialized)
    return (channel_info_list);
  
  data_rs = alloca (fiid_obj_len_bytes (tmpl_get_channel_info_rs));
  
  for (i = 0, ci = 0; i < 8; i++)
    {
      if (ipmi_kcs_get_channel_info (fi_get_sms_io_base (),
				     i,
				     data_rs) != 0)
	continue;
      
      if (IPMI_COMP_CODE(data_rs) != IPMI_COMMAND_SUCCESS)
	continue;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    "actual_channel_number", 
		    &val);
      channel_info_list[ci].channel_number = (u_int8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    "channel_medium_type", 
		    &val);
      channel_info_list[ci].medium_type = 
	channel_info_list[ci].actual_medium_type = (u_int8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    "channel_protocol_type", 
		    &val);
      channel_info_list[ci].protocol_type = 
	channel_info_list[ci].actual_protocol_type = (u_int8_t) val;
      
      if (channel_info_list[ci].actual_medium_type >= 0x0D && 
	  channel_info_list[ci].actual_medium_type <= 0x5F)
	channel_info_list[ci].medium_type = IPMI_CHANNEL_MEDIUM_TYPE_RESERVED;
      
      if (channel_info_list[ci].actual_medium_type >= 0x60 && 
	  channel_info_list[ci].actual_medium_type <= 0x7F)
	channel_info_list[ci].medium_type = IPMI_CHANNEL_MEDIUM_TYPE_OEM;
      
      if (channel_info_list[ci].actual_protocol_type == 0x03 || 
	  (channel_info_list[ci].actual_protocol_type >= 0x0A && 
	   channel_info_list[ci].actual_protocol_type <= 0x1B))
	channel_info_list[ci].protocol_type = IPMI_CHANNEL_PROTOCOL_TYPE_RESERVED;
      
      if (channel_info_list[ci].actual_protocol_type >= 0x1C && 
	  channel_info_list[ci].actual_protocol_type <= 0x1F)
	channel_info_list[ci].protocol_type = IPMI_CHANNEL_PROTOCOL_TYPE_OEM;
      
      ci++;
   }
  channel_info_list_initialized = true;
  return (channel_info_list);
}

u_int8_t 
get_lan_channel_number_orig ()
{
  channel_info *channel_list;
  u_int8_t i;
  
  channel_list = get_channel_info_list ();
  
  for (i = 0; i < 8; i++)
    {
      if (channel_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
	return channel_list[i].channel_number;
    }
  
  return (-1);
}

u_int8_t 
get_serial_channel_number_orig ()
{
  channel_info *channel_list;
  u_int8_t i;
  
  channel_list = get_channel_info_list ();
  
  for (i = 0; i < 8; i++)
    {
      if (channel_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_RS232)
	return channel_list[i].channel_number;
    }
  
  return (-1);
}

u_int8_t 
get_lan_channel_number ()
{
  return 7;
}

u_int8_t 
get_serial_channel_number ()
{
  return 1;
}

int
display_get_dev_id (u_int8_t *cmd_rs, u_int32_t cmd_rs_len)
{
  u_int64_t val;
  if (cmd_rs == NULL) 
      return (-1);

  if (cmd_rs_len < fiid_obj_len_bytes (tmpl_cmd_get_dev_id_rs))
    return (-1);

  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "comp_code", &val);
  if (val != IPMI_KCS_STATUS_SUCCESS)
    {
      char err[IPMI_ERR_STR_MAX_LEN];
      ipmi_kcs_strstatus_r(val, err, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, "bmc-info: %s\n", err);
      return (-1);
    }
  
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "dev_id", &val);
  fprintf (stdout, "Device ID:         %X\n", (unsigned int) val);
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "dev_rev.rev", &val);
  fprintf (stdout, "Device Revision:   %X\n", (unsigned int) val);
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "dev_rev.sdr_support", &val);
  if (val)
    fprintf (stdout, "                   [SDR Support]\n");
  {
    u_int64_t maj, min;
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "firmware_rev1.major_rev", &maj);
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "firmware_rev2.minor_rev", &min);
    fprintf (stdout, "Firmware Revision: %X.%X\n", (unsigned int) maj, (unsigned int) min);
  }
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "firmware_rev1.dev_available", &val);
  if (val == 0)
    fprintf (stdout, "                   [Device Available (normal operation)]\n");
  else
    {
      fprintf (stdout, "                   [Device Not Available]\n");
      fprintf (stdout, "                   [firmware, SDR update or self init in progress]\n");
    }
  {
    u_int64_t ms, ls;
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "ipmi_ver.ms_bits", &ms);
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "ipmi_ver.ls_bits", &ls);
    fprintf (stdout, "IPMI Version:      %X.%X\n", (unsigned int) ms, (unsigned int) ls);
  }
  fprintf (stdout, "Additional Device Support:\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.sensor_dev", &val);
  if(val)
    fprintf (stdout, "                   [Sensor Device]\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.sdr_repo_dev", &val);
  if(val)
    fprintf (stdout, "                   [SDR Repository Device]\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.sel_dev", &val);
  if(val)
    fprintf (stdout, "                   [SEL Device]\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.fru_inventory_dev", &val);
  if(val)
    fprintf (stdout, "                   [FRU Inventory Device]\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.ipmb_evnt_receiver", &val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Receiver]\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.ipmb_evnt_generator", &val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Generator]\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.bridge", &val);
  if(val)
    fprintf (stdout, "                   [Bridge]\n");
  FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "additional_dev_support.chassis_dev", &val);
  if(val)
    fprintf (stdout, "                   [Chassis Device]\n");

  {
    u_int64_t manf_id, prod_id;
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "manf_id.id", &manf_id);
    fprintf (stdout, "Manufacturer ID:   %Xh\n", (unsigned int) manf_id);
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "prod_id", &prod_id);
    fprintf (stdout, "Product ID:        %Xh\n", (unsigned int) prod_id);
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "aux_firmware_rev_info", &val);
    
    switch (manf_id)
      {
      case IPMI_MANF_ID_INTEL: 
	switch (prod_id)
	  {
	  case IPMI_PROD_ID_SR870BN4:
	    {
	      u_int64_t bc_maj, bc_min, pia_maj, pia_min;
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.boot_code.major",
			    &bc_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.boot_code.minor",
			    &bc_min);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.pia.major",
			    &pia_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.pia.minor",
			    &pia_min);
	      fprintf (stdout, "Aux Firmware Revision Info: Boot Code v%02x.%2x, PIA v%02x.%2x\n", (unsigned int) bc_maj, (unsigned int) bc_min, (unsigned int) pia_maj, (unsigned int) pia_min);
	      break;
	    }
	  }
	break;
      default:
	fprintf (stdout, "Aux Firmware Revision Info: %Xh\n", (unsigned int) val);
      }
  }
  return (0);
}
