/*****************************************************************************\
 *  $Id: ipmipower_powercmd.c,v 1.46 2006-03-12 20:36:27 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <assert.h>
#include <errno.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "ipmipower.h"
#include "ipmipower_authentication.h"
#include "ipmipower_output.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_packet.h"
#include "ipmipower_privilege.h"
#include "ipmipower_check.h"
#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"

extern cbuf_t ttyout;
extern struct ipmipower_config *conf;

/* Queue of all pending power commands */
static List pending = NULL;

/* _destroy_ipmipower_powercmd
 * - cleanup/destroy an ipmipower_powercmd_t structure stored within a List
 */
static void 
_destroy_ipmipower_powercmd(ipmipower_powercmd_t ip) 
{
  assert(ip != NULL);

  Fiid_obj_destroy(ip->obj_rmcp_hdr_req);
  Fiid_obj_destroy(ip->obj_rmcp_hdr_res);
  Fiid_obj_destroy(ip->obj_lan_session_hdr_req);
  Fiid_obj_destroy(ip->obj_lan_session_hdr_res);
  Fiid_obj_destroy(ip->obj_lan_msg_hdr_req);
  Fiid_obj_destroy(ip->obj_lan_msg_hdr_res);
  Fiid_obj_destroy(ip->obj_lan_msg_trlr_res);
  Fiid_obj_destroy(ip->obj_rmcpplus_session_hdr_req);
  Fiid_obj_destroy(ip->obj_rmcpplus_session_hdr_res);
  Fiid_obj_destroy(ip->obj_rmcpplus_payload_res);
  Fiid_obj_destroy(ip->obj_rmcpplus_session_trlr_req);
  Fiid_obj_destroy(ip->obj_rmcpplus_session_trlr_res);
  Fiid_obj_destroy(ip->obj_authentication_capabilities_v20_req);
  Fiid_obj_destroy(ip->obj_authentication_capabilities_v20_res);
  Fiid_obj_destroy(ip->obj_authentication_capabilities_req);
  Fiid_obj_destroy(ip->obj_authentication_capabilities_res);
  Fiid_obj_destroy(ip->obj_get_session_challenge_req);
  Fiid_obj_destroy(ip->obj_get_session_challenge_res);
  Fiid_obj_destroy(ip->obj_activate_session_req);
  Fiid_obj_destroy(ip->obj_activate_session_res);
  Fiid_obj_destroy(ip->obj_set_session_privilege_req);
  Fiid_obj_destroy(ip->obj_set_session_privilege_res);
  Fiid_obj_destroy(ip->obj_get_channel_cipher_suites_req);
  Fiid_obj_destroy(ip->obj_get_channel_cipher_suites_res);
  Fiid_obj_destroy(ip->obj_open_session_req);
  Fiid_obj_destroy(ip->obj_open_session_res);
  Fiid_obj_destroy(ip->obj_rakp_message_1_req);
  Fiid_obj_destroy(ip->obj_rakp_message_2_res);
  Fiid_obj_destroy(ip->obj_rakp_message_3_req);
  Fiid_obj_destroy(ip->obj_rakp_message_4_res);
  Fiid_obj_destroy(ip->obj_close_session_req);
  Fiid_obj_destroy(ip->obj_close_session_res);
  Fiid_obj_destroy(ip->obj_get_chassis_status_req);
  Fiid_obj_destroy(ip->obj_get_chassis_status_res);
  Fiid_obj_destroy(ip->obj_chassis_control_req);
  Fiid_obj_destroy(ip->obj_chassis_control_res);

  /* Close all sockets that were saved during the Get Session
   * Challenge phase of the IPMI protocol.
   */
  if (list_count(ip->sockets_to_close) > 0) {
    int *fd;
    while ((fd = list_pop(ip->sockets_to_close))) 
      {
	Close(*fd);
	Free(fd);
      }
  }

  list_destroy(ip->sockets_to_close);

  Free(ip);
}

void 
ipmipower_powercmd_setup() 
{
  assert(pending == NULL);  /* need to cleanup first! */
    
  pending = list_create((ListDelF)_destroy_ipmipower_powercmd);
  if (pending == NULL)
    err_exit("list_create() error");
}

void 
ipmipower_powercmd_cleanup() 
{
  assert(pending != NULL);  /* did not run ipmipower_powercmd_setup() */
  list_destroy(pending);
  pending = NULL;
}

void 
ipmipower_powercmd_queue(power_cmd_t cmd, struct ipmipower_connection *ic) 
{ 
  ipmipower_powercmd_t ip;

  assert(pending != NULL);  /* did not run ipmipower_powercmd_setup() */
  assert(ic != NULL);
  assert(POWER_CMD_VALID(cmd));

  ip = (ipmipower_powercmd_t)Malloc(sizeof(struct ipmipower_powercmd));
  memset(ip, '\0', sizeof(struct ipmipower_powercmd));
    
  ip->cmd = cmd;
  ip->protocol_state = PROTOCOL_STATE_START;

  /*
   * Protocol State Machine Variables
   */
  Gettimeofday(&(ip->time_begin), NULL);
  ip->error_occurred = IPMIPOWER_FALSE;
  ip->retry_count = 0;
  ip->close_timeout = 0;

  /*
   * Protocol Maintenance Variables
   */

  /* ip->ipmi_version is set after Get Authentication Capabilities
   * Response stage.
   */

  ip->session_inbound_count = 0;

  /* ip->highest_received_sequence_number is determined
   * after the ipmi_version is determined.
   */
  ip->previously_received_list = 0xFF;

  if (conf->privilege == PRIVILEGE_TYPE_AUTO)
    {
      /* Following are default minimum privileges according to the IPMI
       * specification 
       */
      if (cmd == POWER_CMD_POWER_STATUS)
        ip->privilege = IPMI_PRIVILEGE_LEVEL_USER;
      else
        ip->privilege = IPMI_PRIVILEGE_LEVEL_OPERATOR;
    }
  else
    ip->privilege = ipmipower_ipmi_privilege_type(conf->privilege);

  /* IPMI 1.5 */

#if 0
  if (conf->ipmi_version == IPMI_VERSION_AUTO
      || conf->ipmi_version == IPMI_VERSION_1_5)
    {
      /* ip->permsgauth_enabled is set after the Get Authentication
       * Capabilities Response and/or Activate Session Response is
       * received
       */
      
      /* ip->authentication_type is set after Get Authentication Capabilities
       * Response
       */      
    }
#endif 

  /* IPMI 2.0 */

  if (conf->ipmi_version == IPMI_VERSION_AUTO
      || conf->ipmi_version == IPMI_VERSION_2_0)
    {
      ip->authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
      ip->integrity_algorithm = IPMI_INTEGRITY_ALGORITHM_NONE;
      ip->confidentiality_algorithm = IPMI_CONFIDENTIALITY_ALGORITHM_NONE;
      ip->requested_maximum_privilege = IPMI_PRIVILEGE_LEVEL_ADMIN;
      ip->initial_message_tag = (uint8_t)get_rand();
      ip->message_tag_count = 0;
      ip->session_sequence_number = 0;
      ip->name_only_lookup = IPMI_NAME_ONLY_LOOKUP;
      do 
        {
          ip->remote_console_session_id = get_rand();
        } while (!ip->remote_console_session_id);

      /* Even if this fails, we'll just live with it */
      if (ipmi_get_random(ip->remote_console_random_number, 
                          IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH) < 0)
        {
          dbg("ipmipower_powercmd_queue: ipmi_get_random: %s ",
              strerror(errno));
        }
    }

  ip->ic = ic;

  ip->obj_rmcp_hdr_req = Fiid_obj_create(tmpl_rmcp_hdr); 
  ip->obj_rmcp_hdr_res = Fiid_obj_create(tmpl_rmcp_hdr); 
  ip->obj_lan_session_hdr_req = Fiid_obj_create(tmpl_lan_session_hdr); 
  ip->obj_lan_session_hdr_res = Fiid_obj_create(tmpl_lan_session_hdr); 
  ip->obj_lan_msg_hdr_req = Fiid_obj_create(tmpl_lan_msg_hdr_rq); 
  ip->obj_lan_msg_hdr_res = Fiid_obj_create(tmpl_lan_msg_hdr_rs); 
  ip->obj_lan_msg_trlr_res = Fiid_obj_create(tmpl_lan_msg_trlr); 
  ip->obj_rmcpplus_session_hdr_req = Fiid_obj_create(tmpl_rmcpplus_session_hdr);
  ip->obj_rmcpplus_session_hdr_res = Fiid_obj_create(tmpl_rmcpplus_session_hdr);
  ip->obj_rmcpplus_payload_res = Fiid_obj_create(tmpl_rmcpplus_payload);
  ip->obj_rmcpplus_session_trlr_req = Fiid_obj_create(tmpl_rmcpplus_session_trlr);
  ip->obj_rmcpplus_session_trlr_res = Fiid_obj_create(tmpl_rmcpplus_session_trlr);
  ip->obj_authentication_capabilities_v20_req = Fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_v20_rq); 
  ip->obj_authentication_capabilities_v20_res = Fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_v20_rs); 
  ip->obj_authentication_capabilities_req = Fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_rq); 
  ip->obj_authentication_capabilities_res = Fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_rs); 
  ip->obj_get_session_challenge_req = Fiid_obj_create(tmpl_cmd_get_session_challenge_rq); 
  ip->obj_get_session_challenge_res = Fiid_obj_create(tmpl_cmd_get_session_challenge_rs); 
  ip->obj_activate_session_req = Fiid_obj_create(tmpl_cmd_activate_session_rq); 
  ip->obj_activate_session_res = Fiid_obj_create(tmpl_cmd_activate_session_rs); 
  ip->obj_set_session_privilege_req = Fiid_obj_create(tmpl_cmd_set_session_privilege_level_rq); 
  ip->obj_set_session_privilege_res = Fiid_obj_create(tmpl_cmd_set_session_privilege_level_rs); 
  ip->obj_get_channel_cipher_suites_req = Fiid_obj_create(tmpl_cmd_get_channel_cipher_suites_rq); 
  ip->obj_get_channel_cipher_suites_res = Fiid_obj_create(tmpl_cmd_get_channel_cipher_suites_list_supported_algorithms_rs); 
  ip->obj_open_session_req = Fiid_obj_create(tmpl_rmcpplus_open_session_rq); 
  ip->obj_open_session_res = Fiid_obj_create(tmpl_rmcpplus_open_session_rs); 
  ip->obj_rakp_message_1_req = Fiid_obj_create(tmpl_rmcpplus_rakp_message_1); 
  ip->obj_rakp_message_2_res = Fiid_obj_create(tmpl_rmcpplus_rakp_message_2); 
  ip->obj_rakp_message_3_req = Fiid_obj_create(tmpl_rmcpplus_rakp_message_3); 
  ip->obj_rakp_message_4_res = Fiid_obj_create(tmpl_rmcpplus_rakp_message_4); 
  ip->obj_close_session_req = Fiid_obj_create(tmpl_cmd_close_session_rq); 
  ip->obj_close_session_res = Fiid_obj_create(tmpl_cmd_close_session_rs); 
  ip->obj_get_chassis_status_req = Fiid_obj_create(tmpl_cmd_get_chassis_status_rq); 
  ip->obj_get_chassis_status_res = Fiid_obj_create(tmpl_cmd_get_chassis_status_rs); 
  ip->obj_chassis_control_req = Fiid_obj_create(tmpl_cmd_chassis_control_rq); 
  ip->obj_chassis_control_res = Fiid_obj_create(tmpl_cmd_chassis_control_rs); 

  if ((ip->sockets_to_close = list_create(NULL)) == NULL)
    err_exit("list_create() error");

  list_append(pending, ip);
}

int 
ipmipower_powercmd_pending() 
{
  assert(pending != NULL);  /* did not run ipmipower_powercmd_setup() */
  return !list_is_empty(pending);
}

/* _send_packet
 * - Send a packet of the specified type
 * - updates state and counts
 * - if this is a retransmission, do not update inbound and rqseq
 *   count.  BMC may need to know if this is a retransmission.  Must
 *   increment outbound sequence number, since BMC may increase outbound
 *   sequence number.
 */
static void 
_send_packet(ipmipower_powercmd_t ip, packet_type_t pkt, int is_retry) 
{
  int len = 0;
  char buffer[IPMI_PACKET_BUFLEN];

  assert(PACKET_TYPE_VALID_REQ(pkt));

  /* The following sequence number counts must be set before
   * ipmipower_packet_create, so the same value that is sent can be
   * matched later.
   */
  ip->ic->ipmi_requester_sequence_number_counter++;
  
  if (pkt == PROTOCOL_STATE_OPEN_SESSION_SENT
      || pkt == PROTOCOL_STATE_RAKP_MESSAGE_1_SENT
      || pkt == PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
    ip->message_tag_count++;
  else if (ip->ipmi_version == IPMI_VERSION_2_0
	   && (pkt == CLOSE_SESSION_REQ
	       || pkt == GET_CHASSIS_STATUS_REQ
	       || pkt == CHASSIS_CONTROL_REQ))
    {
      /* IPMI 2.0 is special, sequence numbers of 0 don't count */
      ip->session_sequence_number++;
      if (!ip->session_sequence_number)
        ip->session_sequence_number++;
    }

  len = ipmipower_packet_create(ip, pkt, buffer, IPMI_PACKET_BUFLEN);
  ipmipower_packet_dump(ip, pkt, buffer, len);
  Cbuf_write(ip->ic->ipmi_out, buffer, len);
       
  if (pkt == AUTHENTICATION_CAPABILITIES_V20_REQ)
    ip->protocol_state = PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT;
  else if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
    ip->protocol_state = PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT;
  else if (pkt == GET_SESSION_CHALLENGE_REQ)
    ip->protocol_state = PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT;
  else if (pkt == ACTIVATE_SESSION_REQ) 
    {
      ip->protocol_state = PROTOCOL_STATE_ACTIVATE_SESSION_SENT;

      /* IPMI Workaround (achu)
       *
       * Close all sockets that were saved during the Get Session
       * Challenge phase of the IPMI protocol.  See comments in
       * _retry_packets().
       */
      if (list_count(ip->sockets_to_close) > 0) {
	int *fd;
	while ((fd = list_pop(ip->sockets_to_close))) 
	  {
	    Close(*fd);
	    Free(fd);
	  }
      }
    }
  else if (pkt == SET_SESSION_PRIVILEGE_REQ)
    ip->protocol_state = PROTOCOL_STATE_SET_SESSION_PRIVILEGE_SENT;
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_REQ)
    ip->protocol_state = PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT;
  else if (pkt == OPEN_SESSION_REQ)
    ip->protocol_state = PROTOCOL_STATE_OPEN_SESSION_SENT;
  else if (pkt == RAKP_MESSAGE_1_REQ)
    ip->protocol_state = PROTOCOL_STATE_RAKP_MESSAGE_1_SENT;
  else if (pkt == RAKP_MESSAGE_3_REQ)
    ip->protocol_state = PROTOCOL_STATE_RAKP_MESSAGE_3_SENT;
  else if (pkt == CLOSE_SESSION_REQ)
    ip->protocol_state = PROTOCOL_STATE_CLOSE_SESSION_SENT;
  else if (pkt == GET_CHASSIS_STATUS_REQ)
    ip->protocol_state = PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT;
  else if (pkt == CHASSIS_CONTROL_REQ)
    ip->protocol_state = PROTOCOL_STATE_CHASSIS_CONTROL_SENT;

  /* Session inbound count is incremented after the packet is sent,
   * since the first inbound sequence number is specified by the
   * activate session command.
   */
  if (pkt == SET_SESSION_PRIVILEGE_REQ 
      || (ip->ipmi_version == IPMI_VERSION_1_5
	  && (pkt == CLOSE_SESSION_REQ 
	      || pkt == GET_CHASSIS_STATUS_REQ 
	      || pkt == CHASSIS_CONTROL_REQ)))
    ip->session_inbound_count++;

  Gettimeofday(&(ip->ic->last_ipmi_send), NULL);
}

/* _recv_packet
 * - Receive a packet
 * Returns 1 if packet is of correct size and passes checks
 * Returns 0 if no packet received yet or packet should be ignored
 * Returns -1 if packet returned error
 */
static int 
_recv_packet(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  char recv_buf[IPMI_PACKET_BUFLEN];
  int recv_len = 0;

  assert(PACKET_TYPE_VALID_RES(pkt));

  if (!(recv_len = Cbuf_peek_and_drop(ip->ic->ipmi_in, recv_buf, IPMI_PACKET_BUFLEN)))
    return 0;

  ipmipower_packet_dump(ip, pkt, recv_buf, recv_len);
      
  /* IPMI 1.5 Packet Checks */

  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES 
      || pkt == AUTHENTICATION_CAPABILITIES_RES 
      || pkt == GET_SESSION_CHALLENGE_RES
      || pkt == ACTIVATE_SESSION_RES
      || pkt == SET_SESSION_PRIVILEGE_RES
      || pkt == GET_CHANNEL_CIPHER_SUITES_RES
      || (ip->ipmi_version == IPMI_VERSION_1_5
          && (pkt == GET_CHASSIS_STATUS_RES
              || pkt == CHASSIS_CONTROL_RES
              || pkt == CLOSE_SESSION_RES)))
    {
      /* Return 0 if the packet is unparseable */
      if (ipmipower_packet_store(ip, pkt, recv_buf, recv_len) < 0)
        return 0;

      if (!ipmipower_check_checksum(ip, pkt))
	return 0;

      if (!ipmipower_check_authentication_code(ip, 
					       pkt, 
					       (uint8_t *)recv_buf, 
					       (uint32_t)recv_len))
	return 0;

      if (!ipmipower_check_outbound_sequence_number(ip, pkt))
	return 0;
      
      if (!ipmipower_check_session_id(ip, pkt))
	return 0;

      if (!ipmipower_check_network_function(ip, pkt))
	return 0;

      if (!ipmipower_check_command(ip, pkt))
	return 0;

      if (!ipmipower_check_requester_sequence_number(ip, pkt))
	{
	  if (pkt == CLOSE_SESSION_RES)
	    goto close_session_workaround;
	  return 0;
	}

      /* If everything else is correct besides completion code, packet
       * returned an error.
       */
      if (!ipmipower_check_completion_code(ip, pkt))
	{
	  if (pkt == CLOSE_SESSION_RES)
	    goto close_session_workaround;

          /* Special Case: let _process_ipmi_packets deal with this
	     packet type's output */
          if (pkt != AUTHENTICATION_CAPABILITIES_V20_RES)
            ipmipower_output(ipmipower_packet_errmsg(ip, pkt), ip->ic->hostname);
          
          ip->retry_count = 0;  /* important to reset */
          Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
          ip->error_occurred = IPMIPOWER_TRUE; 
          return -1;
	}
    }
  else /* IPMI 2.0 Packet Checks
          
          (pkt == OPEN_SESSION_RES
           || pkt == RAKP_MESSAGE_2_RES
           || pkt == RAKP_MESSAGE_4_RES
           || (ip->ipmi_version == IPMI_VERSION_2_0
               && (pkt == GET_CHASSIS_STATUS_RES
                   || pkt == CHASSIS_CONTROL_RES
                   || pkt == CLOSE_SESSION_RES)))
        */
    {
      if (ipmipower_packet_store(ip, pkt, recv_buf, recv_len) < 0)
	return 0;

      if (pkt == OPEN_SESSION_RES
	  || pkt == RAKP_MESSAGE_2_RES
	  || pkt == RAKP_MESSAGE_4_RES)
	{
	  if (!ipmipower_check_payload_type(ip, pkt))
	    return 0;

	  if (!ipmipower_check_message_tag(ip, pkt))
	    return 0;

	  /* Unlike IPMI 1.5 completion codes, I don't think there is
	   * a guarantee the data in the RAKP response will have good
	   * authentication codes or session ids if there is a status
	   * code error.  So we check this status code first, then the
	   * other stuff afterwards.
	   */
	  if (!ipmipower_check_rmcpplus_status_code(ip, pkt))
	    {
	      /* XXX output error */
	      return -1;
	    }
	  
	  if (!ipmipower_check_session_id(ip, pkt))
	    return 0;

	  if (pkt == RAKP_MESSAGE_2_RES)
	    {
	      if (!ipmipower_check_rakp_2_key_exchange_authentication_code(ip, pkt))
		return 0;
	    }
	  else if (pkt == RAKP_MESSAGE_4_RES)
	    {
	      if (!ipmipower_check_rakp_4_integrity_check_value(ip, pkt))
		return 0;
	    }
	}
      else /* (ip->ipmi_version == IPMI_VERSION_2_0
               && (pkt == GET_CHASSIS_STATUS_RES
                   || pkt == CHASSIS_CONTROL_RES
                   || pkt == CLOSE_SESSION_RES)) */
	{
	  if (!ipmipower_check_payload_type(ip, pkt))
	    return 0;

	  if (!ipmipower_check_payload_pad(ip, pkt))
	    return 0;

	  if (!ipmipower_check_integrity_pad(ip, pkt))
	    return 0;

	  if (!ipmipower_check_authentication_code(ip, 
						   pkt, 
						   (uint8_t *)recv_buf, 
						   (uint32_t)recv_len))
	    return 0;

	  if (!ipmipower_check_checksum(ip, pkt))
	    return 0;

	  if (!ipmipower_check_outbound_sequence_number(ip, pkt))
	    return 0;
      
	  if (!ipmipower_check_session_id(ip, pkt))
	    return 0;
	  
	  if (!ipmipower_check_network_function(ip, pkt))
	    return 0;
	  
	  if (!ipmipower_check_command(ip, pkt))
	    return 0;
	  
	  if (!ipmipower_check_requester_sequence_number(ip, pkt))
	    {
	      if (pkt == CLOSE_SESSION_RES)
		goto close_session_workaround;
	      return 0;
	    }

	  /* If everything else is correct besides completion code, packet
	   * returned an error.
	   */
	  if (!ipmipower_check_completion_code(ip, pkt))
	    {
	      if (pkt == CLOSE_SESSION_RES)
		goto close_session_workaround;
	      
	      ip->retry_count = 0;  /* important to reset */
	      Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
	      ip->error_occurred = IPMIPOWER_TRUE; 
	      return -1;
	    }
  
	}
    }

  /* Yipee everything passed, the packet is good.  Continue */

  /* achu: If this is the close session response and the packet is
   * mostly legit, go ahead and just accept the packet.  We'll
   * close the session anyways.
   */
 close_session_workaround:
  ip->retry_count = 0;  /* important to reset */
  Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
  return 1;
}

/* _has_timed_out
 * - Check if command timed out
 * Returns 1 if timed out, 0 if not
 */
static int 
_has_timed_out(ipmipower_powercmd_t ip) 
{
  struct timeval cur_time;
    
  Gettimeofday(&cur_time, NULL);

  /* Must use >=, otherwise we could potentially spin */
  if (millisec_diff(&cur_time, &(ip->time_begin)) >= conf->timeout_len) 
    {
      /* Don't bother outputting timeout if we have finished the power control operation */
      if (ip->protocol_state != PROTOCOL_STATE_CLOSE_SESSION_SENT)
        ipmipower_output(MSG_TYPE_TIMEDOUT, ip->ic->hostname);
      return 1;
    }
  
  return 0;
}

/* _retry_packets
 * - Check if we should retransmit and retransmit if necessary
 * Returns 1 if we sent a packet, 0 if not
 */
static int 
_retry_packets(ipmipower_powercmd_t ip) 
{
  struct timeval cur_time, end_time;
  int time_left;
  int retry_timeout_len;

  /* Don't retransmit if any of the following are true */
  if (ip->protocol_state == PROTOCOL_STATE_START /* we haven't started yet */
      || conf->retry_timeout_len == 0             /* no retransmissions */
      || ip->error_occurred == IPMIPOWER_TRUE)   /* we hit an error */
    return 0;

  /* Did we timeout on this packet? */
  Gettimeofday(&cur_time, NULL);
    
  retry_timeout_len = (conf->retry_backoff_count) ? (conf->retry_timeout_len * (1 + (ip->retry_count/conf->retry_backoff_count))) : conf->retry_timeout_len;

  if (millisec_diff(&cur_time, &(ip->ic->last_ipmi_send)) < retry_timeout_len)
    return 0;

  /* Do we have enough time to retransmit? */
  millisec_add(&cur_time, &end_time, conf->timeout_len);
  time_left = millisec_diff(&end_time, &cur_time);
  if (time_left < retry_timeout_len)
    return 0;

  ip->retry_count++;
  dbg("_retry_packets(%s:%d): Sending retry, retry count=%d",
      ip->ic->hostname, ip->protocol_state, ip->retry_count);

  if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT)
    _send_packet(ip, AUTHENTICATION_CAPABILITIES_V20_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT)
    _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT) 
    {
      /* IPMI Workaround (achu)
       *
       * Discovered on Intel Tiger4 (SR870BN4)
       *
       * If the reply from a previous Get Session Challenge request is
       * lost on the network, the following retransmission will make
       * the BMC confused and it will not respond to future packets.
       *
       * The problem seems to exist only when the retransmitted packet
       * is transmitted from the same source port.  Therefore, the fix
       * is to send the retransmission from a different source port.
       * So we'll create a new socket, re-bind to an ephemereal port
       * (guaranteeing us a brand new port), and store this new
       * socket.
       *
       * In the event we need to resend this packet multiple times, we
       * do not want the chance that old ports will be used again.  We
       * store the old file descriptrs (which are bound to the old
       * ports) on a list, and close all of them after we have gotten
       * past the Get Session Challenge phase of the protocol.
       */
      int new_fd, *old_fd;
      struct sockaddr_in srcaddr;

      if ((new_fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
        {
          if (errno != EMFILE)
            lsd_fatal_error(__FILE__, __LINE__, "socket");
          else
            ipmipower_output(MSG_TYPE_RESOURCES, ip->ic->hostname);
          return -1;
        }

      bzero(&srcaddr, sizeof(struct sockaddr_in));
      srcaddr.sin_family = AF_INET;
      srcaddr.sin_port = htons(0);
      srcaddr.sin_addr.s_addr = htonl(INADDR_ANY);
        
      Bind(new_fd, &srcaddr, sizeof(struct sockaddr_in));

      old_fd = (int *)Malloc(sizeof(int));
      *old_fd = ip->ic->ipmi_fd;
      list_push(ip->sockets_to_close, old_fd);

      ip->ic->ipmi_fd = new_fd;

      _send_packet(ip, GET_SESSION_CHALLENGE_REQ, 1);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_ACTIVATE_SESSION_SENT)
    _send_packet(ip, ACTIVATE_SESSION_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_SET_SESSION_PRIVILEGE_SENT)
    _send_packet(ip, SET_SESSION_PRIVILEGE_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT)
    _send_packet(ip, GET_CHANNEL_CIPHER_SUITES_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_OPEN_SESSION_SENT)
    _send_packet(ip, OPEN_SESSION_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
    _send_packet(ip, RAKP_MESSAGE_1_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
    _send_packet(ip, RAKP_MESSAGE_3_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT)
    _send_packet(ip, GET_CHASSIS_STATUS_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_CHASSIS_CONTROL_SENT)
    _send_packet(ip, CHASSIS_CONTROL_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_CLOSE_SESSION_SENT)
    /* 
     * It's pointless to retransmit a close-session.  
     *
     * 1) The power control operation has already completed.
     *
     * 2) There is no guarantee the remote BMC will respond.  If the
     * previous close session response was dropped by the network,
     * then the session has already been closed by the BMC.  Any
     * retransmission will send a session id that is unknown to the
     * BMC, and they will either respond with an error or ignore the
     * packet.
     *
     * _send_packet(ip, CLOSE_SESSION_REQ, 1); 
     */
    ip->close_timeout++;

  return 1;
}

/* _check_authentication_capabilities
 * 
 * Check the contents of a ipmi 1.5 or 2.0 authentication capabilities
 * response.
 *
 * Returns  1 if authentication capabilities should be retried,
 * Returns  0 if authentication passed and the protocol should continue
 * Returns -1 on error
 */
static int
_check_authentication_capabilities(ipmipower_powercmd_t ip,
				   packet_type_t pkt)
{
  uint64_t authentication_type_none, authentication_type_md2, 
    authentication_type_md5, authentication_type_straight_password_key, 
    authentication_status_anonymous_login, authentication_status_null_username, 
    authentication_status_non_null_username,
    authentication_status_per_message_authentication;
  int authentication_type_try_higher_priv = 0;
  fiid_obj_t obj_authentication_capabilities_res;

  assert(pkt == AUTHENTICATION_CAPABILITIES_V20_RES
	 || pkt == AUTHENTICATION_CAPABILITIES_RES);

  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES)
    obj_authentication_capabilities_res = ip->obj_authentication_capabilities_v20_res;
  else
    obj_authentication_capabilities_res = ip->obj_authentication_capabilities_res;

  /* Using results from Get Authentication Capabilities Response,
   * determine:
   *
   * 1) If we are capable of authenticating with the remote host.
   *
   * 2) How to authenticate with the remote host.
   */
  
  Fiid_obj_get(obj_authentication_capabilities_res, 
	       "authentication_type.none", 
	       &authentication_type_none);
  Fiid_obj_get(obj_authentication_capabilities_res, 
	       "authentication_type.md2", 
	       &authentication_type_md2);
  Fiid_obj_get(obj_authentication_capabilities_res, 
	       "authentication_type.md5", 
	       &authentication_type_md5);
  Fiid_obj_get(obj_authentication_capabilities_res, 
	       "authentication_type.straight_password_key", 
	       &authentication_type_straight_password_key);
  Fiid_obj_get(obj_authentication_capabilities_res, 
	       "authentication_status.anonymous_login", 
	       &authentication_status_anonymous_login);
  Fiid_obj_get(obj_authentication_capabilities_res, 
	       "authentication_status.null_username",
	       &authentication_status_null_username);
  Fiid_obj_get(obj_authentication_capabilities_res, 
	       "authentication_status.non_null_username", 
	       &authentication_status_non_null_username);
  Fiid_obj_get(obj_authentication_capabilities_res,
	       "authentication_status.per_message_authentication",
	       &authentication_status_per_message_authentication);

  /* Does the remote BMC's authentication configuration support
   * our username/password combination 
   */
  if ((!strlen(conf->username) && !strlen(conf->password)
       && !authentication_status_anonymous_login
       && !authentication_type_none)
      || (!strlen(conf->username) 
	  && !authentication_status_anonymous_login
	  && !authentication_status_null_username)
      || (strlen(conf->username)
	  && !authentication_status_non_null_username))
    {
#ifndef NDEBUG
      ipmipower_output(MSG_TYPE_USERNAME, ip->ic->hostname);
#else
      ipmipower_output(MSG_TYPE_PERMISSION, ip->ic->hostname);
#endif
      ip->error_occurred = IPMIPOWER_TRUE; 
      return -1;
    }

  if (conf->authentication_type == AUTHENTICATION_TYPE_AUTO)
    {
      /* Choose the best authentication type available.
       * none and null password > md5 > md2 > straight_password_key > none
       */
      if (!strlen(conf->password) && authentication_type_none)
	ip->authentication_type = ipmipower_ipmi_authentication_type(AUTHENTICATION_TYPE_NONE);
      else if (authentication_type_md5)
	ip->authentication_type = ipmipower_ipmi_authentication_type(AUTHENTICATION_TYPE_MD5);
      else if (authentication_type_md2)
	ip->authentication_type = ipmipower_ipmi_authentication_type(AUTHENTICATION_TYPE_MD2);
      else if (authentication_type_straight_password_key)
	ip->authentication_type = ipmipower_ipmi_authentication_type(AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY);
      else if (authentication_type_none)
	ip->authentication_type = ipmipower_ipmi_authentication_type(AUTHENTICATION_TYPE_NONE);
      else if (conf->privilege == PRIVILEGE_TYPE_AUTO)
	{
	  /* achu: It may not seem possible to get to this point
	   * since the check for anonymous_login, null_username,
	   * or non_null_username has passed, but there's a few
	   * ways we can fail. That iffy OEM authentication type
	   * could be enabled (shame on you evil vendor!!) or
	   * authentication at this privilege level isn't allowed.
	   */
	  if (ip->privilege == IPMI_PRIVILEGE_LEVEL_ADMIN)
	    {
	      /* Time to give up */
#ifndef NDEBUG	      
	      ipmipower_output(MSG_TYPE_AUTO, ip->ic->hostname);
#else
	      ipmipower_output(MSG_TYPE_PERMISSION, ip->ic->hostname);
#endif
	      return -1;
	    }
	  else
	    authentication_type_try_higher_priv = 1;
	}
      else
	{
#ifndef NDEBUG	      
	  ipmipower_output(MSG_TYPE_GIVEN_PRIVILEGE, ip->ic->hostname);
#else
	  ipmipower_output(MSG_TYPE_PERMISSION, ip->ic->hostname);
#endif
	  return -1;
	}
    }
  else
    {
      /* Can we authenticate with the user specified
       * authentication type?
       */
      if ((conf->authentication_type == AUTHENTICATION_TYPE_NONE
	   && authentication_type_none)
	  || (conf->authentication_type == AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY 
	      && authentication_type_straight_password_key)
	  || (conf->authentication_type == AUTHENTICATION_TYPE_MD2
	      && authentication_type_md2)
	  || (conf->authentication_type == AUTHENTICATION_TYPE_MD5
	      && authentication_type_md5))
	ip->authentication_type = ipmipower_ipmi_authentication_type(conf->authentication_type);
      else
	{
	  if (ip->privilege == IPMI_PRIVILEGE_LEVEL_ADMIN)
	    {
	      /* Time to give up */
	      ipmipower_output(MSG_TYPE_AUTHENTICATION_TYPE, ip->ic->hostname);
	      return -1;
	    }
	  else
	    authentication_type_try_higher_priv = 1;
	}
    }
         
  /* We can't authenticate with any mechanism for the current
   * privilege level.  But we may able to authenticate at a higher
   * one.  Lets up the privilege level and try again.
   */
  if (authentication_type_try_higher_priv)
    {
      /* Try a higher privilege level */
      if (ip->privilege == IPMI_PRIVILEGE_LEVEL_USER)
	ip->privilege = IPMI_PRIVILEGE_LEVEL_OPERATOR;
      else if (ip->privilege == IPMI_PRIVILEGE_LEVEL_OPERATOR)
	ip->privilege = IPMI_PRIVILEGE_LEVEL_ADMIN;
      else
	err_exit("_check_authentication_privileges: invalid privilege state: %d", 
		 ip->privilege);

      return 1;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on IBM eServer 325
   *
   * The remote BMC ignores if permsg authentiction is enabled
   * or disabled.  So we need to force it no matter what.
   */
  if (!conf->force_permsg_authentication)
    {
      if (!authentication_status_per_message_authentication)
	ip->permsgauth_enabled = IPMIPOWER_TRUE;
      else
	ip->permsgauth_enabled = IPMIPOWER_FALSE;
    }
  else
    ip->permsgauth_enabled = IPMIPOWER_TRUE;

  return 0;
}
					    
/* _process_ipmi_packets
 * - Main function that handles packet sends/receives for
 *   the power control protocol
 * - Returns timeout length, or < 0 if command completed and should
 *   be removed from pending.
 */
static int 
_process_ipmi_packets(ipmipower_powercmd_t ip) 
{
  int rv, timeout; 
  struct timeval cur_time, end_time;

  assert(ip != NULL);
  assert(PROTOCOL_STATE_VALID(ip->protocol_state));

  /* if timeout, give up */
  if (_has_timed_out(ip))
    return -1;
    
  /* retransmit? */ 
  if ((rv = _retry_packets(ip)) != 0) 
    { 
      if (rv < 0)
        return -1;
      goto done;
    }

  if (ip->protocol_state == PROTOCOL_STATE_START)
    {
      if (conf->ipmi_version == IPMI_VERSION_AUTO
          || conf->ipmi_version == IPMI_VERSION_2_0)
	_send_packet(ip, AUTHENTICATION_CAPABILITIES_V20_REQ, 0);
      else
	_send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT)
    {
      uint64_t ipmi_v20_extended_capabilities_available, 
	channel_supports_ipmi_v15_connections,
	channel_supports_ipmi_v20_connections;

      /* If the remote machine does not support IPMI 2.0, we move onto
       * IPMI 1.5 protocol respectively appropriately.
       */

      if ((rv = _recv_packet(ip, AUTHENTICATION_CAPABILITIES_V20_RES)) != 1) 
        {
          if (rv < 0) 
	    {
              if (conf->ipmi_version == IPMI_VERSION_AUTO
                  || conf->ipmi_version == IPMI_VERSION_2_0)
                {
                  uint64_t comp_code;
                  
                  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res, 
                               "comp_code", 
                               &comp_code);
                  
                  dbg("_process_ipmi_packets(%s:%d): bad comp_code on "
                      "authentication capabilities 2.0: %x", 
                      ip->ic->hostname, ip->protocol_state, comp_code);
                  
                  if (comp_code == IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD)
                    {
                      if (conf->ipmi_version == IPMI_VERSION_AUTO)
                        {
                          /* Try the IPMI 1.5 version of Get Authentication Capabilities */
                          ip->error_occurred = IPMIPOWER_FALSE; 
                          _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ, 0);
                          goto done;
                        }
                      else
                        ipmipower_output(MSG_TYPE_VERSION_NOT_SUPPORTED, ip->ic->hostname); 
                    }
                  else
                    ipmipower_output(ipmipower_packet_errmsg(ip, 
                                                             AUTHENTICATION_CAPABILITIES_V20_RES), 
                                     ip->ic->hostname);
                }
	      return -1;
	    }
          goto done;
        }

      Fiid_obj_get(ip->obj_authentication_capabilities_v20_res,
		   "authentication_type.ipmi_v2.0_extended_capabilities_available",
		   &ipmi_v20_extended_capabilities_available);
      Fiid_obj_get(ip->obj_authentication_capabilities_v20_res,
		   "channel_supports_ipmi_v1.5_connections",
		   &channel_supports_ipmi_v15_connections);
      Fiid_obj_get(ip->obj_authentication_capabilities_v20_res,
		   "channel_supports_ipmi_v2.0_connections",
		   &channel_supports_ipmi_v20_connections);

      /* If we can't detect IPMI 1.5 with
       * 'channel_supports_ipmi_v15_connections', we assume its a bug,
       * and try the IPMI 1.5 version of Get Authentication
       * Capabilities if appropriate.
       */
      if (conf->ipmi_version == IPMI_VERSION_AUTO)
        {
          if (ipmi_v20_extended_capabilities_available
              && !channel_supports_ipmi_v15_connections
              && !channel_supports_ipmi_v20_connections)
            _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ, 0);
          else if (!ipmi_v20_extended_capabilities_available
                   || (channel_supports_ipmi_v15_connections
                       && !channel_supports_ipmi_v20_connections))
            {
              int check;
              
              if ((check = _check_authentication_capabilities(ip, 
                                                              AUTHENTICATION_CAPABILITIES_V20_RES)) < 0)
                return -1;
              
              if (check)
                {
                  /* Don't consider this a retransmission */
                  _send_packet(ip, AUTHENTICATION_CAPABILITIES_V20_REQ, 0);
                  goto done;
                }
              /* else we continue with the IPMI 1.5 protocol */
              
              ip->ipmi_version = IPMI_VERSION_1_5;
	      ip->highest_received_sequence_number = IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER;
              _send_packet(ip, GET_SESSION_CHALLENGE_REQ, 0);
            }
          else
            {
              ip->ipmi_version = IPMI_VERSION_2_0;
	      ip->highest_received_sequence_number = IPMIPOWER_RMCPPLUS_INITIAL_OUTBOUND_SEQUENCE_NUMBER;
              _send_packet(ip, GET_CHANNEL_CIPHER_SUITES_REQ, 0);
            }
        }
      else if (conf->ipmi_version == IPMI_VERSION_1_5)
        err_exit("_process_ipmi_packets: invalid ipmi_version: %d", conf->ipmi_version);
      else if (conf->ipmi_version == IPMI_VERSION_2_0)
	{
          if (!ipmi_v20_extended_capabilities_available
              || !channel_supports_ipmi_v20_connections)
            {
              ipmipower_output(MSG_TYPE_VERSION_NOT_SUPPORTED, ip->ic->hostname); 
              return -1;
            }

          ip->ipmi_version = IPMI_VERSION_2_0;
	  ip->highest_received_sequence_number = IPMIPOWER_RMCPPLUS_INITIAL_OUTBOUND_SEQUENCE_NUMBER;
          _send_packet(ip, GET_CHANNEL_CIPHER_SUITES_REQ, 0);
	}
    }
  else if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT) 
    {
      int check_val;

      if ((rv = _recv_packet(ip, AUTHENTICATION_CAPABILITIES_RES)) != 1) 
        {
          if (rv < 0) 
            return -1;
          goto done;
        }

      if ((check_val = _check_authentication_capabilities(ip, 
							  AUTHENTICATION_CAPABILITIES_RES)) < 0)
	return -1;
      
      if (check_val)
	{
	  /* Don't consider this a retransmission */
	  _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ, 0);
          goto done;
        }
      /* else we continue with the IPMI 1.5 protocol */

      ip->ipmi_version = IPMI_VERSION_1_5;
      ip->highest_received_sequence_number = IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER;
      _send_packet(ip, GET_SESSION_CHALLENGE_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT) 
    {
      if ((rv = _recv_packet(ip, GET_SESSION_CHALLENGE_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      _send_packet(ip, ACTIVATE_SESSION_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_ACTIVATE_SESSION_SENT) 
    {
      uint64_t authentication_type;

      if ((rv = _recv_packet(ip, ACTIVATE_SESSION_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      Fiid_obj_get(ip->obj_activate_session_res,
		   "authentication_type",
		   &authentication_type);

      if (!conf->force_permsg_authentication)
	{
	  if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
	    {
	      if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
		{
		  dbg("_process_ipmi_packets(%s:%d): not none authentcation",
		      ip->ic->hostname, ip->protocol_state);
		  ip->retry_count = 0;  /* important to reset */
		  Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
		  ip->error_occurred = IPMIPOWER_TRUE; 
		  /* XXX Session is not up, is it ok to quit here?  Or
		   * should we timeout?? */
		  return -1;
		}
	    }
	  else
	    {
	      if (authentication_type != ip->authentication_type)
		{
		  dbg("_process_ipmi_packets(%s:%d): authentication_type mismatch",
		      ip->ic->hostname, ip->protocol_state);
		  ip->retry_count = 0;  /* important to reset */
		  Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
		  ip->error_occurred = IPMIPOWER_TRUE; 
		  /* XXX Session is not up, is it ok to quit here?  Or
		   * should we timeout?? */
		  return -1;
		}
	    }
	}
      
      /* We can skip SET_SESSION_PRIVILEGE_REQ on a power status
       * check, because the default IPMI session privilege level is
       * the user privilege level
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS)
        _send_packet(ip, GET_CHASSIS_STATUS_REQ, 0);
      else
        _send_packet(ip, SET_SESSION_PRIVILEGE_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_SET_SESSION_PRIVILEGE_SENT) 
    {
      if ((rv = _recv_packet(ip, SET_SESSION_PRIVILEGE_RES)) != 1) 
        {
          if (rv < 0) 
            /* Session is up, so close it */
            _send_packet(ip, CLOSE_SESSION_REQ, 0);
          goto done;
        }

      /* Next packet we send depends on the power command and the
       * options set.  The POWER_CMD_POWER_STATUS command shouldn't be
       * possible at this point (see comments above under
       * protocol_state == PROTOCOL_STATE_ACTIVATE_SESSION_SENT), but
       * we leave the code below anyway.
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS
          || (conf->on_if_off 
              && (ip->cmd == POWER_CMD_POWER_CYCLE
                  || ip->cmd == POWER_CMD_POWER_RESET)))
        _send_packet(ip, GET_CHASSIS_STATUS_REQ, 0);
      else
        _send_packet(ip, CHASSIS_CONTROL_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT)
    {
      if ((rv = _recv_packet(ip, GET_CHANNEL_CIPHER_SUITES_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      /* XXX IPMI 2.0 TODO
       *
       * Check if user input of auth/intg/conf algorithm is legit and supportable
       *
       * Support multiple iterations
       */

      _send_packet(ip, OPEN_SESSION_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_OPEN_SESSION_SENT)
    {
      if ((rv = _recv_packet(ip, OPEN_SESSION_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      _send_packet(ip, RAKP_MESSAGE_1_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
    {
      if ((rv = _recv_packet(ip, RAKP_MESSAGE_2_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      _send_packet(ip, RAKP_MESSAGE_3_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
    {
      if ((rv = _recv_packet(ip, RAKP_MESSAGE_4_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      /* Next packet we send depends on the power command and the
       * options set.
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS
          || (conf->on_if_off 
              && (ip->cmd == POWER_CMD_POWER_CYCLE
                  || ip->cmd == POWER_CMD_POWER_RESET)))
        _send_packet(ip, GET_CHASSIS_STATUS_REQ, 0);
      else
        _send_packet(ip, CHASSIS_CONTROL_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT) 
    {
      uint64_t power_state;
      
      if ((rv = _recv_packet(ip, GET_CHASSIS_STATUS_RES)) != 1) 
        {
          if (rv < 0)  
            /* Session is up, so close it */
            _send_packet(ip, CLOSE_SESSION_REQ, 0);
          goto done;
        }

      Fiid_obj_get(ip->obj_get_chassis_status_res, 
                   "current_power_state.power_is_on",
                   &power_state);

      if (ip->cmd == POWER_CMD_POWER_STATUS) 
        {
          ipmipower_output((power_state) ? MSG_TYPE_ON : MSG_TYPE_OFF, 
                           ip->ic->hostname); 
          _send_packet(ip, CLOSE_SESSION_REQ, 0);
        }
      else if (conf->on_if_off && (ip->cmd == POWER_CMD_POWER_CYCLE
                                  || ip->cmd == POWER_CMD_POWER_RESET)) 
        {
          if (!power_state) 
            {
              /* This is now a power-on operation */
              ip->cmd = POWER_CMD_POWER_ON;
            }
          _send_packet(ip, CHASSIS_CONTROL_REQ, 0);
        }
      else
        err_exit("_process_ipmi_packets: invalid command state: %d", ip->cmd);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_CHASSIS_CONTROL_SENT) 
    {
      if ((rv = _recv_packet(ip, CHASSIS_CONTROL_RES)) != 1) 
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet(ip, CLOSE_SESSION_REQ, 0);
          goto done;
        }
        
      ipmipower_output(MSG_TYPE_OK, ip->ic->hostname);

      /* Typically there is no response from the IPMI close command if
       * the POWER_CMD_POWER_RESET power control command is
       * successful.  So just skip the close session.
       */
      if (ip->cmd == POWER_CMD_POWER_RESET)
        goto finish_up;
      else
        _send_packet(ip, CLOSE_SESSION_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_CLOSE_SESSION_SENT) 
    {
      /* achu: Note that it's possible we're timing out too early and
       * the close session response will still arrive.  It's no
       * matter.  If we are in non-interactive mode, the file
       * descriptor will be closed and the packet lost.  If we are in
       * interactive mode, the next power control command will call
       * 'ipmipower_connection_clear' and get rid of the packet if it
       * is sitting on a buffer.
       */
      if (ip->close_timeout)
        {
          dbg("_process_ipmi_packets: close session timeout, skip retransmission");
          goto finish_up;
        }

      if (!_recv_packet(ip, CLOSE_SESSION_RES)) 
        goto done;
 
      /* Regardless of packet error or success, finish up */
    finish_up:
      ip->protocol_state = PROTOCOL_STATE_END;
      return -1; /* don't goto done and calculate timeout */
    }
  else
    err_exit("_process_ipmi_packets: invalid state: %d", ip->protocol_state);

 done:
  Gettimeofday(&cur_time, NULL);
  millisec_add(&(ip->time_begin), &end_time, conf->timeout_len);
  timeout = millisec_diff(&end_time, &cur_time);

  /* shorter timeout b/c of retransmission timeout */
  if (conf->retry_timeout_len) 
    {
      int retry_timeout_len = (conf->retry_backoff_count) ? (conf->retry_timeout_len * (1 + (ip->retry_count/conf->retry_backoff_count))) : conf->retry_timeout_len;
      if (timeout > retry_timeout_len)
        timeout = retry_timeout_len;
    }

  return timeout;
}

int 
ipmipower_powercmd_process_pending(int *timeout)
{
  ListIterator itr;
  ipmipower_powercmd_t ip;
  int max_timeout = 0;
  int num_pending;

  assert(pending != NULL);  /* did not run ipmipower_powercmd_setup() */
  assert(timeout != NULL);

  /* if there are no pending jobs, don't edit the timeout */
  if (list_is_empty(pending))
    return 0;

  itr = list_iterator_create(pending);
  while ((ip = (ipmipower_powercmd_t)list_next(itr))) 
    {
      int tmp_timeout = -1;

      if ((tmp_timeout = _process_ipmi_packets(ip)) < 0) 
        {
          if (list_delete(itr) == 0)
            err_exit("ipmipower_powercmd_process_pending: list_delete");
          continue;
        }

      if (tmp_timeout > max_timeout)
        max_timeout = tmp_timeout;
    }
  list_iterator_destroy(itr);

  if ((num_pending = list_count(pending)) == 0) 
    ipmipower_output_finish();
  
  *timeout = max_timeout;
  return num_pending;
}
