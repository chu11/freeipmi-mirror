/*****************************************************************************\
 *  $Id: ipmipower_powercmd.c,v 1.122 2008-03-28 00:14:47 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
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
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */

#include "ipmipower.h"
#include "ipmipower_authentication_type.h"
#include "ipmipower_cipher_suite_id.h"
#include "ipmipower_output.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_packet.h"
#include "ipmipower_privilege_level.h"
#include "ipmipower_check.h"
#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"

extern cbuf_t ttyout;
extern struct ipmipower_config *conf;

/* Queue of all pending power commands */
static List pending = NULL;

/* The following are the ranking of Cipher Suite IDs we will consider to
 * be from most to least secure.  This was determined by the following 
 * critera (X > Y means we consider X more secure than Y).
 *
 * Authentication Algorithms:
 * RAKP-HMAC-SHA1 > RAKP-HMAC-MD5
 * RAKP-HMAC-MD5 > NONE
 *
 * Integrity Algorithms:
 * HMAC-SHA1-96 > HMAC-MD5-128
 * HMAC-MD5-128 > MD5-128
 * MD5-128 > NONE
 *
 * Confidentiality Algorithms: 
 * AES-CBC-128 > NONE
 *
 */
static uint8_t cipher_suite_id_ranking[] =
  {
    3,                 /* RAKP-HMAC-SHA1, HMAC-SHA1-96, AES-CBC-128 */
    8,                 /* RAKP-HMAC-MD5, HMAC-MD5-128, AES-CBC-128 */
    12,                /* RAKP-HMAC-MD5, MD5-128, AES-CBC-128 */
    2,                 /* RAKP-HMAC-SHA1, HMAC-SHA1-96, NONE */
    7,                 /* RAKP-HMAC-MD5, HMAC-MD5-128, NONE */
    11,                /* RAKP-HMAC-MD5, MD5-128, NONE */
    1,                 /* RAKP-HMAC-SHA1, NONE, NONE */
    6,                 /* RAKP-HMAC-MD5, NONE, NONE */
    0,                 /* NONE, NONE, NONE */
  };
static unsigned int cipher_suite_id_ranking_count = 9;

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
  Fiid_obj_destroy(ip->obj_get_channel_cipher_suites_req);
  Fiid_obj_destroy(ip->obj_get_channel_cipher_suites_res);
  Fiid_obj_destroy(ip->obj_open_session_req);
  Fiid_obj_destroy(ip->obj_open_session_res);
  Fiid_obj_destroy(ip->obj_rakp_message_1_req);
  Fiid_obj_destroy(ip->obj_rakp_message_2_res);
  Fiid_obj_destroy(ip->obj_rakp_message_3_req);
  Fiid_obj_destroy(ip->obj_rakp_message_4_res);
  Fiid_obj_destroy(ip->obj_set_session_privilege_level_req);
  Fiid_obj_destroy(ip->obj_set_session_privilege_level_res);
  Fiid_obj_destroy(ip->obj_get_chassis_status_req);
  Fiid_obj_destroy(ip->obj_get_chassis_status_res);
  Fiid_obj_destroy(ip->obj_chassis_control_req);
  Fiid_obj_destroy(ip->obj_chassis_control_res);
  Fiid_obj_destroy(ip->obj_close_session_req);
  Fiid_obj_destroy(ip->obj_close_session_res);

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

static void
_init_ipmi_2_0_randomized_data(ipmipower_powercmd_t ip)
{
  assert(ip);

  /* In IPMI 2.0, session_ids of 0 are special */
  do 
    {
      ip->remote_console_session_id = get_rand();
    } while (!ip->remote_console_session_id);

  /* Even if this fails, we'll just live with it */
  if (ipmi_get_random(ip->remote_console_random_number, 
                      IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH) < 0)
    err_output("ipmipower_powercmd_queue: ipmi_get_random: %s ",
               strerror(errno));
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
  ip->retransmission_count = 0;
  ip->close_timeout = 0;

  /*
   * Protocol Maintenance Variables
   */

  /* ip->ipmi_version is set after Get Authentication Capabilities
   * Response stage.
   */

  ip->session_inbound_count = 0;

  /* initial ip->highest_received_sequence_number is determined after
   * the ipmi_version is determined.
   */
  ip->previously_received_list = 0xFF;

  if (conf->privilege_level == PRIVILEGE_LEVEL_AUTO)
    {
      /* Following are default minimum privileges according to the IPMI
       * specification 
       */
      if (cmd == POWER_CMD_POWER_STATUS)
        ip->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
      else
        ip->privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
    }
  else
    ip->privilege_level = ipmipower_ipmi_privilege_level(conf->privilege_level);

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
#endif /* 0 */

  /* IPMI 2.0 */

  if (conf->ipmi_version == IPMI_VERSION_AUTO
      || conf->ipmi_version == IPMI_VERSION_2_0)
    {
      if (conf->cipher_suite_id != CIPHER_SUITE_ID_AUTO)
        {
          ip->cipher_suite_id = ipmipower_ipmi_cipher_suite_id(conf->cipher_suite_id);
          if (ipmi_cipher_suite_id_to_algorithms(ip->cipher_suite_id,
                                                 &(ip->authentication_algorithm),
                                                 &(ip->integrity_algorithm),
                                                 &(ip->confidentiality_algorithm)) < 0)
            err_exit("ipmipower_powercmd_queue: ipmi_cipher_suite_id_to_algorithms: ",
                     "conf->cipher_suite_id: %d; cipher_suite_id: %d; %s",
                     conf->cipher_suite_id, ip->cipher_suite_id, strerror(errno));
        }     
      /* IPMI Workaround (achu)
       *
       * Discovered on SE7520AF2 with Intel Server Management Module
       * (Professional Edition)
       *
       * The Intel's return IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL instead
       * of an actual privilege.
       */
      if (conf->workaround_flags & WORKAROUND_FLAG_INTEL_2_0_SESSION)
	ip->requested_maximum_privilege_level = ip->privilege_level;
      else
	ip->requested_maximum_privilege_level = IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL;
      memset(ip->sik_key, '\0', IPMI_MAX_SIK_KEY_LENGTH);
      ip->sik_key_ptr = ip->sik_key;
      ip->sik_key_len = IPMI_MAX_SIK_KEY_LENGTH;
      memset(ip->integrity_key, '\0', IPMI_MAX_INTEGRITY_KEY_LENGTH);
      ip->integrity_key_ptr = ip->integrity_key;
      ip->integrity_key_len = IPMI_MAX_INTEGRITY_KEY_LENGTH;
      memset(ip->confidentiality_key, '\0', IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH);
      ip->confidentiality_key_ptr = ip->confidentiality_key;
      ip->confidentiality_key_len = IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH;

      ip->initial_message_tag = (uint8_t)get_rand();
      ip->message_tag_count = 0;
      ip->session_sequence_number = 0;
      ip->name_only_lookup = IPMI_NAME_ONLY_LOOKUP;
      _init_ipmi_2_0_randomized_data(ip);

      ip->cipher_suite_list_index = 0;
      memset(ip->cipher_suite_record_data, '\0', IPMI_CIPHER_SUITE_RECORD_DATA_BUFFER_LENGTH);
      ip->cipher_suite_record_data_bytes = 0;
      memset(ip->cipher_suite_ids, '\0', IPMI_CIPHER_SUITE_IDS_LENGTH);
      ip->cipher_suite_ids_num = 0;
      ip->wait_until_on_state = 0;
      ip->wait_until_off_state = 0;
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
  ip->obj_get_channel_cipher_suites_req = Fiid_obj_create(tmpl_cmd_get_channel_cipher_suites_rq); 
  ip->obj_get_channel_cipher_suites_res = Fiid_obj_create(tmpl_cmd_get_channel_cipher_suites_rs); 
  ip->obj_open_session_req = Fiid_obj_create(tmpl_rmcpplus_open_session_request); 
  ip->obj_open_session_res = Fiid_obj_create(tmpl_rmcpplus_open_session_response); 
  ip->obj_rakp_message_1_req = Fiid_obj_create(tmpl_rmcpplus_rakp_message_1); 
  ip->obj_rakp_message_2_res = Fiid_obj_create(tmpl_rmcpplus_rakp_message_2); 
  ip->obj_rakp_message_3_req = Fiid_obj_create(tmpl_rmcpplus_rakp_message_3); 
  ip->obj_rakp_message_4_res = Fiid_obj_create(tmpl_rmcpplus_rakp_message_4); 
  ip->obj_set_session_privilege_level_req = Fiid_obj_create(tmpl_cmd_set_session_privilege_level_rq); 
  ip->obj_set_session_privilege_level_res = Fiid_obj_create(tmpl_cmd_set_session_privilege_level_rs); 
  ip->obj_get_chassis_status_req = Fiid_obj_create(tmpl_cmd_get_chassis_status_rq); 
  ip->obj_get_chassis_status_res = Fiid_obj_create(tmpl_cmd_get_chassis_status_rs); 
  ip->obj_chassis_control_req = Fiid_obj_create(tmpl_cmd_chassis_control_rq); 
  ip->obj_chassis_control_res = Fiid_obj_create(tmpl_cmd_chassis_control_rs); 
  ip->obj_close_session_req = Fiid_obj_create(tmpl_cmd_close_session_rq); 
  ip->obj_close_session_res = Fiid_obj_create(tmpl_cmd_close_session_rs); 

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
_send_packet(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  int len = 0;
  char buffer[IPMI_PACKET_BUFLEN];

  assert(PACKET_TYPE_VALID_REQ(pkt));

  /* The following sequence number counts must be set before
   * ipmipower_packet_create, so the same value that is sent can be
   * matched later.
   */
  ip->ic->ipmi_requester_sequence_number_counter++;
  
  if (pkt == OPEN_SESSION_REQ
      || pkt == RAKP_MESSAGE_1_REQ
      || pkt == RAKP_MESSAGE_3_REQ)
    ip->message_tag_count++;
  else if (ip->ipmi_version == IPMI_VERSION_2_0
	   && (pkt == SET_SESSION_PRIVILEGE_LEVEL_REQ
	       || pkt == GET_CHASSIS_STATUS_REQ
	       || pkt == CHASSIS_CONTROL_REQ
	       || pkt == CLOSE_SESSION_REQ))
    {
      /* IPMI 2.0 is special, sequence numbers of 0 don't count */
      ip->session_sequence_number++;
      if (!ip->session_sequence_number)
        ip->session_sequence_number++;
    }

  len = ipmipower_packet_create(ip, pkt, buffer, IPMI_PACKET_BUFLEN);
  ipmipower_packet_dump(ip, pkt, buffer, len);
  Cbuf_write(ip->ic->ipmi_out, buffer, len);
  secure_memset(buffer, '\0', IPMI_PACKET_BUFLEN);

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
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_REQ)
    ip->protocol_state = PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT;
  else if (pkt == OPEN_SESSION_REQ)
    ip->protocol_state = PROTOCOL_STATE_OPEN_SESSION_SENT;
  else if (pkt == RAKP_MESSAGE_1_REQ)
    ip->protocol_state = PROTOCOL_STATE_RAKP_MESSAGE_1_SENT;
  else if (pkt == RAKP_MESSAGE_3_REQ)
    ip->protocol_state = PROTOCOL_STATE_RAKP_MESSAGE_3_SENT;
  else if (pkt == SET_SESSION_PRIVILEGE_LEVEL_REQ)
    ip->protocol_state = PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT;
  else if (pkt == GET_CHASSIS_STATUS_REQ)
    ip->protocol_state = PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT;
  else if (pkt == CHASSIS_CONTROL_REQ)
    ip->protocol_state = PROTOCOL_STATE_CHASSIS_CONTROL_SENT;
  else if (pkt == CLOSE_SESSION_REQ)
    ip->protocol_state = PROTOCOL_STATE_CLOSE_SESSION_SENT;

  /* Session inbound count is incremented after the packet is sent,
   * since the first inbound sequence number is specified by the
   * activate session command.
   */
  if (ip->ipmi_version == IPMI_VERSION_1_5
      && (pkt == SET_SESSION_PRIVILEGE_LEVEL_REQ 
	  || pkt == GET_CHASSIS_STATUS_REQ 
	  || pkt == CHASSIS_CONTROL_REQ
	  || pkt == CLOSE_SESSION_REQ))
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
  int rv = -1;

  assert(PACKET_TYPE_VALID_RES(pkt));

  if (!(recv_len = Cbuf_peek_and_drop(ip->ic->ipmi_in, recv_buf, IPMI_PACKET_BUFLEN)))
    return 0;

  ipmipower_packet_dump(ip, pkt, recv_buf, recv_len);
      
  /* IPMI 1.5 Packet Checks */

  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES 
      || pkt == AUTHENTICATION_CAPABILITIES_RES 
      || pkt == GET_SESSION_CHALLENGE_RES
      || pkt == ACTIVATE_SESSION_RES
      || (ip->ipmi_version == IPMI_VERSION_1_5
          && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
	      || pkt == GET_CHASSIS_STATUS_RES
              || pkt == CHASSIS_CONTROL_RES
              || pkt == CLOSE_SESSION_RES)))
    {
      /* rv = 0 if the packet is unparseable */
      if (ipmipower_packet_store(ip, pkt, recv_buf, recv_len) < 0)
	{
	  rv = 0;
	  goto cleanup;
	}

      if (!ipmipower_check_checksum(ip, pkt))
	{
	  rv = 0;
	  goto cleanup;
	}

      if (!ipmipower_check_authentication_code(ip, 
					       pkt, 
					       (uint8_t *)recv_buf, 
					       (uint32_t)recv_len))
	{
	  rv = 0;
	  goto cleanup;
	}

      if (!ipmipower_check_outbound_sequence_number(ip, pkt))
	{
	  rv = 0;
	  goto cleanup;
	}
      
      if (!ipmipower_check_session_id(ip, pkt))
	{
	  rv = 0;
	  goto cleanup;
	}

      if (!ipmipower_check_network_function(ip, pkt))
	{
	  rv = 0;
	  goto cleanup;
	}

      if (!ipmipower_check_command(ip, pkt))
	{
	  rv = 0;
	  goto cleanup;
	}

      if (!ipmipower_check_requester_sequence_number(ip, pkt))
	{
	  if (pkt == CLOSE_SESSION_RES)
	    goto close_session_workaround;
	  rv = 0;
	  goto cleanup;
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
          
          ip->retransmission_count = 0;  /* important to reset */
          Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
	  goto cleanup;
	}
    }
  else /* IPMI 2.0 Packet Checks
          
          (pkt == GET_CHANNEL_CIPHER_SUITES_RES
           || pkt == OPEN_SESSION_RES
           || pkt == RAKP_MESSAGE_2_RES
           || pkt == RAKP_MESSAGE_4_RES
           || (ip->ipmi_version == IPMI_VERSION_2_0
               && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
	           || pkt == GET_CHASSIS_STATUS_RES
                   || pkt == CHASSIS_CONTROL_RES
                   || pkt == CLOSE_SESSION_RES)))
        */
    {
      if (ipmipower_packet_store(ip, pkt, recv_buf, recv_len) < 0)
	{
	  rv = 0;
	  goto cleanup;
	}

      if (pkt == OPEN_SESSION_RES
	  || pkt == RAKP_MESSAGE_2_RES
	  || pkt == RAKP_MESSAGE_4_RES)
	{
	  if (!ipmipower_check_payload_type(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }

	  if (!ipmipower_check_message_tag(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }

	  /* I don't think there is a guarantee the data
	   * (i.e. authentication keys, session id's, etc.) in the
	   * RAKP response will be valid if there is a status code
	   * error.  So we check this status code first, then the
	   * other stuff afterwards.
	   */
	  if (!ipmipower_check_rmcpplus_status_code(ip, pkt))
	    {
              /* Special Case: let _process_ipmi_packets deal with this
                 packet type's output */
              if (pkt != OPEN_SESSION_RES)
                ipmipower_output(ipmipower_packet_errmsg(ip, pkt), ip->ic->hostname);

	      ip->retransmission_count = 0;  /* important to reset */
	      Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
	      goto cleanup;
	    }
	  
	  if (!ipmipower_check_session_id(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }

          if (pkt == OPEN_SESSION_RES)
            {
	      /* achu: In this case, we need to return an error and
	       * let the state machine use it's retry logic.
	       */
              if (!ipmipower_check_open_session_response_privilege(ip, pkt))
		goto cleanup;
            }
	  else if (pkt == RAKP_MESSAGE_2_RES)
	    {
	      if (!ipmipower_check_rakp_2_key_exchange_authentication_code(ip, pkt))
		{
		  ipmipower_output(MSG_TYPE_PASSWORD_INVALID, ip->ic->hostname); 
		  goto cleanup;
		}
	    }
	  else if (pkt == RAKP_MESSAGE_4_RES)
	    {
	      if (!ipmipower_check_rakp_4_integrity_check_value(ip, pkt))
		{
		  ipmipower_output(MSG_TYPE_K_G_INVALID, ip->ic->hostname); 
		  goto cleanup;
		}
	    }
	}
      else /* (pkt == pkt == GET_CHANNEL_CIPHER_SUITES_RES
              || (ip->ipmi_version == IPMI_VERSION_2_0
                  && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
                      || pkt == GET_CHASSIS_STATUS_RES
                      || pkt == CHASSIS_CONTROL_RES
                      || pkt == CLOSE_SESSION_RES))) */
	{
	  if (!ipmipower_check_payload_type(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }

	  if (!ipmipower_check_payload_pad(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }

	  if (!ipmipower_check_integrity_pad(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }

	  if (!ipmipower_check_checksum(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }

	  if (!ipmipower_check_authentication_code(ip, 
						   pkt, 
						   (uint8_t *)recv_buf, 
						   (uint32_t)recv_len))
	    {
	      rv = 0;
	      goto cleanup;
	    }

	  if (!ipmipower_check_outbound_sequence_number(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }
      
	  if (!ipmipower_check_session_id(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }
	  
	  if (!ipmipower_check_network_function(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }
	  
	  if (!ipmipower_check_command(ip, pkt))
	    {
	      rv = 0;
	      goto cleanup;
	    }
	  
	  if (!ipmipower_check_requester_sequence_number(ip, pkt))
	    {
	      if (pkt == CLOSE_SESSION_RES)
		goto close_session_workaround;
	      rv = 0;
	      goto cleanup;
	    }

	  /* If everything else is correct besides completion code, packet
	   * returned an error.
	   */
	  if (!ipmipower_check_completion_code(ip, pkt))
	    {
	      if (pkt == CLOSE_SESSION_RES)
		goto close_session_workaround;
	      
	      ip->retransmission_count = 0;  /* important to reset */
	      Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
	      goto cleanup;
	    }
	}
    }

  /* Yipee everything passed, the packet is good.  Continue */

  /* achu: If this is the close session response and the packet is
   * mostly legit, go ahead and just accept the packet.  We'll
   * close the session anyways.
   */
 close_session_workaround:
  ip->retransmission_count = 0;  /* important to reset */
  Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
  rv = 1;

 cleanup:
  /* Clear out data */
  secure_memset(recv_buf, '\0', IPMI_PACKET_BUFLEN);
  Fiid_obj_clear(ip->obj_lan_session_hdr_res);
  Fiid_obj_clear(ip->obj_rmcpplus_session_trlr_res);
  return rv;
}

/* _has_timed_out
 * - Check if command timed out
 * Returns 1 if timed out, 0 if not
 */
static int 
_has_timed_out(ipmipower_powercmd_t ip) 
{
  struct timeval cur_time, result;
  unsigned int session_timeout_len;
    
  Gettimeofday(&cur_time, NULL);
  timeval_sub(&cur_time, &(ip->time_begin), &result);
  timeval_millisecond_calc(&result, &session_timeout_len);

  /* Must use >=, otherwise we could potentially spin */
  if (session_timeout_len >= conf->session_timeout_len) 
    {
      /* Don't bother outputting timeout if we have finished the power
         control operation */
      if (ip->protocol_state != PROTOCOL_STATE_CLOSE_SESSION_SENT)
        {
          /* Special cases: these are probably due to bad passwords */
          if (ip->protocol_state == PROTOCOL_STATE_ACTIVATE_SESSION_SENT)
            ipmipower_output(MSG_TYPE_PASSWORD_VERIFICATION_TIMEOUT, ip->ic->hostname);
          else
            ipmipower_output(MSG_TYPE_SESSION_TIMEOUT, ip->ic->hostname);
        }
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
  struct timeval cur_time, end_time, result;
  unsigned int time_since_last_ipmi_send;
  unsigned int time_left;
  unsigned int retransmission_timeout_len;

  /* Don't retransmit if any of the following are true */
  if (ip->protocol_state == PROTOCOL_STATE_START /* we haven't started yet */
      || conf->retransmission_timeout_len == 0             /* no retransmissions */
      || (((conf->wait_until_on == IPMIPOWER_TRUE
            && ip->cmd == POWER_CMD_POWER_ON)
           || (conf->wait_until_off == IPMIPOWER_TRUE
               && ip->cmd == POWER_CMD_POWER_OFF))
	  && conf->retransmission_wait_timeout_len == 0))
    return 0;

  /* Did we timeout on this packet? */
  if ((conf->wait_until_on == IPMIPOWER_TRUE
       && ip->cmd == POWER_CMD_POWER_ON)
      || (conf->wait_until_off == IPMIPOWER_TRUE
          && ip->cmd == POWER_CMD_POWER_OFF))
    retransmission_timeout_len = (conf->retransmission_backoff_count) ? (conf->retransmission_wait_timeout_len * (1 + (ip->retransmission_count/conf->retransmission_backoff_count))) : conf->retransmission_wait_timeout_len;
  else
    retransmission_timeout_len = (conf->retransmission_backoff_count) ? (conf->retransmission_timeout_len * (1 + (ip->retransmission_count/conf->retransmission_backoff_count))) : conf->retransmission_timeout_len;

  Gettimeofday(&cur_time, NULL);
  timeval_sub(&cur_time, &(ip->ic->last_ipmi_send), &result);
  timeval_millisecond_calc(&result, &time_since_last_ipmi_send);

  if (time_since_last_ipmi_send < retransmission_timeout_len)
    return 0;

  /* Do we have enough time to retransmit? */
  timeval_add_ms(&cur_time, conf->session_timeout_len, &end_time);
  timeval_sub(&end_time, &cur_time, &result);
  timeval_millisecond_calc(&result, &time_left);
  if (time_left < retransmission_timeout_len)
    return 0;

  ip->retransmission_count++;
  dbg("_retry_packets(%s:%d): Sending retry, retry count=%d",
      ip->ic->hostname, ip->protocol_state, ip->retransmission_count);

  if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT)
    _send_packet(ip, AUTHENTICATION_CAPABILITIES_V20_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT)
    _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ);
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

      _send_packet(ip, GET_SESSION_CHALLENGE_REQ);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_ACTIVATE_SESSION_SENT)
    _send_packet(ip, ACTIVATE_SESSION_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT)
    _send_packet(ip, GET_CHANNEL_CIPHER_SUITES_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_OPEN_SESSION_SENT)
    _send_packet(ip, OPEN_SESSION_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
    _send_packet(ip, RAKP_MESSAGE_1_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
    _send_packet(ip, RAKP_MESSAGE_3_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT)
    _send_packet(ip, SET_SESSION_PRIVILEGE_LEVEL_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT)
    _send_packet(ip, GET_CHASSIS_STATUS_REQ);
  else if (ip->protocol_state == PROTOCOL_STATE_CHASSIS_CONTROL_SENT)
    _send_packet(ip, CHASSIS_CONTROL_REQ);
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
     * _send_packet(ip, CLOSE_SESSION_REQ); 
     */
    ip->close_timeout++;

  return 1;
}

/* _check_ipmi_1_5_authentication_capabilities
 * 
 * Check the contents of a ipmi 1.5 or 2.0 authentication capabilities
 * response.
 *
 * Returns  1 if authentication capabilities should be retried,
 * Returns  0 if authentication passed and the protocol should continue
 * Returns -1 on ipmi protocol error or discovery error
 */
static int
_check_ipmi_1_5_authentication_capabilities(ipmipower_powercmd_t ip,
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

  /* IPMI Workaround (achu)
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * The ASUS motherboard reports incorrect settings of anonymous
   * vs. null vs non-null username capabilities. The workaround is to
   * skip these checks.
   */
  if (!(conf->workaround_flags & WORKAROUND_FLAG_AUTHENTICATION_CAPABILITIES))
    {
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
          ipmipower_output(MSG_TYPE_USERNAME_INVALID, ip->ic->hostname);
          return -1;
        }
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
      else if (conf->privilege_level == PRIVILEGE_LEVEL_AUTO)
	{
	  /* achu: It may not seem possible to get to this point
	   * since the check for anonymous_login, null_username,
	   * or non_null_username has passed, but there's a few
	   * ways we can fail. That iffy OEM authentication type
	   * could be enabled (shame on you evil vendor!!) or
	   * authentication at this privilege level isn't allowed.
	   */
	  if (ip->privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN)
	    {
	      /* Time to give up */
	      ipmipower_output(MSG_TYPE_1_5_AUTO, ip->ic->hostname);
	      return -1;
	    }
	  else
	    authentication_type_try_higher_priv = 1;
	}
      else
	{
	  ipmipower_output(MSG_TYPE_AUTHENTICATION_TYPE_UNAVAILABLE, ip->ic->hostname);	
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
	  if (ip->privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN)
	    {
	      /* Time to give up */
	      ipmipower_output(MSG_TYPE_AUTHENTICATION_TYPE_UNAVAILABLE, ip->ic->hostname);	
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
      if (ip->privilege_level == IPMI_PRIVILEGE_LEVEL_USER)
	ip->privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
      else if (ip->privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR)
	ip->privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
      else
	err_exit("_check_authentication_privileges: invalid privilege state: %d", 
		 ip->privilege_level);

      return 1;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on IBM eServer 325
   *
   * The remote BMC ignores if permsg authentiction is enabled
   * or disabled.  So we need to force it no matter what.
   */
  if (!(conf->workaround_flags & WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION))
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
					    
/* _check_ipmi_2_0_authentication_capabilities
 * 
 * Check the contents of a ipmi 2.0 authentication capabilities response.
 *
 * Returns  0 if authentication passed
 * Returns -1 on ipmi protocol error
 */
static int
_check_ipmi_2_0_authentication_capabilities(ipmipower_powercmd_t ip)
{
  uint64_t authentication_status_anonymous_login, authentication_status_null_username, 
    authentication_status_non_null_username, authentication_status_k_g;

  /* Using results from Get Authentication Capabilities Response,
   * determine:
   *
   * 1) If we are capable of authenticating with the remote host.
   *
   * 2) How to authenticate with the remote host.
   */
  
  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res, 
	       "authentication_status.anonymous_login", 
	       &authentication_status_anonymous_login);
  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res, 
	       "authentication_status.null_username",
	       &authentication_status_null_username);
  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res, 
	       "authentication_status.non_null_username", 
	       &authentication_status_non_null_username);
  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res,
	       "authentication_status.k_g",
	       &authentication_status_k_g);

  /* IPMI Workaround (achu)
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * The ASUS motherboard reports incorrect settings of anonymous
   * vs. null vs non-null username capabilities.  The workaround is to
   * skip these checks.
   *
   * Discovered on an ASUS P5MT-R motherboard 
   *
   * K_g status is reported incorrectly too.  Again, skip the checks.
   */
  if (!(conf->workaround_flags & WORKAROUND_FLAG_AUTHENTICATION_CAPABILITIES))
    {
      /* Does the remote BMC's authentication configuration support
       * our username/password combination 
       */
      if ((!strlen(conf->username) && !strlen(conf->password)
           && !authentication_status_anonymous_login)
          || (!strlen(conf->username) 
              && !authentication_status_anonymous_login
              && !authentication_status_null_username)
          || (strlen(conf->username)
              && !authentication_status_non_null_username))
        {
          ipmipower_output(MSG_TYPE_USERNAME_INVALID, ip->ic->hostname); 
          return -1;
        }
      
      if ((!conf->k_g_len && authentication_status_k_g)
          || (conf->k_g_len && !authentication_status_k_g))
        {
          ipmipower_output(MSG_TYPE_K_G_INVALID, ip->ic->hostname);	
          return -1;
        }
    }

  return 0;
}

/* _check_ipmi_2_0_authentication_capabilities_errors
 * 
 * Check if there is a legitimate ipmi 2.0 authentication capabilities
 * response error.
 *
 * Returns  0 if IPMI 1.5 authentication capabilities should be retried,
 * Returns -1 on ipmi protocol error error
 */
static int
_check_ipmi_2_0_authentication_capabilities_error(ipmipower_powercmd_t ip)
{
  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT);

  if (conf->ipmi_version == IPMI_VERSION_AUTO
      || conf->ipmi_version == IPMI_VERSION_2_0)
    {
      uint64_t comp_code;
      
      Fiid_obj_get(ip->obj_authentication_capabilities_v20_res, 
                   "comp_code", 
                   &comp_code);
      
      dbg("_check_ipmi_2_0_authentication_capabilities_error(%s:%d): "
          "bad comp_code in authentication capabilities 2.0: %x", 
          ip->ic->hostname, ip->protocol_state, comp_code);
      
      if (comp_code == IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD)
        {
          if (conf->ipmi_version == IPMI_VERSION_AUTO)
            return 0;
          else
            {
              ipmipower_output(MSG_TYPE_IPMI_2_0_UNAVAILABLE, ip->ic->hostname); 
              return -1;
            }
        }
    }
  
  ipmipower_output(ipmipower_packet_errmsg(ip, AUTHENTICATION_CAPABILITIES_V20_RES), 
                   ip->ic->hostname);
  return -1;
}

/* _check_ipmi_version_support
 * 
 * Check for IPMI 2.0 support 
 *
 * Returns 0 on success and flags set in ipmi_1_5 and ipmi_2_0
 * Returns -1 on error
 */
static int
_check_ipmi_version_support(ipmipower_powercmd_t ip, int *ipmi_1_5, int *ipmi_2_0)
{
  uint64_t ipmi_v20_extended_capabilities_available, 
    channel_supports_ipmi_v15_connections,
    channel_supports_ipmi_v20_connections;
  
  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT);
  assert(ipmi_1_5);
  assert(ipmi_2_0);

  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res,
               "authentication_type.ipmi_v2.0_extended_capabilities_available",
               &ipmi_v20_extended_capabilities_available);
  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res,
               "channel_supports_ipmi_v1.5_connections",
               &channel_supports_ipmi_v15_connections);
  Fiid_obj_get(ip->obj_authentication_capabilities_v20_res,
               "channel_supports_ipmi_v2.0_connections",
               &channel_supports_ipmi_v20_connections);
  

  if (!ipmi_v20_extended_capabilities_available)
    {
      *ipmi_1_5 = 1;
      *ipmi_2_0 = 0;
    }
  else
    {
      if (channel_supports_ipmi_v15_connections)
	*ipmi_1_5 = 1;
      else
	*ipmi_1_5 = 0;
      
      if (channel_supports_ipmi_v20_connections)
	*ipmi_2_0 = 1;
      else
	*ipmi_2_0 = 0;
    }

  return 0;
}

/* _check_activate_session_authentication_type
 * 
 * Check if the activate session response has the appropriate
 * authentication type.
 *
 * Returns  0 if yes
 * Returns -1 if no, which is an ipmi protocol error
 */
static int
_check_activate_session_authentication_type(ipmipower_powercmd_t ip)
{
  uint64_t authentication_type;

  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_ACTIVATE_SESSION_SENT);

  Fiid_obj_get(ip->obj_activate_session_res,
               "authentication_type",
               &authentication_type);
  
  if (conf->workaround_flags & WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION)
    return 0;

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro H8QME with SIMSO daughter card.
   *
   * (Note: This could work for "IBM eServer 325" per msg auth
   * problem.  But I don't have hardware to test it :-()
   *
   * The remote BMC ignores if permsg authentiction is disabled.
   * Handle it appropriately by just not doing permsg authentication.
   */
  if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
    {
      if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
        {
          dbg("_process_ipmi_packets(%s:%d): not none authentcation",
              ip->ic->hostname, ip->protocol_state);
          ip->permsgauth_enabled = IPMIPOWER_TRUE;
        }
    }

  if (ip->permsgauth_enabled == IPMIPOWER_TRUE)
    {
      if (authentication_type != ip->authentication_type)
        {
          dbg("_process_ipmi_packets(%s:%d): authentication_type mismatch",
              ip->ic->hostname, ip->protocol_state);
          ipmipower_output(MSG_TYPE_BMC_ERROR, ip->ic->hostname);
          
          ip->retransmission_count = 0;  /* important to reset */
          Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
          return -1;
        }
    }

  return 0;
}

/* _calculate_cipher_suite_ids
 * 
 * Determine the cipher suite ids the remote machine supports.
 *
 * Returns  0 on success
 * Returns -1 on ipmi protocol error
 */
static int
_calculate_cipher_suite_ids(ipmipower_powercmd_t ip)
{
  fiid_obj_t obj_cipher_suite_record_header = NULL;
  fiid_obj_t obj_cipher_suite_record = NULL;
  fiid_obj_t obj_oem_cipher_suite_record = NULL;
  int bytes_parsed = 0;
  int rv = -1;

  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT);

  if (!ip->cipher_suite_record_data_bytes)
    {
      dbg("_calculate_cipher_suite_ids(%s:%d): "
          "No cipher suite data records retrieved",
          ip->ic->hostname, ip->protocol_state);
      goto cleanup;
    }
  
  obj_cipher_suite_record_header = Fiid_obj_create(tmpl_cipher_suite_record_header);
  obj_cipher_suite_record = Fiid_obj_create(tmpl_cipher_suite_record);
  obj_oem_cipher_suite_record = Fiid_obj_create(tmpl_oem_cipher_suite_record);

  while (bytes_parsed < ip->cipher_suite_record_data_bytes)
    {
      uint64_t record_format, tag_bits;
      int32_t len;
      
      Fiid_obj_clear(obj_cipher_suite_record_header);

      len = Fiid_obj_set_all(obj_cipher_suite_record_header,
                             ip->cipher_suite_record_data + bytes_parsed,
                             ip->cipher_suite_record_data_bytes - bytes_parsed);

      Fiid_obj_get(obj_cipher_suite_record_header,
                   "tag_bits",
                   &tag_bits);

      Fiid_obj_get(obj_cipher_suite_record_header,
                   "record_format",
                   &record_format);

      if (tag_bits != IPMI_CIPHER_SUITE_TAG_BITS_RECORD)
        {
          dbg("_calculate_cipher_suite_ids(%s:%d): "
              "invalid tag bits: %x",
              ip->ic->hostname, ip->protocol_state, (uint8_t)tag_bits);

          /* IPMI Workaround (achu)
           *
           * Discovered on Sun Fire 4100.
           *
           * The tag bits for some of the cipher records are wrong, but
           * not until the good ones have been parsed.  So just break out
           * if we had bad ones.
           */
          if (conf->workaround_flags & WORKAROUND_FLAG_SUN_2_0_SESSION)
            break;

          ipmipower_output(MSG_TYPE_BMC_ERROR, ip->ic->hostname);
          goto cleanup;
        }

      if (!IPMI_CIPHER_SUITE_RECORD_FORMAT_VALID(record_format))
        {
          dbg("_calculate_cipher_suite_ids(%s:%d): "
              "invalid record format: %x",
              ip->ic->hostname, ip->protocol_state, (uint8_t)record_format);
          ipmipower_output(MSG_TYPE_BMC_ERROR, ip->ic->hostname);
          goto cleanup;
        }

      if (record_format == IPMI_CIPHER_SUITE_RECORD_FORMAT_STANDARD)
        {
          uint64_t cipher_suite_id;
          
          Fiid_obj_clear(obj_cipher_suite_record);
          len = Fiid_obj_set_all(obj_cipher_suite_record,
                                 ip->cipher_suite_record_data + bytes_parsed,
                                 ip->cipher_suite_record_data_bytes - bytes_parsed);
          
          /* If the record is short (and we're likely at the end of
           * the buffer), this could fail, so we can't use a wrapper
           * function
           */
          if (!(fiid_obj_get(obj_cipher_suite_record,
                             "cipher_suite_id",
                             &cipher_suite_id) < 0))
            {
              ip->cipher_suite_ids[ip->cipher_suite_ids_num] = (uint8_t)cipher_suite_id;
              ip->cipher_suite_ids_num++;
            }
          
          bytes_parsed += len;
        }
      else
        {
          /* achu: We currently don't support OEM ciphers, so don't
             store any cipher ids unless we're assuming they're
             actually non-OEM.
          */
          Fiid_obj_clear(obj_oem_cipher_suite_record);
          len = Fiid_obj_set_all(obj_oem_cipher_suite_record,
                                 ip->cipher_suite_record_data + bytes_parsed,
                                 ip->cipher_suite_record_data_bytes - bytes_parsed);
          bytes_parsed += len;
        }
    }

  rv = 0;
 cleanup:
  if (obj_cipher_suite_record_header)
    Fiid_obj_destroy(obj_cipher_suite_record_header);
  if (obj_cipher_suite_record)
    Fiid_obj_destroy(obj_cipher_suite_record);
  if (obj_oem_cipher_suite_record)
    Fiid_obj_destroy(obj_oem_cipher_suite_record);
  return (rv);
}

/* _store_and_calculate_cipher_suite_ids
 * 
 * Store data from a get channel cipher suite ids response and calculate
 * the cipher suite ids.
 *
 * Returns  1 if more data should be retrieved
 * Returns  0 on success and all record data received and cipher ids have been calculated
 * Returns -1 on ipmi protocol error
 */
static int
_store_and_calculate_cipher_suite_ids(ipmipower_powercmd_t ip)
{
  int32_t record_len;

  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT);

  record_len = Fiid_obj_get_data(ip->obj_get_channel_cipher_suites_res,
                                 "cipher_suite_record_data",
                                 ip->cipher_suite_record_data + ip->cipher_suite_record_data_bytes,
                                 IPMI_CIPHER_SUITE_RECORD_DATA_BUFFER_LENGTH - ip->cipher_suite_record_data_bytes);
  ip->cipher_suite_record_data_bytes += record_len;

  /* In the IPMI 2.0 spec, Table 22-17 - Get Channel Cipher Suites
   * Command, if the record data returned from the BMC is == 16,
   * then there is more data to retrieve.  So we increment the
   * list index and get more data.
   * 
   * If the return is short, we resend, otherwise we got all the data
   * we care about.
   */
  if (record_len == IPMI_CIPHER_SUITE_RECORD_DATA_LENGTH
      && ip->cipher_suite_record_data_bytes < IPMI_CIPHER_SUITE_RECORD_DATA_BUFFER_LENGTH)
    return 1;

  /* Else, we got all the data we care about, so lets convert the
   * records into usable information.
   */
  if (_calculate_cipher_suite_ids(ip) < 0)
    return -1;

  return 0;
}

/* _determine_cipher_suite_id_to_use
 * 
 * Determine which cipher suite id to use
 *
 * Returns  0 if we found something to try
 * Returns -1 on ipmi protocol error or can't find a cipher id to try
 */
static int
_determine_cipher_suite_id_to_use(ipmipower_powercmd_t ip)
{
  int i, j, cipher_suite_found = 0;
  
  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT
	 || ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT);

  /* Ok, if the cipher suite has been specified by the user, then
   * lets make sure that the remote machine supports this cipher
   * suite id.
   */
  if (conf->cipher_suite_id != CIPHER_SUITE_ID_AUTO)
    {         
      for (i = 0; i < ip->cipher_suite_ids_num; i++)
        {
          if (ip->cipher_suite_id == ip->cipher_suite_ids[i])
            {
              cipher_suite_found++;
              break;
            }
        }
      
      if (!cipher_suite_found)
        {
          dbg("_determine_cipher_suite_id_to_use(%s:%d): "
              " cipher suite not found: %x",
              ip->ic->hostname, ip->protocol_state, ip->cipher_suite_id);
          ipmipower_output(MSG_TYPE_CIPHER_SUITE_ID_UNAVAILABLE, ip->ic->hostname); 
          return -1;
        }
    }
  else
    {
      /* If the user wants us to find a cipher suite id, lets find
       * a cipher suite we can use.  Otherwise, we report to the
       * user that we're screwed.
       */
      
      for (i = 0; i < cipher_suite_id_ranking_count; i++)
        {
          for (j = 0; j < ip->cipher_suite_ids_num; j++)
            {
              if (cipher_suite_id_ranking[i] == ip->cipher_suite_ids[j])
                {
                  cipher_suite_found++;
                  break;
                }
            }
          
          if (cipher_suite_found)
            break;
        }
      
      if (!cipher_suite_found)
        {
          dbg("_determine_cipher_suite_id_to_use(%s:%d): "
              " can't find usable cipher suite",
              ip->ic->hostname, ip->protocol_state);
          ipmipower_output(MSG_TYPE_2_0_AUTO, ip->ic->hostname); 
          return -1;
        }
      
      ip->cipher_suite_id = cipher_suite_id_ranking[i];
      ip->cipher_suite_id_ranking_index = i;
      if (ipmi_cipher_suite_id_to_algorithms(ip->cipher_suite_id,
                                                 &(ip->authentication_algorithm),
                                             &(ip->integrity_algorithm),
                                             &(ip->confidentiality_algorithm)) < 0)
        err_exit("_determine_cipher_suite_id_to_use: ipmi_cipher_suite_id_to_algorithms: ",
                 "cipher_suite_id: %d; %s",
                 ip->cipher_suite_id, strerror(errno));
    }

  return 0;
}

/* _check_open_session_error
 * 
 * Determine if a legit error occurred, and if so which new cipher suite id to use
 *
 * Returns  0 if we found something to try, so resend
 * Returns -1 on ipmi protocol error or can't find a cipher id to try
 */
static int
_check_open_session_error(ipmipower_powercmd_t ip)
{
  uint64_t rmcpplus_status_code;
  uint64_t maximum_privilege_level;
  int priv_check = 0;

  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_OPEN_SESSION_SENT);

  /* 
   * IPMI Workaround (achu)
   *
   * Sigh.  There are two interpretations of the IPMI 2.0 Spec.
   *
   * Interpretation #1:
   *
   * Cipher Suite IDs (and thus authentication mechanisms) are not
   * attached to specific privilege levels.  Cipher Suite IDs are
   * assigned a privilege level limit.  So if we cannot connect at a
   * lower privilege, there is no need to see if we can connect at a
   * higher privilege.
   *
   * Interpretation #2:
   *
   * Cipher Suite Ids (and thus authentication algorithms) are
   * attached to specific privilege levels.  You can authenticate only
   * at that privilege level.
   *
   * In other words, the interpretations are nearly opposite of each other.
   * 
   * Well, when the privilege is auto detected, we send the "request
   * highest privilege" flag in the open session request.  This should
   * be enough to work around both interpretations.
   *
   * Sigh ... 
   */
  
  Fiid_obj_get(ip->obj_open_session_res, 
	       "rmcpplus_status_code", 
	       &rmcpplus_status_code);
  
  Fiid_obj_get(ip->obj_open_session_res,
	       "maximum_privilege_level",
	       &maximum_privilege_level);

  /* A rmcpplus status error takes precedence over a privilege error */
  if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS)
    {
      if (ip->requested_maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL
	  && conf->privilege_level == PRIVILEGE_LEVEL_AUTO)
	{
	  if (ip->cmd == POWER_CMD_POWER_STATUS)
	    {
	      if (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_USER
		  || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
		  || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN)
		priv_check = 1;
	      else
		priv_check = 0;
	    }
	  else
	    {
	      if (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
		  || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN)
		priv_check = 1;
	      else
		priv_check = 0;
	    }
	}
      else
	priv_check = (maximum_privilege_level == ip->requested_maximum_privilege_level) ? 1 : 0;
      
      if (conf->cipher_suite_id != CIPHER_SUITE_ID_AUTO 
	  && !priv_check)
	{
	  ipmipower_output(MSG_TYPE_NECESSARY_PRIVILEGE_LEVEL, ip->ic->hostname);	
	  return -1;
	}
    }

  if (conf->cipher_suite_id == CIPHER_SUITE_ID_AUTO)
    {
      int i, j, cipher_suite_found = 0;
    
      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_CIPHER_SUITE_MATCH_WITH_PROPOSED_SECURITY_ALGORITHMS
	  || !priv_check)
        {
          /* Lets try the next Cipher Suite ID if there is one */
          if (ip->cipher_suite_id_ranking_index == (cipher_suite_id_ranking_count - 1))
            {
              ipmipower_output(MSG_TYPE_2_0_AUTO, ip->ic->hostname);
              return -1;
            }
          else
            {
              /* Lets find the next cipher suite we can use.  Otherwise,
               * we report to the user that we're screwed.
               */
              for (i = ip->cipher_suite_id_ranking_index + 1; i < cipher_suite_id_ranking_count; i++)
                {
                  for (j = 0; j < ip->cipher_suite_ids_num; j++)
                    {
                      if (cipher_suite_id_ranking[i] == ip->cipher_suite_ids[i])
                        {
                          cipher_suite_found++;
                          break;
                        }
                    }
                  
                  if (cipher_suite_found)
                    break;
                }
              
              if (!cipher_suite_found)
                {
                  ipmipower_output(MSG_TYPE_2_0_AUTO, ip->ic->hostname); 
                  return -1;
                }
              
              ip->cipher_suite_id = cipher_suite_id_ranking[i];
              ip->cipher_suite_id_ranking_index = i;
              if (ipmi_cipher_suite_id_to_algorithms(ip->cipher_suite_id,
                                                     &(ip->authentication_algorithm),
                                                     &(ip->integrity_algorithm),
                                                     &(ip->confidentiality_algorithm)) < 0)
                err_exit("_check_open_session_error: ipmi_cipher_suite_id_to_algorithms: ",
                         "cipher_suite_id: %d; %s",
                         ip->cipher_suite_id, strerror(errno));
              
              return 0;
            }
        }
    }

  ipmipower_output(ipmipower_packet_errmsg(ip, OPEN_SESSION_RES), ip->ic->hostname);
  return -1;
}

/* _calculate_cipher_keys
 * 
 * Calculate cipher keys for the remaining IPMI 2.0 protocol
 *
 * Returns  0 on success
 * Returns -1 on ipmi protocol error
 */
static int
_calculate_cipher_keys(ipmipower_powercmd_t ip)
{
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int32_t managed_system_random_number_len;
  char *username;
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  uint32_t username_len;
  char *password;
  uint32_t password_len;
  uint8_t *k_g;

  assert(ip);
  assert(ip->protocol_state == PROTOCOL_STATE_RAKP_MESSAGE_1_SENT);
  
  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The username must be padded despite explicitly not being
   * allowed.  "No Null characters (00h) are allowed in the name".
   * Table 13-11 in the IPMI 2.0 spec.
   */
  if (conf->workaround_flags & WORKAROUND_FLAG_INTEL_2_0_SESSION)
    {
      memset(username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (strlen(conf->username))
	strcpy(username_buf, (char *)conf->username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      if (strlen(conf->username))
	username = conf->username;
      else
	username = NULL;
      username_len = (username) ? strlen(username) : 0;
    }
  
  if (strlen(conf->password))
    password = conf->password;
  else
    password = NULL;

  password_len = (password) ? strlen(password) : 0;

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * When the authentication algorithm is HMAC-MD5-128 and the
   * password is greater than 16 bytes, the Intel BMC truncates the
   * password to 16 bytes when generating keys, hashes, etc.  So we
   * have to do the same when generating keys, hashes, etc.
   */
  if ((conf->workaround_flags & WORKAROUND_FLAG_INTEL_2_0_SESSION)
      && ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;

  if (conf->k_g_len)
    k_g = conf->k_g;
  else
    k_g = NULL;
  
  managed_system_random_number_len = Fiid_obj_get_data(ip->obj_rakp_message_2_res,
                                                       "managed_system_random_number",
                                                       managed_system_random_number,
                                                       IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH);

  if (ipmi_calculate_rmcpplus_session_keys(ip->authentication_algorithm,
                                           ip->integrity_algorithm,
                                           ip->confidentiality_algorithm,
                                           (uint8_t *)password,
                                           password_len,
                                           k_g,
                                           (k_g) ? conf->k_g_len : 0,
                                           ip->remote_console_random_number,
                                           IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                           managed_system_random_number,
                                           managed_system_random_number_len,
                                           ip->name_only_lookup,
                                           ip->privilege_level,
                                           username,
                                           username_len,
                                           &(ip->sik_key_ptr),
                                           &(ip->sik_key_len),
                                           &(ip->integrity_key_ptr),
                                           &(ip->integrity_key_len),
                                           &(ip->confidentiality_key_ptr),
                                           &(ip->confidentiality_key_len)) < 0)
    err_exit("_calculate_cipher_keys(%s:%d): ipmi_calculate_rmcpplus_session_keys: %s",
             ip->ic->hostname, ip->protocol_state, strerror(errno));

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
  struct timeval cur_time, end_time, result;
  unsigned int timeout;
  int rv;

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
	_send_packet(ip, AUTHENTICATION_CAPABILITIES_V20_REQ);
      else
	_send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT)
    {
      int ipmi_1_5, ipmi_2_0;

      /* If the remote machine does not support IPMI 2.0, we move onto
       * IPMI 1.5 protocol appropriately.
       */
      if ((rv = _recv_packet(ip, AUTHENTICATION_CAPABILITIES_V20_RES)) != 1) 
        {
          if (rv < 0) 
	    {
              if (!_check_ipmi_2_0_authentication_capabilities_error(ip))
                {
                  /* Try the IPMI 1.5 version of Get Authentication Capabilities */
                  
                  _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ);
                  goto done;
                }
              return -1;
	    }
          goto done;
        }

      if (_check_ipmi_version_support(ip, &ipmi_1_5, &ipmi_2_0) < 0)
	return -1;
      
      if (conf->ipmi_version == IPMI_VERSION_AUTO)
        {
          /* If we can't detect IPMI 1.5 or IPMI 2.0, assume it's an error
           * and go with the IPMI 1.5 get authentication capabilities
           * response.
           */
          if (!ipmi_1_5 && !ipmi_2_0)
            _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ);
          else if (ipmi_1_5)
            {
              if ((rv = _check_ipmi_1_5_authentication_capabilities(ip, AUTHENTICATION_CAPABILITIES_V20_RES)) < 0)
                return -1;
              
              if (rv)
                {
                  /* Don't consider this a retransmission */
                  _send_packet(ip, AUTHENTICATION_CAPABILITIES_V20_REQ);
                  goto done;
                }
              /* else we continue with the IPMI 1.5 protocol */
              
              ip->ipmi_version = IPMI_VERSION_1_5;
	      ip->highest_received_sequence_number = IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER;

              if (strlen(conf->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
                {
                  ipmipower_output(MSG_TYPE_PASSWORD_LENGTH_INVALID, ip->ic->hostname);
                  return -1;
                }

              _send_packet(ip, GET_SESSION_CHALLENGE_REQ);
            }
          else
            {
	      if (_check_ipmi_2_0_authentication_capabilities(ip) < 0)
		return -1;

              ip->ipmi_version = IPMI_VERSION_2_0;
	      ip->highest_received_sequence_number = IPMIPOWER_RMCPPLUS_INITIAL_OUTBOUND_SEQUENCE_NUMBER;
              _send_packet(ip, GET_CHANNEL_CIPHER_SUITES_REQ);
            }
        }
      else if (conf->ipmi_version == IPMI_VERSION_1_5)
	{
          /* achu: This else if statement block might not be
           * accessible anymore, b/c if the user configured
           * IPMI_VERSION_1_5, a get authentication capabilties for
           * v20 shouldn't have been sent.  We'll leave this code here
           * anyways.
           */
          if (!ipmi_1_5)
            {
              ipmipower_output(MSG_TYPE_IPMI_1_5_UNAVAILABLE, ip->ic->hostname); 
              return -1;
            }
          /* else we continue with the IPMI 1.5 protocol */
	  
	  if ((rv = _check_ipmi_1_5_authentication_capabilities(ip, AUTHENTICATION_CAPABILITIES_V20_RES)) < 0)
	    return -1;
	  
	  if (rv)
	    {
	      /* Don't consider this a retransmission */
	      _send_packet(ip, AUTHENTICATION_CAPABILITIES_V20_REQ);
	      goto done;
	    }
	  /* else we continue with the IPMI 1.5 protocol */
              
	  ip->ipmi_version = IPMI_VERSION_1_5;
	  ip->highest_received_sequence_number = IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER;
	  
	  if (strlen(conf->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
	    {
	      ipmipower_output(MSG_TYPE_PASSWORD_LENGTH_INVALID, ip->ic->hostname);
	      return -1;
	    }
	  
	  _send_packet(ip, GET_SESSION_CHALLENGE_REQ);
	}
      else /* conf->ipmi_version == IPMI_VERSION_2_0 */
        {
          if (!ipmi_2_0)
            {
              ipmipower_output(MSG_TYPE_IPMI_2_0_UNAVAILABLE, ip->ic->hostname); 
              return -1;
            }
          /* else we continue with the IPMI 2.0 protocol */

	  if (_check_ipmi_2_0_authentication_capabilities(ip) < 0)
	    return -1;

          ip->ipmi_version = IPMI_VERSION_2_0;
	  ip->highest_received_sequence_number = IPMIPOWER_RMCPPLUS_INITIAL_OUTBOUND_SEQUENCE_NUMBER;

          _send_packet(ip, GET_CHANNEL_CIPHER_SUITES_REQ);
        }
    }
  else if (ip->protocol_state == PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT) 
    {
      if ((rv = _recv_packet(ip, AUTHENTICATION_CAPABILITIES_RES)) != 1) 
        {
          if (rv < 0) 
            return -1;
          goto done;
        }

      if ((rv = _check_ipmi_1_5_authentication_capabilities(ip, AUTHENTICATION_CAPABILITIES_RES)) < 0)
	return -1;
      
      if (rv)
	{
	  /* Don't consider this a retransmission */
	  _send_packet(ip, AUTHENTICATION_CAPABILITIES_REQ);
          goto done;
        }
      /* else we continue with the IPMI 1.5 protocol */

      ip->ipmi_version = IPMI_VERSION_1_5;
      ip->highest_received_sequence_number = IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER;

      if (strlen(conf->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
        {
          ipmipower_output(MSG_TYPE_PASSWORD_LENGTH_INVALID, ip->ic->hostname);
          return -1;
        }

      _send_packet(ip, GET_SESSION_CHALLENGE_REQ);
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

      _send_packet(ip, ACTIVATE_SESSION_REQ);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_ACTIVATE_SESSION_SENT) 
    {
      if ((rv = _recv_packet(ip, ACTIVATE_SESSION_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      if (_check_activate_session_authentication_type(ip) < 0)
        /* XXX Session is not up, is it ok to quit here?  Or
         * should we timeout?? */
        return -1;
      
      /* We can skip SET_SESSION_PRIVILEGE_LEVEL_REQ on a power status
       * check, because the default IPMI session privilege level is
       * the user privilege level
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS)
	_send_packet(ip, GET_CHASSIS_STATUS_REQ);
      else
        _send_packet(ip, SET_SESSION_PRIVILEGE_LEVEL_REQ);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT)
    {
      /* achu: There is probably a lot of confusing code here,
       * hopefully my comments will guide you through it.
       */
      
      if ((rv = _recv_packet(ip, GET_CHANNEL_CIPHER_SUITES_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      if ((rv = _store_and_calculate_cipher_suite_ids(ip)) < 0)
        return -1;

      if (rv)
        {
          ip->cipher_suite_list_index++;
          _send_packet(ip, GET_CHANNEL_CIPHER_SUITES_REQ);
          goto done;
        }

      if (_determine_cipher_suite_id_to_use(ip) < 0)
        return -1;
     
      _send_packet(ip, OPEN_SESSION_REQ);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_OPEN_SESSION_SENT)
    {
      if ((rv = _recv_packet(ip, OPEN_SESSION_RES)) != 1) 
        {
          if (rv < 0) 
            {
              if (!_check_open_session_error(ip))
                {
                  /* achu: Need to re-init */
                  _init_ipmi_2_0_randomized_data(ip);
                  
                  _send_packet(ip, OPEN_SESSION_REQ);
                  goto done;
                }

              /* XXX Session is not up, is it ok to quit here?  Or
               * should we timeout?? */
              return -1;
            }
          goto done;
        }
     
      _send_packet(ip, RAKP_MESSAGE_1_REQ);
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

      if (_calculate_cipher_keys(ip) < 0)
        return -1;

      _send_packet(ip, RAKP_MESSAGE_3_REQ);
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

      /* We can skip SET_SESSION_PRIVILEGE_LEVEL_REQ on a power status
       * check, because the default IPMI session privilege level is
       * the user privilege level
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS)
	_send_packet(ip, GET_CHASSIS_STATUS_REQ);
      else
        _send_packet(ip, SET_SESSION_PRIVILEGE_LEVEL_REQ);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT) 
    {
      if ((rv = _recv_packet(ip, SET_SESSION_PRIVILEGE_LEVEL_RES)) != 1) 
        {
          if (rv < 0) 
            /* Session is up, so close it */
            _send_packet(ip, CLOSE_SESSION_REQ);
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
        _send_packet(ip, GET_CHASSIS_STATUS_REQ);
      else
        _send_packet(ip, CHASSIS_CONTROL_REQ);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT) 
    {
      uint64_t power_state;
      
      if ((rv = _recv_packet(ip, GET_CHASSIS_STATUS_RES)) != 1) 
        {
          if (rv < 0)  
            /* Session is up, so close it */
            _send_packet(ip, CLOSE_SESSION_REQ);
          goto done;
        }

      Fiid_obj_get(ip->obj_get_chassis_status_res, 
                   "current_power_state.power_is_on",
                   &power_state);

      if (conf->wait_until_on == IPMIPOWER_TRUE
          && ip->cmd == POWER_CMD_POWER_ON
	  && ip->wait_until_on_state)
	{
          if (power_state)
	    {
	      ipmipower_output(MSG_TYPE_OK, ip->ic->hostname);
	      ip->wait_until_on_state = 0;
	      _send_packet(ip, CLOSE_SESSION_REQ);
	    }
	}
      else if (conf->wait_until_off == IPMIPOWER_TRUE
               && ip->cmd == POWER_CMD_POWER_OFF
               && ip->wait_until_off_state)
	{
          if (!power_state)
	    {
	      ipmipower_output(MSG_TYPE_OK, ip->ic->hostname);
	      ip->wait_until_off_state = 0;
	      _send_packet(ip, CLOSE_SESSION_REQ);
	    }
	}
      else if (ip->cmd == POWER_CMD_POWER_STATUS) 
        {
          ipmipower_output((power_state) ? MSG_TYPE_ON : MSG_TYPE_OFF, 
                           ip->ic->hostname); 
          _send_packet(ip, CLOSE_SESSION_REQ);
        }
      else if (conf->on_if_off && (ip->cmd == POWER_CMD_POWER_CYCLE
                                  || ip->cmd == POWER_CMD_POWER_RESET)) 
        {
          if (!power_state) 
            {
              /* This is now a power-on operation */
              ip->cmd = POWER_CMD_POWER_ON;
            }
          _send_packet(ip, CHASSIS_CONTROL_REQ);
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
            _send_packet(ip, CLOSE_SESSION_REQ);
          goto done;
        }
        
      if ((conf->wait_until_on == IPMIPOWER_TRUE
           && ip->cmd == POWER_CMD_POWER_ON)
          || (conf->wait_until_off == IPMIPOWER_TRUE
              && ip->cmd == POWER_CMD_POWER_OFF))
	{
          if (ip->cmd == POWER_CMD_POWER_ON)
            ip->wait_until_on_state++;
          else
            ip->wait_until_off_state++;
	  _send_packet(ip, GET_CHASSIS_STATUS_REQ);
	}
      else
        {
          ipmipower_output(MSG_TYPE_OK, ip->ic->hostname);
          
          /* IPMI Workaround (achu)
           *
           * Discovered on Intel Tiger4 (SR870BN4)
           *
           * There is no response from the IPMI close command if the
           * POWER_CMD_POWER_RESET power control command is
           * successful.  So just skip the close session.
           */
          if (ip->cmd == POWER_CMD_POWER_RESET)
            goto finish_up;
          else
            _send_packet(ip, CLOSE_SESSION_REQ);
        }
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
  timeval_add_ms(&(ip->time_begin), conf->session_timeout_len, &end_time);
  timeval_sub(&end_time, &cur_time, &result);
  timeval_millisecond_calc(&result, &timeout);

  /* shorter timeout b/c of retransmission timeout */
  if (conf->wait_until_off
      && conf->retransmission_wait_timeout_len)
    {
      int retransmission_timeout_len = (conf->retransmission_backoff_count) ? (conf->retransmission_wait_timeout_len * (1 + (ip->retransmission_count/conf->retransmission_backoff_count))) : conf->retransmission_wait_timeout_len;
      if (timeout > retransmission_timeout_len)
        timeout = retransmission_timeout_len;
    }
  else if (conf->retransmission_timeout_len) 
    {
      int retransmission_timeout_len = (conf->retransmission_backoff_count) ? (conf->retransmission_timeout_len * (1 + (ip->retransmission_count/conf->retransmission_backoff_count))) : conf->retransmission_timeout_len;
      if (timeout > retransmission_timeout_len)
        timeout = retransmission_timeout_len;
    }

  return (int)timeout;
}

int 
ipmipower_powercmd_process_pending(int *timeout)
{
  ListIterator itr;
  ipmipower_powercmd_t ip;
  int min_timeout = conf->session_timeout_len;
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

      if (tmp_timeout < min_timeout)
        min_timeout = tmp_timeout;
    }
  list_iterator_destroy(itr);

  if ((num_pending = list_count(pending)) == 0) 
    ipmipower_output_finish();
  
  /* If the last pending power control command finished, the timeout
   * is 0 to get the primary poll loop to "re-init" at the start of
   * the loop.
   */
  if (num_pending)
    *timeout = min_timeout;
  else
    *timeout = 0;
  return num_pending;
}
