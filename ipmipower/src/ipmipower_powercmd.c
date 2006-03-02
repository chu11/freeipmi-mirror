/*****************************************************************************\
 *  $Id: ipmipower_powercmd.c,v 1.25 2006-03-02 19:11:34 chu11 Exp $
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
#include "ipmipower_auth.h"
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

  Fiid_obj_destroy(ip->rmcp_req);
  Fiid_obj_destroy(ip->rmcp_res);
  Fiid_obj_destroy(ip->session_req);
  Fiid_obj_destroy(ip->session_res);
  Fiid_obj_destroy(ip->msg_req);
  Fiid_obj_destroy(ip->msg_res);
  Fiid_obj_destroy(ip->trlr_res);

  Fiid_obj_destroy(ip->auth_req);
  Fiid_obj_destroy(ip->auth_res);
  Fiid_obj_destroy(ip->sess_req);
  Fiid_obj_destroy(ip->sess_res);
  Fiid_obj_destroy(ip->actv_req);
  Fiid_obj_destroy(ip->actv_res);
  Fiid_obj_destroy(ip->priv_req);
  Fiid_obj_destroy(ip->priv_res);
  Fiid_obj_destroy(ip->clos_req);
  Fiid_obj_destroy(ip->clos_res);
  Fiid_obj_destroy(ip->chas_req);
  Fiid_obj_destroy(ip->chas_res);
  Fiid_obj_destroy(ip->ctrl_req);
  Fiid_obj_destroy(ip->ctrl_res);

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
    
  ip->rmcp_req = Fiid_obj_create(tmpl_rmcp_hdr); 
  ip->rmcp_res = Fiid_obj_create(tmpl_rmcp_hdr); 
  ip->session_req = Fiid_obj_create(tmpl_lan_session_hdr); 
  ip->session_res = Fiid_obj_create(tmpl_lan_session_hdr); 
  ip->msg_req = Fiid_obj_create(tmpl_lan_msg_hdr_rq); 
  ip->msg_res = Fiid_obj_create(tmpl_lan_msg_hdr_rs); 
  ip->trlr_res = Fiid_obj_create(tmpl_lan_msg_trlr); 

  ip->auth_req = Fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_rq); 
  ip->auth_res = Fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_rs); 
  ip->sess_req = Fiid_obj_create(tmpl_cmd_get_session_challenge_rq); 
  ip->sess_res = Fiid_obj_create(tmpl_cmd_get_session_challenge_rs); 
  ip->actv_req = Fiid_obj_create(tmpl_cmd_activate_session_rq); 
  ip->actv_res = Fiid_obj_create(tmpl_cmd_activate_session_rs); 
  ip->priv_req = Fiid_obj_create(tmpl_cmd_set_session_privilege_level_rq); 
  ip->priv_res = Fiid_obj_create(tmpl_cmd_set_session_privilege_level_rs); 
  ip->clos_req = Fiid_obj_create(tmpl_cmd_close_session_rq); 
  ip->clos_res = Fiid_obj_create(tmpl_cmd_close_session_rs); 
  ip->chas_req = Fiid_obj_create(tmpl_cmd_get_chassis_status_rq); 
  ip->chas_res = Fiid_obj_create(tmpl_cmd_get_chassis_status_rs); 
  ip->ctrl_req = Fiid_obj_create(tmpl_cmd_chassis_control_rq); 
  ip->ctrl_res = Fiid_obj_create(tmpl_cmd_chassis_control_rs); 

  ip->cmd = cmd;
  ip->protocol_state = PROTOCOL_STATE_START;

  Gettimeofday(&(ip->time_begin), NULL);
  ip->session_inbound_count = 0;
  ip->initial_outbound_sequence_number = get_rand();
  ip->highest_received_sequence_number = ip->initial_outbound_sequence_number;
  ip->previously_received_list = 0xFF;
  ip->retry_count = 0;
  ip->error_occurred = IPMIPOWER_FALSE;
  ip->permsgauth_enabled = IPMIPOWER_TRUE;

  /* ip->authtype is set after Get Authentication Capabilities
   * Response is received 
   */

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

  ip->close_timeout = 0;
  ip->ic = ic;

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

  /* Must set before ipmipower_packet_create, so requester sequence
   * number is set properly.
   */
  ip->ic->ipmi_requester_sequence_number_counter++;
  
  len = ipmipower_packet_create(ip, pkt, buffer, IPMI_PACKET_BUFLEN);
  ipmipower_packet_dump(ip, pkt, buffer, len);
  Cbuf_write(ip->ic->ipmi_out, buffer, len);
                     
  if (pkt == AUTH_REQ)
    ip->protocol_state = PROTOCOL_STATE_AUTH_SENT;
  else if (pkt == SESS_REQ)
    ip->protocol_state = PROTOCOL_STATE_SESS_SENT;
  else if (pkt == ACTV_REQ) 
    {
      ip->protocol_state = PROTOCOL_STATE_ACTV_SENT;

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
  else if (pkt == PRIV_REQ)
    ip->protocol_state = PROTOCOL_STATE_PRIV_SENT;
  else if (pkt == CLOS_REQ)
    ip->protocol_state = PROTOCOL_STATE_CLOS_SENT;
  else if (pkt == CHAS_REQ)
    ip->protocol_state = PROTOCOL_STATE_CHAS_SENT;
  else if (pkt == CTRL_REQ)
    ip->protocol_state = PROTOCOL_STATE_CTRL_SENT;

  if (pkt == PRIV_REQ || pkt == CLOS_REQ || pkt == CHAS_REQ || pkt == CTRL_REQ) 
    ip->session_inbound_count++;

  Gettimeofday(&(ip->ic->last_ipmi_send), NULL);
}

/* _bad_packet
 * - handle debugging and error checking
 * Returns 0 if packet should be ignored, -1 if packet error was returned
 */
static int 
_bad_packet(ipmipower_powercmd_t ip, packet_type_t pkt, 
            int oseq_flag, int sid_flag, int netfn_flag, 
            int rseq_flag, int cmd_flag, int cc_flag) 
{
  uint8_t cc, netfn, cmd, rseq;
  uint32_t sid, oseq;

  /* If everything else is correct besides completion code, packet
   * returned an error.
   */
  if (oseq_flag && sid_flag && netfn_flag && rseq_flag && cmd_flag && !cc_flag) 
    {
      ipmipower_output(ipmipower_packet_errmsg(ip, pkt), ip->ic->hostname);

      ip->retry_count = 0;  /* important to reset */
      Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
      ip->error_occurred = IPMIPOWER_TRUE; 
      return -1;
    }

  ipmipower_packet_response_data(ip, pkt, &oseq, &sid, &netfn, &rseq, &cmd, &cc);

  /* I guess the packet is corrupted or is a retransmission of something we don't need */
  dbg("_bad_packet(%s:%d): ignoring bad packet: oseq=%x sid=%x "
      "netfn=%x rseq=%x cmd=%x cc=%x",
      ip->ic->hostname, ip->protocol_state, oseq, sid, netfn, 
      rseq, cmd, cc);
  return 0;
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
  int ret, at, len = 0;
  char buffer[IPMI_PACKET_BUFLEN];
  uint8_t *password;
  int check_authcode_retry_flag = 0;
  int oseq_flag, sid_flag, netfn_flag, rseq_flag, cmd_flag, cc_flag;

  assert(PACKET_TYPE_VALID_RES(pkt));

  if (!(len = Cbuf_peek_and_drop(ip->ic->ipmi_in, buffer, IPMI_PACKET_BUFLEN)))
    return 0;

  ipmipower_packet_dump(ip, pkt, buffer, len);
      
  if ((ret = ipmi_lan_check_checksum((uint8_t *)buffer, len)) < 0)
    err_exit("_recv_packet(%s:%d): ipmi_lan_check_checksum: %s",
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if (!ret) 
    {
      dbg("_recv_packet(%s:%d): bad chksum",
          ip->ic->hostname, ip->protocol_state);
      return 0;
    }
  
  if (pkt == AUTH_RES || pkt == SESS_RES)
    at = IPMI_AUTHENTICATION_TYPE_NONE;
  else if (pkt == ACTV_RES)
    at = ip->authtype;
  else
    {
      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        {
          at = IPMI_AUTHENTICATION_TYPE_NONE;
          check_authcode_retry_flag++;
        }
      else
        at = ip->authtype;
    }

  if (at != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      if (strlen(conf->password))
        password = (uint8_t *)conf->password;
      else
        password = NULL;
    }
  else
    password = NULL;

  if ((ret = ipmi_lan_check_session_authentication_code((uint8_t *)buffer, 
                                                        len,
                                                        at,
                                                        (uint8_t *)password,
                                                        strlen(conf->password))) < 0)
    err_exit("_recv_packet(%s:%d): ipmi_lan_check_session_authentication_code: %s",
             ip->ic->hostname, ip->protocol_state, strerror(errno));
      
  /* IPMI Workaround (achu)
   *
   * Discovered on Dell PowerEdge 2850
   *
   * When per-message authentication is disabled, and we send a
   * message to a remote machine with auth-type none, the Dell
   * motherboard will respond with a message with the auth-type used
   * in the activate session stage and the appropriate authcode. So
   * here is our second session-authcode check attempt under these
   * circumstances.
   */
  if (conf->check_unexpected_authcode == IPMIPOWER_TRUE && !ret && check_authcode_retry_flag)
    {
      dbg("_recv_packet(%s:%d): retry authcode check", 
	  ip->ic->hostname, ip->protocol_state, strerror(errno));

      at = ip->authtype;

      if (at != IPMI_AUTHENTICATION_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      if ((ret = ipmi_lan_check_session_authentication_code((uint8_t *)buffer, 
                                                            len,
                                                            at,
                                                            (uint8_t *)password,
                                                            strlen(conf->password))) < 0)
        err_exit("_recv_packet(%s:%d): ipmi_lan_check_session_authentication_code: %s",
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (ret)
        dbg("_recv_packet(%s:%d): permsgauth authcode re-check passed",
            ip->ic->hostname, ip->protocol_state);
    }

  if (!ret)
    {
      dbg("_recv_packet(%s:%d): bad authcode",
          ip->ic->hostname, ip->protocol_state);
      return 0;
    }

  ipmipower_packet_store(ip, pkt, buffer, len);

  if (ipmipower_check_packet(ip, pkt, &oseq_flag, &sid_flag, &netfn_flag, 
                             &rseq_flag, &cmd_flag, &cc_flag))
    {
      ip->retry_count = 0;  /* important to reset */
      Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
      return 1;
    }

  /* IPMI Workaround (achu)
   *
   * Disocvered on Intel SE7520JR2 with National Semiconductor PC87431M mBMC
   *
   * Note: Later changes in ipmipower have removed the need for these
   * workarounds.  I still note them for convenience.
   *
   * The initial outbound sequence number on activate session response
   * is off by one.  The activate session response packet is supposed
   * to contain the initial outbound sequence number passed during the
   * request.  The outbound sequence number on a close session reponse
   * may also be incorrect.
   */

  /* achu: If this is the close session response, go ahead and just
   * accept the packet under most circumstances.  We'll just close the
   * session anyways.
   */
  if (pkt == CLOS_RES && sid_flag && netfn_flag && cmd_flag)
    {
      ip->retry_count = 0;  /* important to reset */
      Gettimeofday(&ip->ic->last_ipmi_recv, NULL);
      return 1;
    }

  return _bad_packet(ip, pkt, oseq_flag, sid_flag, netfn_flag, rseq_flag, cmd_flag, cc_flag);
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
      if (ip->protocol_state != PROTOCOL_STATE_CLOS_SENT)
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

  if (ip->protocol_state == PROTOCOL_STATE_AUTH_SENT)
    _send_packet(ip, AUTH_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_SESS_SENT) 
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

      _send_packet(ip, SESS_REQ, 1);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_ACTV_SENT)
    _send_packet(ip, ACTV_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_PRIV_SENT)
    _send_packet(ip, PRIV_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_CHAS_SENT)
    _send_packet(ip, CHAS_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_CTRL_SENT)
    _send_packet(ip, CTRL_REQ, 1);
  else if (ip->protocol_state == PROTOCOL_STATE_CLOS_SENT)
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
     * _send_packet(ip, CLOS_REQ, 1); 
     */
    ip->close_timeout++;

  return 1;
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
    _send_packet(ip, AUTH_REQ, 0);
  else if (ip->protocol_state == PROTOCOL_STATE_AUTH_SENT) 
    {
      uint64_t auth_type_none, auth_type_md2, auth_type_md5, 
        auth_type_straight_password_key, auth_status_anonymous_login, 
        auth_status_null_username, auth_status_non_null_username, 
        auth_status_per_message_authentication;
      int authtype_try_higher_priv = 0;

      if ((rv = _recv_packet(ip, AUTH_RES)) != 1) 
        {
          if (rv < 0) 
            return -1;
          goto done;
        }

      /* Using results from Get Authentication Capabilities Response,
       * determine:
       *
       * 1) If we are capable of authenticating with the remote host.
       *
       * 2) How to authenticate with the remote host.
       */

      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_type.none", 
		   &auth_type_none);
      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_type.md2", 
		   &auth_type_md2);
      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_type.md5", 
		   &auth_type_md5);
      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_type.straight_password_key", 
		   &auth_type_straight_password_key);
      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_status.anonymous_login", 
		   &auth_status_anonymous_login);
      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_status.null_username",
		   &auth_status_null_username);
      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_status.non_null_username", 
		   &auth_status_non_null_username);
      Fiid_obj_get(ip->auth_res, 
                   (uint8_t *)"authentication_status.per_message_authentication",
		   &auth_status_per_message_authentication);

      /* Does the remote BMC's authentication configuration support
       * our username/password combination 
       */
      if ((!strlen(conf->username) && !strlen(conf->password)
           && !auth_status_anonymous_login
           && !auth_type_none)
          || (!strlen(conf->username) 
              && !auth_status_anonymous_login
              && !auth_status_null_username)
	  || (strlen(conf->username)
	      && !auth_status_non_null_username))
	{
#ifndef NDEBUG
	  ipmipower_output(MSG_TYPE_USERNAME, ip->ic->hostname);
#else
	  ipmipower_output(MSG_TYPE_PERMISSION, ip->ic->hostname);
#endif
	  ip->error_occurred = IPMIPOWER_TRUE; 
	  return -1;
	}

      if (conf->authtype == AUTH_TYPE_AUTO)
	{
	  /* Choose the best authentication type available.
	   * none and null password > md5 > md2 > straight_password_key > none
	   */
	  if (!strlen(conf->password) && auth_type_none)
            ip->authtype = ipmipower_ipmi_auth_type(AUTH_TYPE_NONE);
	  else if (auth_type_md5)
            ip->authtype = ipmipower_ipmi_auth_type(AUTH_TYPE_MD5);
          else if (auth_type_md2)
            ip->authtype = ipmipower_ipmi_auth_type(AUTH_TYPE_MD2);
          else if (auth_type_straight_password_key)
            ip->authtype = ipmipower_ipmi_auth_type(AUTH_TYPE_STRAIGHT_PASSWORD_KEY);
          else if (auth_type_none)
            ip->authtype = ipmipower_ipmi_auth_type(AUTH_TYPE_NONE);
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
                authtype_try_higher_priv = 1;
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
 	  if ((conf->authtype == AUTH_TYPE_NONE
               && auth_type_none)
              || (conf->authtype == AUTH_TYPE_STRAIGHT_PASSWORD_KEY 
                  && auth_type_straight_password_key)
              || (conf->authtype == AUTH_TYPE_MD2
		  && auth_type_md2)
	      || (conf->authtype == AUTH_TYPE_MD5
		  && auth_type_md5))
            ip->authtype = ipmipower_ipmi_auth_type(conf->authtype);
          else
            {
              if (ip->privilege == IPMI_PRIVILEGE_LEVEL_ADMIN)
                {
                  /* Time to give up */
                  ipmipower_output(MSG_TYPE_AUTHTYPE, ip->ic->hostname);
                  return -1;
                }
              else
                authtype_try_higher_priv = 1;
	    }
	}
         
      /* We can't authenticate with any mechanism for the current
       * privilege level.  But we may able to authenticate at a higher
       * one.  Lets up the privilege level and try again.
       */
      if (authtype_try_higher_priv)
        {
          /* Try a higher privilege level */
          if (ip->privilege == IPMI_PRIVILEGE_LEVEL_USER)
            ip->privilege = IPMI_PRIVILEGE_LEVEL_OPERATOR;
          else if (ip->privilege == IPMI_PRIVILEGE_LEVEL_OPERATOR)
            ip->privilege = IPMI_PRIVILEGE_LEVEL_ADMIN;
          else
            err_exit("_process_ipmi_packets: invalid privilege state: %d", 
                     ip->privilege);

          /* Don't consider this a retransmission */
          _send_packet(ip, AUTH_REQ, 0);
          goto done;
        }

      if (!conf->force_permsg_auth)
        {
          if (!auth_status_per_message_authentication)
            ip->permsgauth_enabled = IPMIPOWER_TRUE;
          else
            ip->permsgauth_enabled = IPMIPOWER_FALSE;
        }
      else
        ip->permsgauth_enabled = IPMIPOWER_TRUE;

      _send_packet(ip, SESS_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_SESS_SENT) 
    {
      if ((rv = _recv_packet(ip, SESS_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      _send_packet(ip, ACTV_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_ACTV_SENT) 
    {
      if ((rv = _recv_packet(ip, ACTV_RES)) != 1) 
        {
          if (rv < 0) 
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return -1;
          goto done;
        }

      /* We can skip PRIV_REQ on a power status check, because the
       * default IPMI session privilege level is the user privilege
       * level
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS)
        _send_packet(ip, CHAS_REQ, 0);
      else
        _send_packet(ip, PRIV_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_PRIV_SENT) 
    {
      if ((rv = _recv_packet(ip, PRIV_RES)) != 1) 
        {
          if (rv < 0) 
            /* Session is up, so close it */
            _send_packet(ip, CLOS_REQ, 0);
          goto done;
        }

      /* Next packet we send depends on the power command and the
       * options set.  The POWER_CMD_POWER_STATUS command shouldn't be
       * possible at this point (see comments above under
       * protocol_state == PROTOCOL_STATE_ACTV_SENT), but we leave the
       * code below anyway.
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS
          || (conf->on_if_off 
              && (ip->cmd == POWER_CMD_POWER_CYCLE
                  || ip->cmd == POWER_CMD_POWER_RESET)))
        _send_packet(ip, CHAS_REQ, 0);
      else
        _send_packet(ip, CTRL_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_CHAS_SENT) 
    {
      uint64_t power_state;

      if ((rv = _recv_packet(ip, CHAS_RES)) != 1) 
        {
          if (rv < 0)  
            /* Session is up, so close it */
            _send_packet(ip, CLOS_REQ, 0);
          goto done;
        }

      Fiid_obj_get(ip->chas_res, 
                   (uint8_t *)"current_power_state.power_is_on",
                   &power_state);

      if (ip->cmd == POWER_CMD_POWER_STATUS) 
        {
          ipmipower_output((power_state) ? MSG_TYPE_ON : MSG_TYPE_OFF, 
                           ip->ic->hostname); 
          _send_packet(ip, CLOS_REQ, 0);
        }
      else if (conf->on_if_off && (ip->cmd == POWER_CMD_POWER_CYCLE
                                  || ip->cmd == POWER_CMD_POWER_RESET)) 
        {
          if (!power_state) 
            {
              /* This is now a power-on operation */
              ip->cmd = POWER_CMD_POWER_ON;
            }
          _send_packet(ip, CTRL_REQ, 0);
        }
      else
        err_exit("_process_ipmi_packets: invalid command state: %d", ip->cmd);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_CTRL_SENT) 
    {
      if ((rv = _recv_packet(ip, CTRL_RES)) != 1) 
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet(ip, CLOS_REQ, 0);
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
        _send_packet(ip, CLOS_REQ, 0);
    }
  else if (ip->protocol_state == PROTOCOL_STATE_CLOS_SENT) 
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

      if (!_recv_packet(ip, CLOS_RES)) 
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
