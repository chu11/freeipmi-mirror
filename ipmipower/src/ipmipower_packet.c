/*****************************************************************************\
 *  $Id: ipmipower_packet.c,v 1.12.2.4 2006-01-29 16:08:00 chu11 Exp $
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
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <errno.h>
#include <assert.h>
#include <stdint.h>

#include "ipmipower_packet.h"
#include "ipmipower_auth.h"
#include "ipmipower_wrappers.h"

extern struct ipmipower_config *conf;

/* fiid_template_t */
fiid_field_t *
ipmipower_packet_cmd_template(ipmipower_powercmd_t ip, packet_type_t pkt)
{
  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_PKT(pkt));

  if (pkt == AUTH_REQ)
    return &tmpl_cmd_get_channel_auth_caps_rq[0];
  else if (pkt == AUTH_RES)
    return &tmpl_cmd_get_channel_auth_caps_rs[0];
  else if (pkt == SESS_REQ)
    return &tmpl_cmd_get_session_challenge_rq[0];
  else if (pkt == SESS_RES)
    return &tmpl_cmd_get_session_challenge_rs[0];
  else if (pkt == ACTV_REQ)
    return &tmpl_cmd_activate_session_rq[0];
  else if (pkt == ACTV_RES)
    return &tmpl_cmd_activate_session_rs[0];
  else if (pkt == PRIV_REQ)
    return &tmpl_cmd_set_session_priv_level_rq[0];
  else if (pkt == PRIV_RES)
    return &tmpl_cmd_set_session_priv_level_rs[0];
  else if (pkt == CLOS_REQ)
    return &tmpl_cmd_close_session_rq[0];
  else if (pkt == CLOS_RES)
    return &tmpl_cmd_close_session_rs[0];
  else if (pkt == CHAS_REQ)
    return &tmpl_cmd_get_chassis_status_rq[0];
  else if (pkt == CHAS_RES)
    return &tmpl_cmd_get_chassis_status_rs[0];
  else if (pkt == CTRL_REQ)
    return &tmpl_cmd_chassis_ctrl_rq[0];
  else if (pkt == CTRL_RES)
    return &tmpl_cmd_chassis_ctrl_rs[0];
  else
    err_exit("ipmipower_packet_cmd_template: Invalid packet type(%s:%d)",
             ip->ic->hostname, ip->protocol_state);

  return NULL;                  /* NOT REACHED */
}

fiid_obj_t
ipmipower_packet_cmd_obj(ipmipower_powercmd_t ip, packet_type_t pkt)
{
  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_PKT(pkt));

  if (pkt == AUTH_REQ)
    return ip->auth_req;
  else if (pkt == AUTH_RES)
    return ip->auth_res;
  else if (pkt == SESS_REQ)
    return ip->sess_req;
  else if (pkt == SESS_RES)
    return ip->sess_res;
  else if (pkt == ACTV_REQ)
    return ip->actv_req;
  else if (pkt == ACTV_RES)
    return ip->actv_res;
  else if (pkt == PRIV_REQ)
    return ip->priv_req;
  else if (pkt == PRIV_RES)
    return ip->priv_res;
  else if (pkt == CLOS_REQ)
    return ip->clos_req;
  else if (pkt == CLOS_RES)
    return ip->clos_res;
  else if (pkt == CHAS_REQ)
    return ip->chas_req;
  else if (pkt == CHAS_RES)
    return ip->chas_res;
  else if (pkt == CTRL_REQ)
    return ip->ctrl_req;
  else if (pkt == CTRL_RES)
    return ip->ctrl_res;
  else
    err_exit("ipmipower_packet_cmd_obj: Invalid packet type(%s:%d)",
             ip->ic->hostname, ip->protocol_state);

  return NULL;                  /* NOT REACHED */
}

void
ipmipower_packet_dump(ipmipower_powercmd_t ip, packet_type_t pkt,
                      char *buffer, int len)
{
  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_PKT(pkt));
  assert (buffer != NULL);

#ifndef NDEBUG
  if (conf->ipmidump)
    {
      char *hdr;
      if (pkt == AUTH_REQ)
        hdr = 
          "============================================\n"
          "= Get Authentication Capabilities Request  =\n"
          "============================================";
      else if (pkt == AUTH_RES)
        hdr = 
          "============================================\n"
          "= Get Authentication Capabilities Response =\n"
          "============================================";
      else if (pkt == SESS_REQ)
        hdr = 
          "============================================\n"
          "= Get Session Challenge Request            =\n"
          "============================================";
      else if (pkt == SESS_RES)
        hdr = 
          "============================================\n"
          "= Get Session Challenge Response           =\n"
          "============================================";
      else if (pkt == ACTV_REQ)
        hdr = 
          "============================================\n"
          "= Activate Session Request                 =\n"
          "============================================";
      else if (pkt == ACTV_RES)
        hdr = 
          "============================================\n"
          "= Activate Session Response                =\n"
          "============================================";
      else if (pkt == PRIV_REQ)
        hdr = 
          "============================================\n"
          "= Set Session Privilege Request            =\n"
          "============================================";
      else if (pkt == PRIV_RES)
        hdr = 
          "============================================\n"
          "= Set Session Privilege Response           =\n"
          "============================================";
      else if (pkt == CLOS_REQ)
        hdr = 
          "============================================\n"
          "= Close Session Request                    =\n"
          "============================================";
      else if (pkt == CLOS_RES)
        hdr = 
          "============================================\n"
          "= Close Session Response                   =\n"
          "============================================";
      else if (pkt == CHAS_REQ)
        hdr = 
          "============================================\n"
          "= Get Chassis Status Request               =\n"
          "============================================";
      else if (pkt == CHAS_RES)
        hdr = 
          "============================================\n"
          "= Get Chassis Status Response              =\n"
          "============================================";
      else if (pkt == CTRL_REQ)
        hdr = 
          "============================================\n"
          "= Chassis Control Request                  =\n"
          "============================================";
      else if (pkt == CTRL_RES)
        hdr = 
          "============================================\n"
          "= Chassis Control Response                 =\n"
          "============================================";
      
      if (pkt & PACKET_TYPE_REQ_MASK)
        Fiid_obj_dump_lan(STDERR_FILENO, 
			  ip->ic->hostname, 
			  hdr, 
			  (uint8_t *)buffer, 
			  len,
                          tmpl_lan_msg_hdr_rq,
                          ipmipower_packet_cmd_template(ip, pkt));
      else
        Fiid_obj_dump_lan(STDERR_FILENO, 
			  ip->ic->hostname, 
			  hdr, 
			  (uint8_t *)buffer, 
			  len,
                          tmpl_lan_msg_hdr_rs,
                          ipmipower_packet_cmd_template(ip, pkt));
    }
#endif
}

void 
ipmipower_packet_store(ipmipower_powercmd_t ip, packet_type_t pkt,
                       char *buffer, int len) 
{
  fiid_obj_t obj;
  
  assert(ip != NULL);
  assert(buffer != NULL);
  assert(len > 0);
  assert(PACKET_TYPE_VALID_RES(pkt));

  obj = ipmipower_packet_cmd_obj(ip, pkt);
  
  Fiid_obj_clear(ip->rmcp_res);
  Fiid_obj_clear(ip->session_res);
  Fiid_obj_clear(ip->msg_res);
  Fiid_obj_clear(ip->trlr_res);
  Fiid_obj_clear(obj);

  if (unassemble_ipmi_lan_pkt((uint8_t *)buffer, 
			      len, 
			      ip->rmcp_res, 
			      ip->session_res, 
			      ip->msg_res, 
			      ipmipower_packet_cmd_obj(ip, pkt), 
			      ip->trlr_res) < 0)
    err_exit("ipmipower_packet_store: unassemble_ipmi_lan_pkt: %s", strerror(errno));
}

int
ipmipower_packet_create(ipmipower_powercmd_t ip, packet_type_t pkt,
                        char *buffer, int buflen) 
{
  uint8_t at;
  int len = 0;
  fiid_obj_t obj;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_REQ(pkt));

  obj = ipmipower_packet_cmd_obj(ip, pkt);

  Fiid_obj_clear(ip->rmcp_req);
  Fiid_obj_clear(ip->session_req);
  Fiid_obj_clear(ip->msg_req);
  Fiid_obj_clear(obj);

  if (fill_hdr_rmcp_ipmi(ip->rmcp_req) < 0)
    err_exit("ipmipower_packet_create(%s: %d): fill_hdr_rmcp_ipmi: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if (pkt == AUTH_REQ)
    {
      if (fill_hdr_session(IPMI_SESSION_AUTH_TYPE_NONE, 
                           0, 
			   0, 
			   NULL, 
			   0, 
                           tmpl_cmd_get_channel_auth_caps_rq, 
			   ip->session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_hdr_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_seq_num_counter % (IPMIPOWER_RSEQ_MAX + 1)), 
                           ip->msg_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (fill_cmd_get_channel_auth_caps(IPMI_CHANNEL_CURRENT_CHANNEL,
                                         ip->privilege, 
                                         ip->auth_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_channel_auth_caps: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if ((len = assemble_ipmi_lan_pkt(ip->rmcp_req, 
				       ip->session_req, 
				       ip->msg_req, 
                                       ip->auth_req, 
                                       (uint8_t *)buffer, 
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == SESS_REQ)
    {
      uint8_t *username;

      if (fill_hdr_session(IPMI_SESSION_AUTH_TYPE_NONE, 
                           0, 
			   0, 
			   NULL,
			   0, 
                           tmpl_cmd_get_session_challenge_rq, 
			   ip->session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_hdr_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_seq_num_counter % (IPMIPOWER_RSEQ_MAX + 1)), 
                           ip->msg_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (strlen(conf->username))
        username = (uint8_t *)conf->username;
      else
        username = NULL;

      if (fill_cmd_get_session_challenge(ip->authtype, 
                                         (char *)username, 
                                         strlen(conf->username),
                                         ip->sess_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_session_challenge: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if ((len = assemble_ipmi_lan_pkt(ip->rmcp_req, 
				       ip->session_req, 
				       ip->msg_req, 
                                       ip->sess_req,
                                       (uint8_t *)buffer, 
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == ACTV_REQ)
    {
      uint64_t tmp_session_id;
      uint8_t *password;
      uint8_t challenge_str[IPMI_SESSION_CHALLENGE_STR_LEN];
      int32_t challenge_str_len;

      if (strlen(conf->password))
        password = (uint8_t *)conf->password;
      else
        password = NULL;

      Fiid_obj_get(ip->sess_res, (uint8_t *)"tmp_session_id", &tmp_session_id);
      
      if (fill_hdr_session(ip->authtype, 
                           0,
                           (uint32_t)tmp_session_id, 
                           password,
                           strlen(conf->password), 
                           tmpl_cmd_activate_session_rq, 
			   ip->session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_hdr_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_seq_num_counter % (IPMIPOWER_RSEQ_MAX + 1)), 
                           ip->msg_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((challenge_str_len = fiid_obj_get_data(ip->sess_res,
						 (uint8_t *)"challenge_str",
						 challenge_str,
						 IPMI_SESSION_CHALLENGE_STR_LEN)) < 0)
	err_exit("ipmipower_packet_create(%s: %d): fiid_obj_get_data: %s",
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (!challenge_str_len)
	err_exit("ipmipower_packet_create(%s: %d): empty challenge string",
                 ip->ic->hostname, ip->protocol_state);

      if (fill_cmd_activate_session(ip->authtype, 
				    ip->privilege, 
				    challenge_str,
				    challenge_str_len,
                                    ip->initial_outbound_seq_num,
                                    ip->actv_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_activate_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->rmcp_req, 
				       ip->session_req, 
				       ip->msg_req, 
                                       ip->actv_req, 
                                       (uint8_t *)buffer,
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == PRIV_REQ)
    {
      uint64_t initial_inbound_seq_num;
      uint64_t session_id;
      uint8_t *password;
      uint8_t priv;

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_SESSION_AUTH_TYPE_NONE; 
      else
        at = ip->authtype;

      if (at != IPMI_SESSION_AUTH_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      /* Although we may have authenticated at a higher privilege level than
       * necessary, we will only set the session privilege the maximum
       * required for the appropriate power control command.
       *
       * Note that the protocol in ipmipower technically skips the set
       * session privilege command for a power status query since a
       * power status query only requires a USER privilege level.  I
       * leave the if statement below anyways.
       */
      if (ip->cmd == POWER_CMD_POWER_STATUS)
        priv = IPMI_PRIV_LEVEL_USER;
      else
        priv = IPMI_PRIV_LEVEL_OPERATOR;

      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"initial_inbound_seq_num", 
		   &initial_inbound_seq_num);
      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_hdr_session(at, 
                           initial_inbound_seq_num + ip->session_inbound_count, 
                           (uint32_t)session_id, 
                           password,
                           strlen(conf->password), 
                           tmpl_cmd_set_session_priv_level_rq, 
                           ip->session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_hdr_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_seq_num_counter % (IPMIPOWER_RSEQ_MAX + 1)), 
                           ip->msg_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_set_session_priv_level(priv, ip->priv_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_set_session_priv_level: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->rmcp_req, 
				       ip->session_req, 
				       ip->msg_req, 
                                       ip->priv_req,
                                       (uint8_t *)buffer, 
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == CLOS_REQ)
    {
      uint64_t initial_inbound_seq_num;
      uint64_t session_id;
      uint8_t *password;

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_SESSION_AUTH_TYPE_NONE; 
      else
        at = ip->authtype;

      if (at != IPMI_SESSION_AUTH_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"initial_inbound_seq_num", 
		   &initial_inbound_seq_num);
      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_hdr_session(at, 
                           initial_inbound_seq_num + ip->session_inbound_count, 
                           (uint32_t)session_id, 
                           password,
                           strlen(conf->password), 
                           tmpl_cmd_close_session_rq, 
                           ip->session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_hdr_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_seq_num_counter % (IPMIPOWER_RSEQ_MAX + 1)), 
                           ip->msg_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_close_session((uint32_t)session_id, ip->clos_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_close_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->rmcp_req, 
				       ip->session_req, 
				       ip->msg_req, 
                                       ip->clos_req, 
                                       (uint8_t *)buffer,
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == CHAS_REQ)
    {
      uint64_t initial_inbound_seq_num;
      uint64_t session_id;
      uint8_t *password;

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_SESSION_AUTH_TYPE_NONE; 
      else
        at = ip->authtype;

      if (at != IPMI_SESSION_AUTH_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"initial_inbound_seq_num", 
		   &initial_inbound_seq_num);
      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_hdr_session(at,
			   initial_inbound_seq_num + ip->session_inbound_count, 
                           (uint32_t)session_id, 
                           password,
                           strlen(conf->password), 
                           tmpl_cmd_get_chassis_status_rq, 
                           ip->session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_hdr_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_CHASSIS_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_seq_num_counter % (IPMIPOWER_RSEQ_MAX + 1)), 
                           ip->msg_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_get_chassis_status(ip->chas_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_chassis_status: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->rmcp_req,
				       ip->session_req, 
				       ip->msg_req, 
                                       ip->chas_req,
                                       (uint8_t *)buffer,
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == CTRL_REQ) 
    {
      uint8_t command = 0;
      uint64_t initial_inbound_seq_num;
      uint64_t session_id;
      uint8_t *password;

      assert(ip->cmd == POWER_CMD_POWER_OFF 
             || ip->cmd == POWER_CMD_POWER_ON  
             || ip->cmd == POWER_CMD_POWER_CYCLE 
             || ip->cmd == POWER_CMD_POWER_RESET
             || ip->cmd == POWER_CMD_PULSE_DIAG_INTR
             || ip->cmd == POWER_CMD_SOFT_SHUTDOWN_OS);

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_SESSION_AUTH_TYPE_NONE; 
      else
        at = ip->authtype;

      if (at != IPMI_SESSION_AUTH_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      if (ip->cmd == POWER_CMD_POWER_OFF)
        command = IPMI_CHASSIS_CTRL_POWER_DOWN;
      else if (ip->cmd == POWER_CMD_POWER_ON)
        command = IPMI_CHASSIS_CTRL_POWER_UP;
      else if (ip->cmd == POWER_CMD_POWER_CYCLE)
        command = IPMI_CHASSIS_CTRL_POWER_CYCLE;
      else if (ip->cmd == POWER_CMD_POWER_RESET)
        command = IPMI_CHASSIS_CTRL_HARD_RESET;
      else if (ip->cmd == POWER_CMD_PULSE_DIAG_INTR)
        command = IPMI_CHASSIS_CTRL_PULSE_DIAG_INTR;
      else if (ip->cmd == POWER_CMD_SOFT_SHUTDOWN_OS)
        command = IPMI_CHASSIS_CTRL_INIT_SOFT_SHUTDOWN;

      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"initial_inbound_seq_num", 
		   &initial_inbound_seq_num);
      Fiid_obj_get(ip->actv_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_hdr_session(at, 
                           initial_inbound_seq_num + ip->session_inbound_count, 
                           (uint32_t)session_id, 
                           password,
                           strlen(conf->password), 
                           tmpl_cmd_chassis_ctrl_rq, 
                           ip->session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_hdr_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_CHASSIS_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_seq_num_counter % (IPMIPOWER_RSEQ_MAX + 1)), 
                           ip->msg_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_chassis_ctrl(command, ip->ctrl_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_chassis_ctrl: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->rmcp_req,
				       ip->session_req, 
                                       ip->msg_req, 
                                       ip->ctrl_req, 
                                       (uint8_t *)buffer, 
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }

  return len;
}

void 
ipmipower_packet_response_data(ipmipower_powercmd_t ip, packet_type_t pkt,
                               uint32_t *session_seq_num, 
                               uint32_t *session_id,
                               uint8_t *network_function, 
                               uint8_t *requester_seq_num,
                               uint8_t *command, 
                               uint8_t *completion_code) 
{
  uint64_t sseq, sid, netfn, rseq, cmd, cc;
  fiid_obj_t obj;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));

  obj = ipmipower_packet_cmd_obj(ip, pkt);
        
  Fiid_obj_get(ip->session_res, 
               (uint8_t *)"session_seq_num", 
	       &sseq);
  Fiid_obj_get(ip->session_res, 
               (uint8_t *)"session_id", 
	       &sid);
  Fiid_obj_get(ip->msg_res, 
               (uint8_t *)"net_fn", 
	       &netfn);
  Fiid_obj_get(ip->msg_res, 
               (uint8_t *)"rq_seq", 
	       &rseq);
  Fiid_obj_get(obj, 
	       (uint8_t *)"cmd", 
	       &cmd);
  Fiid_obj_get(obj, 
	       (uint8_t *)"comp_code", 
	       &cc);
  
  if (session_seq_num) 
    *session_seq_num = sseq;
  if (session_id)
    *session_id = sid;
  if (network_function) 
    *network_function = netfn;
  if (requester_seq_num)
    *requester_seq_num = rseq;
  if (command)
    *command = cmd;
  if (completion_code)
    *completion_code = cc;
}

msg_type_t
ipmipower_packet_errmsg(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  uint8_t cc;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));

  ipmipower_packet_response_data(ip, pkt, 0, 0, 0, 0, 0, &cc);
    
  if (cc == IPMI_COMMAND_SUCCESS)
    err_exit("ipmipower_packet_errmsg(%s:%d:%d): called with cc == SUCCESS",
             ip->ic->hostname, ip->protocol_state, pkt);
  else if (pkt == SESS_RES 
           && (cc == IPMI_ERR_INVALID_USERNAME 
               || cc == IPMI_ERR_NULL_USERNAME_NOT_ENABLED))
    {
#ifndef NDEBUG
      return MSG_TYPE_USERNAME;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == ACTV_RES 
           && cc == IPMI_ERR_EXCEEDS_PRIV_LEVEL)
    {
#ifndef NDEBUG
      return MSG_TYPE_PRIVILEGE;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == PRIV_RES 
           && (cc == IPMI_ERR_RQ_LEVEL_NOT_AVAILABLE_FOR_USER 
               || cc == IPMI_ERR_RQ_LEVEL_EXCEEDS_USER_PRIV_LIMIT 
               || cc == IPMI_ERR_CANNOT_DISABLE_USER_LEVEL_AUTH))
    {
#ifndef NDEBUG
      return MSG_TYPE_PRIVILEGE;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == ACTV_RES 
           && (cc == IPMI_ERR_NO_SESSION_SLOT_AVAILABLE 
               || cc == IPMI_ERR_NO_SLOT_AVAILABLE_FOR_GIVEN_USER 
               || cc == IPMI_ERR_NO_SLOT_AVAILABLE_TO_SUPPORT_USER))
    return MSG_TYPE_BMCBUSY;
  else if (pkt == CTRL_RES 
           && cc == IPMI_ERR_REQUEST_PARAMETER_NOT_SUPPORTED)
    return MSG_TYPE_OPERATION;
  
  return MSG_TYPE_BMCERROR;
}
