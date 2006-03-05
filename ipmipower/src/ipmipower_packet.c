/*****************************************************************************\
 *  $Id: ipmipower_packet.c,v 1.23 2006-03-05 22:44:43 chu11 Exp $
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
#include <errno.h>
#include <assert.h>
#include <stdint.h>

#include "ipmipower_packet.h"
#include "ipmipower_authentication.h"
#include "ipmipower_wrappers.h"

extern struct ipmipower_config *conf;

/* fiid_template_t */
fiid_field_t *
ipmipower_packet_cmd_template(ipmipower_powercmd_t ip, packet_type_t pkt)
{
  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_PKT(pkt));

  if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
    return &tmpl_cmd_get_channel_authentication_capabilities_rq[0];
  else if (pkt == AUTHENTICATION_CAPABILITIES_RES)
    return &tmpl_cmd_get_channel_authentication_capabilities_rs[0];
  else if (pkt == GET_SESSION_CHALLENGE_REQ)
    return &tmpl_cmd_get_session_challenge_rq[0];
  else if (pkt == GET_SESSION_CHALLENGE_RES)
    return &tmpl_cmd_get_session_challenge_rs[0];
  else if (pkt == ACTIVATE_SESSION_REQ)
    return &tmpl_cmd_activate_session_rq[0];
  else if (pkt == ACTIVATE_SESSION_RES)
    return &tmpl_cmd_activate_session_rs[0];
  else if (pkt == SET_SESSION_PRIVILEGE_REQ)
    return &tmpl_cmd_set_session_privilege_level_rq[0];
  else if (pkt == SET_SESSION_PRIVILEGE_RES)
    return &tmpl_cmd_set_session_privilege_level_rs[0];
  else if (pkt == CLOSE_SESSION_REQ)
    return &tmpl_cmd_close_session_rq[0];
  else if (pkt == CLOSE_SESSION_RES)
    return &tmpl_cmd_close_session_rs[0];
  else if (pkt == CHASSIS_STATUS_REQ)
    return &tmpl_cmd_get_chassis_status_rq[0];
  else if (pkt == CHASSIS_STATUS_RES)
    return &tmpl_cmd_get_chassis_status_rs[0];
  else if (pkt == CHASSIS_CONTROL_REQ)
    return &tmpl_cmd_chassis_control_rq[0];
  else if (pkt == CHASSIS_CONTROL_RES)
    return &tmpl_cmd_chassis_control_rs[0];
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

  if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
    return ip->obj_authentication_capabilities_req;
  else if (pkt == AUTHENTICATION_CAPABILITIES_RES)
    return ip->obj_authentication_capabilities_res;
  else if (pkt == GET_SESSION_CHALLENGE_REQ)
    return ip->obj_get_session_challenge_req;
  else if (pkt == GET_SESSION_CHALLENGE_RES)
    return ip->obj_get_session_challenge_res;
  else if (pkt == ACTIVATE_SESSION_REQ)
    return ip->obj_activate_session_req;
  else if (pkt == ACTIVATE_SESSION_RES)
    return ip->obj_activate_session_res;
  else if (pkt == SET_SESSION_PRIVILEGE_REQ)
    return ip->obj_set_session_privilege_req;
  else if (pkt == SET_SESSION_PRIVILEGE_RES)
    return ip->obj_set_session_privilege_res;
  else if (pkt == CLOSE_SESSION_REQ)
    return ip->obj_close_session_req;
  else if (pkt == CLOSE_SESSION_RES)
    return ip->obj_close_session_res;
  else if (pkt == CHASSIS_STATUS_REQ)
    return ip->obj_chassis_status_req;
  else if (pkt == CHASSIS_STATUS_RES)
    return ip->obj_chassis_status_res;
  else if (pkt == CHASSIS_CONTROL_REQ)
    return ip->obj_chassis_control_req;
  else if (pkt == CHASSIS_CONTROL_RES)
    return ip->obj_chassis_control_res;
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
      if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
        hdr = 
          "============================================\n"
          "= Get Authentication Capabilities Request  =\n"
          "============================================";
      else if (pkt == AUTHENTICATION_CAPABILITIES_RES)
        hdr = 
          "============================================\n"
          "= Get Authentication Capabilities Response =\n"
          "============================================";
      else if (pkt == GET_SESSION_CHALLENGE_REQ)
        hdr = 
          "============================================\n"
          "= Get Session Challenge Request            =\n"
          "============================================";
      else if (pkt == GET_SESSION_CHALLENGE_RES)
        hdr = 
          "============================================\n"
          "= Get Session Challenge Response           =\n"
          "============================================";
      else if (pkt == ACTIVATE_SESSION_REQ)
        hdr = 
          "============================================\n"
          "= Activate Session Request                 =\n"
          "============================================";
      else if (pkt == ACTIVATE_SESSION_RES)
        hdr = 
          "============================================\n"
          "= Activate Session Response                =\n"
          "============================================";
      else if (pkt == SET_SESSION_PRIVILEGE_REQ)
        hdr = 
          "============================================\n"
          "= Set Session Privilege Request            =\n"
          "============================================";
      else if (pkt == SET_SESSION_PRIVILEGE_RES)
        hdr = 
          "============================================\n"
          "= Set Session Privilege Response           =\n"
          "============================================";
      else if (pkt == CLOSE_SESSION_REQ)
        hdr = 
          "============================================\n"
          "= Close Session Request                    =\n"
          "============================================";
      else if (pkt == CLOSE_SESSION_RES)
        hdr = 
          "============================================\n"
          "= Close Session Response                   =\n"
          "============================================";
      else if (pkt == CHASSIS_STATUS_REQ)
        hdr = 
          "============================================\n"
          "= Get Chassis Status Request               =\n"
          "============================================";
      else if (pkt == CHASSIS_STATUS_RES)
        hdr = 
          "============================================\n"
          "= Get Chassis Status Response              =\n"
          "============================================";
      else if (pkt == CHASSIS_CONTROL_REQ)
        hdr = 
          "============================================\n"
          "= Chassis Control Request                  =\n"
          "============================================";
      else if (pkt == CHASSIS_CONTROL_RES)
        hdr = 
          "============================================\n"
          "= Chassis Control Response                 =\n"
          "============================================";
      
      if (pkt & PACKET_TYPE_REQ_MASK)
        Ipmi_dump_lan_packet(STDERR_FILENO, 
                             ip->ic->hostname, 
                             hdr, 
                             (uint8_t *)buffer, 
                             len,
                             tmpl_lan_msg_hdr_rq,
                             ipmipower_packet_cmd_template(ip, pkt));
      else
        Ipmi_dump_lan_packet(STDERR_FILENO, 
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
  
  Fiid_obj_clear(ip->obj_rmcp_hdr_res);
  Fiid_obj_clear(ip->obj_lan_session_hdr_res);
  Fiid_obj_clear(ip->obj_lan_msg_hdr_res);
  Fiid_obj_clear(ip->obj_lan_msg_trlr_res);
  Fiid_obj_clear(obj);

  if (unassemble_ipmi_lan_pkt((uint8_t *)buffer, 
			      len, 
			      ip->obj_rmcp_hdr_res, 
			      ip->obj_lan_session_hdr_res, 
			      ip->obj_lan_msg_hdr_res, 
			      ipmipower_packet_cmd_obj(ip, pkt), 
			      ip->obj_lan_msg_trlr_res) < 0)
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

  Fiid_obj_clear(ip->obj_rmcp_hdr_req);
  Fiid_obj_clear(ip->obj_lan_session_hdr_req);
  Fiid_obj_clear(ip->obj_lan_msg_hdr_req);
  Fiid_obj_clear(obj);

  if (fill_rmcp_hdr_ipmi(ip->obj_rmcp_hdr_req) < 0)
    err_exit("ipmipower_packet_create(%s: %d): fill_rmcp_hdr_ipmi: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
    {
      if (fill_lan_session_hdr(IPMI_AUTHENTICATION_TYPE_NONE, 
                               0, 
                               0, 
                               NULL, 
                               0, 
                               ip->obj_lan_session_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_session_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                           ip->obj_lan_msg_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (fill_cmd_get_channel_authentication_capabilities(IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                           ip->privilege, 
                                                           ip->obj_authentication_capabilities_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_channel_authentication_capabilities: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req, 
				       ip->obj_lan_session_hdr_req, 
				       ip->obj_lan_msg_hdr_req, 
                                       ip->obj_authentication_capabilities_req, 
				       NULL,
				       0,
                                       (uint8_t *)buffer, 
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == GET_SESSION_CHALLENGE_REQ)
    {
      uint8_t *username;

      if (fill_lan_session_hdr(IPMI_AUTHENTICATION_TYPE_NONE, 
                               0, 
                               0, 
                               NULL,
                               0, 
                               ip->obj_lan_session_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_session_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                           ip->obj_lan_msg_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (strlen(conf->username))
        username = (uint8_t *)conf->username;
      else
        username = NULL;

      if (fill_cmd_get_session_challenge(ip->authentication_type, 
                                         (char *)username, 
                                         strlen(conf->username),
                                         ip->obj_get_session_challenge_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_session_challenge: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req, 
				       ip->obj_lan_session_hdr_req, 
				       ip->obj_lan_msg_hdr_req, 
                                       ip->obj_get_session_challenge_req,
				       NULL,
				       0,
                                       (uint8_t *)buffer, 
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == ACTIVATE_SESSION_REQ)
    {
      uint64_t temp_session_id;
      uint8_t *password;
      uint8_t challenge_string[IPMI_CHALLENGE_STRING_LENGTH];
      int32_t challenge_string_len;

      if (strlen(conf->password))
        password = (uint8_t *)conf->password;
      else
        password = NULL;

      Fiid_obj_get(ip->obj_get_session_challenge_res, 
		   (uint8_t *)"temp_session_id", 
		   &temp_session_id);
      
      if (fill_lan_session_hdr(ip->authentication_type, 
                               0,
                               (uint32_t)temp_session_id, 
                               NULL,
                               0,
                               ip->obj_lan_session_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_session_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                           ip->obj_lan_msg_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((challenge_string_len = fiid_obj_get_data(ip->obj_get_session_challenge_res,
                                                    (uint8_t *)"challenge_string",
                                                    challenge_string,
                                                    IPMI_CHALLENGE_STRING_LENGTH)) < 0)
	err_exit("ipmipower_packet_create(%s: %d): fiid_obj_get_data: %s",
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (!challenge_string_len)
	err_exit("ipmipower_packet_create(%s: %d): empty challenge string",
                 ip->ic->hostname, ip->protocol_state);

      if (fill_cmd_activate_session(ip->authentication_type, 
				    ip->privilege, 
				    challenge_string,
				    challenge_string_len,
                                    ip->initial_outbound_sequence_number,
                                    ip->obj_activate_session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_activate_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req, 
				       ip->obj_lan_session_hdr_req, 
				       ip->obj_lan_msg_hdr_req, 
                                       ip->obj_activate_session_req, 
				       password,
				       strlen(conf->password), 
                                       (uint8_t *)buffer,
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == SET_SESSION_PRIVILEGE_REQ)
    {
      uint64_t initial_inbound_sequence_number;
      uint64_t session_id;
      uint8_t *password;
      uint8_t priv;

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_AUTHENTICATION_TYPE_NONE; 
      else
        at = ip->authentication_type;

      if (at != IPMI_AUTHENTICATION_TYPE_NONE)
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
        priv = IPMI_PRIVILEGE_LEVEL_USER;
      else
        priv = IPMI_PRIVILEGE_LEVEL_OPERATOR;

      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"initial_inbound_sequence_number", 
		   &initial_inbound_sequence_number);
      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_lan_session_hdr(at, 
                               initial_inbound_sequence_number + ip->session_inbound_count, 
                               (uint32_t)session_id, 
                               NULL,
                               0,
                               ip->obj_lan_session_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_session_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                           ip->obj_lan_msg_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_set_session_privilege_level(priv, 
					       ip->obj_set_session_privilege_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_set_session_privilege_level: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req, 
				       ip->obj_lan_session_hdr_req, 
				       ip->obj_lan_msg_hdr_req, 
                                       ip->obj_set_session_privilege_req,
				       password,
				       strlen(conf->password), 
                                       (uint8_t *)buffer, 
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == CLOSE_SESSION_REQ)
    {
      uint64_t initial_inbound_sequence_number;
      uint64_t session_id;
      uint8_t *password;

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_AUTHENTICATION_TYPE_NONE; 
      else
        at = ip->authentication_type;

      if (at != IPMI_AUTHENTICATION_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"initial_inbound_sequence_number", 
		   &initial_inbound_sequence_number);
      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_lan_session_hdr(at, 
                               initial_inbound_sequence_number + ip->session_inbound_count, 
                               (uint32_t)session_id, 
                               NULL,
                               0,
                               ip->obj_lan_session_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_session_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                           ip->obj_lan_msg_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_close_session((uint32_t)session_id, ip->obj_close_session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_close_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req, 
				       ip->obj_lan_session_hdr_req, 
				       ip->obj_lan_msg_hdr_req, 
                                       ip->obj_close_session_req, 
				       password,
				       strlen(conf->password), 
                                       (uint8_t *)buffer,
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == CHASSIS_STATUS_REQ)
    {
      uint64_t initial_inbound_sequence_number;
      uint64_t session_id;
      uint8_t *password;

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_AUTHENTICATION_TYPE_NONE; 
      else
        at = ip->authentication_type;

      if (at != IPMI_AUTHENTICATION_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"initial_inbound_sequence_number", 
		   &initial_inbound_sequence_number);
      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_lan_session_hdr(at,
                               initial_inbound_sequence_number + ip->session_inbound_count, 
                               (uint32_t)session_id, 
                               NULL,
                               0,
                               ip->obj_lan_session_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_session_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_CHASSIS_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                           ip->obj_lan_msg_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_get_chassis_status(ip->obj_chassis_status_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_chassis_status: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req,
				       ip->obj_lan_session_hdr_req, 
				       ip->obj_lan_msg_hdr_req, 
                                       ip->obj_chassis_status_req,
				       password,
				       strlen(conf->password), 
                                       (uint8_t *)buffer,
				       buflen)) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "assemble_ipmi_lan_pkt: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
    }
  else if (pkt == CHASSIS_CONTROL_REQ) 
    {
      uint8_t command = 0;
      uint64_t initial_inbound_sequence_number;
      uint64_t session_id;
      uint8_t *password;

      assert(ip->cmd == POWER_CMD_POWER_OFF 
             || ip->cmd == POWER_CMD_POWER_ON  
             || ip->cmd == POWER_CMD_POWER_CYCLE 
             || ip->cmd == POWER_CMD_POWER_RESET
             || ip->cmd == POWER_CMD_PULSE_DIAG_INTR
             || ip->cmd == POWER_CMD_SOFT_SHUTDOWN_OS);

      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        at = IPMI_AUTHENTICATION_TYPE_NONE; 
      else
        at = ip->authentication_type;

      if (at != IPMI_AUTHENTICATION_TYPE_NONE)
        {
          if (strlen(conf->password))
            password = (uint8_t *)conf->password;
          else
            password = NULL;
        }
      else
        password = NULL;

      if (ip->cmd == POWER_CMD_POWER_OFF)
        command = IPMI_CHASSIS_CONTROL_POWER_DOWN;
      else if (ip->cmd == POWER_CMD_POWER_ON)
        command = IPMI_CHASSIS_CONTROL_POWER_UP;
      else if (ip->cmd == POWER_CMD_POWER_CYCLE)
        command = IPMI_CHASSIS_CONTROL_POWER_CYCLE;
      else if (ip->cmd == POWER_CMD_POWER_RESET)
        command = IPMI_CHASSIS_CONTROL_HARD_RESET;
      else if (ip->cmd == POWER_CMD_PULSE_DIAG_INTR)
        command = IPMI_CHASSIS_CONTROL_PULSE_DIAGNOSTIC_INTERRUPT;
      else if (ip->cmd == POWER_CMD_SOFT_SHUTDOWN_OS)
        command = IPMI_CHASSIS_CONTROL_INITIATE_SOFT_SHUTDOWN;

      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"initial_inbound_sequence_number", 
		   &initial_inbound_sequence_number);
      Fiid_obj_get(ip->obj_activate_session_res, 
                   (uint8_t *)"session_id", 
		   &session_id);
      
      if (fill_lan_session_hdr(at, 
                               initial_inbound_sequence_number + ip->session_inbound_count, 
                               (uint32_t)session_id, 
                               NULL,
                               0,
                               ip->obj_lan_session_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_session_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_lan_msg_hdr(IPMI_NET_FN_CHASSIS_RQ, 
			   IPMI_BMC_IPMB_LUN_BMC, 
                           (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                           ip->obj_lan_msg_hdr_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if (fill_cmd_chassis_control(command, ip->obj_chassis_control_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_chassis_control: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      
      if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req,
				       ip->obj_lan_session_hdr_req, 
                                       ip->obj_lan_msg_hdr_req, 
                                       ip->obj_chassis_control_req, 
				       password,
				       strlen(conf->password), 
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
                               uint32_t *session_sequence_number, 
                               uint32_t *session_id,
                               uint8_t *network_function, 
                               uint8_t *requester_sequence_number,
                               uint8_t *command, 
                               uint8_t *completion_code) 
{
  uint64_t sseq, sid, netfn, rseq, cmd, cc;
  fiid_obj_t obj;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));

  obj = ipmipower_packet_cmd_obj(ip, pkt);
        
  Fiid_obj_get(ip->obj_lan_session_hdr_res, 
               (uint8_t *)"session_sequence_number", 
	       &sseq);
  Fiid_obj_get(ip->obj_lan_session_hdr_res, 
               (uint8_t *)"session_id", 
	       &sid);
  Fiid_obj_get(ip->obj_lan_msg_hdr_res, 
               (uint8_t *)"net_fn", 
	       &netfn);
  Fiid_obj_get(ip->obj_lan_msg_hdr_res, 
               (uint8_t *)"rq_seq", 
	       &rseq);
  Fiid_obj_get(obj, 
	       (uint8_t *)"cmd", 
	       &cmd);
  Fiid_obj_get(obj, 
	       (uint8_t *)"comp_code", 
	       &cc);
  
  if (session_sequence_number) 
    *session_sequence_number = sseq;
  if (session_id)
    *session_id = sid;
  if (network_function) 
    *network_function = netfn;
  if (requester_sequence_number)
    *requester_sequence_number = rseq;
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
    
  if (cc == IPMI_COMP_CODE_COMMAND_SUCCESS)
    err_exit("ipmipower_packet_errmsg(%s:%d:%d): called with cc == SUCCESS",
             ip->ic->hostname, ip->protocol_state, pkt);
  else if (pkt == GET_SESSION_CHALLENGE_RES 
           && (cc == IPMI_COMP_CODE_INVALID_USERNAME 
               || cc == IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED))
    {
#ifndef NDEBUG
      return MSG_TYPE_USERNAME;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == ACTIVATE_SESSION_RES 
           && cc == IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL)
    {
#ifndef NDEBUG
      return MSG_TYPE_PRIVILEGE;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == SET_SESSION_PRIVILEGE_RES 
           && (cc == IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER 
               || cc == IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT 
               || cc == IPMI_COMP_CODE_CANNOT_DISABLE_USER_LEVEL_AUTHENTICATION))
    {
#ifndef NDEBUG
      return MSG_TYPE_PRIVILEGE;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == ACTIVATE_SESSION_RES 
           && (cc == IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE 
               || cc == IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER 
               || cc == IPMI_COMP_CODE_NO_SLOT_AVAILABLE_TO_SUPPORT_USER))
    return MSG_TYPE_BMCBUSY;
  else if (pkt == CHASSIS_CONTROL_RES 
           && cc == IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED)
    return MSG_TYPE_OPERATION;
  
  return MSG_TYPE_BMCERROR;
}
