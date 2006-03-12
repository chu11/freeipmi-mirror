/*****************************************************************************\
 *  $Id: ipmipower_packet.c,v 1.37 2006-03-12 20:36:27 chu11 Exp $
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

  if (pkt == AUTHENTICATION_CAPABILITIES_V20_REQ)
    return &tmpl_cmd_get_channel_authentication_capabilities_v20_rq[0];
  else if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES)
    return &tmpl_cmd_get_channel_authentication_capabilities_v20_rs[0];
  else if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
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
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_REQ)
    return &tmpl_cmd_get_channel_cipher_suites_rq[0];
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_RES)
    return &tmpl_cmd_get_channel_cipher_suites_list_supported_algorithms_rs[0];
  else if (pkt == OPEN_SESSION_REQ)
    return &tmpl_rmcpplus_open_session_rq[0];
  else if (pkt == OPEN_SESSION_RES)
    return &tmpl_rmcpplus_open_session_rs[0];
  else if (pkt == RAKP_MESSAGE_1_REQ)
    return &tmpl_rmcpplus_rakp_message_1[0];
  else if (pkt == RAKP_MESSAGE_2_RES)
    return &tmpl_rmcpplus_rakp_message_2[0];
  else if (pkt == RAKP_MESSAGE_3_REQ)
    return &tmpl_rmcpplus_rakp_message_3[0];
  else if (pkt == RAKP_MESSAGE_4_RES)
    return &tmpl_rmcpplus_rakp_message_4[0];
  else if (pkt == CLOSE_SESSION_REQ)
    return &tmpl_cmd_close_session_rq[0];
  else if (pkt == CLOSE_SESSION_RES)
    return &tmpl_cmd_close_session_rs[0];
  else if (pkt == GET_CHASSIS_STATUS_REQ)
    return &tmpl_cmd_get_chassis_status_rq[0];
  else if (pkt == GET_CHASSIS_STATUS_RES)
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

  if (pkt == AUTHENTICATION_CAPABILITIES_V20_REQ)
    return ip->obj_authentication_capabilities_v20_req;
  else if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES)
    return ip->obj_authentication_capabilities_v20_res;
  else if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
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
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_REQ)
    return ip->obj_get_channel_cipher_suites_req;
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_RES)
    return ip->obj_get_channel_cipher_suites_res;
  else if (pkt == OPEN_SESSION_REQ)
    return ip->obj_open_session_req;
  else if (pkt == OPEN_SESSION_RES)
    return ip->obj_open_session_res;
  else if (pkt == RAKP_MESSAGE_1_REQ)
    return ip->obj_rakp_message_1_req;
  else if (pkt == RAKP_MESSAGE_2_RES)
    return ip->obj_rakp_message_2_res;
  else if (pkt == RAKP_MESSAGE_3_REQ)
    return ip->obj_rakp_message_3_req;
  else if (pkt == RAKP_MESSAGE_4_RES)
    return ip->obj_rakp_message_4_res;
  else if (pkt == CLOSE_SESSION_REQ)
    return ip->obj_close_session_req;
  else if (pkt == CLOSE_SESSION_RES)
    return ip->obj_close_session_res;
  else if (pkt == GET_CHASSIS_STATUS_REQ)
    return ip->obj_get_chassis_status_req;
  else if (pkt == GET_CHASSIS_STATUS_RES)
    return ip->obj_get_chassis_status_res;
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
  fiid_field_t *tmpl_lan_msg_hdr;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_PKT(pkt));
  assert (buffer != NULL);

#ifndef NDEBUG
  if (conf->ipmidump)
    {
      char *hdr;
      if (pkt == AUTHENTICATION_CAPABILITIES_V20_REQ)
        hdr = 
          "================================================\n"
          "= Get Authentication Capabilities V20 Request  =\n"
          "================================================";
      else if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES)
        hdr = 
          "================================================\n"
          "= Get Authentication Capabilities V20 Response =\n"
          "================================================";
      else if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
        hdr = 
          "================================================\n"
          "= Get Authentication Capabilities Request      =\n"
          "================================================";
      else if (pkt == AUTHENTICATION_CAPABILITIES_RES)
        hdr = 
          "================================================\n"
          "= Get Authentication Capabilities Response     =\n"
          "================================================";
      else if (pkt == GET_SESSION_CHALLENGE_REQ)
        hdr = 
          "================================================\n"
          "= Get Session Challenge Request                =\n"
          "================================================";
      else if (pkt == GET_SESSION_CHALLENGE_RES)
        hdr = 
          "================================================\n"
          "= Get Session Challenge Response               =\n"
          "================================================";
      else if (pkt == ACTIVATE_SESSION_REQ)
        hdr = 
          "================================================\n"
          "= Activate Session Request                     =\n"
          "================================================";
      else if (pkt == ACTIVATE_SESSION_RES)
        hdr = 
          "================================================\n"
          "= Activate Session Response                    =\n"
          "================================================";
      else if (pkt == SET_SESSION_PRIVILEGE_REQ)
        hdr = 
          "================================================\n"
          "= Set Session Privilege Request                =\n"
          "================================================";
      else if (pkt == SET_SESSION_PRIVILEGE_RES)
        hdr = 
          "================================================\n"
          "= Set Session Privilege Response               =\n"
          "================================================";
      else if (pkt == GET_CHANNEL_CIPHER_SUITES_REQ)
        hdr = 
          "================================================\n"
          "= Get Channel Cipher Suites Request            =\n"
          "================================================";
      else if (pkt == GET_CHANNEL_CIPHER_SUITES_RES)
        hdr = 
          "================================================\n"
          "= Get Channel Cipher Suites Response           =\n"
          "================================================";
      else if (pkt == OPEN_SESSION_REQ)
        hdr = 
          "================================================\n"
          "= Open Session Request                         =\n"
          "================================================";
      else if (pkt == OPEN_SESSION_RES)
        hdr = 
          "================================================\n"
          "= Open Session Response                        =\n"
          "================================================";
      else if (pkt == RAKP_MESSAGE_1_REQ)
        hdr = 
          "================================================\n"
          "= Rakp Message 1 Request                       =\n"
          "================================================";
      else if (pkt == RAKP_MESSAGE_2_RES)
        hdr = 
          "================================================\n"
          "= Rakp Message 2 Response                      =\n"
          "================================================";
      else if (pkt == RAKP_MESSAGE_3_REQ)
        hdr = 
          "================================================\n"
          "= Rakp Message 3 Request                       =\n"
          "================================================";
      else if (pkt == RAKP_MESSAGE_4_RES)
        hdr = 
          "================================================\n"
          "= Rakp Message 4 Response                      =\n"
          "================================================";
      else if (pkt == CLOSE_SESSION_REQ)
        hdr = 
          "================================================\n"
          "= Close Session Request                        =\n"
          "================================================";
      else if (pkt == CLOSE_SESSION_RES)
        hdr = 
          "================================================\n"
          "= Close Session Response                       =\n"
          "================================================";
      else if (pkt == GET_CHASSIS_STATUS_REQ)
        hdr = 
          "================================================\n"
          "= Get Chassis Status Request                   =\n"
          "================================================";
      else if (pkt == GET_CHASSIS_STATUS_RES)
        hdr = 
          "================================================\n"
          "= Get Chassis Status Response                  =\n"
          "================================================";
      else if (pkt == CHASSIS_CONTROL_REQ)
        hdr = 
          "================================================\n"
          "= Chassis Control Request                      =\n"
          "================================================";
      else if (pkt == CHASSIS_CONTROL_RES)
        hdr = 
          "================================================\n"
          "= Chassis Control Response                     =\n"
          "================================================";
      
      if (pkt & PACKET_TYPE_REQ_MASK)
        tmpl_lan_msg_hdr = &tmpl_lan_msg_hdr_rq[0];
      else
        tmpl_lan_msg_hdr = &tmpl_lan_msg_hdr_rs[0];
        
      if (pkt == OPEN_SESSION_REQ
          || pkt == OPEN_SESSION_RES
          || pkt == RAKP_MESSAGE_1_REQ
          || pkt == RAKP_MESSAGE_2_RES
          || pkt == RAKP_MESSAGE_3_REQ
          || pkt == RAKP_MESSAGE_4_RES
          || (ip->ipmi_version == IPMI_VERSION_2_0
              && (pkt == GET_CHASSIS_STATUS_REQ
                  || pkt == GET_CHASSIS_STATUS_RES
                  || pkt == CHASSIS_CONTROL_REQ
                  || pkt == CHASSIS_CONTROL_RES
                  || pkt == CLOSE_SESSION_REQ
                  || pkt == CLOSE_SESSION_RES)))
        /* XXX temporary - need to make generic */
        Ipmi_dump_rmcpplus_packet(STDERR_FILENO,
                                  ip->ic->hostname,
                                  hdr,
                                  IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE,
                                  IPMI_INTEGRITY_ALGORITHM_NONE,
                                  IPMI_CONFIDENTIALITY_ALGORITHM_NONE,
                                  NULL,
                                  0,
                                  NULL,
                                  0,
                                  (uint8_t *)buffer,
                                  (uint32_t)len,
                                  tmpl_lan_msg_hdr,
                                  ipmipower_packet_cmd_template(ip, pkt));
      else
        Ipmi_dump_lan_packet(STDERR_FILENO, 
                             ip->ic->hostname, 
                             hdr, 
                             (uint8_t *)buffer, 
                             (uint32_t)len,
                             tmpl_lan_msg_hdr,
                             ipmipower_packet_cmd_template(ip, pkt));
    }
#endif
}

int
ipmipower_packet_store(ipmipower_powercmd_t ip, packet_type_t pkt,
                       char *buffer, int len) 
{
  fiid_obj_t obj;
  int32_t rv = -1;
  
  assert(ip != NULL);
  assert(buffer != NULL);
  assert(len > 0);
  assert(PACKET_TYPE_VALID_RES(pkt));

  obj = ipmipower_packet_cmd_obj(ip, pkt);
  
  Fiid_obj_clear(ip->obj_rmcp_hdr_res);
  Fiid_obj_clear(ip->obj_lan_session_hdr_res);
  Fiid_obj_clear(ip->obj_lan_msg_hdr_res);
  Fiid_obj_clear(ip->obj_lan_msg_trlr_res);
  if (ip->ipmi_version == IPMI_VERSION_2_0)
    {
      Fiid_obj_clear(ip->obj_rmcpplus_session_hdr_req);
      Fiid_obj_clear(ip->obj_rmcpplus_session_hdr_res);
      Fiid_obj_clear(ip->obj_rmcpplus_payload_res);
      Fiid_obj_clear(ip->obj_rmcpplus_session_trlr_res);
    }
  Fiid_obj_clear(obj);

  /* XXX tighten this */
  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES
      || pkt == AUTHENTICATION_CAPABILITIES_RES
      || pkt == GET_SESSION_CHALLENGE_RES
      || pkt == ACTIVATE_SESSION_RES
      || pkt == GET_CHANNEL_CIPHER_SUITES_RES
      || ip->ipmi_version == IPMI_VERSION_1_5)
    {
      if ((rv = unassemble_ipmi_lan_pkt((uint8_t *)buffer, 
					len, 
					ip->obj_rmcp_hdr_res, 
					ip->obj_lan_session_hdr_res, 
					ip->obj_lan_msg_hdr_res, 
					obj,
					ip->obj_lan_msg_trlr_res)) < 0)
	dbg("ipmipower_packet_store: unassemble_ipmi_lan_pkt: %s", strerror(errno));
    }
  else
    {
      /* XXX temporary - need to make generic */
      if ((rv = unassemble_ipmi_rmcpplus_pkt(ip->authentication_algorithm,
					     ip->integrity_algorithm,
					     ip->confidentiality_algorithm,
					     NULL,
					     0,
					     NULL,
					     0,
					     (uint8_t *)buffer, 
					     len, 
					     ip->obj_rmcp_hdr_res, 
					     ip->obj_rmcpplus_session_hdr_res,
					     ip->obj_rmcpplus_payload_res,
					     ip->obj_lan_msg_hdr_res, 
					     obj,
					     ip->obj_lan_msg_trlr_res,
					     ip->obj_rmcpplus_session_trlr_res)) < 0)
	dbg("ipmipower_packet_store: unassemble_ipmi_rmcpplus_pkt: %s", strerror(errno));
    }
  
  return (rv);
}

static int32_t
_ipmi_1_5_packet_create(ipmipower_powercmd_t ip,
                        packet_type_t pkt,
                        uint8_t authentication_type,
                        uint32_t inbound_sequence_number,
                        uint32_t session_id,
                        uint8_t *authentication_code_data,
                        uint32_t authentication_code_data_len,
                        uint8_t net_fn,
                        fiid_obj_t obj_cmd_req,
                        char *buffer, 
                        int buflen)
{
  int32_t len;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_REQ(pkt));
  assert(fiid_obj_valid(obj_cmd_req));
  assert(buffer != NULL);
  assert(buflen > 0);

  Fiid_obj_clear(ip->obj_rmcp_hdr_req);
  Fiid_obj_clear(ip->obj_lan_session_hdr_req);
  Fiid_obj_clear(ip->obj_lan_msg_hdr_req);

  if (fill_rmcp_hdr_ipmi(ip->obj_rmcp_hdr_req) < 0)
    err_exit("_ipmi_1_5_packet_create(%s: %d): fill_rmcp_hdr_ipmi: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if (fill_rmcp_hdr_ipmi(ip->obj_rmcp_hdr_req) < 0)
    err_exit("_ipmi_1_5_packet_create(%s: %d): fill_rmcp_hdr_ipmi: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if (fill_lan_session_hdr(authentication_type, 
                           inbound_sequence_number, 
                           session_id,
                           NULL,
                           0,
                           ip->obj_lan_session_hdr_req) < 0)
    err_exit("_ipmi_1_5_packet_create(%s: %d): fill_lan_session_hdr: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));
  
  if (fill_lan_msg_hdr(net_fn, 
                       IPMI_BMC_IPMB_LUN_BMC, 
                       (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                       ip->obj_lan_msg_hdr_req) < 0)
    err_exit("_ipmi_1_5_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if ((len = assemble_ipmi_lan_pkt(ip->obj_rmcp_hdr_req, 
                                   ip->obj_lan_session_hdr_req, 
                                   ip->obj_lan_msg_hdr_req, 
                                   obj_cmd_req,
                                   authentication_code_data, 
                                   authentication_code_data_len,
                                   (uint8_t *)buffer, 
                                   buflen)) < 0)
    err_exit("_ipmi_1_5_packet_create(%s: %d): "
             "assemble_ipmi_lan_pkt: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  return (len);
}

static int32_t
_ipmi_2_0_packet_create(ipmipower_powercmd_t ip,
                        packet_type_t pkt,
                        uint8_t payload_type,
                        uint8_t payload_authenticated,
                        uint8_t payload_encrypted,
                        uint32_t session_id,
                        uint32_t session_sequence_number,
                        uint8_t *authentication_code_data,
                        uint32_t authentication_code_data_len,
                        uint8_t net_fn,
                        fiid_obj_t obj_cmd_req,
                        char *buffer, 
                        int buflen)
{
  int32_t len;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_REQ(pkt));
  assert(fiid_obj_valid(obj_cmd_req));
  assert(buffer != NULL);
  assert(buflen > 0);

  Fiid_obj_clear(ip->obj_rmcpplus_session_hdr_req);
  Fiid_obj_clear(ip->obj_rmcpplus_session_trlr_req);

  if (fill_rmcp_hdr_ipmi(ip->obj_rmcp_hdr_req) < 0)
    err_exit("_ipmi_2_0_packet_create(%s: %d): fill_rmcp_hdr_ipmi: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if (fill_rmcpplus_session_hdr(payload_type,
                                payload_authenticated,
                                payload_encrypted,
                                0,
                                0,
                                session_id,
                                session_sequence_number,
                                ip->obj_rmcpplus_session_hdr_req) < 0)
    err_exit("_ipmi_2_0_packet_create(%s:%d: fill_rmcpplus_session_hdr: %s",
             ip->ic->hostname, ip->protocol_state, strerror(errno));
  
  if (fill_lan_msg_hdr(net_fn, 
                       IPMI_BMC_IPMB_LUN_BMC, 
                       (ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1)), 
                       ip->obj_lan_msg_hdr_req) < 0)
    err_exit("_ipmi_2_0_packet_create(%s: %d): fill_lan_msg_hdr: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  if (fill_rmcpplus_session_trlr(NULL, 
                                 0, 
                                 ip->obj_rmcpplus_session_trlr_req) < 0)
    err_exit("_ipmi_2_0_packet_create(%s: %d): fill_rmcpplus_session_trlr: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));
  
  if ((len = assemble_ipmi_rmcpplus_pkt(ip->authentication_algorithm,
                                        ip->integrity_algorithm,
                                        ip->confidentiality_algorithm,
                                        NULL,
                                        0,
                                        NULL,
                                        0,
                                        authentication_code_data,
                                        authentication_code_data_len,
                                        ip->obj_rmcp_hdr_req, 
                                        ip->obj_rmcpplus_session_hdr_req,
                                        ip->obj_lan_msg_hdr_req, 
                                        obj_cmd_req,
                                        ip->obj_rmcpplus_session_trlr_req,
                                        (uint8_t *)buffer, 
                                        buflen)) < 0)
    err_exit("_ipmi_2_0_packet_create(%s: %d): "
             "assemble_ipmi_rmcpplus_pkt: %s", 
             ip->ic->hostname, ip->protocol_state, strerror(errno));

  return (len);
}

int
ipmipower_packet_create(ipmipower_powercmd_t ip, packet_type_t pkt,
                        char *buffer, int buflen) 
{
  uint8_t *username, *password;
  uint64_t session_id, managed_system_session_id;
  uint32_t sequence_number;
  uint8_t authentication_type, net_fn, payload_authenticated, payload_encrypted,
    payload_type;
  fiid_obj_t obj_cmd_req;
  int32_t len;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_REQ(pkt));
  assert(buffer);
  assert(buflen);

  if (pkt == GET_SESSION_CHALLENGE_REQ
      || pkt == RAKP_MESSAGE_1_REQ)
    {
      if (strlen(conf->username))
        username = (uint8_t *)conf->username;
      else
        username = NULL;
    }
  else
    username = NULL;

  /* Calculate Password */
  if (pkt == ACTIVATE_SESSION_REQ
      || pkt == SET_SESSION_PRIVILEGE_REQ
      || pkt == OPEN_SESSION_REQ
      || pkt == RAKP_MESSAGE_1_REQ
      || pkt == RAKP_MESSAGE_3_REQ
      || pkt == CLOSE_SESSION_REQ
      || pkt == GET_CHASSIS_STATUS_REQ
      || pkt == CHASSIS_CONTROL_REQ)
    {
      if (strlen(conf->password))
        password = (uint8_t *)conf->password;
      else
        password = NULL;
    }     
  else
    password = NULL;
    
  /* Calculate Session ID */
  if (pkt == ACTIVATE_SESSION_REQ)
    Fiid_obj_get(ip->obj_get_session_challenge_res, 
                 "temp_session_id", 
                 &session_id);
  else if (pkt == SET_SESSION_PRIVILEGE_REQ
           || (ip->ipmi_version == IPMI_VERSION_1_5
               && (pkt == CLOSE_SESSION_REQ
                   || pkt == GET_CHASSIS_STATUS_REQ
                   || pkt == CHASSIS_CONTROL_REQ)))
    Fiid_obj_get(ip->obj_activate_session_res, 
                 "session_id", 
                 &session_id);
  else if (ip->ipmi_version == IPMI_VERSION_2_0
           && (pkt == CLOSE_SESSION_REQ
               || pkt == GET_CHASSIS_STATUS_REQ
               || pkt == CHASSIS_CONTROL_REQ))
    Fiid_obj_get(ip->obj_open_session_res,
                 "managed_system_session_id",
                 &session_id);
  else
    session_id = 0;

  /* Calculate Sequence Number */
  if (pkt == SET_SESSION_PRIVILEGE_REQ
      || (ip->ipmi_version == IPMI_VERSION_1_5
          && (pkt == CLOSE_SESSION_REQ
              || pkt == GET_CHASSIS_STATUS_REQ
              || pkt == CHASSIS_CONTROL_REQ)))
    {
      uint64_t initial_inbound_sequence_number;
      
      Fiid_obj_get(ip->obj_activate_session_res, 
                   "initial_inbound_sequence_number", 
                   &initial_inbound_sequence_number);
      
      sequence_number = initial_inbound_sequence_number + ip->session_inbound_count;
    }
  else if (ip->ipmi_version == IPMI_VERSION_2_0
           && (pkt == CLOSE_SESSION_REQ
               || pkt == GET_CHASSIS_STATUS_REQ
               || pkt == CHASSIS_CONTROL_REQ))
    sequence_number = ip->session_sequence_number;
  else
    sequence_number = 0;

  /* Calculate Network Function */
  if (pkt == GET_CHASSIS_STATUS_REQ
      || pkt == CHASSIS_CONTROL_REQ)
    net_fn = IPMI_NET_FN_CHASSIS_RQ;
  else
    net_fn = IPMI_NET_FN_APP_RQ;

  /* Calculate Authentication Type */
  if (pkt == ACTIVATE_SESSION_REQ)
    authentication_type = ip->authentication_type;
  else if (pkt == SET_SESSION_PRIVILEGE_REQ
           || (ip->ipmi_version == IPMI_VERSION_1_5
               && (pkt == CLOSE_SESSION_REQ
                   || pkt == GET_CHASSIS_STATUS_REQ
                   || pkt == CHASSIS_CONTROL_REQ)))
    {
      if (ip->permsgauth_enabled == IPMIPOWER_FALSE)
        authentication_type = IPMI_AUTHENTICATION_TYPE_NONE; 
      else
        authentication_type = ip->authentication_type;
      
      if (authentication_type == IPMI_AUTHENTICATION_TYPE_NONE)
        password = NULL;
    }
  else
    authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
    
  if (ip->ipmi_version == IPMI_VERSION_2_0)
    {
      /* XXX need to fix */
      /* Calculate Payload Authenticated */
      payload_authenticated = IPMI_PAYLOAD_FLAG_UNAUTHENTICATED;
      
      /* Calculate Payload Encrypted */
      payload_encrypted = IPMI_PAYLOAD_FLAG_UNENCRYPTED;
      
      /* Calculate Payload Type */
      if (pkt == OPEN_SESSION_REQ)
        payload_type = IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST;
      else if (pkt == RAKP_MESSAGE_1_REQ)
        payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1;
      else if (pkt == RAKP_MESSAGE_3_REQ)
        payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3;
      else
        payload_type = IPMI_PAYLOAD_TYPE_IPMI;

      if (pkt == RAKP_MESSAGE_1_REQ
          || pkt == RAKP_MESSAGE_3_REQ)
        Fiid_obj_get(ip->obj_open_session_res,
                     "managed_system_session_id",
                     &managed_system_session_id);
    }

  /* Calculate/Fill Command Object */
  if (pkt == AUTHENTICATION_CAPABILITIES_V20_REQ)
    {
      if (fill_cmd_get_channel_authentication_capabilities_v20(IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                               ip->privilege, 
                                                               IPMI_GET_IPMI_V20_EXTENDED_DATA,
                                                               ip->obj_authentication_capabilities_v20_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_channel_authentication_capabilities_v20: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_authentication_capabilities_v20_req;
    }
  else if (pkt == AUTHENTICATION_CAPABILITIES_REQ)
    {
      if (fill_cmd_get_channel_authentication_capabilities(IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                           ip->privilege, 
                                                           ip->obj_authentication_capabilities_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_channel_authentication_capabilities: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_authentication_capabilities_req;      
    }
  else if (pkt == GET_SESSION_CHALLENGE_REQ)
    {
      /* Note: The session_authentication_type is none, this authentication type may be different.
       */
      if (fill_cmd_get_session_challenge(ip->authentication_type, 
                                         username, 
                                         strlen(conf->username),
                                         ip->obj_get_session_challenge_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_session_challenge: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_get_session_challenge_req;
    }
  else if (pkt == ACTIVATE_SESSION_REQ)
    {
      uint8_t challenge_string[IPMI_CHALLENGE_STRING_LENGTH];
      int32_t challenge_string_len;
    
      if ((challenge_string_len = fiid_obj_get_data(ip->obj_get_session_challenge_res,
                                                    "challenge_string",
                                                    challenge_string,
                                                    IPMI_CHALLENGE_STRING_LENGTH)) < 0)
	err_exit("ipmipower_packet_create(%s: %d): fiid_obj_get_data: %s",
                 ip->ic->hostname, ip->protocol_state, strerror(errno));

      if (!challenge_string_len)
	err_exit("ipmipower_packet_create(%s: %d): empty challenge string",
                 ip->ic->hostname, ip->protocol_state);
      
      if (fill_cmd_activate_session(authentication_type, 
				    ip->privilege, 
				    challenge_string,
				    challenge_string_len,
                                    IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER,
                                    ip->obj_activate_session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_activate_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_activate_session_req;
    }
  else if (pkt == SET_SESSION_PRIVILEGE_REQ)
    {
      uint8_t priv;

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

      if (fill_cmd_set_session_privilege_level(priv, 
					       ip->obj_set_session_privilege_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_set_session_privilege_level: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_set_session_privilege_req;
    }
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_REQ)
    {
      if (fill_cmd_get_channel_cipher_suites (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                              IPMI_PAYLOAD_TYPE_IPMI,
                                              0, /* XXX need legit index mechanism */
                                              IPMI_LIST_SUPPORTED_ALGORITHMS,
                                              ip->obj_get_channel_cipher_suites_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_channel_cipher_suites: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_get_channel_cipher_suites_req;
    }
  else if (pkt == OPEN_SESSION_REQ)
    {
      if (fill_rmcpplus_open_session (ip->initial_message_tag + ip->message_tag_count,
                                      ip->requested_maximum_privilege,
                                      ip->remote_console_session_id,
                                      ip->authentication_algorithm,
                                      ip->integrity_algorithm,
                                      ip->confidentiality_algorithm,
                                      ip->obj_open_session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_rmcpplus_open_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_open_session_req;
    }
  else if (pkt == RAKP_MESSAGE_1_REQ)
    {
      if (fill_rmcpplus_rakp_message_1 (ip->initial_message_tag + ip->message_tag_count,
                                        managed_system_session_id,
                                        ip->remote_console_random_number,
                                        IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                        ip->requested_maximum_privilege,
                                        ip->name_only_lookup,
                                        username,
                                        strlen(conf->username),
                                        ip->obj_rakp_message_1_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_rmcpplus_rakp_message_1: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_rakp_message_1_req;
    }
  else if (pkt == RAKP_MESSAGE_3_REQ)
    {
      if (fill_rmcpplus_rakp_message_3 (ip->initial_message_tag + ip->message_tag_count,
                                        RMCPPLUS_STATUS_NO_ERRORS,
                                        managed_system_session_id,
                                        NULL,
                                        0,
                                        ip->obj_rakp_message_3_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_rmcpplus_rakp_message_3: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_rakp_message_3_req;      
    }
  else if (pkt == CLOSE_SESSION_REQ)
    {
      if (fill_cmd_close_session((uint32_t)session_id, ip->obj_close_session_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_close_session: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_close_session_req;
    }
  else if (pkt == GET_CHASSIS_STATUS_REQ)
    {
      if (fill_cmd_get_chassis_status(ip->obj_get_chassis_status_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_get_chassis_status: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_get_chassis_status_req;
    }
  else if (pkt == CHASSIS_CONTROL_REQ) 
    {
      uint8_t command = 0;

      assert(ip->cmd == POWER_CMD_POWER_OFF 
             || ip->cmd == POWER_CMD_POWER_ON  
             || ip->cmd == POWER_CMD_POWER_CYCLE 
             || ip->cmd == POWER_CMD_POWER_RESET
             || ip->cmd == POWER_CMD_PULSE_DIAG_INTR
             || ip->cmd == POWER_CMD_SOFT_SHUTDOWN_OS);

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

      if (fill_cmd_chassis_control(command, ip->obj_chassis_control_req) < 0)
        err_exit("ipmipower_packet_create(%s: %d): "
                 "fill_cmd_chassis_control: %s", 
                 ip->ic->hostname, ip->protocol_state, strerror(errno));
      obj_cmd_req = ip->obj_chassis_control_req;
    }

  /* Construct packets */
  if (pkt == AUTHENTICATION_CAPABILITIES_V20_REQ
      || pkt == AUTHENTICATION_CAPABILITIES_REQ
      || pkt == GET_SESSION_CHALLENGE_REQ
      || pkt == ACTIVATE_SESSION_REQ
      || pkt == SET_SESSION_PRIVILEGE_REQ
      || pkt == GET_CHANNEL_CIPHER_SUITES_REQ
      || (ip->ipmi_version == IPMI_VERSION_1_5
          && (pkt == CLOSE_SESSION_REQ
              || pkt == GET_CHASSIS_STATUS_REQ
              || pkt == CHASSIS_CONTROL_REQ)))
    len = _ipmi_1_5_packet_create(ip,
                                  pkt,
                                  authentication_type,
                                  sequence_number,
                                  (uint32_t)session_id,
                                  password,
                                  (password) ? strlen((char *)password) : 0,
                                  net_fn,
                                  obj_cmd_req,
                                  buffer, 
                                  buflen);
  else if (pkt == OPEN_SESSION_REQ
           || pkt == RAKP_MESSAGE_1_REQ
           || pkt == RAKP_MESSAGE_3_REQ
           || (ip->ipmi_version == IPMI_VERSION_2_0
               && (pkt == CLOSE_SESSION_REQ
                   || pkt == GET_CHASSIS_STATUS_REQ
                   || pkt == CHASSIS_CONTROL_REQ)))
    len = _ipmi_2_0_packet_create(ip,
                                  pkt,
                                  payload_type,
                                  payload_authenticated,
                                  payload_encrypted,
                                  (uint32_t)session_id,
                                  sequence_number,
                                  password,
                                  (password) ? strlen((char *)password) : 0,
                                  net_fn, 
                                  obj_cmd_req,
                                  buffer,
                                  buflen);
  else
    err_exit("ipmipower_packet_create(%s: %d): invalid logic", 
             ip->ic->hostname, ip->protocol_state);

  return len;
}

msg_type_t
ipmipower_packet_errmsg(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  uint64_t comp_code;
  fiid_obj_t obj_cmd;
  
  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
  /* Assert this is not an IPMI 2.0 Session Setup Packet */
  assert(pkt != OPEN_SESSION_RES
         && pkt != GET_CHASSIS_STATUS_RES
         && pkt != CHASSIS_CONTROL_RES);

  /* XXX need to fix for ipmi 2.0 */

  obj_cmd = ipmipower_packet_cmd_obj(ip, pkt);
  Fiid_obj_get(obj_cmd, "comp_code", &comp_code);
    
  if (comp_code == IPMI_COMP_CODE_COMMAND_SUCCESS)
    err_exit("ipmipower_packet_errmsg(%s:%d:%d): "
	     "called with comp_code == SUCCESS",
             ip->ic->hostname, ip->protocol_state, pkt);
  else if (pkt == GET_SESSION_CHALLENGE_RES 
           && (comp_code == IPMI_COMP_CODE_INVALID_USERNAME 
               || comp_code == IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED))
    {
#ifndef NDEBUG
      return MSG_TYPE_USERNAME;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == ACTIVATE_SESSION_RES 
           && comp_code == IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL)
    {
#ifndef NDEBUG
      return MSG_TYPE_PRIVILEGE;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == SET_SESSION_PRIVILEGE_RES 
           && (comp_code == IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER 
               || comp_code == IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT 
               || comp_code == IPMI_COMP_CODE_CANNOT_DISABLE_USER_LEVEL_AUTHENTICATION))
    {
#ifndef NDEBUG
      return MSG_TYPE_PRIVILEGE;
#else
      return MSG_TYPE_PERMISSION;
#endif
    }
  else if (pkt == ACTIVATE_SESSION_RES 
           && (comp_code == IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE 
               || comp_code == IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER 
               || comp_code == IPMI_COMP_CODE_NO_SLOT_AVAILABLE_TO_SUPPORT_USER))
    return MSG_TYPE_BMCBUSY;
  else if (pkt == CHASSIS_CONTROL_RES 
           && comp_code == IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED)
    return MSG_TYPE_OPERATION;
  
  return MSG_TYPE_BMCERROR;
}
