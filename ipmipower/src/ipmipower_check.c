/*****************************************************************************\
 *  $Id: ipmipower_check.c,v 1.2 2004-10-08 23:16:44 ab Exp $
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
#include <assert.h>

#include "ipmipower_check.h"
#include "ipmipower_packet.h"
#include "ipmipower_wrappers.h"      

static int 
_check_outbound_seq_num(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  u_int64_t pktoseq = 0;
  u_int64_t myoseq;
  int retval = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
  
  myoseq = IPMIPOWER_INITIAL_OUTBOUND_SEQ_NUM + ip->session_outbound_count;
  
  if (pkt == AUTH_RES || pkt == SESS_RES)
    /* Outbound sequence numbers have not started yet */ 
    return 1;
  else 
    Fiid_obj_get(ip->session_res, tmpl_hdr_session_auth_calc,
                 "session_seq_num", &pktoseq);
  
  if (pktoseq == myoseq)
    retval++;
  else if (ip->retry_count > 0) 
    {
      /* If packets were retransmitted, we won't know for sure what
       * the outbound sequence number will be.  It will be in a
       * potential range.  We'll check the range, then re-sync our
       * outbound sequence number to match the BMC outbound sequence
       * number.
       */
      
      if (myoseq >= ip->retry_count 
          && pktoseq < myoseq
          && pktoseq >= (myoseq - ip->retry_count)) 
        {
          retval++;
          ip->session_outbound_count -= (myoseq - pktoseq);
          dbg("_check_outbound_seq_num(%s:%d): oseq in range, "
              "oseq: %d, expected: %d",
              ip->ic->hostname, ip->protocol_state, pktoseq, myoseq);
        }
      else 
        {
          u_int32_t max = 0xffffffff;
          u_int32_t num = max - (ip->retry_count - myoseq);
          if (pktoseq < myoseq || pktoseq > num) 
            {
              retval++;
              if (pktoseq < myoseq)
                ip->session_outbound_count -= (myoseq - pktoseq);
              else
                ip->session_outbound_count -= (myoseq + (max - num));
              dbg("_check_outbound_seq_num(%s:%d): oseq in range, "
                  "oseq: %d, expected: %d",
                  ip->ic->hostname, ip->protocol_state, pktoseq, myoseq);
            }
          else
            dbg("_check_outbound_seq_num(%s:%d): oseq: %d, "
                "expected: %d",
                ip->ic->hostname, ip->protocol_state, pktoseq, myoseq);
        }
    }

  return retval;
}

static int 
_check_session_id(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  u_int64_t session_id = 0;
  u_int64_t actv_res_session_id = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  if (pkt == AUTH_RES || pkt == SESS_RES || pkt == ACTV_RES)
    return 1;
  else
    {
      Fiid_obj_get(ip->session_res, tmpl_hdr_session_auth_calc,
                   "session_id", &session_id);
      Fiid_obj_get(ip->actv_res, tmpl_cmd_activate_session_rs,
                   "session_id", &actv_res_session_id);
    }
  
  if (session_id != actv_res_session_id)
    dbg("_check_session_id(%s:%d): session id bad: %x expected: %x",
        ip->ic->hostname, ip->protocol_state, session_id, 
        actv_res_session_id);
  
  return ((session_id == actv_res_session_id) ? 1 : 0);
}

static int 
_check_network_function(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  u_int64_t netfn = 0;
  u_int64_t expected_netfn;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  Fiid_obj_get(ip->msg_res, tmpl_lan_msg_hdr_rs, "net_fn", &netfn);

  if (pkt == CHAS_RES || pkt == CTRL_RES)
    expected_netfn = IPMI_NET_FN_CHASSIS_RS;
  else
    expected_netfn = IPMI_NET_FN_APP_RS;
  
  if (netfn != expected_netfn)
    dbg("_check_network_function(%s:%d): netfn bad: %x", 
        ip->ic->hostname, ip->protocol_state, netfn);

  return ((netfn == expected_netfn) ? 1 : 0);
}

static int 
_check_requester_seq_num(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  u_int64_t pktrseq = 0;
  u_int64_t myrseq = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  myrseq = ip->ic->ipmi_send_count % (IPMIPOWER_RSEQ_MAX + 1);

  Fiid_obj_get(ip->msg_res, tmpl_lan_msg_hdr_rs, "rq_seq", &pktrseq);

  if (pktrseq != myrseq)
    dbg("_check_requester_seq_num(%s:%d): rseq: %d, expected: %d",
        ip->ic->hostname, ip->protocol_state, pktrseq, myrseq);
  
  return ((pktrseq == myrseq) ? 1 : 0);
}

static int 
_check_command(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  u_int64_t cmd = 0;
  u_int64_t expected_cmd = -1;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
  
  Fiid_obj_get(ipmipower_packet_cmd_obj(ip, pkt),
               ipmipower_packet_cmd_template(ip, pkt),
               "cmd", &cmd);

  if (pkt == AUTH_RES) 
    expected_cmd = IPMI_CMD_GET_CHANNEL_AUTH_CAPS;
  else if (pkt == SESS_RES) 
    expected_cmd = IPMI_CMD_GET_SESSION_CHALLENGE;
  else if (pkt == ACTV_RES) 
    expected_cmd = IPMI_CMD_ACTIVATE_SESSION;
  else if (pkt == PRIV_RES) 
    expected_cmd = IPMI_CMD_SET_SESSION_PRIV_LEVEL;
  else if (pkt == CLOS_RES) 
    expected_cmd = IPMI_CMD_CLOSE_SESSION;
  else if (pkt == CHAS_RES) 
    expected_cmd = IPMI_CMD_GET_CHASSIS_STATUS;
  else if (pkt == CTRL_RES) 
    expected_cmd = IPMI_CMD_CHASSIS_CTRL;
  
  if (cmd != expected_cmd)
    dbg("_check_command(%s:%d): cmd bad: %x", 
        ip->ic->hostname, ip->protocol_state, cmd);
  
  return ((cmd == expected_cmd) ? 1 : 0);
}

static int 
_check_completion_code(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  u_int64_t cc = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  Fiid_obj_get(ipmipower_packet_cmd_obj(ip, pkt),
               ipmipower_packet_cmd_template(ip, pkt),
               "comp_code", &cc);
  
  if (cc != IPMI_COMMAND_SUCCESS)
    dbg("_check_completion_code(%s:%d): cc bad: %x", 
        ip->ic->hostname, ip->protocol_state, cc);
  
  return ((cc == IPMI_COMMAND_SUCCESS) ? 1 : 0);
}

int 
ipmipower_check_packet(ipmipower_powercmd_t ip, packet_type_t pkt,
                       int oseq, int sid, int netfn, int rseq, 
                       int cmd, int cc) 
{
  int e = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  if (oseq && !_check_outbound_seq_num(ip, pkt)) 
    e++;
  if (sid && !_check_session_id(ip, pkt))
    e++;
  if (netfn && !_check_network_function(ip, pkt)) 
    e++;
  if (rseq && !_check_requester_seq_num(ip, pkt)) 
    e++;
  if (cmd && !_check_command(ip, pkt)) 
    e++;
  if (cc & !_check_completion_code(ip, pkt)) 
    e++;
  
  if (e)
    dbg("ipmipower_check_packet(%s:%d): packet failed checks",
        ip->ic->hostname, ip->protocol_state);
  
  return ((e) ? 0 : 1);
}
