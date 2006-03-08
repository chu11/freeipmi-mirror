/*****************************************************************************\
 *  $Id: ipmipower_check.c,v 1.25 2006-03-08 19:05:57 chu11 Exp $
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

#include "ipmipower_check.h"
#include "ipmipower_packet.h"
#include "ipmipower_wrappers.h"      

extern struct ipmipower_config *conf;

static int 
_check_outbound_sequence_number(ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint32_t shift_num, wrap_val, max_sequence_number = 0xFFFFFFFF;
  uint64_t pktoseq = 0;
  int retval = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
  
  /* achu: This algorithm is more or less from Appendix A of the IPMI
   * spec.  It may not be entirely necessary for ipmipower, since the
   * requester sequence number puts packets into lock-step mode.  Oh
   * well.
   *
   * I know that technically I could remove a lot of code here if I
   * just let unsigned ints be unsigned ints (i.e. 0x00 - 0xff = 1).
   * I dunno, I like to see all of the code actually written out b/c
   * it makes more sense to the casual code reviewer.  Maybe I'll
   * change it later.
   */

  /* Outbound sequence numbers have not started yet */ 
  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES 
      || pkt == AUTHENTICATION_CAPABILITIES_RES 
      || pkt == GET_SESSION_CHALLENGE_RES
      || pkt == GET_CHANNEL_CIPHER_SUITES_RES)
    return 1;

  Fiid_obj_get(ip->obj_lan_session_hdr_res,
               "session_sequence_number", 
	       &pktoseq);
  
  if (pkt == ACTIVATE_SESSION_RES)
    {
      /* achu: On some buggy BMCs the initial outbound sequence number on
       * the activate session response is off by one.  So we just accept
       * whatever sequence number they give us even if it isn't the
       * initial outbound sequence number.
       */
      ip->highest_received_sequence_number = pktoseq;
      return 1;
    }
  
  /* Drop duplicate packet */
  if (pktoseq == ip->highest_received_sequence_number)
    goto out;

  /* Check if sequence number is greater than highest received and is
   * within range 
   */
  if (ip->highest_received_sequence_number > (max_sequence_number - IPMIPOWER_SEQUENCE_NUMBER_WINDOW))
    {
      wrap_val = IPMIPOWER_SEQUENCE_NUMBER_WINDOW - (max_sequence_number - ip->highest_received_sequence_number) - 1;

      if (pktoseq > ip->highest_received_sequence_number || pktoseq <= wrap_val)
        {
          if (pktoseq > ip->highest_received_sequence_number && pktoseq <= max_sequence_number)
            shift_num = pktoseq - ip->highest_received_sequence_number;
          else
            shift_num = pktoseq + (max_sequence_number - ip->highest_received_sequence_number) + 1;
          
          ip->highest_received_sequence_number = pktoseq;
          ip->previously_received_list <<= shift_num;
          ip->previously_received_list |= (0x1 << (shift_num - 1));
          retval++;
        }
    }
  else
    {
      if (pktoseq > ip->highest_received_sequence_number
          && (pktoseq - ip->highest_received_sequence_number) <= IPMIPOWER_SEQUENCE_NUMBER_WINDOW)
        {
          shift_num = (pktoseq - ip->highest_received_sequence_number);
          ip->highest_received_sequence_number = pktoseq;
          ip->previously_received_list <<= shift_num;
          ip->previously_received_list |= (0x1 << (shift_num - 1));
          retval++;
        }
    }
  
  /* Check if sequence number is lower than highest received, is
   * within range, and hasn't been seen yet
   */
  if (ip->highest_received_sequence_number < IPMIPOWER_SEQUENCE_NUMBER_WINDOW)
    {
      uint32_t wrap_val = max_sequence_number - (IPMIPOWER_SEQUENCE_NUMBER_WINDOW - ip->highest_received_sequence_number) + 1;
      
      if (pktoseq < ip->highest_received_sequence_number || pktoseq >= wrap_val)
        {
          if (pktoseq > ip->highest_received_sequence_number && pktoseq <= max_sequence_number)
            shift_num = ip->highest_received_sequence_number + (max_sequence_number - pktoseq) + 1;
          else
            shift_num = ip->highest_received_sequence_number - pktoseq;
          
          /* Duplicate packet check*/
          if (ip->previously_received_list & (0x1 << (shift_num - 1)))
            goto out;
          
          ip->previously_received_list |= (0x1 << (shift_num - 1));
          retval++;
        }
    }
  else
    {
      if (pktoseq < ip->highest_received_sequence_number
          && pktoseq >= (ip->highest_received_sequence_number - IPMIPOWER_SEQUENCE_NUMBER_WINDOW))
        {
          shift_num = ip->highest_received_sequence_number - pktoseq;
          
          /* Duplicate packet check*/
          if (ip->previously_received_list & (0x1 << (shift_num - 1)))
            goto out;
          
          ip->previously_received_list |= (0x1 << (shift_num - 1));
          retval++;
        }
    }
  
 out:
  if (!retval)
    dbg("_check_outbound_sequence_number(%s:%d): pktoseq: %u, high: %u",
        ip->ic->hostname, ip->protocol_state, (unsigned int)pktoseq, ip->highest_received_sequence_number);
  
  return retval;
}

static int 
_check_session_id(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  uint64_t session_id = 0;
  uint64_t actv_res_session_id = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES 
      || pkt == AUTHENTICATION_CAPABILITIES_RES 
      || pkt == GET_SESSION_CHALLENGE_RES 
      || pkt == ACTIVATE_SESSION_RES
      || pkt == GET_CHANNEL_CIPHER_SUITES_RES)      
    return 1;
  else
    {
      Fiid_obj_get(ip->obj_lan_session_hdr_res, 
                   "session_id", 
		   &session_id);
      Fiid_obj_get(ip->obj_activate_session_res, 
                   "session_id", 
		   &actv_res_session_id);
    }
  
  if (session_id != actv_res_session_id && session_id != 0)
    dbg("_check_session_id(%s:%d): session id bad: %x expected: %x",
        ip->ic->hostname, ip->protocol_state, (unsigned int)session_id, 
        (unsigned int)actv_res_session_id);
  
  /* IPMI Workaround (achu)
   *
   * Discovered on Tyan S2882 w/ m3289 BMC
   *
   * The remote BMC returns zeroes for the session id instead of the
   * actual session id.  To work around this problem, we'll assume the
   * session id is correct if it is equal to zero.
   */

  if (conf->accept_session_id_zero == IPMIPOWER_TRUE && !session_id)
    return (1);

  return (session_id == actv_res_session_id);
}

static int 
_check_network_function(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  uint64_t netfn = 0;
  uint64_t expected_netfn;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  Fiid_obj_get(ip->obj_lan_msg_hdr_res, "net_fn", &netfn);

  if (pkt == CHASSIS_STATUS_RES 
      || pkt == CHASSIS_CONTROL_RES)
    expected_netfn = IPMI_NET_FN_CHASSIS_RS;
  else
    expected_netfn = IPMI_NET_FN_APP_RS;
  
  if (netfn != expected_netfn)
    dbg("_check_network_function(%s:%d): netfn bad: %x, expected: %x", 
        ip->ic->hostname, ip->protocol_state, (unsigned int)netfn, expected_netfn);

  return ((netfn == expected_netfn) ? 1 : 0);
}

static int 
_check_requester_sequence_number(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  uint64_t pktrseq = 0;
  uint64_t myrseq = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  myrseq = ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);

  Fiid_obj_get(ip->obj_lan_msg_hdr_res, "rq_seq", &pktrseq);

  if (pktrseq != myrseq)
    dbg("_check_requester_sequence_number(%s:%d): rseq: %x, expected: %x",
        ip->ic->hostname, ip->protocol_state, (unsigned int)pktrseq, (unsigned int)myrseq);
  
  return ((pktrseq == myrseq) ? 1 : 0);
}

static int 
_check_command(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  uint64_t cmd = 0;
  uint64_t expected_cmd = -1;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
  
  Fiid_obj_get(ipmipower_packet_cmd_obj(ip, pkt),
               "cmd", 
	       &cmd);

  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES
      || pkt == AUTHENTICATION_CAPABILITIES_RES)
    expected_cmd = IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES;
  else if (pkt == GET_SESSION_CHALLENGE_RES) 
    expected_cmd = IPMI_CMD_GET_SESSION_CHALLENGE;
  else if (pkt == ACTIVATE_SESSION_RES) 
    expected_cmd = IPMI_CMD_ACTIVATE_SESSION;
  else if (pkt == SET_SESSION_PRIVILEGE_RES) 
    expected_cmd = IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL;
  else if (pkt == CLOSE_SESSION_RES) 
    expected_cmd = IPMI_CMD_CLOSE_SESSION;
  else if (pkt == GET_CHANNEL_CIPHER_SUITES_RES) 
    expected_cmd = IPMI_CMD_GET_CHANNEL_CIPHER_SUITES;
  else if (pkt == CHASSIS_STATUS_RES) 
    expected_cmd = IPMI_CMD_GET_CHASSIS_STATUS;
  else if (pkt == CHASSIS_CONTROL_RES) 
    expected_cmd = IPMI_CMD_CHASSIS_CONTROL;
  
  if (cmd != expected_cmd)
    dbg("_check_command(%s:%d): cmd bad: %x", 
        ip->ic->hostname, ip->protocol_state, (unsigned int)cmd);
  
  return ((cmd == expected_cmd) ? 1 : 0);
}

static int 
_check_completion_code(ipmipower_powercmd_t ip, packet_type_t pkt) 
{
  uint64_t cc = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));
    
  Fiid_obj_get(ipmipower_packet_cmd_obj(ip, pkt),
               "comp_code", 
	       &cc);
  
  if (cc != IPMI_COMP_CODE_COMMAND_SUCCESS)
    dbg("_check_completion_code(%s:%d): cc bad: %x", 
        ip->ic->hostname, ip->protocol_state, (unsigned int)cc);
  
  return ((cc == IPMI_COMP_CODE_COMMAND_SUCCESS) ? 1 : 0);
}

int 
ipmipower_check_packet(ipmipower_powercmd_t ip, packet_type_t pkt,
                       int *oseq, int *sid, int *netfn, int *rseq, 
                       int *cmd, int *cc)
{
  int e = 0;

  assert(ip != NULL);
  assert(PACKET_TYPE_VALID_RES(pkt));

  if (oseq && !(*oseq = _check_outbound_sequence_number(ip, pkt)))
    e++;
  if (sid && !(*sid = _check_session_id(ip, pkt)))
    e++;
  if (netfn && !(*netfn = _check_network_function(ip, pkt)))
    e++;
  if (rseq && !(*rseq = _check_requester_sequence_number(ip, pkt)))
    e++;
  if (cmd && !(*cmd = _check_command(ip, pkt)))
    e++;
  if (cc && !(*cc = _check_completion_code(ip, pkt))) 
    e++;
  
  if (e)
    dbg("ipmipower_check_packet(%s:%d): packet failed checks",
        ip->ic->hostname, ip->protocol_state);
  
  return ((e) ? 0 : 1);
}
