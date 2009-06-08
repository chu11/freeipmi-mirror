/*****************************************************************************\
 *  $Id: ipmipower_check.c,v 1.107 2009-06-08 20:24:26 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
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
#include <errno.h>
#include <assert.h>

#include "ipmipower_check.h"
#include "ipmipower_packet.h"

#include "ierror.h"

#include "freeipmi-portability.h"

extern struct ipmipower_arguments cmd_args;

#define IPMIPOWER_SEQUENCE_NUMBER_WINDOW 8
#define IPMIPOWER_MAX_SEQUENCE_NUMBER    0xFFFFFFFF

int
ipmipower_check_checksum (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  fiid_obj_t obj_cmd;
  int rv;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (pkt == AUTHENTICATION_CAPABILITIES_V20_RES
          || pkt == AUTHENTICATION_CAPABILITIES_RES
          || pkt == GET_SESSION_CHALLENGE_RES
          || pkt == ACTIVATE_SESSION_RES
          || pkt == SET_SESSION_PRIVILEGE_LEVEL_RES /* IPMI 1.5 or 2.0 */
          || pkt == GET_CHASSIS_STATUS_RES /* IPMI 1.5 or 2.0 */
          || pkt == CHASSIS_CONTROL_RES /* IPMI 1.5 or 2.0 */
          || pkt == CHASSIS_IDENTIFY_RES /* IPMI 1.5 or 2.0 */
          || pkt == CLOSE_SESSION_RES); /* IPMI 1.5 or 2.0 */

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);
  if ((rv = ipmi_lan_check_checksum (ip->obj_lan_msg_hdr_res,
                                     obj_cmd,
                                     ip->obj_lan_msg_trlr_res)) < 0)
    ierr_exit ("ipmipower_check_checksum(%s:%d): "
               "ipmi_lan_check_checksum: %s",
               ip->ic->hostname, ip->protocol_state, strerror (errno));

  if (!rv)
    ierr_dbg ("ipmipower_check_checksum(%s:%d): checksum check failed",
              ip->ic->hostname, ip->protocol_state);

  return (rv);
}

int
ipmipower_check_authentication_code (ipmipower_powercmd_t ip,
                                     packet_type_t pkt,
                                     const void *buf,
                                     unsigned int buflen)
{
  char *password;
  int rv = -1;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (pkt == ACTIVATE_SESSION_RES
          || pkt == SET_SESSION_PRIVILEGE_LEVEL_RES /* IPMI 1.5 or 2.0 */
          || pkt == GET_CHASSIS_STATUS_RES /* IPMI 1.5 or 2.0 */
          || pkt == CHASSIS_CONTROL_RES /* IPMI 1.5 or 2.0 */
          || pkt == CHASSIS_IDENTIFY_RES /* IPMI 1.5 or 2.0 */
          || pkt == CLOSE_SESSION_RES); /* IPMI 1.5 or 2.0 */
  assert (buf && buflen);

  /* IPMI 1.5 Checks */
  if (pkt == ACTIVATE_SESSION_RES
      || (cmd_args.common.driver_type == IPMI_DEVICE_LAN
          && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
              || pkt == GET_CHASSIS_STATUS_RES
              || pkt == CHASSIS_CONTROL_RES
              || pkt == CHASSIS_IDENTIFY_RES
              || pkt == CLOSE_SESSION_RES)))
    {
      uint8_t authentication_type;
      int check_authcode_retry_flag = 0;

      if (pkt == ACTIVATE_SESSION_RES)
        authentication_type = cmd_args.common.authentication_type;
      else /* pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
              || pkt == GET_CHASSIS_STATUS_RES
              || pkt == CHASSIS_CONTROL_RES
              || pkt == CHASSIS_IDENTIFY_RES
              || pkt == CLOSE_SESSION_RES
           */
        {
          if (!ip->permsgauth_enabled)
            {
              authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
              check_authcode_retry_flag++;
            }
          else
            authentication_type = cmd_args.common.authentication_type;
        }

      if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
        password = cmd_args.common.password;
      else
        password = NULL;

      if ((rv = ipmi_lan_check_packet_session_authentication_code (buf,
                                                                   buflen,
                                                                   authentication_type,
                                                                   password,
                                                                   password ? strlen (password) : 0)) < 0)
        ierr_exit ("ipmipower_check_authentication_code(%s:%d): "
                   "ipmi_lan_check_packet_session_authentication_code: %s",
                   ip->ic->hostname, ip->protocol_state, strerror (errno));

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
      if ((cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
          && !rv
          && check_authcode_retry_flag)
        {
          ierr_dbg ("ipmipower_check_authentication_code(%s:%d): retry authcode check",
                    ip->ic->hostname, ip->protocol_state, strerror (errno));

          authentication_type = cmd_args.common.authentication_type;
          if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
            password = cmd_args.common.password;
          else
            password = NULL;

          if ((rv = ipmi_lan_check_packet_session_authentication_code (buf,
                                                                       buflen,
                                                                       authentication_type,
                                                                       password,
                                                                       password ? strlen (password) : 0)) < 0)
            ierr_exit ("ipmipower_check_authentication_code(%s:%d): "
                       "ipmi_lan_check_session_authentication_code: %s",
                       ip->ic->hostname, ip->protocol_state, strerror (errno));

          if (rv)
            ierr_dbg ("ipmipower_check_authentication_code(%s:%d): "
                      "permsgauth authcode re-check passed",
                      ip->ic->hostname, ip->protocol_state);
        }
    }
  else  /*
          (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0
          && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
          || pkt == GET_CHASSIS_STATUS_RES
          || pkt == CHASSIS_CONTROL_RES
          || pkt == CHASSIS_IDENTIFY_RES
          || pkt == CLOSE_SESSION_RES))
        */
    {
      /* IPMI 2.0 Checks */
      uint8_t integrity_algorithm;

      integrity_algorithm = ip->integrity_algorithm;

      password = cmd_args.common.password;

      if ((rv = ipmi_rmcpplus_check_packet_session_authentication_code (integrity_algorithm,
                                                                        buf,
                                                                        buflen,
                                                                        ip->integrity_key_ptr,
                                                                        ip->integrity_key_len,
                                                                        password,
                                                                        (password) ? strlen (password) : 0,
                                                                        ip->obj_rmcpplus_session_trlr_res)) < 0)
        ierr_exit ("ipmipower_check_authentication_code(%s:%d): "
                   "ipmi_rmcpplus_check_session_authentication_code: %s",
                   ip->ic->hostname, ip->protocol_state, strerror (errno));
    }

  if (!rv)
    ierr_dbg ("ipmipower_check_authentication_code(%s:%d): "
              "authentication code check failed",
              ip->ic->hostname, ip->protocol_state);

  return (rv);
}

int
ipmipower_check_outbound_sequence_number (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint32_t shift_num, wrap_val;
  uint32_t seq_num = 0;
  uint64_t val;
  int rv = 0;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (pkt != AUTHENTICATION_CAPABILITIES_V20_RES
          && pkt != AUTHENTICATION_CAPABILITIES_RES
          && pkt != GET_SESSION_CHALLENGE_RES
          && pkt != ACTIVATE_SESSION_RES
          && pkt != OPEN_SESSION_RES
          && pkt != RAKP_MESSAGE_2_RES
          && pkt != RAKP_MESSAGE_4_RES);

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

  if (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0
      && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
          || pkt == GET_CHASSIS_STATUS_RES
          || pkt == CHASSIS_CONTROL_RES
          || pkt == CHASSIS_IDENTIFY_RES
          || pkt == CLOSE_SESSION_RES))
    {
      if (FIID_OBJ_GET (ip->obj_rmcpplus_session_hdr_res,
                        "session_sequence_number",
                        &val) < 0)
        ierr_exit ("FIID_OBJ_GET: 'session_sequence_number': %s",
                   fiid_obj_errormsg (ip->obj_rmcpplus_session_hdr_res));
      seq_num = val;
    }
  else /*
         (cmd_args.common.driver_type == IPMI_DEVICE_LAN
         && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
         || pkt == GET_CHASSIS_STATUS_RES
         || pkt == CHASSIS_CONTROL_RES
         || pkt == CHASSIS_IDENTIFY_RES
         || pkt == CLOSE_SESSION_RES))
       */
    {
      if (FIID_OBJ_GET (ip->obj_lan_session_hdr_res,
                        "session_sequence_number",
                        &val) < 0)
        ierr_exit ("FIID_OBJ_GET: 'session_sequence_number': %s",
                   fiid_obj_errormsg (ip->obj_lan_session_hdr_res));
      seq_num = val;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on Sun Fire 4100.
   *
   * The session sequence numbers for IPMI 1.5 are the wrong endian.
   * So we have to flip the bits to workaround it.
   */
  if (cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
    {
      uint32_t tmp_seq_num = seq_num;

      seq_num =
        ((tmp_seq_num & 0xFF000000) >> 24)
        | ((tmp_seq_num & 0x00FF0000) >> 8)
        | ((tmp_seq_num & 0x0000FF00) << 8)
        | ((tmp_seq_num & 0x000000FF) << 24);
    }

  /* Drop duplicate packet */
  if (seq_num == ip->highest_received_sequence_number)
    goto out;

  /* In IPMI 2.0, sequence number 0 is special, and shouldn't happen */
  if (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0 && !seq_num)
    goto out;

  /* Check if sequence number is greater than highest received and is
   * within range
   */
  if (ip->highest_received_sequence_number > (IPMIPOWER_MAX_SEQUENCE_NUMBER - IPMIPOWER_SEQUENCE_NUMBER_WINDOW))
    {
      wrap_val = IPMIPOWER_SEQUENCE_NUMBER_WINDOW - (IPMIPOWER_MAX_SEQUENCE_NUMBER - ip->highest_received_sequence_number) - 1;

      /* In IPMI 2.0, sequence number 0 isn't possible, so adjust wrap_val */
      if (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0)
        wrap_val++;

      if (seq_num > ip->highest_received_sequence_number || seq_num <= wrap_val)
        {
          if (seq_num > ip->highest_received_sequence_number && seq_num <= IPMIPOWER_MAX_SEQUENCE_NUMBER)
            shift_num = seq_num - ip->highest_received_sequence_number;
          else
            {
              if (cmd_args.common.driver_type == IPMI_DEVICE_LAN)
                shift_num = seq_num + (IPMIPOWER_MAX_SEQUENCE_NUMBER - ip->highest_received_sequence_number) + 1;
              else
                /* IPMI 2.0 Special Case b/c 0 isn't a legit sequence number */
                shift_num = seq_num + (IPMIPOWER_MAX_SEQUENCE_NUMBER - ip->highest_received_sequence_number);
            }

          ip->highest_received_sequence_number = seq_num;
          ip->previously_received_list <<= shift_num;
          ip->previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }
  else
    {
      if (seq_num > ip->highest_received_sequence_number
          && (seq_num - ip->highest_received_sequence_number) <= IPMIPOWER_SEQUENCE_NUMBER_WINDOW)
        {
          shift_num = (seq_num - ip->highest_received_sequence_number);
          ip->highest_received_sequence_number = seq_num;
          ip->previously_received_list <<= shift_num;
          ip->previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }

  /* Check if sequence number is lower than highest received, is
   * within range, and hasn't been seen yet
   */
  if (ip->highest_received_sequence_number < IPMIPOWER_SEQUENCE_NUMBER_WINDOW)
    {
      uint32_t wrap_val = IPMIPOWER_MAX_SEQUENCE_NUMBER - (IPMIPOWER_SEQUENCE_NUMBER_WINDOW - ip->highest_received_sequence_number) + 1;

      /* In IPMI 2.0, sequence number 0 isn't possible, so adjust wrap_val */
      if (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0)
        wrap_val--;

      if (seq_num < ip->highest_received_sequence_number || seq_num >= wrap_val)
        {
          if (seq_num > ip->highest_received_sequence_number && seq_num <= IPMIPOWER_MAX_SEQUENCE_NUMBER)
            {
              if (cmd_args.common.driver_type == IPMI_DEVICE_LAN)
                shift_num = ip->highest_received_sequence_number + (IPMIPOWER_MAX_SEQUENCE_NUMBER - seq_num) + 1;
              else
                /* IPMI 2.0 Special Case b/c 0 isn't a legit sequence number */
                shift_num = ip->highest_received_sequence_number + (IPMIPOWER_MAX_SEQUENCE_NUMBER - seq_num);
            }
          else
            shift_num = ip->highest_received_sequence_number - seq_num;

          /* Duplicate packet check*/
          if (ip->previously_received_list & (0x1 << (shift_num - 1)))
            goto out;

          ip->previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }
  else
    {
      if (seq_num < ip->highest_received_sequence_number
          && seq_num >= (ip->highest_received_sequence_number - IPMIPOWER_SEQUENCE_NUMBER_WINDOW))
        {
          shift_num = ip->highest_received_sequence_number - seq_num;

          /* Duplicate packet check*/
          if (ip->previously_received_list & (0x1 << (shift_num - 1)))
            goto out;

          ip->previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }

  /* IPMI Workaround (achu)
   *
   * Disocvered on Intel SE7520JR2 with National Semiconductor PC87431M mBMC
   *
   * Note: Later changes in ipmipower have removed the need for these
   * workarounds.  I still note them for historical purposes.
   *
   * The initial outbound sequence number on activate session response
   * is off by one.  The activate session response packet is supposed
   * to contain the initial outbound sequence number passed during the
   * request.  The outbound sequence number on a close session reponse
   * may also be incorrect.
   */

 out:
  if (!rv)
    ierr_dbg ("ipmipower_check_outbound_sequence_number(%s:%d): seq_num: %u, high: %u",
              ip->ic->hostname, ip->protocol_state, seq_num,
              ip->highest_received_sequence_number);

  return (rv);
}

int
ipmipower_check_session_id (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint32_t session_id = 0;
  uint32_t expected_session_id = 0;
  uint64_t val;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (pkt != AUTHENTICATION_CAPABILITIES_V20_RES
          && pkt != AUTHENTICATION_CAPABILITIES_RES
          && pkt != GET_SESSION_CHALLENGE_RES
          && pkt != ACTIVATE_SESSION_RES);

  if (cmd_args.common.driver_type == IPMI_DEVICE_LAN
      && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
          || pkt == GET_CHASSIS_STATUS_RES
          || pkt == CHASSIS_CONTROL_RES
          || pkt == CHASSIS_IDENTIFY_RES
          || pkt == CLOSE_SESSION_RES))
    {
      if (FIID_OBJ_GET (ip->obj_lan_session_hdr_res,
                        "session_id",
                        &val) < 0)
        ierr_exit ("FIID_OBJ_GET: 'session_id': %s",
                   fiid_obj_errormsg (ip->obj_lan_session_hdr_res));
      session_id = val;

      if (FIID_OBJ_GET (ip->obj_activate_session_res,
                        "session_id",
                        &val) < 0)
        ierr_exit ("FIID_OBJ_GET: 'session_id': %s",
                   fiid_obj_errormsg (ip->obj_activate_session_res));
      expected_session_id = val;
    }
  else if (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0
           && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
               || pkt == GET_CHASSIS_STATUS_RES
               || pkt == CHASSIS_CONTROL_RES
               || pkt == CHASSIS_IDENTIFY_RES
               || pkt == CLOSE_SESSION_RES))
    {
      if (FIID_OBJ_GET (ip->obj_rmcpplus_session_hdr_res,
                        "session_id",
                        &val) < 0)
        ierr_exit ("FIID_OBJ_GET: 'session_id': %s",
                   fiid_obj_errormsg (ip->obj_rmcpplus_session_hdr_res));
      session_id = val;
      expected_session_id = ip->remote_console_session_id;
    }
  else /* 
          (pkt == OPEN_SESSION_RES
          || pkt == RAKP_MESSAGE_2_RES
          || pkt == RAKP_MESSAGE_4_RES)
       */
    {
      fiid_obj_t obj_cmd;

      obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

      if (FIID_OBJ_GET (obj_cmd,
                        "remote_console_session_id",
                        &val) < 0)
        ierr_exit ("FIID_OBJ_GET: 'remote_console_session_id': %s",
                   fiid_obj_errormsg (obj_cmd));
      session_id = val;
      expected_session_id = ip->remote_console_session_id;
    }

  if (session_id != expected_session_id)
    ierr_dbg ("ipmipower_check_session_id(%s:%d): session id: %Xh expected: %Xh",
              ip->ic->hostname, ip->protocol_state, session_id,
              expected_session_id);

  /* IPMI Workaround (achu)
   *
   * Discovered on Tyan S2882 w/ m3289 BMC
   *
   * The remote BMC returns zeroes for the session id instead of the
   * actual session id.  To work around this problem, we'll assume the
   * session id is correct if it is equal to zero.
   */

  if ((cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO)
      && !session_id)
    return (1);

  return ((session_id == expected_session_id) ? 1 : 0);
}

int
ipmipower_check_network_function (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t netfn = 0;
  uint8_t expected_netfn;
  uint64_t val;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  /* Assert this is not an IPMI 2.0 Session Setup Packet */
  assert (pkt != OPEN_SESSION_RES
          && pkt != RAKP_MESSAGE_2_RES
          && pkt != RAKP_MESSAGE_4_RES);

  if (FIID_OBJ_GET (ip->obj_lan_msg_hdr_res,
                    "net_fn",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'net_fn': %s",
               fiid_obj_errormsg (ip->obj_lan_msg_hdr_res));
  netfn = val;

  if (pkt == GET_CHASSIS_STATUS_RES
      || pkt == CHASSIS_CONTROL_RES
      || pkt == CHASSIS_IDENTIFY_RES)
    expected_netfn = IPMI_NET_FN_CHASSIS_RS;
  else
    expected_netfn = IPMI_NET_FN_APP_RS;

  if (netfn != expected_netfn)
    ierr_dbg ("ipmipower_check_network_function(%s:%d): netfn: %Xh, expected: %Xh",
              ip->ic->hostname, ip->protocol_state, netfn, expected_netfn);

  return ((netfn == expected_netfn) ? 1 : 0);
}

int
ipmipower_check_command (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t cmd = 0;
  uint8_t expected_cmd = 0;
  uint64_t val;
  fiid_obj_t obj_cmd;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  /* Assert this is not an IPMI 2.0 Session Setup Packet */
  assert (pkt != OPEN_SESSION_RES
          && pkt != RAKP_MESSAGE_2_RES
          && pkt != RAKP_MESSAGE_4_RES);

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "cmd",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'cmd': %s",
               fiid_obj_errormsg (obj_cmd));
  cmd = val;

  if (pkt == AUTHENTICATION_CAPABILITIES_V20_RES
      || pkt == AUTHENTICATION_CAPABILITIES_RES)
    expected_cmd = IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES;
  else if (pkt == GET_SESSION_CHALLENGE_RES)
    expected_cmd = IPMI_CMD_GET_SESSION_CHALLENGE;
  else if (pkt == ACTIVATE_SESSION_RES)
    expected_cmd = IPMI_CMD_ACTIVATE_SESSION;
  else if (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES)
    expected_cmd = IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL;
  else if (pkt == GET_CHASSIS_STATUS_RES)
    expected_cmd = IPMI_CMD_GET_CHASSIS_STATUS;
  else if (pkt == CHASSIS_CONTROL_RES)
    expected_cmd = IPMI_CMD_CHASSIS_CONTROL;
  else if (pkt == CHASSIS_IDENTIFY_RES)
    expected_cmd = IPMI_CMD_CHASSIS_IDENTIFY;
  else if (pkt == CLOSE_SESSION_RES)
    expected_cmd = IPMI_CMD_CLOSE_SESSION;

  if (cmd != expected_cmd)
    ierr_dbg ("ipmipower_check_command(%s:%d): cmd: %Xh, expected: %Xh",
              ip->ic->hostname, ip->protocol_state,
              cmd, expected_cmd);

  return ((cmd == expected_cmd) ? 1 : 0);
}

int
ipmipower_check_requester_sequence_number (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t req_seq = 0;
  uint8_t expected_req_seq = 0;
  uint64_t val;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  /* Assert this is not an IPMI 2.0 Session Setup Packet */
  assert (pkt != OPEN_SESSION_RES
          && pkt != RAKP_MESSAGE_2_RES
          && pkt != RAKP_MESSAGE_4_RES);

  expected_req_seq = ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);

  if (FIID_OBJ_GET (ip->obj_lan_msg_hdr_res,
                    "rq_seq",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'rq_seq': %s",
               fiid_obj_errormsg (ip->obj_lan_msg_hdr_res));
  req_seq = val;

  if (req_seq != expected_req_seq)
    ierr_dbg ("ipmipower_check_requester_sequence_number(%s:%d): req_seq: %Xh, expected: %Xh",
              ip->ic->hostname, ip->protocol_state,
              req_seq, expected_req_seq);

  return ((req_seq == expected_req_seq) ? 1 : 0);
}

int
ipmipower_check_completion_code (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t comp_code = 0;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  /* Assert this is not an IPMI 2.0 Session Setup Packet */
  assert (pkt != OPEN_SESSION_RES
          && pkt != RAKP_MESSAGE_2_RES
          && pkt != RAKP_MESSAGE_4_RES);

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "comp_code",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'comp_code': %s",
               fiid_obj_errormsg (obj_cmd));
  comp_code = val;

  if (comp_code != IPMI_COMP_CODE_COMMAND_SUCCESS)
    ierr_dbg ("ipmipower_check_completion_code(%s:%d): comp_code: %Xh",
              ip->ic->hostname, ip->protocol_state, comp_code);

  return ((comp_code == IPMI_COMP_CODE_COMMAND_SUCCESS) ? 1 : 0);
}

int
ipmipower_check_payload_type (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t payload_type;
  uint8_t expected_payload_type;
  uint64_t val;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (pkt == OPEN_SESSION_RES
          || pkt == RAKP_MESSAGE_2_RES
          || pkt == RAKP_MESSAGE_4_RES
          || (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0
              && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
                  || pkt == GET_CHASSIS_STATUS_RES
                  || pkt == CHASSIS_CONTROL_RES
                  || pkt == CHASSIS_IDENTIFY_RES
                  || pkt == CLOSE_SESSION_RES)));

  if (FIID_OBJ_GET (ip->obj_rmcpplus_session_hdr_res,
                    "payload_type",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'payload_type': %s",
               fiid_obj_errormsg (ip->obj_rmcpplus_session_hdr_res));
  payload_type = val;

  if (pkt == OPEN_SESSION_RES)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE;
  else if (pkt == RAKP_MESSAGE_2_RES)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2;
  else if (pkt == RAKP_MESSAGE_4_RES)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4;
  else
    expected_payload_type = IPMI_PAYLOAD_TYPE_IPMI;

  if (payload_type != expected_payload_type)
    ierr_dbg ("ipmipower_check_payload_type(%s:%d): "
              "payload_type: %Xh, expected: %Xh",
              ip->ic->hostname, ip->protocol_state,
              payload_type, expected_payload_type);

  return ((payload_type == expected_payload_type) ? 1 : 0);
}

int
ipmipower_check_message_tag (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t message_tag;
  uint8_t expected_message_tag;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (pkt == OPEN_SESSION_RES
          || pkt == RAKP_MESSAGE_2_RES
          || pkt == RAKP_MESSAGE_4_RES);

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "message_tag",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'message_tag': %s",
               fiid_obj_errormsg (obj_cmd));
  message_tag = val;

  expected_message_tag = ip->initial_message_tag + ip->message_tag_count;

  if (message_tag != expected_message_tag)
    ierr_dbg ("ipmipower_check_message_tag(%s:%d): "
              "message_tag: %Xh, expected: %Xh",
              ip->ic->hostname, ip->protocol_state,
              message_tag,
              expected_message_tag);

  return ((message_tag == expected_message_tag) ? 1 : 0);
}

int
ipmipower_check_rmcpplus_status_code (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t rmcpplus_status_code;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (pkt == OPEN_SESSION_RES
          || pkt == RAKP_MESSAGE_2_RES
          || pkt == RAKP_MESSAGE_4_RES);

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "rmcpplus_status_code",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'rmcpplus_status_code': %s",
               fiid_obj_errormsg (obj_cmd));
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    ierr_dbg ("ipmipower_check_rmcpplus_status_code(%s:%d): "
              "rmcpplus_status_code: %Xh",
              ip->ic->hostname, ip->protocol_state,
              rmcpplus_status_code);

  return ((rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS) ? 1 : 0);
}

int
ipmipower_check_open_session_response_privilege (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t maximum_privilege_level;
  uint64_t val;
  int rv;

  assert (ip);
  assert (pkt == OPEN_SESSION_RES);

  /*
   * IPMI Workaround (achu)
   *
   * See comments in ipmipower_powercmd.c (_check_open_session_error).
   */

  if (FIID_OBJ_GET (ip->obj_open_session_res,
                    "maximum_privilege_level",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'maximum_privilege_level': %s",
               fiid_obj_errormsg (ip->obj_open_session_res));
  maximum_privilege_level = val;

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The Intel's don't work with IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL.
   * So check that we get back what we sent.
   */
  if (cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    rv = (maximum_privilege_level == ip->requested_maximum_privilege_level) ? 1 : 0;
  else
    {
      if (cmd_args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_USER
          && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_USER
              || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
              || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
              || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
        rv = 1;
      else if (cmd_args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
               && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
                   || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
                   || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
        rv = 1;
      else if (cmd_args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
               && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
                   || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
        rv = 1;
      else
        rv = 0;
    }

  if (!rv)
    ierr_dbg ("ipmipower_check_open_session_response_privilege(%s:%d): "
              "invalid privilege: %Xh, expected: %Xh",
              ip->ic->hostname, ip->protocol_state,
              maximum_privilege_level,
              ip->requested_maximum_privilege_level);

  return (rv);
}

int
ipmipower_check_rakp_2_key_exchange_authentication_code (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int managed_system_random_number_len;
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int managed_system_guid_len;
  char *username;
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  char *password;
  unsigned int username_len, password_len;
  uint32_t managed_system_session_id;
  uint64_t val;
  int rv;

  assert (ip);
  assert (pkt == RAKP_MESSAGE_2_RES);

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The username must be padded despite explicitly not being
   * allowed.  "No Null characters (00h) are allowed in the name".
   * Table 13-11 in the IPMI 2.0 spec.
   */

  if (cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    {
      memset (username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (cmd_args.common.username)
        strcpy (username_buf, cmd_args.common.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      username = cmd_args.common.username;
      username_len = (username) ? strlen (username) : 0;
    }

  if (cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
    {
      uint8_t keybuf[IPMIPOWER_PACKET_BUFLEN];
      int keybuf_len;

      /* IPMI Workaround (achu)
       *
       * Discovered on Supermicro H8QME with SIMSO daughter card.
       *
       * The IPMI 2.0 packet responses for RAKP 2 have payload lengths
       * that are off by 1 (i.e. if the payload length should be X,
       * the payload length returned in the packet is X + 1)
       *
       * We fix/adjust for the situation here.
       */

      if ((keybuf_len = fiid_obj_get_data (ip->obj_rakp_message_2_res,
                                           "key_exchange_authentication_code",
                                           keybuf,
                                           IPMIPOWER_PACKET_BUFLEN)) < 0)
        ierr_exit ("fiid_obj_get_data: 'key_exchange_authentication_code': %s",
                   fiid_obj_errormsg (ip->obj_rakp_message_2_res));
      
      if (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
          && keybuf_len == 1)
        {
          if (fiid_obj_clear_field (ip->obj_rakp_message_2_res,
                                    "key_exchange_authentication_code") < 0)
            ierr_exit ("fiid_obj_clear_field: 'key_exchange_authentication_code': %s",
                       fiid_obj_errormsg (ip->obj_rakp_message_2_res));
        }
      else if (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
               && keybuf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (ip->obj_rakp_message_2_res,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            ierr_exit ("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                       fiid_obj_errormsg (ip->obj_rakp_message_2_res));
        }
      else if (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
               && keybuf_len == (IPMI_HMAC_MD5_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (ip->obj_rakp_message_2_res,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_MD5_DIGEST_LENGTH) < 0)
            ierr_exit ("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                       fiid_obj_errormsg (ip->obj_rakp_message_2_res));
        }
    }

  password = cmd_args.common.password;
  password_len = (password) ? strlen (password) : 0;

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
  if (cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION
      && ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;

  if (FIID_OBJ_GET (ip->obj_open_session_res,
                    "managed_system_session_id",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'managed_system_session_id': %s",
               fiid_obj_errormsg (ip->obj_open_session_res));
  managed_system_session_id = val;

  if ((managed_system_random_number_len = fiid_obj_get_data (ip->obj_rakp_message_2_res,
                                                             "managed_system_random_number",
                                                             managed_system_random_number,
                                                             IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    ierr_exit ("fiid_obj_get_data: 'managed_system_random_number': %s",
               fiid_obj_errormsg (ip->obj_rakp_message_2_res));
  
  if ((managed_system_guid_len = fiid_obj_get_data (ip->obj_rakp_message_2_res,
                                                    "managed_system_guid",
                                                    managed_system_guid,
                                                    IPMI_MANAGED_SYSTEM_GUID_LENGTH)) < 0)
    ierr_exit ("fiid_obj_get_data: 'managed_system_guid': %s",
               fiid_obj_errormsg (ip->obj_rakp_message_2_res));

  /* IPMI Workaround (achu)
   *
   * Discovered on Sun Fire 4100.
   *
   * The key exchange authentication code is the wrong length.  We
   * need to shorten it.
   *
   * Notes: Cipher suite 1,2,3 are the ones that use HMAC-SHA1 and
   * have the problem.
   */
  if (cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION
      && (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1))
    {
      uint8_t buf[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
      int buf_len;

      if ((buf_len = fiid_obj_get_data (ip->obj_rakp_message_2_res,
                                        "key_exchange_authentication_code",
                                        buf,
                                        IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
        ierr_exit ("fiid_obj_get_data: 'key_exchange_authentication_code': %s",
                   fiid_obj_errormsg (ip->obj_rakp_message_2_res));

      if (buf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_clear_field (ip->obj_rakp_message_2_res,
                                    "key_exchange_authentication_code") < 0)
            ierr_exit ("fiid_obj_clear_field: 'key_exchange_authentication_code': %s",
                       fiid_obj_errormsg (ip->obj_rakp_message_2_res));

          if (fiid_obj_set_data (ip->obj_rakp_message_2_res,
                                 "key_exchange_authentication_code",
                                 buf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            ierr_exit ("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                       fiid_obj_errormsg (ip->obj_rakp_message_2_res));
        }
    }

  if ((rv = ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code (ip->authentication_algorithm,
                                                                         password,
                                                                         password_len,
                                                                         ip->remote_console_session_id,
                                                                         managed_system_session_id,
                                                                         ip->remote_console_random_number,
                                                                         IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                                         managed_system_random_number,
                                                                         managed_system_random_number_len,
                                                                         managed_system_guid,
                                                                         managed_system_guid_len,
                                                                         ip->name_only_lookup,
                                                                         cmd_args.common.privilege_level,
                                                                         username,
                                                                         username_len,
                                                                         ip->obj_rakp_message_2_res)) < 0)
    ierr_exit ("ipmipower_check_rakp_2_key_exchange_authentication_code(%s:%d): "
               "ipmi_rmcpplus_check_rakp_message_2_key_exchange_authentication_code: %s",
               ip->ic->hostname, ip->protocol_state, strerror (errno));

  if (!rv)
    ierr_dbg ("ipmipower_check_rakp_2_key_exchange_authentication_code(%s:%d): "
              "rakp 2 check failed",
              ip->ic->hostname, ip->protocol_state);

  return (rv);
}

int
ipmipower_check_rakp_4_integrity_check_value (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int managed_system_guid_len;
  uint32_t managed_system_session_id;
  uint8_t authentication_algorithm = 0;
  uint64_t val;
  int rv;

  assert (ip);
  assert (pkt == RAKP_MESSAGE_4_RES);

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * For some reason, the intel ipmi 2.0 responds with the integrity
   * check value based on the integrity algorithm instead of the
   * authentication algorithm.
   *
   * Thanks to the ipmitool folks (ipmitool.sourceforge.net) for this
   * one.  Would have taken me awhile to figure this one out :-)
   */

  if (cmd_args.common.workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    {
      if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
      else if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1;
      else if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5;
      else if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        /* achu: I have thus far been unable to reverse engineer this
         * corner case.  So we're just going to accept whatever the
         * remote BMC gives us.  This has been documented in the
         * manpage.
         */
        return (1);
    }
  else
    authentication_algorithm = ip->authentication_algorithm;

  if (FIID_OBJ_GET (ip->obj_open_session_res,
                    "managed_system_session_id",
                    &val) < 0)
    ierr_exit ("FIID_OBJ_GET: 'managed_system_session_id': %s",
               fiid_obj_errormsg (ip->obj_open_session_res));
  managed_system_session_id = val;

  if ((managed_system_guid_len = fiid_obj_get_data (ip->obj_rakp_message_2_res,
                                                    "managed_system_guid",
                                                    managed_system_guid,
                                                    IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    ierr_exit ("fiid_obj_get_data: 'managed_system_guid': %s",
               fiid_obj_errormsg (ip->obj_rakp_message_2_res));

  if ((rv = ipmi_rmcpplus_check_rakp_4_integrity_check_value (authentication_algorithm,
                                                              ip->sik_key_ptr,
                                                              ip->sik_key_len,
                                                              ip->remote_console_random_number,
                                                              IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                              managed_system_session_id,
                                                              managed_system_guid,
                                                              managed_system_guid_len,
                                                              ip->obj_rakp_message_4_res)) < 0)
    ierr_exit ("ipmipower_check_rakp_4_integrity_check_value(%s:%d): "
               "ipmipower_check_rakp_4_integrity_check_value: %s",
               ip->ic->hostname, ip->protocol_state, strerror (errno));

  if (!rv)
    ierr_dbg ("ipmipower_check_rakp_4_integrity_check_value(%s:%d): "
              "rakp 4 check failed",
              ip->ic->hostname, ip->protocol_state);

  return (rv);
}

int
ipmipower_check_payload_pad (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  uint8_t confidentiality_algorithm;
  int rv;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0
          && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
              || pkt == GET_CHASSIS_STATUS_RES
              || pkt == CHASSIS_CONTROL_RES
              || pkt == CHASSIS_IDENTIFY_RES
              || pkt == CLOSE_SESSION_RES));

  confidentiality_algorithm = ip->confidentiality_algorithm;

  if ((rv = ipmi_rmcpplus_check_payload_pad (confidentiality_algorithm,
                                             ip->obj_rmcpplus_payload_res)) < 0)
    ierr_exit ("ipmipower_check_payload_pad(%s:%d): "
               "ipmi_rmcpplus_check_payload_pad: %s",
               ip->ic->hostname, ip->protocol_state, strerror (errno));

  if (!rv)
    ierr_dbg ("ipmipower_check_payload_pad(%s:%d): "
              "payload pad check failed",
              ip->ic->hostname, ip->protocol_state);

  return (rv);
}

int
ipmipower_check_integrity_pad (ipmipower_powercmd_t ip, packet_type_t pkt)
{
  int rv;

  assert (ip);
  assert (PACKET_TYPE_VALID_RES (pkt));
  assert (cmd_args.common.driver_type == IPMI_DEVICE_LAN_2_0
          && (pkt == SET_SESSION_PRIVILEGE_LEVEL_RES
              || pkt == GET_CHASSIS_STATUS_RES
              || pkt == CHASSIS_CONTROL_RES
              || pkt == CHASSIS_IDENTIFY_RES
              || pkt == CLOSE_SESSION_RES));

  if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    return (1);

  if ((rv = ipmi_rmcpplus_check_integrity_pad (ip->obj_rmcpplus_session_trlr_res)) < 0)
    ierr_exit ("ipmipower_check_integrity_pad(%s:%d): "
               "ipmi_rmcpplus_check_integrity_pad: %s",
               ip->ic->hostname, ip->protocol_state, strerror (errno));

  if (!rv)
    ierr_dbg ("ipmipower_check_integrity_pad(%s:%d): "
              "integrity pad check failed",
              ip->ic->hostname, ip->protocol_state);

  return (rv);
}

