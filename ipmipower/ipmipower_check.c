/*****************************************************************************\
 *  $Id: ipmipower_check.c,v 1.124 2010-08-03 00:10:59 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#include "ipmipower_error.h"
#include "ipmipower_packet.h"

#include "freeipmi-portability.h"

extern struct ipmipower_arguments cmd_args;

int
ipmipower_check_checksum (ipmipower_powercmd_t ip,
			  ipmipower_packet_type_t pkt)
{
  fiid_obj_t obj_cmd;
  int rv;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_1_5_SETUP_RS (pkt)
	  || IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt)); /* 1.5 or 2.0 */

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro X9SCM-iiF, Supermicro X9DRi-F
   *
   * Checksums are computed incorrectly.
   */
  if (cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK
      || cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK)
    return (1);

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);
  if ((rv = ipmi_lan_check_checksum (ip->obj_lan_msg_hdr_rs,
                                     obj_cmd,
                                     ip->obj_lan_msg_trlr_rs)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_lan_check_checksum: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; checksum check failed",
                      ip->ic->hostname, ip->protocol_state));

  return (rv);
}

int
ipmipower_check_authentication_code (ipmipower_powercmd_t ip,
                                     ipmipower_packet_type_t pkt,
                                     const void *buf,
                                     unsigned int buflen)
{
  char *password;
  int rv = -1;

  assert (ip);
  assert (pkt == IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RS
	  || IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));
  assert (buf && buflen);

  /* IPMI 1.5 Checks */
  if (pkt == IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RS
      || cmd_args.common_args.driver_type == IPMI_DEVICE_LAN)
    {
      uint8_t authentication_type;
      int check_authcode_retry_flag = 0;

      /* IPMI Workaround (achu)
       *
       * Discovered on Xyratex HB-F8-SRAY
       *
       * For some reason, the authentication code is always blank when
       * using "Straight Password Key".
       */
      if (cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK)
	{
	  rv = 1;
	  goto out;
	}

      if (pkt == IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RS)
        authentication_type = cmd_args.common_args.authentication_type;
      else /* IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt) */
        {
          if (!ip->permsgauth_enabled)
            {
              authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
              check_authcode_retry_flag++;
            }
          else
            authentication_type = cmd_args.common_args.authentication_type;
        }

      if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
        password = cmd_args.common_args.password;
      else
        password = NULL;

      if ((rv = ipmi_lan_check_packet_session_authentication_code (buf,
                                                                   buflen,
                                                                   authentication_type,
                                                                   password,
                                                                   password ? strlen (password) : 0)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_lan_check_packet_session_authentication_code: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

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
      if ((cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE)
          && !rv
          && check_authcode_retry_flag)
        {
          IPMIPOWER_DEBUG (("host = %s; p = %d; retry authcode check",
                            ip->ic->hostname, ip->protocol_state));

          authentication_type = cmd_args.common_args.authentication_type;
          if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
            password = cmd_args.common_args.password;
          else
            password = NULL;

          if ((rv = ipmi_lan_check_packet_session_authentication_code (buf,
                                                                       buflen,
                                                                       authentication_type,
                                                                       password,
                                                                       password ? strlen (password) : 0)) < 0)
            {
              IPMIPOWER_ERROR (("ipmi_lan_check_packet_session_authentication_code: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }

          if (rv)
            IPMIPOWER_DEBUG (("host = %s; p = %d; permsgauth authcode re-check passed",
                              ip->ic->hostname, ip->protocol_state));
        }
    }
  else /* cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0 */
    {
      /* IPMI 2.0 Checks */
      uint8_t integrity_algorithm;

      integrity_algorithm = ip->integrity_algorithm;

      password = cmd_args.common_args.password;

      if ((rv = ipmi_rmcpplus_check_packet_session_authentication_code (integrity_algorithm,
                                                                        buf,
                                                                        buflen,
                                                                        ip->integrity_key_ptr,
                                                                        ip->integrity_key_len,
                                                                        password,
                                                                        (password) ? strlen (password) : 0,
                                                                        ip->obj_rmcpplus_session_trlr_rs)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_rmcpplus_check_packet_session_authentication_code: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
    }

 out:

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; authentication code check failed",
                      ip->ic->hostname, ip->protocol_state));

  return (rv);
}

int
ipmipower_check_outbound_sequence_number (ipmipower_powercmd_t ip,
					  ipmipower_packet_type_t pkt)
{
  uint32_t session_sequence_number = 0;
  uint64_t val;
  int rv = 0;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

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

  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0)
    {
      if (FIID_OBJ_GET (ip->obj_rmcpplus_session_hdr_rs,
                        "session_sequence_number",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'session_sequence_number': %s",
                            fiid_obj_errormsg (ip->obj_rmcpplus_session_hdr_rs)));
          exit (EXIT_FAILURE);
        }
      session_sequence_number = val;
    }
  else /* cmd_args.common_args.driver_type == IPMI_DEVICE_LAN */
    {
      if (FIID_OBJ_GET (ip->obj_lan_session_hdr_rs,
                        "session_sequence_number",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'session_sequence_number': %s",
                            fiid_obj_errormsg (ip->obj_lan_session_hdr_rs)));
          exit (EXIT_FAILURE);
        }
      session_sequence_number = val;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on Sun Fire 4100.
   *
   * The session sequence numbers for IPMI 1.5 are the wrong endian.
   * So we have to flip the bits to workaround it.
   */
  if (cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER)
    {
      uint32_t tmp_session_sequence_number = session_sequence_number;

      session_sequence_number =
        ((tmp_session_sequence_number & 0xFF000000) >> 24)
        | ((tmp_session_sequence_number & 0x00FF0000) >> 8)
        | ((tmp_session_sequence_number & 0x0000FF00) << 8)
        | ((tmp_session_sequence_number & 0x000000FF) << 24);
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

  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN)
    {
      if ((rv = ipmi_check_session_sequence_number_1_5 (session_sequence_number,
                                                        &(ip->highest_received_sequence_number),
                                                        &(ip->previously_received_list),
                                                        0)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_session_sequence_number_1_5: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
    }
  else
    {
      if ((rv = ipmi_check_session_sequence_number_2_0 (session_sequence_number,
                                                        &(ip->highest_received_sequence_number),
                                                        &(ip->previously_received_list),
                                                        0)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_session_sequence_number_2_0: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
    }

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; session_sequence_number failed: %u; high = %u",
                      ip->ic->hostname,
                      ip->protocol_state,
                      session_sequence_number,
                      ip->highest_received_sequence_number));

  return (rv);
}

int
ipmipower_check_session_id (ipmipower_powercmd_t ip,
			    ipmipower_packet_type_t pkt)
{
  uint32_t session_id = 0;
  uint32_t expected_session_id = 0;
  uint64_t val;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_2_0_SETUP_RS (pkt)
	  || IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN
      && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt))
    {
      if (FIID_OBJ_GET (ip->obj_lan_session_hdr_rs,
                        "session_id",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'session_id': %s",
                            fiid_obj_errormsg (ip->obj_lan_session_hdr_rs)));
          exit (EXIT_FAILURE);
        }
      session_id = val;

      if (FIID_OBJ_GET (ip->obj_activate_session_rs,
                        "session_id",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'session_id': %s",
                            fiid_obj_errormsg (ip->obj_activate_session_rs)));
          exit (EXIT_FAILURE);
        }
      expected_session_id = val;
    }
  else if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0
	   && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt))
    {
      if (FIID_OBJ_GET (ip->obj_rmcpplus_session_hdr_rs,
                        "session_id",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'session_id': %s",
                            fiid_obj_errormsg (ip->obj_rmcpplus_session_hdr_rs)));
          exit (EXIT_FAILURE);
        }
      session_id = val;
      expected_session_id = ip->remote_console_session_id;
    }
  else /* IPMIPOWER_PACKET_TYPE_IPMI_2_0_SETUP_RS (pkt) */
    {
      fiid_obj_t obj_cmd;

      obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

      if (FIID_OBJ_GET (obj_cmd,
                        "remote_console_session_id",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'remote_console_session_id': %s",
                            fiid_obj_errormsg (obj_cmd)));
          exit (EXIT_FAILURE);
        }
      session_id = val;
      expected_session_id = ip->remote_console_session_id;
    }

  if (session_id != expected_session_id)
    IPMIPOWER_DEBUG (("host = %s; p = %d; session id failed: %Xh; expected = %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      session_id,
                      expected_session_id));

  /* IPMI Workaround (achu)
   *
   * Discovered on Tyan S2882 w/ m3289 BMC
   *
   * The remote BMC returns zeroes for the session id instead of the
   * actual session id.  To work around this problem, we'll assume the
   * session id is correct if it is equal to zero.
   */

  if ((cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO)
      && !session_id)
    return (1);

  return ((session_id == expected_session_id) ? 1 : 0);
}

int
ipmipower_check_network_function (ipmipower_powercmd_t ip,
				  ipmipower_packet_type_t pkt)
{
  uint8_t netfn = 0;
  uint8_t expected_netfn;
  uint64_t val;

  assert (ip);
  /* Assert this is not an IPMI 2.0 Session Setup Packet */
  assert (IPMIPOWER_PACKET_TYPE_IPMI_1_5_SETUP_RS (pkt)
	  || IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

  if (FIID_OBJ_GET (ip->obj_lan_msg_hdr_rs,
                    "net_fn",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'net_fn': %s",
                        fiid_obj_errormsg (ip->obj_lan_msg_hdr_rs)));
      exit (EXIT_FAILURE);
    }
  netfn = val;

  if (pkt == IPMIPOWER_PACKET_TYPE_GET_CHASSIS_STATUS_RS
      || pkt == IPMIPOWER_PACKET_TYPE_CHASSIS_CONTROL_RS
      || pkt == IPMIPOWER_PACKET_TYPE_CHASSIS_IDENTIFY_RS)
    expected_netfn = IPMI_NET_FN_CHASSIS_RS;
  else if (pkt == IPMIPOWER_PACKET_TYPE_C410X_GET_SENSOR_READING_RS)
    expected_netfn = IPMI_NET_FN_SENSOR_EVENT_RS;
  else if (pkt == IPMIPOWER_PACKET_TYPE_C410X_SLOT_POWER_CONTROL_RS)
    expected_netfn = IPMI_NET_FN_OEM_DELL_GENERIC_RS;
  else /* pkt == IPMIPOWER_PACKET_TYPE_AUTHENTICATION_CAPABILITIES_RS
	  || pkt == IPMIPOWER_PACKET_TYPE_GET_SESSION_CHALLENGE_RS
	  || pkt == IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RS
	  || pkt == IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RS
	*/
    expected_netfn = IPMI_NET_FN_APP_RS;
  
  if (netfn != expected_netfn)
    IPMIPOWER_DEBUG (("host = %s; p = %d; netfn failed: %Xh; expected = %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      netfn,
                      expected_netfn));

  return ((netfn == expected_netfn) ? 1 : 0);
}

int
ipmipower_check_command (ipmipower_powercmd_t ip,
			 ipmipower_packet_type_t pkt)
{
  uint8_t cmd = 0;
  uint8_t expected_cmd = 0;
  uint64_t val;
  fiid_obj_t obj_cmd;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_1_5_SETUP_RS (pkt)
	  || IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "cmd",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'cmd': %s",
                        fiid_obj_errormsg (obj_cmd)));
      exit (EXIT_FAILURE);
    }
  cmd = val;

  switch (pkt)
    {
    case IPMIPOWER_PACKET_TYPE_AUTHENTICATION_CAPABILITIES_RS:
      expected_cmd = IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES;
      break;
    case IPMIPOWER_PACKET_TYPE_GET_SESSION_CHALLENGE_RS:
      expected_cmd = IPMI_CMD_GET_SESSION_CHALLENGE;
      break;
    case IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RS:
      expected_cmd = IPMI_CMD_ACTIVATE_SESSION;
      break;
    case IPMIPOWER_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS:
      expected_cmd = IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL;
      break;
    case IPMIPOWER_PACKET_TYPE_GET_CHASSIS_STATUS_RS:
      expected_cmd = IPMI_CMD_GET_CHASSIS_STATUS;
      break;
    case IPMIPOWER_PACKET_TYPE_CHASSIS_CONTROL_RS:
      expected_cmd = IPMI_CMD_CHASSIS_CONTROL;
      break;
    case IPMIPOWER_PACKET_TYPE_CHASSIS_IDENTIFY_RS:
      expected_cmd = IPMI_CMD_CHASSIS_IDENTIFY;
      break;
    case IPMIPOWER_PACKET_TYPE_C410X_GET_SENSOR_READING_RS:
      expected_cmd = IPMI_CMD_GET_SENSOR_READING;
      break;
    case IPMIPOWER_PACKET_TYPE_C410X_SLOT_POWER_CONTROL_RS:
      expected_cmd = IPMI_CMD_OEM_DELL_SLOT_POWER_CONTROL;
      break;
    case IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RS:
      expected_cmd = IPMI_CMD_CLOSE_SESSION;
      break;
    default:
      IPMIPOWER_ERROR (("ipmipower_check_command: invalid pkt type: %d", pkt));
      exit (EXIT_FAILURE);
    }

  if (cmd != expected_cmd)
    IPMIPOWER_DEBUG (("host = %s; p = %d; cmd failed: %Xh; expected = %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      cmd,
                      expected_cmd));

  return ((cmd == expected_cmd) ? 1 : 0);
}

int
ipmipower_check_requester_sequence_number (ipmipower_powercmd_t ip,
					   ipmipower_packet_type_t pkt)
{
  uint8_t req_seq = 0;
  uint8_t expected_req_seq = 0;
  uint64_t val;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_1_5_SETUP_RS (pkt)
	  || IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

  expected_req_seq = ip->ic->ipmi_requester_sequence_number_counter % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);

  if (FIID_OBJ_GET (ip->obj_lan_msg_hdr_rs,
                    "rq_seq",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'rq_seq': %s",
                        fiid_obj_errormsg (ip->obj_lan_msg_hdr_rs)));
      exit (EXIT_FAILURE);
    }
  req_seq = val;

  if (req_seq != expected_req_seq)
    IPMIPOWER_DEBUG (("host = %s; p = %d; req_seq failed: %Xh; expected = %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      req_seq,
                      expected_req_seq));

  return ((req_seq == expected_req_seq) ? 1 : 0);
}

int
ipmipower_check_completion_code (ipmipower_powercmd_t ip,
				 ipmipower_packet_type_t pkt)
{
  uint8_t comp_code = 0;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_RS (pkt));
  assert (IPMIPOWER_PACKET_TYPE_IPMI_1_5_SETUP_RS (pkt)
	  || IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "comp_code",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'comp_code': %s",
                        fiid_obj_errormsg (obj_cmd)));
      exit (EXIT_FAILURE);
    }
  comp_code = val;

  if (comp_code != IPMI_COMP_CODE_COMMAND_SUCCESS)
    IPMIPOWER_DEBUG (("host = %s; p = %d; comp_code failed: %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      comp_code));

  return ((comp_code == IPMI_COMP_CODE_COMMAND_SUCCESS) ? 1 : 0);
}

int
ipmipower_check_payload_type (ipmipower_powercmd_t ip,
			      ipmipower_packet_type_t pkt)
{
  uint8_t payload_type;
  uint8_t expected_payload_type;
  uint64_t val;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_2_0_SETUP_RS (pkt)
          || (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0
              && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt)));

  if (FIID_OBJ_GET (ip->obj_rmcpplus_session_hdr_rs,
                    "payload_type",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'payload_type': %s",
                        fiid_obj_errormsg (ip->obj_rmcpplus_session_hdr_rs)));
      exit (EXIT_FAILURE);
    }
  payload_type = val;

  if (pkt == IPMIPOWER_PACKET_TYPE_OPEN_SESSION_RESPONSE)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE;
  else if (pkt == IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_2)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2;
  else if (pkt == IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_4)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4;
  else /* IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt) */
    expected_payload_type = IPMI_PAYLOAD_TYPE_IPMI;

  if (payload_type != expected_payload_type)
    IPMIPOWER_DEBUG (("host = %s; p = %d; payload_type failed: %Xh; expected = %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      payload_type,
                      expected_payload_type));

  return ((payload_type == expected_payload_type) ? 1 : 0);
}

int
ipmipower_check_message_tag (ipmipower_powercmd_t ip,
			     ipmipower_packet_type_t pkt)
{
  uint8_t message_tag;
  uint8_t expected_message_tag;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_2_0_SETUP_RS (pkt));

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "message_tag",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'message_tag': %s",
                        fiid_obj_errormsg (obj_cmd)));
      exit (EXIT_FAILURE);
    }
  message_tag = val;

  expected_message_tag = ip->initial_message_tag + ip->message_tag_count;

  if (message_tag != expected_message_tag)
    IPMIPOWER_DEBUG (("host = %s; p = %d; message_tag failed: %Xh; expected = %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      message_tag,
                      expected_message_tag));

  return ((message_tag == expected_message_tag) ? 1 : 0);
}

int
ipmipower_check_rmcpplus_status_code (ipmipower_powercmd_t ip,
				      ipmipower_packet_type_t pkt)
{
  uint8_t rmcpplus_status_code;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_IPMI_2_0_SETUP_RS (pkt));

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if (FIID_OBJ_GET (obj_cmd,
                    "rmcpplus_status_code",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'rmcpplus_status_code': %s",
                        fiid_obj_errormsg (obj_cmd)));
      exit (EXIT_FAILURE);
    }
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    IPMIPOWER_DEBUG (("host = %s; p = %d; rmcpplus_status_code failed: %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      rmcpplus_status_code));

  return ((rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS) ? 1 : 0);
}

int
ipmipower_check_packet (ipmipower_powercmd_t ip,
			ipmipower_packet_type_t pkt)
{
  fiid_obj_t obj_cmd;
  int ret;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_RS (pkt));

  obj_cmd = ipmipower_packet_cmd_obj (ip, pkt);

  if ((ret = fiid_obj_packet_valid (obj_cmd)) < 0)
    {
      IPMIPOWER_ERROR (("fiid_obj_packet_valid: %s",
                        fiid_obj_errormsg (obj_cmd)));
      exit (EXIT_FAILURE);
    }

  if (!ret)
    IPMIPOWER_DEBUG (("host = %s; p = %d; packet invalid",
                      ip->ic->hostname,
                      ip->protocol_state));

  return (ret);
}

int
ipmipower_check_open_session_response_privilege (ipmipower_powercmd_t ip,
						 ipmipower_packet_type_t pkt)
{
  uint8_t maximum_privilege_level;
  uint64_t val;
  int rv;

  assert (ip);
  assert (pkt == IPMIPOWER_PACKET_TYPE_OPEN_SESSION_RESPONSE);

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The Intel's don't work with IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL.
   * So check that we get back what we sent.
   */
  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      if (FIID_OBJ_GET (ip->obj_open_session_rs,
                        "maximum_privilege_level",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'maximum_privilege_level': %s",
                            fiid_obj_errormsg (ip->obj_open_session_rs)));
          exit (EXIT_FAILURE);
        }
      maximum_privilege_level = val;

      rv = (maximum_privilege_level == ip->requested_maximum_privilege_level) ? 1 : 0;
    }
  else
    {
      if ((rv = ipmi_check_open_session_maximum_privilege (cmd_args.common_args.privilege_level,
                                                           ip->obj_open_session_rs)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_open_session_maximum_privilege: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
    }

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; invalid privilege: expected = %Xh",
                      ip->ic->hostname,
                      ip->protocol_state,
                      cmd_args.common_args.privilege_level));

  return (rv);
}

int
ipmipower_check_rakp_2_key_exchange_authentication_code (ipmipower_powercmd_t ip,
							 ipmipower_packet_type_t pkt)
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
  assert (pkt == IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_2);

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The username must be padded despite explicitly not being
   * allowed.  "No Null characters (00h) are allowed in the name".
   * Table 13-11 in the IPMI 2.0 spec.
   */

  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      memset (username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (cmd_args.common_args.username)
        strcpy (username_buf, cmd_args.common_args.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      username = cmd_args.common_args.username;
      username_len = (username) ? strlen (username) : 0;
    }

  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION)
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

      if ((keybuf_len = fiid_obj_get_data (ip->obj_rakp_message_2_rs,
                                           "key_exchange_authentication_code",
                                           keybuf,
                                           IPMIPOWER_PACKET_BUFLEN)) < 0)
        {
          IPMIPOWER_ERROR (("fiid_obj_get_data: 'key_exchange_authentication_code': %s",
                            fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
          exit (EXIT_FAILURE);
        }
      
      if (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
          && keybuf_len == 1)
        {
          if (fiid_obj_clear_field (ip->obj_rakp_message_2_rs,
                                    "key_exchange_authentication_code") < 0)
            {
              IPMIPOWER_ERROR (("fiid_obj_clear_field: 'key_exchange_authentication_code': %s",
                                fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
              exit (EXIT_FAILURE);
            }
        }
      else if (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
               && keybuf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (ip->obj_rakp_message_2_rs,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            {
              IPMIPOWER_ERROR (("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                                fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
              exit (EXIT_FAILURE);
            }
        }
      else if (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
               && keybuf_len == (IPMI_HMAC_MD5_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (ip->obj_rakp_message_2_rs,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_MD5_DIGEST_LENGTH) < 0)
            {
              IPMIPOWER_ERROR (("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                                fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
              exit (EXIT_FAILURE);
            }
        }
    }

  password = cmd_args.common_args.password;
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
  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION
      && ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;

  if (FIID_OBJ_GET (ip->obj_open_session_rs,
                    "managed_system_session_id",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'managed_system_session_id': %s",
                        fiid_obj_errormsg (ip->obj_open_session_rs)));
      exit (EXIT_FAILURE);
    }
  managed_system_session_id = val;

  if ((managed_system_random_number_len = fiid_obj_get_data (ip->obj_rakp_message_2_rs,
                                                             "managed_system_random_number",
                                                             managed_system_random_number,
                                                             IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    {
      IPMIPOWER_ERROR (("fiid_obj_get_data: 'managed_system_random_number': %s",
                        fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
      exit (EXIT_FAILURE);
    }
  
  if ((managed_system_guid_len = fiid_obj_get_data (ip->obj_rakp_message_2_rs,
                                                    "managed_system_guid",
                                                    managed_system_guid,
                                                    IPMI_MANAGED_SYSTEM_GUID_LENGTH)) < 0)
    {
      IPMIPOWER_ERROR (("fiid_obj_get_data: 'managed_system_guid': %s",
                        fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
      exit (EXIT_FAILURE);
    }

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
  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION
      && (ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1))
    {
      uint8_t buf[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
      int buf_len;

      if ((buf_len = fiid_obj_get_data (ip->obj_rakp_message_2_rs,
                                        "key_exchange_authentication_code",
                                        buf,
                                        IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
        {
          IPMIPOWER_ERROR (("fiid_obj_get_data: 'key_exchange_authentication_code': %s",
                            fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
          exit (EXIT_FAILURE);
        }

      if (buf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_clear_field (ip->obj_rakp_message_2_rs,
                                    "key_exchange_authentication_code") < 0)
            {
              IPMIPOWER_ERROR (("fiid_obj_clear_field: 'key_exchange_authentication_code': %s",
                                fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
              exit (EXIT_FAILURE);
            }

          if (fiid_obj_set_data (ip->obj_rakp_message_2_rs,
                                 "key_exchange_authentication_code",
                                 buf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            {
              IPMIPOWER_ERROR (("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                                fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
              exit (EXIT_FAILURE);
            }
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
                                                                         cmd_args.common_args.privilege_level,
                                                                         username,
                                                                         username_len,
                                                                         ip->obj_rakp_message_2_rs)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; rakp 2 check failed",
                      ip->ic->hostname,
                      ip->protocol_state));

  return (rv);
}

int
ipmipower_check_rakp_4_integrity_check_value (ipmipower_powercmd_t ip,
					      ipmipower_packet_type_t pkt)
{
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int managed_system_guid_len;
  uint32_t managed_system_session_id;
  uint8_t authentication_algorithm = 0;
  uint64_t val;
  int rv;

  assert (ip);
  assert (pkt == IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_4);

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

  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
      else if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1;
      else if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5;
      else if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        {
          /* achu: I have thus far been unable to reverse engineer this
           * corner case.  Since we cannot provide a reasonable two
           * part authentication, we're going to error out.
           */
          IPMIPOWER_DEBUG (("host = %s; p = %d; rakp 4 check failed",
                            ip->ic->hostname,
                            ip->protocol_state));
          return (0);
        }
    }
  else
    authentication_algorithm = ip->authentication_algorithm;

  if (FIID_OBJ_GET (ip->obj_open_session_rs,
                    "managed_system_session_id",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'managed_system_session_id': %s",
                        fiid_obj_errormsg (ip->obj_open_session_rs)));
      exit (EXIT_FAILURE);
    }
  managed_system_session_id = val;

  if ((managed_system_guid_len = fiid_obj_get_data (ip->obj_rakp_message_2_rs,
                                                    "managed_system_guid",
                                                    managed_system_guid,
                                                    IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    {
      IPMIPOWER_ERROR (("fiid_obj_get_data: 'managed_system_guid': %s",
                        fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
      exit (EXIT_FAILURE);
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro X8DTG, Supermicro X8DTU, Intel
   * S5500WBV/Penguin Relion 700
   *
   * For whatever reason, with cipher suite 0, the RAKP 4 response
   * returns with an Integrity Check Value when it should be empty.
   */

  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE
      && !cmd_args.common_args.cipher_suite_id)
    {
      if (fiid_obj_clear_field (ip->obj_rakp_message_4_rs,
                                "integrity_check_value") < 0)
        {
          IPMIPOWER_ERROR (("fiid_obj_clear_field: 'integrity_check_value': %s",
                            fiid_obj_errormsg (ip->obj_open_session_rs)));
          exit (EXIT_FAILURE);
        }
    }

  if ((rv = ipmi_rmcpplus_check_rakp_4_integrity_check_value (authentication_algorithm,
                                                              ip->sik_key_ptr,
                                                              ip->sik_key_len,
                                                              ip->remote_console_random_number,
                                                              IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                              managed_system_session_id,
                                                              managed_system_guid,
                                                              managed_system_guid_len,
                                                              ip->obj_rakp_message_4_rs)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_rmcpplus_check_rakp_4_integrity_check_value: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; rakp 4 check failed",
                      ip->ic->hostname,
                      ip->protocol_state));

  return (rv);
}

int
ipmipower_check_payload_pad (ipmipower_powercmd_t ip,
			     ipmipower_packet_type_t pkt)
{
  uint8_t confidentiality_algorithm;
  int rv;

  assert (ip);
  assert (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0
          && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

  confidentiality_algorithm = ip->confidentiality_algorithm;

  if ((rv = ipmi_rmcpplus_check_payload_pad (confidentiality_algorithm,
                                             ip->obj_rmcpplus_payload_rs)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_rmcpplus_check_payload_pad: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; payload pad check failed",
                      ip->ic->hostname,
                      ip->protocol_state));

  return (rv);
}

int
ipmipower_check_integrity_pad (ipmipower_powercmd_t ip,
			       ipmipower_packet_type_t pkt)
{
  int rv;

  assert (ip);
  assert (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0
          && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt));

  if (ip->integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    return (1);

  if ((rv = ipmi_rmcpplus_check_integrity_pad (ip->obj_rmcpplus_session_trlr_rs)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_rmcpplus_check_integrity_pad: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!rv)
    IPMIPOWER_DEBUG (("host = %s; p = %d; integrity pad check failed",
                      ip->ic->hostname,
                      ip->protocol_state));

  return (rv);
}

