/*****************************************************************************\
 *  $Id: ipmiconsole_checks.c,v 1.17 2007-09-27 20:27:36 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *  
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmiconsole is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmiconsole is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "ipmiconsole_checks.h"
#include "ipmiconsole_ctx.h"
#include "ipmiconsole_debug.h"
#include "ipmiconsole_fiid_wrappers.h"
#include "ipmiconsole_packet.h"

int
ipmiconsole_check_checksum(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  fiid_obj_t obj_cmd;
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  obj_cmd = ipmiconsole_packet_object(c, p);
  if ((rv = ipmi_lan_check_checksum(c->connection.obj_lan_msg_hdr_rs,
				    obj_cmd,
				    c->connection.obj_lan_msg_trlr_rs)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("ipmi_lan_check_checksum: p = %d; %s", p, strerror(errno)));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("checksum check failed; p = %d", p));

  return ((int)rv);
}

int
ipmiconsole_check_authentication_code(ipmiconsole_ctx_t c, 
				       ipmiconsole_packet_type_t p,
				       uint8_t *buf,
				       uint32_t buflen)
{
  uint8_t *password;
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
         || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);
  assert(buf);
  assert(buflen);

  if (strlen(c->config.password))
    password = (uint8_t *)c->config.password;
  else
    password = NULL;

  if ((rv = ipmi_rmcpplus_check_packet_session_authentication_code(c->config.integrity_algorithm,
                                                                   buf,
                                                                   buflen,
                                                                   c->session.integrity_key_ptr,
                                                                   c->session.integrity_key_len,
                                                                   password,
                                                                   (password) ? strlen((char *)password) : 0,
                                                                   c->connection.obj_rmcpplus_session_trlr_rs)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("ipmi_rmcpplus_check_packet_session_authentication_code: p = %d; %s", p, strerror(errno)));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("authentication code check failed; p = %d", p));
  
  return ((int)rv);
}

int 
ipmiconsole_check_outbound_sequence_number(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint32_t shift_num, wrap_val;
  uint32_t session_sequence_number;
  uint64_t val;
  int rv = 0;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
         || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);
  
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

  if (Fiid_obj_get(c,
		   c->connection.obj_rmcpplus_session_hdr_rs,
		   "session_sequence_number", 
		   &val) < 0)
    return -1;
  session_sequence_number = val;

  /* Sequence Number Zero is special and shouldn't be possible */
  if (!session_sequence_number)
    goto out;

  /* Drop duplicate packet */
  if (session_sequence_number == c->session.highest_received_sequence_number)
    goto out;

  /* Check if sequence number is greater than highest received and is
   * within range 
   */
  if (c->session.highest_received_sequence_number > (IPMI_SESSION_MAX_SEQUENCE_NUMBER - IPMI_SESSION_SEQUENCE_NUMBER_WINDOW))
    {
      wrap_val = IPMI_SESSION_SEQUENCE_NUMBER_WINDOW - (IPMI_SESSION_MAX_SEQUENCE_NUMBER - c->session.highest_received_sequence_number);

      if (session_sequence_number > c->session.highest_received_sequence_number || session_sequence_number <= wrap_val)
	{
	  if (session_sequence_number > c->session.highest_received_sequence_number && session_sequence_number <= IPMI_SESSION_MAX_SEQUENCE_NUMBER)
	    shift_num = session_sequence_number - c->session.highest_received_sequence_number;
	  else
	    shift_num = session_sequence_number + (IPMI_SESSION_MAX_SEQUENCE_NUMBER - c->session.highest_received_sequence_number);
          
	  c->session.highest_received_sequence_number = session_sequence_number;
	  c->session.previously_received_list <<= shift_num;
	  c->session.previously_received_list |= (0x1 << (shift_num - 1));
	  rv++;
	}
    }
  else
    {
      if (session_sequence_number > c->session.highest_received_sequence_number
	  && (session_sequence_number - c->session.highest_received_sequence_number) <= IPMI_SESSION_SEQUENCE_NUMBER_WINDOW)
	{
	  shift_num = (session_sequence_number - c->session.highest_received_sequence_number);
	  c->session.highest_received_sequence_number = session_sequence_number;
	  c->session.previously_received_list <<= shift_num;
	  c->session.previously_received_list |= (0x1 << (shift_num - 1));
	  rv++;
	}
    }
  
  /* Check if sequence number is lower than highest received, is
   * within range, and hasn't been seen yet
   */
  if (c->session.highest_received_sequence_number < IPMI_SESSION_SEQUENCE_NUMBER_WINDOW)
    {
      wrap_val = IPMI_SESSION_MAX_SEQUENCE_NUMBER - (IPMI_SESSION_SEQUENCE_NUMBER_WINDOW - c->session.highest_received_sequence_number);

      if (session_sequence_number < c->session.highest_received_sequence_number || session_sequence_number >= wrap_val)
	{
	  if (session_sequence_number > c->session.highest_received_sequence_number && session_sequence_number <= IPMI_SESSION_MAX_SEQUENCE_NUMBER)
	    shift_num = c->session.highest_received_sequence_number + (IPMI_SESSION_MAX_SEQUENCE_NUMBER - session_sequence_number);
	  else
	    shift_num = c->session.highest_received_sequence_number - session_sequence_number;
          
	  /* Duplicate packet check*/
	  if (c->session.previously_received_list & (0x1 << (shift_num - 1)))
	    goto out;
          
	  c->session.previously_received_list |= (0x1 << (shift_num - 1));
	  rv++;
	}
    }
  else
    {
      if (session_sequence_number < c->session.highest_received_sequence_number
	  && session_sequence_number >= (c->session.highest_received_sequence_number - IPMI_SESSION_SEQUENCE_NUMBER_WINDOW))
	{
	  shift_num = c->session.highest_received_sequence_number - session_sequence_number;
          
	  /* Duplicate packet check */
	  if (c->session.previously_received_list & (0x1 << (shift_num - 1)))
	    goto out;
          
	  c->session.previously_received_list |= (0x1 << (shift_num - 1));
	  rv++;
	}
    }
  
 out:
  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("session sequence number check failed; p = %d; session_sequence_number = %u; highest_received_sequence_number = %u", p, session_sequence_number, c->session.highest_received_sequence_number));
  
  return rv;
}

int 
ipmiconsole_check_session_id(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p) 
{
  uint32_t session_id, expected_session_id; 
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
         || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
         || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
         || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
         || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
         || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
         || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
         || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
         || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
         || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2 
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4)
    {
      fiid_obj_t obj_cmd;

      obj_cmd = ipmiconsole_packet_object(c, p);
      
      if (Fiid_obj_get(c,
		       obj_cmd,
		       "remote_console_session_id", 
		       &val) < 0)
        return -1;
      session_id = val;
      expected_session_id = c->session.remote_console_session_id;
    }
  else 
    {
      if (Fiid_obj_get(c,
		       c->connection.obj_rmcpplus_session_hdr_rs,
		       "session_id", 
		       &val) < 0)
        return -1;
      session_id = val;
      expected_session_id = c->session.remote_console_session_id;
    }
  
  if (session_id != expected_session_id)
    IPMICONSOLE_CTX_DEBUG(c, ("session id check failed; p = %d; session_id = %X; expected_session_id = %X", p, session_id, expected_session_id));
  
  return ((session_id == expected_session_id) ? 1 : 0);
}

int 
ipmiconsole_check_network_function(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p) 
{
  uint8_t netfn, expected_netfn;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
         || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);
  
  if (Fiid_obj_get(c,
		   c->connection.obj_lan_msg_hdr_rs, 
		   "net_fn",
		   &val) < 0)
    return -1;
  netfn = val;
  expected_netfn = IPMI_NET_FN_APP_RS;
  
  if (netfn != expected_netfn)
    IPMICONSOLE_CTX_DEBUG(c, ("network function check failed; p = %d; netfn = %X; expected_netfn = %X", p, netfn, expected_netfn));

  return ((netfn == expected_netfn) ? 1 : 0);
}

int 
ipmiconsole_check_command(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p) 
{
  uint8_t cmd, expected_cmd;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);
  
  obj_cmd = ipmiconsole_packet_object(c, p);
  if (Fiid_obj_get(c,
		   obj_cmd, 
		   "cmd",
		   &val) < 0)
    return -1;
  cmd = val;
  
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS)
    expected_cmd = IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES;
  else if (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS)
    expected_cmd = IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL;
  else if (p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS) 
    expected_cmd = IPMI_CMD_GET_CHANNEL_PAYLOAD_SUPPORT;
  else if (p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS) 
    expected_cmd = IPMI_CMD_GET_PAYLOAD_ACTIVATION_STATUS;
  else if (p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS) 
    expected_cmd = IPMI_CMD_ACTIVATE_PAYLOAD;
  else if (p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS) 
    expected_cmd = IPMI_CMD_GET_CHANNEL_PAYLOAD_VERSION;
  else if (p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS) 
    expected_cmd = IPMI_CMD_DEACTIVATE_PAYLOAD;
  else /* p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS */  
    expected_cmd = IPMI_CMD_CLOSE_SESSION;
    
  if (cmd != expected_cmd)
    IPMICONSOLE_CTX_DEBUG(c, ("command check failed; p = %d; cmd = %X; expected_cmd = %X", p, cmd, expected_cmd));
  
  return ((cmd == expected_cmd) ? 1 : 0);
}

int 
ipmiconsole_check_requester_sequence_number(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p) 
{
  uint8_t req_seq, expected_req_seq;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);
    
  if (Fiid_obj_get(c,
		   c->connection.obj_lan_msg_hdr_rs, 
		   "rq_seq",
		   &val) < 0)
    return -1;
  req_seq = val;
  expected_req_seq = c->session.requester_sequence_number;

  if (req_seq != expected_req_seq)
    IPMICONSOLE_CTX_DEBUG(c, ("requester sequence number check failed; p = %d; req_seq = %X; expected_req_seq = %X", p, req_seq, expected_req_seq));
  
  return ((req_seq == expected_req_seq) ? 1 : 0);
}

int 
ipmiconsole_check_completion_code(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p) 
{
  uint8_t comp_code;
  fiid_obj_t obj_cmd;
  uint64_t val;
  
  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);
	 
  obj_cmd = ipmiconsole_packet_object(c, p);
  if (Fiid_obj_get(c,
		   obj_cmd, 
		   "comp_code",
		   &val) < 0)
    return -1;
  comp_code = val;

  if (comp_code != IPMI_COMP_CODE_COMMAND_SUCCESS)
    IPMICONSOLE_CTX_DEBUG(c, ("completion code check failed; p = %d; comp_code = %X", p, comp_code));

  return ((comp_code == IPMI_COMP_CODE_COMMAND_SUCCESS) ? 1 : 0);
}

int
ipmiconsole_check_payload_type(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t payload_type, expected_payload_type;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
	 || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
	 || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
         || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if (Fiid_obj_get(c,
		   c->connection.obj_rmcpplus_session_hdr_rs, 
		   "payload_type",
		   &val) < 0)
    return -1;
  payload_type = val;

  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE;
  else if (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2;
  else if (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4)
    expected_payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4;
  else if (p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS)
    expected_payload_type = IPMI_PAYLOAD_TYPE_SOL;
  else
    expected_payload_type = IPMI_PAYLOAD_TYPE_IPMI;

  if (payload_type != expected_payload_type)
    IPMICONSOLE_CTX_DEBUG(c, ("payload type check failed; p = %d; payload_type = %X; expected_payload_type = %X", p, payload_type, expected_payload_type));

  return ((payload_type == expected_payload_type) ? 1 : 0);
}

int
ipmiconsole_check_message_tag(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t message_tag, expected_message_tag;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
	 || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
	 || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4);

  obj_cmd = ipmiconsole_packet_object(c, p);
  if (Fiid_obj_get(c,
		   obj_cmd,
		   "message_tag",
		   &val) < 0)
    return -1;
  message_tag = val;
  expected_message_tag = c->session.message_tag;

  if (message_tag != expected_message_tag)
    IPMICONSOLE_CTX_DEBUG(c, ("message tag check failed; p = %d", p));

  return ((message_tag == expected_message_tag) ? 1 : 0);
}

int
ipmiconsole_check_rmcpplus_status_code(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t rmcpplus_status_code;
  uint64_t val;
  fiid_obj_t obj_cmd;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
	 || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
	 || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4);

  obj_cmd = ipmiconsole_packet_object(c, p); 
  if (Fiid_obj_get(c,
		   obj_cmd,
		   "rmcpplus_status_code",
		   &val) < 0)
    return -1;
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    IPMICONSOLE_CTX_DEBUG(c, ("rmcpplus status code check failed; p = %d", p));

  return ((rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS) ? 1 : 0);
}

int
ipmiconsole_check_open_session_response_privilege(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t privilege;
  uint64_t val;
  int rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE);

  if (Fiid_obj_get(c,
		   c->connection.obj_open_session_response,
		   "maximum_privilege_level",
		   &val) < 0)
    return -1;
  privilege = val;
  
  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations don't support the highest level privilege.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION)
    rv = (privilege == c->config.privilege_level) ? 1 : 0;
  else
    {
      if (c->config.privilege_level == IPMI_PRIVILEGE_LEVEL_USER
          && (privilege == IPMI_PRIVILEGE_LEVEL_USER
              || privilege == IPMI_PRIVILEGE_LEVEL_OPERATOR
              || privilege == IPMI_PRIVILEGE_LEVEL_ADMIN
	      || privilege == IPMI_PRIVILEGE_LEVEL_OEM))
        rv = 1;
      else if (c->config.privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
               && (privilege == IPMI_PRIVILEGE_LEVEL_OPERATOR
                   || privilege == IPMI_PRIVILEGE_LEVEL_ADMIN
		   || privilege == IPMI_PRIVILEGE_LEVEL_OEM))
        rv = 1;
      else if (c->config.privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
               && (privilege == IPMI_PRIVILEGE_LEVEL_ADMIN
		   || privilege == IPMI_PRIVILEGE_LEVEL_OEM))
        rv = 1;
      else
        rv = 0;
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("open session response privilege check failed; p = %d", p));
  
  return rv; 
}

int
ipmiconsole_check_rakp_2_key_exchange_authentication_code(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int32_t managed_system_random_number_len;
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int32_t managed_system_guid_len;
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  char *username;
  uint32_t username_len;
  char *password;
  uint32_t password_len;
  uint32_t managed_system_session_id;
  uint64_t val;
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2);

  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations pad their usernames.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION)
    {
      memset(username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (strlen(c->config.username))
        strcpy(username_buf, c->config.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      if (strlen(c->config.username))
        username = c->config.username;
      else
        username = NULL;
      username_len = (username) ? strlen(username) : 0;
    }

  /* IPMI Workaround
   * 
   * Supermicro IPMI 2.0 implementations may have invalid payload lengths
   * on the RAKP response packet.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0_SESSION)
    {
      uint8_t keybuf[IPMICONSOLE_PACKET_BUFLEN];
      int32_t keybuf_len;

      if ((keybuf_len = Fiid_obj_get_data(c,
					  c->connection.obj_rakp_message_2,
					  "key_exchange_authentication_code",
					  keybuf,
					  IPMICONSOLE_PACKET_BUFLEN)) < 0)
	return -1;

      if (c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
          && keybuf_len == 1)
	{
	  if (Fiid_obj_clear_field(c,
				   c->connection.obj_rakp_message_2, 
				   "key_exchange_authentication_code") < 0)
	    return -1;
	}
      else if (c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
               && keybuf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
	{
	  if (Fiid_obj_set_data(c,
				c->connection.obj_rakp_message_2,
				"key_exchange_authentication_code",
				keybuf,
				IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
	    return -1;
	}
      else if (c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
               && keybuf_len == (IPMI_HMAC_MD5_DIGEST_LENGTH + 1))
	{
	  if (Fiid_obj_set_data(c,
				c->connection.obj_rakp_message_2,
				"key_exchange_authentication_code",
				keybuf,
				IPMI_HMAC_MD5_DIGEST_LENGTH) < 0)
	    return -1;
	}
    }

  if (strlen(c->config.password))
    password = c->config.password;
  else
    password = NULL;
  password_len = (password) ? strlen(password) : 0;

  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations improperly calculate HMAC-MD5-128 hashes
   * when the passwords are > 16 bytes long.  The BMCs probably assume
   * all keys are <= 16 bytes in length.  So we have to adjust.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION
      && c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;

  /* IPMI Workaround
   *
   * Discovered on Sun Fire 4100.
   *
   * The key exchange authentication code is the wrong length.  We
   * need to shorten it.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_SUN_2_0_SESSION
      && c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      uint8_t buf[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
      int32_t buf_len;

      buf_len = Fiid_obj_get_data(c,
                                  c->connection.obj_rakp_message_2,
                                  "key_exchange_authentication_code",
                                  buf,
                                  IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);

      if (buf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          Fiid_obj_clear_field(c,
                               c->connection.obj_rakp_message_2,
                               "key_exchange_authentication_code");
          Fiid_obj_set_data(c,
                            c->connection.obj_rakp_message_2,
                            "key_exchange_authentication_code",
                            buf,
                            IPMI_HMAC_SHA1_DIGEST_LENGTH);
        }
    }


  if (Fiid_obj_get(c,
		   c->connection.obj_open_session_response,
		   "managed_system_session_id",
		   &val) < 0)
    return -1;
  managed_system_session_id = val;
  
  if ((managed_system_random_number_len = Fiid_obj_get_data(c,
							    c->connection.obj_rakp_message_2,
							    "managed_system_random_number",
							    managed_system_random_number,
							    IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    return -1;

  if (managed_system_random_number_len != IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_get_data: invalid managed system random number length: %d", managed_system_random_number_len));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  if ((managed_system_guid_len = Fiid_obj_get_data(c,
						   c->connection.obj_rakp_message_2,
						   "managed_system_guid",
						   managed_system_guid,
						   IPMI_MANAGED_SYSTEM_GUID_LENGTH)) < 0)
    return -1;

  if (managed_system_guid_len != IPMI_MANAGED_SYSTEM_GUID_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_get_data: invalid managed system guid length: %d", managed_system_guid_len));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }
  
  if ((rv = ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code(c->config.authentication_algorithm,
                                                                        (uint8_t *)password,
                                                                        password_len,
                                                                        c->session.remote_console_session_id,
                                                                        managed_system_session_id,
                                                                        c->session.remote_console_random_number,
                                                                        IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                                        managed_system_random_number,
                                                                        managed_system_random_number_len,
                                                                        managed_system_guid,
                                                                        managed_system_guid_len,
                                                                        c->session.name_only_lookup,
                                                                        c->config.privilege_level,
                                                                        username,
                                                                        username_len,
                                                                        c->connection.obj_rakp_message_2)) < 0)
    IPMICONSOLE_CTX_DEBUG(c, ("ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code: p = %d; %s", p, strerror(errno)));
  
  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("rakp 2 key exchanged authentication code check failed; p = %d", p));

  return ((int)rv);
}

int
ipmiconsole_check_rakp_4_integrity_check_value(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int32_t managed_system_guid_len;
  uint32_t managed_system_session_id;
  uint8_t authentication_algorithm = 0;
  uint64_t val;
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4);

  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations respond with the integrity check
   * value based on the integrity algorithm rather than the
   * authentication algorithm.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION)
    {
      if (c->config.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
      else if (c->config.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1;
      else if (c->config.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5;
      else if (c->config.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        /* achu: I have not been able to reverse engineer this.  So accept it */
        return 1;
    }
  else
    authentication_algorithm = c->config.authentication_algorithm;

  if (Fiid_obj_get(c,
		   c->connection.obj_open_session_response,
		   "managed_system_session_id",
		   &val) < 0)
    return -1;
  managed_system_session_id = val;

  if ((managed_system_guid_len = Fiid_obj_get_data(c,
						   c->connection.obj_rakp_message_2,
						   "managed_system_guid",
						   managed_system_guid,
						   IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    return -1;
  if (managed_system_guid_len != IPMI_MANAGED_SYSTEM_GUID_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_get_data: invalid managed system guid length: %d", managed_system_guid_len));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  if ((rv = ipmi_rmcpplus_check_rakp_4_integrity_check_value(authentication_algorithm,
                                                             c->session.sik_key_ptr,
                                                             c->session.sik_key_len,
                                                             c->session.remote_console_random_number,
                                                             IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                             managed_system_session_id,
                                                             managed_system_guid,
                                                             managed_system_guid_len,
                                                             c->connection.obj_rakp_message_4)) < 0)
    IPMICONSOLE_CTX_DEBUG(c, ("ipmi_rmcpplus_check_rakp_4_integrity_check_value: p = %d; %s", p, strerror(errno)));

  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("rakp 4 integrity check value check failed; p = %d", p));

  return ((int)rv);
}

int
ipmiconsole_check_payload_pad(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
         || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if ((rv = ipmi_rmcpplus_check_payload_pad(c->config.confidentiality_algorithm,
					    c->connection.obj_rmcpplus_payload_rs)) < 0)
    IPMICONSOLE_CTX_DEBUG(c, ("ipmi_rmcpplus_check_payload_pad: p = %d; %s", p, strerror(errno)));

  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("payload pad check failed; p = %d", p));

  return ((int)rv);
}

int
ipmiconsole_check_integrity_pad(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(p));
  assert(p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS 
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
	 || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
         || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
	 || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
	 || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
	 || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if ((rv = ipmi_rmcpplus_check_integrity_pad(c->connection.obj_rmcpplus_session_trlr_rs)) < 0)

    IPMICONSOLE_CTX_DEBUG(c, ("ipmi_rmcpplus_check_integrity_pad: p = %d; %s", p, strerror(errno)));

  if (!rv)
    IPMICONSOLE_CTX_DEBUG(c, ("integrity pad check failed; p = %d", p));

  return ((int)rv);
}

