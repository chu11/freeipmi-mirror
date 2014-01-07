/*****************************************************************************\
 *  $Id: ipmiconsole_checks.c,v 1.48 2010-08-03 00:10:59 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
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
#include "ipmiconsole_packet.h"

#include "freeipmi-portability.h"

int
ipmiconsole_check_checksum (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  fiid_obj_t obj_cmd;
  int rv;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS
          || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  /* IPMI Workaround
   *
   * Discovered on Supermicro X9SCM-iiF, Supermicro X9DRi-F
   *
   * Checksums are computed incorrectly.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_NO_CHECKSUM_CHECK)
    return (1);

  obj_cmd = ipmiconsole_packet_object (c, p);
  if ((rv = ipmi_lan_check_checksum (c->connection.obj_lan_msg_hdr_rs,
                                     obj_cmd,
                                     c->connection.obj_lan_msg_trlr_rs)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_lan_check_checksum: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("checksum check failed; p = %d", p));

  return (rv);
}

int
ipmiconsole_check_authentication_code (ipmiconsole_ctx_t c,
                                       ipmiconsole_packet_type_t p,
                                       void *buf,
                                       unsigned int buflen)
{
  char *password;
  int rv;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);
  assert (buf);
  assert (buflen);

  if (strlen (c->config.password))
    password = c->config.password;
  else
    password = NULL;

  if ((rv = ipmi_rmcpplus_check_packet_session_authentication_code (c->config.integrity_algorithm,
                                                                    buf,
                                                                    buflen,
                                                                    c->session.integrity_key_ptr,
                                                                    c->session.integrity_key_len,
                                                                    password,
                                                                    (password) ? strlen (password) : 0,
                                                                    c->connection.obj_rmcpplus_session_trlr_rs)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_rmcpplus_check_packet_session_authentication_code: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("authentication code check failed; p = %d", p));

  return (rv);
}

int
ipmiconsole_check_outbound_sequence_number (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint32_t session_sequence_number;
  uint64_t val;
  int rv = 0;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if (FIID_OBJ_GET (c->connection.obj_rmcpplus_session_hdr_rs,
                    "session_sequence_number",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'session_sequence_number': %s",
                                 fiid_obj_errormsg (c->connection.obj_rmcpplus_session_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  session_sequence_number = val;

  if ((rv = ipmi_check_session_sequence_number_2_0 (session_sequence_number,
                                                    &(c->session.highest_received_sequence_number),
                                                    &(c->session.previously_received_list),
                                                    0)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_check_session_sequence_number_2_0: 'session_sequence_number': %s",
                                 fiid_obj_errormsg (c->connection.obj_rmcpplus_session_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("session sequence number check failed; p = %d; session_sequence_number = %u; highest_received_sequence_number = %u", p, session_sequence_number, c->session.highest_received_sequence_number));

  return (rv);
}

int
ipmiconsole_check_session_id (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint32_t session_id, expected_session_id;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
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

      obj_cmd = ipmiconsole_packet_object (c, p);

      if (FIID_OBJ_GET (obj_cmd,
                        "remote_console_session_id",
                        &val) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'remote_console_session_id': %s",
                                     fiid_obj_errormsg (obj_cmd)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      session_id = val;
      expected_session_id = c->session.remote_console_session_id;
    }
  else
    {
      if (FIID_OBJ_GET (c->connection.obj_rmcpplus_session_hdr_rs,
                        "session_id",
                        &val) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'session_id': %s",
                                     fiid_obj_errormsg (c->connection.obj_rmcpplus_session_hdr_rs)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      session_id = val;
      expected_session_id = c->session.remote_console_session_id;
    }

  if (session_id != expected_session_id)
    IPMICONSOLE_CTX_DEBUG (c, ("session id check failed; p = %d; session_id = %Xh; expected_session_id = %Xh", p, session_id, expected_session_id));

  return ((session_id == expected_session_id) ? 1 : 0);
}

int
ipmiconsole_check_network_function (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t netfn, expected_netfn;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS
          || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if (FIID_OBJ_GET (c->connection.obj_lan_msg_hdr_rs,
                    "net_fn",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'net_fn': %s",
                                 fiid_obj_errormsg (c->connection.obj_lan_msg_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  netfn = val;
  expected_netfn = IPMI_NET_FN_APP_RS;

  if (netfn != expected_netfn)
    IPMICONSOLE_CTX_DEBUG (c, ("network function check failed; p = %d; netfn = %Xh; expected_netfn = %Xh", p, netfn, expected_netfn));

  return ((netfn == expected_netfn) ? 1 : 0);
}

int
ipmiconsole_check_command (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t cmd, expected_cmd;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS
          || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  obj_cmd = ipmiconsole_packet_object (c, p);
  if (FIID_OBJ_GET (obj_cmd,
                    "cmd",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'cmd': %s",
                                 fiid_obj_errormsg (obj_cmd)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  cmd = val;

  switch (p)
    {
    case IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS:
      expected_cmd = IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES;
      break;
    case IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS:
      expected_cmd = IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL;
      break;
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS:
      expected_cmd = IPMI_CMD_GET_CHANNEL_PAYLOAD_SUPPORT;
      break;
    case IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS:
      expected_cmd = IPMI_CMD_GET_PAYLOAD_ACTIVATION_STATUS;
      break;
    case IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS:
      expected_cmd = IPMI_CMD_ACTIVATE_PAYLOAD;
      break;
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS:
      expected_cmd = IPMI_CMD_GET_CHANNEL_PAYLOAD_VERSION;
      break;
    case IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS:
      expected_cmd = IPMI_CMD_DEACTIVATE_PAYLOAD;
      break;
    case IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS:
      expected_cmd = IPMI_CMD_CLOSE_SESSION;
      break;
    default:
      IPMICONSOLE_CTX_DEBUG (c, ("invalid packet type: p = %d", p));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (cmd != expected_cmd)
    IPMICONSOLE_CTX_DEBUG (c, ("command check failed; p = %d; cmd = %Xh; expected_cmd = %Xh", p, cmd, expected_cmd));

  return ((cmd == expected_cmd) ? 1 : 0);
}

int
ipmiconsole_check_requester_sequence_number (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t req_seq, expected_req_seq;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS
          || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if (FIID_OBJ_GET (c->connection.obj_lan_msg_hdr_rs,
                    "rq_seq",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'rq_seq': %s",
                                 fiid_obj_errormsg (c->connection.obj_lan_msg_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  req_seq = val;
  expected_req_seq = c->session.requester_sequence_number;

  if (req_seq != expected_req_seq)
    IPMICONSOLE_CTX_DEBUG (c, ("requester sequence number check failed; p = %d; req_seq = %Xh; expected_req_seq = %Xh", p, req_seq, expected_req_seq));

  return ((req_seq == expected_req_seq) ? 1 : 0);
}

int
ipmiconsole_check_completion_code (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t comp_code;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS
          || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  obj_cmd = ipmiconsole_packet_object (c, p);
  if (FIID_OBJ_GET (obj_cmd,
                    "comp_code",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'comp_code': %s",
                                 fiid_obj_errormsg (obj_cmd)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  comp_code = val;

  if (comp_code != IPMI_COMP_CODE_COMMAND_SUCCESS)
    IPMICONSOLE_CTX_DEBUG (c, ("completion code check failed; p = %d; comp_code = %Xh", p, comp_code));

  return ((comp_code == IPMI_COMP_CODE_COMMAND_SUCCESS) ? 1 : 0);
}

int
ipmiconsole_check_payload_type (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t payload_type, expected_payload_type;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
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

  if (FIID_OBJ_GET (c->connection.obj_rmcpplus_session_hdr_rs,
                    "payload_type",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'payload_type': %s",
                                 fiid_obj_errormsg (c->connection.obj_rmcpplus_session_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
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
    IPMICONSOLE_CTX_DEBUG (c, ("payload type check failed; p = %d; payload_type = %Xh; expected_payload_type = %Xh", p, payload_type, expected_payload_type));

  return ((payload_type == expected_payload_type) ? 1 : 0);
}

int
ipmiconsole_check_message_tag (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t message_tag, expected_message_tag;
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4);

  obj_cmd = ipmiconsole_packet_object (c, p);
  if (FIID_OBJ_GET (obj_cmd,
                    "message_tag",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'message_tag': %s",
                                 fiid_obj_errormsg (obj_cmd)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  message_tag = val;
  expected_message_tag = c->session.message_tag;

  if (message_tag != expected_message_tag)
    IPMICONSOLE_CTX_DEBUG (c, ("message tag check failed; p = %d", p));

  return ((message_tag == expected_message_tag) ? 1 : 0);
}

int
ipmiconsole_check_rmcpplus_status_code (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t rmcpplus_status_code;
  uint64_t val;
  fiid_obj_t obj_cmd;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4);

  obj_cmd = ipmiconsole_packet_object (c, p);
  if (FIID_OBJ_GET (obj_cmd,
                    "rmcpplus_status_code",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'rmcpplus_status_code': %s",
                                 fiid_obj_errormsg (obj_cmd)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    IPMICONSOLE_CTX_DEBUG (c, ("rmcpplus status code check failed; p = %d", p));

  return ((rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS) ? 1 : 0);
}

int
ipmiconsole_check_packet (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  fiid_obj_t obj_cmd;
  int rv;
  
  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));

  obj_cmd = ipmiconsole_packet_object (c, p);
  if ((rv = fiid_obj_packet_valid (obj_cmd)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_packet_valid: %s",
                                 fiid_obj_errormsg (obj_cmd)));
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("invalid packet received; p = %d", p));

  return (rv);
}

int
ipmiconsole_check_open_session_response_privilege (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  int rv;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE);

  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations don't support the highest level privilege.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION)
    {
      uint8_t maximum_privilege_level;
      uint64_t val;

      if (FIID_OBJ_GET (c->connection.obj_open_session_response,
                        "maximum_privilege_level",
                        &val) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'maximum_privilege_level': %s",
                                     fiid_obj_errormsg (c->connection.obj_open_session_response)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      maximum_privilege_level = val;
      
      rv = (maximum_privilege_level == c->config.privilege_level) ? 1 : 0;
    }
  else
    {
      if ((rv = ipmi_check_open_session_maximum_privilege (c->config.privilege_level,
                                                           c->connection.obj_open_session_response)) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("ipmi_check_open_session_maximum_privilege: %s",
                                     strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("open session response privilege check failed; p = %d", p));

  return (rv);
}

int
ipmiconsole_check_rakp_2_key_exchange_authentication_code (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int managed_system_random_number_len;
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int managed_system_guid_len;
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  char *username;
  unsigned int username_len;
  char *password;
  unsigned int password_len;
  uint32_t managed_system_session_id;
  uint64_t val;
  int rv;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2);

  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations pad their usernames.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION)
    {
      memset (username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (strlen (c->config.username))
        strcpy (username_buf, c->config.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      if (strlen (c->config.username))
        username = c->config.username;
      else
        username = NULL;
      username_len = (username) ? strlen (username) : 0;
    }

  /* IPMI Workaround
   *
   * Supermicro IPMI 2.0 implementations may have invalid payload lengths
   * on the RAKP response packet.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0_SESSION)
    {
      uint8_t keybuf[IPMICONSOLE_PACKET_BUFLEN];
      int keybuf_len;

      if ((keybuf_len = fiid_obj_get_data (c->connection.obj_rakp_message_2,
                                           "key_exchange_authentication_code",
                                           keybuf,
                                           IPMICONSOLE_PACKET_BUFLEN)) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: 'key_exchange_authentication_code': %s",
                                     fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }

      if (c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
          && keybuf_len == 1)
        {
          if (fiid_obj_clear_field (c->connection.obj_rakp_message_2,
                                    "key_exchange_authentication_code") < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear_field: 'key_exchange_authentication_code': %s",
                                         fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }
        }
      else if (c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
               && keybuf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (c->connection.obj_rakp_message_2,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                                         fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }
        }
      else if (c->config.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
               && keybuf_len == (IPMI_HMAC_MD5_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (c->connection.obj_rakp_message_2,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_MD5_DIGEST_LENGTH) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                                         fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }
        }
    }

  if (strlen (c->config.password))
    password = c->config.password;
  else
    password = NULL;
  password_len = (password) ? strlen (password) : 0;

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
      int buf_len;

      /* trace, but do not return potential error */

      if ((buf_len = fiid_obj_get_data (c->connection.obj_rakp_message_2,
                                        "key_exchange_authentication_code",
                                        buf,
                                        IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
        IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: 'key_exchange_authentication_code': %s",
                                   fiid_obj_errormsg (c->connection.obj_rakp_message_2)));

      if (buf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          /* trace, but do not return potential error */

          if (fiid_obj_clear_field (c->connection.obj_rakp_message_2,
                                    "key_exchange_authentication_code") < 0)
            IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear_field: 'key_exchange_authentication_code': %s",
                                       fiid_obj_errormsg (c->connection.obj_rakp_message_2)));

          if (fiid_obj_set_data (c->connection.obj_rakp_message_2,
                                 "key_exchange_authentication_code",
                                 buf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_set_data: 'key_exchange_authentication_code': %s",
                                       fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
        }
    }


  if (FIID_OBJ_GET (c->connection.obj_open_session_response,
                    "managed_system_session_id",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'managed_system_session_id': %s",
                                 fiid_obj_errormsg (c->connection.obj_open_session_response)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  managed_system_session_id = val;

  if ((managed_system_random_number_len = fiid_obj_get_data (c->connection.obj_rakp_message_2,
                                                             "managed_system_random_number",
                                                             managed_system_random_number,
                                                             IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: 'managed_system_random_number': %s",
                                 fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (managed_system_random_number_len != IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: invalid managed system random number length: %d",
                                 managed_system_random_number_len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((managed_system_guid_len = fiid_obj_get_data (c->connection.obj_rakp_message_2,
                                                    "managed_system_guid",
                                                    managed_system_guid,
                                                    IPMI_MANAGED_SYSTEM_GUID_LENGTH)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: 'managed_system_guid': %s",
                                 fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (managed_system_guid_len != IPMI_MANAGED_SYSTEM_GUID_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: invalid managed system guid length: %d",
                                 managed_system_guid_len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((rv = ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code (c->config.authentication_algorithm,
                                                                         password,
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
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("rakp 2 key exchanged authentication code check failed; p = %d", p));

  return (rv);
}

int
ipmiconsole_check_rakp_4_integrity_check_value (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int managed_system_guid_len;
  uint32_t managed_system_session_id;
  uint8_t authentication_algorithm = 0;
  uint64_t val;
  int rv;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4);

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
        {
          /* achu: I have thus far been unable to reverse engineer this
           * corner case.  Since we cannot provide a reasonable two
           * part authentication, we're going to error out.
           */
          IPMICONSOLE_CTX_DEBUG (c, ("Intel Non-Compliance: Cannot Reverse Engineer"));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_BMC_ERROR);
          return (0);
        }
    }
  else
    authentication_algorithm = c->config.authentication_algorithm;

  if (FIID_OBJ_GET (c->connection.obj_open_session_response,
                    "managed_system_session_id",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("FIID_OBJ_GET: 'managed_system_session_id': %s",
                                 fiid_obj_errormsg (c->connection.obj_open_session_response)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  managed_system_session_id = val;

  if ((managed_system_guid_len = fiid_obj_get_data (c->connection.obj_rakp_message_2,
                                                    "managed_system_guid",
                                                    managed_system_guid,
                                                    IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: 'managed_system_guid': %s",
                                 fiid_obj_errormsg (c->connection.obj_rakp_message_2)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (managed_system_guid_len != IPMI_MANAGED_SYSTEM_GUID_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: invalid managed system guid length: %d",
                                 managed_system_guid_len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro X8DTG, Supermicro X8DTU, Intel
   * S5500WBV/Penguin Relion 700
   *
   * For whatever reason, with cipher suite 0, the RAKP 4 response
   * returns with an Integrity Check Value when it should be empty.
   */
  
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_NON_EMPTY_INTEGRITY_CHECK_VALUE
      && !c->config.cipher_suite_id)
    {
      if (fiid_obj_clear_field (c->connection.obj_rakp_message_4,
                                "integrity_check_value") < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear_field: 'integrity_check_value': %s",
                                     fiid_obj_errormsg (c->connection.obj_rakp_message_4)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
    }

  if ((rv = ipmi_rmcpplus_check_rakp_4_integrity_check_value (authentication_algorithm,
                                                              c->session.sik_key_ptr,
                                                              c->session.sik_key_len,
                                                              c->session.remote_console_random_number,
                                                              IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                              managed_system_session_id,
                                                              managed_system_guid,
                                                              managed_system_guid_len,
                                                              c->connection.obj_rakp_message_4)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_rmcpplus_check_rakp_4_integrity_check_value: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("rakp 4 integrity check value check failed; p = %d", p));

  return (rv);
}

int
ipmiconsole_check_payload_pad (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  int rv;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if ((rv = ipmi_rmcpplus_check_payload_pad (c->config.confidentiality_algorithm,
                                             c->connection.obj_rmcpplus_payload_rs)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_rmcpplus_check_payload_pad: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("payload pad check failed; p = %d", p));

  return (rv);
}

int
ipmiconsole_check_integrity_pad (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  int rv;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));
  assert (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
          || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS
          || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS
          || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS
          || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

  if (c->config.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    return (1);

  if ((rv = ipmi_rmcpplus_check_integrity_pad (c->connection.obj_rmcpplus_session_trlr_rs)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_rmcpplus_check_integrity_pad: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!rv)
    IPMICONSOLE_CTX_DEBUG (c, ("integrity pad check failed; p = %d", p));

  return (rv);
}

