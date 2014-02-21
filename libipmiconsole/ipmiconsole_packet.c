/*****************************************************************************\
 *  $Id: ipmiconsole_packet.c,v 1.64 2010-08-03 00:10:59 chu11 Exp $
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
#include <assert.h>
#include <errno.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "ipmiconsole_packet.h"
#include "ipmiconsole_ctx.h"
#include "ipmiconsole_debug.h"

#include "freeipmi-portability.h"
#include "cbuf.h"
#include "debug-util.h"
#include "secure.h"

#define IPMICONSOLE_MAX_PACKET_DUMP_HDR_LEN 1024

fiid_field_t *
ipmiconsole_packet_template (ipmiconsole_ctx_t c,
                             ipmiconsole_packet_type_t p)
{
  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_VALID (p));

  switch (p)
    {
    case IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ:
      return (&tmpl_cmd_get_channel_authentication_capabilities_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS:
      return (&tmpl_cmd_get_channel_authentication_capabilities_rs[0]);
    case IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST:
      return (&tmpl_rmcpplus_open_session_request[0]);
    case IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE:
      return (&tmpl_rmcpplus_open_session_response[0]);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1:
      return (&tmpl_rmcpplus_rakp_message_1[0]);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2:
      return (&tmpl_rmcpplus_rakp_message_2[0]);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3:
      return (&tmpl_rmcpplus_rakp_message_3[0]);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4:
      return (&tmpl_rmcpplus_rakp_message_4[0]);
    case IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ:
      return (&tmpl_cmd_set_session_privilege_level_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS:
      return (&tmpl_cmd_set_session_privilege_level_rs[0]);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ:
      return (&tmpl_cmd_get_channel_payload_support_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS:
      return (&tmpl_cmd_get_channel_payload_support_rs[0]);
    case IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ:
      return (&tmpl_cmd_get_payload_activation_status_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS:
      return (&tmpl_cmd_get_payload_activation_status_rs[0]);
    case IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ:
      return (&tmpl_cmd_activate_payload_sol_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS:
      return (&tmpl_cmd_activate_payload_sol_rs[0]);
    case IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ:
      return (&tmpl_sol_payload_data_remote_console_to_bmc[0]);
    case IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS:
      return (&tmpl_sol_payload_data_bmc_to_remote_console[0]);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ:
      return (&tmpl_cmd_get_channel_payload_version_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS:
      return (&tmpl_cmd_get_channel_payload_version_rs[0]);
    case IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ:
      return (&tmpl_cmd_deactivate_payload_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS:
      return (&tmpl_cmd_deactivate_payload_rs[0]);
    case IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ:
      return (&tmpl_cmd_chassis_control_rq[0]);
    case IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS:
      return (&tmpl_cmd_chassis_control_rs[0]);
    default:
      IPMICONSOLE_CTX_DEBUG (c, ("invalid packet type: %d", p));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (NULL);
    }

  return (NULL);			/* NOT REACHED */
}

fiid_obj_t
ipmiconsole_packet_object (ipmiconsole_ctx_t c,
                           ipmiconsole_packet_type_t p)
{
  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_VALID (p));

  switch (p)
    {
    case IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ:
      return (c->connection.obj_authentication_capabilities_rq);
    case IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS:
      return (c->connection.obj_authentication_capabilities_rs);
    case IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST:
      return (c->connection.obj_open_session_request);
    case IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE:
      return (c->connection.obj_open_session_response);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1:
      return (c->connection.obj_rakp_message_1);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2:
      return (c->connection.obj_rakp_message_2);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3:
      return (c->connection.obj_rakp_message_3);
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4:
      return (c->connection.obj_rakp_message_4);
    case IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ:
      return (c->connection.obj_set_session_privilege_level_rq);
    case IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS:
      return (c->connection.obj_set_session_privilege_level_rs);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ:
      return (c->connection.obj_get_channel_payload_support_rq);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS:
      return (c->connection.obj_get_channel_payload_support_rs);
    case IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ:
      return (c->connection.obj_get_payload_activation_status_rq);
    case IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS:
      return (c->connection.obj_get_payload_activation_status_rs);
    case IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ:
      return (c->connection.obj_activate_payload_rq);
    case IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS:
      return (c->connection.obj_activate_payload_rs);
    case IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ:
      return (c->connection.obj_sol_payload_data_rq);
    case IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS:
      return (c->connection.obj_sol_payload_data_rs);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ:
      return (c->connection.obj_get_channel_payload_version_rq);
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS:
      return (c->connection.obj_get_channel_payload_version_rs);
    case IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ:
      return (c->connection.obj_deactivate_payload_rq);
    case IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS:
      return (c->connection.obj_deactivate_payload_rs);
    case IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ:
      return (c->connection.obj_close_session_rq);
    case IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS:
      return (c->connection.obj_close_session_rs);
    default:
      IPMICONSOLE_CTX_DEBUG (c, ("invalid packet type: %d", p));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (NULL);
    }

  return (NULL);			/* NOT REACHED */
}

static int
_packet_dump_hdr (ipmiconsole_ctx_t c,
                  ipmiconsole_packet_type_t p,
                  char *hdr,
                  unsigned int hdrlen)
{
  uint8_t packet_type;
  uint8_t packet_direction;
  const char *str_cmd = NULL;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_VALID (p));
  assert (hdr);
  assert (hdrlen);

  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS)
    packet_type = DEBUG_UTIL_TYPE_IPMI_1_5;
  else
    packet_type = DEBUG_UTIL_TYPE_IPMI_2_0;

  switch (p)
    {
    case IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ:
    case IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES);
      break;
    case IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST:
    case IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE:
      str_cmd = DEBUG_UTIL_OPEN_SESSION_STR;
      break;
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1:
      str_cmd = DEBUG_UTIL_RAKP_1_STR;
      break;
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2:
      str_cmd = DEBUG_UTIL_RAKP_2_STR;
      break;
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3:
      str_cmd = DEBUG_UTIL_RAKP_3_STR;
      break;
    case IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4:
      str_cmd = DEBUG_UTIL_RAKP_4_STR;
      break;
    case IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ:
    case IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL);
      break;
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ:
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_GET_CHANNEL_PAYLOAD_SUPPORT);
      break;
    case IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ:
    case IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_GET_PAYLOAD_ACTIVATION_STATUS);
      break;
    case IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ:
    case IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_ACTIVATE_PAYLOAD);
      break;
    case IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ:
      str_cmd = "SOL Remote Console to BMC";
      break;
    case IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS:
      str_cmd = "SOL BMC to Remote Console";
      break;
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ:
    case IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_GET_CHANNEL_PAYLOAD_VERSION);
      break;
    case IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ:
    case IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_DEACTIVATE_PAYLOAD);
      break;
    case IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ:
    case IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS:
      str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_CLOSE_SESSION);
      break;
    default:
      IPMICONSOLE_CTX_DEBUG (c, ("invalid packet type: %d", p));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  /* special case: there is no "Request/Response" with SOL data */
  if (p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ
      || p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS)
    packet_direction = DEBUG_UTIL_DIRECTION_NONE;
  else if (IPMICONSOLE_PACKET_TYPE_REQUEST (p))
    packet_direction = DEBUG_UTIL_DIRECTION_REQUEST;
  else
    packet_direction = DEBUG_UTIL_DIRECTION_RESPONSE;

  if (debug_hdr_str (packet_type,
                     packet_direction,
		     DEBUG_UTIL_FLAGS_DEFAULT,
                     str_cmd,
                     hdr,
                     hdrlen) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("debug_hdr_str: p = %d", p));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (0);
}

int
ipmiconsole_packet_dump (ipmiconsole_ctx_t c,
                         ipmiconsole_packet_type_t p,
                         const void *buf,
                         unsigned int buflen)
{
  fiid_field_t *tmpl_lan_msg_hdr;
  fiid_field_t *tmpl_cmd;
  char hdr[IPMICONSOLE_MAX_PACKET_DUMP_HDR_LEN];
  int fd;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_VALID (p));
  assert (buf);
  assert (buflen);

  if (c->config.debug_flags & IPMICONSOLE_DEBUG_STDOUT)
    fd = STDOUT_FILENO;
  else if (c->config.debug_flags & IPMICONSOLE_DEBUG_STDERR)
    fd = STDERR_FILENO;
  else if (c->config.debug_flags & IPMICONSOLE_DEBUG_FILE)
    fd = c->debug.debug_fd;
  else
    return (0);

  tmpl_cmd = ipmiconsole_packet_template (c, p);

  if (IPMICONSOLE_PACKET_TYPE_REQUEST (p))
    tmpl_lan_msg_hdr = &tmpl_lan_msg_hdr_rq[0];
  else
    tmpl_lan_msg_hdr = &tmpl_lan_msg_hdr_rs[0];

  if (_packet_dump_hdr (c, p, hdr, IPMICONSOLE_MAX_PACKET_DUMP_HDR_LEN) < 0)
    return (-1);

  /* IPMI 1.5 Style Packets */
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS)
    {
      if (ipmi_dump_lan_packet (fd,
                                c->config.hostname,
                                hdr,
                                NULL,
                                buf,
                                buflen,
                                tmpl_lan_msg_hdr,
                                tmpl_cmd) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("ipmi_dump_lan_packet: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
    }
  else /* IPMI 2.0 Style Packets */
    {
      /* IPMI 2.0 RAKP Session Setup Packets use no cipher
       * algorithms
       */
      if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST
          || p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3
          || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4)
        {
          if (ipmi_dump_rmcpplus_packet (fd,
                                         c->config.hostname,
                                         hdr,
                                         NULL,
                                         IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE,
                                         IPMI_INTEGRITY_ALGORITHM_NONE,
                                         IPMI_CONFIDENTIALITY_ALGORITHM_NONE,
                                         NULL,
                                         0,
                                         NULL,
                                         0,
                                         buf,
                                         buflen,
                                         tmpl_lan_msg_hdr,
                                         tmpl_cmd) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("ipmi_dump_rmcpplus_packet: p = %d; %s", p, strerror (errno)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }
        }
      else
        {
          if (ipmi_dump_rmcpplus_packet (fd,
                                         c->config.hostname,
                                         hdr,
                                         NULL,
                                         c->config.authentication_algorithm,
                                         c->config.integrity_algorithm,
                                         c->config.confidentiality_algorithm,
                                         c->session.integrity_key_ptr,
                                         c->session.integrity_key_len,
                                         c->session.confidentiality_key_ptr,
                                         c->session.confidentiality_key_len,
                                         buf,
                                         buflen,
                                         tmpl_lan_msg_hdr,
                                         tmpl_cmd) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("ipmi_dump_rmcpplus_packet: p = %d; %s", p, strerror (errno)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }

        }
    }

  return (0);
}

static int
_packet_dump_unknown_hdr (ipmiconsole_ctx_t c,
                          const void *buf,
                          unsigned int buflen,
                          char *hdr,
                          unsigned int hdrlen)
{
  char *fmt =
    "================================================\n"
    "%s\n"
    "================================================";
  char *str;
  int ret;
  int len;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (buf);
  assert (buflen);
  assert (hdr);
  assert (hdrlen);

  if ((ret = ipmi_is_ipmi_1_5_packet (buf, buflen)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_is_ipmi_1_5_packet: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (ret)
    str = "= Unexpected IPMI 1.5 Packet                   =";
  else
    {
      uint8_t payload_type;

      if (ipmi_rmcpplus_calculate_payload_type (buf, buflen, &payload_type) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("ipmi_rmcpplus_calculate_payload_type: %s", strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }

      switch (payload_type)
	{
	case IPMI_PAYLOAD_TYPE_SOL:
	  str = "= Unexpected SOL Packet                        =";
	  break;
	case IPMI_PAYLOAD_TYPE_IPMI:
	  str = "= Unexpected IPMI 2.0 Packet                   =";
	  break;
	case IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST:
	  str = "= Unexpected Open Session Request              =";
	  break;
	case IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE:
	  str = "= Unexpected Open Session Response             =";
	  break;
	case IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1:
	  str = "= Unexpected RAKP Message 1                    =";
	  break;
	case IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2:
	  str = "= Unexpected RAKP Message 2                    =";
	  break;
	case IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3:
	  str = "= Unexpected RAKP Message 3                    =";
	  break;
	case IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4:
	  str = "= Unexpected RAKP Message 4                    =";
	  break;
	default:
	  str = "= Unexpected Packet                            =";
	}
    }

  if ((len = snprintf (hdr, hdrlen, fmt, str)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("snprintf"));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (len >= hdrlen)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("snprintf truncation: len = %d", len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (len);
}

int
ipmiconsole_packet_dump_unknown (ipmiconsole_ctx_t c,
                                 const void *buf,
                                 unsigned int buflen)
{
  char hdr[IPMICONSOLE_MAX_PACKET_DUMP_HDR_LEN];
  int fd;
  int ret;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (buf);
  assert (buflen);

  if (c->config.debug_flags & IPMICONSOLE_DEBUG_STDOUT)
    fd = STDOUT_FILENO;
  else if (c->config.debug_flags & IPMICONSOLE_DEBUG_STDERR)
    fd = STDERR_FILENO;
  else if (c->config.debug_flags & IPMICONSOLE_DEBUG_FILE)
    fd = c->debug.debug_fd;
  else
    return (0);

  if (_packet_dump_unknown_hdr (c, buf, buflen, hdr, IPMICONSOLE_MAX_PACKET_DUMP_HDR_LEN) < 0)
    return (-1);

  if ((ret = ipmi_is_ipmi_1_5_packet (buf, buflen)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_is_ipmi_1_5_packet: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (ret)
    {
      if (ipmi_dump_lan_packet (fd,
                                c->config.hostname,
                                hdr,
                                NULL,
                                buf,
                                buflen,
                                tmpl_lan_msg_hdr_rs,
                                tmpl_unexpected_data) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("ipmi_dump_lan_packet: %s", strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
    }
  else
    {
      if (ipmi_dump_rmcpplus_packet (fd,
                                     c->config.hostname,
                                     hdr,
                                     NULL,
                                     c->config.authentication_algorithm,
                                     c->config.integrity_algorithm,
                                     c->config.confidentiality_algorithm,
                                     c->session.integrity_key_ptr,
                                     c->session.integrity_key_len,
                                     c->session.confidentiality_key_ptr,
                                     c->session.confidentiality_key_len,
                                     buf,
                                     buflen,
                                     tmpl_lan_msg_hdr_rs,
                                     tmpl_unexpected_data) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("ipmi_dump_rmcpplus_packet: %s", strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
    }

  return (0);
}


static int
_ipmi_1_5_packet_assemble (ipmiconsole_ctx_t c,
                           ipmiconsole_packet_type_t p,
                           uint8_t authentication_type,
                           uint32_t inbound_sequence_number,
                           uint32_t session_id,
                           void *authentication_code_data,
                           unsigned int authentication_code_data_len,
                           uint8_t net_fn,
                           fiid_obj_t obj_cmd_rq,
                           void *buf,
                           unsigned int buflen)
{
  int pkt_len;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_REQUEST (p));
  assert (buf);
  assert (buflen);

  if (fiid_obj_clear (c->connection.obj_rmcp_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcp_hdr_rq)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_lan_session_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_lan_session_hdr_rq)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_lan_msg_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_lan_msg_hdr_rq)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (fill_rmcp_hdr_ipmi (c->connection.obj_rmcp_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_rmcp_hdr_ipmi: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (fill_lan_session_hdr (authentication_type,
                            inbound_sequence_number,
                            session_id,
                            c->connection.obj_lan_session_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_lan_session_hdr: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
                        net_fn,
                        IPMI_BMC_IPMB_LUN_BMC,
                        c->session.requester_sequence_number,
                        c->connection.obj_lan_msg_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_lan_msg_hdr: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((pkt_len = assemble_ipmi_lan_pkt (c->connection.obj_rmcp_hdr_rq,
                                        c->connection.obj_lan_session_hdr_rq,
                                        c->connection.obj_lan_msg_hdr_rq,
                                        obj_cmd_rq,
                                        authentication_code_data,
                                        authentication_code_data_len,
                                        buf,
                                        buflen,
					IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("assemble_ipmi_lan_pkt: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (pkt_len);
}

static int
_ipmi_2_0_packet_assemble (ipmiconsole_ctx_t c,
                           ipmiconsole_packet_type_t p,
                           uint8_t payload_type,
                           uint8_t payload_authenticated,
                           uint8_t payload_encrypted,
                           uint32_t session_id,
                           uint32_t session_sequence_number,
                           void *authentication_code_data,
                           unsigned int authentication_code_data_len,
                           uint8_t net_fn,
                           uint8_t authentication_algorithm,
                           uint8_t integrity_algorithm,
                           uint8_t confidentiality_algorithm,
                           void *integrity_key,
                           unsigned int integrity_key_len,
                           void *confidentiality_key,
                           unsigned int confidentiality_key_len,
                           fiid_obj_t obj_cmd_rq,
                           void *buf,
                           unsigned int buflen)
{
  int pkt_len;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_REQUEST (p));
  assert (buf);
  assert (buflen);

  if (fiid_obj_clear (c->connection.obj_rmcp_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcp_hdr_rq)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_lan_msg_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_lan_msg_hdr_rq)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_rmcpplus_session_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcpplus_session_hdr_rq)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_rmcpplus_session_trlr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcpplus_session_trlr_rq)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (fill_rmcp_hdr_ipmi (c->connection.obj_rmcp_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_rmcp_hdr_ipmi: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (fill_rmcpplus_session_hdr (payload_type,
                                 payload_authenticated,
                                 payload_encrypted,
                                 0, /* oem_iana */
                                 0, /* oem_payload_id */
                                 session_id,
                                 session_sequence_number,
                                 c->connection.obj_rmcpplus_session_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_rmcpplus_session_hdr: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
                        net_fn,
                        IPMI_BMC_IPMB_LUN_BMC,
                        c->session.requester_sequence_number,
                        c->connection.obj_lan_msg_hdr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_lan_msg_hdr: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (fill_rmcpplus_session_trlr (c->connection.obj_rmcpplus_session_trlr_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_rmcpplus_session_trlr: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((pkt_len = assemble_ipmi_rmcpplus_pkt (authentication_algorithm,
                                             integrity_algorithm,
                                             confidentiality_algorithm,
                                             integrity_key,
                                             integrity_key_len,
                                             confidentiality_key,
                                             confidentiality_key_len,
                                             authentication_code_data,
                                             authentication_code_data_len,
                                             c->connection.obj_rmcp_hdr_rq,
                                             c->connection.obj_rmcpplus_session_hdr_rq,
                                             c->connection.obj_lan_msg_hdr_rq,
                                             obj_cmd_rq,
                                             c->connection.obj_rmcpplus_session_trlr_rq,
                                             buf,
                                             buflen,
					     IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("assemble_ipmi_rmcpplus_pkt: p = %d; %s", p, strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (pkt_len);
}


int
ipmiconsole_ipmi_packet_assemble (ipmiconsole_ctx_t c,
                                  ipmiconsole_packet_type_t p,
                                  void *buf,
                                  unsigned int buflen)
{
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  char *username = NULL;
  unsigned int username_len;
  char *password = NULL;
  unsigned int password_len;
  uint8_t authentication_type = 0;
  uint32_t session_id = 0;
  uint32_t managed_system_session_id = 0;
  uint32_t session_sequence_number = 0;
  uint8_t net_fn = 0;
  uint8_t payload_type = 0;
  uint8_t authentication_algorithm = 0;
  uint8_t integrity_algorithm = 0;
  void *integrity_key = NULL;
  unsigned int integrity_key_len = 0;
  uint8_t confidentiality_algorithm = 0;
  void *confidentiality_key = NULL;
  unsigned int confidentiality_key_len = 0;
  uint8_t payload_authenticated = 0;
  uint8_t payload_encrypted = 0;
  fiid_obj_t obj_cmd_rq = NULL;
  int pkt_len;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_REQUEST (p));
  assert (p != IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ);
  assert (buf);
  assert (buflen);

  /* Determine Username */
  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations pad their usernames.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION
      && p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1)
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

  /* Determine Password */
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ)
    password = NULL;
  else
    {
      if (strlen (c->config.password))
        password = c->config.password;
      else
        password = NULL;
    }

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

  /* Determine Authentication Type */
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ)
    authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;

  /* Determine Session ID */
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ
      || p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3)
    session_id = 0;
  else
    {
      if (FIID_OBJ_GET (c->connection.obj_open_session_response,
                        "managed_system_session_id",
                        &val) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get: 'managed_system_session_id': %s",
                                     fiid_obj_errormsg (c->connection.obj_open_session_response)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      session_id = val;
    }

  /* Determine Managed System Session ID */
  if (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3)
    {
      if (FIID_OBJ_GET (c->connection.obj_open_session_response,
                        "managed_system_session_id",
                        &val) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get: 'managed_system_session_id': %s",
                                     fiid_obj_errormsg (c->connection.obj_open_session_response)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      managed_system_session_id = val;
    }

  /* Determine Sequence Number */
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ
      || p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3)
    session_sequence_number = 0;
  else
    session_sequence_number = c->session.session_sequence_number;

  /* Determine Network Function */
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ
      || p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ
      || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ
      || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ
      || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ)
    net_fn = IPMI_NET_FN_APP_RQ;

  /* Determine Payload Type */
  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST)
    payload_type = IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST;
  else if (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1)
    payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1;
  else if (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3)
    payload_type = IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3;
  else if (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ
           || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ
           || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ
           || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ
           || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ
           || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ
           || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ)
    payload_type = IPMI_PAYLOAD_TYPE_IPMI;
  else if (p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ)
    payload_type = IPMI_PAYLOAD_TYPE_SOL;

  /* Determine Authentication/Integrity/Confidentiality Keys */
  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3)
    {
      authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
      integrity_algorithm = IPMI_INTEGRITY_ALGORITHM_NONE;
      confidentiality_algorithm = IPMI_CONFIDENTIALITY_ALGORITHM_NONE;
      integrity_key = NULL;
      integrity_key_len = 0;
      confidentiality_key = NULL;
      confidentiality_key_len = 0;
    }
  else
    {
      authentication_algorithm = c->config.authentication_algorithm;
      integrity_algorithm = c->config.integrity_algorithm;
      confidentiality_algorithm = c->config.confidentiality_algorithm;
      integrity_key = c->session.integrity_key_ptr;
      integrity_key_len = c->session.integrity_key_len;
      confidentiality_key = c->session.confidentiality_key_ptr;
      confidentiality_key_len = c->session.confidentiality_key_len;
    }

  /* Determine Payload Authenticated Flag */
  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3
      || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    payload_authenticated = IPMI_PAYLOAD_FLAG_UNAUTHENTICATED;
  else
    payload_authenticated = IPMI_PAYLOAD_FLAG_AUTHENTICATED;

  /* Determine Payload Encrypted Flag */
  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3
      || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    payload_encrypted = IPMI_PAYLOAD_FLAG_UNENCRYPTED;
  else
    payload_encrypted = IPMI_PAYLOAD_FLAG_ENCRYPTED;

  /* Fill/Determine Object */

  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ)
    {
      if (fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                            c->config.privilege_level,
                                                            IPMI_GET_IPMI_V20_EXTENDED_DATA,
                                                            c->connection.obj_authentication_capabilities_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_get_channel_authentication_capabilities: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_authentication_capabilities_rq;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST)
    {
      uint8_t privilege_level;

      /*
       * IPMI Workaround (achu)
       *
       * Forgotten Motherboard
       *
       * Cipher suite IDs are attached to specific privilege levels
       * rather than a maximum privilege level limit.  So you can only
       * authenticate at the configured privilege level rather than a
       * privilege level <= to it.
       *
       * To deal with this situation.  We send the "request highest
       * privilege" flag in the open session request.  This should be
       * enough to work around this issue but still work with other
       * motherboards.
       */

      /* IPMI Workaround (achu)
       *
       * Discovered on SE7520AF2 with Intel Server Management Module
       * (Professional Edition)
       *
       * The Intel's return IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL instead
       * of an actual privilege, so have to pass the actual privilege
       * we want to use.
       */

      /* IPMI Workaround (achu)
       *
       * Discovered on Sun Fire 4100, Inventec 5441/Dell Xanadu II,
       * Supermicro X8DTH, Supermicro X8DTG, Supermicro X8DTU, Intel
       * S5500WBV/Penguin Relion 700
       *
       * The remote BMC incorrectly calculates keys using the privilege
       * specified in the open session stage rather than the privilege
       * used during the RAKP1 stage.  This can be problematic if you
       * specify IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL during that stage
       * instead of a real privilege level.  So we must pass the actual
       * privilege we want to use.
       */
      if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION
          || c->config.workaround_flags & IPMICONSOLE_WORKAROUND_SUN_2_0_SESSION
          || c->config.workaround_flags & IPMICONSOLE_WORKAROUND_OPEN_SESSION_PRIVILEGE)
        privilege_level = c->config.privilege_level;
      else
        privilege_level = IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL;

      if (fill_rmcpplus_open_session (c->session.message_tag,
                                      privilege_level,
                                      c->session.remote_console_session_id,
                                      c->config.authentication_algorithm,
                                      c->config.integrity_algorithm,
                                      c->config.confidentiality_algorithm,
                                      c->connection.obj_open_session_request) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_rmcpplus_open_session: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_open_session_request;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1)
    {
      if (fill_rmcpplus_rakp_message_1 (c->session.message_tag,
                                        managed_system_session_id,
                                        c->session.remote_console_random_number,
                                        IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                        c->config.privilege_level,
                                        c->session.name_only_lookup,
                                        username,
                                        username_len,
                                        c->connection.obj_rakp_message_1) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_rmcpplus_rakp_message_1: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_rakp_message_1;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3)
    {
      uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
      int managed_system_random_number_len;
      uint8_t key_exchange_authentication_code[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
      int key_exchange_authentication_code_len;
      uint8_t name_only_lookup;

      /* IPMI Workaround
       *
       * Intel IPMI 2.0 implementations use this flag incorrectly.
       */
      if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION)
        name_only_lookup = IPMI_USER_NAME_PRIVILEGE_LOOKUP;
      else
        name_only_lookup = c->session.name_only_lookup;

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
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get_data: invalid managed system random number length: %d", managed_system_random_number_len));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }

      if ((key_exchange_authentication_code_len = ipmi_calculate_rakp_3_key_exchange_authentication_code (c->config.authentication_algorithm,
                                                                                                          password,
                                                                                                          password_len,
                                                                                                          managed_system_random_number,
                                                                                                          managed_system_random_number_len,
                                                                                                          c->session.remote_console_session_id,
                                                                                                          name_only_lookup,
                                                                                                          c->config.privilege_level,
                                                                                                          username,
                                                                                                          username_len,
                                                                                                          key_exchange_authentication_code,
                                                                                                          IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("ipmi_calculate_rakp_3_key_exchange_authentication_code: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }

      if (fill_rmcpplus_rakp_message_3 (c->session.message_tag,
                                        RMCPPLUS_STATUS_NO_ERRORS,
                                        managed_system_session_id,
                                        key_exchange_authentication_code,
                                        key_exchange_authentication_code_len,
                                        c->connection.obj_rakp_message_3) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_rmcpplus_rakp_message_3: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_rakp_message_3;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ)
    {
      if (fill_cmd_set_session_privilege_level (c->config.privilege_level,
                                                c->connection.obj_set_session_privilege_level_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_set_session_privilege_level: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_set_session_privilege_level_rq;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ)
    {
      if (fill_cmd_get_channel_payload_support (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                c->connection.obj_get_channel_payload_support_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_get_channel_payload_support: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_get_channel_payload_support_rq;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ)
    {
      if (fill_cmd_get_payload_activation_status (IPMI_PAYLOAD_TYPE_SOL,
                                                  c->connection.obj_get_payload_activation_status_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_get_payload_activation_status: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_get_payload_activation_status_rq;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ)
    {
      uint8_t sol_startup_handshake;
      uint8_t authentication_activation;
      uint8_t encryption_activation;
      uint8_t shared_serial_alert_behavior;

      /* IPMI Workaround
       *
       * Intel IPMI 2.0 specific issue.  I have no idea why this is.
       * I took it from ipmiutil.  Ipmiutil author Andrew Cress cannot
       * remember why this is the case either.
       */
      if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION)
        sol_startup_handshake = IPMI_SOL_STARTUP_HANDSHAKE_CTS_AND_DCD_SDR_ASSERTED;
      else
        sol_startup_handshake = IPMI_SOL_STARTUP_HANDSHAKE_CTS_AND_DCD_SDR_DEASSERTED;

      if (c->config.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_activation = IPMI_ACTIVATE_PAYLOAD_WITHOUT_AUTHENTICATION;
      else
        authentication_activation = IPMI_ACTIVATE_PAYLOAD_WITH_AUTHENTICATION;

      if (c->config.confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        encryption_activation = IPMI_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION;
      else
        encryption_activation = IPMI_ACTIVATE_PAYLOAD_WITH_ENCRYPTION;

      /* Workaround
       *
       * Discovered on Intel Windmill/Quanta Winterfell/Wiwynn Windmill
       *
       * Nodes like this to be set, I have no idea why.  The boards
       * don't even have serial ports.
       */
      if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_SERIAL_ALERTS_DEFERRED)
	shared_serial_alert_behavior = IPMI_SERIAL_MODEM_ALERTS_DEFERRED_WHILE_SOL_ACTIVE;
      else
	shared_serial_alert_behavior = IPMI_SERIAL_MODEM_ALERTS_FAIL_WHILE_SOL_ACTIVE;

      if (fill_cmd_activate_payload_sol (IPMI_PAYLOAD_TYPE_SOL,
                                         (uint8_t)c->config.sol_payload_instance,
                                         sol_startup_handshake,
                                         shared_serial_alert_behavior,
                                         IPMI_TEST_MODE_DEACTIVATED,
                                         authentication_activation,
                                         encryption_activation,
                                         c->connection.obj_activate_payload_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_activate_payload_sol: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_activate_payload_rq;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ)
    {
      assert (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);

      if (fill_cmd_get_channel_payload_version (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                IPMI_PAYLOAD_TYPE_SOL,
                                                c->connection.obj_get_channel_payload_version_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_get_channel_payload_version: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_get_channel_payload_version_rq;
    }
  else if (p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ)
    {
      uint8_t payload_instance;

      if (c->session.deactivate_payload_instances)
        payload_instance = c->session.sol_instances_activated[c->session.sol_instances_deactivated_count];
      else
        payload_instance = (uint8_t)c->config.sol_payload_instance;

      if (fill_cmd_deactivate_payload (IPMI_PAYLOAD_TYPE_SOL,
                                       payload_instance,
                                       0, /* achu: IPMI SPEC says: 0h for SOL */
                                       c->connection.obj_deactivate_payload_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_deactivate_payload: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_deactivate_payload_rq;
    }
  else /* p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ */
    {
      if (fill_cmd_close_session (session_id,
                                  NULL,
                                  c->connection.obj_close_session_rq) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fill_cmd_close_session: p = %d; %s", p, strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      obj_cmd_rq = c->connection.obj_close_session_rq;
    }

  /* IPMI 1.5 Style Packets */
  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ)
    {
      if ((pkt_len = _ipmi_1_5_packet_assemble (c,
                                                p,
                                                authentication_type,
                                                session_sequence_number,
                                                session_id,
                                                password,
                                                password_len,
                                                net_fn,
                                                obj_cmd_rq,
                                                buf,
                                                buflen)) < 0)
        return (-1);
    }
  else /* IPMI 2.0 Style Packets */
    {
      if ((pkt_len = _ipmi_2_0_packet_assemble (c,
                                                p,
                                                payload_type,
                                                payload_authenticated,
                                                payload_encrypted,
                                                session_id,
                                                session_sequence_number,
                                                password,
                                                password_len,
                                                net_fn,
                                                authentication_algorithm,
                                                integrity_algorithm,
                                                confidentiality_algorithm,
                                                integrity_key,
                                                integrity_key_len,
                                                confidentiality_key,
                                                confidentiality_key_len,
                                                obj_cmd_rq,
                                                buf,
                                                buflen)) < 0)
        return (-1);
    }

  return (pkt_len);
}

int
ipmiconsole_sol_packet_assemble (ipmiconsole_ctx_t c,
                                 uint8_t packet_sequence_number,
                                 uint8_t packet_ack_nack_sequence_number,
                                 uint8_t accepted_character_count,
                                 uint8_t generate_break,
                                 void *character_data,
                                 unsigned int character_data_len,
                                 void *buf,
                                 unsigned int buflen)
{
  char *password = NULL;
  uint32_t session_id = 0;
  uint8_t payload_authenticated = 0;
  uint8_t payload_encrypted = 0;
  uint8_t ack;
  int pkt_len;
  uint64_t val;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);
  assert (character_data_len <= c->session.max_sol_character_send_size);

  /* If the packet sequence number is 0, that means this is an ACK
   * only packet.  So the ack_nack sequence number should be non-zero
   * and no character data should be sent.  The accepted character
   * count can be 0 to indicate a NACK instead of an ACK.
   */
  assert (packet_sequence_number
          || (!packet_sequence_number
              && packet_ack_nack_sequence_number
              && !character_data_len));
  assert (buf);
  assert (buflen);

  if (strlen (c->config.password))
    password = c->config.password;
  else
    password = NULL;

  if (FIID_OBJ_GET (c->connection.obj_open_session_response,
                    "managed_system_session_id",
                    &val) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get: 'managed_system_session_id': %s",
                                 fiid_obj_errormsg (c->connection.obj_open_session_response)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }
  session_id = val;

  /* Determine Payload Authenticated Flag */
  if (c->config.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    payload_authenticated = IPMI_PAYLOAD_FLAG_UNAUTHENTICATED;
  else
    payload_authenticated = IPMI_PAYLOAD_FLAG_AUTHENTICATED;

  /* Determine Payload Encrypted Flag */
  if (c->config.confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    payload_encrypted = IPMI_PAYLOAD_FLAG_UNENCRYPTED;
  else
    payload_encrypted = IPMI_PAYLOAD_FLAG_ENCRYPTED;

  /* Determine ACK flag */
  if (!packet_sequence_number)
    {
      if (accepted_character_count)
        ack = IPMI_SOL_ACK;
      else
        ack = IPMI_SOL_NACK;
    }
  else
    /* XXX: Hopefully this is right. Specification is unclear */
    ack = 0;

  /* Fill/Determine Object */
  if (fill_sol_payload_data_remote_console_to_bmc (packet_sequence_number,
                                                   packet_ack_nack_sequence_number,
                                                   accepted_character_count,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   generate_break,
                                                   0,
                                                   ack,
                                                   character_data,
                                                   character_data_len,
                                                   c->connection.obj_sol_payload_data_rq) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fill_sol_payload_data_remote_console_to_bmc: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if ((pkt_len = _ipmi_2_0_packet_assemble (c,
                                            IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ,
                                            IPMI_PAYLOAD_TYPE_SOL,
                                            payload_authenticated,
                                            payload_encrypted,
                                            session_id,
                                            c->session.session_sequence_number,
                                            password,
                                            (password) ? strlen (password) : 0,
                                            0,
                                            c->config.authentication_algorithm,
                                            c->config.integrity_algorithm,
                                            c->config.confidentiality_algorithm,
                                            c->session.integrity_key_ptr,
                                            c->session.integrity_key_len,
                                            c->session.confidentiality_key_ptr,
                                            c->session.confidentiality_key_len,
                                            c->connection.obj_sol_payload_data_rq,
                                            buf,
                                            buflen)) < 0)
    goto cleanup;

  rv = pkt_len;
 cleanup:
  /* Clear out data */
  fiid_obj_clear (c->connection.obj_sol_payload_data_rq);
  return (rv);
}


int
ipmiconsole_packet_unassemble (ipmiconsole_ctx_t c,
                               ipmiconsole_packet_type_t *p,
                               const void *buf,
                               unsigned int buflen)
{
  ipmiconsole_packet_type_t pkt;
  fiid_obj_t obj_cmd = NULL;
  int ret, pkt_ret = 0;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (p);
  assert (buf);
  assert (buflen);

  if (fiid_obj_clear (c->connection.obj_rmcp_hdr_rs) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcp_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_lan_session_hdr_rs) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_lan_session_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_lan_msg_hdr_rs) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_lan_msg_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_lan_msg_trlr_rs) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_lan_msg_trlr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_rmcpplus_session_hdr_rs) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcpplus_session_hdr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_rmcpplus_payload_rs) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcpplus_payload_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }
  if (fiid_obj_clear (c->connection.obj_rmcpplus_session_trlr_rs) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (c->connection.obj_rmcpplus_session_trlr_rs)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  /* Calculate packet type */

  if ((ret = ipmi_is_ipmi_1_5_packet (buf, buflen)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_is_ipmi_1_5_packet: %s", strerror (errno)));
      /* Assume malformed packet */
      pkt_ret = 0;
      goto out;
    }

  if (ret)
    {
      if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_SENT)
        pkt = IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS;
      else
        {
          /* Unexpected packet, throw it away */
          IPMICONSOLE_CTX_DEBUG (c, ("Unexpected IPMI 1.5 Packet: protocol_state = %d", c->session.protocol_state));
          pkt_ret = 0;
          goto out;
        }

      obj_cmd =  ipmiconsole_packet_object (c, pkt);
      if (fiid_obj_clear (obj_cmd) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (obj_cmd)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }

      if ((pkt_ret = unassemble_ipmi_lan_pkt (buf,
                                              buflen,
                                              c->connection.obj_rmcp_hdr_rs,
                                              c->connection.obj_lan_session_hdr_rs,
                                              c->connection.obj_lan_msg_hdr_rs,
                                              obj_cmd,
                                              c->connection.obj_lan_msg_trlr_rs,
					      IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("unassemble_ipmi_lan_pkt: %s", strerror (errno)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      *p = pkt;
    }
  else
    {
      uint8_t payload_type;

      if (ipmi_rmcpplus_calculate_payload_type (buf, buflen, &payload_type) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("ipmi_rmcpplus_calculate_payload_type: %s", strerror (errno)));
	  /* Assume malformed packet */
	  pkt_ret = 0;
	  goto out;
        }

      if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
          || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
          || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
        {
          if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT)
            pkt = IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE;
          else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
            pkt = IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2;
          else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
            pkt = IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4;
          else
            {
              IPMICONSOLE_CTX_DEBUG (c, ("Unexpected IPMI 2.0 Session Setup Packet: protocol_state = %d", c->session.protocol_state));
              pkt_ret = 0;
              goto out;
            }

          obj_cmd =  ipmiconsole_packet_object (c, pkt);
          if (fiid_obj_clear (obj_cmd) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (obj_cmd)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }

          /* IPMI 2.0 Pre-Session Establishment Packets */
          if ((pkt_ret = unassemble_ipmi_rmcpplus_pkt (IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE,
                                                       IPMI_INTEGRITY_ALGORITHM_NONE,
                                                       IPMI_CONFIDENTIALITY_ALGORITHM_NONE,
                                                       NULL,
                                                       0,
                                                       NULL,
                                                       0,
                                                       buf,
                                                       buflen,
                                                       c->connection.obj_rmcp_hdr_rs,
                                                       c->connection.obj_rmcpplus_session_hdr_rs,
                                                       c->connection.obj_rmcpplus_payload_rs,
                                                       c->connection.obj_lan_msg_hdr_rs,
                                                       obj_cmd,
                                                       c->connection.obj_lan_msg_trlr_rs,
                                                       c->connection.obj_rmcpplus_session_trlr_rs,
						       IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("unassemble_ipmi_rmcpplus_pkt: %s", strerror (errno)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }
          *p = pkt;
        }
      else if (payload_type == IPMI_PAYLOAD_TYPE_IPMI
               || payload_type == IPMI_PAYLOAD_TYPE_SOL)
        {
          if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
            {
              if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT)
                pkt = IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS;
              else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_CHANNEL_PAYLOAD_SUPPORT_SENT)
                pkt = IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS;
              else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT)
                pkt = IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS;
              else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT)
                pkt = IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS;
              else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION)
                pkt = IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS;
              else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT)
                pkt = IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS;
              else if (c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT)
                pkt = IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS;
              else
                {
                  IPMICONSOLE_CTX_DEBUG (c, ("Unexpected IPMI 2.0 IPMI Packet: protocol_state = %d", c->session.protocol_state));
                  pkt_ret = 0;
                  goto out;
                }
            }
          else
            {
              if (c->session.protocol_state != IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION)
                {
                  IPMICONSOLE_CTX_DEBUG (c, ("Unexpected IPMI 2.0 SOL Packet: protocol_state = %d", c->session.protocol_state));
                  pkt_ret = 0;
                  goto out;
                }
              pkt = IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS;
            }

          obj_cmd =  ipmiconsole_packet_object (c, pkt);
          if (fiid_obj_clear (obj_cmd) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_clear: %s", fiid_obj_errormsg (obj_cmd)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }

          /* IPMI 2.0 Session Packets */
          if ((pkt_ret = unassemble_ipmi_rmcpplus_pkt (c->config.authentication_algorithm,
                                                       c->config.integrity_algorithm,
                                                       c->config.confidentiality_algorithm,
                                                       c->session.integrity_key_ptr,
                                                       c->session.integrity_key_len,
                                                       c->session.confidentiality_key_ptr,
                                                       c->session.confidentiality_key_len,
                                                       buf,
                                                       buflen,
                                                       c->connection.obj_rmcp_hdr_rs,
                                                       c->connection.obj_rmcpplus_session_hdr_rs,
                                                       c->connection.obj_rmcpplus_payload_rs,
                                                       c->connection.obj_lan_msg_hdr_rs,
                                                       obj_cmd,
                                                       c->connection.obj_lan_msg_trlr_rs,
                                                       c->connection.obj_rmcpplus_session_trlr_rs,
						       IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("unassemble_ipmi_rmcpplus_pkt: %s", strerror (errno)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }
          *p = pkt;
        }
      else
        {
          IPMICONSOLE_CTX_DEBUG (c, ("Unexpected payload_type: payload_type = %u", payload_type));
          pkt_ret = 0;
          goto out;
        }
    }

 out:
  return (pkt_ret);
}

int
ipmiconsole_calculate_errnum (ipmiconsole_ctx_t c,
                              ipmiconsole_packet_type_t p)
{
  fiid_obj_t obj_cmd;
  uint64_t val;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (IPMICONSOLE_PACKET_TYPE_RESPONSE (p));

  if (!(obj_cmd = ipmiconsole_packet_object (c, p)))
    return (-1);

  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4)
    {
      uint8_t rmcpplus_status_code;

      if (FIID_OBJ_GET (obj_cmd, "rmcpplus_status_code", &val) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get: 'rmcpplus_status_code': %s",
                                     fiid_obj_errormsg (obj_cmd)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      rmcpplus_status_code = val;

      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("called with valid success code; p = %d", p));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION
               || rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_BMC_BUSY);
          return (0);
        }
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_UNAUTHORIZED_ROLE_OR_PRIVILEGE_LEVEL_REQUESTED
	       || rmcpplus_status_code == RMCPPLUS_STATUS_INVALID_ROLE)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
          return (0);
        }
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_UNAUTHORIZED_NAME)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_USERNAME_INVALID);
          return (0);
        }
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_CIPHER_SUITE_MATCH_WITH_PROPOSED_SECURITY_ALGORITHMS)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_CIPHER_SUITE_ID_UNAVAILABLE);
          return (0);
        }
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INVALID_INTEGRITY_CHECK_VALUE)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_PASSWORD_INVALID);
          return (0);
        }
    }
  else
    {
      uint8_t comp_code;

      if (FIID_OBJ_GET (obj_cmd, "comp_code", &val) < 0)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("fiid_obj_get: 'comp_code': %s",
                                     fiid_obj_errormsg (obj_cmd)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }
      comp_code = val;

      if (comp_code == IPMI_COMP_CODE_COMMAND_SUCCESS)
        {
          IPMICONSOLE_CTX_DEBUG (c, ("called with valid success code; p = %d", p));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          return (-1);
        }

      if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS
          && comp_code == IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_IPMI_2_0_UNAVAILABLE);
          return (0);
        }
      else if (p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
               && (comp_code == IPMI_COMP_CODE_ACTIVATE_PAYLOAD_PAYLOAD_ALREADY_ACTIVE_ON_ANOTHER_SESSION
                   || comp_code == IPMI_COMP_CODE_ACTIVATE_PAYLOAD_PAYLOAD_ACTIVATION_LIMIT_REACHED))
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SOL_INUSE);
          return (0);
        }
      else if (p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
               && comp_code == IPMI_COMP_CODE_ACTIVATE_PAYLOAD_CANNOT_ACTIVATE_PAYLOAD_WITH_ENCRYPTION)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SOL_REQUIRES_NO_ENCRYPTION);
          return (0);
        }
      /* Workaround
       *
       * Inventec 5441/Dell Xanadu II returns
       * IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE, so we'll assume that
       * return code always means we need encryption.
       */
      else if (p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
               && (comp_code == IPMI_COMP_CODE_ACTIVATE_PAYLOAD_CANNOT_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION
                   || comp_code == IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE))
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SOL_REQUIRES_ENCRYPTION);
          return (0);
        }
      else if (p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS
               && comp_code == IPMI_COMP_CODE_ACTIVATE_PAYLOAD_PAYLOAD_TYPE_IS_DISABLED)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SOL_UNAVAILABLE);
          return (0);
        }
      else if (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
               && (comp_code == IPMI_COMP_CODE_SET_SESSION_PRIVILEGE_LEVEL_REQUESTED_LEVEL_NOT_AVAILABLE_FOR_USER
                   || comp_code == IPMI_COMP_CODE_SET_SESSION_PRIVILEGE_LEVEL_REQUESTED_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT))
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
          return (0);
        }

      if (comp_code == IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL)
        {
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_PRIVILEGE_LEVEL_INSUFFICIENT);
          return (0);
        }

    }

  ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_BMC_ERROR);
  return (0);
}
