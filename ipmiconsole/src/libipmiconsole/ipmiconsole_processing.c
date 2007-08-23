/*****************************************************************************\
 *  $Id: ipmiconsole_processing.c,v 1.38 2007-08-23 17:34:57 chu11 Exp $
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
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "cbuf.h"
#include "list.h"
#include "secure.h"
#include "timeval.h"
#include "ipmiconsole_processing.h"
#include "ipmiconsole_checks.h"
#include "ipmiconsole_debug.h"
#include "ipmiconsole_engine.h"
#include "ipmiconsole_fiid_wrappers.h"
#include "ipmiconsole_packet.h"

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_send_ipmi_packet(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t pkt[IPMICONSOLE_PACKET_BUFLEN];
  int n, pkt_len, dropped = 0;
  struct timeval *t;
  int secure_malloc_flag;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(IPMICONSOLE_PACKET_TYPE_REQUEST(p));

  s = &(c->session);

  secure_malloc_flag = (c->config.security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY) ? 1 : 0;

  if (p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ
      || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ
      || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ
      || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ)
    {
      s->requester_sequence_number++;
      if (s->requester_sequence_number > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
        s->requester_sequence_number = 0;
    }
  
  /* Note: Message Tags are 1 byte, so wraparounds are automatic */
  if (p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1
      || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3)
    s->message_tag++;
  
  if (p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ
      || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ
      || p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ
      || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ
      || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ)
    {
      /* Sequence Numbers of 0 are considered special by IPMI 2.0 */
      s->session_sequence_number++;
      if (!(s->session_sequence_number))
	s->session_sequence_number++;
    }

  if ((pkt_len = ipmiconsole_ipmi_packet_assemble(c, p, pkt, IPMICONSOLE_PACKET_BUFLEN)) < 0)
    return -1;

  if (c->config.debug_flags & IPMICONSOLE_DEBUG_IPMI_PACKETS)
    {
      if (ipmiconsole_packet_dump(c, p, pkt, pkt_len) < 0)
        return -1;
    }
  
  if ((n = cbuf_write(s->ipmi_to_bmc, pkt, pkt_len, &dropped, secure_malloc_flag)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (n != pkt_len)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: invalid bytes written; n=%d; pkt_len=%d", n, pkt_len));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (dropped)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: dropped data: dropped=%d", dropped));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  if (p != IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ)
    t = &(s->last_ipmi_packet_sent);
  else
    t = &(s->last_keepalive_packet_sent);
  
  if (gettimeofday(t, NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  return 0;
}

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_send_sol_packet_with_character_data(ipmiconsole_ctx_t c, 
                                     uint8_t packet_ack_nack_sequence_number,
                                     uint8_t accepted_character_count,
                                     int is_retransmission)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t pkt[IPMICONSOLE_PACKET_BUFLEN];
  int n, pkt_len, dropped = 0;
  int rv = -1;
  int secure_malloc_flag;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);
  assert(!cbuf_is_empty(c->session.console_remote_console_to_bmc));

  s = &(c->session);
  
  secure_malloc_flag = (c->config.security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY) ? 1 : 0;

  /* 
   * Notes: The IPMI session sequence number should be incremented.  Since
   * the BMC accepts packets in a window. But the SOL packet sequence number
   * should not be incremented on a retransmission.
   */

  /* Sequence Numbers of 0 are considered special by IPMI 2.0 */
  s->session_sequence_number++;
  if (!(s->session_sequence_number))
    s->session_sequence_number++;

  if (!is_retransmission)
    {
      int max_character_send_size;

      s->sol_input_packet_sequence_number++;
      if (s->sol_input_packet_sequence_number > IPMI_SOL_PACKET_SEQUENCE_NUMBER_MAX)
        /* Sequence number 0 is special, so start at 1 */
        s->sol_input_packet_sequence_number = 1;

      /*  
       * If a serial break has occurred, we want to generate the break
       * after all input before the break has been dealt with and none
       * after the break have been sent.
       */
      if (!s->console_remote_console_to_bmc_bytes_before_break)
        max_character_send_size = s->max_sol_character_send_size;
      else
        max_character_send_size = s->console_remote_console_to_bmc_bytes_before_break;

      /* Notes: Since s->console_remote_console_to_bmc is a circular buffer, it may
       * not be apparent why 's->sol_input_character_data' and
       * 's->sol_input_character_data_len' is necessary.
       *
       * The reason it is needed is because the user may have typed
       * additional info, thus increasing the amount of data in
       * 's->console_remote_console_to_bmc'.  We need to assure that if there is
       * an SOL retransmission, the character data it is sending is
       * perfectly identical.
       */
      if ((n = cbuf_peek(s->console_remote_console_to_bmc, 
                         s->sol_input_character_data,
                         max_character_send_size)) < 0)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("cbuf_read: %s", strerror(errno)));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      s->sol_input_character_data_len = n;
    }

  if ((pkt_len = ipmiconsole_sol_packet_assemble(c, 
                                                  s->sol_input_packet_sequence_number,
                                                  packet_ack_nack_sequence_number,
                                                  accepted_character_count,
                                                  0,
                                                  s->sol_input_character_data,
                                                  s->sol_input_character_data_len,
                                                  pkt, 
                                                  IPMICONSOLE_PACKET_BUFLEN)) < 0)
    goto cleanup;
  
  if (c->config.debug_flags & IPMICONSOLE_DEBUG_IPMI_PACKETS)
    {
      if (ipmiconsole_packet_dump(c, IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ, pkt, pkt_len) < 0)
        goto cleanup;
    }
  
  if ((n = cbuf_write(s->ipmi_to_bmc, pkt, pkt_len, &dropped, secure_malloc_flag)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (n != pkt_len)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: invalid bytes written; n=%d; pkt_len=%d", n, pkt_len));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (dropped)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: dropped data: dropped=%d", dropped));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
  
  if (gettimeofday(&(s->last_sol_input_packet_sent), NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
    
  s->sol_input_waiting_for_ack++;
  rv = 0;
cleanup:
  /* Clear out data */
  secure_memset(pkt, '\0', IPMICONSOLE_PACKET_BUFLEN);
  return rv;
}

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_send_sol_packet_ack_only(ipmiconsole_ctx_t c, 
                          uint8_t packet_ack_nack_sequence_number,
                          uint8_t accepted_character_count)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t pkt[IPMICONSOLE_PACKET_BUFLEN];
  int n, pkt_len, dropped = 0;
  int secure_malloc_flag;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);
  
  s = &(c->session);
  
  secure_malloc_flag = (c->config.security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY) ? 1 : 0;

  /* 
   * Notes: The IPMI session sequence number should be incremented.  Since
   * the BMC accepts packets in a window. But the SOL packet sequence number
   * should not be incremented on a retransmission.
   */

  /* Sequence Numbers of 0 are considered special by IPMI 2.0 */
  s->session_sequence_number++;
  if (!(s->session_sequence_number))
    s->session_sequence_number++;

  if ((pkt_len = ipmiconsole_sol_packet_assemble(c, 
                                                  0,
                                                  packet_ack_nack_sequence_number,
                                                  accepted_character_count,
                                                  0,
                                                  NULL,
                                                  0,
                                                  pkt, 
                                                  IPMICONSOLE_PACKET_BUFLEN)) < 0)
    return -1;
  
  if (c->config.debug_flags & IPMICONSOLE_DEBUG_IPMI_PACKETS)
    {
      if (ipmiconsole_packet_dump(c, IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ, pkt, pkt_len) < 0)
        return -1;
    }
  
  if ((n = cbuf_write(s->ipmi_to_bmc, pkt, pkt_len, &dropped, secure_malloc_flag)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (n != pkt_len)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: invalid bytes written; n=%d; pkt_len=%d", n, pkt_len));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (dropped)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: dropped data: dropped=%d", dropped));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  return 0;
}

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_send_sol_packet_generate_break(ipmiconsole_ctx_t c, 
                                int is_retransmission)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t pkt[IPMICONSOLE_PACKET_BUFLEN];
  int n, pkt_len, dropped = 0;
  int secure_malloc_flag;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);
  
  s = &(c->session);
  
  secure_malloc_flag = (c->config.security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY) ? 1 : 0;

  /* 
   * Notes: The IPMI session sequence number should be incremented.  Since
   * the BMC accepts packets in a window. But the SOL packet sequence number
   * should not be incremented on a retransmission.
   */

  /* Sequence Numbers of 0 are considered special by IPMI 2.0 */
  s->session_sequence_number++;
  if (!(s->session_sequence_number))
    s->session_sequence_number++;

  if (!is_retransmission)
    {
      s->sol_input_packet_sequence_number++;
      if (s->sol_input_packet_sequence_number > IPMI_SOL_PACKET_SEQUENCE_NUMBER_MAX)
        /* Sequence number 0 is special, so start at 1 */
        s->sol_input_packet_sequence_number = 1;

      /* achu: For the time being we will not send data along with a break */
      s->sol_input_character_data_len = 0;
    }
  
  if ((pkt_len = ipmiconsole_sol_packet_assemble(c, 
                                                  s->sol_input_packet_sequence_number,
						  0,
						  0,
                                                  IPMI_SOL_GENERATE_BREAK,
                                                  s->sol_input_character_data,
                                                  s->sol_input_character_data_len,
                                                  pkt, 
                                                  IPMICONSOLE_PACKET_BUFLEN)) < 0)
    return -1;
  
  if (c->config.debug_flags & IPMICONSOLE_DEBUG_IPMI_PACKETS)
    {
      if (ipmiconsole_packet_dump(c, IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ, pkt, pkt_len) < 0)
        return -1;
    }
  
  if ((n = cbuf_write(s->ipmi_to_bmc, pkt, pkt_len, &dropped, secure_malloc_flag)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (n != pkt_len)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: invalid bytes written; n=%d; pkt_len=%d", n, pkt_len));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (dropped)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: dropped data: dropped=%d", dropped));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  if (gettimeofday(&(s->last_sol_input_packet_sent), NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  s->sol_input_waiting_for_ack++;
  s->sol_input_waiting_for_break_ack++;
  return 0;
}

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_receive_packet_data_reset(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);

  s = &(c->session);
  
  s->retransmission_count = 0;
  s->errors_count = 0;
  s->session_sequence_number_errors_count = 0;
  if (gettimeofday(&(s->last_ipmi_packet_received), NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  return 0;
}

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_receive_ping_packet_data_reset(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);

  s = &(c->session);
  
  if (gettimeofday(&(s->last_ipmi_packet_received), NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  return 0;
}

/* 
 * Returns 1 if packet received
 * Returns 0 if no packet received (whether real or dropped due to packet errors)
 * Returns -1 on error (whether system error or IPMI errors)
 */
static int
_receive_packet(ipmiconsole_ctx_t c, ipmiconsole_packet_type_t *p)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t pkt[IPMICONSOLE_PACKET_BUFLEN];
  int ret, pkt_len, dropped, rv = -1;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(p);

  s = &(c->session);

  if ((pkt_len = cbuf_peek(s->ipmi_from_bmc, pkt, IPMICONSOLE_PACKET_BUFLEN)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_peek: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (!pkt_len)
    return 0;

  if ((dropped = cbuf_drop(s->ipmi_from_bmc, pkt_len)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_drop: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (dropped != pkt_len)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("cbuf_drop: invalid bytes dropped: dropped=%d pkt_len=%d", dropped, pkt_len));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (ipmiconsole_packet_unassemble(c, p, pkt, pkt_len) < 0)
    {
      /* Assume it's an error if the packet is unparseable, but don't
       * exit 
       */
      if (c->config.debug_flags & IPMICONSOLE_DEBUG_IPMI_PACKETS)
        ipmiconsole_packet_dump_unknown(c, pkt, pkt_len);
      s->errors_count++;
      rv = 0;
      goto cleanup;
    }
  
  assert(IPMICONSOLE_PACKET_TYPE_RESPONSE(*p));

  if (c->config.debug_flags & IPMICONSOLE_DEBUG_IPMI_PACKETS)
    /* It's debugging, who cares if the call succeeds or fails */
    ipmiconsole_packet_dump(c, *p, pkt, pkt_len);

  if (*p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS)
    {
      /* Notes: These checks are for IPMI 1.5 pre-session packets, so
       * there is no authentication code, session sequence number, or
       * session id to check.
       */
      
      if ((ret = ipmiconsole_check_checksum(c, *p)) < 0)
	goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      if ((ret = ipmiconsole_check_network_function(c, *p)) < 0)
	goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_command(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_requester_sequence_number(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      /* If everything passes except the completion code, then there
       * is an IPMI error.
       */
      if ((ret = ipmiconsole_check_completion_code(c, *p)) < 0)
        goto cleanup;

      if (!ret)
        {
          _receive_packet_data_reset(c);
          ipmiconsole_calculate_errnum(c, *p);
          goto cleanup;
        }
    }
  else if (*p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE
           || *p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2
           || *p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4)
    {
      /* Notes: These are IPMI 2.0 session-setup packets, so 
       * a number of typical checks are not required. 
       */

      /* This check isn't really necessary due to the way the
       * unassemble function has been programmed
       */
      if ((ret = ipmiconsole_check_payload_type(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_message_tag(c, *p)) < 0)
        goto cleanup;
      
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      /* XXX: I don't think there is a guarantee the
       * other data (i.e. authentication keys, session id's, etc.)
       * will be valid in the responses if the status code 
       * contains an error.  So unlike the IPMI 1.5 pre-session
       * or IPMI 2.0 session checks, we check the status
       * code prior to checking remaining data
       */

      if ((ret = ipmiconsole_check_rmcpplus_status_code(c, *p)) < 0)
        goto cleanup;

      if (!ret)
        {
          _receive_packet_data_reset(c);
          ipmiconsole_calculate_errnum(c, *p);
          goto cleanup;
        }
      
      if ((ret = ipmiconsole_check_session_id(c, *p)) < 0)
        goto cleanup;
      
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      if (*p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE)
        {
          if ((ret = ipmiconsole_check_open_session_response_privilege(c, *p)) < 0)
            goto cleanup;

          if (!ret)
            {
              _receive_packet_data_reset(c);
              c->errnum = IPMICONSOLE_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED;
              goto cleanup;
            }
        }
      else if (*p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2)
        {
          if ((ret = ipmiconsole_check_rakp_2_key_exchange_authentication_code(c, *p)) < 0)
            goto cleanup;

          if (!ret)
            {
              _receive_packet_data_reset(c);
              c->errnum = IPMICONSOLE_ERR_PASSWORD_INVALID;
              goto cleanup;

            }
        }
      else if (*p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4)
        {
          if ((ret = ipmiconsole_check_rakp_4_integrity_check_value(c, *p)) < 0)
            goto cleanup;
          
          if (!ret)
            {
              _receive_packet_data_reset(c);
              c->errnum = IPMICONSOLE_ERR_K_G_INVALID;
              goto cleanup;
            }
        }
    }
  else if (*p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS
	   || *p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS
           || *p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS
           || *p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS)
    {
      /* This check isn't really necessary due to the way the
       * unassemble function has been programmed
       */
      if ((ret = ipmiconsole_check_payload_type(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_payload_pad(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_integrity_pad(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      if ((ret = ipmiconsole_check_checksum(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      if ((ret = ipmiconsole_check_authentication_code(c, *p, pkt, pkt_len)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_outbound_sequence_number(c, *p)) < 0)
        goto cleanup;
      
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_session_id(c, *p)) < 0)
        goto cleanup;
      
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_network_function(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_command(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_requester_sequence_number(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      /* If everything passes except the completion code, then there
       * is an IPMI error.
       */
      if ((ret = ipmiconsole_check_completion_code(c, *p)) < 0)
        goto cleanup;

      if (!ret)
        {
	  if (*p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS)
	    {
	      /* Special case: we have raced with another IPMI session.
	       *
	       * See comments in _process_ctx().
	       */
	      uint8_t comp_code;
	      uint64_t val;

	      if (Fiid_obj_get(c,
			       s->obj_activate_payload_rs,
			       "comp_code",
			       &val) < 0)
		goto cleanup;
	      comp_code = val;
	      
	      if (comp_code == IPMI_COMP_CODE_PAYLOAD_ALREADY_ACTIVE_ON_ANOTHER_SESSION
                  || comp_code == IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED
		  || comp_code == IPMI_COMP_CODE_PAYLOAD_ACTIVATION_LIMIT_REACHED
                  || comp_code == IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITH_ENCRYPTION
                  || comp_code == IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION)
		goto accept_packet;
	    }
	  
          _receive_packet_data_reset(c);
          ipmiconsole_calculate_errnum(c, *p);
          goto cleanup;
        }
    }
  else if (*p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS)
    {
      /* This check isn't really necessary due to the way the
       * unassemble function has been programmed
       */
      if ((ret = ipmiconsole_check_payload_type(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_payload_pad(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_integrity_pad(c, *p)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      if ((ret = ipmiconsole_check_authentication_code(c, *p, pkt, pkt_len)) < 0)
        goto cleanup;

      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_outbound_sequence_number(c, *p)) < 0)
        goto cleanup;
      
      if (!ret)
	{
	  s->errors_count++;
	  s->session_sequence_number_errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_session_id(c, *p)) < 0)
        goto cleanup;
      
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
    }
  else if (*p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS)
    {
      /* We do not care about the validity of most of this packet.  If
       * we get it, it means we kept the session alive.  We will still
       * call the check functions so debugging will be logged
       * appropriately and outbound sequence number checks can move
       * the window along appropriately.
       */

      if ((ret = ipmiconsole_check_payload_type(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_payload_pad(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_integrity_pad(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_checksum(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_authentication_code(c, *p, pkt, pkt_len)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_outbound_sequence_number(c, *p)) < 0)
        goto cleanup;
      
      if ((ret = ipmiconsole_check_session_id(c, *p)) < 0)
        goto cleanup;
      
      /* This is one check we do care about */
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_network_function(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_command(c, *p)) < 0)
        goto cleanup;

      /* This is one check we do care about */
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_requester_sequence_number(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_completion_code(c, *p)) < 0)
        goto cleanup;
    }
  else if (*p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS)
    {
      /* Generally, we *do* care about the validity of this packet, 
       * with the exception of when we are closing the session.
       */

      /* This check isn't really necessary due to the way the
       * unassemble function has been programmed
       */
      if ((ret = ipmiconsole_check_payload_type(c, *p)) < 0)
        goto cleanup;

      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_payload_pad(c, *p)) < 0)
        goto cleanup;

      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_integrity_pad(c, *p)) < 0)
        goto cleanup;

      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      if ((ret = ipmiconsole_check_checksum(c, *p)) < 0)
        goto cleanup;

      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      if ((ret = ipmiconsole_check_authentication_code(c, *p, pkt, pkt_len)) < 0)
        goto cleanup;

      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_outbound_sequence_number(c, *p)) < 0)
        goto cleanup;
      
      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_session_id(c, *p)) < 0)
        goto cleanup;
      
      /* This is one check we always care about */
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_network_function(c, *p)) < 0)
        goto cleanup;

      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_command(c, *p)) < 0)
        goto cleanup;

      /* This is one check we always care about */
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_requester_sequence_number(c, *p)) < 0)
        goto cleanup;

      if (!s->close_session_flag && !ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}
      
      /* If everything passes except the completion code, then there
       * is an IPMI error.
       */
      if ((ret = ipmiconsole_check_completion_code(c, *p)) < 0)
        goto cleanup;

      if (!ret)
        {
	  uint8_t comp_code;
	  uint64_t val;

          if (s->try_new_port_flag)
            {
              /* XXX: Come back later when you learn more */
              /* If we're trying a different port, this call may or may
               * not succeed.  If the completion code is invalid, it's ok.
               */
              goto accept_packet;
            }

	  /* Special case: If the payload is already
	   * deactivated/disabled, who cares.  Either we are closing
	   * the session and we don't care, or we are trying to
	   * deactivate a payload that was earlier detected as being
	   * activated but (perhaps due to a race) is not anymore.
	   */
	  
	  if (Fiid_obj_get(c,
			   s->obj_deactivate_payload_rs,
			   "comp_code",
			   &val) < 0)
	    goto cleanup;
	  comp_code = val;
	  
	  if (comp_code == IPMI_COMP_CODE_PAYLOAD_ALREADY_DEACTIVATED
	      || comp_code == IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED)
	    goto accept_packet;

          _receive_packet_data_reset(c);
          ipmiconsole_calculate_errnum(c, *p);
          goto cleanup;
        }
    }
  else if (*p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS)
    {
      /* Generally we do not care about the validity of this packet
       * b/c we are closing the session.  We will still call the check
       * functions so debugging will be logged appropriately and
       * outbound sequence number checks can move the window along
       * appropriately.
       */

      if ((ret = ipmiconsole_check_payload_type(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_payload_pad(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_integrity_pad(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_checksum(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_authentication_code(c, *p, pkt, pkt_len)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_outbound_sequence_number(c, *p)) < 0)
        goto cleanup;
      
      if ((ret = ipmiconsole_check_session_id(c, *p)) < 0)
        goto cleanup;
      
      /* This is one check we do care about */
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_network_function(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_command(c, *p)) < 0)
        goto cleanup;

      /* This is one check we do care about */
      if (!ret)
	{
	  s->errors_count++;
	  rv = 0;
	  goto cleanup;
	}

      if ((ret = ipmiconsole_check_requester_sequence_number(c, *p)) < 0)
        goto cleanup;

      if ((ret = ipmiconsole_check_completion_code(c, *p)) < 0)
        goto cleanup;
    }
  else
    {
      IPMICONSOLE_CTX_DEBUG(c, ("invalid packet type: %d", p));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

 accept_packet:
  /* Special case for ping */
  if (*p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS)
    {
      if (_receive_ping_packet_data_reset(c) < 0)
	goto cleanup;
    }
  else
    {
      if (_receive_packet_data_reset(c) < 0)
	goto cleanup;
    }

  rv = 1;

 cleanup:
  secure_memset(pkt, '\0', IPMICONSOLE_PACKET_BUFLEN);
  Fiid_obj_clear(c, s->obj_lan_session_hdr_rs);
  Fiid_obj_clear(c, s->obj_rmcpplus_session_trlr_rs);
  return rv;
}

/* 
 * Returns 1 if close session has been initiated and packet sent
 * Returns 0 if close session has not been initiated or packet not sent
 * Returns -1 on error or to quit
 */
static int
_close_session(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.close_session_flag);

  s = &(c->session);

  /* 
   * Close the session differently depending on the state of the session
   */
  if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_START
      || s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT
      || s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT
      || s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
    {
      /* Session has not started yet, so we can inform the state
       * machine to just quit
       */
      return -1;
    }
  /*
   * else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
   *
   * The session may or may not be up yet, depending on if the RAKP Message 4
   * response has arrived.  
   *
   * Code logic in _ipmi_retransmission_timeout() and _process_ctx()
   * will handle this situation.
   */
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT
	   || s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_CHANNEL_PAYLOAD_SUPPORT_SENT
           || s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT)
    {
      /* Session is up, but the SOL payload is not activated, so close the session */
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
        return -1;
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
      return 1;
    }
  /* else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT)
   *
   * The SOL payload activation may or may not be up yet,
   * depending on if the Activate Payload response has arrived.
   *
   * Code logic in _ipmi_retransmission_timeout() and _process_ctx()
   * will handle this situation.
   */
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION)
    {
      /* The SOL session is setup, so deactivate the payload first */
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
	return -1;
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
      return 1;
    }
  /* else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT
   *          || s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT)
   *
   * We're happily on our way to closing, so let the state machine finish up 
   */

  return 0;
}

/* 
 * Returns 1 if close session has been initiated and packet sent
 * Returns 0 if close session has not been initiated or packet not sent
 * Returns -1 on error or to quit
 */
static int
_check_close_session(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);

  s = &(c->session);

  if (!s->close_session_flag)
    return 0;

  return _close_session(c);
}

/* 
 * Returns  1 if session timed out
 * Returns  0 if session did not time out
 * Returns -1 on error
 */
static int
_session_timeout(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  struct timeval current;
  struct timeval timeout;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);

  s = &(c->session);
  timeval_add_ms(&(s->last_ipmi_packet_received), c->config.session_timeout_len, &timeout);
  if (gettimeofday(&current, NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  return timeval_gt(&current, &timeout);
}

/* 
 * Returns  1 if packet timed out and packet is constructed
 * Returns  0 if packet did not time out
 * Returns -1 on error
 */
static int
_ipmi_retransmission_timeout(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  struct timeval current;
  struct timeval timeout;
  unsigned int retransmission_timeout_len;
  unsigned int retransmission_timeout_multiplier;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state != IPMICONSOLE_PROTOCOL_STATE_START);
  assert(c->session.protocol_state != IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);

  s = &(c->session);

  if (c->config.retransmission_backoff_count)
    retransmission_timeout_multiplier = (s->retransmission_count / c->config.retransmission_backoff_count) + 1;
  else
    retransmission_timeout_multiplier = 1;
  retransmission_timeout_len = c->config.retransmission_timeout_len * retransmission_timeout_multiplier;
      
  timeval_add_ms(&(s->last_ipmi_packet_sent), retransmission_timeout_len, &timeout);

  if (gettimeofday(&current, NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  if (!timeval_gt(&current, &timeout))
    return 0;
  
  s->retransmission_count++;
  if (s->retransmission_count > c->config.maximum_retransmission_count)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("closing session due to excessive retransmissions"));
      c->errnum = IPMICONSOLE_ERR_EXCESS_RETRANSMISSIONS_SENT;
      return -1;
    }
#if 0
  IPMICONSOLE_CTX_DEBUG(c, ("retransmission: retransmission_count = %d; maximum_retransmission_count = %d; protocol_state = %d", s->retransmission_count, c->config.maximum_retransmission_count, s->protocol_state));
#endif
  
  /* Note: 
   *
   * With the exception of close-session conditions, protocol
   * states do not have to be changed.  These are retransmissions,
   * so the protocol state can stay the same.
   */
  if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RQ) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
    {
      if (s->close_session_flag)
        {
          /* We assume the RAKP 4 response message was lost.  So the 
           * IPMI session was never established.  We just return -1 so the 
           * session can be closed.
           */
          return -1;
        }
      
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_CHANNEL_PAYLOAD_SUPPORT_SENT)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT)
    {
      if (s->close_session_flag)
        {
          /* We assume the Activate Payload response message was
           * lost.  So the IPMI session was established but the
           * payload session wasn't.  Therefore, we send the close
           * session packet to begin the teardown.
           */
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            return -1;
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          return 0;
        }          
      
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
        return -1;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT)
    {
      if (s->close_session_flag)
        {
          /* If we timeout on a close session packet and truly
           * wish to close the session, don't bother
           * retransmitting.  The previous close session response
           * may have been dropped by the network.  If that's the
           * case, any retransmitted close session responses will
           * fail b/c the session is already done.
           */
          s->close_timeout_flag++;
        }
    }
  else
    {
      IPMICONSOLE_CTX_DEBUG(c, ("invalid protocol_state: %d", s->protocol_state));
      return -1;
    }

  return 1;
}

/* 
 * Returns  1 if sol packet timed out and packet is constructed
 * Returns  0 if sol packet did not time out
 * Returns -1 on error
 */
static int
_sol_retransmission_timeout(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  struct timeval current;
  struct timeval timeout;
  
  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);

  s = &(c->session);
  
  timeval_add_ms(&(s->last_sol_input_packet_sent), c->config.retransmission_timeout_len, &timeout);
  if (gettimeofday(&current, NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }
  
  if (!timeval_gt(&current, &timeout))
    return 0;

  s->retransmission_count++;
  if (s->retransmission_count > c->config.maximum_retransmission_count)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("closing session due to excessive sol retransmissions"));
      c->errnum = IPMICONSOLE_ERR_EXCESS_RETRANSMISSIONS_SENT;
      return -1;
    }
#if 0
  IPMICONSOLE_CTX_DEBUG(c, ("sol retransmission: retransmission_count = %d; maximum_retransmission_count = %d; protocol_state = %d", s->retransmission_count, c->config.maximum_retransmission_count, s->protocol_state));
#endif

  if (!s->sol_input_waiting_for_break_ack)
    {
      /* Notes: If the previous sol transmission included an ACK,
       * it will be lost here.  The BMC is responsible for retransmitting
       * character data for any data it was not ACKed.
       */
      if (_send_sol_packet_with_character_data(c, 0, 0, 1) < 0)
        return -1;
    }
  else
    {
      if (_send_sol_packet_generate_break(c, 1) < 0)
        return -1;
    }

  return 1;
}

/* 
 * Returns 1 if keepalive is necessary
 * Returns 0 if keepalive is not necessary
 * Returns -1 on error
 */
static int
_keepalive_is_necessary(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  struct timeval current;
  struct timeval timeout;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);

  s = &(c->session);

  timeval_add_ms(&(s->last_ipmi_packet_received), c->config.keepalive_timeout_len, &timeout);
  if (gettimeofday(&current, NULL) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  if (timeval_gt(&current, &timeout))
    return 1;
  else
    return 0;
}

/* 
 * Returns  1 if keepalive timed out and packet is constructed
 * Returns  0 if keepalive did not time out
 * Returns -1 on error
 */
static int
_keepalive_timeout(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  struct timeval current;
  struct timeval timeout;
  int rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);

  s = &(c->session);

  /* 
   * Timeout logic will send a keepalive packet if:
   *
   * A) If it is required
   * B) If one hasn't been sent in keepalive timeout time period 
   */

  if ((rv = _keepalive_is_necessary(c)) < 0)
    return -1;

  if (rv)
    {
      if (gettimeofday(&current, NULL) < 0)
	{
	  IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
	  c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
	  return -1;
	}

      timeval_add_ms(&(s->last_keepalive_packet_sent), c->config.retransmission_keepalive_timeout_len, &timeout);
      if (timeval_gt(&current, &timeout))
	{
	  /* Note that the protocol_state stays in SOL_SESSION */
	  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ) < 0)
	    return -1;
	  return 1;
	}
    }

  return 0;
}

/* 
 * Return 1 if IPMI 2.0 is supported
 * Return 0 if IPMI 2.0 is not supported
 * Return -1 on error
 */
static int
_check_for_ipmi_2_0_support(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t ipmi_v20_extended_capabilities_available;
  uint8_t channel_supports_ipmi_v20_connections;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT);

  s = &(c->session);

  if (Fiid_obj_get(c,
                   s->obj_authentication_capabilities_v20_rs,
                   "authentication_type.ipmi_v2.0_extended_capabilities_available",
                   &val) < 0)
    return -1;
  ipmi_v20_extended_capabilities_available = val;
  
  if (Fiid_obj_get(c,
                   s->obj_authentication_capabilities_v20_rs,
                   "channel_supports_ipmi_v2.0_connections",
                   &val) < 0)
    return -1;
  channel_supports_ipmi_v20_connections = val;
  
  if (!ipmi_v20_extended_capabilities_available
      || !channel_supports_ipmi_v20_connections)
    {
      c->errnum = IPMICONSOLE_ERR_IPMI_2_0_UNAVAILABLE;
      return 0;
    }
  
  return 1;
}

/* 
 * Return 0 if authentication is supported
 * Return -1 if authentication is not supported or error
 */
static int
_check_for_authentication_support(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t authentication_status_anonymous_login;
  uint8_t authentication_status_null_username;
  uint8_t authentication_status_non_null_username;
  uint8_t authentication_status_k_g;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT);

  s = &(c->session);

  if (Fiid_obj_get(c,
                   s->obj_authentication_capabilities_v20_rs,
                   "authentication_status.anonymous_login",
                   &val) < 0)
    return -1;
  authentication_status_anonymous_login = val;
  
  if (Fiid_obj_get(c,
                   s->obj_authentication_capabilities_v20_rs,
                   "authentication_status.null_username",
                   &val) < 0)
    return -1;
  authentication_status_null_username = val;

  if (Fiid_obj_get(c,
                   s->obj_authentication_capabilities_v20_rs,
                   "authentication_status.non_null_username",
                   &val) < 0)
    return -1;
  authentication_status_non_null_username = val;

  if (Fiid_obj_get(c,
                   s->obj_authentication_capabilities_v20_rs,
                   "authentication_status.k_g",
                   &val) < 0)
    return -1;
  authentication_status_k_g = val;
  
  /* IPMI Workaround
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * The ASUS motherboard reports incorrect settings of anonymous
   * vs. null vs non-null username capabilities.  The motherboard also
   * reports K_g status incorrectly too.  The workaround is to skip
   * all these checks.
   *
   */
  if (!(c->config.workaround_flags & IPMICONSOLE_WORKAROUND_AUTHENTICATION_CAPABILITIES))
    {
      if ((!strlen((char *)c->config.username) && !strlen((char *)c->config.password)
           && !authentication_status_anonymous_login)
          || (!strlen((char *)c->config.username)
              && !authentication_status_anonymous_login
              && !authentication_status_null_username)
          || (strlen((char *)c->config.username)
              && !authentication_status_non_null_username))
        {
          c->errnum = IPMICONSOLE_ERR_USERNAME_INVALID;
          return -1;
        }

      if ((!c->config.k_g_len && authentication_status_k_g)
          || (c->config.k_g_len && !authentication_status_k_g))
        {
          c->errnum = IPMICONSOLE_ERR_K_G_INVALID;
          return -1;
        }
    }

  return 0;
}

/* 
 * Return 0 on success
 * Return -1 on error
 */
static int
_calculate_cipher_keys(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int32_t managed_system_random_number_len;
  uint8_t username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  uint8_t *username;
  uint32_t username_len;
  uint8_t *password;
  uint32_t password_len;
  uint8_t *k_g;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT);
  assert(c->session.sik_key_ptr == c->session.sik_key);
  assert(c->session.sik_key_len == IPMI_MAX_SIK_KEY_LENGTH);
  assert(c->session.integrity_key_ptr == c->session.integrity_key);
  assert(c->session.integrity_key_len == IPMI_MAX_INTEGRITY_KEY_LENGTH);
  assert(c->session.confidentiality_key_ptr == c->session.confidentiality_key);
  assert(c->session.confidentiality_key_len == IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH);

  s = &(c->session);
 
  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations pad their usernames.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0)
    {
      memset(username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (strlen((char *)c->config.username))
        strcpy((char *)username_buf, (char *)c->config.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      if (strlen((char *)c->config.username))
        username = c->config.username;
      else
        username = NULL;
      username_len = (username) ? strlen((char *)username) : 0;
    }

  if (strlen((char *)c->config.password))
    password = c->config.password;
  else
    password = NULL;
  
  password_len = (password) ? strlen((char *)password) : 0;

  /* IPMI Workaround
   *
   * Intel IPMI 2.0 implementations improperly calculate HMAC-MD5-128 hashes
   * when the passwords are > 16 bytes long.  The BMCs probably assume
   * all keys are <= 16 bytes in length.  So we have to adjust.
   */
  if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_INTEL_2_0
      && s->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;
  
  if (c->config.k_g_len)
    k_g = (uint8_t *)c->config.k_g;
  else
    k_g = NULL;
  
  if ((managed_system_random_number_len = Fiid_obj_get_data(c,
                                                            s->obj_rakp_message_2,
                                                            "managed_system_random_number",
                                                            managed_system_random_number,
                                                            IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    return -1;

  if (managed_system_random_number_len != IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("invalid managed_system_random_number_len = %d", managed_system_random_number_len));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (ipmi_calculate_rmcpplus_session_keys(s->authentication_algorithm,
                                           s->integrity_algorithm,
                                           s->confidentiality_algorithm,
                                           password,
                                           password_len,
                                           k_g,
                                           (k_g) ? c->config.k_g_len : 0,
                                           s->remote_console_random_number,
                                           IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                           managed_system_random_number,
                                           IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH,
                                           s->name_only_lookup,
                                           c->config.privilege_level,
                                           username,
                                           username_len,
                                           &(s->sik_key_ptr),
                                           &(s->sik_key_len),
                                           &(s->integrity_key_ptr),
                                           &(s->integrity_key_len),
                                           &(s->confidentiality_key_ptr),
                                           &(s->confidentiality_key_len)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("ipmi_calculate_rmcpplus_session_keys: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  return 0;
}

/*
 * Return 1 if SOL is supported
 * Return 0 if SOL is not supported
 * Return -1 on error
 */
static int
_check_sol_supported(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t standard_payload_type_1_supported;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_CHANNEL_PAYLOAD_SUPPORT_SENT);
  
  s = &(c->session);

  /* 
   * Note: Payload Type #1 is the SOL Payload Type.  See IPMI 2.0 Spec
   * Section 13.27.3 and Table 13-16.
   */

  if (Fiid_obj_get(c,
                   s->obj_get_channel_payload_support_rs,
                   "standard_payload_type_1_supported",
                   &val) < 0)
    return -1;
  standard_payload_type_1_supported = val;

  return (standard_payload_type_1_supported ? 1 : 0);
}

/*
 * Return 1 if SOL is activated
 * Return 0 if SOL is not activated
 * Return -1 on error
 */
static int
_check_sol_activated(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  uint64_t val;
  int i;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT);
  assert(!c->session.deactivate_payload_instances_and_try_again_flag);
  
  s = &(c->session);

  /* May not be 0, see notes in _process_ctx() */
  if (s->sol_instance_capacity
      || s->sol_instances_activated_count
      || s->sol_instances_deactivated_count)
    {
      s->sol_instance_capacity = 0;
      memset(s->sol_instances_activated, '\0', IPMI_INSTANCES_ACTIVATED_LENGTH);
      s->sol_instances_activated_count = 0;
      s->sol_instances_deactivated_count = 0;
    }

  if (Fiid_obj_get(c,
                   s->obj_get_payload_activation_status_rs,
                   "instance_capacity",
                   &val) < 0)
    return -1;
  s->sol_instance_capacity = val;

  if (s->sol_instance_capacity > IPMI_INSTANCES_ACTIVATED_LENGTH)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("invalid instance capacity: %d", s->sol_instance_capacity));
      c->errnum = IPMICONSOLE_ERR_BMC_IMPLEMENTATION;
      return -1;
    }
  
  for (i = 0; i < s->sol_instance_capacity; i++)
    {
      char fieldstr[64];

      memset(fieldstr, '\0', 64);
      snprintf(fieldstr, 64, "instance_%d", i+1);
      
      if (Fiid_obj_get(c,
                       s->obj_get_payload_activation_status_rs,
                       fieldstr,
                       &val) < 0)
        return -1;

      if (val)
        {
          s->sol_instances_activated[s->sol_instances_activated_count] = i+1;
          s->sol_instances_activated_count++;
        }
    }
  
  if (c->config.security_flags & IPMICONSOLE_SECURITY_ERROR_ON_SOL_INUSE
      && s->sol_instances_activated_count)
    {
      c->errnum = IPMICONSOLE_ERR_SOL_INUSE;
      return -1;
    }

  return (s->sol_instances_activated_count ? 1 : 0);
}

/*
 * Return 1 if SOL is activated
 * Return 0 if SOL is not activated
 * Return -1 on error
 */
static int
_check_sol_activated2(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  uint64_t val;
  uint8_t comp_code;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT);
  
  s = &(c->session);

  if (Fiid_obj_get(c,
                   s->obj_activate_payload_rs,
                   "comp_code",
                   &val) < 0)
    return -1;
  comp_code = val;

  if (comp_code == IPMI_COMP_CODE_PAYLOAD_ALREADY_ACTIVE_ON_ANOTHER_SESSION
      || comp_code == IPMI_COMP_CODE_PAYLOAD_ACTIVATION_LIMIT_REACHED)
    {
      if (c->config.security_flags & IPMICONSOLE_SECURITY_ERROR_ON_SOL_INUSE)
	{
	  c->errnum = IPMICONSOLE_ERR_SOL_INUSE;
	  return -1;
	}
      
      return 1;
    }

  if (comp_code == IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("SOL unavailable"));
      c->errnum = IPMICONSOLE_ERR_SOL_UNAVAILABLE;
      return -1;
    }

  if (comp_code == IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITH_ENCRYPTION)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("SOL requires no encryption"));
      c->errnum = IPMICONSOLE_ERR_SOL_REQUIRES_NO_ENCRYPTION;
      return -1;
    }

  if (comp_code == IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("SOL requires encryption"));
      c->errnum = IPMICONSOLE_ERR_SOL_REQUIRES_ENCRYPTION;
      return -1;
    }

  return 0;
}

/* 
 * Return 1 if payload sizes are ok
 * Return 0 if payload sizes don't make any sense
 * Return -1 on error
 */
static int
_check_payload_sizes_legitimate(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  int32_t sol_hdr_len;
  uint64_t val;
  uint16_t max_inbound_payload_size;
  uint16_t max_outbound_payload_size;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT);
  
  s = &(c->session);

  /* In the IPMI 2.0 Spec, Section 15.9, Table 15-2, you can see that
   * the accepted character count is 1 byte.  Therefore, the maximum
   * number of serial characters that should be sent in one packet is
   * 255.  Adding in the headers of a SOL payload packet (packet
   * sequence number = 1 byte, ack/nack flag = 1 byte, accepted
   * character count = 1 byte, operation bits = 1 byte), the
   * in/outbound payload sizes shouldn't be greater than 259.  The
   * minimum must be the SOL header bytes plus 1 character byte, which
   * is 5.
   */

  if (Fiid_obj_get(c,
                   s->obj_activate_payload_rs,
                   "inbound_payload_size",
                   &val) < 0)
    return -1;
  max_inbound_payload_size = val;

  if (Fiid_obj_get(c,
                   s->obj_activate_payload_rs,
                   "outbound_payload_size",
                   &val) < 0)
    return -1;
  max_outbound_payload_size = val;

  if ((sol_hdr_len = Fiid_template_block_len_bytes(c,
                                                   tmpl_sol_payload_data,
                                                   "packet_sequence_number",
                                                   "operation_status")) < 0)
    return -1;

  /* IPMI Workaround
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * The ASUS motherboard reports incorrect payload sizes.  Skip the
   * check and assume a reasonable size.
   *
   */
  if (!(c->config.workaround_flags & IPMICONSOLE_WORKAROUND_ASUS_2_0))
    {
      if (max_inbound_payload_size >= IPMICONSOLE_MIN_CHARACTER_DATA + sol_hdr_len
          && max_inbound_payload_size <= IPMICONSOLE_MAX_CHARACTER_DATA + sol_hdr_len
          && max_outbound_payload_size >= IPMICONSOLE_MIN_CHARACTER_DATA + sol_hdr_len
          && max_outbound_payload_size <= IPMICONSOLE_MAX_CHARACTER_DATA + sol_hdr_len)
        {
          s->max_sol_character_send_size = max_outbound_payload_size - sol_hdr_len;
          return 1;
        }
    }
  else
    {
      /* Lets try 32, seems like a decent power of two number */
      s->max_sol_character_send_size = 32;
      return 1;
    }
  
  IPMICONSOLE_CTX_DEBUG(c, ("payload sizes invalid: max_inbound_payload_size=%d max_outbound_payload_size=%d", max_inbound_payload_size, max_outbound_payload_size));
  return 0;
}

/* 
 * Return 1 if we should try a new port
 * Return 0 if we should not try a new port
 * Return -1 on error
 */
static int
_check_try_new_port(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  int16_t console_port;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT);

  s = &(c->session);

  if (Fiid_obj_get(c,
                   s->obj_activate_payload_rs,
                   "payload_udp_port_number",
                   &val) < 0)
    return -1;
  console_port = val;

  /* Note: The state machine currently gives the new console port
   * only 1 try.  No cycling through a bunch of port options.
   */
  if (s->console_port == RMCP_PRIMARY_RMCP_PORT)
    {
      if (s->console_port != console_port)
        {
          s->console_port = console_port;
          return 1;
        }
    }
  else
    {
      if (s->console_port != console_port)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("multiple new console ports attempted"));
          c->errnum = IPMICONSOLE_ERR_BMC_IMPLEMENTATION;
          return -1;
        }
      return 1;
    }
  return 0;
}

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_sol_bmc_to_remote_console_packet(ipmiconsole_ctx_t c)
{
  struct ipmiconsole_ctx_session *s;
  uint8_t packet_sequence_number;
  uint8_t packet_ack_nack_sequence_number;
  uint8_t accepted_character_count;
  uint8_t break_condition;
  uint8_t transmit_overrun;
  uint8_t sol_deactivating;
  uint8_t nack;
  uint64_t val;
  int n, dropped, rv = -1;
  int secure_malloc_flag;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->session.protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION);

  s = &(c->session);

  secure_malloc_flag = (c->config.security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY) ? 1 : 0;

  /* 
   * The packet is either an ACK to a packet we sent, or
   * output from the console.
   *
   * If the packet contains an ACK to a packet we sent, we must first
   * verify that the packet sequence number matches.  If it does, we
   * check the accepted character count.  If the accepted character
   * count is less than the character count we sent, we adjust,
   * possibly read more data from the console input and send out
   * another SOL input packet.
   *
   * If the packet contains character data from the BMC, we accept all
   * of the data and send an acknowledgement of it, possibly with
   * additional character data.
   */

  if (Fiid_obj_get(c,
                   s->obj_sol_payload_data_rs,
                   "packet_sequence_number",
                   &val) < 0)
    goto cleanup;
  packet_sequence_number = val;
  
  if (Fiid_obj_get(c,
                   s->obj_sol_payload_data_rs,
                   "packet_ack_nack_sequence_number",
                   &val) < 0)
    goto cleanup;
  packet_ack_nack_sequence_number = val;
  
  if (Fiid_obj_get(c,
                   s->obj_sol_payload_data_rs,
                   "accepted_character_count",
                   &val) < 0)
    goto cleanup;
  accepted_character_count = val;

  if (Fiid_obj_get(c,
                   s->obj_sol_payload_data_rs,
                   "break_condition",
                   &val) < 0)
    goto cleanup;
  break_condition = val;

  if (Fiid_obj_get(c,
                   s->obj_sol_payload_data_rs,
                   "transmit_overrun",
                   &val) < 0)
    goto cleanup;
  transmit_overrun = val;

  if (Fiid_obj_get(c,
                   s->obj_sol_payload_data_rs,
                   "sol_deactivating",
                   &val) < 0)
    goto cleanup;
  sol_deactivating = val;

  if (Fiid_obj_get(c,
                   s->obj_sol_payload_data_rs,
                   "nack",
                   &val) < 0)
    goto cleanup;
  nack = val;

  if (sol_deactivating)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("SOL Deactivating"));
      c->errnum = IPMICONSOLE_ERR_SOL_STOLEN;
      goto cleanup;
    }

  if (transmit_overrun)
    IPMICONSOLE_CTX_DEBUG(c, ("Transmit overrun.  Character data lost"));

  /* Packet contains ACK */
  if (packet_ack_nack_sequence_number
      && s->sol_input_waiting_for_ack
      && s->sol_input_packet_sequence_number == packet_ack_nack_sequence_number)
    {
      if (!s->sol_input_waiting_for_break_ack)
        {
          /* It's ok if it's a NACK, but we'll log for debugging anyways */
          if (nack == IPMI_SOL_NACK
              || accepted_character_count != s->sol_input_character_data_len)
            {
              IPMICONSOLE_CTX_DEBUG(c, ("SOL packet NACKED: accepted_character_count: accepted_character_count = %d,  s->sol_input_character_data_len = %d\n", accepted_character_count, s->sol_input_character_data_len));
            }

          if (accepted_character_count > s->sol_input_character_data_len)
            {
              /* Ummm, We'll assume its wrong and just lessen the
               * accepted character count??? 
               */
              IPMICONSOLE_CTX_DEBUG(c, ("Unexpected accepted_character_count: accepted_character_count = %d,  s->sol_input_character_data_len = %d\n", accepted_character_count, s->sol_input_character_data_len));
              accepted_character_count = s->sol_input_character_data_len;
            }
         
          if ((n = cbuf_drop(s->console_remote_console_to_bmc, accepted_character_count)) < 0)
            {
              IPMICONSOLE_CTX_DEBUG(c, ("cbuf_drop: %s", strerror(errno)));
              c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
              goto cleanup;
            }
          
          if (s->console_remote_console_to_bmc_bytes_before_break)
            {
              if (accepted_character_count > s->console_remote_console_to_bmc_bytes_before_break)
                {
                  IPMICONSOLE_CTX_DEBUG(c, ("Unexpected accepted_character_count: accepted_character_count = %d,  s->console_remote_console_to_bmc_bytes_before_break = %d\n", accepted_character_count, s->console_remote_console_to_bmc_bytes_before_break));
                  accepted_character_count = s->console_remote_console_to_bmc_bytes_before_break;
                }
              s->console_remote_console_to_bmc_bytes_before_break -= accepted_character_count;
            }
          s->sol_input_waiting_for_ack = 0;
          s->sol_input_character_data_len = 0;
        }
      else
        {
          /* achu: Note, it's possible the BMC Nacks the data.  B/c we
	     didn't send any character data long with the break */

#if 0
	  /* achu: On some IPMI 2.0 Tyan boards, the break seems to be
	   * "acked" with a break condition.  But upon further
	   * reading, this may not be the appropriate interpretation.
	   * We'll leave this code in here for historical purposes
	   * though.
	   */

          /* Since the sequence number is acknowledged, we're going to
           * assume the break happened even if the break condition is
           * not specified
           */
          if (break_condition != IPMI_SOL_BREAK_CONDITION_DETECTED)
            IPMICONSOLE_CTX_DEBUG(c, ("SOL packet w/o break condition detected"));
#endif
	  s->break_requested = 0;
          s->sol_input_waiting_for_ack = 0;
          s->sol_input_waiting_for_break_ack = 0;
          s->sol_input_character_data_len = 0;
        }
    }
  else if (packet_ack_nack_sequence_number
	   && !s->sol_input_waiting_for_ack)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("Unexpected ACK: packet_ack_nack_sequence_number = %d, s->sol_input_packet_sequence_number = %d", packet_ack_nack_sequence_number, s->sol_input_packet_sequence_number));
    }      

  if (packet_sequence_number)
    {
      /* There is some data for the user */
      char character_data[IPMICONSOLE_MAX_CHARACTER_DATA+1];
      int32_t character_data_len = 0;
      int32_t character_data_len_to_write = 0;
      int32_t character_data_index = 0;

      memset(character_data, '\0', IPMICONSOLE_MAX_CHARACTER_DATA + 1);
      
      if ((character_data_len = Fiid_obj_get_data(c,
                                                  s->obj_sol_payload_data_rs,
                                                  "character_data",
                                                  (uint8_t *)character_data,
                                                  IPMICONSOLE_MAX_CHARACTER_DATA)) < 0)
	goto cleanup;

      /* XXX: Should do > or >= check?? */
      if (s->last_sol_output_packet_sequence_number == packet_sequence_number)
        {
          /* Retransmission from the BMC */

          /* The BMC elected to transfer additional data with the
           * retransmission.  We will give the user only the new information,
           * but we must ACK all of the data.
           */
          if (character_data_len >= s->last_sol_output_accepted_character_count)
            {
              character_data_len_to_write = character_data_len - s->last_sol_output_accepted_character_count;
              character_data_index = s->last_sol_output_accepted_character_count;
            }
        }
      else
        character_data_len_to_write = character_data_len;

      if (character_data_len_to_write)
        {
          n = cbuf_write(s->console_bmc_to_remote_console,
			 character_data + character_data_index,
			 character_data_len_to_write,
			 &dropped,
                         secure_malloc_flag);
        
	  /* Clear out data */
	  secure_memset(character_data, '\0', IPMICONSOLE_MAX_CHARACTER_DATA+1);
     
	  if (n < 0)
	    {
              IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: %s", strerror(errno)));
              c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
              goto cleanup;
            }

          if (n != character_data_len_to_write)
            {
              IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: invalid bytes written; n=%d; character_data_len_to_write=%d", n, character_data_len_to_write));
              c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
              goto cleanup;
            }
          
          if (dropped)
            {
              IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: dropped data: dropped=%d", dropped));
              c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
              goto cleanup;
            }
       }

      s->last_sol_output_packet_sequence_number = packet_sequence_number;
      s->last_sol_output_accepted_character_count = character_data_len;
      
      /* Acknowledge this data.  Send some user data if we got some.*/
      if (!s->sol_input_waiting_for_ack && !cbuf_is_empty(s->console_remote_console_to_bmc))
        {
          if (_send_sol_packet_with_character_data(c, 
                                                   packet_sequence_number,
                                                   character_data_len,
                                                   0) < 0)
            goto cleanup;
        }
      else
        {
          if (_send_sol_packet_ack_only(c,
                                        packet_sequence_number,
                                        character_data_len) < 0)
            goto cleanup;
        }
    }
  else
    {
      if (!cbuf_is_empty(s->console_remote_console_to_bmc)
          && !s->sol_input_waiting_for_ack
          && (!s->break_requested
              || (s->break_requested && s->console_remote_console_to_bmc_bytes_before_break)))
        {
          if (_send_sol_packet_with_character_data(c, 0, 0, 0) < 0)
            goto cleanup;
        }
      else if (s->break_requested)
        {
          if (_send_sol_packet_generate_break(c, 0) < 0)
            goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  fiid_obj_clear(s->obj_sol_payload_data_rs);
  return rv;
}
          

/* 
 * Returns 0 on success
 * Returns -1 on error
 */
static int
_calculate_timeout(ipmiconsole_ctx_t c, unsigned int *timeout)
{
  struct ipmiconsole_ctx_session *s;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(timeout);

  s = &(c->session);

  if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION)
    {
      struct timeval current;
      struct timeval session_timeout;
      struct timeval session_timeout_val;
      struct timeval sol_retransmission_timeout;
      struct timeval sol_retransmission_timeout_val;
      struct timeval keepalive_timeout;
      struct timeval keepalive_timeout_val;
      unsigned int session_timeout_ms;
      unsigned int sol_retransmission_timeout_len;
      unsigned int sol_retransmission_timeout_multiplier;
      unsigned int sol_retransmission_timeout_ms;
      unsigned int keepalive_timeout_ms;
      int rv;

      if (gettimeofday(&current, NULL) < 0)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
          c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
          return -1;
        }
      
      timeval_add_ms(&s->last_ipmi_packet_received, c->config.session_timeout_len, &session_timeout);
      timeval_sub(&session_timeout, &current, &session_timeout_val);
      timeval_millisecond_calc(&session_timeout_val, &session_timeout_ms);
      *timeout = session_timeout_ms;

      if (s->sol_input_waiting_for_ack)
	{
	  if (c->config.retransmission_backoff_count)
	    sol_retransmission_timeout_multiplier = (s->retransmission_count / c->config.retransmission_backoff_count) + 1;
	  else
	    sol_retransmission_timeout_multiplier = 1;
	    
	  sol_retransmission_timeout_len = c->config.retransmission_timeout_len * sol_retransmission_timeout_multiplier;

	  timeval_add_ms(&s->last_sol_input_packet_sent, sol_retransmission_timeout_len, &sol_retransmission_timeout);
	  timeval_sub(&sol_retransmission_timeout, &current, &sol_retransmission_timeout_val);
	  timeval_millisecond_calc(&sol_retransmission_timeout_val, &sol_retransmission_timeout_ms);
	  if (sol_retransmission_timeout_ms < *timeout)
	    *timeout = sol_retransmission_timeout_ms;
	}

      if ((rv = _keepalive_is_necessary(c)) < 0)
	return -1;

      if (rv)
	{
	  /* Time within we should retransmit the current keepalive packet */
	  timeval_add_ms(&s->last_keepalive_packet_sent, c->config.retransmission_keepalive_timeout_len, &keepalive_timeout);
	  timeval_sub(&keepalive_timeout, &current, &keepalive_timeout_val);
	  timeval_millisecond_calc(&keepalive_timeout_val, &keepalive_timeout_ms);
	  if (keepalive_timeout_ms < *timeout)
	    *timeout = keepalive_timeout_ms;
	}
      else
	{
	  /* When a keepalive packet will be necessary again */
	  timeval_add_ms(&s->last_ipmi_packet_received, c->config.keepalive_timeout_len, &keepalive_timeout);
	  timeval_sub(&keepalive_timeout, &current, &keepalive_timeout_val);
	  timeval_millisecond_calc(&keepalive_timeout_val, &keepalive_timeout_ms);
	  if (keepalive_timeout_ms < *timeout)
	    *timeout = keepalive_timeout_ms;
	}
    }
  else
    {
      struct timeval current;
      struct timeval session_timeout;
      struct timeval session_timeout_val;
      struct timeval retransmission_timeout;
      struct timeval retransmission_timeout_val;
      unsigned int retransmission_timeout_len;
      unsigned int retransmission_timeout_multiplier;
      unsigned int session_timeout_ms;
      unsigned int retransmission_timeout_ms;
      
      if (gettimeofday(&current, NULL) < 0)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("gettimeofday: %s", strerror(errno)));
          c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
          return -1;
        }
      
      timeval_add_ms(&s->last_ipmi_packet_received, c->config.session_timeout_len, &session_timeout);
      timeval_sub(&session_timeout, &current, &session_timeout_val);
      timeval_millisecond_calc(&session_timeout_val, &session_timeout_ms);

      if (c->config.retransmission_backoff_count)
	retransmission_timeout_multiplier = (s->retransmission_count / c->config.retransmission_backoff_count) + 1;
      else
	retransmission_timeout_multiplier = 1;
	
      retransmission_timeout_len = c->config.retransmission_timeout_len * retransmission_timeout_multiplier;

      timeval_add_ms(&s->last_ipmi_packet_sent, retransmission_timeout_len, &retransmission_timeout);
      timeval_sub(&retransmission_timeout, &current, &retransmission_timeout_val);
      timeval_millisecond_calc(&retransmission_timeout_val, &retransmission_timeout_ms);

      if (retransmission_timeout_ms < session_timeout_ms)
        *timeout = retransmission_timeout_ms;
      else
        *timeout = session_timeout_ms;
    }

  return 0;
}

/* 
 * Return -1 if context has an error or has timed out
 *
 * XXX: Argh!  This function has now grown to be nearly 1000 lines
 * long.  Come back and clean this up.
 */
static int
_process_ctx(ipmiconsole_ctx_t c, unsigned int *timeout)
{
  struct ipmiconsole_ctx_session *s;
  ipmiconsole_packet_type_t p;
  int perr, ret, rv = -1;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(timeout);

  s = &(c->session);

  *timeout = 0;

  if ((ret = _check_close_session(c)) < 0)
    goto close_session;

  if (ret)
    goto calculate_timeout;

  /* Protocol State Special Case
   *
   * Since the IPMI session has not yet begun, timeouts aren't
   * possible, there are no packets to read, there are no retransmissions
   * that may be necessary, etc.
   */
  if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_START)
    {
      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RQ) < 0)

        /* The session isn't setup, no need to attempt to close it */
        goto close_session;
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT;
      goto calculate_timeout;
    } 

  if ((ret = _session_timeout(c)) < 0)
    goto close_session;

  if (ret)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("closing session due to session timeout"));
      c->errnum = IPMICONSOLE_ERR_SESSION_TIMEOUT;
      goto close_session;
    }

  if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION)
    {
      if (s->sol_input_waiting_for_ack)
        {
          if ((ret = _sol_retransmission_timeout(c)) < 0)
            {
              /* Attempt to close the session cleanly */
              s->close_session_flag++;
	      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
		goto close_session;
	      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
	      goto calculate_timeout;
            }

          if (ret)
            goto calculate_timeout;
        }
      else
        {
          /* _send_sol_packet_with_character_data() will not send more
           * than s->console_remote_console_to_bmc_bytes_before_break
           */
          if (!cbuf_is_empty(s->console_remote_console_to_bmc)
              && (!s->break_requested
                  || (s->break_requested && s->console_remote_console_to_bmc_bytes_before_break)))
            {
              if (_send_sol_packet_with_character_data(c, 0, 0, 0) < 0)
                {
		  /* Attempt to close the session cleanly */
		  s->close_session_flag++;
		  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
		    goto close_session;
		  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
		  goto calculate_timeout;
                }
              goto calculate_timeout;
            }
          
          if (s->break_requested)
            {
              if (_send_sol_packet_generate_break(c, 0) < 0)
                {
		  /* Attempt to close the session cleanly */
		  s->close_session_flag++;
		  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
		    goto close_session;
		  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
		  goto calculate_timeout;
                }
              goto calculate_timeout;
            }
        }

      /* Will handle keepalive retransmits too */
      if ((ret = _keepalive_timeout(c)) < 0)
        {
	  /* Attempt to close the session cleanly */
	  s->close_session_flag++;
	  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
	    goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
	  goto calculate_timeout;
        }
      
      if (ret)
        goto calculate_timeout;
    }
  else
    {
      if ((ret = _ipmi_retransmission_timeout(c)) < 0)
        {
          /* Attempt to close the session cleanly */
	  s->close_session_flag++;
	  if ((ret = _close_session(c)) < 0)
	    goto close_session;
	  if (ret)
	    goto calculate_timeout;
	  else
	    goto state_machine;
        }
      
      if (ret)
	goto calculate_timeout;

      if (s->close_timeout_flag)
	goto state_machine;
  }

  if ((ret = _receive_packet(c, &p)) < 0)
    {
      /* Attempt to close the session cleanly */
      s->close_session_flag++;
      if ((ret = _close_session(c)) < 0)
	goto close_session;
      if (ret)
        goto calculate_timeout;
      else
	goto state_machine;
    }
  
  if (!ret)
    {
      /* Notes:
       *
       * The errors_count and acceptable_packet_errors_count are
       * mostly to handle the corner case when the BMC gets out of
       * whack with its sequence numbers.  This has been witnessed
       * when a machine is rebooted with a network booted kernel.  The
       * SOL goes out during the network boot (according to vendors,
       * due to limited memory on the ethernet controller) and the SOL
       * output is "thrown out" by the network card.
       * 
       * However, the BMC (internally) still increments its session
       * sequence numbers, so by the time the kernel is booted, the
       * session sequence numbers are way out of whack.
       */
      if (s->errors_count > c->config.acceptable_packet_errors_count
	  && !s->close_session_flag)
	{
	  /* Attempt to close the session cleanly */
	  IPMICONSOLE_CTX_DEBUG(c, ("closing with excessive errors"));
	  c->errnum = IPMICONSOLE_ERR_EXCESS_ERRORS_RECEIVED;
	  s->close_session_flag++;
	  if ((ret = _close_session(c)) < 0)
	    goto close_session;
	  if (ret)
	    goto calculate_timeout;
	  else
	    goto state_machine;
	}
      goto calculate_timeout;
    }

 state_machine:
  if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT)
    {
      assert(p == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS);

      if ((ret = _check_for_ipmi_2_0_support(c)) < 0)
        /* The session isn't setup, no need to attempt to close it */
        goto close_session;
      
      if (!ret)
        /* The session isn't setup, no need to attempt to close it */
        goto close_session;

      if (_check_for_authentication_support(c) < 0)
        /* The session isn't setup, no need to attempt to close it */
        goto close_session;

      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST) < 0)
        /* The session isn't setup, no need to attempt to close it */
        goto close_session;
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT)
    {
      assert(p == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE);

      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1) < 0)
        /* The session isn't setup, no need to attempt to close it */
        goto close_session;
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
    {
      assert(p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2);

      if (_calculate_cipher_keys(c) < 0)
        /* The session isn't setup, no need to attempt to close it */
        goto close_session;

      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3) < 0)
        /* The session isn't setup, no need to attempt to close it */
        goto close_session;
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
    {
      assert(s->close_session_flag
	     || p == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4);

      if (s->close_session_flag)
        {
          /* The session could be up, depending on timeouts, etc. but
             since we aren't sure, we don't attempt to close it */
          goto close_session;
        }

      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ) < 0)
        {
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
        }
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT)
    {
      assert(p == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS);
      
      /* IPMI Workaround
       *
       * Discovered on Sun Fire 4100.
       *
       * The Get Channel Payload Support isn't supported in Sun's.  Skip this 
       * part of the state machine and pray for the best I guess.
       */
      if (c->config.workaround_flags & IPMICONSOLE_WORKAROUND_SUN_2_0)
        {
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ) < 0)
            {
              s->close_session_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
              goto calculate_timeout;
            }
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT;
          goto calculate_timeout;
        }

      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ) < 0)
        {
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
        }
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_GET_CHANNEL_PAYLOAD_SUPPORT_SENT;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_CHANNEL_PAYLOAD_SUPPORT_SENT)
    {
      assert(p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS);

      if ((ret = _check_sol_supported(c)) < 0)
        {
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
        }

      if (!ret)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("SOL unavailable"));
          c->errnum = IPMICONSOLE_ERR_SOL_UNAVAILABLE;
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
        }

      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ) < 0)
	{
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
	}
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT)
    {
      assert(p == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS);

      if ((ret = _check_sol_activated(c)) < 0)
        {
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
        }

      if (c->config.security_flags & IPMICONSOLE_SECURITY_DEACTIVATE_ONLY)
        {
          if (ret)
            {
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                {
                  s->close_session_flag++;
                  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
                    goto close_session;
                  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
                  goto calculate_timeout;
                }
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }
          else
            {
              s->close_session_flag++;
              s->deactivate_only_succeeded_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
              goto calculate_timeout;
            }
        }

      if (ret)
        {
          s->deactivate_payload_instances_and_try_again_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            {
	      s->close_session_flag++;
	      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
		goto close_session;
	      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
	      goto calculate_timeout;
            }
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }

      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ) < 0)
        {
	  s->close_session_flag++;
	  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
	    goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
	  goto calculate_timeout;
        }
      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT)
    {
      assert(s->close_session_flag
	     || p == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS);

      /* Note: 
       *
       * There are several possible races here.
       *
       * 1) It's possible we get a SOL packet before we get a activate
       * payload response.  For example, the packets are received out
       * of order, or perhaps the activate payload response is lost on
       * the network.
       *
       * If this happens, SOL packets will be thrown out.  We will not
       * accept an SOL packet until the activate payload stage has
       * been fully completed.
       *
       * 2) Between the activation status stage and the activate
       * payload stage, it's possible a different user has established
       * an SOL session.  
       *
       * This will be checked and handled appropriately.
       */

      if (s->close_session_flag)
        {
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }

      if ((ret = _check_sol_activated2(c)) < 0)
        {
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
        }

      if (ret)
	{
	  IPMICONSOLE_CTX_DEBUG(c, ("activate payload race"));
	  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ) < 0)
	    {
	      s->close_session_flag++;
	      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
		goto close_session;
	      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
	      goto calculate_timeout;
	    }
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT;
	  goto calculate_timeout;
	}

      if ((ret = _check_payload_sizes_legitimate(c)) < 0)
        {
	  s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }
      
      if (!ret)
        {
          c->errnum = IPMICONSOLE_ERR_BMC_IMPLEMENTATION;
	  s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }
      
      if ((ret = _check_try_new_port(c)) < 0)
        {
	  s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }
          
      if (ret)
        {
          /* XXX: Supposed to deactivate?  I don't know.  Come back later when you
           * figure out proper behavior?
           */

          IPMICONSOLE_CTX_DEBUG(c, ("trying new port: %X", s->console_port));
          s->try_new_port_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            {
              s->close_session_flag++;
	      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
		goto close_session;
	      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
	      goto calculate_timeout;
            }
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }
     
      if ((perr = pthread_mutex_lock(&(c->blocking_mutex))) != 0)
        {
          IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;

          /* Attempt to close the session cleanly */
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }

      /* Wake up code waiting for SOL to be established */
      if (c->blocking_submit_requested)
        {
          uint8_t val;
          
          c->sol_session_established++;

          val = IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_ESTABLISHED;
          if (write(c->blocking_notification[1], &val, 1) < 0)
            {
              if ((perr = pthread_mutex_lock(&(c->blocking_mutex))) != 0)
                IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));

              IPMICONSOLE_CTX_DEBUG(c, ("write: %s", strerror(errno)));
              c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
              /* Attempt to close the session cleanly */
              s->close_session_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }
        }

      if ((perr = pthread_mutex_unlock(&(c->blocking_mutex))) != 0)
        {
          IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;

          /* Attempt to close the session cleanly */
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }

      if ((perr = pthread_mutex_lock(&(c->status_mutex))) != 0)
        {
          IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;

          /* Attempt to close the session cleanly */
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }

      c->status = IPMICONSOLE_CONTEXT_STATUS_SOL_ESTABLISHED;

      if ((perr = pthread_mutex_unlock(&(c->status_mutex))) != 0)
        {
          IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;

          /* Attempt to close the session cleanly */
          s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        } 

      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION;

      if (c->config.engine_flags & IPMICONSOLE_ENGINE_OUTPUT_ON_SOL_ESTABLISHED)
        {
          int n;
          int dropped;
          int secure_malloc_flag;

          secure_malloc_flag = (c->config.security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY) ? 1 : 0;

          n = cbuf_write(s->console_bmc_to_remote_console,
                         "\0",
                         1,
                         &dropped,
                         secure_malloc_flag);

          if (n < 0)
            {
              IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: %s", strerror(errno)));
              c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;

              /* Attempt to close the session cleanly */
              s->close_session_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }

          if (n != 1)
            {
              IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: invalid bytes written; n=%d", n));
              c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;

              /* Attempt to close the session cleanly */
              s->close_session_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }

          if (dropped)
            {
              IPMICONSOLE_CTX_DEBUG(c, ("cbuf_write: dropped data: dropped=%d", dropped));
              c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;

              /* Attempt to close the session cleanly */
              s->close_session_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }
        }

      /* It's possible the user entered some data before the SOL
       * session was established.  We send that data now.  Otherwise
       * we'd have to wait until the next poll() has passed to
       * ipmiconsole_process_ctxs() is called.
       * 
       * TODO: This is identical to code above.  Clean this up later.
       */

      /* _send_sol_packet_with_character_data() will not send more
       * than s->console_remote_console_to_bmc_bytes_before_break
       */
      if (!cbuf_is_empty(s->console_remote_console_to_bmc)
          && (!s->break_requested
              || (s->break_requested && s->console_remote_console_to_bmc_bytes_before_break)))
        {
          if (_send_sol_packet_with_character_data(c, 0, 0, 0) < 0)
            {
              /* Attempt to close the session cleanly */
              s->close_session_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }
          goto calculate_timeout;
        }
      
      if (s->break_requested)
        {
          if (_send_sol_packet_generate_break(c, 0) < 0)
            {
              /* Attempt to close the session cleanly */
              s->close_session_flag++;
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                goto close_session;
              s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }
          goto calculate_timeout;
        }
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION)
    {
      /* Note: Under this protocol state, we can receive one of two
       * packet types.  The packet will either be a SOL packet or a
       * IPMI "ping" packet that was used to keep the session alive.
       * The payload type will determine what type of packet it is.
       */

      if (s->close_session_flag)
        {
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }

      if (p == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS)
        {
          if (_sol_bmc_to_remote_console_packet(c) < 0)
            {
              s->close_session_flag++;
	      if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
		goto close_session;
	      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
	      goto calculate_timeout;
            }
        }
      else if (p == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS)
        {
          /* Don't have to do anything, yippee that the packet was received */
        }
      else
        {
          IPMICONSOLE_CTX_DEBUG(c, ("invalid packet received: p = %d", p));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
	  s->close_session_flag++;
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
          goto calculate_timeout;
        }
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT)
    {
      assert(s->close_session_flag
	     || p == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS);

      if (c->config.security_flags & IPMICONSOLE_SECURITY_DEACTIVATE_ONLY)
        s->deactivate_only_succeeded_flag++;

      if (s->close_session_flag)
        {
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
            goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
          goto calculate_timeout;
        }
      else if (s->deactivate_payload_instances_and_try_again_flag)
        {
          s->sol_instances_deactivated_count++;
          if (s->sol_instances_activated_count == s->sol_instances_deactivated_count)
            {
              s->deactivate_payload_instances_and_try_again_flag = 0;
	      s->deactivate_active_payloads_count++;
              
	      /* IPMI Workaround
	       * 
	       * Supermicro IPMI 2.0 implementations may not
	       * deactivate paylods properly and signal that a proper
	       * deativation occurred.  This leads to the state
	       * machine looping forever:
	       *  
	       * - Get Activation Status says SOL is activated
	       * - Deactivate Payloads tries to deactivate SOL
	       *   - deactivation fails, but command returns success
	       * - Activate Payload says it can't activate SOL b/c
               *   it's already activated
	       *
	       * And the loop re-begins.  Therefore the need for this
	       * workaround.
	       */ 
	      
	      /* +1 b/c one deactivate_active_payloads_count is acceptable and expected */
	      if (s->deactivate_active_payloads_count > c->config.acceptable_packet_errors_count + 1)
		{
                  /* achu:
                   *
                   * I've been going back and forth on what this error
                   * code should actually be.  It is conceivable that
                   * this occurs b/c two different libipmiconsole()
                   * threads are attempting to get the same SOL
                   * session going, and they are "blocking" each
                   * other.
                   *
                   * For now, we will assume that the above Supermicro 
                   * issue or something similar is the real problem and it
                   * is a flaw due to the implementation of the BMC.
                   * 
                   */
		  IPMICONSOLE_CTX_DEBUG(c, ("closing with excessive payload deactivations"));
		  c->errnum = IPMICONSOLE_ERR_BMC_IMPLEMENTATION;
		  s->close_session_flag++;
		  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
		    goto close_session;
		  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
		  goto calculate_timeout;
		}

              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ) < 0)
                {
                  s->close_session_flag++;
		  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
		    goto close_session;
		  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
		  goto calculate_timeout;
                }
	      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }
          else
            {
              if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ) < 0)
                {
                  s->close_session_flag++;
		  if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) < 0)
		    goto close_session;
		  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT;
		  goto calculate_timeout;
                }
	      s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT;
              goto calculate_timeout;
            }
        }
     
      /* Shouldn't be possible to reach this point */
      IPMICONSOLE_CTX_DEBUG(c, ("deactivate payload logic bug"));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto close_session;
    }
  else if (s->protocol_state == IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT)
    {
      assert(s->close_session_flag
	     || s->close_timeout_flag
	     || p == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS);

      if (s->close_session_flag)
        {
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_END;
          goto close_session;
        }

      if (s->close_timeout_flag)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("closing session via close session packet timeout"));
          s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_END;
          goto close_session;
        }

      if (s->try_new_port_flag)
        {
          /* Yippee, we get to start over! */
          assert(s->console_port != RMCP_PRIMARY_RMCP_PORT);

          s->try_new_port_flag = 0;

          if (_ipmiconsole_ctx_session_maintenance_information_setup(c) < 0)
	    /* Session is closed, just exit on error */
	    goto close_session;
          
          if (_send_ipmi_packet(c, IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RQ) < 0)
	    /* Session is closed, just exit on error */
	    goto close_session;
	  s->protocol_state = IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT;
          goto calculate_timeout;
        }
    }
  else
    {
      IPMICONSOLE_CTX_DEBUG(c, ("invalid protocol_state: %d", s->protocol_state));
      goto close_session;
    }
    
 calculate_timeout:
  if (_calculate_timeout(c, timeout) < 0)
    goto close_session;
  rv = 0;
 close_session:
  return rv;
}

int
ipmiconsole_process_ctxs(List console_engine_ctxs, unsigned int *timeout)
{
  ListIterator itr = NULL;
  ipmiconsole_ctx_t c;
  int ctxs_count = 0;
  unsigned int min_timeout = UINT_MAX;
  int rv = -1;

  assert(console_engine_ctxs);
  assert(timeout);

  *timeout = 0;

  if (!list_count(console_engine_ctxs))
    return 0;

  if (!(itr = list_iterator_create(console_engine_ctxs)))
    {
      IPMICONSOLE_DEBUG(("list_iterator_create: %s", strerror(errno)));
      goto cleanup;
    }

  while ((c = (ipmiconsole_ctx_t)list_next(itr)))
    {
      unsigned int ctx_timeout;

      assert(c);
      assert(c->magic == IPMICONSOLE_CTX_MAGIC);

      if ((_process_ctx(c, &ctx_timeout)) < 0)
        {
          /* On delete, function to cleanup ctx session will be done.
           * Error will be seen by the user via a EOF on a read() or
           * EPIPE on a write().
           */
          if (!list_delete(itr))
            {
              IPMICONSOLE_DEBUG(("list_delete: %s", strerror(errno)));
              goto cleanup;
            }

          continue;
        }

      if (ctx_timeout < min_timeout)
        min_timeout = ctx_timeout;
      ctxs_count++;
    }
  
  rv = ctxs_count;
  if (rv)
    *timeout = min_timeout;
 cleanup:
  if (itr)
    list_iterator_destroy(itr);
  return rv;
}
