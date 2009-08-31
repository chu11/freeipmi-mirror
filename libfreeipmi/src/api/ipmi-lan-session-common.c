/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/socket.h>
#include <netinet/in.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif  /* !TIME_WITH_SYS_TIME */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"
#include "freeipmi/util/ipmi-lan-util.h"
#include "freeipmi/util/ipmi-rmcpplus-util.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"
#include "ipmi-lan-session-common.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

#define IPMI_LAN_BACKOFF_COUNT         2

#define IPMI_SEQUENCE_NUMBER_WINDOW    8
#define IPMI_MAX_SEQUENCE_NUMBER       0xFFFFFFFF

struct socket_to_close {
  int fd;
  struct socket_to_close *next;
};

void
ipmi_lan_cmd_get_session_parameters (ipmi_ctx_t ctx,
				     uint8_t *authentication_type,
				     uint32_t *internal_workaround_flags)
{
  assert(ctx 
         && ctx->magic == IPMI_CTX_MAGIC
         && ctx->type == IPMI_DEVICE_LAN
	 && authentication_type
	 && internal_workaround_flags);
  
  (*authentication_type) = IPMI_AUTHENTICATION_TYPE_NONE;
  (*internal_workaround_flags) = 0;
  
  if (ctx->io.outofband.per_msg_auth_disabled)
    {
      (*authentication_type) = IPMI_AUTHENTICATION_TYPE_NONE;
      if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
        (*internal_workaround_flags) |= IPMI_LAN_INTERNAL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
    }
  else
    (*authentication_type) = ctx->io.outofband.authentication_type;
}

void
ipmi_lan_2_0_cmd_get_session_parameters (ipmi_ctx_t ctx,
					 uint8_t *payload_authenticated,
					 uint8_t *payload_encrypted)
{
  assert(ctx 
         && ctx->magic == IPMI_CTX_MAGIC
         && ctx->type == IPMI_DEVICE_LAN_2_0
	 && payload_authenticated
	 && payload_encrypted);
  
  if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    (*payload_authenticated) = IPMI_PAYLOAD_FLAG_UNAUTHENTICATED;
  else
    (*payload_authenticated) = IPMI_PAYLOAD_FLAG_AUTHENTICATED;

  if (ctx->io.outofband.confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    (*payload_encrypted) = IPMI_PAYLOAD_FLAG_UNENCRYPTED;
  else
    (*payload_encrypted) = IPMI_PAYLOAD_FLAG_ENCRYPTED;
}

static int
_session_timed_out(ipmi_ctx_t ctx)
{
  struct timeval current;
  struct timeval session_timeout;
  struct timeval session_timeout_len;

  assert(ctx 
         && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0));
  
  session_timeout_len.tv_sec = ctx->io.outofband.session_timeout / 1000;
  session_timeout_len.tv_usec = (ctx->io.outofband.session_timeout - (session_timeout_len.tv_sec * 1000)) * 1000;
  timeradd(&(ctx->io.outofband.last_received), &session_timeout_len, &session_timeout);

  API_ERR (!(gettimeofday(&current, NULL) < 0));

  return timercmp(&current, &session_timeout, >);
}

static int
_calculate_timeout(ipmi_ctx_t ctx, 
                   unsigned int retransmission_count,
                   struct timeval *timeout)
{
  struct timeval current;
  struct timeval session_timeout;
  struct timeval session_timeout_len;
  struct timeval session_timeout_val;
  struct timeval retransmission_timeout;
  struct timeval retransmission_timeout_len;
  struct timeval retransmission_timeout_val;
  unsigned int retransmission_timeout_multiplier;

  assert(ctx 
	 && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0)
	 && timeout);

  API_ERR (!(gettimeofday(&current, NULL) < 0));

  session_timeout_len.tv_sec = ctx->io.outofband.session_timeout / 1000;
  session_timeout_len.tv_usec = (ctx->io.outofband.session_timeout - (session_timeout_len.tv_sec * 1000)) * 1000;

  timeradd(&current, &session_timeout_len, &session_timeout);
  timersub(&session_timeout, &current, &session_timeout_val);

  retransmission_timeout_multiplier = (retransmission_count / IPMI_LAN_BACKOFF_COUNT) + 1;
  
  retransmission_timeout_len.tv_sec = (retransmission_timeout_multiplier * ctx->io.outofband.retransmission_timeout) / 1000;
  retransmission_timeout_len.tv_usec = ((retransmission_timeout_multiplier * ctx->io.outofband.retransmission_timeout) - (retransmission_timeout_len.tv_sec * 1000)) * 1000;

  timeradd(&ctx->io.outofband.last_send, &retransmission_timeout_len, &retransmission_timeout);
  timersub(&retransmission_timeout, &current, &retransmission_timeout_val);

  if (timercmp(&retransmission_timeout_val, &session_timeout_val, <))
    {
      timeout->tv_sec = retransmission_timeout_val.tv_sec;
      timeout->tv_usec = retransmission_timeout_val.tv_usec;
    }
  else
    {
      timeout->tv_sec = session_timeout_val.tv_sec;
      timeout->tv_usec = session_timeout_val.tv_usec;
    }

  return 0;
}

static void
_ipmi_lan_dump_rq (ipmi_ctx_t ctx, 
                   uint8_t *pkt,
                   uint32_t pkt_len,
		   uint8_t cmd,
		   uint8_t net_fn,
                   fiid_obj_t obj_cmd_rq)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && pkt
          && pkt_len
	  && fiid_obj_valid(obj_cmd_rq));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template(obj_cmd_rq)))
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd(DEBUG_UTIL_TYPE_IPMI_1_5,
                    DEBUG_UTIL_DIRECTION_REQUEST,
                    net_fn,
                    cmd,
                    hdrbuf,
                    DEBUG_UTIL_HDR_BUFLEN);

      if (ctx->tmpl_ipmb_cmd_rq)
        ipmi_dump_lan_packet_ipmb (STDERR_FILENO,
                                   ctx->io.outofband.hostname,
                                   hdrbuf,
                                   NULL,
                                   pkt,
                                   pkt_len,
                                   tmpl_lan_msg_hdr_rq,
                                   tmpl_cmd,
                                   tmpl_ipmb_msg_hdr_rq,
                                   ctx->tmpl_ipmb_cmd_rq);
      else
        ipmi_dump_lan_packet (STDERR_FILENO,
                              ctx->io.outofband.hostname,
                              hdrbuf,
                              NULL,
                              pkt,
                              pkt_len,
                              tmpl_lan_msg_hdr_rq,
                              tmpl_cmd);

      fiid_template_free (tmpl_cmd);
    }
}

static void
_ipmi_lan_dump_rs (ipmi_ctx_t ctx, 
                   uint8_t *pkt,
                   uint32_t pkt_len,
		   uint8_t cmd,
		   uint8_t net_fn,
                   fiid_obj_t obj_cmd_rs)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && pkt
          && pkt_len
	  && fiid_obj_valid(obj_cmd_rs));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template(obj_cmd_rs)))
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd(DEBUG_UTIL_TYPE_IPMI_1_5,
                    DEBUG_UTIL_DIRECTION_RESPONSE,
                    net_fn,
                    cmd,
                    hdrbuf,
                    DEBUG_UTIL_HDR_BUFLEN);

      ipmi_dump_lan_packet (STDERR_FILENO,
                            ctx->io.outofband.hostname,
                            hdrbuf,
                            NULL,
                            pkt,
                            pkt_len,
                            tmpl_lan_msg_hdr_rs,
                            tmpl_cmd);

      fiid_template_free (tmpl_cmd);
    }
}

static int8_t
_ipmi_check_session_sequence_number(ipmi_ctx_t ctx,
                                    uint32_t session_sequence_number)
{
  uint32_t shift_num, wrap_val;
  int8_t rv = 0;

  /* achu: This algorithm is more or less from Appendix A of the IPMI
   * spec.  It may not be entirely necessary, since the requester
   * sequence number puts packets into lock-step mode.  Oh well.
   *
   * I know that technically I could remove a lot of code here if I
   * just let unsigned ints be unsigned ints (i.e. 0x00 - 0xff = 1).
   * I dunno, I like to see all of the code actually written out b/c
   * it makes more sense to the casual code reviewer.  Maybe I'll
   * change it later.
   */

  assert(ctx 
         && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0));

  /* IPMI Workaround (achu)
   *
   * Discovered on Sun Fire 4100.
   *
   * The session sequence numbers for IPMI 1.5 are the wrong endian.
   * So we have to flip the bits to workaround it.
   */
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
    {
      uint32_t tmp_session_sequence_number = session_sequence_number;

      session_sequence_number =
        ((tmp_session_sequence_number & 0xFF000000) >> 24)
        | ((tmp_session_sequence_number & 0x00FF0000) >> 8)
        | ((tmp_session_sequence_number & 0x0000FF00) << 8)
        | ((tmp_session_sequence_number & 0x000000FF) << 24);
    }

  /* Drop duplicate packet */
  if (session_sequence_number == ctx->io.outofband.highest_received_sequence_number)
    goto out;
  
  /* In IPMI 2.0, sequence number 0 is special, and shouldn't happen */
  if (ctx->type == IPMI_DEVICE_LAN_2_0 && session_sequence_number == 0)
    goto out;

  /* Check if sequence number is greater than highest received and is
   * within range
   */
  if (ctx->io.outofband.highest_received_sequence_number > (IPMI_MAX_SEQUENCE_NUMBER - IPMI_SEQUENCE_NUMBER_WINDOW))
    {
      wrap_val = IPMI_SEQUENCE_NUMBER_WINDOW - (IPMI_MAX_SEQUENCE_NUMBER - ctx->io.outofband.highest_received_sequence_number) - 1;

      /* In IPMI 2.0, sequence number 0 isn't possible, so adjust wrap_val */
      if (ctx->type == IPMI_DEVICE_LAN_2_0)
        wrap_val++;
      
      if (session_sequence_number > ctx->io.outofband.highest_received_sequence_number || session_sequence_number <= wrap_val)
        {
          if (session_sequence_number > ctx->io.outofband.highest_received_sequence_number && session_sequence_number <= IPMI_MAX_SEQUENCE_NUMBER)
            shift_num = session_sequence_number - ctx->io.outofband.highest_received_sequence_number;
          else
            {
              if (ctx->type == IPMI_DEVICE_LAN)
                shift_num = session_sequence_number + (IPMI_MAX_SEQUENCE_NUMBER - ctx->io.outofband.highest_received_sequence_number) + 1;
              else
                /* IPMI 2.0 Special Case b/c 0 isn't a legit sequence number */
                shift_num = session_sequence_number + (IPMI_MAX_SEQUENCE_NUMBER - ctx->io.outofband.highest_received_sequence_number);
            }

          ctx->io.outofband.highest_received_sequence_number = session_sequence_number;
          ctx->io.outofband.previously_received_list <<= shift_num;
          ctx->io.outofband.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }
  else
    {
      if (session_sequence_number > ctx->io.outofband.highest_received_sequence_number
          && (session_sequence_number - ctx->io.outofband.highest_received_sequence_number) <= IPMI_SEQUENCE_NUMBER_WINDOW)
        {
          shift_num = (session_sequence_number - ctx->io.outofband.highest_received_sequence_number);
          ctx->io.outofband.highest_received_sequence_number = session_sequence_number;
          ctx->io.outofband.previously_received_list <<= shift_num;
          ctx->io.outofband.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }

  /* Check if sequence number is lower than highest received, is
   * within range, and hasn't been seen yet
   */
  if (ctx->io.outofband.highest_received_sequence_number < IPMI_SEQUENCE_NUMBER_WINDOW)
    {
      uint32_t wrap_val = IPMI_MAX_SEQUENCE_NUMBER - (IPMI_SEQUENCE_NUMBER_WINDOW - ctx->io.outofband.highest_received_sequence_number) + 1;
      
      /* In IPMI 2.0, sequence number 0 isn't possible, so adjust wrap_val */
      if (ctx->type == IPMI_DEVICE_LAN_2_0)
        wrap_val--;

      if (session_sequence_number < ctx->io.outofband.highest_received_sequence_number || session_sequence_number >= wrap_val)
        {
          if (session_sequence_number > ctx->io.outofband.highest_received_sequence_number && session_sequence_number <= IPMI_MAX_SEQUENCE_NUMBER)
            {
              if (ctx->type == IPMI_DEVICE_LAN)
                shift_num = ctx->io.outofband.highest_received_sequence_number + (IPMI_MAX_SEQUENCE_NUMBER - session_sequence_number) + 1;
              else
                /* IPMI 2.0 Special Case b/c 0 isn't a legit sequence number */
                shift_num = ctx->io.outofband.highest_received_sequence_number + (IPMI_MAX_SEQUENCE_NUMBER - session_sequence_number);
            }
          else
            shift_num = ctx->io.outofband.highest_received_sequence_number - session_sequence_number;
          
          /* Duplicate packet check*/
          if (ctx->io.outofband.previously_received_list & (0x1 << (shift_num - 1)))
            goto out;
          
          ctx->io.outofband.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }
  else
    {
      if (session_sequence_number < ctx->io.outofband.highest_received_sequence_number
          && session_sequence_number >= (ctx->io.outofband.highest_received_sequence_number - IPMI_SEQUENCE_NUMBER_WINDOW))
        {
          shift_num = ctx->io.outofband.highest_received_sequence_number - session_sequence_number;
          
          /* Duplicate packet check*/
          if (ctx->io.outofband.previously_received_list & (0x1 << (shift_num - 1)))
            goto out;

          ctx->io.outofband.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }

 out:
  return rv;
}
                                        
static int8_t
_ipmi_lan_cmd_send (ipmi_ctx_t ctx,
                    uint8_t lun,
                    uint8_t net_fn,
                    uint8_t authentication_type,
                    uint32_t session_sequence_number,
                    uint32_t session_id,
                    uint8_t rq_seq,
                    char *password,
                    uint32_t password_len,
		    uint8_t cmd, /* for debug dumping */
                    fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt;
  int32_t pkt_len = 1024;
  int32_t send_len = 0;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0)
	 && ctx->io.outofband.sockfd 
         && IPMI_BMC_LUN_VALID(lun)
         && IPMI_NET_FN_VALID(net_fn)
         && IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type)
         && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq));

  API_FIID_OBJ_CLEAR(ctx->io.outofband.rq.obj_rmcp_hdr);
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rq.obj_lan_session_hdr);
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rq.obj_lan_msg_hdr);
    
  pkt = alloca (pkt_len);
  API_ERR (pkt);
  memset (pkt, 0, pkt_len);
    
  API_ERR (fill_rmcp_hdr_ipmi (ctx->io.outofband.rq.obj_rmcp_hdr) != -1);

  API_ERR (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
			     net_fn,
                             lun,
			     rq_seq,
			     ctx->io.outofband.rq.obj_lan_msg_hdr) != -1);

  API_ERR (fill_lan_session_hdr (authentication_type,
				 session_sequence_number,
                                 session_id,
				 ctx->io.outofband.rq.obj_lan_session_hdr) != -1);
  API_ERR ((send_len = assemble_ipmi_lan_pkt (ctx->io.outofband.rq.obj_rmcp_hdr,
					      ctx->io.outofband.rq.obj_lan_session_hdr,
					      ctx->io.outofband.rq.obj_lan_msg_hdr,
					      obj_cmd_rq,
                                              (uint8_t *)password,
                                              password_len,
					      pkt,
					      pkt_len)) != -1);

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && send_len)
    _ipmi_lan_dump_rq (ctx, pkt, send_len, cmd, net_fn, obj_cmd_rq);

  API_ERR (!(ipmi_lan_sendto (ctx->io.outofband.sockfd, 
			      pkt, 
			      send_len, 
			      0, 
			      (struct sockaddr *)&(ctx->io.outofband.remote_host), 
			      sizeof(struct sockaddr_in)) < 0));

  API_ERR (!(gettimeofday(&ctx->io.outofband.last_send, NULL) < 0));
  
  return (0);
}

static int8_t
_ipmi_lan_cmd_recv (ipmi_ctx_t ctx, 
                    uint8_t *pkt,
                    uint32_t pkt_len,
                    unsigned int retransmission_count,
		    uint8_t cmd, /* for debug dumping */
		    uint8_t net_fn, /* for debug dumping */
                    fiid_obj_t obj_cmd_rs)
{
  struct sockaddr_in from;
  socklen_t fromlen = 0;
  struct timeval timeout;
  fd_set read_set;
  int status = 0;
  int32_t recv_len;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0)
	 && ctx->io.outofband.sockfd 
         && pkt
         && pkt_len
	 && fiid_obj_valid(obj_cmd_rs));  
  
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_rmcp_hdr); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_lan_session_hdr); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_lan_msg_hdr); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_lan_msg_trlr); 

  if (ctx->io.outofband.retransmission_timeout != 0)
    {
      FD_ZERO (&read_set);
      FD_SET (ctx->io.outofband.sockfd, &read_set);
      
      API_ERR (!(_calculate_timeout(ctx, retransmission_count, &timeout) < 0));
      
      API_ERR (!((status = select ((ctx->io.outofband.sockfd + 1), 
				   &read_set,
				   NULL,
				   NULL, 
				   &timeout)) < 0));
      if (status == 0)
        return (0); /* resend the request */
    }

  API_ERR (!((recv_len = ipmi_lan_recvfrom (ctx->io.outofband.sockfd, 
					    pkt, 
					    pkt_len, 
					    0, 
					    (struct sockaddr *) &from, 
					    &fromlen)) < 0));
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && recv_len)
    _ipmi_lan_dump_rs (ctx, 
                       pkt,
                       recv_len,
		       cmd,
		       net_fn,
                       obj_cmd_rs);

  API_ERR (unassemble_ipmi_lan_pkt(pkt,
				   recv_len,
 				   ctx->io.outofband.rs.obj_rmcp_hdr,
				   ctx->io.outofband.rs.obj_lan_session_hdr,
				   ctx->io.outofband.rs.obj_lan_msg_hdr,
				   obj_cmd_rs,
				   ctx->io.outofband.rs.obj_lan_msg_trlr) != -1);

  return (recv_len);
}

/* < 0 - error
 * == 1 good packet
 * == 0 bad packet
 */
static int8_t
_ipmi_lan_cmd_wrapper_verify_packet (ipmi_ctx_t ctx,
				     uint8_t internal_workaround_flags,
				     uint8_t authentication_type,
				     uint32_t *session_sequence_number,
				     uint32_t session_id,
				     uint8_t *rq_seq,
				     char *password,
				     uint32_t password_len,
				     fiid_obj_t obj_cmd_rs)
{
  uint64_t rs_session_id;
  uint64_t rs_session_sequence_number;
  int8_t rv = -1;
  int ret;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0)
	 && ctx->io.outofband.sockfd
         && IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type)
         && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
         && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_lan_session_hdr, "session_id", &rs_session_id);

  if (session_id != rs_session_id)
    {
      /* IPMI Workaround (achu)
       *
       * Discovered on Tyan S2882 w/ m3289 BMC
       *
       * The remote BMC returns zeroes for the session id instead of the
       * actual session id.  To work around this problem, we'll assume the
       * session id is correct if it is equal to zero.
       */
      if ((ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO)
	  && !rs_session_id)
	/* you get a second chance - continue on checking */
	;
      else
	{
	  rv = 0;
	  goto cleanup;
	}
    }

  API_ERR_CLEANUP (!((ret = ipmi_lan_check_checksum (ctx->io.outofband.rs.obj_lan_msg_hdr,
						     obj_cmd_rs,
						     ctx->io.outofband.rs.obj_lan_msg_trlr)) < 0));
  
  if (!ret)
    {
      rv = 0;
      goto cleanup;
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

  API_ERR_CLEANUP (!((ret = ipmi_lan_check_session_authentication_code (ctx->io.outofband.rs.obj_lan_session_hdr,
									ctx->io.outofband.rs.obj_lan_msg_hdr,
									obj_cmd_rs,
									ctx->io.outofband.rs.obj_lan_msg_trlr,
									authentication_type,
									(uint8_t *)password,
									password_len)) < 0));

  if ((internal_workaround_flags & IPMI_LAN_INTERNAL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
      && !ret)
    {
      API_ERR_CLEANUP (!((ret = ipmi_lan_check_session_authentication_code (ctx->io.outofband.rs.obj_lan_session_hdr,
									    ctx->io.outofband.rs.obj_lan_msg_hdr,
									    obj_cmd_rs,
									    ctx->io.outofband.rs.obj_lan_msg_trlr,
									    ctx->io.outofband.authentication_type,
									    (uint8_t *)password,
									    password_len)) < 0));
    }
  
  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }
  
  if (session_sequence_number)
    {
      API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_lan_session_hdr,
				"session_sequence_number",
				&rs_session_sequence_number);
      
      API_ERR_CLEANUP (!((ret = _ipmi_check_session_sequence_number(ctx, 
								    (uint32_t)rs_session_sequence_number)) < 0));
      
      if (!ret)
	{
	  rv = 0;
	  goto cleanup;
	}
    }
          
  API_ERR_CLEANUP (!((ret = ipmi_lan_check_rq_seq(ctx->io.outofband.rs.obj_lan_msg_hdr, 
						  (rq_seq) ? *rq_seq : 0)) < 0));
  
  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  rv = 1;
 cleanup:
  return (rv);
}

int8_t 
ipmi_lan_cmd_wrapper (ipmi_ctx_t ctx, 
                      uint32_t internal_workaround_flags,
                      uint8_t lun,
                      uint8_t net_fn,
                      uint8_t authentication_type,
                      uint32_t *session_sequence_number,
                      uint32_t session_id,
                      uint8_t *rq_seq,
                      char *password,
                      uint32_t password_len,
                      fiid_obj_t obj_cmd_rq,
                      fiid_obj_t obj_cmd_rs)
{
  int retval = -1;
  int ret;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  int32_t recv_len;
  struct socket_to_close *sockets = NULL;
  uint64_t cmd = 0;		/* used for debugging */
  
  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0)
	 && ctx->io.outofband.sockfd 
         && IPMI_BMC_LUN_VALID(lun)
         && IPMI_NET_FN_VALID(net_fn)
         && IPMI_1_5_AUTHENTICATION_TYPE_VALID(authentication_type)
         && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq)
         && fiid_obj_valid(obj_cmd_rs));

  if (!ctx->io.outofband.last_received.tv_sec
      && !ctx->io.outofband.last_received.tv_usec)
    API_ERR (!(gettimeofday(&ctx->io.outofband.last_received, NULL) < 0));

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    API_FIID_OBJ_GET_NO_RETURN(obj_cmd_rq, "cmd", &cmd);

  if (_ipmi_lan_cmd_send (ctx, 
			  lun, 
			  net_fn,
			  authentication_type,
			  (session_sequence_number) ? *session_sequence_number : 0,
			  session_id,
			  (rq_seq) ? *rq_seq : 0,
			  password,
			  password_len,
			  cmd,	/* for debug dumping */
			  obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      if (_session_timed_out(ctx))
        {
	  API_ERR_SET_ERRNUM (IPMI_ERR_SESSION_TIMEOUT);
          retval = -1;
          break;
        }
     
      /* its ok to use the "request" net_fn, dump code doesn't care */
      memset(pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _ipmi_lan_cmd_recv (ctx, 
                                          pkt,
                                          IPMI_MAX_PKT_LEN,
                                          retransmission_count,			  
					  cmd, /* for debug dumping */
					  net_fn, /* for debug dumping */
                                          obj_cmd_rs)) < 0)
        {
          retval = -1;
          break;
        }

      if (!recv_len)
        {
          if (session_sequence_number)
            (*session_sequence_number)++;
          if (rq_seq)
            *rq_seq = ((*rq_seq) + 1) % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);
          
          retransmission_count++;
          
          /* IPMI Workaround (achu)
           *
           * Discovered on Intel Tiger4 (SR870BN4)
           *
           * If the reply from a previous Get Session Challenge request is
           * lost on the network, the following retransmission will make
           * the BMC confused and it will not respond to future packets.
           *
           * The problem seems to exist only when the retransmitted packet
           * is transmitted from the same source port.  Therefore, the fix
           * is to send the retransmission from a different source port.
           * So we'll create a new socket, re-bind to an ephemereal port
           * (guaranteeing us a brand new port), and store this new
           * socket.
           *
           * In the event we need to resend this packet multiple times, we
           * do not want the chance that old ports will be used again.  We
           * store the old file descriptrs (which are bound to the old
           * ports) on a list, and close all of them after we have gotten
           * past the Get Session Challenge phase of the protocol.
           */
          if (internal_workaround_flags & IPMI_LAN_INTERNAL_WORKAROUND_FLAGS_GET_SESSION_CHALLENGE)
            {
              struct socket_to_close *s;
              struct sockaddr_in addr;

              API_ERR_CLEANUP ((s = (struct socket_to_close *)malloc(sizeof(struct socket_to_close))));
              s->fd = ctx->io.outofband.sockfd;
              s->next = sockets;
              sockets = s;
              
              API_ERR_SYSTEM_ERROR_CLEANUP (!((ctx->io.outofband.sockfd = socket (AF_INET, SOCK_DGRAM, 0)) < 0));
              
              memset (&addr, 0, sizeof (struct sockaddr_in));
              addr.sin_family = AF_INET;
              addr.sin_port = htons (0);
              addr.sin_addr.s_addr = htonl (INADDR_ANY);

              API_ERR_SYSTEM_ERROR_CLEANUP (!(bind(ctx->io.outofband.sockfd,
                                                   (struct sockaddr *)&addr,
                                                   sizeof(struct sockaddr_in)) < 0));
            }

          if (_ipmi_lan_cmd_send (ctx, 
                                  lun,
                                  net_fn,
                                  authentication_type,
                                  (session_sequence_number) ? *session_sequence_number : 0,
                                  session_id,
                                  (rq_seq) ? *rq_seq : 0,
                                  password,
                                  password_len,
				  cmd,	/* for debug dumping */
                                  obj_cmd_rq) < 0)
            goto cleanup;
          
          continue;
        }

      /* else received a packet */

      if ((ret = _ipmi_lan_cmd_wrapper_verify_packet (ctx,
						      internal_workaround_flags,
						      authentication_type,
						      session_sequence_number,
						      session_id,
						      rq_seq,
						      password,
						      password_len,
						      obj_cmd_rs)) < 0)
	goto cleanup;
      
      if (!ret)
	continue;

      API_ERR_CLEANUP (!(gettimeofday(&(ctx->io.outofband.last_received), NULL) < 0));
      retval = recv_len;
      break;
    }
  
 cleanup:
  if (session_sequence_number)
    (*session_sequence_number)++;
  if (rq_seq)
    *rq_seq = ((*rq_seq) + 1) % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);
  while (sockets)
    {
      close(sockets->fd);
      sockets = sockets->next;
    }
  return (retval);
}

int8_t
_ipmi_cmd_send_ipmb (ipmi_ctx_t ctx,
		     uint8_t rs_addr,
		     uint8_t lun,
		     uint8_t net_fn,
		     fiid_obj_t obj_cmd_rq)
{
  uint8_t tbuf[IPMI_MAX_PKT_LEN];
  fiid_obj_t obj_ipmb_msg_hdr_rq = NULL;
  fiid_obj_t obj_ipmb_msg_rq = NULL;
  fiid_obj_t obj_send_cmd_rs = NULL;
  int8_t rv = -1;
  int32_t len;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
	 && (ctx->type == IPMI_DEVICE_LAN
	     || ctx->type == IPMI_DEVICE_LAN_2_0)
	 && ctx->io.outofband.sockfd 
	 && IPMI_BMC_LUN_VALID(lun)
	 && IPMI_NET_FN_VALID(net_fn)
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq));

  API_FIID_OBJ_CREATE_CLEANUP(obj_ipmb_msg_hdr_rq, tmpl_ipmb_msg_hdr_rq);
  API_FIID_OBJ_CREATE_CLEANUP(obj_ipmb_msg_rq, tmpl_ipmb_msg);
  API_FIID_OBJ_CREATE_CLEANUP(obj_send_cmd_rs, tmpl_cmd_send_message_rs);

  API_ERR_CLEANUP (fill_ipmb_msg_hdr (rs_addr,
				      net_fn,
				      lun,
				      IPMI_SLAVE_ADDRESS_BMC,
				      IPMI_BMC_IPMB_LUN_SMS_MSG_LUN,
				      ctx->io.outofband.rq_seq,
				      obj_ipmb_msg_hdr_rq) != -1);

  API_ERR_CLEANUP (assemble_ipmi_ipmb_msg (obj_ipmb_msg_hdr_rq,
					   obj_cmd_rq,
					   obj_ipmb_msg_rq) != -1);

  memset(tbuf, '\0', IPMI_MAX_PKT_LEN);
  API_FIID_OBJ_GET_ALL_LEN_CLEANUP (len,
				    obj_ipmb_msg_rq,
				    tbuf,
				    IPMI_MAX_PKT_LEN);
  

  if (ipmi_cmd_send_message (ctx,
			     IPMI_CHANNEL_NUMBER_PRIMARY_IPMB,
			     IPMI_SEND_MESSAGE_AUTHENTICATION_NOT_REQUIRED,
			     IPMI_SEND_MESSAGE_ENCRYPTION_NOT_REQUIRED,
			     IPMI_SEND_MESSAGE_TRACKING_OPERATION_TRACKING_REQUEST,
			     tbuf,
			     len,
			     obj_send_cmd_rs) < 0)
    {
      API_BAD_COMPLETION_CODE_TO_API_ERRNUM(ctx, obj_send_cmd_rs);
      goto cleanup;
    }

  /* reset to original, would have been changed in send_message call */
  ctx->lun = lun;
  ctx->net_fn = net_fn;

  /* "pretend" a request was just sent */
  API_ERR_CLEANUP (!(gettimeofday(&ctx->io.outofband.last_send, NULL) < 0));

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_ipmb_msg_hdr_rq);
  API_FIID_OBJ_DESTROY(obj_ipmb_msg_rq);
  API_FIID_OBJ_DESTROY(obj_send_cmd_rs);
  return (rv);
}

int8_t 
ipmi_lan_cmd_wrapper_ipmb (ipmi_ctx_t ctx, 
			   fiid_obj_t obj_cmd_rq,
			   fiid_obj_t obj_cmd_rs)
{
  int retval = -1;
  int ret;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  int32_t recv_len;
  uint64_t cmd = 0;		/* used for debugging */
  uint8_t rq_seq_orig;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0)
	 && ctx->io.outofband.sockfd 
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq)
         && fiid_obj_valid(obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    API_FIID_OBJ_GET_NO_RETURN(obj_cmd_rq, "cmd", &cmd);

  /* for debugging */
  ctx->tmpl_ipmb_cmd_rq = fiid_obj_template(obj_cmd_rq);
  ctx->tmpl_ipmb_cmd_rs = fiid_obj_template(obj_cmd_rs);

  /* ipmb response packet will use the request sequence number from
   * the earlier packet.  Save it for verification.
   */

  rq_seq_orig = ctx->io.outofband.rq_seq;

  if (_ipmi_cmd_send_ipmb (ctx,
			   ctx->rs_addr,
			   ctx->lun,
			   ctx->net_fn,
			   obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      uint8_t authentication_type;
      uint32_t internal_workaround_flags = 0;

      if (_session_timed_out(ctx))
        {
	  API_ERR_SET_ERRNUM (IPMI_ERR_SESSION_TIMEOUT);
          retval = -1;
          break;
        }
     
      /* its ok to use the "request" net_fn, dump code doesn't care */
      memset(pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _ipmi_lan_cmd_recv (ctx, 
                                          pkt,
                                          IPMI_MAX_PKT_LEN,
                                          retransmission_count,			  
					  cmd, /* for debug dumping */
					  ctx->net_fn, /* for debug dumping */
                                          obj_cmd_rs)) < 0)
        {
          retval = -1;
          break;
        }

      if (!recv_len)
        {
          retransmission_count++;

	  /* don't increment sequence numbers, will be done in _ipmi_cmd_send_ipmb */
          
	  /* ipmb response packet will use the request sequence number from
	   * the earlier packet.  Save it for verification.
	   */

	  rq_seq_orig = ctx->io.outofband.rq_seq;

	  API_ERR_CLEANUP (!(_ipmi_cmd_send_ipmb (ctx,
						  ctx->rs_addr,
						  ctx->lun,
						  ctx->net_fn,
						  obj_cmd_rq) < 0));

          continue;
        }

      /* else received a packet */

      ipmi_lan_cmd_get_session_parameters (ctx,
					   &authentication_type,
					   &internal_workaround_flags);

      if ((ret = _ipmi_lan_cmd_wrapper_verify_packet (ctx,
						      internal_workaround_flags,
						      authentication_type,
						      &(ctx->io.outofband.session_sequence_number),
						      ctx->io.outofband.session_id,
						      &rq_seq_orig,
						      ctx->io.outofband.password,
						      IPMI_1_5_MAX_PASSWORD_LENGTH,
						      obj_cmd_rs)) < 0)
	goto cleanup;
	
      if (!ret)
	continue;

      API_ERR_CLEANUP (!(gettimeofday(&(ctx->io.outofband.last_received), NULL) < 0));
      retval = recv_len;
      break;
    }
  
 cleanup:
  ctx->io.outofband.session_sequence_number++;
  /* rq_seq already incremented via _ipmi_cmd_send_ipmb call */
  API_FIID_TEMPLATE_FREE (ctx->tmpl_ipmb_cmd_rq);
  ctx->tmpl_ipmb_cmd_rq = NULL;
  API_FIID_TEMPLATE_FREE (ctx->tmpl_ipmb_cmd_rs);
  ctx->tmpl_ipmb_cmd_rs = NULL;

  return (retval);
}

int8_t 
ipmi_lan_open_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t authentication_status_anonymous_login;
  uint64_t authentication_status_null_username;
  uint64_t authentication_status_non_null_username;
  uint64_t supported_authentication_type = 0;
  uint64_t temp_session_id = 0;
  uint64_t session_id = 0;
  uint8_t challenge_string[IPMI_CHALLENGE_STRING_LENGTH];
  uint64_t session_sequence_number = 0;
  uint32_t initial_outbound_sequence_number = 0;
  unsigned int seedp;
  int8_t rv = -1;
  int8_t ret;
  uint64_t val;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->io.outofband.sockfd
          && ctx->type == IPMI_DEVICE_LAN
          && strlen(ctx->io.outofband.username) <= IPMI_MAX_USER_NAME_LENGTH
          && strlen(ctx->io.outofband.password) <= IPMI_1_5_MAX_PASSWORD_LENGTH
          && IPMI_1_5_AUTHENTICATION_TYPE_VALID (ctx->io.outofband.authentication_type)
          && IPMI_PRIVILEGE_LEVEL_VALID (ctx->io.outofband.privilege_level));
  
  /* Random number generation */
  seedp = (unsigned int) clock () + (unsigned int) time (NULL);
  srand (seedp);

  ctx->io.outofband.rq_seq = (double)(IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX) * (rand()/(RAND_MAX + 1.0));
  
  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_rq);
  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_cmd_get_channel_authentication_capabilities_rs);
  
  API_ERR_CLEANUP (!(fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                                       ctx->io.outofband.privilege_level,
                                                                       obj_cmd_rq) < 0));

  if (ipmi_lan_cmd_wrapper (ctx,
                            0,
                            IPMI_BMC_IPMB_LUN_BMC,
                            IPMI_NET_FN_APP_RQ,
                            IPMI_AUTHENTICATION_TYPE_NONE,
                            NULL,
                            0,
                            &(ctx->io.outofband.rq_seq),
                            NULL,
                            0,
                            obj_cmd_rq,
                            obj_cmd_rs) < 0)
    {
      /* at this point in the protocol, we set a connection timeout */
      if (ctx->errnum == IPMI_ERR_SESSION_TIMEOUT)
        API_ERR_SET_ERRNUM (IPMI_ERR_CONNECTION_TIMEOUT);
      goto cleanup;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * Also seen on Intel X38ML motherboard.
   *
   * The ASUS motherboard reports incorrect settings of anonymous
   * vs. null vs non-null username capabilities.  The workaround is to
   * skip these checks.
   */
  if (!(ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES))
    {
      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                                "authentication_status.anonymous_login",
                                &authentication_status_anonymous_login);
      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                                "authentication_status.null_username",
                                &authentication_status_null_username);
      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                                "authentication_status.non_null_username",
                                &authentication_status_non_null_username);
      
      if ((!strlen(ctx->io.outofband.username) && !strlen(ctx->io.outofband.password)
           && !authentication_status_anonymous_login)
          || (!strlen(ctx->io.outofband.username)
              && !authentication_status_anonymous_login
              && !authentication_status_null_username)
          || (strlen(ctx->io.outofband.username)
              && !authentication_status_non_null_username))
        {
          ctx->errnum = IPMI_ERR_USERNAME_INVALID;
          goto cleanup;
        }
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on IBM eServer 325
   *
   * The remote BMC ignores if permsg authentiction is enabled
   * or disabled.  So we need to force it no matter what.
   */
  if (!(ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION))
    {
      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                                "authentication_status.per_message_authentication",
                                &val);
      ctx->io.outofband.per_msg_auth_disabled = val;
    }
  else
    ctx->io.outofband.per_msg_auth_disabled = 0;


  /* IPMI Workaround (achu)
   *
   * Not discovered yet, assume will happen.
   *
   * Authentication capabilities flags are not listed properly in the
   * response.  The workaround is to skip these checks.
   */
  if (!(ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES))
    {
      switch (ctx->io.outofband.authentication_type)
        {
        case IPMI_AUTHENTICATION_TYPE_NONE:
          API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                                    "authentication_type.none", 
                                    &supported_authentication_type);
          break;
        case IPMI_AUTHENTICATION_TYPE_MD2:
          API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                                    "authentication_type.md2", 
                                    &supported_authentication_type);
          break;
        case IPMI_AUTHENTICATION_TYPE_MD5:
          API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                                    "authentication_type.md5", 
                                    &supported_authentication_type);
          break;
        case IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY:
          API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                                    "authentication_type.straight_password_key", 
                                    &supported_authentication_type);
          break;
        case IPMI_AUTHENTICATION_TYPE_OEM_PROP:
          API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                                    "authentication_type.oem_prop", 
                                    &supported_authentication_type);
          break;
        }
      
      if (!supported_authentication_type)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE);
    }
      
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);

  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rq, tmpl_cmd_get_session_challenge_rq);
  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs);

  API_ERR_CLEANUP (!(fill_cmd_get_session_challenge (ctx->io.outofband.authentication_type,
                                                     ctx->io.outofband.username,
                                                     IPMI_MAX_USER_NAME_LENGTH,
                                                     obj_cmd_rq) < 0));

  if (ipmi_lan_cmd_wrapper (ctx,
                            IPMI_LAN_INTERNAL_WORKAROUND_FLAGS_GET_SESSION_CHALLENGE,
                            IPMI_BMC_IPMB_LUN_BMC,
                            IPMI_NET_FN_APP_RQ,
                            IPMI_AUTHENTICATION_TYPE_NONE,
                            NULL,
                            0,
                            &(ctx->io.outofband.rq_seq),
                            NULL,
                            0,
                            obj_cmd_rq,
                            obj_cmd_rs) < 0)
    goto cleanup;

  API_ERR_CLEANUP (!((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0));

  if (!ret)
    {
      if (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_INVALID_USERNAME) == 1
          || ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED) == 1)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_USERNAME_INVALID);
      else
        API_BAD_COMPLETION_CODE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      API_ERR_LOG_CLEANUP(0);
    }
  
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "temp_session_id", 
			    &temp_session_id);
  
  API_FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, 
				 "challenge_string", 
				 challenge_string,
				 IPMI_CHALLENGE_STRING_LENGTH);

  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);

  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rq, tmpl_cmd_activate_session_rq);
  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_activate_session_rs);

  initial_outbound_sequence_number = rand ();

  API_ERR_CLEANUP (!(fill_cmd_activate_session (ctx->io.outofband.authentication_type,
                                                ctx->io.outofband.privilege_level,
                                                challenge_string,
                                                IPMI_CHALLENGE_STRING_LENGTH,
                                                initial_outbound_sequence_number,
                                                obj_cmd_rq) < 0));

  if (ipmi_lan_cmd_wrapper (ctx,
                            0,
                            IPMI_BMC_IPMB_LUN_BMC,
                            IPMI_NET_FN_APP_RQ,
                            ctx->io.outofband.authentication_type,
                            NULL,
                            (uint32_t)temp_session_id,
                            &(ctx->io.outofband.rq_seq),
                            ctx->io.outofband.password,
                            IPMI_1_5_MAX_PASSWORD_LENGTH,
                            obj_cmd_rq,
                            obj_cmd_rs) < 0)
    {
      if (ctx->errnum == IPMI_ERR_SESSION_TIMEOUT)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT);
      goto cleanup;
    }

  API_ERR_CLEANUP (!((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0));

  if (!ret)
    {
      if (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE) == 1
          || ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER) == 1
          || ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_NO_SLOT_AVAILABLE_TO_SUPPORT_USER) == 1)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_BMC_BUSY);
      else if (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL) == 1)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
      else
        API_BAD_COMPLETION_CODE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      API_ERR_LOG_CLEANUP(0);
    }

  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "session_id", 
			    &session_id);

  ctx->io.outofband.session_id = session_id;

  /* achu: On some buggy BMCs the initial outbound sequence number on
   * the activate session response is off by one.  So we just accept
   * whatever sequence number they give us even if it isn't the
   * initial outbound sequence number.
   */
  API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_lan_session_hdr,
                            "session_sequence_number",
                            &val);
  ctx->io.outofband.highest_received_sequence_number = val;
  
  /* IPMI Workaround (achu)
   *
   * Discovered on Sun Fire 4100.
   *
   * The session sequence numbers for IPMI 1.5 are the wrong endian.
   * So we have to flip the bits to workaround it.
   */
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
    {
      uint32_t tmp_session_sequence_number = ctx->io.outofband.highest_received_sequence_number;

      ctx->io.outofband.highest_received_sequence_number =
        ((tmp_session_sequence_number & 0xFF000000) >> 24)
        | ((tmp_session_sequence_number & 0x00FF0000) >> 8)
        | ((tmp_session_sequence_number & 0x0000FF00) << 8)
        | ((tmp_session_sequence_number & 0x000000FF) << 24);
    }

  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "initial_inbound_sequence_number", 
			    &session_sequence_number);

  ctx->io.outofband.session_sequence_number = session_sequence_number;
  
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
			    "authentication_type",
			    &val);

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro H8QME with SIMSO daughter card.
   *
   * (Note: This could work for "IBM eServer 325" per msg auth
   * problem.  But I don't have hardware to test it :-()
   *
   * The remote BMC ignores if permsg authentiction is disabled.
   * Handle it appropriately by just not doing permsg authentication.
   */
  if (ctx->io.outofband.per_msg_auth_disabled
      && val != IPMI_AUTHENTICATION_TYPE_NONE)
    ctx->io.outofband.per_msg_auth_disabled = 0;

  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);

  /* achu: At this point in time, the session is actually setup
   * legitimately, so we can use the actual set session privilege
   * level API function.
   */
  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_set_session_privilege_level_rs);

  if (ipmi_cmd_set_session_privilege_level (ctx, 
                                            ctx->io.outofband.privilege_level,
                                            obj_cmd_rs) < 0)
    {
      if (ctx->errnum == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  if (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER) == 1
	      || ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT) == 1)
	    API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
	}
      API_ERR_LOG_CLEANUP(0);
    }

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

int8_t 
ipmi_lan_close_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;

  assert (ctx 
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd);

  API_FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_close_session_rs);

  if (ctx->type == IPMI_DEVICE_LAN)
    {
      if (ipmi_cmd_close_session(ctx,
                                 ctx->io.outofband.session_id,
                                 obj_cmd_rs) < 0)
        goto cleanup;
    }
  else
    {
      if (ipmi_cmd_close_session(ctx,
                                 ctx->io.outofband.managed_system_session_id,
                                 obj_cmd_rs) < 0)
        goto cleanup;
    }
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static void
_ipmi_lan_2_0_dump_rq (ipmi_ctx_t ctx, 
                       uint8_t authentication_algorithm,
                       uint8_t integrity_algorithm,
                       uint8_t confidentiality_algorithm,
                       uint8_t *integrity_key,
                       uint32_t integrity_key_len,
                       uint8_t *confidentiality_key,
                       uint32_t confidentiality_key_len,
                       uint8_t *pkt,
                       uint32_t pkt_len,
		       uint8_t cmd,
		       uint8_t net_fn,
                       fiid_obj_t obj_cmd_rq)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
          && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
          && pkt
          && pkt_len
	  && fiid_obj_valid(obj_cmd_rq));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template(obj_cmd_rq)))
    {
      const char *cmd_str;
      
      if ((cmd_str = ipmi_cmd_str (net_fn, cmd)))
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

          /* Handle a few IPMI 2.0 special cases */
          if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_open_session_request) == 1)
            cmd_str = DEBUG_UTIL_OPEN_SESSION_STR;
          else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_1) == 1)
            cmd_str = DEBUG_UTIL_RAKP_1_STR;
          else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_3) == 1)
            cmd_str = DEBUG_UTIL_RAKP_3_STR;
          
          debug_hdr_str(DEBUG_UTIL_TYPE_IPMI_2_0,
                        DEBUG_UTIL_DIRECTION_REQUEST,
                        cmd_str,
                        hdrbuf,
                        DEBUG_UTIL_HDR_BUFLEN);
          
          if (ctx->tmpl_ipmb_cmd_rq)
            ipmi_dump_rmcpplus_packet_ipmb (STDERR_FILENO,
                                            ctx->io.outofband.hostname,
                                            hdrbuf,
                                            NULL,
                                            authentication_algorithm,
                                            integrity_algorithm,
                                            confidentiality_algorithm,
                                            integrity_key,
                                            integrity_key_len,
                                            confidentiality_key,
                                            confidentiality_key_len,
                                            pkt,
                                            pkt_len,
                                            tmpl_lan_msg_hdr_rs,
                                            tmpl_cmd,
                                            tmpl_ipmb_msg_hdr_rq,
                                            ctx->tmpl_ipmb_cmd_rq);
          else
            ipmi_dump_rmcpplus_packet (STDERR_FILENO,
                                       ctx->io.outofband.hostname,
                                       hdrbuf,
                                       NULL,
                                       authentication_algorithm,
                                       integrity_algorithm,
                                       confidentiality_algorithm,
                                       integrity_key,
                                       integrity_key_len,
                                       confidentiality_key,
                                       confidentiality_key_len,
                                       pkt,
                                       pkt_len,
                                       tmpl_lan_msg_hdr_rq,
                                       tmpl_cmd);

          fiid_template_free (tmpl_cmd);
        }
    }
}

static void
_ipmi_lan_2_0_dump_rs (ipmi_ctx_t ctx, 
                       uint8_t authentication_algorithm,
                       uint8_t integrity_algorithm,
                       uint8_t confidentiality_algorithm,
                       uint8_t *integrity_key,
                       uint32_t integrity_key_len,
                       uint8_t *confidentiality_key,
                       uint32_t confidentiality_key_len,
                       uint8_t *pkt,
                       uint32_t pkt_len,
		       uint8_t cmd,
		       uint8_t net_fn,
                       fiid_obj_t obj_cmd_rs)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
          && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
          && pkt
          && pkt_len
	  && fiid_obj_valid(obj_cmd_rs));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template(obj_cmd_rs)))
    {
      const char *cmd_str;

      if ((cmd_str = ipmi_cmd_str (net_fn, cmd)))
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

          /* Handle a few IPMI 2.0 special cases */
          if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_open_session_response) == 1)
            cmd_str = DEBUG_UTIL_OPEN_SESSION_STR;
          else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_2) == 1)
            cmd_str = DEBUG_UTIL_RAKP_2_STR;
          else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_4) == 1)
            cmd_str = DEBUG_UTIL_RAKP_4_STR;

          debug_hdr_str(DEBUG_UTIL_TYPE_IPMI_2_0,
                        DEBUG_UTIL_DIRECTION_RESPONSE,
                        cmd_str,
                        hdrbuf,
                        DEBUG_UTIL_HDR_BUFLEN);

          ipmi_dump_rmcpplus_packet (STDERR_FILENO,
                                     ctx->io.outofband.hostname,
                                     hdrbuf,
                                     NULL,
                                     authentication_algorithm,
                                     integrity_algorithm,
                                     confidentiality_algorithm,
                                     integrity_key,
                                     integrity_key_len,
                                     confidentiality_key,
                                     confidentiality_key_len,
                                     pkt,
                                     pkt_len,
                                     tmpl_lan_msg_hdr_rs,
                                     tmpl_cmd);
          
          fiid_template_free (tmpl_cmd);
        }
    }
}

static int8_t
_ipmi_lan_2_0_cmd_send (ipmi_ctx_t ctx,
                        uint8_t lun,
                        uint8_t net_fn,
                        uint8_t payload_type,
                        uint8_t payload_authenticated,
                        uint8_t payload_encrypted,
                        uint32_t session_sequence_number,
                        uint32_t session_id,
                        uint8_t rq_seq,
                        uint8_t authentication_algorithm,
                        uint8_t integrity_algorithm,
                        uint8_t confidentiality_algorithm,
                        uint8_t *integrity_key,
                        uint32_t integrity_key_len,
                        uint8_t *confidentiality_key,
                        uint32_t confidentiality_key_len,
                        char *password,
                        uint32_t password_len,
			uint8_t cmd, /* for debug dumping */
                        fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt;
  int32_t pkt_len = 1024;
  int32_t send_len = 0;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
	 && ctx->io.outofband.sockfd 
         && IPMI_BMC_LUN_VALID(lun)
         && IPMI_NET_FN_VALID(net_fn)
         && (payload_type == IPMI_PAYLOAD_TYPE_IPMI
             || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
             || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
             || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
         && IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID(payload_authenticated)
         && IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_encrypted)
         && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
         && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
         && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
         && !(password && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq));

  API_FIID_OBJ_CLEAR(ctx->io.outofband.rq.obj_rmcp_hdr);
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rq.obj_rmcpplus_session_hdr);
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rq.obj_lan_msg_hdr);
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rq.obj_rmcpplus_session_trlr);
    
  pkt = alloca (pkt_len);
  API_ERR (pkt);
  memset (pkt, 0, pkt_len);
    
  API_ERR (fill_rmcp_hdr_ipmi (ctx->io.outofband.rq.obj_rmcp_hdr) != -1);

  API_ERR (fill_rmcpplus_session_hdr (payload_type,
                                      payload_authenticated,
                                      payload_encrypted,
                                      0, /* oem_iana */
                                      0, /* oem_payload_id */
                                      session_id,
                                      session_sequence_number,
                                      ctx->io.outofband.rq.obj_rmcpplus_session_hdr) != -1);

  API_ERR (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
			     net_fn,
                             lun,
			     rq_seq,
			     ctx->io.outofband.rq.obj_lan_msg_hdr) != -1);

  API_ERR (fill_rmcpplus_session_trlr(ctx->io.outofband.rq.obj_rmcpplus_session_trlr) != -1);

  API_ERR ((send_len = assemble_ipmi_rmcpplus_pkt(authentication_algorithm,
                                                  integrity_algorithm,
                                                  confidentiality_algorithm,
                                                  integrity_key,
                                                  integrity_key_len,
                                                  confidentiality_key,
                                                  confidentiality_key_len,
                                                  (uint8_t *)password,
                                                  password_len,
                                                  ctx->io.outofband.rq.obj_rmcp_hdr,
                                                  ctx->io.outofband.rq.obj_rmcpplus_session_hdr,
                                                  ctx->io.outofband.rq.obj_lan_msg_hdr,
                                                  obj_cmd_rq,
                                                  ctx->io.outofband.rq.obj_rmcpplus_session_trlr,
                                                  pkt,
                                                  pkt_len)) != -1);

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && send_len)
    _ipmi_lan_2_0_dump_rq (ctx, 
                           authentication_algorithm,
                           integrity_algorithm,
                           confidentiality_algorithm,
                           integrity_key,
                           integrity_key_len,
                           confidentiality_key,
                           confidentiality_key_len,
                           pkt,
                           send_len,
			   cmd,
			   net_fn,
                           obj_cmd_rq);

  API_ERR (!(ipmi_lan_sendto (ctx->io.outofband.sockfd, 
			      pkt, 
			      send_len, 
			      0, 
			      (struct sockaddr *)&(ctx->io.outofband.remote_host), 
			      sizeof(struct sockaddr_in)) < 0));

  API_ERR (!(gettimeofday(&ctx->io.outofband.last_send, NULL) < 0));
  
  return (0);
}

static int8_t
_ipmi_lan_2_0_cmd_recv (ipmi_ctx_t ctx, 
                        uint8_t authentication_algorithm,
                        uint8_t integrity_algorithm,
                        uint8_t confidentiality_algorithm,
                        uint8_t *integrity_key,
                        uint32_t integrity_key_len,
                        uint8_t *confidentiality_key,
                        uint32_t confidentiality_key_len,
                        uint8_t *pkt,
                        uint32_t pkt_len,
                        unsigned int retransmission_count,
			uint8_t cmd, /* for debug dumping */
			uint8_t net_fn, /* for debug dumping */
                        fiid_obj_t obj_cmd_rs)
{
  struct sockaddr_in from;
  socklen_t fromlen = 0;
  struct timeval timeout;
  int32_t recv_len = 0;
  
  fd_set read_set;
  int status = 0;
  
  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
	 && ctx->io.outofband.sockfd 
         && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
         && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
         && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
         && pkt
         && pkt_len
	 && fiid_obj_valid(obj_cmd_rs));
  
    
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_rmcp_hdr); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_rmcpplus_session_hdr); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_lan_msg_hdr); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_rmcpplus_payload); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_lan_msg_trlr); 
  API_FIID_OBJ_CLEAR(ctx->io.outofband.rs.obj_rmcpplus_session_trlr); 
  
  if (ctx->io.outofband.retransmission_timeout != 0)
    {
      FD_ZERO (&read_set);
      FD_SET (ctx->io.outofband.sockfd, &read_set);
      
      API_ERR (!(_calculate_timeout(ctx, retransmission_count, &timeout) < 0));
      
      API_ERR (!((status = select ((ctx->io.outofband.sockfd + 1), 
				   &read_set,
				   NULL,
				   NULL, 
				   &timeout)) < 0));
      if (status == 0)
        return (0); /* resend the request */
    }

  API_ERR (!((recv_len = ipmi_lan_recvfrom (ctx->io.outofband.sockfd, 
					    pkt, 
					    pkt_len, 
					    0, 
					    (struct sockaddr *) &from, 
					    &fromlen)) < 0));
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && recv_len)
    _ipmi_lan_2_0_dump_rs (ctx, 
                           authentication_algorithm,
                           integrity_algorithm,
                           confidentiality_algorithm,
                           integrity_key,
                           integrity_key_len,
                           confidentiality_key,
                           confidentiality_key_len,
                           pkt,
                           recv_len,
			   cmd,
			   net_fn,
                           obj_cmd_rs);

  API_ERR (unassemble_ipmi_rmcpplus_pkt (authentication_algorithm,
                                         integrity_algorithm,
                                         confidentiality_algorithm,
                                         integrity_key,
                                         integrity_key_len,
                                         confidentiality_key,
                                         confidentiality_key_len,
                                         pkt,
                                         recv_len,
                                         ctx->io.outofband.rs.obj_rmcp_hdr,
                                         ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
                                         ctx->io.outofband.rs.obj_rmcpplus_payload,
                                         ctx->io.outofband.rs.obj_lan_msg_hdr,
                                         obj_cmd_rs,
                                         ctx->io.outofband.rs.obj_lan_msg_trlr,
                                         ctx->io.outofband.rs.obj_rmcpplus_session_trlr) != -1);

  return (recv_len);
}

/* < 0 - error
 * == 1 good packet
 * == 0 bad packet
 */
static int8_t
_ipmi_lan_2_0_cmd_wrapper_verify_packet (ipmi_ctx_t ctx,
					 uint8_t payload_type,
					 uint8_t *message_tag,
					 uint32_t *session_sequence_number,
					 uint32_t session_id,
					 uint8_t *rq_seq,
					 uint8_t integrity_algorithm,
					 uint8_t *integrity_key,
					 uint32_t integrity_key_len,
					 char *password,
					 uint32_t password_len,
					 fiid_obj_t obj_cmd_rs,
					 uint8_t *pkt,
					 unsigned int pkt_len)
{
  uint64_t rs_session_sequence_number;
  uint64_t val;
  int8_t rv = -1;
  int ret;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && ctx->type == IPMI_DEVICE_LAN_2_0
	 && ctx->io.outofband.sockfd
         && (payload_type == IPMI_PAYLOAD_TYPE_IPMI
             || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
             || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
             || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
         && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
         && !(password && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
         && fiid_obj_valid(obj_cmd_rs)
	 && pkt
	 && pkt_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      uint64_t payload_type;
      
      API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
				"payload_type",
				&payload_type);
      if (payload_type != IPMI_PAYLOAD_TYPE_IPMI)
	{
	  rv = 0;
	  goto cleanup;
	}

      API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
				"session_id",
				&val);
          
      if (val != ctx->io.outofband.remote_console_session_id)
	{
	  rv = 0;
	  goto cleanup;
	}
      
      API_ERR_CLEANUP (!((ret = ipmi_lan_check_checksum (ctx->io.outofband.rs.obj_lan_msg_hdr,
							 obj_cmd_rs,
							 ctx->io.outofband.rs.obj_lan_msg_trlr)) < 0));
      
      if (!ret)
	{
	  rv = 0;
	  goto cleanup;
	}

      API_ERR_CLEANUP (!((ret = ipmi_rmcpplus_check_packet_session_authentication_code (integrity_algorithm,
											pkt,
											pkt_len,
											integrity_key,
											integrity_key_len,
											(uint8_t *)password,
											password_len,
											ctx->io.outofband.rs.obj_rmcpplus_session_trlr)) < 0));
      
      if (!ret)
	{
	  rv = 0;
	  goto cleanup;
	}
      
      if (session_sequence_number)
	{
	  API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
				    "session_sequence_number",
				    &rs_session_sequence_number);
	  
	  API_ERR_CLEANUP (!((ret = _ipmi_check_session_sequence_number(ctx, 
									(uint32_t)rs_session_sequence_number)) < 0));
	  
	  if (!ret)
	    {
	      rv = 0;
	      goto cleanup;
	    }
	}
      
      API_ERR_CLEANUP (!((ret = ipmi_lan_check_rq_seq(ctx->io.outofband.rs.obj_lan_msg_hdr, 
						      (rq_seq) ? *rq_seq : 0)) < 0));
      
      if (!ret)
	{
	  rv = 0;
	  goto cleanup;
	}
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    {
      uint64_t val;
      
      API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
				"payload_type",
				&val);
      if (val != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
	{
	  rv = 0;
	  goto cleanup;
	}

      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				"remote_console_session_id",
				&val);
          
      if (val != ctx->io.outofband.remote_console_session_id)
	{
	  rv = 0;
	  goto cleanup;
	}
      
      if (message_tag)
	{
	  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				    "message_tag",
				    &val);
	  if (val != *message_tag)
	    {
	      rv = 0;
	      goto cleanup;
	    }
	}
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    {
      uint64_t rmcpplus_status_code;
      uint64_t val;
      
      API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
				"payload_type",
				&val);
      if (val != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
	{
	  rv = 0;
	  goto cleanup;
	}

      if (message_tag)
	{
	  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				    "message_tag",
				    &val);
	  if (val != *message_tag)
	    {
	      rv = 0;
	      goto cleanup;
	    }
	}

      /* There is no guarantee that other data (i.e. authentication
       * keys, session id's, etc.) in the RAKP response will be valid
       * if there is a status code error.  So we check this status
       * code along with this stuff.
       */

      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				"rmcpplus_status_code",
				&rmcpplus_status_code);

      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				"remote_console_session_id",
				&val);
      
      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS
          && val != ctx->io.outofband.remote_console_session_id)
	{
	  rv = 0;
	  goto cleanup;
	}     
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      uint64_t rmcpplus_status_code;
      uint64_t val;
      
      API_FIID_OBJ_GET_CLEANUP (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
				"payload_type",
				&val);
      if (val != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
	{
	  rv = 0;
	  goto cleanup;
	}

      if (message_tag)
	{
	  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				    "message_tag",
				    &val);
	  if (val != *message_tag)
	    {
	      rv = 0;
	      goto cleanup;
	    }
	}

      /* There is no guarantee that other data (i.e. authentication
       * keys, session id's, etc.) in the RAKP response will be valid
       * if there is a status code error.  So we check this status
       * code along with this stuff.
       */

      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				"rmcpplus_status_code",
				&rmcpplus_status_code);

      API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
				"remote_console_session_id",
				&val);
      
      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS
          && val != ctx->io.outofband.remote_console_session_id)
	{
	  rv = 0;
	  goto cleanup;
	}      
    }

  rv = 1;
 cleanup:
  return (rv);
}

int8_t 
ipmi_lan_2_0_cmd_wrapper (ipmi_ctx_t ctx, 
                          uint8_t lun,
                          uint8_t net_fn,
                          uint8_t payload_type,
                          uint8_t payload_authenticated,
                          uint8_t payload_encrypted,
                          uint8_t *message_tag,
                          uint32_t *session_sequence_number,
                          uint32_t session_id,
                          uint8_t *rq_seq,
                          uint8_t authentication_algorithm,
                          uint8_t integrity_algorithm,
                          uint8_t confidentiality_algorithm,
                          uint8_t *integrity_key,
                          uint32_t integrity_key_len,
                          uint8_t *confidentiality_key,
                          uint32_t confidentiality_key_len,
                          char *password,
                          uint32_t password_len,
                          fiid_obj_t obj_cmd_rq,
                          fiid_obj_t obj_cmd_rs)
{
  int retval = -1;
  int ret;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  int32_t recv_len;
  uint64_t cmd = 0;		/* used for debugging */

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && ctx->type == IPMI_DEVICE_LAN_2_0
	 && ctx->io.outofband.sockfd 
         && IPMI_BMC_LUN_VALID(lun)
         && IPMI_NET_FN_VALID(net_fn) 
         && (payload_type == IPMI_PAYLOAD_TYPE_IPMI
             || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
             || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
             || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
	 && IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID(payload_authenticated)
         && IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_encrypted)
         && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
         && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
         && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
         && !(password && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq)
         && fiid_obj_valid(obj_cmd_rs));

  if (!ctx->io.outofband.last_received.tv_sec
      && !ctx->io.outofband.last_received.tv_usec)
    API_ERR (!(gettimeofday(&ctx->io.outofband.last_received, NULL) < 0));

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    API_FIID_OBJ_GET_NO_RETURN(obj_cmd_rq, "cmd", &cmd);

  if (_ipmi_lan_2_0_cmd_send (ctx, 
			      lun, 
			      net_fn,
			      payload_type,
			      payload_authenticated,
			      payload_encrypted,
			      (session_sequence_number) ? *session_sequence_number : 0,
			      session_id,
			      (rq_seq) ? *rq_seq : 0,
			      authentication_algorithm,
			      integrity_algorithm,
			      confidentiality_algorithm,
			      integrity_key,
			      integrity_key_len,
			      confidentiality_key,
			      confidentiality_key_len,
			      password,
			      password_len,
			      cmd, /* for debug dumping */
			      obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      if (_session_timed_out(ctx))
        {
	  API_ERR_SET_ERRNUM (IPMI_ERR_SESSION_TIMEOUT);
          retval = -1;
          break;
        }
     
      /* its ok to use the "request" net_fn, dump code doesn't care */
      memset(pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _ipmi_lan_2_0_cmd_recv (ctx, 
                                              authentication_algorithm,
                                              integrity_algorithm,
                                              confidentiality_algorithm,
                                              integrity_key,
                                              integrity_key_len,
                                              confidentiality_key,
                                              confidentiality_key_len,
                                              pkt,
                                              IPMI_MAX_PKT_LEN,
                                              retransmission_count,
					      cmd, /* for debug dumping */
					      net_fn, /* for debug dumping */
                                              obj_cmd_rs)) < 0)
        {
          retval = -1;
          break;
        }

      if (!recv_len)
        {
          if (message_tag)
            (*message_tag)++;
          /* In IPMI 2.0, session sequence numbers of 0 are special */
          if (session_sequence_number)
            {
              (*session_sequence_number)++;
              if (!(*session_sequence_number))
                (*session_sequence_number)++;
            }
          if (rq_seq)
            *rq_seq = ((*rq_seq) + 1) % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);

          retransmission_count++;

          if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
            {
              /* Unlike most packets, the open session request, rakp 1
               * and rakp 3 messages have the message tags in a
               * non-header field.  So this is a special case.
               */
              API_FIID_OBJ_SET_CLEANUP(obj_cmd_rq, "message_tag", (*message_tag));
            }

          if (_ipmi_lan_2_0_cmd_send (ctx, 
                                      lun, 
                                      net_fn,
                                      payload_type,
                                      payload_authenticated,
                                      payload_encrypted,
                                      (session_sequence_number) ? *session_sequence_number : 0,
                                      session_id,
                                      (rq_seq) ? *rq_seq : 0,
                                      authentication_algorithm,
                                      integrity_algorithm,
                                      confidentiality_algorithm,
                                      integrity_key,
                                      integrity_key_len,
                                      confidentiality_key,
                                      confidentiality_key_len,
                                      password,
                                      password_len,
				      cmd, /* for debug dumping */
                                      obj_cmd_rq) < 0)
            goto cleanup;
          
          continue;
        }

      /* else received a packet */

      if ((ret = _ipmi_lan_2_0_cmd_wrapper_verify_packet (ctx,
							  payload_type,
							  message_tag,
							  session_sequence_number,
							  session_id,
							  rq_seq,
							  integrity_algorithm,
							  integrity_key,
							  integrity_key_len,
							  password,
							  password_len,
							  obj_cmd_rs,
							  pkt,
							  recv_len)) < 0)
	goto cleanup;

      if (!ret)
	continue;
     
      API_ERR_CLEANUP (!(gettimeofday(&ctx->io.outofband.last_received, NULL) < 0));
      retval = recv_len;
      break;
    }

 cleanup:
  if (message_tag)
    (*message_tag)++;
  /* In IPMI 2.0, session sequence numbers of 0 are special */
  if (session_sequence_number)
    {
      (*session_sequence_number)++;
      if (!(*session_sequence_number))
        (*session_sequence_number)++;
    }
  if (rq_seq)
    *rq_seq = ((*rq_seq) + 1) % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);
  return (retval);
}

int8_t 
ipmi_lan_2_0_cmd_wrapper_ipmb (ipmi_ctx_t ctx,
			       fiid_obj_t obj_cmd_rq,
			       fiid_obj_t obj_cmd_rs)
{
  int retval = -1;
  int ret;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  int32_t recv_len;
  uint64_t cmd = 0;		/* used for debugging */
  uint8_t rq_seq_orig;

  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
         && ctx->type == IPMI_DEVICE_LAN_2_0
	 && ctx->io.outofband.sockfd 
	 && fiid_obj_valid(obj_cmd_rq)
	 && fiid_obj_packet_valid(obj_cmd_rq)
         && fiid_obj_valid(obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    API_FIID_OBJ_GET_NO_RETURN(obj_cmd_rq, "cmd", &cmd);

  /* for debugging */
  ctx->tmpl_ipmb_cmd_rq = fiid_obj_template(obj_cmd_rq);
  ctx->tmpl_ipmb_cmd_rs = fiid_obj_template(obj_cmd_rs);

  /* ipmb response packet will use the request sequence number from
   * the earlier packet.  Save it for verification.
   */

  rq_seq_orig = ctx->io.outofband.rq_seq;

  if (_ipmi_cmd_send_ipmb (ctx,
			   ctx->rs_addr,
			   ctx->lun,
			   ctx->net_fn,
			   obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      uint8_t payload_authenticated;
      uint8_t payload_encrypted;

      if (_session_timed_out(ctx))
        {
	  API_ERR_SET_ERRNUM (IPMI_ERR_SESSION_TIMEOUT);
          retval = -1;
          break;
        }
     
      /* its ok to use the "request" net_fn, dump code doesn't care */
      memset(pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _ipmi_lan_2_0_cmd_recv (ctx, 
					      ctx->io.outofband.authentication_algorithm,
					      ctx->io.outofband.integrity_algorithm,
					      ctx->io.outofband.confidentiality_algorithm,
					      ctx->io.outofband.integrity_key_ptr,
					      ctx->io.outofband.integrity_key_len,
					      ctx->io.outofband.confidentiality_key_ptr,
					      ctx->io.outofband.confidentiality_key_len,
                                              pkt,
                                              IPMI_MAX_PKT_LEN,
                                              retransmission_count,
					      cmd, /* for debug dumping */
					      ctx->net_fn, /* for debug dumping */
                                              obj_cmd_rs)) < 0)
        {
          retval = -1;
          break;
        }

      if (!recv_len)
        {
          retransmission_count++;
          
	  /* don't increment sequence numbers, will be done in _ipmi_cmd_send_ipmb */

	  /* ipmb response packet will use the request sequence number from
	   * the earlier packet.  Save it for verification.
	   */

	  rq_seq_orig = ctx->io.outofband.rq_seq;
	  
	  API_ERR_CLEANUP (!(_ipmi_cmd_send_ipmb (ctx,
						  ctx->rs_addr,
						  ctx->lun,
						  ctx->net_fn,
						  obj_cmd_rq) < 0));

          continue;
        }

      /* else received a packet */

      ipmi_lan_2_0_cmd_get_session_parameters (ctx,
					       &payload_authenticated,
					       &payload_encrypted);
      
      if ((ret = _ipmi_lan_2_0_cmd_wrapper_verify_packet (ctx,
							  IPMI_PAYLOAD_TYPE_IPMI,
							  NULL,
							  &(ctx->io.outofband.session_sequence_number),
							  ctx->io.outofband.managed_system_session_id,
							  &rq_seq_orig,
							  ctx->io.outofband.integrity_algorithm,
							  ctx->io.outofband.integrity_key_ptr,
							  ctx->io.outofband.integrity_key_len,
							  strlen(ctx->io.outofband.password) ? ctx->io.outofband.password : NULL,
							  strlen(ctx->io.outofband.password),
							  
							  obj_cmd_rs,
							  pkt,
							  recv_len)) < 0)
	goto cleanup;
      
      if (!ret)
	continue;

      API_ERR_CLEANUP (!(gettimeofday(&(ctx->io.outofband.last_received), NULL) < 0));
      retval = recv_len;
      break;
    }
  
 cleanup:
  ctx->io.outofband.session_sequence_number++;
  /* rq_seq already incremented via _ipmi_cmd_send_ipmb call */
  API_FIID_TEMPLATE_FREE (ctx->tmpl_ipmb_cmd_rq);
  ctx->tmpl_ipmb_cmd_rq = NULL;
  API_FIID_TEMPLATE_FREE (ctx->tmpl_ipmb_cmd_rs);
  ctx->tmpl_ipmb_cmd_rs = NULL;

  return (retval);
}

int8_t 
ipmi_lan_2_0_open_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rq = NULL;  
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t authentication_status_anonymous_login;
  uint64_t authentication_status_null_username;
  uint64_t authentication_status_non_null_username;
  uint64_t authentication_status_k_g;
  uint64_t ipmi_v20_extended_capabilities_available;
  uint64_t channel_supports_ipmi_v20_connections;
  uint8_t maximum_privilege_level;
  uint8_t rmcpplus_status_code;
  uint8_t remote_console_random_number[IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH];
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int32_t managed_system_random_number_len;
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int32_t managed_system_guid_len;
  uint8_t key_exchange_authentication_code[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  int32_t key_exchange_authentication_code_len;
  uint8_t message_tag;
  char *username;
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  uint32_t username_len;
  char *password;
  uint32_t password_len;
  uint8_t authentication_algorithm = 0; /* init to 0 to remove gcc warning */
  uint8_t requested_maximum_privilege;
  uint8_t assume_rakp_4_success = 0;
  uint8_t name_only_lookup;
  int8_t rv = -1;
  unsigned int seedp;
  int ret;
  uint64_t val;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->io.outofband.sockfd
          && ctx->type == IPMI_DEVICE_LAN_2_0
          && strlen(ctx->io.outofband.username) <= IPMI_MAX_USER_NAME_LENGTH
          && strlen(ctx->io.outofband.password) <= IPMI_2_0_MAX_PASSWORD_LENGTH
          && IPMI_PRIVILEGE_LEVEL_VALID (ctx->io.outofband.privilege_level)
          && IPMI_CIPHER_SUITE_ID_SUPPORTED (ctx->io.outofband.cipher_suite_id)
          && ctx->io.outofband.sik_key_ptr == ctx->io.outofband.sik_key
          && ctx->io.outofband.sik_key_len == IPMI_MAX_SIK_KEY_LENGTH
          && ctx->io.outofband.integrity_key_ptr == ctx->io.outofband.integrity_key
          && ctx->io.outofband.integrity_key_len == IPMI_MAX_INTEGRITY_KEY_LENGTH
          && ctx->io.outofband.confidentiality_key_ptr == ctx->io.outofband.confidentiality_key
          && ctx->io.outofband.confidentiality_key_len == IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH);

  /* Random number generation */
  seedp = (unsigned int) clock () + (unsigned int) time (NULL);
  srand (seedp);

  /* Unlike IPMI 1.5, there is no initial sequence number negotiation, so we don't 
   * start at a random sequence number.
   */
  ctx->io.outofband.session_sequence_number = 1;
    
  ctx->io.outofband.rq_seq = (uint8_t)((double)(IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX) * (rand()/(RAND_MAX + 1.0)));
  
  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_v20_rq);
  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_cmd_get_channel_authentication_capabilities_v20_rs);
  
  API_ERR_CLEANUP (!(fill_cmd_get_channel_authentication_capabilities_v20 (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                                           ctx->io.outofband.privilege_level,
                                                                           IPMI_GET_IPMI_V20_EXTENDED_DATA,
                                                                           obj_cmd_rq) < 0));

  /* This portion of the protocol is sent via IPMI 1.5 */
  if (ipmi_lan_cmd_wrapper (ctx,
                            0,
                            IPMI_BMC_IPMB_LUN_BMC,
                            IPMI_NET_FN_APP_RQ,
                            IPMI_AUTHENTICATION_TYPE_NONE,
                            NULL,
                            0,
                            &(ctx->io.outofband.rq_seq),
                            NULL,
                            0,
                            obj_cmd_rq,
                            obj_cmd_rs) < 0)
    {
      /* at this point in the protocol, we set a connection timeout */
      if (ctx->errnum == IPMI_ERR_SESSION_TIMEOUT)
        API_ERR_SET_ERRNUM (IPMI_ERR_CONNECTION_TIMEOUT);
      goto cleanup;
    }

  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                            "authentication_status.anonymous_login",
                            &authentication_status_anonymous_login);
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                            "authentication_status.null_username",
                            &authentication_status_null_username);
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                            "authentication_status.non_null_username",
                            &authentication_status_non_null_username);
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                            "authentication_status.k_g",
                            &authentication_status_k_g);
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                            "authentication_type.ipmi_v2.0_extended_capabilities_available",
                            &ipmi_v20_extended_capabilities_available);
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                            "channel_supports_ipmi_v2.0_connections",
                            &channel_supports_ipmi_v20_connections);

  if (!ipmi_v20_extended_capabilities_available
      || !channel_supports_ipmi_v20_connections)
    {
      ctx->errnum = IPMI_ERR_IPMI_2_0_UNAVAILABLE;
      goto cleanup;
    }

  /* IPMI Workaround
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * The ASUS motherboard reports incorrect settings of anonymous
   * vs. null vs non-null username capabilities.  The workaround is to
   * skip all these checks.
   *
   * Discovered on an ASUS P5MT-R motherboard
   *
   * K_g status is reported incorrectly too.  Again, skip the checks.
   */
  if (!(ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES))
    {
      if ((!strlen(ctx->io.outofband.username) && !strlen(ctx->io.outofband.password)
           && !authentication_status_anonymous_login)
          || (!strlen(ctx->io.outofband.username)
              && !authentication_status_anonymous_login
              && !authentication_status_null_username)
          || (strlen(ctx->io.outofband.username)
              && !authentication_status_non_null_username))
        {
          ctx->errnum = IPMI_ERR_USERNAME_INVALID;
          goto cleanup;
        }
      
      if ((!ctx->io.outofband.k_g_configured && authentication_status_k_g)
          || (ctx->io.outofband.k_g_configured && !authentication_status_k_g))
        {
          API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_K_G_INVALID);
          API_ERR_LOG_CLEANUP(0);
        }
    }

  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);

  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rq, tmpl_rmcpplus_open_session_request);
  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_rmcpplus_open_session_response);

  message_tag = (uint8_t)rand();

  /* In IPMI 2.0, session_ids of 0 are special */
  do
    {
      API_ERR_CLEANUP (!(ipmi_get_random((uint8_t *)&(ctx->io.outofband.remote_console_session_id),
                                         sizeof(ctx->io.outofband.remote_console_session_id)) < 0));
    } while (!ctx->io.outofband.remote_console_session_id);

  API_ERR_CLEANUP (!(ipmi_cipher_suite_id_to_algorithms(ctx->io.outofband.cipher_suite_id,
                                                        &(ctx->io.outofband.authentication_algorithm),
                                                        &(ctx->io.outofband.integrity_algorithm),
                                                        &(ctx->io.outofband.confidentiality_algorithm)) < 0));

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The Intel's return IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL instead
   * of an actual privilege, so have to pass the actual privilege
   * we want to use.
   */
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION
      || ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_SUN_2_0_SESSION
      || ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE)
    requested_maximum_privilege = ctx->io.outofband.privilege_level;
  else
    requested_maximum_privilege = IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL;

  API_ERR_CLEANUP (!(fill_rmcpplus_open_session (message_tag,
                                                 requested_maximum_privilege,
                                                 ctx->io.outofband.remote_console_session_id,
                                                 ctx->io.outofband.authentication_algorithm,
                                                 ctx->io.outofband.integrity_algorithm,
                                                 ctx->io.outofband.confidentiality_algorithm,
                                                 obj_cmd_rq)));

  if (ipmi_lan_2_0_cmd_wrapper (ctx,
                                IPMI_BMC_IPMB_LUN_BMC, /* doesn't actually matter here */
                                IPMI_NET_FN_APP_RQ, /* doesn't actually matter here */
                                IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST,
                                IPMI_PAYLOAD_FLAG_UNAUTHENTICATED,
                                IPMI_PAYLOAD_FLAG_UNENCRYPTED,
                                &message_tag,
                                NULL,
                                0,
                                NULL,
                                IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE,
                                IPMI_INTEGRITY_ALGORITHM_NONE,
                                IPMI_CONFIDENTIALITY_ALGORITHM_NONE,
                                NULL,
                                0,
                                NULL,
                                0,
                                NULL,
                                0,
                                obj_cmd_rq,
                                obj_cmd_rs) < 0)
    goto cleanup;
  
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                            "rmcpplus_status_code",
                            &val);
  rmcpplus_status_code = val;
  
  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    {
      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_CIPHER_SUITE_MATCH_WITH_PROPOSED_SECURITY_ALGORITHMS)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION
               || rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_BMC_BUSY);
      else
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE);
      API_ERR_LOG_CLEANUP(0);
    }

  /* Check if we can eventually authentication at the privilege we want */
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                            "maximum_privilege_level",
                            &val);
  maximum_privilege_level = val;
  
  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The Intel's return IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL instead
   * of an actual privilege, so have to pass the actual privilege
   * we want to use.
   */

  if (!((ctx->io.outofband.privilege_level == IPMI_PRIVILEGE_LEVEL_USER
         && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_USER
             || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
             || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
             || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
        || (ctx->io.outofband.privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
            && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR
                || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
                || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
        || (ctx->io.outofband.privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
            && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN
                || maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_OEM))
        || ((ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
            && (maximum_privilege_level == IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL))))
    {
      API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
      API_ERR_LOG_CLEANUP(0);
    }

  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                            "managed_system_session_id",
                            &val);
  ctx->io.outofband.managed_system_session_id = val;

  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);

  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rq, tmpl_rmcpplus_rakp_message_1);
  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_rmcpplus_rakp_message_2);

  API_ERR_CLEANUP (!(ipmi_get_random(remote_console_random_number,
                                     IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH) < 0));
  
  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The username must be padded despite explicitly not being
   * allowed.  "No Null characters (00h) are allowed in the name".
   * Table 13-11 in the IPMI 2.0 spec.
   *
   * achu: This should only be done for the RAKP 1 message.
   */
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    {
      memset(username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (strlen(ctx->io.outofband.username))
        strcpy(username_buf, ctx->io.outofband.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      if (strlen(ctx->io.outofband.username))
        username = ctx->io.outofband.username;
      else
        username = NULL;
      username_len = (username) ? strlen(username) : 0;
    }

  /* achu: Unlike IPMI 1.5, the length of the username must be actual
   * length, it can't be the maximum length.
   */
  API_ERR_CLEANUP (!(fill_rmcpplus_rakp_message_1 (message_tag,
                                                   ctx->io.outofband.managed_system_session_id,
                                                   remote_console_random_number,
                                                   IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                   ctx->io.outofband.privilege_level,
                                                   IPMI_NAME_ONLY_LOOKUP,
                                                   username,
                                                   username_len,
                                                   obj_cmd_rq)));

  if (ipmi_lan_2_0_cmd_wrapper (ctx,
                                IPMI_BMC_IPMB_LUN_BMC, /* doesn't actually matter here */
                                IPMI_NET_FN_APP_RQ, /* doesn't actually matter here */
                                IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1,
                                IPMI_PAYLOAD_FLAG_UNAUTHENTICATED,
                                IPMI_PAYLOAD_FLAG_UNENCRYPTED,
                                &message_tag,
                                NULL,
                                0,
                                NULL,
                                IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE,
                                IPMI_INTEGRITY_ALGORITHM_NONE,
                                IPMI_CONFIDENTIALITY_ALGORITHM_NONE,
                                NULL,
                                0,
                                NULL,
                                0,
                                NULL,
                                0,
                                obj_cmd_rq,
                                obj_cmd_rs) < 0)
    goto cleanup;

  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                            "rmcpplus_status_code",
                            &val);
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    {
      if (rmcpplus_status_code == RMCPPLUS_STATUS_UNAUTHORIZED_NAME)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_USERNAME_INVALID);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_UNAUTHORIZED_ROLE_OR_PRIVILEGE_LEVEL_REQUESTED)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION
               || rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_BMC_BUSY);
      else
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE);
      API_ERR_LOG_CLEANUP(0);
    }

  API_FIID_OBJ_GET_DATA_LEN_CLEANUP (managed_system_random_number_len,
                                     obj_cmd_rs,
                                     "managed_system_random_number",
                                     managed_system_random_number,
                                     IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH);

  API_FIID_OBJ_GET_DATA_LEN_CLEANUP (managed_system_guid_len,
                                     obj_cmd_rs,
                                     "managed_system_guid",
                                     managed_system_guid,
                                     IPMI_MANAGED_SYSTEM_GUID_LENGTH);

  if (managed_system_random_number_len != IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH
      || managed_system_guid_len != IPMI_MANAGED_SYSTEM_GUID_LENGTH)
    {
      API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_IPMI_ERROR);
      API_ERR_LOG_CLEANUP(0);
    }

  if (strlen(ctx->io.outofband.password))
    password = ctx->io.outofband.password;
  else
    password = NULL;
  password_len = (password) ? strlen(password) : 0;

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
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION
      && ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;

  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
    {
      uint8_t keybuf[IPMI_MAX_PKT_LEN];
      int32_t keybuf_len;

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

      API_FIID_OBJ_GET_DATA_LEN_CLEANUP (keybuf_len,
                                         obj_cmd_rs,
                                         "key_exchange_authentication_code",
                                         keybuf,
                                         IPMI_MAX_PKT_LEN);

      if (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
          && keybuf_len == 1)
        API_FIID_OBJ_CLEAR_FIELD_CLEANUP (obj_cmd_rs, "key_exchange_authentication_code");
      else if (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
               && keybuf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        API_FIID_OBJ_SET_DATA_CLEANUP (obj_cmd_rs,
                                       "key_exchange_authentication_code",
                                       keybuf,
                                       IPMI_HMAC_SHA1_DIGEST_LENGTH);
      else if (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
               && keybuf_len == (IPMI_HMAC_MD5_DIGEST_LENGTH + 1))
        API_FIID_OBJ_SET_DATA_CLEANUP (obj_cmd_rs,
                                       "key_exchange_authentication_code",
                                       keybuf,
                                       IPMI_HMAC_MD5_DIGEST_LENGTH);
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
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_SUN_2_0_SESSION
      && (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1))
    {
      uint8_t buf[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
      int32_t buf_len;

      API_FIID_OBJ_GET_DATA_LEN_CLEANUP (buf_len,
                                         obj_cmd_rs,
                                         "key_exchange_authentication_code",
                                         buf,
                                         IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH);

      if (buf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          API_FIID_OBJ_CLEAR_FIELD_CLEANUP (obj_cmd_rs,
                                            "key_exchange_authentication_code");
          API_FIID_OBJ_SET_DATA_CLEANUP (obj_cmd_rs,
                                         "key_exchange_authentication_code",
                                         buf,
                                         IPMI_HMAC_SHA1_DIGEST_LENGTH);
        }
    }

  API_ERR_CLEANUP (!((ret = ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code (ctx->io.outofband.authentication_algorithm,
                                                                                         (uint8_t *)password,
                                                                                         password_len,
                                                                                         ctx->io.outofband.remote_console_session_id,
                                                                                         ctx->io.outofband.managed_system_session_id,
                                                                                         remote_console_random_number,
                                                                                         IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                                                         managed_system_random_number,
                                                                                         managed_system_random_number_len,
                                                                                         managed_system_guid,
                                                                                         managed_system_guid_len,
                                                                                         IPMI_NAME_ONLY_LOOKUP,
                                                                                         ctx->io.outofband.privilege_level,
                                                                                         username,
                                                                                         username_len,
                                                                                         obj_cmd_rs)) < 0));
  
  if (!ret)
    {
      /* XXX: achu: some systems, password could be correct, but
       * privilege is too high.  The error is b/c the privilege error
       * is not handled properly in the open session stage (i.e. they
       * tell me I can authenticate at a high privilege level, that in
       * reality is not allowed).  Dunno how to deal with this.
       */
      API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PASSWORD_INVALID);
      API_ERR_LOG_CLEANUP(0);
    }

  /* achu: note, for INTEL_2_0 workaround, this must have the username/password adjustments */
  API_ERR_CLEANUP (!(ipmi_calculate_rmcpplus_session_keys(ctx->io.outofband.authentication_algorithm,
                                                          ctx->io.outofband.integrity_algorithm,
                                                          ctx->io.outofband.confidentiality_algorithm,
                                                          (uint8_t *)password,
                                                          password_len,
                                                          (ctx->io.outofband.k_g_configured) ? ctx->io.outofband.k_g : NULL,
                                                          (ctx->io.outofband.k_g_configured) ? IPMI_MAX_K_G_LENGTH : 0,
                                                          remote_console_random_number,
                                                          IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                          managed_system_random_number,
                                                          IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH,
                                                          IPMI_NAME_ONLY_LOOKUP,
                                                          ctx->io.outofband.privilege_level,
                                                          username,
                                                          username_len,
                                                          &(ctx->io.outofband.sik_key_ptr),
                                                          &(ctx->io.outofband.sik_key_len),
                                                          &(ctx->io.outofband.integrity_key_ptr),
                                                          &(ctx->io.outofband.integrity_key_len),
                                                          &(ctx->io.outofband.confidentiality_key_ptr),
                                                          &(ctx->io.outofband.confidentiality_key_len)) < 0));
  
  /* achu: If INTEL_2_0 workaround is set, get back to original username &
   * username_len, because that isn't needed for the RAKP3/4 part.
   */
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    {
      if (strlen(ctx->io.outofband.username))
        username = ctx->io.outofband.username;
      else
        username = NULL;
      username_len = (username) ? strlen(username) : 0;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * For some reason we have to create this key with the name only
   * lookup turned off.  I was skeptical about this actually being
   * a bug until I saw that the ipmitool folks implemented the
   * same workaround.
   */
  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    name_only_lookup = IPMI_USER_NAME_PRIVILEGE_LOOKUP;
  else
    name_only_lookup = IPMI_NAME_ONLY_LOOKUP;

  API_ERR_CLEANUP (!((key_exchange_authentication_code_len = ipmi_calculate_rakp_3_key_exchange_authentication_code(ctx->io.outofband.authentication_algorithm,
                                                                                                                    (uint8_t *)password,
                                                                                                                    password_len,
                                                                                                                    managed_system_random_number,
                                                                                                                    managed_system_random_number_len,
                                                                                                                    ctx->io.outofband.remote_console_session_id,
                                                                                                                    name_only_lookup,
                                                                                                                    ctx->io.outofband.privilege_level,
                                                                                                                    username,
                                                                                                                    username_len,
                                                                                                                    key_exchange_authentication_code,
                                                                                                                    IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0));

  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);

  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rq, tmpl_rmcpplus_rakp_message_3);
  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_rmcpplus_rakp_message_4);
  
  API_ERR_CLEANUP (!(fill_rmcpplus_rakp_message_3(message_tag,
                                                  RMCPPLUS_STATUS_NO_ERRORS,
                                                  ctx->io.outofband.managed_system_session_id,
                                                  key_exchange_authentication_code,
                                                  key_exchange_authentication_code_len,
                                                  obj_cmd_rq) < 0));

  if (ipmi_lan_2_0_cmd_wrapper (ctx,
                                IPMI_BMC_IPMB_LUN_BMC, /* doesn't actually matter here */
                                IPMI_NET_FN_APP_RQ, /* doesn't actually matter here */
                                IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3,
                                IPMI_PAYLOAD_FLAG_UNAUTHENTICATED,
                                IPMI_PAYLOAD_FLAG_UNENCRYPTED,
                                &message_tag,
                                NULL,
                                0,
                                NULL,
                                IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE,
                                IPMI_INTEGRITY_ALGORITHM_NONE,
                                IPMI_CONFIDENTIALITY_ALGORITHM_NONE,
                                NULL,
                                0,
                                NULL,
                                0,
                                NULL,
                                0,
                                obj_cmd_rq,
                                obj_cmd_rs) < 0)
    goto cleanup;
  
  API_FIID_OBJ_GET_CLEANUP (obj_cmd_rs,
                            "rmcpplus_status_code",
                            &val);
  rmcpplus_status_code = val;
  
  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    {
      if (rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION
          || rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME)
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_BMC_BUSY);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INVALID_INTEGRITY_CHECK_VALUE)
        /* XXX: achu: some systems, password could be correct, but
         * privilege used in hashing is incorrect on the BMC side
         * (OPEN_SESSION_PRIVILEGE workaround).
         */
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PASSWORD_INVALID);
      else
        API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE);
      API_ERR_LOG_CLEANUP(0);
    }
  
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

  if (ctx->workaround_flags & IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    {
      if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
      else if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1;
      else if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5;
      else if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        /* achu: I have thus far been unable to reverse engineer this
         * corner case.  So we're just going to accept whatever the
         * remote BMC gives us.  This has been documented in the
         * manpage.
         */
        assume_rakp_4_success++;
    }
  else
    authentication_algorithm = ctx->io.outofband.authentication_algorithm;

  if (!assume_rakp_4_success)
    {
      API_ERR_CLEANUP (!((ret = ipmi_rmcpplus_check_rakp_4_integrity_check_value(authentication_algorithm,
                                                                                 ctx->io.outofband.sik_key_ptr,
                                                                                 ctx->io.outofband.sik_key_len,
                                                                                 remote_console_random_number,
                                                                                 IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                                                 ctx->io.outofband.managed_system_session_id,
                                                                                 managed_system_guid,
                                                                                 managed_system_guid_len,
                                                                                 obj_cmd_rs)) < 0));
      
      if (!ret)
        {
          API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_K_G_INVALID);
          API_ERR_LOG_CLEANUP(0);
        }
    }
  
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);

  /* achu: At this point in time, the session is actually setup
   * legitimately, so we can use the actual set session privilege
   * level API function.
   */

  API_FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_set_session_privilege_level_rs);

  if (ipmi_cmd_set_session_privilege_level (ctx, 
                                            ctx->io.outofband.privilege_level,
                                            obj_cmd_rs) < 0)
    {
      if (ctx->errnum == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  if (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER) == 1
	      || ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT) == 1)
	    API_ERR_SET_ERRNUM_CLEANUP(IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
	}
      API_ERR_LOG_CLEANUP(0);
    }

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

int8_t 
ipmi_lan_2_0_close_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t rv = -1;

  assert (ctx 
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0
          && ctx->io.outofband.sockfd);

  API_FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_close_session_rs);

  if (ipmi_cmd_close_session(ctx,
                             ctx->io.outofband.managed_system_session_id,
                             obj_cmd_rs) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}
