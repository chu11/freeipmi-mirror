/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

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
#include <sys/types.h>
#include <sys/select.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"
#include "freeipmi/util/ipmi-lan-util.h"
#include "freeipmi/util/ipmi-outofband-util.h"
#include "freeipmi/util/ipmi-rmcpplus-util.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-lan-session-common.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

#define IPMI_LAN_BACKOFF_COUNT         2

struct socket_to_close {
  int fd;
  struct socket_to_close *next;
};

#define IPMI_PKT_PAD 1024

void
api_lan_cmd_get_session_parameters (ipmi_ctx_t ctx,
				    uint8_t *authentication_type,
				    unsigned int *internal_workaround_flags)
{
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN
          && authentication_type
          && internal_workaround_flags);

  (*authentication_type) = IPMI_AUTHENTICATION_TYPE_NONE;
  (*internal_workaround_flags) = 0;

  if (ctx->io.outofband.per_msg_auth_disabled)
    {
      (*authentication_type) = IPMI_AUTHENTICATION_TYPE_NONE;
      if (ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE)
        (*internal_workaround_flags) |= IPMI_INTERNAL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
    }
  else
    (*authentication_type) = ctx->io.outofband.authentication_type;
}

void
api_lan_2_0_cmd_get_session_parameters (ipmi_ctx_t ctx,
					uint8_t *payload_authenticated,
					uint8_t *payload_encrypted)
{
  assert (ctx
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
_session_timed_out (ipmi_ctx_t ctx)
{
  struct timeval current;
  struct timeval session_timeout;
  struct timeval session_timeout_len;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0));

  session_timeout_len.tv_sec = ctx->io.outofband.session_timeout / 1000;
  session_timeout_len.tv_usec = (ctx->io.outofband.session_timeout - (session_timeout_len.tv_sec * 1000)) * 1000;
  timeradd (&(ctx->io.outofband.last_received), &session_timeout_len, &session_timeout);

  if (gettimeofday (&current, NULL) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  return (timercmp (&current, &session_timeout, >));
}

/* return 1 on continue, 0 if timeout already happened, -1 on error */
static int
_calculate_timeout (ipmi_ctx_t ctx,
                    unsigned int retransmission_count,
                    struct timeval *recv_starttime,
                    struct timeval *timeout)
{
  struct timeval current;
  struct timeval session_timeout;
  struct timeval session_timeout_len;
  struct timeval session_timeout_val;
  struct timeval retransmission_timeout;
  struct timeval retransmission_timeout_len;
  struct timeval retransmission_timeout_val;
  struct timeval already_timedout_check;
  unsigned int retransmission_timeout_multiplier;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && recv_starttime
          && timeout);

  if (gettimeofday (&current, NULL) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  session_timeout_len.tv_sec = ctx->io.outofband.session_timeout / 1000;
  session_timeout_len.tv_usec = (ctx->io.outofband.session_timeout - (session_timeout_len.tv_sec * 1000)) * 1000;

  timeradd (recv_starttime, &session_timeout_len, &session_timeout);
  timersub (&session_timeout, recv_starttime, &session_timeout_val);

  retransmission_timeout_multiplier = (retransmission_count / IPMI_LAN_BACKOFF_COUNT) + 1;

  retransmission_timeout_len.tv_sec = (retransmission_timeout_multiplier * ctx->io.outofband.retransmission_timeout) / 1000;
  retransmission_timeout_len.tv_usec = ((retransmission_timeout_multiplier * ctx->io.outofband.retransmission_timeout) - (retransmission_timeout_len.tv_sec * 1000)) * 1000;

  timeradd (&ctx->io.outofband.last_send, &retransmission_timeout_len, &retransmission_timeout);
  timersub (&retransmission_timeout, recv_starttime, &retransmission_timeout_val);

  if (timercmp (&retransmission_timeout_val, &session_timeout_val, <))
    {
      timeout->tv_sec = retransmission_timeout_val.tv_sec;
      timeout->tv_usec = retransmission_timeout_val.tv_usec;
    }
  else
    {
      timeout->tv_sec = session_timeout_val.tv_sec;
      timeout->tv_usec = session_timeout_val.tv_usec;
    }

  /* See portability issue below regarding ECONNRESET and ECONNREFUSED
   * to see why there could be two calls to this in a row, and thus
   * this check is necessary
   */

  timersub (&current, recv_starttime, &already_timedout_check);
  
  if (timercmp (timeout, &already_timedout_check, <))
    return (0);

  return (1);
}

static int
_api_lan_recvfrom (ipmi_ctx_t ctx,
		   void *pkt,
		   unsigned int pkt_len,
		   unsigned int retransmission_count,
		   struct timeval *recv_starttime)
{
  struct timeval timeout;
  fd_set read_set;
  int status = 0;
  int recv_len;
  int ret;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd
          && pkt
          && pkt_len
          && recv_starttime);

  if (ctx->io.outofband.retransmission_timeout)
    {
      FD_ZERO (&read_set);
      FD_SET (ctx->io.outofband.sockfd, &read_set);

      if ((ret = _calculate_timeout (ctx,
                                     retransmission_count,
                                     recv_starttime,
                                     &timeout)) < 0)
        return (-1);

      if (!ret)
        return (0);

      if ((status = select ((ctx->io.outofband.sockfd + 1),
                            &read_set,
                            NULL,
                            NULL,
                            &timeout)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          return (-1);
        }

      if (!status)
        return (0); /* resend the request */
    }

  do
    {
      /* For receive side, ipmi_lan_recvfrom and
       * ipmi_rmcpplus_recvfrom are identical.  So we just use
       * ipmi_lan_recvfrom for both.
       *
       * In event of future change, should use util functions
       * ipmi_is_ipmi_1_5_packet or ipmi_is_ipmi_2_0_packet
       * appropriately.
       */
      recv_len = ipmi_lan_recvfrom (ctx->io.outofband.sockfd,
                                    pkt,
                                    pkt_len,
                                    0,
                                    NULL,
                                    NULL);
    } while (recv_len < 0 && errno == EINTR);

  return (recv_len);
}

static void
_api_lan_dump_rq (ipmi_ctx_t ctx,
		  const void *pkt,
		  unsigned int pkt_len,
		  uint8_t cmd,
		  uint8_t net_fn,
		  uint8_t group_extension,
		  fiid_obj_t obj_cmd_rq)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && pkt
          && pkt_len
          && fiid_obj_valid (obj_cmd_rq));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rq)))
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd (DEBUG_UTIL_TYPE_IPMI_1_5,
                     DEBUG_UTIL_DIRECTION_REQUEST,
                     net_fn,
                     cmd,
		     group_extension,
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
_api_lan_dump_rs (ipmi_ctx_t ctx,
		  const void *pkt,
		  unsigned int pkt_len,
		  uint8_t cmd,
		  uint8_t net_fn,
		  uint8_t group_extension,
		  fiid_obj_t obj_cmd_rs)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && pkt
          && pkt_len
          && fiid_obj_valid (obj_cmd_rs));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rs)))
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd (DEBUG_UTIL_TYPE_IPMI_1_5,
                     DEBUG_UTIL_DIRECTION_RESPONSE,
                     net_fn,
                     cmd,
		     group_extension,
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

static int
_ipmi_check_session_sequence_number (ipmi_ctx_t ctx,
                                     uint32_t session_sequence_number)
{
  int rv = 0;

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

  assert (ctx
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
  if (ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER)
    {
      uint32_t tmp_session_sequence_number = session_sequence_number;

      session_sequence_number =
        ((tmp_session_sequence_number & 0xFF000000) >> 24)
        | ((tmp_session_sequence_number & 0x00FF0000) >> 8)
        | ((tmp_session_sequence_number & 0x0000FF00) << 8)
        | ((tmp_session_sequence_number & 0x000000FF) << 24);
    }

  if (ctx->type == IPMI_DEVICE_LAN)
    {
      if ((rv = ipmi_check_session_sequence_number_1_5 (session_sequence_number,
                                                        &(ctx->io.outofband.highest_received_sequence_number),
                                                        &(ctx->io.outofband.previously_received_list),
                                                        0)) < 0)
        {
          {
            API_ERRNO_TO_API_ERRNUM (ctx, errno);
            goto cleanup;
          }
        }
    }
  else
    {
      if ((rv = ipmi_check_session_sequence_number_2_0 (session_sequence_number,
                                                        &(ctx->io.outofband.highest_received_sequence_number),
                                                        &(ctx->io.outofband.previously_received_list),
                                                        0)) < 0)
        {
          {
            API_ERRNO_TO_API_ERRNUM (ctx, errno);
            goto cleanup;
          }
        }
    }

 cleanup:
  return (rv);
}

static int
_api_lan_cmd_send (ipmi_ctx_t ctx,
		   uint8_t lun,
		   uint8_t net_fn,
		   uint8_t authentication_type,
		   uint32_t session_sequence_number,
		   uint32_t session_id,
		   uint8_t rq_seq,
		   const char *password,
		   unsigned int password_len,
		   uint8_t cmd, /* for debug dumping */
		   uint8_t group_extension, /* for debug dumping */
		   fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len = 0;
  int cmd_len = 0;
  int send_len = 0;
  int ret, rv = -1;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd
          && IPMI_BMC_LUN_VALID (lun)
          && IPMI_NET_FN_VALID (net_fn)
          && IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type)
          && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1);

  if (fiid_obj_clear (ctx->io.outofband.rq.obj_rmcp_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rq.obj_rmcp_hdr);
      goto cleanup;
    }
  if (fiid_obj_clear (ctx->io.outofband.rq.obj_lan_session_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rq.obj_lan_session_hdr);
      goto cleanup;
    }
  if (fiid_obj_clear (ctx->io.outofband.rq.obj_lan_msg_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rq.obj_lan_msg_hdr);
      goto cleanup;
    }

  if ((cmd_len = fiid_obj_len_bytes (obj_cmd_rq)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }

  /* variable based on authentication, etc. 1024 extra is enough */
  pkt_len = cmd_len + IPMI_PKT_PAD;

  if (!(pkt = malloc (pkt_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (pkt, '\0', pkt_len);

  if (fill_rmcp_hdr_ipmi (ctx->io.outofband.rq.obj_rmcp_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
                        net_fn,
                        lun,
                        rq_seq,
                        ctx->io.outofband.rq.obj_lan_msg_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_lan_session_hdr (authentication_type,
                            session_sequence_number,
                            session_id,
                            ctx->io.outofband.rq.obj_lan_session_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((send_len = assemble_ipmi_lan_pkt (ctx->io.outofband.rq.obj_rmcp_hdr,
                                         ctx->io.outofband.rq.obj_lan_session_hdr,
                                         ctx->io.outofband.rq.obj_lan_msg_hdr,
                                         obj_cmd_rq,
                                         password,
                                         password_len,
                                         pkt,
                                         pkt_len,
					 IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && send_len)
    _api_lan_dump_rq (ctx,
		      pkt,
		      send_len,
		      cmd,
		      net_fn,
		      group_extension,
		      obj_cmd_rq);

  do
    {
      ret = ipmi_lan_sendto (ctx->io.outofband.sockfd,
                             pkt,
                             send_len,
                             0,
                             (struct sockaddr *)&(ctx->io.outofband.remote_host),
                             sizeof (struct sockaddr_in));
    } while (ret < 0 && errno == EINTR);
  
  if (ret < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (gettimeofday (&ctx->io.outofband.last_send, NULL) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  free (pkt);
  return (rv);
}

/* return receive length on success, 0 on no packet, -1 on error */
static int
_api_lan_cmd_recv (ipmi_ctx_t ctx,
		   void *pkt,
		   unsigned int pkt_len,
		   unsigned int retransmission_count)
{
  struct timeval recv_starttime;
  int recv_len = 0;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd
          && pkt
          && pkt_len);

  if (gettimeofday (&recv_starttime, NULL) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (fiid_obj_clear (ctx->io.outofband.rs.obj_rmcp_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcp_hdr);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_lan_session_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_session_hdr);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_lan_msg_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_msg_hdr);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_lan_msg_trlr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_msg_trlr);
      return (-1);
    }

  recv_len = _api_lan_recvfrom (ctx,
				pkt,
				pkt_len,
				retransmission_count,
				&recv_starttime);

  if (!recv_len)
    return (0); /* resend the request */

  /* achu & hliebig:
   *
   * Premise from ipmitool (http://ipmitool.sourceforge.net/)
   *
   * On some OSes (it seems Unixes), the behavior is to not return
   * port denied errors up to the client for UDP responses (i.e. you
   * need to timeout).  But on some OSes (it seems Windows), the
   * behavior is to return port denied errors up to the user for UDP
   * responses via ECONNRESET or ECONNREFUSED.
   *
   * If this were just the case, we could return or handle errors
   * properly and move on.  However, it's not the case.
   *
   * According to Ipmitool, on some motherboards, both the OS and the
   * BMC are capable of responding to an IPMI request.  That means you
   * can get an ECONNRESET or ECONNREFUSED, then later on, get your
   * real IPMI response.
   *
   * Our solution is copied from Ipmitool, we'll ignore some specific
   * errors and try to read again.
   *
   * If the ECONNREFUSED or ECONNRESET is from the OS, but we will get
   * an IPMI response later, the recvfrom later on gets the packet we
   * want.
   *
   * If the ECONNREFUSED or ECONNRESET is from the OS but there is no
   * BMC (or IPMI disabled, etc.), just do the recvfrom again to
   * eventually get a timeout, which is the behavior we'd like.
   */

  if (recv_len < 0
      && (errno == ECONNRESET
          || errno == ECONNREFUSED))
    {
      recv_len = _api_lan_recvfrom (ctx,
				    pkt,
				    pkt_len,
				    retransmission_count,
				    &recv_starttime);
      
      if (!recv_len)
        return (0); /* resend the request */
    }

  if (recv_len < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  return (recv_len);
}

/* < 0 - error
 * == 1 good packet
 * == 0 bad packet
 */
static int
_api_lan_cmd_wrapper_verify_packet (ipmi_ctx_t ctx,
				    unsigned int internal_workaround_flags,
				    uint8_t authentication_type,
				    int check_authentication_code,
				    uint32_t *session_sequence_number,
				    uint32_t session_id,
				    uint8_t *rq_seq,
				    const char *password,
				    unsigned int password_len,
				    fiid_obj_t obj_cmd_rs)
{
  uint32_t rs_session_id;
  uint32_t rs_session_sequence_number;
  uint64_t val;
  int rv = -1;
  int ret;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd
          && IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type)
          && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid (obj_cmd_rs));

  if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_lan_session_hdr,
                    "session_id",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_session_hdr);
      goto cleanup;
    }
  rs_session_id = val;

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
      if ((ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO)
          && !rs_session_id)
        /* you get a second chance - continue on checking */
        ;
      else
        {

	  /*
	   * IPMI Workaround
	   * 
	   * Discovered on Xyratex HB-F8-SRAY
	   *
	   * The session ID is zero if there is an error.  So if there
	   * is a bad completion code, we'd rather fall through and
	   * continue.  So return "bad packet" if the completion code
	   * is < 0 or 1.
	   */
	  ret = ipmi_check_completion_code_success (obj_cmd_rs);
	  if (ret)
	    {
	      rv = 0;
	      goto cleanup;
	    }
        }
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro X9SCM-iiF, Supermicro X9DRi-F
   *
   * Checksums are computed incorrectly.
   */
  if (!(ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK))
    {
      if ((ret = ipmi_lan_check_checksum (ctx->io.outofband.rs.obj_lan_msg_hdr,
					  obj_cmd_rs,
					  ctx->io.outofband.rs.obj_lan_msg_trlr)) < 0)
	{
	  API_ERRNO_TO_API_ERRNUM (ctx, errno);
	  goto cleanup;
	}
      
      if (!ret)
	{
	  rv = 0;
	  goto cleanup;
	}
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on Xyratex HB-F8-SRAY
   *
   * For some reason, the authentication code is always blank when
   * using "Straight Password Key".
   *
   * Discovered on Intel Windmill/Quanta Winterfell/Wiwynn Windmill
   *
   * Hash is incorrect, unknown why calculation is incorrect.
   */

  if (check_authentication_code
      && !(ctx->flags & IPMI_FLAGS_IGNORE_AUTHENTICATION_CODE)
      && !(ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK))
    {
      if ((ret = ipmi_lan_check_session_authentication_code (ctx->io.outofband.rs.obj_lan_session_hdr,
                                                             ctx->io.outofband.rs.obj_lan_msg_hdr,
                                                             obj_cmd_rs,
                                                             ctx->io.outofband.rs.obj_lan_msg_trlr,
                                                             authentication_type,
                                                             password,
                                                             password_len)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
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
      
      if ((internal_workaround_flags & IPMI_INTERNAL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
          && !ret)
        {
          if ((ret = ipmi_lan_check_session_authentication_code (ctx->io.outofband.rs.obj_lan_session_hdr,
                                                                 ctx->io.outofband.rs.obj_lan_msg_hdr,
                                                                 obj_cmd_rs,
                                                                 ctx->io.outofband.rs.obj_lan_msg_trlr,
                                                                 authentication_type,
                                                                 password,
                                                                 password_len)) < 0)
            {
              API_ERRNO_TO_API_ERRNUM (ctx, errno);
              goto cleanup;
            }
        }

      if (!ret)
        {
          rv = 0;
          goto cleanup;
        }
    }

  if (session_sequence_number)
    {
      if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_lan_session_hdr,
                        "session_sequence_number",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_session_hdr);
          goto cleanup;
        }
      rs_session_sequence_number = val;

      if ((ret = _ipmi_check_session_sequence_number (ctx,
                                                      rs_session_sequence_number)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (!ret)
        {
          rv = 0;
          goto cleanup;
        }
    }

  if ((ret = ipmi_lan_check_rq_seq (ctx->io.outofband.rs.obj_lan_msg_hdr,
                                    (rq_seq) ? *rq_seq : 0)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  rv = 1;
 cleanup:
  return (rv);
}

int
api_lan_cmd_wrapper (ipmi_ctx_t ctx,
		     unsigned int internal_workaround_flags,
		     uint8_t lun,
		     uint8_t net_fn,
		     uint8_t authentication_type,
		     int check_authentication_code,
		     uint32_t *session_sequence_number,
		     uint32_t session_id,
		     uint8_t *rq_seq,
		     const char *password,
		     unsigned int password_len,
		     fiid_obj_t obj_cmd_rq,
		     fiid_obj_t obj_cmd_rs)
{
  int recv_len, ret, rv = -1;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  struct socket_to_close *sockets = NULL;
  uint8_t cmd = 0;             /* used for debugging */
  uint8_t group_extension = 0; /* used for debugging */
  uint64_t val;
  unsigned int intf_flags = IPMI_INTERFACE_FLAGS_DEFAULT;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd
          && IPMI_BMC_LUN_VALID (lun)
          && IPMI_NET_FN_VALID (net_fn)
          && IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type)
          && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    intf_flags |= IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK;

  if (!ctx->io.outofband.last_received.tv_sec
      && !ctx->io.outofband.last_received.tv_usec)
    {
      if (gettimeofday (&ctx->io.outofband.last_received, NULL) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          return (-1);
        }
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* ignore error, continue on */
      if (FIID_OBJ_GET (obj_cmd_rq,
                        "cmd",
                        &val) < 0)
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      else
        cmd = val;

      if (IPMI_NET_FN_GROUP_EXTENSION (net_fn))
	{
	  /* ignore error, continue on */
	  if (FIID_OBJ_GET (obj_cmd_rq,
			    "group_extension_identification",
			    &val) < 0)
	    API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
	  else
	    group_extension = val;
	}
    }

  if (_api_lan_cmd_send (ctx,
			 lun,
			 net_fn,
			 authentication_type,
			 (session_sequence_number) ? *session_sequence_number : 0,
			 session_id,
			 (rq_seq) ? *rq_seq : 0,
			 password,
			 password_len,
			 cmd,  /* for debug dumping */
			 group_extension,  /* for debug dumping */
			 obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      if ((ret = _session_timed_out (ctx)) < 0)
        break;

      if (ret)
        {
	  if (ctx->flags & IPMI_FLAGS_NOSESSION)
	    API_SET_ERRNUM (ctx, IPMI_ERR_MESSAGE_TIMEOUT);
	  else
	    API_SET_ERRNUM (ctx, IPMI_ERR_SESSION_TIMEOUT);
          break;
        }

      memset (pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _api_lan_cmd_recv (ctx,
					 pkt,
					 IPMI_MAX_PKT_LEN,
					 retransmission_count)) < 0)
        break;

      if (!recv_len)
        {
          /* ignore timeout, just cleanly close session */
          if (internal_workaround_flags & IPMI_INTERNAL_WORKAROUND_FLAGS_CLOSE_SESSION_SKIP_RETRANSMIT)
            {
              rv = 0;
              break;
            }

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
          if (internal_workaround_flags & IPMI_INTERNAL_WORKAROUND_FLAGS_GET_SESSION_CHALLENGE)
            {
              struct socket_to_close *s;
              struct sockaddr_in addr;

              if (!(s = (struct socket_to_close *)malloc (sizeof (struct socket_to_close))))
                {
                  API_SET_ERRNUM (ctx, IPMI_ERR_OUT_OF_MEMORY);
                  goto cleanup;
                }
              s->fd = ctx->io.outofband.sockfd;
              s->next = sockets;
              sockets = s;

              if ((ctx->io.outofband.sockfd = socket (AF_INET,
                                                      SOCK_DGRAM,
                                                      0)) < 0)
                {
                  API_ERRNO_TO_API_ERRNUM (ctx, errno);
                  goto cleanup;
                }

              memset (&addr, 0, sizeof (struct sockaddr_in));
              addr.sin_family = AF_INET;
              addr.sin_port = htons (0);
              addr.sin_addr.s_addr = htonl (INADDR_ANY);

              if (bind (ctx->io.outofband.sockfd,
                        (struct sockaddr *)&addr,
                        sizeof (struct sockaddr_in)) < 0)
                {
                  API_ERRNO_TO_API_ERRNUM (ctx, errno);
                  goto cleanup;
                }
            }

          if (_api_lan_cmd_send (ctx,
				 lun,
				 net_fn,
				 authentication_type,
				 (session_sequence_number) ? *session_sequence_number : 0,
				 session_id,
				 (rq_seq) ? *rq_seq : 0,
				 password,
				 password_len,
				 cmd,  /* for debug dumping */
				 group_extension,  /* for debug dumping */
				 obj_cmd_rq) < 0)
            goto cleanup;

          continue;
        }

      /* else received a packet */

      /* its ok to use the "request" net_fn, dump code doesn't care */
      if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
	_api_lan_dump_rs (ctx,
			  pkt,
			  recv_len,
			  cmd,
			  net_fn,
			  group_extension,
			  obj_cmd_rs);

      if ((ret = unassemble_ipmi_lan_pkt (pkt,
					  recv_len,
					  ctx->io.outofband.rs.obj_rmcp_hdr,
					  ctx->io.outofband.rs.obj_lan_session_hdr,
					  ctx->io.outofband.rs.obj_lan_msg_hdr,
					  obj_cmd_rs,
					  ctx->io.outofband.rs.obj_lan_msg_trlr,
					  intf_flags)) < 0)
	{
	  API_ERRNO_TO_API_ERRNUM (ctx, errno);
	  return (-1);
	}
  
      if (!ret)
	continue;
  
      if ((ret = _api_lan_cmd_wrapper_verify_packet (ctx,
						     internal_workaround_flags,
						     authentication_type,
						     check_authentication_code,
						     session_sequence_number,
						     session_id,
						     rq_seq,
						     password,
						     password_len,
						     obj_cmd_rs)) < 0)
	goto cleanup;
      
      if (!ret)
	continue;
      
      if (gettimeofday (&(ctx->io.outofband.last_received), NULL) < 0)
	{
	  API_ERRNO_TO_API_ERRNUM (ctx, errno);
	  return (-1);
	}
      
      rv = 0;
      break;
    }

 cleanup:
  if (session_sequence_number)
    (*session_sequence_number)++;
  if (rq_seq)
    *rq_seq = ((*rq_seq) + 1) % (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);
  while (sockets)
    {
      /* ignore potential error, cleanup path */
      close (sockets->fd);
      sockets = sockets->next;
    }
  return (rv);
}

static int
_ipmi_cmd_send_ipmb (ipmi_ctx_t ctx, fiid_obj_t obj_cmd_rq)
{
  struct ipmi_ctx_target target_save;
  uint8_t tbuf[IPMI_MAX_PKT_LEN];
  fiid_obj_t obj_ipmb_msg_hdr_rq = NULL;
  fiid_obj_t obj_ipmb_msg_rq = NULL;
  fiid_obj_t obj_send_cmd_rs = NULL;
  int len, ret, rv = -1;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1);

  if (!(obj_ipmb_msg_hdr_rq = fiid_obj_create (tmpl_ipmb_msg_hdr_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_ipmb_msg_rq = fiid_obj_create (tmpl_ipmb_msg)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_send_cmd_rs = fiid_obj_create (tmpl_cmd_send_message_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_ipmb_msg_hdr (ctx->target.rs_addr,
                         ctx->target.net_fn,
                         ctx->target.lun,
                         IPMI_SLAVE_ADDRESS_BMC,
                         IPMI_BMC_IPMB_LUN_SMS_MSG_LUN,
                         ctx->io.outofband.rq_seq,
                         obj_ipmb_msg_hdr_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (assemble_ipmi_ipmb_msg (obj_ipmb_msg_hdr_rq,
                              obj_cmd_rq,
                              obj_ipmb_msg_rq,
			      IPMI_INTERFACE_FLAGS_DEFAULT) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  memset (tbuf, '\0', IPMI_MAX_PKT_LEN);
  if ((len = fiid_obj_get_all (obj_ipmb_msg_rq,
                               tbuf,
                               IPMI_MAX_PKT_LEN)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_ipmb_msg_rq);
      goto cleanup;
    }

  /* send_message will send to the BMC, so clear out target information */
  memcpy (&target_save, &ctx->target, sizeof (target_save));
  ctx->target.channel_number_is_set = 0;
  ctx->target.rs_addr_is_set = 0;

  ret = ipmi_cmd_send_message (ctx,
			       target_save.channel_number,
			       IPMI_SEND_MESSAGE_AUTHENTICATION_NOT_REQUIRED,
			       IPMI_SEND_MESSAGE_ENCRYPTION_NOT_REQUIRED,
			       IPMI_SEND_MESSAGE_TRACKING_OPERATION_TRACKING_REQUEST,
			       tbuf,
			       len,
			       obj_send_cmd_rs);
  
  /* restore target info */
  memcpy (&ctx->target, &target_save, sizeof (target_save));
  
  if (ret < 0)
    {
      API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_send_cmd_rs);
      goto cleanup;
    }

  /* "pretend" a request was just sent */
  if (gettimeofday (&ctx->io.outofband.last_send, NULL) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_ipmb_msg_hdr_rq);
  fiid_obj_destroy (obj_ipmb_msg_rq);
  fiid_obj_destroy (obj_send_cmd_rs);
  return (rv);
}

int
api_lan_cmd_wrapper_ipmb (ipmi_ctx_t ctx,
			  fiid_obj_t obj_cmd_rq,
			  fiid_obj_t obj_cmd_rs)
{
  int recv_len, ret, rv = -1;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  uint8_t cmd = 0;             /* used for debugging */
  uint8_t group_extension = 0; /* used for debugging */
  uint8_t rq_seq_orig;
  uint64_t val;
  unsigned int intf_flags = IPMI_INTERFACE_FLAGS_DEFAULT;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_LAN
              || ctx->type == IPMI_DEVICE_LAN_2_0)
          && ctx->io.outofband.sockfd
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    intf_flags |= IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK;

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* ignore error, continue on */
      if (FIID_OBJ_GET (obj_cmd_rq,
                        "cmd",
                        &val) < 0)
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      else
        cmd = val;

      if (IPMI_NET_FN_GROUP_EXTENSION (ctx->target.net_fn))
	{
	  /* ignore error, continue on */
	  if (FIID_OBJ_GET (obj_cmd_rq,
			    "group_extension_identification",
			    &val) < 0)
	    API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
	  else
	    group_extension = val;
	}
    }

  /* for debugging */
  ctx->tmpl_ipmb_cmd_rq = fiid_obj_template (obj_cmd_rq);
  ctx->tmpl_ipmb_cmd_rs = fiid_obj_template (obj_cmd_rs);

  /* ipmb response packet will use the request sequence number from
   * the earlier packet.  Save it for verification.
   */

  rq_seq_orig = ctx->io.outofband.rq_seq;

  if (_ipmi_cmd_send_ipmb (ctx, obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      uint8_t authentication_type;
      unsigned int internal_workaround_flags = 0;

      if ((ret = _session_timed_out (ctx)) < 0)
        break;

      if (ret)
        {
          API_SET_ERRNUM (ctx, IPMI_ERR_SESSION_TIMEOUT);
          break;
        }

      memset (pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _api_lan_cmd_recv (ctx,
					 pkt,
					 IPMI_MAX_PKT_LEN,
					 retransmission_count)) < 0)
        break;
      
      if (!recv_len)
        {
          retransmission_count++;

          /* don't increment sequence numbers, will be done in _ipmi_cmd_send_ipmb */

          /* ipmb response packet will use the request sequence number from
           * the earlier packet.  Save it for verification.
           */

          rq_seq_orig = ctx->io.outofband.rq_seq;

          if (_ipmi_cmd_send_ipmb (ctx, obj_cmd_rq) < 0)
            goto cleanup;

          continue;
        }

      /* else received a packet */

      /* its ok to use the "request" net_fn, dump code doesn't care */
      if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
	_api_lan_dump_rs (ctx,
			  pkt,
			  recv_len,
			  cmd,
			  ctx->target.net_fn,
			  group_extension,
			  obj_cmd_rs);

      if ((ret = unassemble_ipmi_lan_pkt (pkt,
					  recv_len,
					  ctx->io.outofband.rs.obj_rmcp_hdr,
					  ctx->io.outofband.rs.obj_lan_session_hdr,
					  ctx->io.outofband.rs.obj_lan_msg_hdr,
					  obj_cmd_rs,
					  ctx->io.outofband.rs.obj_lan_msg_trlr,
					  intf_flags)) < 0)
	{
	  API_ERRNO_TO_API_ERRNUM (ctx, errno);
	  return (-1);
	}

      if (!ret)
	continue;

      api_lan_cmd_get_session_parameters (ctx,
					  &authentication_type,
					  &internal_workaround_flags);

      if ((ret = _api_lan_cmd_wrapper_verify_packet (ctx,
						     internal_workaround_flags,
						     authentication_type,
						     1, /* always check auth code at this point */
						     &(ctx->io.outofband.session_sequence_number),
						     ctx->io.outofband.session_id,
						     &rq_seq_orig,
						     ctx->io.outofband.password,
						     IPMI_1_5_MAX_PASSWORD_LENGTH,
						     obj_cmd_rs)) < 0)
        goto cleanup;

      if (!ret)
        continue;

      if (gettimeofday (&(ctx->io.outofband.last_received), NULL) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      rv = 0;
      break;
    }

 cleanup:
  ctx->io.outofband.session_sequence_number++;
  /* rq_seq already incremented via _ipmi_cmd_send_ipmb call */
  fiid_template_free (ctx->tmpl_ipmb_cmd_rq);
  ctx->tmpl_ipmb_cmd_rq = NULL;
  fiid_template_free (ctx->tmpl_ipmb_cmd_rs);
  ctx->tmpl_ipmb_cmd_rs = NULL;

  return (rv);
}

static int
_api_lan_rq_seq_init (ipmi_ctx_t ctx)
{
  unsigned int seedp;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->io.outofband.sockfd
	  && (ctx->type == IPMI_DEVICE_LAN
	      || ctx->type == IPMI_DEVICE_LAN_2_0));

  /* Random number generation */
  seedp = (unsigned int) clock () + (unsigned int) time (NULL);
  srand (seedp);

  ctx->io.outofband.rq_seq = (uint8_t)((double)(IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX) * (rand ()/(RAND_MAX + 1.0)));
  return (0);
}

int
api_lan_open_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t authentication_type;
  uint32_t temp_session_id = 0;
  uint8_t challenge_string[IPMI_CHALLENGE_STRING_LENGTH];
  uint32_t initial_outbound_sequence_number = 0;
  char *tmp_username_ptr = NULL;
  char *tmp_password_ptr = NULL;
  int ret, rv = -1;
  uint64_t val;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->io.outofband.sockfd
          && ctx->type == IPMI_DEVICE_LAN
          && strlen (ctx->io.outofband.username) <= IPMI_MAX_USER_NAME_LENGTH
          && strlen (ctx->io.outofband.password) <= IPMI_1_5_MAX_PASSWORD_LENGTH
          && IPMI_1_5_AUTHENTICATION_TYPE_VALID (ctx->io.outofband.authentication_type)
          && IPMI_PRIVILEGE_LEVEL_VALID (ctx->io.outofband.privilege_level));

  if (_api_lan_rq_seq_init (ctx) < 0)
    goto cleanup;

  if (ctx->flags & IPMI_FLAGS_NOSESSION)
    {
      ctx->io.outofband.authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
      goto out;
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                        ctx->io.outofband.privilege_level,
                                                        IPMI_GET_IPMI_V15_DATA,
                                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_lan_cmd_wrapper (ctx,
			   0,
			   IPMI_BMC_IPMB_LUN_BMC,
			   IPMI_NET_FN_APP_RQ,
			   IPMI_AUTHENTICATION_TYPE_NONE,
			   0,
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
        API_SET_ERRNUM (ctx, IPMI_ERR_CONNECTION_TIMEOUT);
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
  if (!(ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES))
    {
      if (strlen (ctx->io.outofband.username))
        tmp_username_ptr = ctx->io.outofband.username;

      if (strlen (ctx->io.outofband.password))
        tmp_password_ptr = ctx->io.outofband.password;

      if ((ret = ipmi_check_authentication_capabilities_username (tmp_username_ptr,
                                                                  tmp_password_ptr,
                                                                  obj_cmd_rs)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (!ret)
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
  if (!(ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION))
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "authentication_status.per_message_authentication",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      ctx->io.outofband.per_msg_auth_disabled = val;
    }
  else
    ctx->io.outofband.per_msg_auth_disabled = 0;

  /* IPMI Workaround (achu)
   *
   * Not discovered yet, assume some motherboard will have it some
   * day.
   *
   * Authentication capabilities flags are not listed properly in the
   * response.  The workaround is to skip these checks.
   */
  if (!(ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES))
    {
      if ((ret = ipmi_check_authentication_capabilities_authentication_type (ctx->io.outofband.authentication_type,
                                                                             obj_cmd_rs)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }
      
      if (!ret)
        {
          API_SET_ERRNUM (ctx, IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE);
          goto cleanup;
        }
    }

  fiid_obj_destroy (obj_cmd_rq);
  obj_cmd_rq = NULL;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_session_challenge_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_session_challenge_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_session_challenge (ctx->io.outofband.authentication_type,
                                      ctx->io.outofband.username,
                                      IPMI_MAX_USER_NAME_LENGTH,
                                      obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_lan_cmd_wrapper (ctx,
			   IPMI_INTERNAL_WORKAROUND_FLAGS_GET_SESSION_CHALLENGE,
			   IPMI_BMC_IPMB_LUN_BMC,
			   IPMI_NET_FN_APP_RQ,
			   IPMI_AUTHENTICATION_TYPE_NONE,
			   0,
			   NULL,
			   0,
			   &(ctx->io.outofband.rq_seq),
			   NULL,
			   0,
			   obj_cmd_rq,
			   obj_cmd_rs) < 0)
    goto cleanup;

  if ((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!ret)
    {
      if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_GET_SESSION_CHALLENGE_INVALID_USERNAME) == 1
          || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_GET_SESSION_CHALLENGE_NULL_USERNAME_NOT_ENABLED) == 1)
        API_SET_ERRNUM (ctx, IPMI_ERR_USERNAME_INVALID);
      else
        API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "temp_session_id",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  temp_session_id = val;

  if (fiid_obj_get_data (obj_cmd_rs,
                         "challenge_string",
                         challenge_string,
                         IPMI_CHALLENGE_STRING_LENGTH) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  fiid_obj_destroy (obj_cmd_rq);
  obj_cmd_rq = NULL;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_activate_session_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_activate_session_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  initial_outbound_sequence_number = rand ();

  if (fill_cmd_activate_session (ctx->io.outofband.authentication_type,
                                 ctx->io.outofband.privilege_level,
                                 challenge_string,
                                 IPMI_CHALLENGE_STRING_LENGTH,
                                 initial_outbound_sequence_number,
                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_lan_cmd_wrapper (ctx,
			   0,
			   IPMI_BMC_IPMB_LUN_BMC,
			   IPMI_NET_FN_APP_RQ,
			   ctx->io.outofband.authentication_type,
			   1,
			   NULL,
			   temp_session_id,
			   &(ctx->io.outofband.rq_seq),
			   ctx->io.outofband.password,
			   IPMI_1_5_MAX_PASSWORD_LENGTH,
			   obj_cmd_rq,
			   obj_cmd_rs) < 0)
    {
      if (ctx->errnum == IPMI_ERR_SESSION_TIMEOUT)
        API_SET_ERRNUM (ctx, IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT);
      goto cleanup;
    }

  if ((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!ret)
    {
      if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_ACTIVATE_SESSION_NO_SESSION_SLOT_AVAILABLE) == 1
          || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_ACTIVATE_SESSION_NO_SLOT_AVAILABLE_FOR_GIVEN_USER) == 1
          || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_ACTIVATE_SESSION_NO_SLOT_AVAILABLE_TO_SUPPORT_USER) == 1)
        API_SET_ERRNUM (ctx, IPMI_ERR_BMC_BUSY);
      else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_ACTIVATE_SESSION_EXCEEDS_PRIVILEGE_LEVEL) == 1)
        API_SET_ERRNUM (ctx, IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
#if 0
      /* achu: noticed this on an Inventec 5441/Dell Xanadu II under
       * some scenarios.  Password Invalid doesn't seem right, b/c on
       * other motherboards it may be a legitimate bad input.  I think
       * it best to comment this out and let the vendor fix their
       * firmware.
       */
       else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1)
	API_SET_ERRNUM (ctx, IPMI_ERR_PASSWORD_INVALID);
#endif
      /*
       * IPMI Workaround
       * 
       * Discovered on Xyratex HB-F8-SRAY
       *
       * For some reason on this system, if you do not specify a
       * privilege level of Admin, this completion code will always be
       * returned.  Reason unknown.  This isn't the best/right error
       * to return, but it will atleast point the user to a way to
       * work around the problem.
       */
       else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL) == 1)
	 API_SET_ERRNUM (ctx, IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
      else
        API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "session_id",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  ctx->io.outofband.session_id = val;

  /* achu: On some buggy BMCs the initial outbound sequence number on
   * the activate session response is off by one.  So we just accept
   * whatever sequence number they give us even if it isn't the
   * initial outbound sequence number.
   */
  if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_lan_session_hdr,
                    "session_sequence_number",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_session_hdr);
      goto cleanup;
    }
  ctx->io.outofband.highest_received_sequence_number = val;

  /* IPMI Workaround (achu)
   *
   * Discovered on Sun Fire 4100.
   *
   * The session sequence numbers for IPMI 1.5 are the wrong endian.
   * So we have to flip the bits to workaround it.
   */
  if (ctx->workaround_flags_outofband & IPMI_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER)
    {
      uint32_t tmp_session_sequence_number = ctx->io.outofband.highest_received_sequence_number;

      ctx->io.outofband.highest_received_sequence_number =
        ((tmp_session_sequence_number & 0xFF000000) >> 24)
        | ((tmp_session_sequence_number & 0x00FF0000) >> 8)
        | ((tmp_session_sequence_number & 0x0000FF00) << 8)
        | ((tmp_session_sequence_number & 0x000000FF) << 24);
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "initial_inbound_sequence_number",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  ctx->io.outofband.session_sequence_number = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "authentication_type",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  authentication_type = val;

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
      && authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    ctx->io.outofband.per_msg_auth_disabled = 0;

  fiid_obj_destroy (obj_cmd_rq);
  obj_cmd_rq = NULL;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  /* if privilege_level == IPMI_PRIVILEGE_LEVEL_USER we shouldn't have
   * to call this, b/c it should be USER by default.  But I don't
   * trust IPMI implementations.  Do it anyways.
   */

  /* achu: At this point in time, the session is actually setup
   * legitimately, so we can use the actual set session privilege
   * level API function.
   */
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_session_privilege_level_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ipmi_cmd_set_session_privilege_level (ctx,
                                            ctx->io.outofband.privilege_level,
                                            obj_cmd_rs) < 0)
    {
      if (ctx->errnum == IPMI_ERR_BAD_COMPLETION_CODE)
        {
          if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_SET_SESSION_PRIVILEGE_LEVEL_REQUESTED_LEVEL_NOT_AVAILABLE_FOR_USER) == 1
              || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_SET_SESSION_PRIVILEGE_LEVEL_REQUESTED_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT) == 1)
            API_SET_ERRNUM (ctx, IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
        }
      ERR_TRACE (ipmi_ctx_strerror (ctx->errnum), ctx->errnum);
      goto cleanup;
    }

 out:
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
api_lan_close_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t authentication_type;
  unsigned int internal_workaround_flags = 0;
  int ret, rv = -1;

  /* Do not use ipmi_cmd_close_session(), we use a close session retransmit workaround */

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN
          && ctx->io.outofband.sockfd);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_close_session_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_close_session_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (fill_cmd_close_session (ctx->io.outofband.session_id,
                              NULL,
                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  api_lan_cmd_get_session_parameters (ctx,
				      &authentication_type,
				      &internal_workaround_flags);

  internal_workaround_flags |= IPMI_INTERNAL_WORKAROUND_FLAGS_CLOSE_SESSION_SKIP_RETRANSMIT;
  if (api_lan_cmd_wrapper (ctx,
			   internal_workaround_flags,
			   IPMI_BMC_IPMB_LUN_BMC,
			   IPMI_NET_FN_APP_RQ,
			   authentication_type,
			   1,
			   &(ctx->io.outofband.session_sequence_number),
			   ctx->io.outofband.session_id,
			   &(ctx->io.outofband.rq_seq),
			   ctx->io.outofband.password,
			   IPMI_1_5_MAX_PASSWORD_LENGTH,
			   obj_cmd_rq,
			   obj_cmd_rs) < 0)
    goto cleanup;

  /* Check completion code just for tracing, but don't return error */

  if ((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto out;
    }

  if (!ret)
    {
      API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto out;
    }
  
 out:
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static void
_api_lan_2_0_dump_rq (ipmi_ctx_t ctx,
		      uint8_t authentication_algorithm,
		      uint8_t integrity_algorithm,
		      uint8_t confidentiality_algorithm,
		      const void *integrity_key,
		      unsigned int integrity_key_len,
		      const void *confidentiality_key,
		      unsigned int confidentiality_key_len,
		      const void *pkt,
		      unsigned int pkt_len,
		      uint8_t cmd,
		      uint8_t net_fn,
		      uint8_t group_extension,
		      fiid_obj_t obj_cmd_rq)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
          && IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && pkt
          && pkt_len
          && fiid_obj_valid (obj_cmd_rq));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rq)))
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
      const char *cmd_str = NULL;

      /* Handle a few IPMI 2.0 special cases */
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_open_session_request) == 1)
	cmd_str = DEBUG_UTIL_OPEN_SESSION_STR;
      else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_1) == 1)
	cmd_str = DEBUG_UTIL_RAKP_1_STR;
      else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_3) == 1)
	cmd_str = DEBUG_UTIL_RAKP_3_STR;

      if (cmd_str)
	debug_hdr_str (DEBUG_UTIL_TYPE_IPMI_2_0,
		       DEBUG_UTIL_DIRECTION_REQUEST,
		       DEBUG_UTIL_FLAGS_DEFAULT,
		       cmd_str,
		       hdrbuf,
		       DEBUG_UTIL_HDR_BUFLEN);
      else
	debug_hdr_cmd (DEBUG_UTIL_TYPE_IPMI_2_0,
		       DEBUG_UTIL_DIRECTION_REQUEST,
		       net_fn,
		       cmd,
		       group_extension,
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

static void
_api_lan_2_0_dump_rs (ipmi_ctx_t ctx,
		      uint8_t authentication_algorithm,
		      uint8_t integrity_algorithm,
		      uint8_t confidentiality_algorithm,
		      const void *integrity_key,
		      unsigned int integrity_key_len,
		      const void *confidentiality_key,
		      unsigned int confidentiality_key_len,
		      const void *pkt,
		      unsigned int pkt_len,
		      uint8_t cmd,
		      uint8_t net_fn,
		      uint8_t group_extension,
		      fiid_obj_t obj_cmd_rs)
{
  fiid_field_t *tmpl_cmd = NULL;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
          && IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && pkt
          && pkt_len
          && fiid_obj_valid (obj_cmd_rs));

  /* Don't cleanup/return an error here.  It's just debug code. */

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rs)))
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
      const char *cmd_str = NULL;

      /* Handle a few IPMI 2.0 special cases */
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_open_session_response) == 1)
	cmd_str = DEBUG_UTIL_OPEN_SESSION_STR;
      else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_2) == 1)
	cmd_str = DEBUG_UTIL_RAKP_2_STR;
      else if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_4) == 1)
	cmd_str = DEBUG_UTIL_RAKP_4_STR;

      if (cmd_str)
	debug_hdr_str (DEBUG_UTIL_TYPE_IPMI_2_0,
		       DEBUG_UTIL_DIRECTION_RESPONSE,
		       DEBUG_UTIL_FLAGS_DEFAULT,
		       cmd_str,
		       hdrbuf,
		       DEBUG_UTIL_HDR_BUFLEN);
      else
	debug_hdr_cmd (DEBUG_UTIL_TYPE_IPMI_2_0,
		       DEBUG_UTIL_DIRECTION_RESPONSE,
		       net_fn,
		       cmd,
		       group_extension,
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

static int
_api_lan_2_0_cmd_send (ipmi_ctx_t ctx,
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
		       const void *integrity_key,
		       unsigned int integrity_key_len,
		       const void *confidentiality_key,
		       unsigned int confidentiality_key_len,
		       const char *password,
		       unsigned int password_len,
		       uint8_t cmd, /* for debug dumping */
		       uint8_t group_extension, /* for debug dumping */
		       fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len = 0;
  int cmd_len = 0;
  int send_len = 0;
  int ret, rv = -1;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->io.outofband.sockfd
          && IPMI_BMC_LUN_VALID (lun)
          && IPMI_NET_FN_VALID (net_fn)
          && (payload_type == IPMI_PAYLOAD_TYPE_IPMI
              || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
          && IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID (payload_authenticated)
          && IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID (payload_encrypted)
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
          && IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && !(password && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1);

  if (fiid_obj_clear (ctx->io.outofband.rq.obj_rmcp_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rq.obj_rmcp_hdr);
      goto cleanup;
    }
  if (fiid_obj_clear (ctx->io.outofband.rq.obj_rmcpplus_session_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rq.obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  if (fiid_obj_clear (ctx->io.outofband.rq.obj_lan_msg_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rq.obj_lan_msg_hdr);
      goto cleanup;
    }
  if (fiid_obj_clear (ctx->io.outofband.rq.obj_rmcpplus_session_trlr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rq.obj_rmcpplus_session_trlr);
      goto cleanup;
    }

  if ((cmd_len = fiid_obj_len_bytes (obj_cmd_rq)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }

  /* variable based on authentication, etc. 1024 extra is enough */
  pkt_len = cmd_len + IPMI_PKT_PAD;

  if (!(pkt = malloc (pkt_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (pkt, '\0', pkt_len);

  if (fill_rmcp_hdr_ipmi (ctx->io.outofband.rq.obj_rmcp_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_rmcpplus_session_hdr (payload_type,
                                 payload_authenticated,
                                 payload_encrypted,
                                 0, /* oem_iana */
                                 0, /* oem_payload_id */
                                 session_id,
                                 session_sequence_number,
                                 ctx->io.outofband.rq.obj_rmcpplus_session_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
                        net_fn,
                        lun,
                        rq_seq,
                        ctx->io.outofband.rq.obj_lan_msg_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_rmcpplus_session_trlr (ctx->io.outofband.rq.obj_rmcpplus_session_trlr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((send_len = assemble_ipmi_rmcpplus_pkt (authentication_algorithm,
                                              integrity_algorithm,
                                              confidentiality_algorithm,
                                              integrity_key,
                                              integrity_key_len,
                                              confidentiality_key,
                                              confidentiality_key_len,
                                              password,
                                              password_len,
                                              ctx->io.outofband.rq.obj_rmcp_hdr,
                                              ctx->io.outofband.rq.obj_rmcpplus_session_hdr,
                                              ctx->io.outofband.rq.obj_lan_msg_hdr,
                                              obj_cmd_rq,
                                              ctx->io.outofband.rq.obj_rmcpplus_session_trlr,
                                              pkt,
                                              pkt_len,
					      IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && send_len)
    _api_lan_2_0_dump_rq (ctx,
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
			  group_extension,
			  obj_cmd_rq);

  do
    {
      ret = ipmi_rmcpplus_sendto (ctx->io.outofband.sockfd,
				  pkt,
				  send_len,
				  0,
				  (struct sockaddr *)&(ctx->io.outofband.remote_host),
				  sizeof (struct sockaddr_in));
    } while (ret < 0 && errno == EINTR);
  
  if (ret < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (gettimeofday (&ctx->io.outofband.last_send, NULL) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  free (pkt);
  return (0);
}

/* return receive length on success, 0 on no packet, -1 on error */
static int
_api_lan_2_0_cmd_recv (ipmi_ctx_t ctx,
		       uint8_t authentication_algorithm,
		       uint8_t integrity_algorithm,
		       uint8_t confidentiality_algorithm,
		       const void *integrity_key,
		       unsigned int integrity_key_len,
		       const void *confidentiality_key,
		       unsigned int confidentiality_key_len,
		       void *pkt,
		       unsigned int pkt_len,
		       unsigned int retransmission_count)
{
  struct timeval recv_starttime;
  int recv_len = 0;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->io.outofband.sockfd
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
          && IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && pkt
          && pkt_len);

  if (gettimeofday (&recv_starttime, NULL) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (fiid_obj_clear (ctx->io.outofband.rs.obj_rmcp_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcp_hdr);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_rmcpplus_session_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_lan_msg_hdr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_msg_hdr);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_rmcpplus_payload) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_payload);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_lan_msg_trlr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_lan_msg_trlr);
      return (-1);
    }
  if (fiid_obj_clear (ctx->io.outofband.rs.obj_rmcpplus_session_trlr) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_trlr);
      return (-1);
    }

  recv_len = _api_lan_recvfrom (ctx,
				pkt,
				pkt_len,
				retransmission_count,
				&recv_starttime);

  if (!recv_len)
    return (0); /* resend the request */

  /* achu & hliebig:
   *
   * Premise from ipmitool (http://ipmitool.sourceforge.net/)
   *
   * On some OSes (it seems Unixes), the behavior is to not return
   * errors up to the client for UDP responses (i.e. you need to
   * timeout).  But on some OSes (it seems Windows), the behavior is
   * to return port denied errors up to the user for UDP responses.
   *
   * In addition (according to Ipmitool), a read may return
   * ECONNREFUSED or ECONNRESET if both the OS and BMC respond to an
   * IPMI request.
   *
   * If the ECONNREFUSED or ECONNRESET is from the OS, but we will get
   * an IPMI response later, we just do the recvfrom again to get the
   * packet we expect.
   *
   * If the ECONNREFUSED or ECONNRESET is from the OS but there is no
   * BMC, just do the recvfrom again to give us the eventually
   * timeout.
   */

  if (recv_len < 0
      && (errno == ECONNRESET
          || errno == ECONNREFUSED))
    {
      recv_len = _api_lan_recvfrom (ctx,
				    pkt,
				    pkt_len,
				    retransmission_count,
				    &recv_starttime);
      
      if (!recv_len)
        return (0); /* resend the request */
    }
 
  if (recv_len < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  return (recv_len);
}

/* < 0 - error
 * == 1 good packet
 * == 0 bad packet
 */
static int
_api_lan_2_0_cmd_wrapper_verify_packet (ipmi_ctx_t ctx,
					uint8_t payload_type,
					uint8_t *message_tag,
					uint32_t *session_sequence_number,
					uint32_t session_id,
					uint8_t *rq_seq,
					uint8_t integrity_algorithm,
					const void *integrity_key,
					unsigned int integrity_key_len,
					const char *password,
					unsigned int password_len,
					fiid_obj_t obj_cmd_rs,
					const void *pkt,
					unsigned int pkt_len)
{
  uint8_t l_payload_type;
  uint32_t l_session_id;
  uint8_t l_message_tag;
  uint32_t rs_session_sequence_number;
  uint8_t rmcpplus_status_code;
  uint64_t val;
  int ret, rv = -1;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0
          && ctx->io.outofband.sockfd
          && (payload_type == IPMI_PAYLOAD_TYPE_IPMI
              || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
          && IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
          && !(password && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid (obj_cmd_rs)
          && pkt
          && pkt_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
                        "payload_type",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
          goto cleanup;
        }
      l_payload_type = val;

      if (l_payload_type != IPMI_PAYLOAD_TYPE_IPMI)
        {
          rv = 0;
          goto cleanup;
        }

      if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
                        "session_id",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
          goto cleanup;
        }
      l_session_id = val;

      if (l_session_id != ctx->io.outofband.remote_console_session_id)
        {
          rv = 0;
          goto cleanup;
        }

      /* IPMI Workaround (achu)
       *
       * Discovered on Supermicro X9SCM-iiF, Supermicro X9DRi-F
       *
       * Checksums are computed incorrectly.
       */
      if (!(ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK))
	{
	  if ((ret = ipmi_lan_check_checksum (ctx->io.outofband.rs.obj_lan_msg_hdr,
					      obj_cmd_rs,
					      ctx->io.outofband.rs.obj_lan_msg_trlr)) < 0)
	    {
	      API_ERRNO_TO_API_ERRNUM (ctx, errno);
	      goto cleanup;
	    }
	  
	  if (!ret)
	    {
	      rv = 0;
	      goto cleanup;
	    }
	}

      if ((ret = ipmi_rmcpplus_check_packet_session_authentication_code (integrity_algorithm,
                                                                         pkt,
                                                                         pkt_len,
                                                                         integrity_key,
                                                                         integrity_key_len,
                                                                         password,
                                                                         password_len,
                                                                         ctx->io.outofband.rs.obj_rmcpplus_session_trlr)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (!ret)
        {
          rv = 0;
          goto cleanup;
        }

      if (session_sequence_number)
        {
          if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
                            "session_sequence_number",
                            &val) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
              goto cleanup;
            }
          rs_session_sequence_number = val;

          if ((ret = _ipmi_check_session_sequence_number (ctx,
                                                          rs_session_sequence_number)) < 0)
            {
              API_ERRNO_TO_API_ERRNUM (ctx, errno);
              goto cleanup;
            }

          if (!ret)
            {
              rv = 0;
              goto cleanup;
            }
        }

      if ((ret = ipmi_lan_check_rq_seq (ctx->io.outofband.rs.obj_lan_msg_hdr,
                                        (rq_seq) ? *rq_seq : 0)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (!ret)
        {
          rv = 0;
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    {
      if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
                        "payload_type",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
          goto cleanup;
        }
      l_payload_type = val;
      
      if (l_payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
        {
          rv = 0;
          goto cleanup;
        }

      if (message_tag)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "message_tag",
                            &val) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
          l_message_tag = val;
          
          if (l_message_tag != *message_tag)
            {
              rv = 0;
              goto cleanup;
            }
        }

      /* There is no guarantee that other data (authentication keys,
       * session id's, etc.) in the RAKP response will be valid if
       * there is a status code error.  So we check this status code
       * along with this stuff.
       */

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "rmcpplus_status_code",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      rmcpplus_status_code = val;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "remote_console_session_id",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      l_session_id = val;
      
      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS
          && l_session_id != ctx->io.outofband.remote_console_session_id)
        {
          rv = 0;
          goto cleanup;
        }

    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    {
      if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
                        "payload_type",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
          goto cleanup;
        }
      l_payload_type = val;

      if (l_payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
        {
          rv = 0;
          goto cleanup;
        }

      if (message_tag)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "message_tag",
                            &val) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
          l_message_tag = val;
          
          if (l_message_tag != *message_tag)
            {
              rv = 0;
              goto cleanup;
            }
        }

      /* There is no guarantee that other data (authentication keys,
       * session id's, etc.) in the RAKP response will be valid if
       * there is a status code error.  So we check this status code
       * along with this stuff.
       */

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "rmcpplus_status_code",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      rmcpplus_status_code = val;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "remote_console_session_id",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      l_session_id = val;

      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS
          && l_session_id != ctx->io.outofband.remote_console_session_id)
        {
          rv = 0;
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      if (FIID_OBJ_GET (ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
                        "payload_type",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
          goto cleanup;
        }
      l_payload_type = val;
      
      if (l_payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
        {
          rv = 0;
          goto cleanup;
        }

      if (message_tag)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "message_tag",
                            &val) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
          l_message_tag = val;
          
          if (l_message_tag != *message_tag)
            {
              rv = 0;
              goto cleanup;
            }
        }

      /* There is no guarantee that other data (e.g. authentication
       * keys, session id's, etc.) in the RAKP response will be valid
       * if there is a status code error.  So we check this status
       * code along with this stuff.
       */

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "rmcpplus_status_code",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      rmcpplus_status_code = val;
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "remote_console_session_id",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      l_session_id = val;

      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_ERRORS
          && l_session_id != ctx->io.outofband.remote_console_session_id)
        {
          rv = 0;
          goto cleanup;
        }
    }

  rv = 1;
 cleanup:
  return (rv);
}

int
api_lan_2_0_cmd_wrapper (ipmi_ctx_t ctx,
			 unsigned int internal_workaround_flags,
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
			 const void *integrity_key,
			 unsigned int integrity_key_len,
			 const void *confidentiality_key,
			 unsigned int confidentiality_key_len,
			 const char *password,
			 unsigned int password_len,
			 fiid_obj_t obj_cmd_rq,
			 fiid_obj_t obj_cmd_rs)
{
  int recv_len, ret, rv = -1;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  uint8_t cmd = 0;             /* used for debugging */
  uint8_t group_extension = 0; /* used for debugging */
  uint64_t val;
  unsigned int intf_flags = IPMI_INTERFACE_FLAGS_DEFAULT;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0
          && ctx->io.outofband.sockfd
          && IPMI_BMC_LUN_VALID (lun)
          && IPMI_NET_FN_VALID (net_fn)
          && (payload_type == IPMI_PAYLOAD_TYPE_IPMI
              || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
              || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
          && IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID (payload_authenticated)
          && IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID (payload_encrypted)
	  && (!message_tag
	      || (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
		  || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
		  || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3))
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
          && IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && !(password && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    intf_flags |= IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK;

  if (!ctx->io.outofband.last_received.tv_sec
      && !ctx->io.outofband.last_received.tv_usec)
    {
      if (gettimeofday (&ctx->io.outofband.last_received, NULL) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          return (-1);
        }
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* ignore error, continue on */
      if (FIID_OBJ_GET (obj_cmd_rq,
                        "cmd",
                        &val) < 0)
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      else
        cmd = val;

      if (IPMI_NET_FN_GROUP_EXTENSION (net_fn))
	{
	  /* ignore error, continue on */
	  if (FIID_OBJ_GET (obj_cmd_rq,
			    "group_extension_identification",
			    &val) < 0)
	    API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
	  else
	    group_extension = val;
	}
    }

  if (_api_lan_2_0_cmd_send (ctx,
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
			     group_extension, /* for debug dumping */
			     obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      if ((ret = _session_timed_out (ctx)) < 0)
        break;

      if (ret)
        {
          API_SET_ERRNUM (ctx, IPMI_ERR_SESSION_TIMEOUT);
          break;
        }

      /* its ok to use the "request" net_fn, dump code doesn't care */
      memset (pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _api_lan_2_0_cmd_recv (ctx,
					     authentication_algorithm,
					     integrity_algorithm,
					     confidentiality_algorithm,
					     integrity_key,
					     integrity_key_len,
					     confidentiality_key,
					     confidentiality_key_len,
					     pkt,
					     IPMI_MAX_PKT_LEN,
					     retransmission_count)) < 0)
        break;

      if (!recv_len)
        {
          /* ignore timeout, just cleanly close session */
          if (internal_workaround_flags & IPMI_INTERNAL_WORKAROUND_FLAGS_CLOSE_SESSION_SKIP_RETRANSMIT)
            {
              rv = 0;
              break;
            }

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
              if (fiid_obj_set (obj_cmd_rq, "message_tag", (*message_tag)) < 0)
                {
                  API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
                  goto cleanup;
                }
            }

          if (_api_lan_2_0_cmd_send (ctx,
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
				     group_extension, /* for debug dumping */
				     obj_cmd_rq) < 0)
            goto cleanup;

          continue;
        }

      /* else received a packet */

      if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
	_api_lan_2_0_dump_rs (ctx,
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
			      group_extension,
			      obj_cmd_rs);

      if ((ret = unassemble_ipmi_rmcpplus_pkt (authentication_algorithm,
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
					       ctx->io.outofband.rs.obj_rmcpplus_session_trlr,
					       intf_flags)) < 0)
	{
	  API_ERRNO_TO_API_ERRNUM (ctx, errno);
	  return (-1);
	}
      
      if (!ret)
	continue;

      if ((ret = _api_lan_2_0_cmd_wrapper_verify_packet (ctx,
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

      if (gettimeofday (&ctx->io.outofband.last_received, NULL) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      rv = 0;
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
  return (rv);
}

int
api_lan_2_0_cmd_wrapper_ipmb (ipmi_ctx_t ctx,
			      fiid_obj_t obj_cmd_rq,
			      fiid_obj_t obj_cmd_rs)
{
  int recv_len, ret, rv = -1;
  unsigned int retransmission_count = 0;
  uint8_t pkt[IPMI_MAX_PKT_LEN];
  uint8_t cmd = 0;             /* used for debugging */
  uint8_t group_extension = 0; /* used for debugging */
  uint8_t rq_seq_orig;
  uint64_t val;
  unsigned int intf_flags = IPMI_INTERFACE_FLAGS_DEFAULT;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0
          && ctx->io.outofband.sockfd
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1
          && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    intf_flags |= IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK;

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* ignore error, continue on */
      if (FIID_OBJ_GET (obj_cmd_rq,
                        "cmd",
                        &val) < 0)
        API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      else
        cmd = val;

      if (IPMI_NET_FN_GROUP_EXTENSION (ctx->target.net_fn))
	{
	  /* ignore error, continue on */
	  if (FIID_OBJ_GET (obj_cmd_rq,
			    "group_extension_identification",
			    &val) < 0)
	    API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
	  else
	    group_extension = val;
	}
    }

  /* for debugging */
  ctx->tmpl_ipmb_cmd_rq = fiid_obj_template (obj_cmd_rq);
  ctx->tmpl_ipmb_cmd_rs = fiid_obj_template (obj_cmd_rs);

  /* ipmb response packet will use the request sequence number from
   * the earlier packet.  Save it for verification.
   */

  rq_seq_orig = ctx->io.outofband.rq_seq;

  if (_ipmi_cmd_send_ipmb (ctx, obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      uint8_t payload_authenticated;
      uint8_t payload_encrypted;

      if ((ret = _session_timed_out (ctx)) < 0)
        break;

      if (ret)
        {
          API_SET_ERRNUM (ctx, IPMI_ERR_SESSION_TIMEOUT);
          break;
        }

      /* its ok to use the "request" net_fn, dump code doesn't care */
      memset (pkt, '\0', IPMI_MAX_PKT_LEN);
      if ((recv_len = _api_lan_2_0_cmd_recv (ctx,
					     ctx->io.outofband.authentication_algorithm,
					     ctx->io.outofband.integrity_algorithm,
					     ctx->io.outofband.confidentiality_algorithm,
					     ctx->io.outofband.integrity_key_ptr,
					     ctx->io.outofband.integrity_key_len,
					     ctx->io.outofband.confidentiality_key_ptr,
					     ctx->io.outofband.confidentiality_key_len,
					     pkt,
					     IPMI_MAX_PKT_LEN,
					     retransmission_count)) < 0)
        {
          rv = -1;
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

          if (_ipmi_cmd_send_ipmb (ctx, obj_cmd_rq) < 0)
            goto cleanup;

          continue;
        }

      /* else received a packet */

      if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
	_api_lan_2_0_dump_rs (ctx,
			      ctx->io.outofband.authentication_algorithm,
			      ctx->io.outofband.integrity_algorithm,
			      ctx->io.outofband.confidentiality_algorithm,
			      ctx->io.outofband.integrity_key_ptr,
			      ctx->io.outofband.integrity_key_len,
			      ctx->io.outofband.confidentiality_key_ptr,
			      ctx->io.outofband.confidentiality_key_len,
			      pkt,
			      recv_len,
			      cmd,
			      ctx->target.net_fn,
			      group_extension,
			      obj_cmd_rs);

      if ((ret = unassemble_ipmi_rmcpplus_pkt (ctx->io.outofband.authentication_algorithm,
					       ctx->io.outofband.integrity_algorithm,
					       ctx->io.outofband.confidentiality_algorithm,
					       ctx->io.outofband.integrity_key_ptr,
					       ctx->io.outofband.integrity_key_len,
					       ctx->io.outofband.confidentiality_key_ptr,
					       ctx->io.outofband.confidentiality_key_len,
					       pkt,
					       recv_len,
					       ctx->io.outofband.rs.obj_rmcp_hdr,
					       ctx->io.outofband.rs.obj_rmcpplus_session_hdr,
					       ctx->io.outofband.rs.obj_rmcpplus_payload,
					       ctx->io.outofband.rs.obj_lan_msg_hdr,
					       obj_cmd_rs,
					       ctx->io.outofband.rs.obj_lan_msg_trlr,
					       ctx->io.outofband.rs.obj_rmcpplus_session_trlr,
					       intf_flags)) < 0)
	{
	  API_ERRNO_TO_API_ERRNUM (ctx, errno);
	  return (-1);
	}
      
      if (!ret)
	continue;

      api_lan_2_0_cmd_get_session_parameters (ctx,
					      &payload_authenticated,
					      &payload_encrypted);

      if ((ret = _api_lan_2_0_cmd_wrapper_verify_packet (ctx,
							 IPMI_PAYLOAD_TYPE_IPMI,
							 NULL,
							 &(ctx->io.outofband.session_sequence_number),
							 ctx->io.outofband.managed_system_session_id,
							 &rq_seq_orig,
							 ctx->io.outofband.integrity_algorithm,
							 ctx->io.outofband.integrity_key_ptr,
							 ctx->io.outofband.integrity_key_len,
							 strlen (ctx->io.outofband.password) ? ctx->io.outofband.password : NULL,
							 strlen (ctx->io.outofband.password),
							 obj_cmd_rs,
							 pkt,
							 recv_len)) < 0)
        goto cleanup;

      if (!ret)
        continue;

      if (gettimeofday (&(ctx->io.outofband.last_received), NULL) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      rv = 0;
      break;
    }

 cleanup:
  ctx->io.outofband.session_sequence_number++;
  /* rq_seq already incremented via _ipmi_cmd_send_ipmb call */
  fiid_template_free (ctx->tmpl_ipmb_cmd_rq);
  ctx->tmpl_ipmb_cmd_rq = NULL;
  fiid_template_free (ctx->tmpl_ipmb_cmd_rs);
  ctx->tmpl_ipmb_cmd_rs = NULL;

  return (rv);
}

int
api_lan_2_0_open_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t rmcpplus_status_code;
  uint8_t remote_console_random_number[IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH];
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int managed_system_random_number_len;
  uint8_t managed_system_guid[IPMI_MANAGED_SYSTEM_GUID_LENGTH];
  int managed_system_guid_len;
  uint8_t key_exchange_authentication_code[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
  int key_exchange_authentication_code_len;
  uint8_t message_tag;
  char *username;
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  unsigned int username_len;
  char *password;
  unsigned int password_len;
  uint8_t authentication_algorithm = 0; /* init to 0 to remove gcc warning */
  uint8_t requested_maximum_privilege;
  uint8_t name_only_lookup;
  char *tmp_username_ptr = NULL;
  char *tmp_password_ptr = NULL;
  void *tmp_k_g_ptr = NULL;
  int ret, rv = -1;
  uint64_t val;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->io.outofband.sockfd
          && ctx->type == IPMI_DEVICE_LAN_2_0
          && strlen (ctx->io.outofband.username) <= IPMI_MAX_USER_NAME_LENGTH
          && strlen (ctx->io.outofband.password) <= IPMI_2_0_MAX_PASSWORD_LENGTH
          && IPMI_PRIVILEGE_LEVEL_VALID (ctx->io.outofband.privilege_level)
          && IPMI_CIPHER_SUITE_ID_SUPPORTED (ctx->io.outofband.cipher_suite_id)
          && ctx->io.outofband.sik_key_ptr == ctx->io.outofband.sik_key
          && ctx->io.outofband.sik_key_len == IPMI_MAX_SIK_KEY_LENGTH
          && ctx->io.outofband.integrity_key_ptr == ctx->io.outofband.integrity_key
          && ctx->io.outofband.integrity_key_len == IPMI_MAX_INTEGRITY_KEY_LENGTH
          && ctx->io.outofband.confidentiality_key_ptr == ctx->io.outofband.confidentiality_key
          && ctx->io.outofband.confidentiality_key_len == IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH);

  if (_api_lan_rq_seq_init (ctx) < 0)
    goto cleanup;

  /* Unlike IPMI 1.5, there is no initial sequence number negotiation, so we don't
   * start at a random sequence number.
   */
  ctx->io.outofband.session_sequence_number = 1;

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                        ctx->io.outofband.privilege_level,
                                                        IPMI_GET_IPMI_V20_EXTENDED_DATA,
                                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* This portion of the protocol is sent via IPMI 1.5 */
  if (api_lan_cmd_wrapper (ctx,
			   0,
			   IPMI_BMC_IPMB_LUN_BMC,
			   IPMI_NET_FN_APP_RQ,
			   IPMI_AUTHENTICATION_TYPE_NONE,
			   0,
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
        API_SET_ERRNUM (ctx, IPMI_ERR_CONNECTION_TIMEOUT);
      goto cleanup;
    }

  if ((ret = ipmi_check_authentication_capabilities_ipmi_2_0 (obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!ret)
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
  if (!(ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES))
    {
      if (strlen (ctx->io.outofband.username))
        tmp_username_ptr = ctx->io.outofband.username;
      
      if (strlen (ctx->io.outofband.password))
        tmp_password_ptr = ctx->io.outofband.password;
      
      if ((ret = ipmi_check_authentication_capabilities_username (tmp_username_ptr,
                                                                  tmp_password_ptr,
                                                                  obj_cmd_rs)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }
      
      if (!ret)
        {
          ctx->errnum = IPMI_ERR_USERNAME_INVALID;
          goto cleanup;
        }

      if (ctx->io.outofband.k_g_configured)
        tmp_k_g_ptr = ctx->io.outofband.k_g;

      if ((ret = ipmi_check_authentication_capabilities_k_g (tmp_k_g_ptr,
                                                             obj_cmd_rs)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (!ret)
        {
          API_SET_ERRNUM (ctx, IPMI_ERR_K_G_INVALID);
          goto cleanup;
        }
    }

  fiid_obj_destroy (obj_cmd_rq);
  obj_cmd_rq = NULL;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_rmcpplus_open_session_request)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_rmcpplus_open_session_response)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  message_tag = (uint8_t)rand ();

  /* In IPMI 2.0, session_ids of 0 are special */
  do
    {
      if (ipmi_get_random (&(ctx->io.outofband.remote_console_session_id),
                           sizeof (ctx->io.outofband.remote_console_session_id)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }
    } while (!ctx->io.outofband.remote_console_session_id);

  if (ipmi_cipher_suite_id_to_algorithms (ctx->io.outofband.cipher_suite_id,
                                          &(ctx->io.outofband.authentication_algorithm),
                                          &(ctx->io.outofband.integrity_algorithm),
                                          &(ctx->io.outofband.confidentiality_algorithm)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

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
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION
      || ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION
      || ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE)
    requested_maximum_privilege = ctx->io.outofband.privilege_level;
  else
    requested_maximum_privilege = IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL;

  if (fill_rmcpplus_open_session (message_tag,
                                  requested_maximum_privilege,
                                  ctx->io.outofband.remote_console_session_id,
                                  ctx->io.outofband.authentication_algorithm,
                                  ctx->io.outofband.integrity_algorithm,
                                  ctx->io.outofband.confidentiality_algorithm,
                                  obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_lan_2_0_cmd_wrapper (ctx,
			       0,
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

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "rmcpplus_status_code",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    {
      if (rmcpplus_status_code == RMCPPLUS_STATUS_NO_CIPHER_SUITE_MATCH_WITH_PROPOSED_SECURITY_ALGORITHMS)
        API_SET_ERRNUM (ctx, IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INVALID_ROLE)
	API_SET_ERRNUM (ctx, IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION
               || rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME)
        API_SET_ERRNUM (ctx, IPMI_ERR_BMC_BUSY);
      else
        API_SET_ERRNUM (ctx, IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE);
      goto cleanup;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The Intel's return IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL instead
   * of an actual privilege, so have to pass the actual privilege
   * we want to use.
   */
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      uint8_t maximum_privilege_level;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "maximum_privilege_level",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      maximum_privilege_level = val;
      
      ret = (maximum_privilege_level == requested_maximum_privilege) ? 1 : 0;
    }
  else
    {
      if ((ret = ipmi_check_open_session_maximum_privilege (ctx->io.outofband.privilege_level,
                                                            obj_cmd_rs)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }

  if (!ret)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "managed_system_session_id",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  ctx->io.outofband.managed_system_session_id = val;

  fiid_obj_destroy (obj_cmd_rq);
  obj_cmd_rq = NULL;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_rmcpplus_rakp_message_1)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_rmcpplus_rakp_message_2)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ipmi_get_random (remote_console_random_number,
                       IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The username must be padded despite explicitly not being
   * allowed.  "No Null characters (00h) are allowed in the name".
   * Table 13-11 in the IPMI 2.0 spec.
   *
   * achu: This should only be done for RAKP 1 message, RAKP 2 check,
   * and session key creation.
   */
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      memset (username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (strlen (ctx->io.outofband.username))
        strcpy (username_buf, ctx->io.outofband.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      if (strlen (ctx->io.outofband.username))
        username = ctx->io.outofband.username;
      else
        username = NULL;
      username_len = (username) ? strlen (username) : 0;
    }

  /* achu: Unlike IPMI 1.5, the length of the username must be actual
   * length, it can't be the maximum length.
   */
  if (fill_rmcpplus_rakp_message_1 (message_tag,
                                    ctx->io.outofband.managed_system_session_id,
                                    remote_console_random_number,
                                    IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                    ctx->io.outofband.privilege_level,
                                    IPMI_NAME_ONLY_LOOKUP,
                                    username,
                                    username_len,
                                    obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_lan_2_0_cmd_wrapper (ctx,
			       0,
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

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "rmcpplus_status_code",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    {
      if (rmcpplus_status_code == RMCPPLUS_STATUS_UNAUTHORIZED_NAME)
        API_SET_ERRNUM (ctx, IPMI_ERR_USERNAME_INVALID);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_UNAUTHORIZED_ROLE_OR_PRIVILEGE_LEVEL_REQUESTED)
        API_SET_ERRNUM (ctx, IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION
               || rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME)
        API_SET_ERRNUM (ctx, IPMI_ERR_BMC_BUSY);
      else
        API_SET_ERRNUM (ctx, IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE);
      goto cleanup;
    }

  if ((managed_system_random_number_len = fiid_obj_get_data (obj_cmd_rs,
                                                             "managed_system_random_number",
                                                             managed_system_random_number,
                                                             IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  if ((managed_system_guid_len = fiid_obj_get_data (obj_cmd_rs,
                                                    "managed_system_guid",
                                                    managed_system_guid,
                                                    IPMI_MANAGED_SYSTEM_GUID_LENGTH)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  if (managed_system_random_number_len != IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH
      || managed_system_guid_len != IPMI_MANAGED_SYSTEM_GUID_LENGTH)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_IPMI_ERROR);
      goto cleanup;
    }

  if (strlen (ctx->io.outofband.password))
    password = ctx->io.outofband.password;
  else
    password = NULL;
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
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION
      && ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;

  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION)
    {
      uint8_t keybuf[IPMI_MAX_PKT_LEN];
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

      if ((keybuf_len = fiid_obj_get_data (obj_cmd_rs,
                                           "key_exchange_authentication_code",
                                           keybuf,
                                           IPMI_MAX_PKT_LEN)) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }

      if (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
          && keybuf_len == 1)
        {
          if (fiid_obj_clear_field (obj_cmd_rs,
                                    "key_exchange_authentication_code") < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
        }
      else if (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1
               && keybuf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (obj_cmd_rs,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
        }
      else if (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
               && keybuf_len == (IPMI_HMAC_MD5_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (obj_cmd_rs,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_MD5_DIGEST_LENGTH) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
        }
      else if (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA256
               && keybuf_len == (IPMI_HMAC_SHA256_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_set_data (obj_cmd_rs,
                                 "key_exchange_authentication_code",
                                 keybuf,
                                 IPMI_HMAC_SHA256_DIGEST_LENGTH) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
        }
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
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION
      && (ctx->io.outofband.authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1))
    {
      uint8_t buf[IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH];
      int buf_len;

      if ((buf_len = fiid_obj_get_data (obj_cmd_rs,
                                        "key_exchange_authentication_code",
                                        buf,
                                        IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }

      if (buf_len == (IPMI_HMAC_SHA1_DIGEST_LENGTH + 1))
        {
          if (fiid_obj_clear_field (obj_cmd_rs,
                                    "key_exchange_authentication_code") < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }

          if (fiid_obj_set_data (obj_cmd_rs,
                                 "key_exchange_authentication_code",
                                 buf,
                                 IPMI_HMAC_SHA1_DIGEST_LENGTH) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
        }
    }

  if ((ret = ipmi_rmcpplus_check_rakp_2_key_exchange_authentication_code (ctx->io.outofband.authentication_algorithm,
                                                                          password,
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
                                                                          obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!ret)
    {
      /* IPMI Compliance Issue
       *
       * On some systems, password could be correct, but privilege is
       * too high.  The error is b/c the privilege error is not
       * handled properly in the open session stage (i.e. they tell me
       * I can authenticate at a high privilege level, that in reality
       * is not allowed).  Dunno how to deal with this.
       */
      API_SET_ERRNUM (ctx, IPMI_ERR_PASSWORD_INVALID);
      goto cleanup;
    }

  /* achu: note, for INTEL_2_0 workaround, this must have the username/password adjustments */
  if (ipmi_calculate_rmcpplus_session_keys (ctx->io.outofband.authentication_algorithm,
                                            ctx->io.outofband.integrity_algorithm,
                                            ctx->io.outofband.confidentiality_algorithm,
                                            password,
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
                                            &(ctx->io.outofband.confidentiality_key_len)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* achu: If INTEL_2_0 workaround is set, get back to original username &
   * username_len, because that isn't needed for the RAKP3/4 part.
   */
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      if (strlen (ctx->io.outofband.username))
        username = ctx->io.outofband.username;
      else
        username = NULL;
      username_len = (username) ? strlen (username) : 0;
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
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    name_only_lookup = IPMI_USER_NAME_PRIVILEGE_LOOKUP;
  else
    name_only_lookup = IPMI_NAME_ONLY_LOOKUP;

  if ((key_exchange_authentication_code_len = ipmi_calculate_rakp_3_key_exchange_authentication_code (ctx->io.outofband.authentication_algorithm,
                                                                                                      password,
                                                                                                      password_len,
                                                                                                      managed_system_random_number,
                                                                                                      managed_system_random_number_len,
                                                                                                      ctx->io.outofband.remote_console_session_id,
                                                                                                      name_only_lookup,
                                                                                                      ctx->io.outofband.privilege_level,
                                                                                                      username,
                                                                                                      username_len,
                                                                                                      key_exchange_authentication_code,
                                                                                                      IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  fiid_obj_destroy (obj_cmd_rq);
  obj_cmd_rq = NULL;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_rmcpplus_rakp_message_3)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_rmcpplus_rakp_message_4)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_rmcpplus_rakp_message_3 (message_tag,
                                    RMCPPLUS_STATUS_NO_ERRORS,
                                    ctx->io.outofband.managed_system_session_id,
                                    key_exchange_authentication_code,
                                    key_exchange_authentication_code_len,
                                    obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_lan_2_0_cmd_wrapper (ctx,
			       0,
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

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "rmcpplus_status_code",
                    &val) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  rmcpplus_status_code = val;

  if (rmcpplus_status_code != RMCPPLUS_STATUS_NO_ERRORS)
    {
      if (rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION
          || rmcpplus_status_code == RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME)
        API_SET_ERRNUM (ctx, IPMI_ERR_BMC_BUSY);
      else if (rmcpplus_status_code == RMCPPLUS_STATUS_INVALID_INTEGRITY_CHECK_VALUE)
        /* XXX: achu: some systems, password could be correct, but
         * privilege used in hashing is incorrect on the BMC side
         * (OPEN_SESSION_PRIVILEGE workaround).
         */
        API_SET_ERRNUM (ctx, IPMI_ERR_PASSWORD_INVALID);
      else
        API_SET_ERRNUM (ctx, IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE);
      goto cleanup;
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

  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
      else if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1;
      else if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        authentication_algorithm = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5;
      else if (ctx->io.outofband.integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        {
          /* achu: I have thus far been unable to reverse engineer this
           * corner case.  Since we cannot provide a reasonable two
           * part authentication, we're going to error out.
           */
          API_SET_ERRNUM (ctx, IPMI_ERR_IPMI_ERROR);
          goto cleanup;
        }
    }
  else
    authentication_algorithm = ctx->io.outofband.authentication_algorithm;

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro X8DTG, Supermicro X8DTU, Intel
   * S5500WBV/Penguin Relion 700
   *
   * For whatever reason, with cipher suite 0, the RAKP 4 response
   * returns with an Integrity Check Value when it should be empty.
   */
  
  if (ctx->workaround_flags_outofband_2_0 & IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE
      && !ctx->io.outofband.cipher_suite_id)
    {
      if (fiid_obj_clear_field (obj_cmd_rs,
				"integrity_check_value") < 0)
	{
	  API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
	  goto cleanup;
	}
    }
  
  if ((ret = ipmi_rmcpplus_check_rakp_4_integrity_check_value (authentication_algorithm,
                                                               ctx->io.outofband.sik_key_ptr,
                                                               ctx->io.outofband.sik_key_len,
                                                               remote_console_random_number,
                                                               IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                                               ctx->io.outofband.managed_system_session_id,
                                                               managed_system_guid,
                                                               managed_system_guid_len,
                                                               obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (!ret)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_K_G_INVALID);
      goto cleanup;
    }

  fiid_obj_destroy (obj_cmd_rq);
  obj_cmd_rq = NULL;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  /* if privilege_level == IPMI_PRIVILEGE_LEVEL_USER we shouldn't have
   * to call this, b/c it should be USER by default.  But I don't
   * trust IPMI implementations.  Do it anyways.
   */

  /* achu: At this point in time, the session is actually setup
   * legitimately, so we can use the actual set session privilege
   * level API function.
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_session_privilege_level_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ipmi_cmd_set_session_privilege_level (ctx,
                                            ctx->io.outofband.privilege_level,
                                            obj_cmd_rs) < 0)
    {
      if (ctx->errnum == IPMI_ERR_BAD_COMPLETION_CODE)
        {
          if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_SET_SESSION_PRIVILEGE_LEVEL_REQUESTED_LEVEL_NOT_AVAILABLE_FOR_USER) == 1
              || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_SET_SESSION_PRIVILEGE_LEVEL_REQUESTED_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT) == 1)
            API_SET_ERRNUM (ctx, IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED);
        }
      ERR_TRACE (ipmi_ctx_strerror (ctx->errnum), ctx->errnum);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
api_lan_2_0_close_session (ipmi_ctx_t ctx)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t payload_authenticated;
  uint8_t payload_encrypted;
  unsigned int internal_workaround_flags = 0;
  int ret, rv = -1;

  /* Do not use ipmi_cmd_close_session(), we use a close session retransmit workaround */

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0
          && ctx->io.outofband.sockfd);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_close_session_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_close_session_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (fill_cmd_close_session (ctx->io.outofband.managed_system_session_id,
                              NULL,
                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  api_lan_2_0_cmd_get_session_parameters (ctx,
					  &payload_authenticated,
					  &payload_encrypted);
  
  internal_workaround_flags |= IPMI_INTERNAL_WORKAROUND_FLAGS_CLOSE_SESSION_SKIP_RETRANSMIT;
  
  if (api_lan_2_0_cmd_wrapper (ctx,
			       internal_workaround_flags,
			       IPMI_BMC_IPMB_LUN_BMC,
			       IPMI_NET_FN_APP_RQ,
			       IPMI_PAYLOAD_TYPE_IPMI,
			       payload_authenticated,
			       payload_encrypted,
			       NULL,
			       &(ctx->io.outofband.session_sequence_number),
			       ctx->io.outofband.managed_system_session_id,
			       &(ctx->io.outofband.rq_seq),
			       ctx->io.outofband.authentication_algorithm,
			       ctx->io.outofband.integrity_algorithm,
			       ctx->io.outofband.confidentiality_algorithm,
			       ctx->io.outofband.integrity_key_ptr,
			       ctx->io.outofband.integrity_key_len,
			       ctx->io.outofband.confidentiality_key_ptr,
			       ctx->io.outofband.confidentiality_key_len,
			       strlen (ctx->io.outofband.password) ? ctx->io.outofband.password : NULL,
			       strlen (ctx->io.outofband.password),
			       obj_cmd_rq,
			       obj_cmd_rs) < 0)
    goto cleanup;
  
  /* Check completion code just for tracing, but don't return error */

  if ((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto out;
    }

  if (!ret)
    {
      API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto out;
    }
  
 out:
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
