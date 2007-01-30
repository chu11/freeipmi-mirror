/*****************************************************************************\
 *  $Id: ipmi_monitoring_ipmi_communication.c,v 1.1 2007-01-30 21:52:57 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <stdarg.h>
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
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
#include <sys/select.h>
#include <assert.h>
#include <errno.h>
#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_fiid_wrappers.h"
#include "ipmi_monitoring_ipmi_communication.h"

#include "secure.h"
#include "timeval.h"

#define GETHOSTBYNAME_AUX_BUFLEN 1024

static void
_inband_cleanup(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (c->comm.kcs_ctx)
    {
      ipmi_kcs_ctx_destroy(c->comm.kcs_ctx);
      c->comm.kcs_ctx = NULL;
    }
}

static int
_inband_init(ipmi_monitoring_ctx_t c)
{
  struct ipmi_locate_info l;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(!c->comm.communication_type);

  memset(&l, '\0', sizeof(struct ipmi_locate_info));
  memset(&(c->comm), '\0', sizeof(struct ipmi_monitoring_communication));
  
  if (ipmi_locate(IPMI_INTERFACE_KCS, &l) < 0) 
    {
      IPMI_MONITORING_DEBUG(("ipmi_locate: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
  
  if (!(c->comm.kcs_ctx = ipmi_kcs_ctx_create()))
    {
      IPMI_MONITORING_DEBUG(("ipmi_kcs_ctx_create: %s", strerror(errno)));
      if (errno == EPERM || errno == EACCES)
        c->errnum = IPMI_MONITORING_ERR_PERMISSION;
      else
        c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
      
  if (ipmi_kcs_ctx_set_driver_address(c->comm.kcs_ctx, l.driver_address) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_kcs_ctx_set_driver_address: %s", ipmi_kcs_ctx_strerror(ipmi_kcs_ctx_errnum(c->comm.kcs_ctx))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
  
  if (ipmi_kcs_ctx_set_register_space(c->comm.kcs_ctx, l.reg_space) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_kcs_ctx_set_register_space: %s", ipmi_kcs_ctx_strerror(ipmi_kcs_ctx_errnum(c->comm.kcs_ctx))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
      
  if (ipmi_kcs_ctx_io_init(c->comm.kcs_ctx) < 0)
    {
      if (ipmi_kcs_ctx_errnum(c->comm.kcs_ctx) == IPMI_KCS_CTX_ERR_PERMISSION)
        c->errnum = IPMI_MONITORING_ERR_PERMISSION;
      else
        c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      IPMI_MONITORING_DEBUG(("ipmi_kcs_ctx_io_init: %s", ipmi_kcs_ctx_strerror(ipmi_kcs_ctx_errnum(c->comm.kcs_ctx))));
      goto cleanup;
    }

  return 0;

 cleanup:
  _inband_cleanup(c);
  return -1;
}
             
static int
_ipmi_packet_assemble(ipmi_monitoring_ctx_t c,
                      uint8_t authentication_type,
                      uint32_t inbound_sequence_number,
                      uint32_t session_id,
                      uint8_t *authentication_code_data,
                      uint32_t authentication_code_data_len,
                      uint8_t lun,
                      uint8_t net_fn,
                      fiid_obj_t obj_cmd,
                      uint8_t *buf,
                      uint32_t buflen)
{
  int32_t pkt_len;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj_cmd));
  assert(buf);
  assert(buflen);

  if (Fiid_obj_clear(c, c->comm.obj_rmcp_hdr_rq) < 0)
    return -1;
  if (Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rq) < 0)
    return -1;
  if (Fiid_obj_clear(c, c->comm.obj_lan_msg_hdr_rq) < 0)
    return -1;

  if (fill_rmcp_hdr_ipmi(c->comm.obj_rmcp_hdr_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_rmcp_hdr_ipmi: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }

  if (fill_lan_session_hdr(authentication_type,
                           inbound_sequence_number,
                           session_id,
                           c->comm.obj_lan_session_hdr_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_lan_session_hdr: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }

  if (fill_lan_msg_hdr(net_fn,
                       lun,
                       c->comm.requester_sequence_number,
                       c->comm.obj_lan_msg_hdr_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_lan_msg_hdr: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }

  if ((pkt_len = assemble_ipmi_lan_pkt(c->comm.obj_rmcp_hdr_rq,
                                       c->comm.obj_lan_session_hdr_rq,
                                       c->comm.obj_lan_msg_hdr_rq,
                                       obj_cmd,
                                       authentication_code_data,
                                       authentication_code_data_len,
                                       buf,
                                       buflen)) < 0)
    {
      IPMI_MONITORING_DEBUG(("assemble_ipmi_lan_pkt: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }

  return pkt_len;
}

static int
_session_timeout(ipmi_monitoring_ctx_t c)
{
  struct timeval current;
  struct timeval timeout;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  timeval_add_ms(&(c->comm.last_ipmi_packet_received), c->comm.session_timeout_len, &timeout);
  if (gettimeofday(&current, NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      return -1;
    }
  
  return timeval_gt(&current, &timeout);
}

static int
_calculate_timeout(ipmi_monitoring_ctx_t c, struct timeval *timeout)
{
  struct timeval current;
  struct timeval session_timeout;
  struct timeval session_timeout_val;
  struct timeval retransmission_timeout;
  struct timeval retransmission_timeout_val;
  unsigned int retransmission_timeout_len;
  unsigned int retransmission_timeout_multiplier;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(timeout);

  if (gettimeofday(&current, NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      return -1;
    }

  timeval_add_ms(&c->comm.last_ipmi_packet_received, c->comm.session_timeout_len, &session_timeout);
  timeval_sub(&session_timeout, &current, &session_timeout_val);

  if (c->comm.retransmission_backoff_count)
    retransmission_timeout_multiplier = (c->comm.retransmission_count / c->comm.retransmission_backoff_count) + 1;
  else
    retransmission_timeout_multiplier = 1;

  retransmission_timeout_len = c->comm.retransmission_timeout_len * retransmission_timeout_multiplier;

  timeval_add_ms(&c->comm.last_ipmi_packet_sent, retransmission_timeout_len, &retransmission_timeout);
  timeval_sub(&retransmission_timeout, &current, &retransmission_timeout_val);

  if (timeval_lt(&retransmission_timeout_val, &session_timeout_val))
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

static int
_receive_response(ipmi_monitoring_ctx_t c,
                  fiid_obj_t obj_cmd)

{
  uint8_t buf[IPMI_MONITORING_PACKET_BUFLEN];
  int buflen;
  struct timeval timeout;
  struct sockaddr_in from;
  unsigned int fromlen = sizeof(struct sockaddr_in);
  fd_set rds;
  int n, rv  = 0;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj_cmd));

  FD_ZERO(&rds);
  FD_SET(c->comm.ipmi_fd, &rds);

  if (_session_timeout(c))
    {
      c->errnum = IPMI_MONITORING_ERR_SESSION_TIMEOUT;
      return -1;
    }

  if (_calculate_timeout(c, &timeout) < 0)
    return -1;
  
  if ((n = select(c->comm.ipmi_fd + 1, &rds, NULL, NULL, &timeout)) < 0)
    {
      IPMI_MONITORING_DEBUG(("select: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      return -1;
    }

  if (n)
    {
      if ((buflen = ipmi_lan_recvfrom(c->comm.ipmi_fd,
                                      buf,
                                      IPMI_MONITORING_PACKET_BUFLEN,
                                      0,
                                      (struct sockaddr *)&from,
                                      &fromlen)) < 0)
        {
          IPMI_MONITORING_DEBUG(("ipmi_lan_recvfrom: %s", strerror(errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL;
          return -1;
        }

      if ((rv = unassemble_ipmi_lan_pkt(buf,
                                        buflen,
                                        c->comm.obj_rmcp_hdr_rs,
                                        c->comm.obj_lan_session_hdr_rs,
                                        c->comm.obj_lan_msg_hdr_rs,
                                        obj_cmd,
                                        c->comm.obj_lan_msg_trlr_rs)) < 0)
        {
          IPMI_MONITORING_DEBUG(("unassemble_ipmi_lan_pkt: %s", strerror(errno)));
          c->errnum = IPMI_MONITORING_ERR_IPMI;
          return -1;
        }
      
      return 1;
    }
  
  return 0;
}

static int
_check_checksum(ipmi_monitoring_ctx_t c, fiid_obj_t obj_cmd)
{
  int rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj_cmd));

  if ((rv = ipmi_lan_check_checksum(c->comm.obj_lan_msg_hdr_rs,
                                    obj_cmd,
                                    c->comm.obj_lan_msg_trlr_rs)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_check_checksum: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }

  if (!rv)
    IPMI_MONITORING_DEBUG(("checksum failed"));

  return rv;
}

static int
_check_authentication_code(ipmi_monitoring_ctx_t c, fiid_obj_t obj_cmd)
{
  int rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj_cmd));

  if ((rv = ipmi_lan_check_session_authentication_code(c->comm.obj_lan_session_hdr_rs,
                                                       c->comm.obj_lan_msg_hdr_rs,
                                                       obj_cmd,
                                                       c->comm.obj_lan_msg_trlr_rs,
                                                       c->comm.authentication_type,
                                                       strlen(c->comm.password) ? c->comm.password : NULL,
                                                       strlen(c->comm.password))) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_check_checksum: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }
  
  if (!rv)
    IPMI_MONITORING_DEBUG(("authentication_code failed"));

  return rv;
}

static int
_check_sequence_number(ipmi_monitoring_ctx_t c)
{
  uint32_t shift_num, wrap_val, max_sequence_number = 0xFFFFFFFF;
  uint32_t session_sequence_number;
  uint64_t val;
  int rv = 0;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (Fiid_obj_get(c,
                   c->comm.obj_lan_session_hdr_rs,
                   "session_sequence_number",
                   &val) < 0)
    return -1;
  session_sequence_number = val;

  /* Drop duplicate packet */
  if (session_sequence_number == c->comm.highest_received_sequence_number)
    goto out;

  /* Check if sequence number is greater than highest received and is
   * within range
   */
  if (c->comm.highest_received_sequence_number > (max_sequence_number - IPMI_MONITORING_SEQUENCE_NUMBER_WINDOW))
    {
      wrap_val = IPMI_MONITORING_SEQUENCE_NUMBER_WINDOW - (max_sequence_number - c->comm.highest_received_sequence_number) - 1;

      if (session_sequence_number > c->comm.highest_received_sequence_number || session_sequence_number <= wrap_val)
        {
          if (session_sequence_number > c->comm.highest_received_sequence_number && session_sequence_number <= max_sequence_number)
            shift_num = session_sequence_number - c->comm.highest_received_sequence_number;
          else
            shift_num = session_sequence_number + (max_sequence_number - c->comm.highest_received_sequence_number) + 1;

          c->comm.highest_received_sequence_number = session_sequence_number;
          c->comm.previously_received_list <<= shift_num;
          c->comm.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }
  else
    {
      if (session_sequence_number > c->comm.highest_received_sequence_number
          && (session_sequence_number - c->comm.highest_received_sequence_number) <= IPMI_MONITORING_SEQUENCE_NUMBER_WINDOW)
        {
          shift_num = (session_sequence_number - c->comm.highest_received_sequence_number);
          c->comm.highest_received_sequence_number = session_sequence_number;
          c->comm.previously_received_list <<= shift_num;
          c->comm.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }

  /* Check if sequence number is lower than highest received, is
   * within range, and hasn't been seen yet
   */
  if (c->comm.highest_received_sequence_number < IPMI_MONITORING_SEQUENCE_NUMBER_WINDOW)
    {
      uint32_t wrap_val = max_sequence_number - (IPMI_MONITORING_SEQUENCE_NUMBER_WINDOW - c->comm.highest_received_sequence_number) + 1;

      if (session_sequence_number < c->comm.highest_received_sequence_number || session_sequence_number >= wrap_val)
        {
          if (session_sequence_number > c->comm.highest_received_sequence_number && session_sequence_number <= max_sequence_number)
            shift_num = c->comm.highest_received_sequence_number + (max_sequence_number - session_sequence_number) + 1;
          else
            shift_num = c->comm.highest_received_sequence_number - session_sequence_number;

          /* Duplicate packet check*/
          if (c->comm.previously_received_list & (0x1 << (shift_num - 1)))
            goto out;

          c->comm.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }
  else
    {
      if (session_sequence_number < c->comm.highest_received_sequence_number
          && session_sequence_number >= (c->comm.highest_received_sequence_number - IPMI_MONITORING_SEQUENCE_NUMBER_WINDOW))
        {
          shift_num = c->comm.highest_received_sequence_number - session_sequence_number;

          /* Duplicate packet check*/
          if (c->comm.previously_received_list & (0x1 << (shift_num - 1)))
            goto out;

          c->comm.previously_received_list |= (0x1 << (shift_num - 1));
          rv++;
        }
    }

 out:
  if (!rv)
    IPMI_MONITORING_DEBUG(("sequence_number failed"));

  return rv;
}

static int
_check_session_id(ipmi_monitoring_ctx_t c)
{
  uint32_t session_id, expected_session_id;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (Fiid_obj_get(c,
                   c->comm.obj_lan_session_hdr_rs,
                   "session_id",
                   &val) < 0)
    return -1;
  session_id = val;
  
  if (Fiid_obj_get(c,
                   c->comm.obj_activate_session_rs,
                   "session_id",
                   &val) < 0)
    return -1;
  expected_session_id = val;
  
  if (c->comm.workaround_flags & IPMI_MONITORING_WORKAROUND_FLAGS_SESSION_ID_ZERO)
    {
      if (session_id != expected_session_id && session_id)
        IPMI_MONITORING_DEBUG(("session id failed"));
      return (((session_id == expected_session_id) || !session_id) ? 1 : 0);
    }

  if (session_id != expected_session_id)
    IPMI_MONITORING_DEBUG(("session id failed"));
  return ((session_id == expected_session_id) ? 1 : 0);
}

static int
_check_rq_seq(ipmi_monitoring_ctx_t c)
{
  int rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if ((rv = ipmi_lan_check_rq_seq(c->comm.obj_lan_msg_hdr_rs, 
                                  c->comm.requester_sequence_number)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_check_rq_seq: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }

  if (!rv)
    IPMI_MONITORING_DEBUG(("rq_seq failed"));

  return rv;
}

static int
_check_completion_code(ipmi_monitoring_ctx_t c, 
                       fiid_obj_t obj_cmd,
                       uint8_t *comp_code)
{
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj_cmd));

  if (Fiid_obj_get(c,
                   obj_cmd,
                   "comp_code",
                   &val) < 0)
    return -1;
  
  if (val != IPMI_COMP_CODE_COMMAND_SUCCESS)
    IPMI_MONITORING_DEBUG(("comp_code failed: 0x%X", ((uint8_t)val)));

  if (comp_code)
    *comp_code = val;

  return ((val == IPMI_COMP_CODE_COMMAND_SUCCESS) ? 1 : 0);
}

static int
_send_get_channel_authentication_capabilities(ipmi_monitoring_ctx_t c)
{
  uint8_t buf[IPMI_MONITORING_PACKET_BUFLEN];
  int buflen, rv = -1;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (fill_cmd_get_channel_authentication_capabilities(IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                       c->comm.privilege_level,
                                                       c->comm.obj_get_channel_authentication_capabilities_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_cmd_get_channel_authentication_capabilities: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }

  c->comm.requester_sequence_number++;
  if (c->comm.requester_sequence_number > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
    c->comm.requester_sequence_number = 0;
  
  if ((buflen = _ipmi_packet_assemble(c,
                                      IPMI_AUTHENTICATION_TYPE_NONE,
                                      0,
                                      0,
                                      NULL,
                                      0,
                                      IPMI_BMC_IPMB_LUN_BMC,                                      
                                      IPMI_NET_FN_APP_RQ,
                                      c->comm.obj_get_channel_authentication_capabilities_rq,
                                      buf,
                                      IPMI_MONITORING_PACKET_BUFLEN)) < 0)
    goto cleanup;
  
  ipmi_monitoring_outofband_dump(c->comm.hostname,
                                 "Get Channel Authentication Capabilities Request",
                                 c->comm.obj_rmcp_hdr_rq,
                                 c->comm.obj_lan_session_hdr_rq,
                                 c->comm.obj_lan_msg_hdr_rq,
                                 c->comm.obj_get_channel_authentication_capabilities_rq,
                                 NULL);

  if (ipmi_lan_sendto(c->comm.ipmi_fd, 
                      buf, 
                      buflen,
                      0,
                      (struct sockaddr *)&(c->comm.addr),
                      sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_sendto: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  if (gettimeofday(&(c->comm.last_ipmi_packet_sent), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  secure_memset(buf, '\0', IPMI_MONITORING_PACKET_BUFLEN);
  return rv;
}

static int
_check_authentication_capabilities(ipmi_monitoring_ctx_t c)
{
  uint8_t authentication_type_none;
  uint8_t authentication_type_md2;
  uint8_t authentication_type_md5;
  uint8_t authentication_type_straight_password_key;
  uint8_t authentication_status_anonymous_login;
  uint8_t authentication_status_null_username;
  uint8_t authentication_status_non_null_username;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (Fiid_obj_get(c,
                   c->comm.obj_get_channel_authentication_capabilities_rs,
                   "authentication_type.none",
                   &val) < 0)
    return -1;
  authentication_type_none = val;
  if (Fiid_obj_get(c,
                   c->comm.obj_get_channel_authentication_capabilities_rs,
                   "authentication_type.md2",
                   &val) < 0)
    return -1;
  authentication_type_md2 = val;
  if (Fiid_obj_get(c,
                   c->comm.obj_get_channel_authentication_capabilities_rs,
                   "authentication_type.md5",
                   &val) < 0)
    return -1;
  authentication_type_md5 = val;
  if (Fiid_obj_get(c,
                   c->comm.obj_get_channel_authentication_capabilities_rs,
                   "authentication_type.straight_password_key",
                   &val) < 0)
    return -1;
  authentication_type_straight_password_key = val;
  if (Fiid_obj_get(c,
                   c->comm.obj_get_channel_authentication_capabilities_rs,
                   "authentication_status.anonymous_login",
                   &val) < 0)
    return -1;
  authentication_status_anonymous_login = val;
  if (Fiid_obj_get(c,
                   c->comm.obj_get_channel_authentication_capabilities_rs,
                   "authentication_status.null_username",
                   &val) < 0)
    return -1;
  authentication_status_null_username = val;
  if (Fiid_obj_get(c,
                   c->comm.obj_get_channel_authentication_capabilities_rs,
                   "authentication_status.non_null_username",
                   &val) < 0)
    return -1;
  authentication_status_non_null_username = val;

  /* Does the remote BMC's authentication configuration support
   * our username/password combination
   */
  if ((!strlen(c->comm.username) && !strlen(c->comm.password)
       && !authentication_status_anonymous_login
       && !authentication_type_none)
      || (!strlen(c->comm.username)
          && !authentication_status_anonymous_login
          && !authentication_status_null_username)
      || (strlen(c->comm.username)
          && !authentication_status_non_null_username))
    {
#ifndef NDEBUG
      c->errnum = IPMI_MONITORING_ERR_USERNAME;
#else  /* !NDEBUG */
      c->errnum = IPMI_MONITORING_ERR_PERMISSION;
#endif /* !NDEBUG */
      return -1;
    }

  if ((c->comm.authentication_type == IPMI_AUTHENTICATION_TYPE_NONE
       && !authentication_type_none)
      || (c->comm.authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
          && !authentication_type_md2)
      || (c->comm.authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
          && !authentication_type_md5)
      || (c->comm.authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
          && !authentication_type_straight_password_key))
    {
#ifndef NDEBUG
      c->errnum = IPMI_MONITORING_ERR_AUTHENTICATION_TYPE;
#else  /* !NDEBUG */
      c->errnum = IPMI_MONITORING_ERR_PERMISSION;
#endif /* !NDEBUG */
      return -1;
    }

  return 0;
}

static int
_get_channel_authentication_capabilities(ipmi_monitoring_ctx_t c)
{
  int rv = -1;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (_send_get_channel_authentication_capabilities(c) < 0)
    goto cleanup;

  while (1)
    {
      if ((ret = _receive_response(c, c->comm.obj_get_channel_authentication_capabilities_rs)) < 0)
        goto cleanup;

      if (!ret)
        {
          if (_send_get_channel_authentication_capabilities(c) < 0)
            goto cleanup;
          c->comm.retransmission_count++;
          continue;
        }

      ipmi_monitoring_outofband_dump(c->comm.hostname,
                                     "Get Channel Authentication Capabilities Response",
                                     c->comm.obj_rmcp_hdr_rs,
                                     c->comm.obj_lan_session_hdr_rs,
                                     c->comm.obj_lan_msg_hdr_rs,
                                     c->comm.obj_get_channel_authentication_capabilities_rs,
                                     c->comm.obj_lan_msg_trlr_rs);

      if ((ret = _check_checksum(c, c->comm.obj_get_channel_authentication_capabilities_rs)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_rq_seq(c)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_completion_code(c, 
					c->comm.obj_get_channel_authentication_capabilities_rs,
					NULL)) < 0)
        goto cleanup;

      if (!ret)
        {
          c->errnum = IPMI_MONITORING_ERR_IPMI;
          goto cleanup;
        }

      break;

    continue_loop:
      /* Clear out data */
      Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
    }

  c->comm.retransmission_count = 0;
  if (gettimeofday(&(c->comm.last_ipmi_packet_received), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  if (_check_authentication_capabilities(c) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  /* Clear out data */
  Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
  return rv;
}

static int
_send_get_session_challenge(ipmi_monitoring_ctx_t c)
{
  uint8_t buf[IPMI_MONITORING_PACKET_BUFLEN];
  int buflen, rv = -1;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (fill_cmd_get_session_challenge(c->comm.authentication_type,
                                     strlen(c->comm.username) ? c->comm.username : NULL,
                                     strlen(c->comm.username),
                                     c->comm.obj_get_session_challenge_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_cmd_get_session_challenge: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
  
  c->comm.requester_sequence_number++;
  if (c->comm.requester_sequence_number > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
    c->comm.requester_sequence_number = 0;
  
  if ((buflen = _ipmi_packet_assemble(c,
                                      IPMI_AUTHENTICATION_TYPE_NONE,
                                      0,
                                      0,
                                      NULL,
                                      0,
                                      IPMI_BMC_IPMB_LUN_BMC,                                      
                                      IPMI_NET_FN_APP_RQ,
                                      c->comm.obj_get_session_challenge_rq,
                                      buf,
                                      IPMI_MONITORING_PACKET_BUFLEN)) < 0)
    goto cleanup;
  
  ipmi_monitoring_outofband_dump(c->comm.hostname,
                                 "Get Session Challenge Request",
                                 c->comm.obj_rmcp_hdr_rq,
                                 c->comm.obj_lan_session_hdr_rq,
                                 c->comm.obj_lan_msg_hdr_rq,
                                 c->comm.obj_get_session_challenge_rq,
                                 NULL);

  if (ipmi_lan_sendto(c->comm.ipmi_fd, 
                      buf, 
                      buflen,
                      0,
                      (struct sockaddr *)&(c->comm.addr),
                      sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_sendto: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  if (gettimeofday(&(c->comm.last_ipmi_packet_sent), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  secure_memset(buf, '\0', IPMI_MONITORING_PACKET_BUFLEN);
  return rv;
}

static int
_get_session_challenge_new_socket(ipmi_monitoring_ctx_t c)
{
  int new_fd = 0;
  int *old_fd = NULL;
  struct sockaddr_in srcaddr;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  /* 
   * Get Session Challenge Retransmissions are buggy on Intel
   * Tiger 4s, need to send from a different port.
   */
  
  if ((new_fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      IPMI_MONITORING_DEBUG(("socket: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  memset(&srcaddr, '\0', sizeof(struct sockaddr_in));
  srcaddr.sin_family = AF_INET;
  srcaddr.sin_port = htons(0);
  srcaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  
  if (bind(new_fd, (struct sockaddr *)&srcaddr, sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("bind: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  if (!(old_fd = (int *)malloc(sizeof(int))))
    {
      IPMI_MONITORING_DEBUG(("malloc: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  *old_fd = c->comm.ipmi_fd;
  list_push(c->comm.sockets_to_close, old_fd);
  c->comm.ipmi_fd = new_fd;
  new_fd = 0;

  return 0;

 cleanup:
  if (new_fd)
    close(new_fd);
  if (old_fd)
    free(old_fd);
  return -1;
}

static int
_get_session_challenge(ipmi_monitoring_ctx_t c)
{
  int rv = -1;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (_send_get_session_challenge(c) < 0)
    goto cleanup;

  while (1)
    {
      uint8_t comp_code;

      if ((ret = _receive_response(c, c->comm.obj_get_session_challenge_rs)) < 0)
        goto cleanup;

      if (!ret)
        {
          if (_get_session_challenge_new_socket(c) < 0)
            goto cleanup;
          if (_send_get_session_challenge(c) < 0)
            goto cleanup;
          c->comm.retransmission_count++;
          continue;
        }

      ipmi_monitoring_outofband_dump(c->comm.hostname,
                                     "Get Session Challenge Response",
                                     c->comm.obj_rmcp_hdr_rs,
                                     c->comm.obj_lan_session_hdr_rs,
                                     c->comm.obj_lan_msg_hdr_rs,
                                     c->comm.obj_get_session_challenge_rs,
                                     c->comm.obj_lan_msg_trlr_rs);

      if ((ret = _check_checksum(c, c->comm.obj_get_session_challenge_rs)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_rq_seq(c)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_completion_code(c, 
					c->comm.obj_get_session_challenge_rs,
					&comp_code)) < 0)
        goto cleanup;

      if (!ret)
        {
          if (comp_code == IPMI_COMP_CODE_INVALID_USERNAME
              || comp_code == IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED)
            {
#ifndef NDEBUG
              c->errnum = IPMI_MONITORING_ERR_USERNAME;
#else  /* !NDEBUG */
              c->errnum = IPMI_MONITORING_ERR_PERMISSION;
#endif /* !NDEBUG */
            }
          else
            c->errnum = IPMI_MONITORING_ERR_IPMI;
          goto cleanup;
        }

      break;

    continue_loop:
      /* Clear out data */
      Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
    }
  
  c->comm.retransmission_count = 0;
  if (gettimeofday(&(c->comm.last_ipmi_packet_received), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
  return rv;
}

static int
_send_activate_session(ipmi_monitoring_ctx_t c)
{
  uint8_t buf[IPMI_MONITORING_PACKET_BUFLEN];
  int buflen, rv = -1;
  int8_t challenge_string[IPMI_CHALLENGE_STRING_LENGTH];
  int32_t challenge_string_len;
  uint32_t session_id;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (Fiid_obj_get(c,
                   c->comm.obj_get_session_challenge_rs,
                   "temp_session_id",
                   &val) < 0)
    goto cleanup;
  session_id = val;

  if ((challenge_string_len = Fiid_obj_get_data(c,
                                                c->comm.obj_get_session_challenge_rs,
                                                "challenge_string",
                                                challenge_string,
                                                IPMI_CHALLENGE_STRING_LENGTH)) < 0)
    goto cleanup;

  if (fill_cmd_activate_session(c->comm.authentication_type,
                                c->comm.privilege_level,
                                challenge_string,
                                challenge_string_len,
                                IPMI_MONITORING_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER,
                                c->comm.obj_activate_session_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_cmd_activate_session: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
  
  c->comm.requester_sequence_number++;
  if (c->comm.requester_sequence_number > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
    c->comm.requester_sequence_number = 0;

  if ((buflen = _ipmi_packet_assemble(c,
                                      c->comm.authentication_type,
                                      0,
                                      session_id,
                                      strlen(c->comm.password) ? c->comm.password : NULL,
                                      strlen(c->comm.password),
                                      IPMI_BMC_IPMB_LUN_BMC,                                      
                                      IPMI_NET_FN_APP_RQ,
                                      c->comm.obj_activate_session_rq,
                                      buf,
                                      IPMI_MONITORING_PACKET_BUFLEN)) < 0)
    goto cleanup;
  
  ipmi_monitoring_outofband_dump(c->comm.hostname,
                                 "Activate Session Request",
                                 c->comm.obj_rmcp_hdr_rq,
                                 c->comm.obj_lan_session_hdr_rq,
                                 c->comm.obj_lan_msg_hdr_rq,
                                 c->comm.obj_activate_session_rq,
                                 NULL);

  if (ipmi_lan_sendto(c->comm.ipmi_fd, 
                      buf, 
                      buflen,
                      0,
                      (struct sockaddr *)&(c->comm.addr),
                      sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_sendto: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  if (gettimeofday(&(c->comm.last_ipmi_packet_sent), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  secure_memset(buf, '\0', IPMI_MONITORING_PACKET_BUFLEN);
  return rv;
}

static int
_activate_session(ipmi_monitoring_ctx_t c)
{
  int rv = -1;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (_send_activate_session(c) < 0)
    goto cleanup;

  while (1)
    {
      uint8_t comp_code;

      if ((ret= _receive_response(c, c->comm.obj_activate_session_rs)) < 0)
        goto cleanup;

      if (!rv)
        {
          if (_send_activate_session(c) < 0)
            goto cleanup;
          c->comm.retransmission_count++;
          continue;
        }

      ipmi_monitoring_outofband_dump(c->comm.hostname,
                                     "Activate Session Response",
                                     c->comm.obj_rmcp_hdr_rs,
                                     c->comm.obj_lan_session_hdr_rs,
                                     c->comm.obj_lan_msg_hdr_rs,
                                     c->comm.obj_activate_session_rs,
                                     c->comm.obj_lan_msg_trlr_rs);

      if ((ret= _check_checksum(c, c->comm.obj_activate_session_rs)) < 0)
        goto cleanup;

      if (!rv)
        goto continue_loop;

      if ((ret= _check_authentication_code(c, c->comm.obj_activate_session_rs)) < 0)
        goto cleanup;

      if (!rv)
        goto continue_loop;

      if ((ret= _check_rq_seq(c)) < 0)
        goto cleanup;

      if (!rv)
        goto continue_loop;

      if ((ret= _check_completion_code(c, 
                                       c->comm.obj_activate_session_rs,
                                       &comp_code)) < 0)
        goto cleanup;
      
      if (!rv)
        {
          if (comp_code == IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL)
            {
#ifndef NDEBUG
              c->errnum = IPMI_MONITORING_ERR_PRIVILEGE_LEVEL;
#else  /* !NDEBUG */
              c->errnum = IPMI_MONITORING_ERR_PERMISSION;
#endif /* !NDEBUG */
            }
          else if (comp_code == IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE
                   || comp_code == IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER
                   || comp_code == IPMI_COMP_CODE_NO_SLOT_AVAILABLE_TO_SUPPORT_USER)
            c->errnum = IPMI_MONITORING_ERR_BMC_BUSY;
          else
            c->errnum = IPMI_MONITORING_ERR_IPMI;
          goto cleanup;
        }

      break;

    continue_loop:
      /* Clear out data */
      Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
    }
  
  c->comm.retransmission_count = 0;
  if (gettimeofday(&(c->comm.last_ipmi_packet_received), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
  return rv;
}

static int
_send_set_session_privilege_level(ipmi_monitoring_ctx_t c)
{
  uint8_t buf[IPMI_MONITORING_PACKET_BUFLEN];
  int buflen, rv = -1;
  uint32_t session_id;
  uint32_t initial_inbound_sequence_number;
  uint32_t inbound_sequence_number;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (Fiid_obj_get(c,
                   c->comm.obj_activate_session_rs,
                   "session_id",
                   &val) < 0)
    goto cleanup;
  session_id = val;

  if (Fiid_obj_get(c,
                   c->comm.obj_activate_session_rs,
                   "initial_inbound_sequence_number",
                   &val) < 0)
    goto cleanup;
  initial_inbound_sequence_number = val;
  
  inbound_sequence_number = initial_inbound_sequence_number + c->comm.session_inbound_count;

  if (fill_cmd_set_session_privilege_level(c->comm.privilege_level,
                                           c->comm.obj_set_session_privilege_level_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_cmd_set_session_privilege_level: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
  
  c->comm.requester_sequence_number++;
  if (c->comm.requester_sequence_number > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
    c->comm.requester_sequence_number = 0;

  if ((buflen = _ipmi_packet_assemble(c,
                                      c->comm.authentication_type,
                                      inbound_sequence_number,
                                      session_id,
                                      strlen(c->comm.password) ? c->comm.password : NULL,
                                      strlen(c->comm.password),
                                      IPMI_BMC_IPMB_LUN_BMC,                                      
                                      IPMI_NET_FN_APP_RQ,
                                      c->comm.obj_set_session_privilege_level_rq,
                                      buf,
                                      IPMI_MONITORING_PACKET_BUFLEN)) < 0)
    goto cleanup;

  ipmi_monitoring_outofband_dump(c->comm.hostname,
                                 "Set Session Privilege Level Request",
                                 c->comm.obj_rmcp_hdr_rq,
                                 c->comm.obj_lan_session_hdr_rq,
                                 c->comm.obj_lan_msg_hdr_rq,
                                 c->comm.obj_set_session_privilege_level_rq,
                                 NULL);

  if (ipmi_lan_sendto(c->comm.ipmi_fd, 
                      buf, 
                      buflen,
                      0,
                      (struct sockaddr *)&(c->comm.addr),
                      sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_sendto: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  c->comm.session_inbound_count++;

  if (gettimeofday(&(c->comm.last_ipmi_packet_sent), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  secure_memset(buf, '\0', IPMI_MONITORING_PACKET_BUFLEN);
  return rv;
}

static int
_set_session_privilege_level(ipmi_monitoring_ctx_t c)
{
  int rv = -1;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (_send_set_session_privilege_level(c) < 0)
    goto cleanup;

  while (1)
    {
      uint8_t comp_code;

      if ((ret = _receive_response(c, c->comm.obj_set_session_privilege_level_rs)) < 0)
        goto cleanup;

      if (!ret)
        {
          if (_send_set_session_privilege_level(c) < 0)
            goto cleanup;
          c->comm.retransmission_count++;
          continue;
        }

      ipmi_monitoring_outofband_dump(c->comm.hostname,
                                     "Set Session Privilege Level Response",
                                     c->comm.obj_rmcp_hdr_rs,
                                     c->comm.obj_lan_session_hdr_rs,
                                     c->comm.obj_lan_msg_hdr_rs,
                                     c->comm.obj_set_session_privilege_level_rs,
                                     c->comm.obj_lan_msg_trlr_rs);

      if ((ret = _check_checksum(c, c->comm.obj_set_session_privilege_level_rs)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_authentication_code(c, c->comm.obj_set_session_privilege_level_rs)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_sequence_number(c)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_session_id(c)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_rq_seq(c)) < 0)
        goto cleanup;

      if (!ret)
        continue;

      if ((ret = _check_completion_code(c, 
					c->comm.obj_set_session_privilege_level_rs,
					&comp_code)) < 0)
        goto cleanup;

      if (!ret)
        {
          if (comp_code == IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER
              || comp_code == IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT
              || comp_code == IPMI_COMP_CODE_CANNOT_DISABLE_USER_LEVEL_AUTHENTICATION)
            {
#ifndef NDEBUG
              c->errnum = IPMI_MONITORING_ERR_PRIVILEGE_LEVEL;
#else  /* !NDEBUG */
              c->errnum = IPMI_MONITORING_ERR_PERMISSION;
#endif /* !NDEBUG */
            }
          else
            c->errnum = IPMI_MONITORING_ERR_IPMI;
          goto cleanup;
        }

      break;

    continue_loop:
      /* Clear out data */
      Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
    }
  
  c->comm.retransmission_count = 0;
  if (gettimeofday(&(c->comm.last_ipmi_packet_received), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
  return rv;
}

static int
_send_close_session(ipmi_monitoring_ctx_t c)
{
  uint8_t buf[IPMI_MONITORING_PACKET_BUFLEN];
  int buflen, rv = -1;
  uint32_t session_id;
  uint32_t initial_inbound_sequence_number;
  uint32_t inbound_sequence_number;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (Fiid_obj_get(c,
                   c->comm.obj_activate_session_rs,
                   "session_id",
                   &val) < 0)
    goto cleanup;
  session_id = val;

  if (Fiid_obj_get(c,
                   c->comm.obj_activate_session_rs,
                   "initial_inbound_sequence_number",
                   &val) < 0)
    goto cleanup;
  initial_inbound_sequence_number = val;
  
  inbound_sequence_number = initial_inbound_sequence_number + c->comm.session_inbound_count;

  if (fill_cmd_close_session(session_id,
                             c->comm.obj_close_session_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_cmd_close_session: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }
  
  c->comm.requester_sequence_number++;
  if (c->comm.requester_sequence_number > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
    c->comm.requester_sequence_number = 0;

  if ((buflen = _ipmi_packet_assemble(c,
                                      c->comm.authentication_type,
                                      inbound_sequence_number,
                                      session_id,
                                      strlen(c->comm.password) ? c->comm.password : NULL,
                                      strlen(c->comm.password),
                                      IPMI_BMC_IPMB_LUN_BMC,                                      
                                      IPMI_NET_FN_APP_RQ,
                                      c->comm.obj_close_session_rq,
                                      buf,
                                      IPMI_MONITORING_PACKET_BUFLEN)) < 0)
    goto cleanup;

  ipmi_monitoring_outofband_dump(c->comm.hostname,
                                 "Close Session Request",
                                 c->comm.obj_rmcp_hdr_rq,
                                 c->comm.obj_lan_session_hdr_rq,
                                 c->comm.obj_lan_msg_hdr_rq,
                                 c->comm.obj_close_session_rq,
                                 NULL);
  
  if (ipmi_lan_sendto(c->comm.ipmi_fd, 
                      buf, 
                      buflen,
                      0,
                      (struct sockaddr *)&(c->comm.addr),
                      sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_sendto: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  c->comm.session_inbound_count++;
  
  if (gettimeofday(&(c->comm.last_ipmi_packet_sent), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  secure_memset(buf, '\0', IPMI_MONITORING_PACKET_BUFLEN);
  return rv;
}

static int
_close_session(ipmi_monitoring_ctx_t c)
{
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (_send_close_session(c) < 0)
    goto cleanup;

  if ((ret = _receive_response(c, c->comm.obj_close_session_rs)) < 0)
    goto cleanup;

  if (ret)
    ipmi_monitoring_outofband_dump(c->comm.hostname,
                                   "Close Session Response",
                                   c->comm.obj_rmcp_hdr_rs,
                                   c->comm.obj_lan_session_hdr_rs,
                                   c->comm.obj_lan_msg_hdr_rs,
                                   c->comm.obj_close_session_rs,
                                   c->comm.obj_lan_msg_trlr_rs);

 cleanup:
  /* Clear out data */
  Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
  /* No matter what, we just assume the session closed properly */
  return 0;
}

static void
_outofband_disconnect(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  _close_session(c);
}

static void
_outofband_cleanup(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  /* If there is a session to teardown, tear it down */
  if (c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_OUTOFBAND)
    _outofband_disconnect(c);

  if (c->comm.ipmi_fd)
    {
      close(c->comm.ipmi_fd);
      c->comm.ipmi_fd = 0;
    }
  if (c->comm.sockets_to_close)
    {
      list_destroy(c->comm.sockets_to_close);
      c->comm.sockets_to_close = NULL;
    }
  if (c->comm.obj_rmcp_hdr_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_rmcp_hdr_rq);
      c->comm.obj_rmcp_hdr_rq = NULL;
    }
  if (c->comm.obj_rmcp_hdr_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_rmcp_hdr_rs);
      c->comm.obj_rmcp_hdr_rs = NULL;
    }
  if (c->comm.obj_lan_session_hdr_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_lan_session_hdr_rq);
      c->comm.obj_lan_session_hdr_rq = NULL;
    }
  if (c->comm.obj_lan_session_hdr_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_lan_session_hdr_rs);
      c->comm.obj_lan_session_hdr_rs = NULL;
    }
  if (c->comm.obj_lan_msg_hdr_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_lan_msg_hdr_rq);
      c->comm.obj_lan_msg_hdr_rq = NULL;
    }
  if (c->comm.obj_lan_msg_hdr_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_lan_msg_hdr_rs);
      c->comm.obj_lan_msg_hdr_rs = NULL;
    }
  if (c->comm.obj_lan_msg_trlr_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_lan_msg_trlr_rs);
      c->comm.obj_lan_msg_trlr_rs = NULL;
    }
  if (c->comm.obj_get_channel_authentication_capabilities_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_get_channel_authentication_capabilities_rq);
      c->comm.obj_get_channel_authentication_capabilities_rq = NULL;
    }
  if (c->comm.obj_get_channel_authentication_capabilities_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_get_channel_authentication_capabilities_rs);
      c->comm.obj_get_channel_authentication_capabilities_rs = NULL;
    }
  if (c->comm.obj_get_session_challenge_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_get_session_challenge_rq);
      c->comm.obj_get_session_challenge_rq = NULL;
    }
  if (c->comm.obj_get_session_challenge_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_get_session_challenge_rs);
      c->comm.obj_get_session_challenge_rs = NULL;
    }
  if (c->comm.obj_activate_session_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_activate_session_rq);
      c->comm.obj_activate_session_rq = NULL;
    }
  if (c->comm.obj_activate_session_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_activate_session_rs);
      c->comm.obj_activate_session_rs = NULL;
    }
  if (c->comm.obj_set_session_privilege_level_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_set_session_privilege_level_rq);
      c->comm.obj_set_session_privilege_level_rq = NULL;
    }
  if (c->comm.obj_set_session_privilege_level_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_set_session_privilege_level_rs);
      c->comm.obj_set_session_privilege_level_rs = NULL;
    }
  if (c->comm.obj_close_session_rq)
    {
      Fiid_obj_destroy(c, c->comm.obj_close_session_rq);
      c->comm.obj_close_session_rq = NULL;
    }
  if (c->comm.obj_close_session_rs)
    {
      Fiid_obj_destroy(c, c->comm.obj_close_session_rs);
      c->comm.obj_close_session_rs = NULL;
    }
}

static int
_outofband_connect(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (_get_channel_authentication_capabilities(c) < 0)
    return -1;

  if (_get_session_challenge(c) < 0)
    return -1;

  if (_activate_session(c) < 0)
    return -1;

  if (_set_session_privilege_level(c) < 0)
    {
      _outofband_disconnect(c);
      return -1;
    }

  return 0;
}

static void
_socket_close(void *x)
{
  int fd;

  assert(x);
  
  fd = *((int *)x);
  close(fd);
  free(x);
}

static int
_outofband_init(ipmi_monitoring_ctx_t c, 
                char *hostname,
                struct ipmi_monitoring_ipmi_config *config)
{
#ifdef HAVE_FUNC_GETHOSTBYNAME_R_6
  struct hostent hent;
  int h_errnop;
  char buf[GETHOSTBYNAME_AUX_BUFLEN];
#endif /* HAVE_FUNC_GETHOSTBYNAME_R_6 */
  struct sockaddr_in srcaddr;
  struct hostent *hptr;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(hostname);
  assert(!c->comm.communication_type);

  if (strlen(hostname) > MAXHOSTNAMELEN
      || (config
          && ((config->username && strlen(config->username) > IPMI_MAX_USER_NAME_LENGTH)
              || (config->password && strlen(config->password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
              || (config->privilege_level >= 0
                  && (config->privilege_level != IPMI_MONITORING_PRIVILEGE_USER
                      && config->privilege_level != IPMI_MONITORING_PRIVILEGE_OPERATOR
                      && config->privilege_level != IPMI_MONITORING_PRIVILEGE_ADMIN))
              || (config->authentication_type >= 0
                  && (config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_NONE
                      && config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
                      && config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_MD2
                      && config->authentication_type != IPMI_MONITORING_AUTHENTICATION_TYPE_MD5))
              || (config->workaround_flags & ~IPMI_MONITORING_WORKAROUND_FLAGS_MASK))))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return -1;
    }

  memset(&(c->comm), '\0', sizeof(struct ipmi_monitoring_communication));

  strcpy(c->comm.hostname, hostname);

  if (config && config->username)
    strcpy((char *)c->comm.username, config->username);

  if (config && config->password)
    strcpy((char *)c->comm.password, config->password);

  if (config && config->privilege_level >= 0)
    {
      if (config->privilege_level == IPMI_MONITORING_PRIVILEGE_USER)
        c->comm.privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
      else if (config->privilege_level == IPMI_MONITORING_PRIVILEGE_OPERATOR)
        c->comm.privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
      else
        c->comm.privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
    }
  else
    c->comm.privilege_level = IPMI_MONITORING_PRIVILEGE_LEVEL_DEFAULT;

  if (config && config->authentication_type >= 0)
    {
      if (config->authentication_type == IPMI_MONITORING_AUTHENTICATION_TYPE_NONE)
        c->comm.authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
      else if (config->authentication_type == IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
        c->comm.authentication_type = IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
      else if (config->authentication_type == IPMI_MONITORING_AUTHENTICATION_TYPE_MD2)
        c->comm.authentication_type = IPMI_AUTHENTICATION_TYPE_MD2;
      else
        c->comm.authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
    }
  else
    c->comm.authentication_type = IPMI_MONITORING_AUTHENTICATION_TYPE_DEFAULT;

  if (config && config->session_timeout_len > 0)
    c->comm.session_timeout_len = config->session_timeout_len;
  else
    c->comm.session_timeout_len = IPMI_MONITORING_SESSION_TIMEOUT_LEN_DEFAULT;

  if (config && config->retransmission_timeout_len > 0)
    c->comm.retransmission_timeout_len = config->retransmission_timeout_len;
  else
    c->comm.retransmission_timeout_len = IPMI_MONITORING_RETRANSMISSION_TIMEOUT_LEN_DEFAULT;

  if (config && config->retransmission_backoff_count > 0)
    c->comm.retransmission_backoff_count = config->retransmission_backoff_count;
  else
    c->comm.retransmission_backoff_count = IPMI_MONITORING_RETRANSMISSION_BACKOFF_COUNT_DEFAULT;

  if (config)
    c->comm.workaround_flags = config->workaround_flags;

  if ((c->comm.ipmi_fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      IPMI_MONITORING_DEBUG(("socket: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  memset(&srcaddr, '\0', sizeof(struct sockaddr_in));
  srcaddr.sin_family = AF_INET;
  srcaddr.sin_port = htons(0);
  srcaddr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(c->comm.ipmi_fd, (struct sockaddr *)&srcaddr, sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("bind: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  if (!(c->comm.sockets_to_close = list_create((ListDelF)_socket_close)))
    {
      IPMI_MONITORING_DEBUG(("list_create: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      goto cleanup;
    }

  memset(&(c->comm.addr), '\0', sizeof(struct sockaddr_in));
  c->comm.addr.sin_family = AF_INET;
  c->comm.addr.sin_port = htons(RMCP_AUX_BUS_SHUNT);

#ifdef HAVE_FUNC_GETHOSTBYNAME_R_6
  memset(&hent, '\0', sizeof(struct hostent));
  if (gethostbyname_r(c->comm.hostname,
                      &hent,
                      buf,
                      GETHOSTBYNAME_AUX_BUFLEN,
                      &hptr,
                      &h_errnop) != 0)
    {
      if (h_errnop == HOST_NOT_FOUND 
          || h_errnop == NO_ADDRESS 
          || h_errnop == NO_DATA)
        {
          c->errnum = IPMI_MONITORING_ERR_HOSTNAME_INVALID;
          goto cleanup;
        }
      IPMI_MONITORING_DEBUG(("gethostbyname_r: %s", hstrerror(h_errnop)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  if (!hptr)
    {
      c->errnum = IPMI_MONITORING_ERR_HOSTNAME_INVALID;
      goto cleanup;
    }
#else /* !HAVE_FUNC_GETHOSTBYNAME_R */
#error Additional threadsafe gethostbyname support needed
#endif /* !HAVE_FUNC_GETHOSTBYNAME_R */

  c->comm.addr.sin_addr = *((struct in_addr *)hptr->h_addr);

  timeval_clear(&(c->comm.last_ipmi_packet_sent));
  /* Note:
   * Initial last_ipmi_packet_received to current time, so session
   * timeout can be calculated in the beginning if necessary.
   */
  if (gettimeofday(&(c->comm.last_ipmi_packet_received), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  c->comm.retransmission_count = 0;
  c->comm.highest_received_sequence_number = IPMI_MONITORING_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER;
  c->comm.previously_received_list = IPMI_MONITORING_PREVIOUSLY_RECEIVED_LIST_INIT;

  if (ipmi_get_random(&(c->comm.requester_sequence_number),
                      sizeof(c->comm.requester_sequence_number)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_get_random: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }
  c->comm.requester_sequence_number %= (IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX + 1);
  c->comm.session_inbound_count = 0;

  if (!(c->comm.obj_rmcp_hdr_rq = Fiid_obj_create(c, tmpl_rmcp_hdr)))
    goto cleanup;
  if (!(c->comm.obj_rmcp_hdr_rs = Fiid_obj_create(c, tmpl_rmcp_hdr)))
    goto cleanup;
  if (!(c->comm.obj_lan_session_hdr_rq = Fiid_obj_create(c, tmpl_lan_session_hdr)))
    goto cleanup;
  if (!(c->comm.obj_lan_session_hdr_rs = Fiid_obj_create(c, tmpl_lan_session_hdr)))
    goto cleanup;
  if (!(c->comm.obj_lan_msg_hdr_rq = Fiid_obj_create(c, tmpl_lan_msg_hdr_rq)))
    goto cleanup;
  if (!(c->comm.obj_lan_msg_hdr_rs = Fiid_obj_create(c, tmpl_lan_msg_hdr_rs)))
    goto cleanup;
  if (!(c->comm.obj_lan_msg_trlr_rs = Fiid_obj_create(c, tmpl_lan_msg_trlr)))
    goto cleanup;
  if (!(c->comm.obj_get_channel_authentication_capabilities_rq = Fiid_obj_create(c, tmpl_cmd_get_channel_authentication_capabilities_rq)))
    goto cleanup;
  if (!(c->comm.obj_get_channel_authentication_capabilities_rs = Fiid_obj_create(c, tmpl_cmd_get_channel_authentication_capabilities_rs)))
    goto cleanup;
  if (!(c->comm.obj_get_session_challenge_rq = Fiid_obj_create(c, tmpl_cmd_get_session_challenge_rq)))
    goto cleanup;
  if (!(c->comm.obj_get_session_challenge_rs = Fiid_obj_create(c, tmpl_cmd_get_session_challenge_rs)))
    goto cleanup;
  if (!(c->comm.obj_activate_session_rq = Fiid_obj_create(c, tmpl_cmd_activate_session_rq)))
    goto cleanup;
  if (!(c->comm.obj_activate_session_rs = Fiid_obj_create(c, tmpl_cmd_activate_session_rs)))
    goto cleanup;
  if (!(c->comm.obj_set_session_privilege_level_rq = Fiid_obj_create(c, tmpl_cmd_set_session_privilege_level_rq)))
    goto cleanup;
  if (!(c->comm.obj_set_session_privilege_level_rs = Fiid_obj_create(c, tmpl_cmd_set_session_privilege_level_rs)))
    goto cleanup;
  if (!(c->comm.obj_close_session_rq = Fiid_obj_create(c, tmpl_cmd_close_session_rq)))
    goto cleanup;
  if (!(c->comm.obj_close_session_rs = Fiid_obj_create(c, tmpl_cmd_close_session_rs)))
    goto cleanup;

  if (_outofband_connect(c) < 0)
    goto cleanup;

  return 0;

 cleanup:
  _outofband_cleanup(c);
  return -1;
}

int 
ipmi_monitoring_ipmi_communication_init(ipmi_monitoring_ctx_t c,
                                        char *hostname,
                                        struct ipmi_monitoring_ipmi_config *config)
{

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(!c->comm.communication_type);

  if (!hostname)
    {
      if (_inband_init(c) < 0)
        return -1;
      c->comm.communication_type = IPMI_MONITORING_COMMUNICATION_INBAND;
    }
  else
    {
      if (_outofband_init(c, hostname, config) < 0)
        return -1;
      c->comm.communication_type = IPMI_MONITORING_COMMUNICATION_OUTOFBAND;
    }

  return 0;
}

static int
_inband_cmd(ipmi_monitoring_ctx_t c,
            uint8_t lun,
            uint8_t net_fn,
            fiid_obj_t obj_cmd_rq,
            fiid_obj_t obj_cmd_rs)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_INBAND);
  assert(c->comm.kcs_ctx);
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_valid(obj_cmd_rs));

  ipmi_monitoring_inband_dump("Inband_Request", obj_cmd_rq);

  if (ipmi_kcs_cmd (c->comm.kcs_ctx,
                    lun,
                    net_fn,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_kcs_cmd: %s", ipmi_kcs_ctx_strerror(ipmi_kcs_ctx_errnum(c->comm.kcs_ctx))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL;
      return -1;
    }
  
  ipmi_monitoring_inband_dump("Inband_Response", obj_cmd_rs);

  return 0;
}

static int
_send_outofband_cmd(ipmi_monitoring_ctx_t c, 
                    uint8_t lun,
                    uint8_t net_fn,
                    fiid_obj_t obj_cmd)
{
  uint8_t buf[IPMI_MONITORING_PACKET_BUFLEN];
  int buflen, rv = -1;
  uint32_t session_id;
  uint32_t initial_inbound_sequence_number;
  uint32_t inbound_sequence_number;
  uint64_t val;
  
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj_cmd));

  if (Fiid_obj_get(c,
                   c->comm.obj_activate_session_rs,
                   "session_id",
                   &val) < 0)
    goto cleanup;
  session_id = val;

  if (Fiid_obj_get(c,
                   c->comm.obj_activate_session_rs,
                   "initial_inbound_sequence_number",
                   &val) < 0)
    goto cleanup;
  initial_inbound_sequence_number = val;
  
  inbound_sequence_number = initial_inbound_sequence_number + c->comm.session_inbound_count;

  c->comm.requester_sequence_number++;
  if (c->comm.requester_sequence_number > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
    c->comm.requester_sequence_number = 0;

  if ((buflen = _ipmi_packet_assemble(c,
                                      c->comm.authentication_type,
                                      inbound_sequence_number,
                                      session_id,
                                      strlen(c->comm.password) ? c->comm.password : NULL,
                                      strlen(c->comm.password),
                                      lun,
                                      net_fn,
                                      obj_cmd,
                                      buf,
                                      IPMI_MONITORING_PACKET_BUFLEN)) < 0)
    goto cleanup;

  ipmi_monitoring_outofband_dump(c->comm.hostname,
                                 "OutofBand Request",
                                 c->comm.obj_rmcp_hdr_rq,
                                 c->comm.obj_lan_session_hdr_rq,
                                 c->comm.obj_lan_msg_hdr_rq,
                                 obj_cmd,
                                 NULL);

  if (ipmi_lan_sendto(c->comm.ipmi_fd, 
                      buf, 
                      buflen,
                      0,
                      (struct sockaddr *)&(c->comm.addr),
                      sizeof(struct sockaddr_in)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_lan_sendto: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  
  c->comm.session_inbound_count++;

  if (gettimeofday(&(c->comm.last_ipmi_packet_sent), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  secure_memset(buf, '\0', IPMI_MONITORING_PACKET_BUFLEN);
  return rv;
}

static int
_outofband_cmd(ipmi_monitoring_ctx_t c,
               uint8_t lun,
               uint8_t net_fn,
               fiid_obj_t obj_cmd_rq,
               fiid_obj_t obj_cmd_rs)
{
  int rv = -1;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_OUTOFBAND);
  assert(c->comm.ipmi_fd);
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_valid(obj_cmd_rs));

  if (_send_outofband_cmd(c, lun, net_fn, obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      if ((ret = _receive_response(c, obj_cmd_rs)) < 0)
        goto cleanup;
      
      if (!ret)
        {
          if (_send_outofband_cmd(c, lun, net_fn, obj_cmd_rq) < 0)
            goto cleanup;
          c->comm.retransmission_count++;
          continue;
        }

      ipmi_monitoring_outofband_dump(c->comm.hostname,
                                     "OutofBand Response",
                                     c->comm.obj_rmcp_hdr_rs,
                                     c->comm.obj_lan_session_hdr_rs,
                                     c->comm.obj_lan_msg_hdr_rs,
                                     obj_cmd_rs,
                                     c->comm.obj_lan_msg_trlr_rs);

      if ((ret = _check_checksum(c, obj_cmd_rs)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_authentication_code(c, obj_cmd_rs)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_sequence_number(c)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_session_id(c)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_rq_seq(c)) < 0)
        goto cleanup;

      if (!ret)
        goto continue_loop;

      if ((ret = _check_completion_code(c, 
					obj_cmd_rs,
					NULL)) < 0)
        goto cleanup;

      if (!ret)
        {
          c->errnum = IPMI_MONITORING_ERR_IPMI;
          goto cleanup;
        }

      break;

    continue_loop:
      /* Clear out data */
      Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
    }
  
  c->comm.retransmission_count = 0;
  if (gettimeofday(&(c->comm.last_ipmi_packet_received), NULL) < 0)
    {
      IPMI_MONITORING_DEBUG(("gettimeofday: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  /* Clear out data */
  Fiid_obj_clear(c, c->comm.obj_lan_session_hdr_rs);
  return rv;
}

int 
ipmi_monitoring_ipmi_sendrecv(ipmi_monitoring_ctx_t c,
                              uint8_t lun,
                              uint8_t net_fn,
                              fiid_obj_t obj_cmd_rq,
                              fiid_obj_t obj_cmd_rs)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_INBAND
         || c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_OUTOFBAND);
  assert((c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_INBAND
          && c->comm.kcs_ctx)
         || (c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_OUTOFBAND
             && c->comm.ipmi_fd));
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_valid(obj_cmd_rs));

  if (c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_INBAND)
    {
      if (_inband_cmd(c, lun, net_fn, obj_cmd_rq, obj_cmd_rs) < 0)
        return -1;
    }
  else
    {
      if (_outofband_cmd(c, lun, net_fn, obj_cmd_rq, obj_cmd_rs) < 0)
        return -1;
    }

  return 0;
}

int 
ipmi_monitoring_ipmi_communication_cleanup(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_INBAND)
    _inband_cleanup(c);
  else if (c->comm.communication_type == IPMI_MONITORING_COMMUNICATION_OUTOFBAND)
    _outofband_cleanup(c);
  /* else not-initialized, don't worry about it */

  c->comm.communication_type = IPMI_MONITORING_COMMUNICATION_UNINITIALIZED;
  return 0;
}
