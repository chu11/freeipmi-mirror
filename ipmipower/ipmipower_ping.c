/*****************************************************************************\
 *  $Id: ipmipower_ping.c,v 1.54 2010-02-08 22:02:31 chu11 Exp $
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
#include <assert.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
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
#include <errno.h>

#include "ipmipower_ping.h"
#include "ipmipower_error.h"
#include "ipmipower_util.h"

#include "freeipmi-portability.h"
#include "cbuf.h"
#include "debug-util.h"
#include "timeval.h"

extern struct ipmipower_arguments cmd_args;
extern struct ipmipower_connection *ics;
extern unsigned int ics_len;

/* next_ping_sends_time, when the next round of pings should be sent */
static struct timeval next_ping_sends_time;

/* force discovery sweep when user reconfigures hostnames */
static int force_discovery_sweep;

/* IPMI has a 6 bit sequence number */
#define IPMI_RQ_SEQ_MAX  0x3F

void
ipmipower_ping_force_discovery_sweep ()
{
  force_discovery_sweep = 1;
}

void
ipmipower_ping_process_pings (int *timeout)
{
  int i, send_pings_flag = 0;
  struct timeval cur_time, result;
  unsigned int ms_time;

  assert (timeout);

  if (!cmd_args.common_args.hostname)
    return;

  if (!cmd_args.ping_interval)
    return;

  if (gettimeofday (&cur_time, NULL) < 0)
    {
      IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (timeval_gt (&cur_time, &next_ping_sends_time) || force_discovery_sweep)
    {
      force_discovery_sweep = 0;
      timeval_add_ms (&cur_time, cmd_args.ping_interval, &next_ping_sends_time);
      send_pings_flag++;
    }

  for (i = 0; i < ics_len; i++)
    {
      uint8_t buf[IPMIPOWER_PACKET_BUFLEN];
      int ret, len;

      if (send_pings_flag)
        {
          int dropped = 0;
          
          memset (buf, '\0', IPMIPOWER_PACKET_BUFLEN);

          /* deal with packet heuristics */
          if (cmd_args.ping_packet_count && cmd_args.ping_percent)
            {
              if (ics[i].ping_packet_count_send == cmd_args.ping_packet_count)
                {
                  if ((((double)(ics[i].ping_packet_count_send - ics[i].ping_packet_count_recv))/ics[i].ping_packet_count_send) > ((double)cmd_args.ping_percent/100))
                    ics[i].link_state = IPMIPOWER_LINK_STATE_BAD;
                  else
                    ics[i].link_state = IPMIPOWER_LINK_STATE_GOOD;

                  ics[i].ping_packet_count_send = 0;
                  ics[i].ping_packet_count_recv = 0;
                }
            }

          if (cmd_args.ping_consec_count)
            {
              if (!ics[i].ping_last_packet_recv_flag)
                ics[i].ping_consec_count = 0;
              ics[i].ping_last_packet_recv_flag = 0;
            }

          /* must increment count before setting message tag, so we
           * can check sequence number correctly later on
           */
          ics[i].ping_sequence_number_counter++;

	  /* Workaround
	   *
	   * Some motherboards don't support RMCP ping/pong :-(
	   *
	   * Discovered on Intel Windmill, Quanta Winterfell, and Wiwynn Windmill
	   */
	  if (cmd_args.common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING)
	    {
	      fiid_obj_t obj_rmcp_hdr = NULL;
	      fiid_obj_t obj_lan_session_hdr = NULL;
	      fiid_obj_t obj_lan_msg_hdr = NULL;
	      fiid_obj_t obj_cmd = NULL;

	      if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rq)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(obj_cmd = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rq)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (fill_rmcp_hdr_ipmi (obj_rmcp_hdr) < 0)
		{
		  IPMIPOWER_ERROR (("fill_rmcp_hdr_ipmi: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (fill_lan_session_hdr (IPMI_AUTHENTICATION_TYPE_NONE,
					0,
					0,
					obj_lan_session_hdr) < 0)
		{
		  IPMIPOWER_ERROR (("fill_lan_session_hdr: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
				    IPMI_NET_FN_APP_RQ,
				    IPMI_BMC_IPMB_LUN_BMC,
				    (ics[i].ping_sequence_number_counter % (IPMI_RQ_SEQ_MAX + 1)),
				    obj_lan_msg_hdr) < 0)
		{
		  IPMIPOWER_ERROR (("fill_lan_msg_hdr: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
								    IPMI_PRIVILEGE_LEVEL_USER,
								    IPMI_GET_IPMI_V15_DATA,
								    obj_cmd) < 0)
		{
		  IPMIPOWER_ERROR (("fill_cmd_get_channel_authentication_capabilities: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if ((len = assemble_ipmi_lan_pkt (obj_rmcp_hdr,
						obj_lan_session_hdr,
						obj_lan_msg_hdr,
						obj_cmd,
						NULL,
						0,
						buf,
						IPMIPOWER_PACKET_BUFLEN,
						IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
		{
		  IPMIPOWER_ERROR (("assemble_ipmi_lan_pkt: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

#ifndef NDEBUG
	      if (cmd_args.rmcpdump)
		{
		  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
		  const char *str_cmd = NULL;

		  str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES);

		  debug_hdr_str (DEBUG_UTIL_TYPE_IPMI_1_5,
				 DEBUG_UTIL_DIRECTION_REQUEST,
				 DEBUG_UTIL_FLAGS_DEFAULT,
				 str_cmd,
				 hdrbuf,
				 DEBUG_UTIL_HDR_BUFLEN);
		  
		  if (ipmi_dump_lan_packet (STDERR_FILENO,
					    ics[i].hostname,
					    hdrbuf,
					    NULL,
					    buf,
					    len,
					    tmpl_lan_msg_hdr_rq,
					    tmpl_cmd_get_channel_authentication_capabilities_rq) < 0)
		    IPMIPOWER_DEBUG (("ipmi_dump_lan_packet: %s", strerror (errno)));
		}
#endif /* NDEBUG */

	      fiid_obj_destroy (obj_rmcp_hdr);
	      fiid_obj_destroy (obj_lan_session_hdr);
	      fiid_obj_destroy (obj_lan_msg_hdr);
	      fiid_obj_destroy (obj_cmd);
	    }
	  else			/* !IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING */
	    {
	      fiid_obj_t rmcp_hdr = NULL;
	      fiid_obj_t rmcp_ping = NULL;

	      if (!(rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(rmcp_ping = fiid_obj_create (tmpl_cmd_asf_presence_ping)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (fill_rmcp_hdr_asf (rmcp_hdr) < 0)
		{
		  IPMIPOWER_ERROR (("fill_rmcp_hdr_asf: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (fill_cmd_asf_presence_ping ((ics[i].ping_sequence_number_counter %
					       (RMCP_ASF_MESSAGE_TAG_MAX + 1)),
					      rmcp_ping) < 0)
		{
		  IPMIPOWER_ERROR (("fill_cmd_asf_presence_ping: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if ((len = assemble_rmcp_pkt (rmcp_hdr,
					    rmcp_ping,
					    buf,
					    IPMIPOWER_PACKET_BUFLEN,
					    IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
		{
		  IPMIPOWER_ERROR (("assemble_rmcp_pkt: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

#ifndef NDEBUG
	      if (cmd_args.rmcpdump)
		{
		  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
		  
		  debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
				 DEBUG_UTIL_DIRECTION_NONE,
				 DEBUG_UTIL_FLAGS_DEFAULT,
				 DEBUG_UTIL_RMCPPING_STR,
				 hdrbuf,
				 DEBUG_UTIL_HDR_BUFLEN);
		  
		  if (ipmi_dump_rmcp_packet (STDERR_FILENO,
					     ics[i].hostname,
					     hdrbuf,
					     NULL,
					     buf,
					     len,
					     tmpl_cmd_asf_presence_ping) < 0)
		    IPMIPOWER_DEBUG (("ipmi_dump_rmcp_packet: %s", strerror (errno)));
		}
#endif /* NDEBUG */

	      fiid_obj_destroy (rmcp_hdr);
	      fiid_obj_destroy (rmcp_ping);
	    } /* !IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING */

          if ((ret = cbuf_write (ics[i].ping_out, buf, len, &dropped)) < 0)
            {
              IPMIPOWER_ERROR (("cbuf_write: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }

          if (ret != len)
            {
              IPMIPOWER_ERROR (("cbuf_write: incorrect bytes written %d", ret));
              exit (EXIT_FAILURE);
            }

          if (dropped)
            IPMIPOWER_DEBUG (("cbuf_write: dropped %d bytes", dropped));

          ics[i].last_ping_send.tv_sec = cur_time.tv_sec;
          ics[i].last_ping_send.tv_usec = cur_time.tv_usec;

          if (cmd_args.ping_packet_count && cmd_args.ping_percent)
            ics[i].ping_packet_count_send++;
        }

      /* Did we receive something? */
      memset (buf, '\0', IPMIPOWER_PACKET_BUFLEN);
      len = ipmipower_cbuf_peek_and_drop (ics[i].ping_in, buf, IPMIPOWER_PACKET_BUFLEN);
      if (len > 0)
        {
          uint8_t message_type = 0, ipmi_supported = 0;
          uint64_t val;

	  /* Workaround
	   *
	   * Some motherboards don't support RMCP ping/pong :-(
	   *
	   * Discovered on Intel Windmill, Quanta Winterfell, and Wiwynn Windmill
	   */
	  if (cmd_args.common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING)
	    {
	      fiid_obj_t obj_rmcp_hdr = NULL;
	      fiid_obj_t obj_lan_session_hdr = NULL;
	      fiid_obj_t obj_lan_msg_hdr = NULL;
	      fiid_obj_t obj_cmd = NULL;
	      fiid_obj_t obj_lan_msg_trlr = NULL;
	      int checksum_ret = 0;
	      int unassemble_ret = 0;
	      int cmd_ret = 0;

	      if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rs)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(obj_cmd = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rs)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

#ifndef NDEBUG
	      if (cmd_args.rmcpdump)
		{
		  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
		  const char *str_cmd = NULL;

		  str_cmd = ipmi_cmd_str (IPMI_NET_FN_APP_RQ, IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES);

		  debug_hdr_str (DEBUG_UTIL_TYPE_IPMI_1_5,
				 DEBUG_UTIL_DIRECTION_RESPONSE,
				 DEBUG_UTIL_FLAGS_DEFAULT,
				 str_cmd,
				 hdrbuf,
				 DEBUG_UTIL_HDR_BUFLEN);
		  
		  if (ipmi_dump_lan_packet (STDERR_FILENO,
					    ics[i].hostname,
					    hdrbuf,
					    NULL,
					    buf,
					    len,
					    tmpl_lan_msg_hdr_rs,
					    tmpl_cmd_get_channel_authentication_capabilities_rs) < 0)
		    IPMIPOWER_DEBUG (("ipmi_dump_lan_packet: %s", strerror (errno)));
		}
#endif /* NDEBUG */

	      if ((checksum_ret = ipmi_lan_check_packet_checksum (buf, len)) < 0)
		{
		  IPMIPOWER_ERROR (("ipmi_lan_check_packet_checksum: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (checksum_ret
		  && ((unassemble_ret = unassemble_ipmi_lan_pkt (buf,
								 len,
								 obj_rmcp_hdr,
								 obj_lan_session_hdr,
								 obj_lan_msg_hdr,
								 obj_cmd,
								 obj_lan_msg_trlr,
								 IPMI_INTERFACE_FLAGS_DEFAULT)) < 0))
		{
		  IPMIPOWER_ERROR (("unassemble_ipmi_lan_pkt: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      /* achu: check for cmd type, but don't bother checking
	       * sequence numbers or completion code.  The fact it
	       * returns is sufficient.  We just need to make sure we
	       * get something back from the BMC to ensure the machine
	       * is still there.
	       */

	      if (checksum_ret
		  && unassemble_ret
		  && ((cmd_ret = ipmi_check_cmd (obj_cmd, IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES)) < 0))
		{
		  IPMIPOWER_ERROR (("ipmi_check_cmd: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}
	      
	      if (checksum_ret && unassemble_ret && cmd_ret)
		{
		  /* We'll say this is equivalent to what pong response from RMCP */
		  message_type = RMCP_ASF_MESSAGE_TYPE_PRESENCE_PONG;
		  ipmi_supported = 1;
		}

	      fiid_obj_destroy (obj_rmcp_hdr);
	      fiid_obj_destroy (obj_lan_session_hdr);
	      fiid_obj_destroy (obj_lan_msg_hdr);
	      fiid_obj_destroy (obj_cmd);
	      fiid_obj_destroy (obj_lan_msg_trlr);
	    }
	  else			/* !IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING */
	    {
	      fiid_obj_t rmcp_hdr = NULL;
	      fiid_obj_t rmcp_pong = NULL;

	      if (!(rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (!(rmcp_pong = fiid_obj_create (tmpl_cmd_asf_presence_pong)))
		{
		  IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

#ifndef NDEBUG
	      if (cmd_args.rmcpdump)
		{
		  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
		  
		  debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
				 DEBUG_UTIL_DIRECTION_NONE,
				 DEBUG_UTIL_FLAGS_DEFAULT,
				 DEBUG_UTIL_RMCPPING_STR,
				 hdrbuf,
				 DEBUG_UTIL_HDR_BUFLEN);
		  
		  if (ipmi_dump_rmcp_packet (STDERR_FILENO,
					     ics[i].hostname,
					     hdrbuf,
					     NULL,
					     buf,
					     len,
					     tmpl_cmd_asf_presence_pong) < 0)
		    IPMIPOWER_DEBUG (("ipmi_dump_rmcp_packet: %s", strerror (errno)));
		}
#endif /* NDEBUG */

	      if ((ret = unassemble_rmcp_pkt (buf,
					      len,
					      rmcp_hdr,
					      rmcp_pong,
					      IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
		{
		  IPMIPOWER_ERROR (("unassemble_rmcp_pkt: %s", strerror (errno)));
		  exit (EXIT_FAILURE);
		}

	      if (ret)
		{
		  /* achu: check for ipmi_support and pong type, but don't
		   * check for message tag.  On occassion, I have witnessed
		   * BMCs send message tags "out of sync".  For example, you
		   * send 8, BMC returns 7.  You send 9, BMC returns 8.  We
		   * really don't care if the BMC is out of sync.  We just
		   * need to make sure we get something back from the BMC to
		   * ensure the machine is still there.
		   */
		  
		  if (FIID_OBJ_GET (rmcp_pong,
				    "message_type",
				    &val) < 0)
		    {
		      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'message_type': %s",
					fiid_obj_errormsg (rmcp_pong)));
		      exit (EXIT_FAILURE);
		    }
		  message_type = val;
		  
		  if (FIID_OBJ_GET (rmcp_pong,
				    "supported_entities.ipmi_supported",
				    &val) < 0)
		    {
		      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'supported_entities.ipmi_supported': %s",
					fiid_obj_errormsg (rmcp_pong)));
		      exit (EXIT_FAILURE);
		    }
		  ipmi_supported = val;
		}

	      fiid_obj_destroy (rmcp_hdr);
	      fiid_obj_destroy (rmcp_pong);
	    }

	  if (message_type == RMCP_ASF_MESSAGE_TYPE_PRESENCE_PONG && ipmi_supported)
	    {
	      if (cmd_args.ping_packet_count && cmd_args.ping_percent)
		ics[i].ping_packet_count_recv++;
	      
	      if (cmd_args.ping_consec_count)
		{
		  /* Don't increment twice, its possible a previous pong
		   * response was late, and we quickly receive two
		   * pong responses
		   */
		  if (!ics[i].ping_last_packet_recv_flag)
		    ics[i].ping_consec_count++;
		  
		  ics[i].ping_last_packet_recv_flag++;
		}
	      
	      if (cmd_args.ping_packet_count && cmd_args.ping_percent)
		{
		  if (ics[i].link_state == IPMIPOWER_LINK_STATE_GOOD)
		    ics[i].discover_state = IPMIPOWER_DISCOVER_STATE_DISCOVERED;
		  else
		    {
		      if (cmd_args.ping_consec_count
			  && ics[i].ping_consec_count >= cmd_args.ping_consec_count)
			ics[i].discover_state = IPMIPOWER_DISCOVER_STATE_DISCOVERED;
		      else
			ics[i].discover_state = IPMIPOWER_DISCOVER_STATE_BADCONNECTION;
		    }
		}
	      else
		{
		  ics[i].discover_state = IPMIPOWER_DISCOVER_STATE_DISCOVERED;
		}
	      ics[i].last_ping_recv.tv_sec = cur_time.tv_sec;
	      ics[i].last_ping_recv.tv_usec = cur_time.tv_usec;
	      
	    }
     	} /* !IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING */

      /* Is the node gone?? */
      timeval_sub (&cur_time, &ics[i].last_ping_recv, &result);
      timeval_millisecond_calc (&result, &ms_time);
      if (ms_time >= cmd_args.ping_timeout)
        ics[i].discover_state = IPMIPOWER_DISCOVER_STATE_UNDISCOVERED;
    }

  timeval_sub (&next_ping_sends_time, &cur_time, &result);
  timeval_millisecond_calc (&result, &ms_time);
  *timeout = ms_time;
}
